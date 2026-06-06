-- | The freeze engine behind @zinc add@ (spec §9): turn a resolved closure
-- into pinned lockfile entries by cloning each dependency at its ref to obtain
-- the exact commit and a content hash. (The CLI/confirm wiring sits on top.)
module Zinc.Add
  ( lockEntry
  , freezeClosure
  , runAdd
  , addInWorkspace
  , enrichWithRepos
  , runUpdate
  , updateInWorkspace
  , vendorInWorkspace
  , runVendor
  , splitNameVersion
  ) where

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.List (find, intercalate)
import System.IO (readFile')
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import System.FilePath (takeDirectory, (</>))
import Zinc.Closure (ClosureReport (crMembers, crNeedsVendoring), installedVersion, runClosure)
import Zinc.Delta (ClosureDelta, closureDelta)
import Zinc.Diagnostic (ZincError (DepNoGitRepo, ManifestParse, NoZincToml))
import Zinc.Except (Result, failWith, failWithError, liftEitherE, liftIO, orFail, orFailE, runResult)
import Zinc.Fetch (gitFetchManifest, resolveRef)
import Zinc.Git (cloneAt)
import Zinc.Hackage (fetchHackageTarball, hackageSourceRepo)
import Zinc.Lock (LockedPackage (..), Source (..), parseLock, renderLock)
import Zinc.Manifest
  ( Dependency (depName, depRef)
  , Ref (Latest, Rev, Vendored)
  , WorkspaceManifest (wsDependencies, wsGhc)
  , depRepos
  , addDep
  , addVendored
  , parseWorkspace
  , renderWorkspace
  )
import Zinc.Report (renderResolution)
import Zinc.Resolve (ResolvedDep (..), isBootLib, resolve)
import Zinc.Store (contentHash, resolveStoreRoot)

-- | Build a lock entry from a resolved dep and its resolved commit + hash. A
-- vendored pin records a tarball source (version from the ref); everything else
-- a git source (repo + resolved commit).
lockEntry :: ResolvedDep -> String -> String -> LockedPackage
lockEntry dep rev sha =
  LockedPackage
    { lockName = rdName dep
    , lockSource = case rdRef dep of
        Vendored ver -> TarballSource ver
        _            -> GitSource (rdRepo dep) rev
    , lockSha256 = sha
    , lockDepends = rdDepends dep
    }

-- | Bring every dep in the closure into the store at its ref — git clone, or a
-- Hackage tarball fetch for a vendored pin (b1z) — capture its exact
-- commit/version + content hash, and produce the lockfile entries.
-- Short-circuits on the first failure.
freezeClosure :: FilePath -> [ResolvedDep] -> IO (Either ZincError [LockedPackage])
freezeClosure storeRoot = runResult . traverse freezeOne
  where
    freezeOne :: ResolvedDep -> Result LockedPackage
    freezeOne dep = do
      let dest = storeRoot </> "checkout" </> rdName dep
      case rdRef dep of
        Vendored ver -> do
          _ <- orFail (first (("freeze " ++ rdName dep ++ ": ") ++) <$> fetchHackageTarball (rdName dep) ver dest)
          sha <- liftIO (contentHash dest)
          pure (lockEntry dep ver sha)
        _ -> do
          refStr <- orFail (first ((rdName dep ++ ": ") ++) <$> resolveRef (rdName dep) (rdRepo dep) (rdRef dep))
          liftIO $ do
            stale <- doesDirectoryExist dest
            when stale (removeDirectoryRecursive dest)
          rev <- orFail (first (("freeze " ++ rdName dep ++ ": ") ++) <$> cloneAt (rdRepo dep) refStr dest)
          sha <- liftIO (contentHash dest)
          pure (lockEntry dep rev sha)

-- | Resolve a workspace's dependency closure (real git fetch), freeze it, and
-- write @zinc.lock@; returns the resolution table for display. Shared by
-- 'runAdd' and 'runUpdate'.
freezeWorkspace :: FilePath -> FilePath -> WorkspaceManifest -> Result String
freezeWorkspace wsFile storeRoot ws = do
  (closure, locks) <- resolveFreeze storeRoot [] ws
  liftIO $ writeFile (takeDirectory wsFile </> "zinc.lock") (renderLock locks)
  pure (renderResolution closure)

-- | Resolve the workspace's dependency closure and freeze it to lock entries,
-- WITHOUT writing — so @update@ can diff the result against the existing lock
-- before committing it (zinc-90j.2). @pins@ holds named deps at a ref without
-- forcing inclusion, for per-package @update \<pkg\>@ (90j.3); empty = full
-- resolve. Returns the resolved closure + its locks.
resolveFreeze :: FilePath -> [(String, Ref)] -> WorkspaceManifest -> Result ([ResolvedDep], [LockedPackage])
resolveFreeze storeRoot pins ws = do
  closure <- orFailE (resolve isBootLib (gitFetchManifest storeRoot (wsGhc ws)) hackageDiscover pins (wsDependencies ws) (depRepos ws))
  locks <- orFailE (freezeClosure storeRoot closure)
  pure (closure, locks)

-- | Discover a transitive dependency's git repo from Hackage when no registry
-- pins it (zinc-49o auto-fill via 5la), so 'resolve' can walk a real upstream's
-- whole closure without every repo hand-listed. A fetch failure / no
-- source-repository degrades to 'Nothing' (the resolver then errors with the
-- precise missing-repo diagnostic).
hackageDiscover :: String -> IO (Maybe String)
hackageDiscover n = either (const Nothing) id <$> hackageSourceRepo n

-- | Add (or refresh) a dependency: update the workspace model, freeze the
-- closure, and write @zinc.lock@ + @zinc.toml@. Returns the resolution table.
-- (Interactive y/N confirmation is layered on by the CLI.)
runAdd :: FilePath -> FilePath -> String -> Ref -> String -> IO (Either ZincError String)
runAdd wsFile storeRoot name ref repo = runResult $ do
  src <- liftIO (readFile wsFile)
  ws <- liftEitherE (first (ManifestParse wsFile) (parseWorkspace src))
  let ws' = addDep ws name ref repo
  res <- freezeWorkspace wsFile storeRoot ws'
  liftIO $ writeFile wsFile (renderWorkspace ws')
  pure res

-- | CLI entry: @zinc add \<name\>@ in the current workspace. If the package's
-- repo is already pinned in the manifest, freezes it directly; otherwise
-- deterministically discovers its non-boot closure + repos (ghc-pkg + Hackage,
-- via 'runClosure'), refuses if any member needs vendoring, then pre-populates
-- the manifest and freezes (spec §9 / zinc-49o).
addInWorkspace :: String -> IO (Either ZincError String)
addInWorkspace name = runResult $ do
  let wsFile = "zinc.toml"
  present <- liftIO (doesFileExist wsFile)
  when (not present) $ failWithError (NoZincToml ".")
  src <- liftIO (readFile wsFile)
  ws <- liftEitherE (first (ManifestParse wsFile) (parseWorkspace src))
  storeRoot <- liftIO resolveStoreRoot
  case lookup name (depRepos ws) of
    -- Repo already pinned in the manifest: freeze it directly (offline).
    Just repo -> do
      let ref = maybe Latest depRef (find ((== name) . depName) (wsDependencies ws))
      orFailE (runAdd wsFile storeRoot name ref repo)
    -- Unknown repo: deterministically discover the package's non-boot closure
    -- (ghc-pkg) + each member's repo (Hackage), refuse if any needs vendoring,
    -- then pre-populate the manifest and freeze (zinc-49o part 4).
    Nothing -> do
      rep <- orFailE (runClosure name)
      when (not (null (crNeedsVendoring rep))) $
        failWithError (DepNoGitRepo (unwords (crNeedsVendoring rep)))
      let found = [(m, r) | (m, Just r) <- crMembers rep]
          enriched = enrichWithRepos ws found
      liftIO (writeFile wsFile (renderWorkspace enriched))
      freezeWorkspace wsFile storeRoot enriched

-- | Fold discovered @(name, repo)@ pairs into a workspace as pinned
-- dependencies, preserving any ref already declared (else 'Latest') and any
-- repo already declared — so a hand-supplied override (e.g. a monorepo
-- @url#subdir@ that Hackage's metadata omits) wins over discovery.
enrichWithRepos :: WorkspaceManifest -> [(String, String)] -> WorkspaceManifest
enrichWithRepos = foldr add
  where
    add (m, discovered) w = addDep w m (refFor w m) (maybe discovered id (lookup m (depRepos w)))
    refFor w m = maybe Latest depRef (find ((== m) . depName) (wsDependencies w))

-- | Re-resolve the workspace's dependencies and return the before->after
-- closure delta. A @\<pkg\>@ target updates JUST that dep and its affected
-- sub-closure — every other locked dep is soft-pinned to its current ref, so it
-- stays put while deps the new \<pkg\> version drops fall out and new ones are
-- added (90j.3); a bare update (Nothing) bumps all movable refs. Writes the new
-- lock unless @dryRun@ (90j.2).
runUpdate :: Maybe String -> Bool -> FilePath -> FilePath -> IO (Either ZincError ClosureDelta)
runUpdate mtarget dryRun wsFile storeRoot = runResult $ do
  src <- liftIO (readFile wsFile)
  ws <- liftEitherE (first (ManifestParse wsFile) (parseWorkspace src))
  let lockFile = takeDirectory wsFile </> "zinc.lock"
  old <- liftIO (readLockOr lockFile)
  let pins = case mtarget of
        Nothing  -> []
        Just pkg -> [(lockName l, pinRef l) | l <- old, lockName l /= pkg]
  (_, locks) <- resolveFreeze storeRoot pins ws
  liftIO $ when (not dryRun) (writeFile lockFile (renderLock locks))
  pure (closureDelta old locks)
  where
    -- Pin a locked dep to its exact current source: the resolved commit (git) or
    -- the vendored version. Soft — applied only if the dep is still walked.
    pinRef l = case lockSource l of
      GitSource _ rev   -> Rev rev
      TarballSource ver -> Vendored ver
    -- Strict read: a lazy readFile would keep the handle open until 'old' is
    -- forced (after the writeFile below), and rewriting the same path then hits
    -- "resource busy". readFile' closes it before we overwrite.
    readLockOr f = do
      there <- doesFileExist f
      if there then either (const []) id . parseLock <$> readFile' f else pure []

-- | CLI entry: @zinc update [PKG] [--dry-run]@ in the current workspace.
updateInWorkspace :: Maybe String -> Bool -> IO (Either ZincError ClosureDelta)
updateInWorkspace mtarget dryRun = runResult $ do
  let wsFile = "zinc.toml"
  present <- liftIO (doesFileExist wsFile)
  when (not present) $ failWithError (NoZincToml ".")
  storeRoot <- liftIO resolveStoreRoot
  orFailE (runUpdate mtarget dryRun wsFile storeRoot)

-- | CLI entry: @zinc vendor \<pkg...\>@ — recover a no-git dependency (colour,
-- tf-random — darcs-era) by pinning its Hackage sdist tarball (b1z, design s2).
-- For each package, resolve a version (an explicit @\<name\>-\<version\>@ arg,
-- else the version installed in this GHC environment — the same toolchain truth
-- closure discovery uses), record it in the manifest as a vendored pin, then
-- re-resolve + freeze the workspace, fetching + hashing the tarball into the
-- store. Hackage is touched only here; @zinc build@ reads the pinned source from
-- the lock + store. The manifest is rewritten only after a successful freeze, so
-- a failed fetch leaves it untouched.
vendorInWorkspace :: [String] -> IO (Either ZincError String)
vendorInWorkspace pkgs = runResult $ do
  let wsFile = "zinc.toml"
  present <- liftIO (doesFileExist wsFile)
  when (not present) $ failWithError (NoZincToml ".")
  storeRoot <- liftIO resolveStoreRoot
  orFailE (runVendor wsFile storeRoot pkgs)

-- | Record the named packages as vendored pins and re-freeze the workspace
-- (explicit paths, the testable core of 'vendorInWorkspace'). The manifest is
-- rewritten only after a successful freeze.
runVendor :: FilePath -> FilePath -> [String] -> IO (Either ZincError String)
runVendor wsFile storeRoot pkgs = runResult $ do
  src <- liftIO (readFile wsFile)
  ws <- liftEitherE (first (ManifestParse wsFile) (parseWorkspace src))
  resolved <- traverse resolveVendorVersion pkgs
  let ws' = foldl (\w (n, v) -> addVendored w n v) ws resolved
  res <- freezeWorkspace wsFile storeRoot ws'
  liftIO (writeFile wsFile (renderWorkspace ws'))
  pure res

-- | Resolve a vendor target to @(name, version)@: an explicit @name-version@
-- arg, otherwise the version installed in this GHC environment.
resolveVendorVersion :: String -> Result (String, String)
resolveVendorVersion arg = case splitNameVersion arg of
  Just nv -> pure nv
  Nothing -> do
    mv <- liftIO (installedVersion arg)
    case mv of
      Just v  -> pure (arg, v)
      Nothing -> failWith (arg ++ ": not installed in this GHC environment; vendor an explicit <name>-<version>")

-- | Split a @name-version@ string into its parts, the version being the trailing
-- dot-separated-digits component. A name may itself contain dashes (e.g.
-- @tf-random@), so the version is the LAST dashed component; 'Nothing' if the
-- arg has no version suffix (a bare name).
splitNameVersion :: String -> Maybe (String, String)
splitNameVersion s = case reverse (splitOn '-' s) of
  (v : rest@(_ : _)) | isVersion v -> Just (intercalate "-" (reverse rest), v)
  _                                -> Nothing
  where
    isVersion v = not (null v) && all (`elem` ("0123456789." :: String)) v
    splitOn c xs = case break (== c) xs of
      (a, [])    -> [a]
      (a, _ : r) -> a : splitOn c r
