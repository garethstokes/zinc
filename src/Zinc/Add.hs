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
  ) where

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.List (find, intercalate)
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import System.FilePath (takeDirectory, (</>))
import Zinc.Closure (ClosureReport (crMembers, crNeedsVendoring), runClosure)
import Zinc.Diagnostic (ZincError (DepNoGitRepo, NoZincToml))
import Zinc.Except (Result, failWithError, liftEither, liftIO, orFail, orFailE, runResult)
import Zinc.Fetch (gitFetchManifest, resolveRef)
import Zinc.Git (cloneAt)
import Zinc.Hackage (hackageSourceRepo)
import Zinc.Lock (LockedPackage (..), renderLock)
import Zinc.Manifest
  ( Dependency (depName, depRef)
  , Ref (Latest)
  , WorkspaceManifest (wsDependencies, wsGhc)
  , depRepos
  , addDep
  , parseWorkspace
  , renderWorkspace
  )
import Zinc.Report (renderResolution)
import Zinc.Resolve (ResolvedDep (..), isBootLib, resolve)
import Zinc.Store (contentHash, resolveStoreRoot)

-- | Build a lock entry from a resolved dep and its resolved commit + hash.
lockEntry :: ResolvedDep -> String -> String -> LockedPackage
lockEntry dep rev sha =
  LockedPackage
    { lockName = rdName dep
    , lockRepo = rdRepo dep
    , lockRev = rev
    , lockSha256 = sha
    , lockDepends = rdDepends dep
    }

-- | Clone every dep in the closure at its ref into the store, capture the exact
-- commit + content hash, and produce the lockfile entries. Short-circuits on
-- the first failure.
freezeClosure :: FilePath -> [ResolvedDep] -> IO (Either ZincError [LockedPackage])
freezeClosure storeRoot = runResult . traverse freezeOne
  where
    freezeOne :: ResolvedDep -> Result LockedPackage
    freezeOne dep = do
      refStr <- orFail (first ((rdName dep ++ ": ") ++) <$> resolveRef (rdName dep) (rdRepo dep) (rdRef dep))
      let dest = storeRoot </> "checkout" </> rdName dep
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
  closure <- orFailE (resolve isBootLib (gitFetchManifest storeRoot (wsGhc ws)) hackageDiscover (wsDependencies ws) (depRepos ws))
  locks <- orFailE (freezeClosure storeRoot closure)
  liftIO $ writeFile (takeDirectory wsFile </> "zinc.lock") (renderLock locks)
  pure (renderResolution closure)

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
  ws <- liftEither (parseWorkspace src)
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
  ws <- liftEither (parseWorkspace src)
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
        failWithError (DepNoGitRepo (intercalate ", " (crNeedsVendoring rep)))
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

-- | Re-resolve the workspace's dependencies (bumping Latest refs) and rewrite
-- the lockfile. Like 'runAdd' but without adding a new dependency.
runUpdate :: FilePath -> FilePath -> IO (Either ZincError String)
runUpdate wsFile storeRoot = runResult $ do
  src <- liftIO (readFile wsFile)
  ws <- liftEither (parseWorkspace src)
  freezeWorkspace wsFile storeRoot ws

-- | CLI entry: @zinc update@ in the current workspace.
updateInWorkspace :: IO (Either ZincError String)
updateInWorkspace = runResult $ do
  let wsFile = "zinc.toml"
  present <- liftIO (doesFileExist wsFile)
  when (not present) $ failWithError (NoZincToml ".")
  storeRoot <- liftIO resolveStoreRoot
  orFailE (runUpdate wsFile storeRoot)
