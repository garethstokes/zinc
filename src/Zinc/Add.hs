-- | The freeze engine behind @zinc add@ (spec §9): turn a resolved closure
-- into pinned lockfile entries by cloning each dependency at its ref to obtain
-- the exact commit and a content hash. (The CLI/confirm wiring sits on top.)
module Zinc.Add
  ( lockEntry
  , freezeClosure
  , runAdd
  , addInWorkspace
  , runUpdate
  , updateInWorkspace
  ) where

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.List (find)
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import System.FilePath (takeDirectory, (</>))
import Zinc.Except (Result, failWith, liftEither, liftIO, orFail, runResult)
import Zinc.Fetch (gitFetchManifest, resolveRef)
import Zinc.Git (cloneAt)
import Zinc.Lock (LockedPackage (..), renderLock)
import Zinc.Manifest
  ( Dependency (depName, depRef)
  , Ref (Latest)
  , WorkspaceManifest (wsDependencies, wsGhc, wsRegistry)
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
freezeClosure :: FilePath -> [ResolvedDep] -> IO (Either String [LockedPackage])
freezeClosure storeRoot = runResult . traverse freezeOne
  where
    freezeOne :: ResolvedDep -> Result LockedPackage
    freezeOne dep = do
      refStr <- orFail (first ((rdName dep ++ ": ") ++) <$> resolveRef (rdRepo dep) (rdRef dep))
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
  closure <- orFail (resolve isBootLib (gitFetchManifest storeRoot (wsGhc ws)) (wsDependencies ws) (wsRegistry ws))
  locks <- orFail (freezeClosure storeRoot closure)
  liftIO $ writeFile (takeDirectory wsFile </> "zinc.lock") (renderLock locks)
  pure (renderResolution closure)

-- | Add (or refresh) a dependency: update the workspace model, freeze the
-- closure, and write @zinc.lock@ + @zinc.toml@. Returns the resolution table.
-- (Interactive y/N confirmation is layered on by the CLI.)
runAdd :: FilePath -> FilePath -> String -> Ref -> String -> IO (Either String String)
runAdd wsFile storeRoot name ref repo = runResult $ do
  src <- liftIO (readFile wsFile)
  ws <- liftEither (parseWorkspace src)
  let ws' = addDep ws name ref repo
  res <- freezeWorkspace wsFile storeRoot ws'
  liftIO $ writeFile wsFile (renderWorkspace ws')
  pure res

-- | CLI entry: @zinc add \<name\>@ in the current workspace. Resolves the repo
-- from the workspace @[registry]@ (Hackage discovery for unknown packages is
-- tracked separately) and stores builds under @~\/.zinc\/store@.
addInWorkspace :: String -> IO (Either String String)
addInWorkspace name = runResult $ do
  let wsFile = "zinc.toml"
  present <- liftIO (doesFileExist wsFile)
  when (not present) $ failWith "no zinc.toml in the current directory"
  src <- liftIO (readFile wsFile)
  ws <- liftEither (parseWorkspace src)
  case lookup name (wsRegistry ws) of
    Nothing ->
      failWith ("no repo known for '" ++ name ++ "' — add it to [registry] (Hackage discovery: zinc-5la)")
    Just repo -> do
      let ref = maybe Latest depRef (find ((== name) . depName) (wsDependencies ws))
      storeRoot <- liftIO resolveStoreRoot
      orFail (runAdd wsFile storeRoot name ref repo)

-- | Re-resolve the workspace's dependencies (bumping Latest refs) and rewrite
-- the lockfile. Like 'runAdd' but without adding a new dependency.
runUpdate :: FilePath -> FilePath -> IO (Either String String)
runUpdate wsFile storeRoot = runResult $ do
  src <- liftIO (readFile wsFile)
  ws <- liftEither (parseWorkspace src)
  freezeWorkspace wsFile storeRoot ws

-- | CLI entry: @zinc update@ in the current workspace.
updateInWorkspace :: IO (Either String String)
updateInWorkspace = runResult $ do
  let wsFile = "zinc.toml"
  present <- liftIO (doesFileExist wsFile)
  when (not present) $ failWith "no zinc.toml in the current directory"
  storeRoot <- liftIO resolveStoreRoot
  orFail (runUpdate wsFile storeRoot)
