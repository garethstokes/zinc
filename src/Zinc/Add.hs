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
import Data.List (find)
import System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory, removeDirectoryRecursive)
import System.FilePath (takeDirectory, (</>))
import Zinc.Fetch (gitFetchManifest, resolveRef)
import Zinc.Git (cloneAt)
import Zinc.Lock (LockedPackage (..), renderLock)
import Zinc.Manifest
  ( Dependency (depName, depRef)
  , Ref (Latest)
  , WorkspaceManifest (wsDependencies, wsRegistry)
  , addDep
  , parseWorkspace
  , renderWorkspace
  )
import Zinc.Report (renderResolution)
import Zinc.Resolve (ResolvedDep (..), isBootLib, resolve)
import Zinc.Store (contentHash)

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
freezeClosure storeRoot = go []
  where
    go acc [] = pure (Right (reverse acc))
    go acc (dep : rest) = do
      resolved <- resolveRef (rdRepo dep) (rdRef dep)
      case resolved of
        Left err -> pure (Left (rdName dep ++ ": " ++ err))
        Right refStr -> do
          let dest = storeRoot </> "checkout" </> rdName dep
          stale <- doesDirectoryExist dest
          when stale (removeDirectoryRecursive dest)
          cloned <- cloneAt (rdRepo dep) refStr dest
          case cloned of
            Left err -> pure (Left ("freeze " ++ rdName dep ++ ": " ++ err))
            Right rev -> do
              sha <- contentHash dest
              go (lockEntry dep rev sha : acc) rest

-- | Add (or refresh) a dependency: update the workspace model, resolve the
-- full closure (real git fetch), freeze it, and write @zinc.lock@ +
-- @zinc.toml@. Returns the resolution table for display. (Interactive y/N
-- confirmation is layered on by the CLI.)
runAdd :: FilePath -> FilePath -> String -> Ref -> String -> IO (Either String String)
runAdd wsFile storeRoot name ref repo = do
  src <- readFile wsFile
  case parseWorkspace src of
    Left err -> pure (Left err)
    Right ws -> do
      let ws' = addDep ws name ref repo
      resolved <- resolve isBootLib (gitFetchManifest storeRoot) (wsDependencies ws') (wsRegistry ws')
      case resolved of
        Left err -> pure (Left err)
        Right closure -> do
          frozen <- freezeClosure storeRoot closure
          case frozen of
            Left err -> pure (Left err)
            Right locks -> do
              writeFile (takeDirectory wsFile </> "zinc.lock") (renderLock locks)
              writeFile wsFile (renderWorkspace ws')
              pure (Right (renderResolution closure))

-- | CLI entry: @zinc add \<name\>@ in the current workspace. Resolves the repo
-- from the workspace @[registry]@ (Hackage discovery for unknown packages is
-- tracked separately) and stores builds under @~/.zinc/store@.
addInWorkspace :: String -> IO (Either String String)
addInWorkspace name = do
  let wsFile = "zinc.toml"
  present <- doesFileExist wsFile
  if not present
    then pure (Left "no zinc.toml in the current directory")
    else do
      home <- getHomeDirectory
      src <- readFile wsFile
      case parseWorkspace src of
        Left err -> pure (Left err)
        Right ws -> case lookup name (wsRegistry ws) of
          Nothing ->
            pure (Left ("no repo known for '" ++ name ++ "' — add it to [registry] (Hackage discovery: zinc-5la)"))
          Just repo ->
            let ref = maybe Latest depRef (find ((== name) . depName) (wsDependencies ws))
             in runAdd wsFile (home </> ".zinc" </> "store") name ref repo

-- | Re-resolve the workspace's dependencies (bumping Latest refs) and rewrite
-- the lockfile. Like 'runAdd' but without adding a new dependency.
runUpdate :: FilePath -> FilePath -> IO (Either String String)
runUpdate wsFile storeRoot = do
  src <- readFile wsFile
  case parseWorkspace src of
    Left err -> pure (Left err)
    Right ws -> do
      resolved <- resolve isBootLib (gitFetchManifest storeRoot) (wsDependencies ws) (wsRegistry ws)
      case resolved of
        Left err -> pure (Left err)
        Right closure -> do
          frozen <- freezeClosure storeRoot closure
          case frozen of
            Left err -> pure (Left err)
            Right locks -> do
              writeFile (takeDirectory wsFile </> "zinc.lock") (renderLock locks)
              pure (Right (renderResolution closure))

-- | CLI entry: @zinc update@ in the current workspace.
updateInWorkspace :: IO (Either String String)
updateInWorkspace = do
  let wsFile = "zinc.toml"
  present <- doesFileExist wsFile
  if not present
    then pure (Left "no zinc.toml in the current directory")
    else do
      home <- getHomeDirectory
      runUpdate wsFile (home </> ".zinc" </> "store")
