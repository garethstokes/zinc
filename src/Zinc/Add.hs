-- | The freeze engine behind @zinc add@ (spec §9): turn a resolved closure
-- into pinned lockfile entries by cloning each dependency at its ref to obtain
-- the exact commit and a content hash. (The CLI/confirm wiring sits on top.)
module Zinc.Add
  ( lockEntry
  , freezeClosure
  ) where

import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import Control.Monad (when)
import Zinc.Fetch (resolveRef)
import Zinc.Git (cloneAt)
import Zinc.Lock (LockedPackage (..))
import Zinc.Resolve (ResolvedDep (..))
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
