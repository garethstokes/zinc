-- | Store garbage collection. The content-addressed store (@~\/.zinc\/store@)
-- grows without bound: every built package key under @pkg\/@ and every fetched
-- source under @src\/@ lingers after the workspace that needed it moves on.
-- GC marks the entries kept alive by a set of /roots/ (each a workspace's
-- locked closure built with a given GHC) and sweeps everything else.
module Zinc.GC
  ( GCRoot (..)
  , liveKeys
  , liveSrcNames
  , gcStore
  , runGc
  ) where

import Data.List (sort)
import qualified Data.Set as Set
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Zinc.Cache (BuildKey (..), buildCacheKey)
import Zinc.Lock (LockedPackage (..), parseLock)
import Zinc.Manifest (WorkspaceManifest (wsGhc), parseWorkspace)
import Zinc.Store (resolveStoreRoot)

-- | A live root: a workspace's locked closure and the GHC it builds with.
-- Together these determine exactly which store entries are reachable.
data GCRoot = GCRoot
  { grGhc   :: String
  , grLocks :: [LockedPackage]
  }
  deriving (Eq, Show)

-- | The @pkg\/@ entry names (build cache keys) kept alive by the roots. Mirrors
-- the key the build driver computes: rev + ghc + dep unit-ids, no extra opts.
liveKeys :: [GCRoot] -> Set.Set String
liveKeys = Set.fromList . concatMap (\r -> map (keyFor (grGhc r)) (grLocks r))
  where
    keyFor ghc l = buildCacheKey (BuildKey (lockRev l) ghc (lockDepends l) [])

-- | The @src\/@ entry names (@name-rev@) kept alive by the roots.
liveSrcNames :: [GCRoot] -> Set.Set String
liveSrcNames = Set.fromList . concatMap (map srcName . grLocks)
  where
    srcName l = lockName l ++ "-" ++ lockRev l

-- | Sweep the store, deleting every @pkg\/@ and @src\/@ entry not kept alive by
-- the given roots. Returns the sorted names removed from @pkg\/@ and @src\/@.
gcStore :: FilePath -> [GCRoot] -> IO ([FilePath], [FilePath])
gcStore root roots = do
  removedPkg <- sweep (root </> "pkg") (liveKeys roots)
  removedSrc <- sweep (root </> "src") (liveSrcNames roots)
  pure (removedPkg, removedSrc)
  where
    sweep dir keep = do
      there <- doesDirectoryExist dir
      if not there
        then pure []
        else do
          entries <- listDirectory dir
          let dead = sort (filter (`Set.notMember` keep) entries)
          mapM_ (removeDirectoryRecursive . (dir </>)) dead
          pure dead

-- | CLI entry: GC the shared store, treating the workspace at @wsDir@ as the
-- sole live root (its @zinc.lock@ closure built with its @[workspace] ghc@).
runGc :: FilePath -> IO (Either String ([FilePath], [FilePath]))
runGc wsDir = do
  hasWs <- doesFileExist (wsDir </> "zinc.toml")
  if not hasWs
    then pure (Left "no zinc.toml in the current directory")
    else do
      wsSrc <- readFile (wsDir </> "zinc.toml")
      case parseWorkspace wsSrc of
        Left err -> pure (Left err)
        Right ws -> do
          hasLock <- doesFileExist (wsDir </> "zinc.lock")
          locks <-
            if hasLock
              then either (const []) id . parseLock <$> readFile (wsDir </> "zinc.lock")
              else pure []
          storeRoot <- resolveStoreRoot
          Right <$> gcStore storeRoot [GCRoot (wsGhc ws) locks]
