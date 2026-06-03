-- | Content-addressed artifact cache keying (spec §7, §8): a built package is
-- identified by a hash of everything that affects its output, so a dependency
-- compiles once per machine and branch-switching reuses builds.
module Zinc.Cache
  ( BuildKey (..)
  , buildCacheKey
  , storePkgPath
  , storeConfPath
  , cacheHit
  , writeCachedConf
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Digest.Pure.SHA (sha256, showDigest)
import Data.List (intercalate, sort)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))

-- | Everything that determines a built package's output.
data BuildKey = BuildKey
  { bkRev        :: String   -- ^ resolved source commit
  , bkGhcVersion :: String
  , bkDepUnitIds :: [String] -- ^ dependency unit-ids (order-insensitive)
  , bkOptions    :: [String] -- ^ ghc-options + extensions (order-insensitive)
  }
  deriving (Eq, Show)

-- | A stable cache key (sha256 hex) over the build inputs.
buildCacheKey :: BuildKey -> String
buildCacheKey bk = showDigest (sha256 (BL8.pack payload))
  where
    payload =
      intercalate
        "\0"
        [ bkRev bk
        , bkGhcVersion bk
        , intercalate "," (sort (bkDepUnitIds bk))
        , intercalate "," (sort (bkOptions bk))
        ]

-- | Location of a cached built package within the store.
storePkgPath :: FilePath -> String -> FilePath
storePkgPath root key = root </> "pkg" </> key

-- | The cached package description (.conf) location for a key.
storeConfPath :: FilePath -> String -> FilePath
storeConfPath root key = storePkgPath root key </> "package.conf"

-- | Is there a cached build for this key?
cacheHit :: FilePath -> String -> IO Bool
cacheHit root key = doesDirectoryExist (storePkgPath root key)

-- | Record a build in the cache by writing its .conf into the store entry.
-- (The compiled .hi/.a are placed alongside by the build driver.)
writeCachedConf :: FilePath -> String -> String -> IO ()
writeCachedConf root key confText = do
  createDirectoryIfMissing True (storePkgPath root key)
  writeFile (storeConfPath root key) confText
