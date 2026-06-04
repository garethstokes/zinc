-- | The content-addressed source store (spec §7,§8) and integrity checking.
-- Fetched dependency sources live under @\<root\>/src/\<name\>-\<rev\>@, and each
-- is identified by a deterministic content hash so a dependency builds once
-- per machine and tampering is detectable.
module Zinc.Store
  ( resolveStoreRoot
  , storeSrcPath
  , contentHash
  , verifyContent
  , hashString
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Digest.Pure.SHA (sha256, showDigest)
import Data.List (sort)
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getHomeDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

-- | Resolve the store root, shared by @add@, @update@, and @build@ so a
-- fetched/built dependency is cached once per machine and reused across
-- commands and projects (spec §8). Honours the @ZINC_STORE@ environment
-- variable when set; otherwise defaults to @~\/.zinc\/store@.
resolveStoreRoot :: IO FilePath
resolveStoreRoot = do
  override <- lookupEnv "ZINC_STORE"
  case override of
    Just dir | not (null dir) -> pure dir
    _ -> (\home -> home </> ".zinc" </> "store") <$> getHomeDirectory

-- | Canonical store location for a package's source at a resolved revision.
storeSrcPath :: FilePath -> String -> String -> FilePath
storeSrcPath root name rev = root </> "src" </> (name ++ "-" ++ rev)

-- | Deterministic content hash of a source tree, independent of git/tar/Nix
-- internals: sha256 over each file's @relpath \\0 contents \\0@ in sorted path
-- order. The @.git@ directory is excluded so the hash reflects source only.
contentHash :: FilePath -> IO String
contentHash dir = do
  rels <- sort <$> listFiles dir
  chunks <- forM rels $ \rel -> do
    body <- BL.readFile (dir </> rel)
    pure (BL8.pack (rel ++ "\0") <> body <> BL8.pack "\0")
  pure ("sha256:" ++ showDigest (sha256 (BL.concat chunks)))

-- | Recompute a tree's content hash and compare to the expected value.
verifyContent :: FilePath -> String -> IO Bool
verifyContent dir expected = (== expected) <$> contentHash dir

-- | A short, stable @sha256:@ digest of a string — used to fingerprint the
-- lockfile so perf records correlate to a dependency set (perf spec §3.1).
hashString :: String -> String
hashString s = "sha256:" ++ take 16 (showDigest (sha256 (BL8.pack s)))

-- | Recursively list files as relative paths, skipping any @.git@ directory.
listFiles :: FilePath -> IO [FilePath]
listFiles root = go ""
  where
    go rel = do
      entries <- listDirectory (root </> rel)
      fmap concat $ forM entries $ \e ->
        if e == ".git"
          then pure []
          else do
            let r = if null rel then e else rel </> e
            isDir <- doesDirectoryExist (root </> r)
            if isDir then go r else pure [r]
