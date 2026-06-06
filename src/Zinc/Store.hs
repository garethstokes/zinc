-- | The content-addressed source store (spec §7,§8) and integrity checking.
-- Fetched dependency sources live under @\<root\>/src/\<name\>-\<rev\>@, and each
-- is identified by a deterministic content hash so a dependency builds once
-- per machine and tampering is detectable.
module Zinc.Store
  ( resolveStoreRoot
  , storeSrcPath
  , srcDirName
  , srcSlug
  , contentHash
  , verifyContent
  , hashString
  , withStoreLock
  ) where

import Data.Char (isAlphaNum)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Digest.Pure.SHA (sha256, showDigest)
import Data.List (dropWhileEnd, sortOn)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM, when)
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, getHomeDirectory, getSymbolicLinkTarget, listDirectory, pathIsSymbolicLink, removeDirectory)
import System.Environment (lookupEnv)
import System.IO.Error (catchIOError)
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
-- Keyed by 'srcKey' (the repo, for git, so a monorepo's sub-packages share one
-- clone; or the name, for a tarball) rather than the package name — see
-- 'srcDirName' (zinc-qln).
storeSrcPath :: FilePath -> String -> String -> FilePath
storeSrcPath root key rev = root </> "src" </> srcDirName key rev

-- | The @src/@ directory name for a source checkout: @\<slug>-\<rev>@. Computed
-- identically here and in GC so the sweep keeps the dirs the builder creates.
srcDirName :: String -> String -> FilePath
srcDirName key rev = srcSlug key ++ "-" ++ rev

-- | A filesystem-safe, collision-resistant slug for a source key (a repo URL or
-- a package name): the readable basename plus a short hash of the full key.
-- The basename keeps store dirs greppable; the hash keeps distinct repos that
-- share a basename (or sanitise alike) apart. Any @#subdir@ is dropped first, so
-- sub-packages of one monorepo (same repo, different subdir) slug identically
-- and thus share a checkout (zinc-qln).
srcSlug :: String -> String
srcSlug key =
  let root  = takeWhile (/= '#') key
      trim  = dropWhileEnd (== '/') root
      base  = reverse (takeWhile (/= '/') (reverse trim))
      clean = map (\c -> if isAlphaNum c || c == '.' || c == '_' then c else '-') base
   in (if null clean then "src" else clean) ++ "-" ++ take 12 (showDigest (sha256 (BL8.pack root)))

-- | Deterministic content hash of a source tree, independent of git/tar/Nix
-- internals: sha256 over each file's @relpath \\0 contents \\0@ in sorted path
-- order. The @.git@ directory is excluded so the hash reflects source only.
contentHash :: FilePath -> IO String
contentHash dir = do
  rels <- sortOn fst <$> listFiles dir
  chunks <- forM rels $ \(rel, ent) -> do
    -- A symlink's content is its target path (as git stores it) — never follow
    -- it: the target may be outside the tree or dangling (e.g. monorepo cbits
    -- symlinks), and following would be non-reproducible or crash.
    body <- case ent of
      RegularFile  -> BL.readFile (dir </> rel)
      SymlinkTo tgt -> pure (BL8.pack tgt)
    pure (BL8.pack (rel ++ "\0") <> body <> BL8.pack "\0")
  pure ("sha256:" ++ showDigest (sha256 (BL.concat chunks)))

-- | Recompute a tree's content hash and compare to the expected value.
verifyContent :: FilePath -> String -> IO Bool
verifyContent dir expected = (== expected) <$> contentHash dir

-- | A short, stable @sha256:@ digest of a string — used to fingerprint the
-- lockfile so perf records correlate to a dependency set (perf spec §3.1).
hashString :: String -> String
hashString s = "sha256:" ++ take 16 (showDigest (sha256 (BL8.pack s)))

-- | Serialize work on a content-addressed store key across processes (spec
-- §3.4): parallel agents / worktrees share @~\/.zinc\/store@, so two builds of
-- the same key must not write into @pkg\/\<key\>@ at once. Uses an atomic
-- @mkdir@ as a portable advisory lock — the directory is either created (lock
-- acquired) or already exists (held elsewhere). Crash-tolerant: a waiter polls
-- for a bounded window, then proceeds best-effort, so a stale lock left by a
-- dead process can never deadlock the build. The lock is removed only by the
-- holder that created it.
withStoreLock :: FilePath -> String -> IO a -> IO a
withStoreLock storeRoot key action = bracket acquire release (const action)
  where
    lockDir = storeRoot </> "locks" </> key
    acquire = do
      createDirectoryIfMissing True (storeRoot </> "locks")
      tryAcquire (0 :: Int)
    tryAcquire n = do
      got <- (createDirectory lockDir >> pure True) `catchIOError` const (pure False)
      if got
        then pure True
        else
          if n >= maxAttempts
            then pure False -- give up waiting; proceed best-effort (no deadlock)
            else threadDelay pollMicros >> tryAcquire (n + 1)
    release held = when held (removeDirectory lockDir `catchIOError` const (pure ()))
    maxAttempts = 2000 -- ~20s at 10ms
    pollMicros = 10000 -- 10ms

-- | A tree entry to hash: a regular file (read its bytes) or a symlink (hash
-- its target path, not the pointed-to content).
data Entry = RegularFile | SymlinkTo String

-- | Recursively list a tree's entries as @(relative path, kind)@, skipping any
-- @.git@ directory. Symlinks are classified BEFORE the directory test so a
-- symlink-to-directory is recorded as a link (its target hashed) rather than
-- followed into — keeping the hash reproducible and crash-free on monorepo
-- layouts that symlink shared @cbits@/sources across packages.
listFiles :: FilePath -> IO [(FilePath, Entry)]
listFiles root = go ""
  where
    go rel = do
      entries <- listDirectory (root </> rel)
      fmap concat $ forM entries $ \e ->
        if e == ".git"
          then pure []
          else do
            let r = if null rel then e else rel </> e
            isLink <- pathIsSymbolicLink (root </> r)
            if isLink
              then do
                tgt <- getSymbolicLinkTarget (root </> r)
                pure [(r, SymlinkTo tgt)]
              else do
                isDir <- doesDirectoryExist (root </> r)
                if isDir then go r else pure [(r, RegularFile)]
