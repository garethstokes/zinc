-- | The L2 remote artifact cache (zinc-vwn.3, spec s4): a pluggable
-- 'CacheBackend' over the content-addressed artifact store, and a first HTTP
-- implementation following the Nix binary-cache model. A build key (from
-- "Zinc.Cache") names a built package; a backend can fetch that package's
-- artifact — the @pkg\/\<key\>\/@ directory (the @.conf@ + @libHS\<unit\>.a@ +
-- @.hi\/.o@), shipped as a single @\<key\>.tar.gz@ — into the local store, so a
-- fresh filesystem (Docker/CI) reuses a build instead of recompiling.
--
-- This module is the interface + pull only; wiring pull into the build
-- (store -> remote -> compile) is zinc-vwn.4, and push is zinc-vwn.5. S3/OCI
-- backends drop in behind 'CacheBackend' later.
module Zinc.CacheBackend
  ( CacheBackend (..)
  , PullOutcome (..)
  , artifactUrl
  , curlOutcome
  , httpBackend
  , remoteCacheFromEnv
  ) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO.Error (catchIOError)
import System.Process (readProcessWithExitCode)
import Zinc.Cache (storePkgPath)

-- | The result of attempting a remote pull for one build key.
data PullOutcome
  = Pulled            -- ^ artifact fetched + unpacked into the local store
  | Miss              -- ^ the backend has no artifact for this key
  | PullFailed String -- ^ a transport/unpack error (NOT a plain miss)
  deriving (Eq, Show)

-- | A pluggable remote artifact cache. 'cbPull' brings a key's artifact into the
-- local store; 'cbPush' uploads a locally-built artifact (an explicit CI publish
-- step, zinc-vwn.5).
data CacheBackend = CacheBackend
  { cbName :: String                                       -- ^ for diagnostics
  , cbPull :: String -> FilePath -> IO PullOutcome         -- ^ key -> storeRoot -> outcome
  , cbPush :: String -> FilePath -> IO (Either String ())  -- ^ key -> storeRoot -> uploaded?
  }

-- | The content-addressed artifact URL for a build @key@ under a base URL:
-- @\<base\>\/\<key\>.tar.gz@ (the tarball of the @pkg\/\<key\>\/@ contents). The
-- base may carry a trailing slash; it is normalised away.
artifactUrl :: String -> String -> String
artifactUrl base key = stripSlash base ++ "/" ++ key ++ ".tar.gz"
  where
    stripSlash s = if not (null s) && last s == '/' then init s else s

-- | Interpret a @curl -f@ exit for a content-addressed GET: success is the
-- artifact; exit 22 (curl's code for an HTTP 4xx under @-f@) is a cache 'Miss';
-- anything else is a transport error ('PullFailed'). @Right ()@ means proceed
-- to unpack.
curlOutcome :: ExitCode -> String -> Either PullOutcome ()
curlOutcome ExitSuccess        _   = Right ()
curlOutcome (ExitFailure 22)   _   = Left Miss
curlOutcome (ExitFailure code) err =
  Left (PullFailed ("curl exit " ++ show code ++ (if null err then "" else ": " ++ err)))

-- | An HTTP/HTTPS content-addressed cache backend (Nix binary-cache model;
-- works behind any static host, bucket, or @file:\/\/@). Pull does
-- @GET \<base\>\/\<key\>.tar.gz@ and unpacks it into @pkg\/\<key\>\/@.
httpBackend :: String -> CacheBackend
httpBackend base = CacheBackend {cbName = "http " ++ base, cbPull = pull, cbPush = push}
  where
    -- Tar the local pkg/<key>/ and upload it to <base>/<key>.tar.gz (curl -T:
    -- an HTTP PUT, or a write to a file:// path). The artifact must exist locally.
    push key storeRoot = do
      let src = storePkgPath storeRoot key
          tmp = src ++ ".push.tar.gz"
      exists <- doesDirectoryExist src
      if not exists
        then pure (Left ("no local artifact for " ++ key))
        else do
          (tc, _, te) <- readProcessWithExitCode "tar" ["-czf", tmp, "-C", src, "."] ""
          case tc of
            ExitFailure _ -> removeIfExists tmp >> pure (Left ("tar " ++ key ++ ": " ++ te))
            ExitSuccess -> do
              (uc, _, ue) <- readProcessWithExitCode "curl" ["-fsS", "-T", tmp, artifactUrl base key] ""
              removeIfExists tmp
              pure $ case uc of
                ExitSuccess   -> Right ()
                ExitFailure n -> Left ("upload " ++ key ++ " (curl exit " ++ show n ++ ")" ++ (if null ue then "" else ": " ++ ue))

    pull key storeRoot = do
      let dest = storePkgPath storeRoot key
          tmp = dest ++ ".pull.tar.gz"
      createDirectoryIfMissing True (takeDirectory dest)
      (code, _, err) <- readProcessWithExitCode "curl" ["-fsSL", artifactUrl base key, "-o", tmp] ""
      case curlOutcome code err of
        Left outcome -> removeIfExists tmp >> pure outcome
        Right () -> do
          stale <- doesDirectoryExist dest
          when stale (removeDirectoryRecursive dest)
          createDirectoryIfMissing True dest
          (xc, _, xe) <- readProcessWithExitCode "tar" ["-xzf", tmp, "-C", dest] ""
          removeIfExists tmp
          pure $ case xc of
            ExitSuccess   -> Pulled
            ExitFailure _ -> PullFailed ("unpack " ++ key ++ ": " ++ xe)
    removeIfExists f = removeFile f `catchIOError` const (pure ())

-- | The remote cache the build should consult, from the @ZINC_CACHE@ env var (a
-- base URL); 'Nothing' when unset, so the build is unchanged by default
-- (opt-in). The trust model (private-only, hash-verify) is zinc-vwn.6.
remoteCacheFromEnv :: IO (Maybe CacheBackend)
remoteCacheFromEnv = do
  mUrl <- lookupEnv "ZINC_CACHE"
  pure $ case mUrl of
    Just url | not (null url) -> Just (httpBackend url)
    _                         -> Nothing
