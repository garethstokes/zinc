-- | Provision and cache the dev environment (spec §6). Evaluating the Nix
-- flake is expensive, so the result is cached on disk keyed by the inputs
-- that affect it (GHC version + system libs) and only re-evaluated when those
-- change. The actual evaluator (e.g. @nix print-dev-env@) is injected, so the
-- caching logic is testable without invoking Nix.
module Zinc.Env
  ( envCacheKey
  , provisionEnv
  , nixPrintDevEnv
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Digest.Pure.SHA (sha256, showDigest)
import Data.List (intercalate, sort)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Zinc.Nix (generateFlake)

-- | A stable key over the inputs that determine the dev env. Order of system
-- libs is irrelevant; the GHC version matters.
envCacheKey :: String -> [String] -> String
envCacheKey ghcVersion systemLibs =
  showDigest (sha256 (BL8.pack (ghcVersion ++ "\0" ++ intercalate "," (sort systemLibs))))

-- | Return the dev-env data for the given toolchain inputs, evaluating via the
-- injected @eval@ only on a cache miss and persisting the result under
-- @cacheRoot@ for reuse.
provisionEnv
  :: (String -> [String] -> IO (Either String String)) -- ^ evaluator: ghc -> libs -> env text
  -> FilePath                                           -- ^ cache root
  -> String                                             -- ^ ghc version
  -> [String]                                           -- ^ system libs
  -> IO (Either String String)
provisionEnv eval cacheRoot ghcVersion systemLibs = do
  let file = cacheRoot </> ("env-" ++ envCacheKey ghcVersion systemLibs)
  hit <- doesFileExist file
  if hit
    then Right <$> readFile file
    else do
      result <- eval ghcVersion systemLibs
      case result of
        Left err -> pure (Left err)
        Right env -> do
          createDirectoryIfMissing True cacheRoot
          writeFile file env
          pure (Right env)

-- | The concrete evaluator (suitable as @provisionEnv@'s @eval@ argument,
-- partially applied to a work dir): generate the flake, git-track it (flakes
-- require tracked files), and run @nix print-dev-env@, returning its output.
nixPrintDevEnv :: FilePath -> String -> [String] -> IO (Either String String)
nixPrintDevEnv workDir ghcVersion systemLibs = do
  createDirectoryIfMissing True workDir
  writeFile (workDir </> "flake.nix") (generateFlake ghcVersion systemLibs)
  _ <- readProcessWithExitCode "git" ["-C", workDir, "init", "-q"] ""
  _ <- readProcessWithExitCode "git" ["-C", workDir, "add", "flake.nix"] ""
  (code, out, err) <-
    readProcessWithExitCode
      "nix"
      ["--extra-experimental-features", "nix-command flakes", "print-dev-env", workDir]
      ""
  pure $ case code of
    ExitSuccess   -> Right out
    ExitFailure _ -> Left (if null err then "nix print-dev-env failed" else err)
