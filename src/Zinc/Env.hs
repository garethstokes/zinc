-- | Provision and cache the dev environment (spec §6). Evaluating the Nix
-- flake is expensive, so the result is cached on disk keyed by the inputs
-- that affect it (GHC version + system libs) and only re-evaluated when those
-- change. The actual evaluator (e.g. @nix print-dev-env@) is injected, so the
-- caching logic is testable without invoking Nix.
module Zinc.Env
  ( envCacheKey
  , provisionEnv
  , nixPrintDevEnv
  , nixPrintDevEnvJson
  , devEnvVars
  , toolchainPath
  , toolchainVars
  , applyDevEnv
  , provisionToolchain
  ) where

import Control.Monad (forM_, unless, when)
import Data.Char (isSpace)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Digest.Pure.SHA (sha256, showDigest)
import Data.List (intercalate, sort)
import Data.Maybe (fromMaybe, isJust)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable)
import System.Environment (lookupEnv, setEnv)
import Zinc.Diagnostic (ZincError)
import Zinc.Except (liftIO, orFail, runResult)
import Zinc.Json (Json (..), parseJson)
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
  -> IO (Either ZincError String)
provisionEnv eval cacheRoot ghcVersion systemLibs = runResult $ do
  let file = cacheRoot </> ("env-" ++ envCacheKey ghcVersion systemLibs)
  hit <- liftIO (doesFileExist file)
  if hit
    then liftIO (readFile file)
    else do
      env <- orFail (eval ghcVersion systemLibs)
      liftIO $ do
        createDirectoryIfMissing True cacheRoot
        writeFile file env
      pure env

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

-- | As 'nixPrintDevEnv', but @--json@ so the variables can be parsed structurally
-- (rather than re-parsing bash). Used to provision the toolchain (zinc-y03).
nixPrintDevEnvJson :: FilePath -> String -> [String] -> IO (Either String String)
nixPrintDevEnvJson workDir ghcVersion systemLibs = do
  createDirectoryIfMissing True workDir
  writeFile (workDir </> "flake.nix") (generateFlake ghcVersion systemLibs)
  _ <- readProcessWithExitCode "git" ["-C", workDir, "init", "-q"] ""
  _ <- readProcessWithExitCode "git" ["-C", workDir, "add", "flake.nix"] ""
  (code, out, err) <-
    readProcessWithExitCode
      "nix"
      ["--extra-experimental-features", "nix-command flakes", "print-dev-env", "--json", workDir]
      ""
  pure $ case code of
    ExitSuccess   -> Right out
    ExitFailure _ -> Left (if null err then "nix print-dev-env --json failed" else err)

-- | The EXPORTED @(name, value)@ variables from @nix print-dev-env --json@
-- output. Pure (testable without Nix). Non-exported (@type: var@) and
-- bash functions are ignored.
devEnvVars :: String -> [(String, String)]
devEnvVars json = case parseJson json of
  Right (JObject top)
    | Just (JObject vars) <- lookup "variables" top ->
        [ (name, val)
        | (name, JObject spec) <- vars
        , Just (JString "exported") <- [lookup "type" spec]
        , Just (JString val) <- [lookup "value" spec]
        ]
  _ -> []

-- | The build-relevant variables zinc applies from a provisioned dev env. We
-- deliberately apply a WHITELIST, never the whole set — the dev env carries
-- sandbox values (@HOME=\/homeless-shelter@, @TMPDIR@, …) that would break a
-- real build. @PATH@ is handled separately ('toolchainPath').
toolchainVars :: [String]
toolchainVars = ["NIX_CFLAGS_COMPILE", "NIX_LDFLAGS", "PKG_CONFIG_PATH"]

-- | The PATH to use after provisioning: the dev env's @PATH@ PREPENDED to the
-- ambient one, so the dev toolchain (ghc/alex/happy/…) wins while the user's own
-- tools (curl, git, …) remain reachable. 'Nothing' when the dev env has no PATH.
toolchainPath :: [(String, String)] -> String -> Maybe String
toolchainPath vars ambient = case lookup "PATH" vars of
  Just dev -> Just (dev ++ if null ambient then "" else ":" ++ ambient)
  Nothing  -> Nothing

-- | Apply a provisioned dev env to the current process: prepend its PATH and set
-- the whitelisted build vars, so every subsequent toolchain shell-out inherits
-- it (the process-env-once model, zinc-y03). Never sets sandbox-only vars.
applyDevEnv :: [(String, String)] -> IO ()
applyDevEnv vars = do
  ambient <- fromMaybe "" <$> lookupEnv "PATH"
  forM_ (toolchainPath vars ambient) (setEnv "PATH")
  forM_ toolchainVars $ \k -> forM_ (lookup k vars) (setEnv k)

-- | Provision the workspace's Nix toolchain into the process env so a build runs
-- without a manual @nix develop@ (zinc-y03). A strict no-op when @ghc@ is
-- already on PATH (the user is already in a provisioned shell — dev/CI/self-host
-- stay exactly as they were), and best-effort otherwise: if @nix@ is absent the
-- detect-and-guide preflight (gtv.2) handles it, and any eval failure leaves the
-- ambient env untouched. The dev env is cached under @cacheRoot@ keyed by
-- @ghc@ + @system-libs@, so re-provisioning is free.
provisionToolchain :: FilePath -> FilePath -> String -> [String] -> IO ()
provisionToolchain cacheRoot workDir ghcVersion systemLibs = do
  -- Skip only when the ghc ALREADY on PATH is the requested version (so dev/CI/
  -- self-host stay no-ops, but a `--ghc <other>` override forces a switch — ey4).
  haveRight <- ambientGhcIs ghcVersion
  unless haveRight $ do
    nixPresent <- isJust <$> findExecutable "nix"
    when nixPresent $ do
      r <- provisionEnv (nixPrintDevEnvJson workDir) cacheRoot ghcVersion systemLibs
      case r of
        Right json -> applyDevEnv (devEnvVars json)
        Left _     -> pure () -- best-effort; gtv.2 preflight guides on a hard miss

-- | Whether the @ghc@ currently on PATH already IS @want@ (its
-- @--numeric-version@), so toolchain provisioning can be skipped.
ambientGhcIs :: String -> IO Bool
ambientGhcIs want = do
  present <- isJust <$> findExecutable "ghc"
  if not present
    then pure False
    else do
      (code, out, _) <- readProcessWithExitCode "ghc" ["--numeric-version"] ""
      pure (code == ExitSuccess && filter (not . isSpace) out == want)
