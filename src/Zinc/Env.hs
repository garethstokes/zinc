-- | Provision and cache the dev environment (spec §6). Evaluating the Nix
-- flake is expensive, so the result is cached on disk keyed by the inputs
-- that affect it (GHC version + system libs) and only re-evaluated when those
-- change. The actual evaluator (e.g. @nix print-dev-env@) is injected, so the
-- caching logic is testable without invoking Nix.
module Zinc.Env
  ( envCacheKey
  , envCacheKeyFor
  , provisionEnv
  , provisionEnvFor
  , nixPrintDevEnv
  , nixPrintDevEnvFor
  , nixPrintDevEnvJson
  , nixPrintDevEnvJsonFor
  , devEnvVars
  , toolchainPath
  , toolchainVars
  , applyDevEnv
  , provisionToolchain
  , provisionToolchainFor
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
import Zinc.Nix (generateFlakeFor)
import Zinc.Target (Target (..), targetTriple)

-- | A stable key over the inputs that determine the dev env. Order of system
-- libs is irrelevant; the GHC version matters. (Native target; see
-- 'envCacheKeyFor'.)
envCacheKey :: String -> [String] -> String
envCacheKey = envCacheKeyFor Native

-- | As 'envCacheKey', but target-aware: a non-native target contributes its
-- triple so native + wasm dev-envs cache separately (zinc-9po.1). @Native@ adds
-- nothing, so its key is byte-identical to the historical 'envCacheKey'.
envCacheKeyFor :: Target -> String -> [String] -> String
envCacheKeyFor target ghcVersion systemLibs =
  showDigest (sha256 (BL8.pack (ghcVersion ++ "\0" ++ intercalate "," (sort systemLibs) ++ targetSuffix)))
  where
    targetSuffix = case target of
      Native -> ""
      _      -> "\0" ++ targetTriple target

-- | Return the dev-env data for the given toolchain inputs, evaluating via the
-- injected @eval@ only on a cache miss and persisting the result under
-- @cacheRoot@ for reuse.
provisionEnv
  :: (String -> [String] -> IO (Either String String)) -- ^ evaluator: ghc -> libs -> env text
  -> FilePath                                           -- ^ cache root
  -> String                                             -- ^ ghc version
  -> [String]                                           -- ^ system libs
  -> IO (Either ZincError String)
provisionEnv = provisionEnvFor Native

-- | As 'provisionEnv', but target-aware: the cache file is keyed by the target
-- too, so native + wasm envs never collide (zinc-9po.1).
provisionEnvFor
  :: Target
  -> (String -> [String] -> IO (Either String String))
  -> FilePath
  -> String
  -> [String]
  -> IO (Either ZincError String)
provisionEnvFor target eval cacheRoot ghcVersion systemLibs = runResult $ do
  let file = cacheRoot </> ("env-" ++ envCacheKeyFor target ghcVersion systemLibs)
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
nixPrintDevEnv = nixPrintDevEnvFor Native

-- | As 'nixPrintDevEnv', but target-aware: a wasm target writes the
-- @ghc-wasm-meta@ flake instead of the native one (zinc-9po.1).
nixPrintDevEnvFor :: Target -> FilePath -> String -> [String] -> IO (Either String String)
nixPrintDevEnvFor target workDir ghcVersion systemLibs =
  runPrintDevEnv ["print-dev-env", workDir] target workDir ghcVersion systemLibs

-- | As 'nixPrintDevEnv', but @--json@ so the variables can be parsed structurally
-- (rather than re-parsing bash). Used to provision the toolchain (zinc-y03).
nixPrintDevEnvJson :: FilePath -> String -> [String] -> IO (Either String String)
nixPrintDevEnvJson = nixPrintDevEnvJsonFor Native

-- | Target-aware variant of 'nixPrintDevEnvJson' (zinc-9po.1).
nixPrintDevEnvJsonFor :: Target -> FilePath -> String -> [String] -> IO (Either String String)
nixPrintDevEnvJsonFor target workDir ghcVersion systemLibs =
  runPrintDevEnv ["print-dev-env", "--json", workDir] target workDir ghcVersion systemLibs

-- Shared driver: write the target's flake, git-track it (flakes require tracked
-- files), and run the given `nix` subcommand over @workDir@.
runPrintDevEnv :: [String] -> Target -> FilePath -> String -> [String] -> IO (Either String String)
runPrintDevEnv nixArgs target workDir ghcVersion systemLibs = do
  createDirectoryIfMissing True workDir
  writeFile (workDir </> "flake.nix") (generateFlakeFor target ghcVersion systemLibs)
  _ <- readProcessWithExitCode "git" ["-C", workDir, "init", "-q"] ""
  _ <- readProcessWithExitCode "git" ["-C", workDir, "add", "flake.nix"] ""
  (code, out, err) <-
    readProcessWithExitCode "nix" (["--extra-experimental-features", "nix-command flakes"] ++ nixArgs) ""
  pure $ case code of
    ExitSuccess   -> Right out
    ExitFailure _ -> Left (if null err then "nix print-dev-env failed" else err)

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
provisionToolchain = provisionToolchainFor Native

-- | As 'provisionToolchain', but for an explicit 'Target' (zinc-9po.1): a wasm
-- target provisions the @ghc-wasm-meta@ toolchain, cached separately from
-- native. The native-skip guard (ambient ghc already right) applies only to
-- 'Native'; a non-native target always provisions its cross-toolchain (the
-- ambient host ghc is never the cross-compiler).
provisionToolchainFor :: Target -> FilePath -> FilePath -> String -> [String] -> IO ()
provisionToolchainFor target cacheRoot workDir ghcVersion systemLibs = do
  -- Skip only when the ghc ALREADY on PATH is the requested version (so dev/CI/
  -- self-host stay no-ops, but a `--ghc <other>` override forces a switch — ey4).
  -- A cross target can never be satisfied by the ambient host ghc.
  -- Skip only when the ambient ghc is right AND no extra C system libraries are
  -- needed. A project whose closure pulls in system libs (e.g. postgresql for
  -- postgresql-libpq, zinc-389) must still provision them — the ambient dev/CI
  -- shell has the compiler but not those per-project libs — so their
  -- NIX_CFLAGS/NIX_LDFLAGS (-I/-L) reach the build's compile + link.
  haveRight <- case target of
    Native | null systemLibs -> ambientGhcIs ghcVersion
    _                        -> pure False
  unless haveRight $ do
    nixPresent <- isJust <$> findExecutable "nix"
    when nixPresent $ do
      r <- provisionEnvFor target (nixPrintDevEnvJsonFor target workDir) cacheRoot ghcVersion systemLibs
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
