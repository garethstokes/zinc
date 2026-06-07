module Main (main) where

import Control.Monad (unless, when)
import System.IO.Error (catchIOError)
import Data.List (intercalate, nub)
import Zinc.Lock (lockSystemLibs, parseLock)
import Data.Maybe (maybeToList)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (std_err, std_in, std_out), StdStream (Inherit), createProcess, proc, waitForProcess)
import Zinc.Add (addInWorkspace, updateInWorkspace, vendorInWorkspace)
import Zinc.CLI (Command (..), helpOverview, parseArgs)
import Zinc.Closure (closureReportJson, renderClosure, runClosure)
import Zinc.Diagnostic (ZincError, envelope, exitCodeFor, humanError, rawToolOutput, toDiagnostic, toDiagnostics, zincVersion, zincVersionLine)
import Zinc.Delta (deltaJson, renderDelta)
import Zinc.Deploy (ProbeChecks (..), ResolvedDeploy (..), deployReadyJson, dhHost, resolveDeploy, runDeploy, runInit)
import Zinc.Docker (runDockerfile)
import Zinc.Env (provisionToolchainFor)
import Zinc.Target (Target (Native), isWasm, parseTarget, targetTriple)
import Zinc.Git (gitInitIfNeeded)
import Zinc.Manifest (parseDeployTargets, parseWorkspace, wsGhc)
import Zinc.Store (resolveStoreRoot)
import Zinc.Doctor (doctorJson, doctorOk, renderDoctor, runDoctor)
import Zinc.Fmt (runFmt)
import Zinc.GC (runGc)
import Zinc.Introspect (explainJson, graphJson, renderExplain, renderGraph, renderStatus, runExplain, runGraph, runStatus, statusJson)
import Zinc.Json (Json (..), renderJson)
import Zinc.Metrics (recordBuild)
import Zinc.Orchestrate (checkLockDrift, resolveRunTargetFor, runBuildReport, runCachePush, runClean, runPackage, runRepl, runTests, runWarm)
import Zinc.Package (parsePackageFormat)
import Zinc.Skill (LockedSkill (..))
import Zinc.SkillCmd (renderSkillList, runSkillAdd, runSkillList, runSkillRemove, runSkillSync)
import Zinc.Outdated (outdatedJson, renderOutdated, runOutdated)
import Zinc.Output (OutputEvent (..), OutputMode (..), emit, resolveMode, withRenderer)
import Zinc.Perf (perfSummaryJson, renderPerf, runPerf)
import Zinc.Prime (runOnboard, runPrime)
import Zinc.Report (PackageReport, PackageStatus (Built, Cached), boExes, boPackages, buildBreakdownLine, buildDataJson, buildSummaryLine, packageReportJson, prName, prStatus, prTimeMs, renderResolution, resolutionJson, timingJson)
import Zinc.Scaffold (materialize, scaffoldNew, scaffoldWorkspace)

-- | Thin executable shim: parse argv into the output flags + a 'Command',
-- resolve one 'OutputMode', and dispatch. Parsing lives in "Zinc.CLI".
main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err          -> putStrLn err
    Right (flags, cmd) -> do
      mode <- resolveMode flags
      when (buildsToolchain cmd) (provisionToolchainHere (targetOf cmd) (ghcOverrideOf cmd))
      dispatch mode cmd

-- | Commands that shell out to the toolchain (ghc/ghc-pkg/ar/preprocessors) and
-- therefore want it provisioned (zinc-y03).
buildsToolchain :: Command -> Bool
buildsToolchain c = case c of
  Build {}  -> True
  Run {}    -> True
  Test _    -> True
  Repl _    -> True
  Warm _    -> True
  Package {} -> True
  _         -> False

-- | A command's explicit @--ghc@ override, if any (build/warm carry it; ey4).
ghcOverrideOf :: Command -> Maybe String
ghcOverrideOf (Build _ g _) = g
ghcOverrideOf (Warm g)      = g
ghcOverrideOf _             = Nothing

-- | A command's compile target (zinc-9po.3): the @--target@ on @build@, else
-- 'Native'. An unparseable value falls back to 'Native' here (the build
-- dispatch re-parses and reports the usage error), so provisioning still runs.
targetOf :: Command -> Target
targetOf (Build _ _ (Just t))  = either (const Native) id (parseTarget t)
targetOf (Run _ _ (Just t))    = either (const Native) id (parseTarget t)
targetOf _                     = Native

-- | Provision the current workspace's Nix toolchain into the process env so the
-- build runs without a manual @nix develop@ (zinc-y03). No-op when the requested
-- @ghc@ is already present; best-effort otherwise. The requested GHC is the
-- @--ghc@ override (ey4) when given, else the manifest's; system-libs are a
-- follow-up (the common case + self-host use none).
provisionToolchainHere :: Target -> Maybe String -> IO ()
provisionToolchainHere target ghcOverride = do
  storeRoot <- resolveStoreRoot
  manifest <- readFile "zinc.toml" `catchIOError` const (pure "")
  -- System libs the build needs (the C libraries closure deps FFI into, recorded
  -- at freeze): provision them into the toolchain env so GHC's linker gets their
  -- -L when emitting -l<lib> for a dependent — e.g. libpq for postgresql-libpq
  -- (zinc-389). Read from the lock alone (no fetch), deduped.
  lockSrc <- readFile "zinc.lock" `catchIOError` const (pure "")
  let systemLibs = nub (concatMap lockSystemLibs (either (const []) id (parseLock lockSrc)))
      ghc = maybe (either (const "9.6.5") wsGhc (parseWorkspace manifest)) id ghcOverride
      cacheRoot = storeRoot ++ "/devenv"
      -- Target-suffixed flake dir so the native + wasm toolchains don't clobber
      -- each other's flake/lock (zinc-9po.3).
      flakeDir = cacheRoot ++ "/flake" ++ (if target == Native then "" else "-" ++ targetTriple target)
  provisionToolchainFor target cacheRoot flakeDir ghc systemLibs

-- | True in @--json@ machine mode.
machine :: OutputMode -> Bool
machine Machine = True
machine _ = False

-- | Whether human color is on (false in machine mode / no-color / piped).
humanColor :: OutputMode -> Bool
humanColor (Human color _ _) = color
humanColor _ = False

-- | Report a failed command and exit with its category's stable code (spec §6).
-- The rich, caret-bearing human rendering (hw6.2) goes to stderr; structured
-- output stays on stdout. (Machine-mode failures use the JSON envelope, not
-- this path.)
failCmd :: OutputMode -> ZincError -> IO ()
failCmd mode e = do
  -- A batch of closure blockers (zinc-91n.1) prints one error block each, so the
  -- user sees every blocker to fix in one pass rather than one per re-run.
  mapM_ (hPutStrLn stderr . humanError (humanColor mode)) (toDiagnostics e)
  -- ZINC_VERBOSE: print the full, untruncated tool output (e.g. GHC's complete
  -- stderr) so a failure the concise caret view summarizes can be fully
  -- inspected — the package-id / module-not-found detail consumers need (rxa).
  verbose <- isVerbose
  case rawToolOutput e of
    Just raw | verbose -> hPutStrLn stderr ("\n--- full compiler output (ZINC_VERBOSE) ---\n" ++ raw)
    Just _             -> hPutStrLn stderr "   (set ZINC_VERBOSE=1 to see the full compiler output)"
    Nothing            -> pure ()
  exitWith (exitCodeFor e)

-- | Whether @ZINC_VERBOSE@ is set (any non-empty value) — the verbosity
-- passthrough that surfaces full tool output on failure (zinc-rxa).
isVerbose :: IO Bool
isVerbose = maybe False (not . null) <$> lookupEnv "ZINC_VERBOSE"

-- | Emit a read-only command's result: the JSON envelope in machine mode
-- (failures carry the diagnostic + category exit code), or human text.
emitIntrospection :: String -> OutputMode -> (a -> Json) -> (a -> String) -> Either ZincError a -> IO ()
emitIntrospection cmd mode toJson toHuman r = case r of
  Left e
    | machine mode -> putStrLn (renderJson (envelope cmd False Nothing Nothing (toDiagnostics e))) >> exitWith (exitCodeFor e)
    | otherwise    -> failCmd mode e
  Right a
    | machine mode -> putStrLn (renderJson (envelope cmd True (Just (toJson a)) Nothing []))
    | otherwise    -> putStr (toHuman a)

-- | One-line summary of a closure-only (@warm@) build.
warmSummary :: [PackageReport] -> String
warmSummary pkgs =
  "Warmed " ++ show (length pkgs) ++ " closure package(s): "
    ++ show (count Built) ++ " built, " ++ show (count Cached) ++ " cached."
  where
    count s = length (filter ((== s) . prStatus) pkgs)

dispatch :: OutputMode -> Command -> IO ()
dispatch _ (New name workspace) = do
  materialize "." (if workspace then scaffoldWorkspace name else scaffoldNew name)
  -- Initialise a git repo unless we're already inside one (zinc-6hf.3).
  -- Best-effort: a missing/again-failing git just leaves the files in place.
  gitInitIfNeeded "." >>= either (\e -> hPutStrLn stderr ("note: skipped git init (" ++ e ++ ")")) (const (pure ()))
  putStrLn $
    if workspace
      then "Created workspace member at ./packages/" ++ name
      else "Created project " ++ name ++ " — `zinc run` to build and run it."
dispatch mode (Add name True) =
  -- --dry-run (zinc-91n.6): preview the non-boot closure + per-member repo
  -- resolvability (what `add` would pull, and which deps need vendoring) without
  -- mutating zinc.toml/zinc.lock. Same discovery as `zinc closure`.
  runClosure name >>= emitIntrospection "add" mode closureReportJson renderClosure
dispatch mode (Add name False) = do
  -- Stream resolve/fetch progress during the multi-second closure walk + freeze
  -- (zinc-91n.5); the renderer drains before the final table/envelope is emitted.
  r <- withRenderer mode (\sink -> addInWorkspace sink name)
  emitIntrospection "add" mode resolutionJson renderResolution r
dispatch mode (Vendor pkgs) = do
  r <- withRenderer mode (\sink -> vendorInWorkspace sink pkgs)
  emitIntrospection "vendor" mode resolutionJson renderResolution r
dispatch mode (Build member ghcOverride targetStr) =
  -- Resolve the compile target (zinc-9po.3); an unknown --target is a usage
  -- error (exit 2), like an unknown package format.
  case maybe (Right Native) parseTarget targetStr of
    Left err -> hPutStrLn stderr err >> exitWith (ExitFailure 2)
    Right tgt -> buildWith tgt
  where
   buildWith tgt = do
    -- Human path shows the lock-drift hint up front; the machine envelope stays
    -- pure JSON. Both run the report-bearing build and persist a metrics record.
    unless (machine mode) $ do
      drift <- checkLockDrift "."
      unless (null drift) $
        putStrLn ("warning: zinc.lock is missing: " ++ intercalate ", " drift ++ " (run `zinc add`)")
    -- The renderer owns stdout for the live event stream; the final summary /
    -- envelope is emitted below, AFTER withRenderer drains and returns, so it
    -- lands last and never races the renderer thread.
    r <- withRenderer mode $ \sink -> do
      res <- runBuildReport sink tgt "." member ghcOverride
      case res of
        Right (_, timing) -> emit sink (Finished (buildSummaryLine False timing))
        Left _            -> pure ()
      pure res
    case r of
      Left e
        | machine mode -> putStrLn (renderJson (envelope "build" False Nothing Nothing [toDiagnostic e])) >> exitWith (exitCodeFor e)
        | otherwise    -> failCmd mode e
      Right (outcome, timing) -> do
        recordBuild "." "build" member timing [(prName p, ms) | p <- boPackages outcome, Just ms <- [prTimeMs p]]
        if machine mode
          then putStrLn (renderJson (envelope "build" True (Just (buildDataJson outcome)) (Just (timingJson timing)) []))
          else do
            -- hw6.3 spectacle: the speed + cache summary headline, then the
            -- optional finer per-phase breakdown (nti.3), then the exes.
            putStrLn (buildSummaryLine (humanColor mode) timing)
            mapM_ putStrLn (maybeToList (buildBreakdownLine (humanColor mode) timing))
            mapM_ (putStrLn . ("  " ++)) (boExes outcome)
dispatch mode (Run sel args targetStr) =
  -- Resolve the compile target (zinc-9po.4); an unknown --target is a usage error.
  case maybe (Right Native) parseTarget targetStr of
    Left err -> hPutStrLn stderr err >> exitWith (ExitFailure 2)
    Right tgt ->
      resolveRunTargetFor tgt "." sel >>= \r -> case r of
        Left e -> failCmd mode e
        Right exe -> do
          -- A wasm artifact is not directly executable — run it through the
          -- Nix-provided wasmtime (on PATH after provisioning); a native exe runs
          -- directly. Either way: live, inherited stdio + the child's exit code.
          let (prog, pargs) = if isWasm tgt then ("wasmtime", exe : args) else (exe, args)
          (_, _, _, ph) <- createProcess (proc prog pargs) {std_in = Inherit, std_out = Inherit, std_err = Inherit}
          waitForProcess ph >>= exitWith
dispatch mode (Test _) =
  runTests "." >>= \r -> case r of
    Left e  -> failCmd mode e
    Right n -> putStrLn (show n ++ " test suite(s) passed")
dispatch mode (Repl _) =
  runRepl "." >>= either (failCmd mode) (const (pure ()))
dispatch mode (Update mpkg dryRun) =
  updateInWorkspace mpkg dryRun >>= emitIntrospection "update" mode deltaJson (renderDelta dryRun)
dispatch _ Clean = do
  runClean "."
  putStrLn "Cleaned build artifacts (kept the store)."
dispatch mode Gc =
  runGc "." >>= \r -> case r of
    Left e -> failCmd mode e
    Right (pkgs, srcs) ->
      putStrLn ("Collected " ++ show (length pkgs) ++ " package(s) and " ++ show (length srcs) ++ " source(s) from the store.")
dispatch mode Perf =
  runPerf "." >>= \s ->
    if machine mode
      then putStrLn (renderJson (envelope "perf" True (Just (perfSummaryJson s)) Nothing []))
      else putStr (renderPerf s)
dispatch mode Doctor = do
  diags <- runDoctor "."
  if machine mode
    then putStrLn (renderJson (doctorJson diags))
    else putStr (renderDoctor diags)
  unless (doctorOk diags) (exitWith (ExitFailure 1))
dispatch mode Status =
  runStatus "." >>= emitIntrospection "status" mode (\(g, m, d, dr, sk) -> statusJson g m d dr sk) (\(g, m, d, dr, sk) -> renderStatus g m d dr sk)
dispatch mode Graph =
  runGraph "." >>= emitIntrospection "graph" mode graphJson renderGraph
dispatch mode (Explain pkg) =
  runExplain "." >>= emitIntrospection "explain" mode (explainJson pkg) (renderExplain pkg)
dispatch mode (Warm ghcOverride) = do
  r <- withRenderer mode $ \sink -> do
    res <- runWarm sink "." ghcOverride
    case res of
      Right pkgs -> emit sink (Finished (warmSummary pkgs))
      Left _     -> pure ()
    pure res
  case r of
    Left e
      | machine mode -> putStrLn (renderJson (envelope "warm" False Nothing Nothing [toDiagnostic e])) >> exitWith (exitCodeFor e)
      | otherwise    -> failCmd mode e
    Right pkgs
      | machine mode -> putStrLn (renderJson (envelope "warm" True (Just (JObject [("packages", JArray (map packageReportJson pkgs))])) Nothing []))
      | otherwise    -> putStrLn (warmSummary pkgs)
dispatch mode Prime =
  runPrime "." >>= either (failCmd mode) putStr
dispatch mode Onboard =
  runOnboard "." >>= either (failCmd mode) putStr
dispatch mode Dockerfile =
  runDockerfile "." >>= either (failCmd mode) putStr
dispatch mode (Closure pkg) =
  runClosure pkg >>= emitIntrospection "closure" mode closureReportJson renderClosure
dispatch mode Version
  | machine mode = putStrLn (renderJson (envelope "version" True (Just (JObject [("version", JString zincVersion)])) Nothing []))
  | otherwise    = putStrLn zincVersionLine
dispatch _ Help = putStr helpOverview
dispatch mode (Package fmtStr tag out to) =
  case parsePackageFormat fmtStr of
    Left err  -> hPutStrLn stderr err >> exitWith (ExitFailure 2)
    Right fmt -> runPackage fmt tag out to "." >>= either (failCmd mode) putStrLn
dispatch mode (Deploy arg service initFlag _rollback _dryRun) = do
  -- nbk.6: resolve <arg> against the manifest's [deploy.*] targets (an ad-hoc
  -- user@host still works); a broken/absent manifest just means no named
  -- targets. nbk.5: --init prints the NixOS trusted-users + linger snippet for
  -- the deploy user. Otherwise nbk.1: probe the host's NixOS preconditions over
  -- SSH and report readiness (each gap → a typed ZINC_DEPLOY_* diagnostic). The
  -- closure copy + activate (nbk.2/.3) and --rollback (nbk.4) layer on top.
  src <- readFile "zinc.toml" `catchIOError` const (pure "")
  let targets = either (const []) id (parseDeployTargets src)
      h = rdHost (resolveDeploy targets arg service)
  if initFlag
    then runInit h >>= \r -> case r of
      Left e -> failDeploy e
      Right snippet
        | machine mode -> putStrLn (renderJson (envelope "deploy" True (Just (JObject [("init", JString snippet)])) Nothing []))
        | otherwise    -> putStrLn "Add this to the host's NixOS configuration, then rebuild:" >> putStr snippet
    else runDeploy h >>= \r -> case r of
      Left e -> failDeploy e
      Right (rh, c)
        | machine mode -> putStrLn (renderJson (envelope "deploy" True (Just (deployReadyJson rh c)) Nothing []))
        | otherwise    -> putStrLn ("Host " ++ dhHost rh ++ " is ready to receive a deploy (nix " ++ tick (pcNix c) ++ ", trusted " ++ tick (pcTrusted c) ++ ", linger " ++ tick (pcLinger c) ++ ").")
  where
    tick b = if b then "\10003" else "\10007"
    failDeploy e
      | machine mode = putStrLn (renderJson (envelope "deploy" False Nothing Nothing [toDiagnostic e])) >> exitWith (exitCodeFor e)
      | otherwise    = failCmd mode e
dispatch mode (SkillAdd repo ref) =
  runSkillAdd repo ref "." >>= either (failCmd mode) putStrLn
dispatch mode SkillList =
  runSkillList "." >>= \r -> case r of
    Left e -> failCmd mode e
    Right sks
      | machine mode -> putStrLn (renderJson (envelope "skill-list" True (Just (JArray (map skillJson sks))) Nothing []))
      | otherwise    -> putStr (renderSkillList sks)
  where
    skillJson s = JObject [("name", JString (lskName s)), ("repo", JString (lskRepo s)), ("rev", JString (lskRev s)), ("sha256", JString (lskSha256 s))]
dispatch mode (SkillRemove name) =
  runSkillRemove name "." >>= either (failCmd mode) putStrLn
dispatch mode SkillSync =
  runSkillSync "." >>= \r -> case r of
    Left e -> failCmd mode e
    Right names
      | machine mode -> putStrLn (renderJson (envelope "skill-sync" True (Just (JObject [("synced", JArray (map JString names))])) Nothing []))
      | otherwise    -> putStrLn ("Synced " ++ show (length names) ++ " skill(s)" ++ if null names then "." else ": " ++ intercalate ", " names ++ ".")
dispatch mode CachePush =
  runCachePush "." >>= \r -> case r of
    Left e -> failCmd mode e
    Right pushed
      | machine mode -> putStrLn (renderJson (envelope "cache-push" True (Just (JObject [("pushed", JArray (map JString pushed))])) Nothing []))
      | otherwise    -> putStrLn ("Pushed " ++ show (length pushed) ++ " artifact(s) to the cache" ++ if null pushed then " (nothing built locally)." else ": " ++ intercalate ", " pushed ++ ".")
dispatch mode (Outdated allClosure) =
  runOutdated allClosure "." >>= emitIntrospection "outdated" mode outdatedJson renderOutdated
dispatch mode (Fmt check) =
  runFmt check "." >>= \r -> case r of
    Left e -> failCmd mode e
    Right clean
      | check && not clean -> hPutStrLn stderr "zinc.toml is not canonical (run `zinc fmt`)" >> exitWith (ExitFailure 1)
      | check              -> putStrLn "zinc.toml is canonical."
      | clean              -> putStrLn "zinc.toml is already canonical."
      | otherwise          -> putStrLn "Formatted zinc.toml."
