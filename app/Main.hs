module Main (main) where

import Control.Monad (unless)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (std_err, std_in, std_out), StdStream (Inherit), createProcess, proc, waitForProcess)
import Zinc.Add (addInWorkspace, updateInWorkspace, vendorInWorkspace)
import Zinc.CLI (Command (..), helpOverview, parseArgs)
import Zinc.Closure (closureReportJson, renderClosure, runClosure)
import Zinc.Diagnostic (ZincError, envelope, exitCodeFor, humanError, toDiagnostic, zincVersion, zincVersionLine)
import Zinc.Docker (runDockerfile)
import Zinc.Git (gitInitIfNeeded)
import Zinc.Doctor (doctorJson, doctorOk, renderDoctor, runDoctor)
import Zinc.Fmt (runFmt)
import Zinc.GC (runGc)
import Zinc.Introspect (explainJson, graphJson, renderExplain, renderGraph, renderStatus, runExplain, runGraph, runStatus, statusJson)
import Zinc.Json (Json (..), renderJson)
import Zinc.Metrics (recordBuild)
import Zinc.Orchestrate (checkLockDrift, resolveRunTarget, runBuildReport, runClean, runRepl, runTests, runWarm)
import Zinc.Output (OutputEvent (..), OutputMode (..), emit, resolveMode, withRenderer)
import Zinc.Perf (perfSummaryJson, renderPerf, runPerf)
import Zinc.Prime (runOnboard, runPrime)
import Zinc.Report (PackageReport, PackageStatus (Built, Cached), boExes, boPackages, buildDataJson, buildSummaryLine, packageReportJson, prName, prStatus, prTimeMs, timingJson)
import Zinc.Scaffold (materialize, scaffoldNew, scaffoldWorkspace)

-- | Thin executable shim: parse argv into the output flags + a 'Command',
-- resolve one 'OutputMode', and dispatch. Parsing lives in "Zinc.CLI".
main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err          -> putStrLn err
    Right (flags, cmd) -> resolveMode flags >>= \mode -> dispatch mode cmd

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
  hPutStrLn stderr (humanError (humanColor mode) (toDiagnostic e))
  exitWith (exitCodeFor e)

-- | Emit a read-only command's result: the JSON envelope in machine mode
-- (failures carry the diagnostic + category exit code), or human text.
emitIntrospection :: String -> OutputMode -> (a -> Json) -> (a -> String) -> Either ZincError a -> IO ()
emitIntrospection cmd mode toJson toHuman r = case r of
  Left e
    | machine mode -> putStrLn (renderJson (envelope cmd False Nothing Nothing [toDiagnostic e])) >> exitWith (exitCodeFor e)
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
dispatch mode (Add name) =
  addInWorkspace name >>= either (failCmd mode) putStr
dispatch mode (Vendor pkgs) =
  vendorInWorkspace pkgs >>= either (failCmd mode) putStr
dispatch mode (Build target) = do
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
    res <- runBuildReport sink "." target
    case res of
      Right (_, timing) -> emit sink (Finished (buildSummaryLine False timing))
      Left _            -> pure ()
    pure res
  case r of
    Left e
      | machine mode -> putStrLn (renderJson (envelope "build" False Nothing Nothing [toDiagnostic e])) >> exitWith (exitCodeFor e)
      | otherwise    -> failCmd mode e
    Right (outcome, timing) -> do
      recordBuild "." "build" target timing [(prName p, ms) | p <- boPackages outcome, Just ms <- [prTimeMs p]]
      if machine mode
        then putStrLn (renderJson (envelope "build" True (Just (buildDataJson outcome)) (Just (timingJson timing)) []))
        else do
          -- hw6.3 spectacle: the speed + cache summary headline, then the exes.
          putStrLn (buildSummaryLine (humanColor mode) timing)
          mapM_ (putStrLn . ("  " ++)) (boExes outcome)
dispatch mode (Run target args) =
  resolveRunTarget "." target >>= \r -> case r of
    Left e -> failCmd mode e
    Right exe -> do
      -- Exec with live, inherited stdio and exit zinc with the child's code.
      (_, _, _, ph) <- createProcess (proc exe args) {std_in = Inherit, std_out = Inherit, std_err = Inherit}
      waitForProcess ph >>= exitWith
dispatch mode (Test _) =
  runTests "." >>= \r -> case r of
    Left e  -> failCmd mode e
    Right n -> putStrLn (show n ++ " test suite(s) passed")
dispatch mode (Repl _) =
  runRepl "." >>= either (failCmd mode) (const (pure ()))
dispatch mode (Update _) =
  updateInWorkspace >>= either (failCmd mode) putStr
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
  runStatus "." >>= emitIntrospection "status" mode (\(g, m, d, dr) -> statusJson g m d dr) (\(g, m, d, dr) -> renderStatus g m d dr)
dispatch mode Graph =
  runGraph "." >>= emitIntrospection "graph" mode graphJson renderGraph
dispatch mode (Explain pkg) =
  runExplain "." >>= emitIntrospection "explain" mode (explainJson pkg) (renderExplain pkg)
dispatch mode Warm = do
  r <- withRenderer mode $ \sink -> do
    res <- runWarm sink "."
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
dispatch mode (Fmt check) =
  runFmt check "." >>= \r -> case r of
    Left e -> failCmd mode e
    Right clean
      | check && not clean -> hPutStrLn stderr "zinc.toml is not canonical (run `zinc fmt`)" >> exitWith (ExitFailure 1)
      | check              -> putStrLn "zinc.toml is canonical."
      | clean              -> putStrLn "zinc.toml is already canonical."
      | otherwise          -> putStrLn "Formatted zinc.toml."
