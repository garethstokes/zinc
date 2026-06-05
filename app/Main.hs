module Main (main) where

import Control.Monad (unless)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (std_err, std_in, std_out), StdStream (Inherit), createProcess, proc, waitForProcess)
import Zinc.Add (addInWorkspace, updateInWorkspace)
import Zinc.CLI (Command (..), parseArgs)
import Zinc.Closure (closureReportJson, renderClosure, runClosure)
import Zinc.Diagnostic (ZincError, envelope, exitCodeFor, renderError, toDiagnostic)
import Zinc.Docker (runDockerfile)
import Zinc.Doctor (doctorJson, doctorOk, renderDoctor, runDoctor)
import Zinc.Fmt (runFmt)
import Zinc.GC (runGc)
import Zinc.Introspect (explainJson, graphJson, renderExplain, renderGraph, renderStatus, runExplain, runGraph, runStatus, statusJson)
import Zinc.Json (Json (..), renderJson)
import Zinc.Metrics (recordBuild)
import Zinc.Orchestrate (checkLockDrift, resolveRunTarget, runBuildReport, runClean, runRepl, runTests, runWarm)
import Zinc.Output (OutputMode (..), resolveMode)
import Zinc.Perf (perfSummaryJson, renderPerf, runPerf)
import Zinc.Prime (runOnboard, runPrime)
import Zinc.Report (PackageReport, PackageStatus (Built, Cached), boExes, boPackages, buildDataJson, packageReportJson, prName, prStatus, prTimeMs, timingJson)
import Zinc.Scaffold (materialize, scaffoldNew)

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

-- | Report a failed command and exit with its category's stable code (spec §6).
-- The error goes to stderr; structured output stays on stdout.
failCmd :: String -> ZincError -> IO ()
failCmd cmd e = do
  hPutStrLn stderr (cmd ++ ": " ++ renderError e)
  exitWith (exitCodeFor e)

-- | Emit a read-only command's result: the JSON envelope in machine mode
-- (failures carry the diagnostic + category exit code), or human text.
emitIntrospection :: String -> OutputMode -> (a -> Json) -> (a -> String) -> Either ZincError a -> IO ()
emitIntrospection cmd mode toJson toHuman r = case r of
  Left e
    | machine mode -> putStrLn (renderJson (envelope cmd False Nothing Nothing [toDiagnostic e])) >> exitWith (exitCodeFor e)
    | otherwise    -> failCmd ("zinc " ++ cmd) e
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
dispatch _ (New name) = do
  materialize "." (scaffoldNew name)
  putStrLn ("Created workspace member at ./packages/" ++ name)
dispatch _ (Add name) =
  addInWorkspace name >>= either (failCmd "zinc add") putStr
dispatch mode (Build target) = do
  -- Human path shows the lock-drift hint up front; the machine envelope stays
  -- pure JSON. Both run the report-bearing build and persist a metrics record.
  unless (machine mode) $ do
    drift <- checkLockDrift "."
    unless (null drift) $
      putStrLn ("warning: zinc.lock is missing: " ++ intercalate ", " drift ++ " (run `zinc add`)")
  runBuildReport "." target >>= \r -> case r of
    Left e
      | machine mode -> putStrLn (renderJson (envelope "build" False Nothing Nothing [toDiagnostic e])) >> exitWith (exitCodeFor e)
      | otherwise    -> failCmd "zinc build" e
    Right (outcome, timing) -> do
      recordBuild "." "build" target timing [(prName p, ms) | p <- boPackages outcome, Just ms <- [prTimeMs p]]
      if machine mode
        then putStrLn (renderJson (envelope "build" True (Just (buildDataJson outcome)) (Just (timingJson timing)) []))
        else do
          putStrLn ("Built " ++ show (length (boExes outcome)) ++ " executable(s):")
          mapM_ (putStrLn . ("  " ++)) (boExes outcome)
dispatch _ (Run target args) =
  resolveRunTarget "." target >>= \r -> case r of
    Left e -> failCmd "zinc run" e
    Right exe -> do
      -- Exec with live, inherited stdio and exit zinc with the child's code.
      (_, _, _, ph) <- createProcess (proc exe args) {std_in = Inherit, std_out = Inherit, std_err = Inherit}
      waitForProcess ph >>= exitWith
dispatch _ (Test _) =
  runTests "." >>= \r -> case r of
    Left e  -> failCmd "zinc test" e
    Right n -> putStrLn (show n ++ " test suite(s) passed")
dispatch _ (Repl _) =
  runRepl "." >>= either (failCmd "zinc repl") (const (pure ()))
dispatch _ (Update _) =
  updateInWorkspace >>= either (failCmd "zinc update") putStr
dispatch _ Clean = do
  runClean "."
  putStrLn "Cleaned build artifacts (kept the store)."
dispatch _ Gc =
  runGc "." >>= \r -> case r of
    Left e -> failCmd "zinc gc" e
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
dispatch mode Warm =
  runWarm "." >>= \r -> case r of
    Left e
      | machine mode -> putStrLn (renderJson (envelope "warm" False Nothing Nothing [toDiagnostic e])) >> exitWith (exitCodeFor e)
      | otherwise    -> failCmd "zinc warm" e
    Right pkgs
      | machine mode -> putStrLn (renderJson (envelope "warm" True (Just (JObject [("packages", JArray (map packageReportJson pkgs))])) Nothing []))
      | otherwise    -> putStrLn (warmSummary pkgs)
dispatch _ Prime =
  runPrime "." >>= either (failCmd "zinc prime") putStr
dispatch _ Onboard =
  runOnboard "." >>= either (failCmd "zinc onboard") putStr
dispatch _ Dockerfile =
  runDockerfile "." >>= either (failCmd "zinc dockerfile") putStr
dispatch mode (Closure pkg) =
  runClosure pkg >>= emitIntrospection "closure" mode closureReportJson renderClosure
dispatch _ (Fmt check) =
  runFmt check "." >>= \r -> case r of
    Left e -> failCmd "zinc fmt" e
    Right clean
      | check && not clean -> hPutStrLn stderr "zinc.toml is not canonical (run `zinc fmt`)" >> exitWith (ExitFailure 1)
      | check              -> putStrLn "zinc.toml is canonical."
      | clean              -> putStrLn "zinc.toml is already canonical."
      | otherwise          -> putStrLn "Formatted zinc.toml."
