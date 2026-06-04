module Main (main) where

import Control.Monad (unless)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (std_err, std_in, std_out), StdStream (Inherit), createProcess, proc, waitForProcess)
import Zinc.Add (addInWorkspace, updateInWorkspace)
import Zinc.CLI (Command (..), parseArgs)
import Zinc.Diagnostic (ZincError, envelope, exitCodeFor, renderError, toDiagnostic)
import Zinc.Docker (runDockerfile)
import Zinc.Doctor (doctorJson, doctorOk, renderDoctor, runDoctor)
import Zinc.GC (runGc)
import Zinc.Introspect (explainJson, graphJson, renderExplain, renderGraph, renderStatus, runExplain, runGraph, runStatus, statusJson)
import Zinc.Json (Json (..), renderJson)
import Zinc.Metrics (recordBuild)
import Zinc.Orchestrate (checkLockDrift, resolveRunTarget, runBuildReport, runClean, runRepl, runTests, runWarm)
import Zinc.Perf (perfSummaryJson, renderPerf, runPerf)
import Zinc.Prime (runOnboard, runPrime)
import Zinc.Report (PackageReport, PackageStatus (Built, Cached), boExes, boPackages, buildDataJson, packageReportJson, prName, prStatus, prTimeMs, timingJson)
import Zinc.Scaffold (materialize, scaffoldNew)

-- | Thin executable shim. Parsing/dispatch logic lives in (and is tested via)
-- "Zinc.CLI" and "Zinc.Scaffold". Most command implementations arrive in later
-- epics (build driver, resolver, orchestration); `new` is wired up now.
main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err  -> putStrLn err
    Right cmd -> dispatch cmd

-- | Report a failed command and exit with its category's stable code (spec §6),
-- so an agent can branch on the exit status without parsing the message. The
-- error goes to stderr; structured output (later: --json) stays on stdout.
failCmd :: String -> ZincError -> IO ()
failCmd cmd e = do
  hPutStrLn stderr (cmd ++ ": " ++ renderError e)
  exitWith (exitCodeFor e)

-- | Emit a read-only command's result: the JSON envelope under --json (failures
-- carry the diagnostic + category exit code), or human text otherwise.
emitIntrospection :: String -> Bool -> (a -> Json) -> (a -> String) -> Either ZincError a -> IO ()
emitIntrospection cmd json toJson toHuman r = case r of
  Left e
    | json      -> putStrLn (renderJson (envelope cmd False Nothing Nothing [toDiagnostic e])) >> exitWith (exitCodeFor e)
    | otherwise -> failCmd ("zinc " ++ cmd) e
  Right a
    | json      -> putStrLn (renderJson (envelope cmd True (Just (toJson a)) Nothing []))
    | otherwise -> putStr (toHuman a)

-- | One-line summary of a closure-only (@warm@) build.
warmSummary :: [PackageReport] -> String
warmSummary pkgs =
  "Warmed " ++ show (length pkgs) ++ " closure package(s): "
    ++ show (count Built) ++ " built, " ++ show (count Cached) ++ " cached."
  where
    count s = length (filter ((== s) . prStatus) pkgs)

dispatch :: Command -> IO ()
dispatch (New name) = do
  materialize "." (scaffoldNew name)
  putStrLn ("Created workspace member at ./packages/" ++ name)
dispatch (Add name) =
  addInWorkspace name >>= either (failCmd "zinc add") putStr
dispatch (Build target json) = do
  -- Human path shows the lock-drift hint up front; the machine envelope stays
  -- pure JSON. Both paths run the report-bearing build and persist a metrics
  -- record (perf spec §3.1) on success.
  unless json $ do
    drift <- checkLockDrift "."
    unless (null drift) $
      putStrLn ("warning: zinc.lock is missing: " ++ intercalate ", " drift ++ " (run `zinc add`)")
  runBuildReport "." target >>= \r -> case r of
    Left e
      | json -> do
          putStrLn (renderJson (envelope "build" False Nothing Nothing [toDiagnostic e]))
          exitWith (exitCodeFor e)
      | otherwise -> failCmd "zinc build" e
    Right (outcome, timing) -> do
      recordBuild "." "build" target timing [(prName p, ms) | p <- boPackages outcome, Just ms <- [prTimeMs p]]
      if json
        then putStrLn (renderJson (envelope "build" True (Just (buildDataJson outcome)) (Just (timingJson timing)) []))
        else do
          putStrLn ("Built " ++ show (length (boExes outcome)) ++ " executable(s):")
          mapM_ (putStrLn . ("  " ++)) (boExes outcome)
dispatch (Run target args) =
  resolveRunTarget "." target >>= \r -> case r of
    Left e -> failCmd "zinc run" e
    Right exe -> do
      -- Exec the chosen program with live, inherited stdio (interactive, TTY,
      -- colors, real stdin) and exit zinc with the child's exit code.
      (_, _, _, ph) <- createProcess (proc exe args) {std_in = Inherit, std_out = Inherit, std_err = Inherit}
      waitForProcess ph >>= exitWith
dispatch (Test _) =
  runTests "." >>= \r -> case r of
    Left e  -> failCmd "zinc test" e
    Right n -> putStrLn (show n ++ " test suite(s) passed")
dispatch (Repl _) =
  runRepl "." >>= either (failCmd "zinc repl") (const (pure ()))
dispatch (Update _) =
  updateInWorkspace >>= either (failCmd "zinc update") putStr
dispatch Clean = do
  runClean "."
  putStrLn "Cleaned build artifacts (kept the store)."
dispatch Gc =
  runGc "." >>= \r -> case r of
    Left e -> failCmd "zinc gc" e
    Right (pkgs, srcs) ->
      putStrLn ("Collected " ++ show (length pkgs) ++ " package(s) and " ++ show (length srcs) ++ " source(s) from the store.")
dispatch (Perf json) =
  runPerf "." >>= \s ->
    if json
      then putStrLn (renderJson (envelope "perf" True (Just (perfSummaryJson s)) Nothing []))
      else putStr (renderPerf s)
dispatch (Doctor json) = do
  diags <- runDoctor "."
  if json
    then putStrLn (renderJson (doctorJson diags))
    else putStr (renderDoctor diags)
  -- Exit non-zero on an error-severity finding so agents/CI can gate on health.
  unless (doctorOk diags) (exitWith (ExitFailure 1))
dispatch (Status json) =
  runStatus "." >>= emitIntrospection "status" json (\(g, m, d, dr) -> statusJson g m d dr) (\(g, m, d, dr) -> renderStatus g m d dr)
dispatch (Graph json) =
  runGraph "." >>= emitIntrospection "graph" json graphJson renderGraph
dispatch (Explain pkg json) =
  runExplain "." >>= emitIntrospection "explain" json (explainJson pkg) (renderExplain pkg)
dispatch (Warm json) =
  runWarm "." >>= \r -> case r of
    Left e
      | json -> putStrLn (renderJson (envelope "warm" False Nothing Nothing [toDiagnostic e])) >> exitWith (exitCodeFor e)
      | otherwise -> failCmd "zinc warm" e
    Right pkgs
      | json -> putStrLn (renderJson (envelope "warm" True (Just (JObject [("packages", JArray (map packageReportJson pkgs))])) Nothing []))
      | otherwise -> putStrLn (warmSummary pkgs)
dispatch Prime =
  runPrime "." >>= either (failCmd "zinc prime") putStr
dispatch Onboard =
  runOnboard "." >>= either (failCmd "zinc onboard") putStr
dispatch Dockerfile =
  runDockerfile "." >>= either (failCmd "zinc dockerfile") putStr
