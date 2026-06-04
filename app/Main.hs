module Main (main) where

import Control.Monad (unless)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.IO (hPutStrLn, stderr)
import Zinc.Add (addInWorkspace, updateInWorkspace)
import Zinc.CLI (Command (..), parseArgs)
import Zinc.Diagnostic (ZincError, envelope, exitCodeFor, renderError, toDiagnostic)
import Zinc.GC (runGc)
import Zinc.Json (renderJson)
import Zinc.Metrics (recordBuild)
import Zinc.Orchestrate (buildAndRun, checkLockDrift, runBuildReport, runClean, runRepl, runTests)
import Zinc.Report (boExes, buildDataJson, timingJson)
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
      recordBuild "." "build" target timing
      if json
        then putStrLn (renderJson (envelope "build" True (Just (buildDataJson outcome)) (Just (timingJson timing)) []))
        else do
          putStrLn ("Built " ++ show (length (boExes outcome)) ++ " executable(s):")
          mapM_ (putStrLn . ("  " ++)) (boExes outcome)
dispatch (Run args) =
  buildAndRun "." args >>= either (failCmd "zinc run") putStr
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
