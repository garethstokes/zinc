module Main (main) where

import Control.Monad (unless)
import Data.List (intercalate)
import System.Environment (getArgs)
import Zinc.Add (addInWorkspace, updateInWorkspace)
import Zinc.CLI (Command (..), parseArgs)
import Zinc.Diagnostic (renderError)
import Zinc.GC (runGc)
import Zinc.Orchestrate (buildAndRun, checkLockDrift, runBuildMember, runClean, runRepl, runTests)
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

dispatch :: Command -> IO ()
dispatch (New name) = do
  materialize "." (scaffoldNew name)
  putStrLn ("Created workspace member at ./packages/" ++ name)
dispatch (Add name) =
  addInWorkspace name >>= either (\e -> putStrLn ("zinc add: " ++ renderError e)) putStr
dispatch (Build target) = do
  drift <- checkLockDrift "."
  unless (null drift) $
    putStrLn ("warning: zinc.lock is missing: " ++ intercalate ", " drift ++ " (run `zinc add`)")
  runBuildMember "." target >>= \r -> case r of
    Left e -> putStrLn ("zinc build: " ++ renderError e)
    Right exes -> do
      putStrLn ("Built " ++ show (length exes) ++ " executable(s):")
      mapM_ (putStrLn . ("  " ++)) exes
dispatch (Run args) =
  buildAndRun "." args >>= either (\e -> putStrLn ("zinc run: " ++ renderError e)) putStr
dispatch (Test _) =
  runTests "." >>= \r -> case r of
    Left e  -> putStrLn ("zinc test: " ++ renderError e)
    Right n -> putStrLn (show n ++ " test suite(s) passed")
dispatch (Repl _) =
  runRepl "." >>= either (\e -> putStrLn ("zinc repl: " ++ renderError e)) (const (pure ()))
dispatch (Update _) =
  updateInWorkspace >>= either (\e -> putStrLn ("zinc update: " ++ renderError e)) putStr
dispatch Clean = do
  runClean "."
  putStrLn "Cleaned build artifacts (kept the store)."
dispatch Gc =
  runGc "." >>= \r -> case r of
    Left e -> putStrLn ("zinc gc: " ++ renderError e)
    Right (pkgs, srcs) ->
      putStrLn ("Collected " ++ show (length pkgs) ++ " package(s) and " ++ show (length srcs) ++ " source(s) from the store.")
