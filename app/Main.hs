module Main (main) where

import Control.Monad (unless)
import Data.List (intercalate)
import System.Environment (getArgs)
import Zinc.Add (addInWorkspace)
import Zinc.CLI (Command (..), parseArgs)
import Zinc.Orchestrate (buildAndRun, checkLockDrift, runBuildMember, runRepl, runTests)
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
  addInWorkspace name >>= either (\e -> putStrLn ("zinc add: " ++ e)) putStr
dispatch (Build target) = do
  drift <- checkLockDrift "."
  unless (null drift) $
    putStrLn ("warning: zinc.lock is missing: " ++ intercalate ", " drift ++ " (run `zinc add`)")
  runBuildMember "." target >>= \r -> case r of
    Left e -> putStrLn ("zinc build: " ++ e)
    Right exes -> do
      putStrLn ("Built " ++ show (length exes) ++ " executable(s):")
      mapM_ (putStrLn . ("  " ++)) exes
dispatch (Run args) =
  buildAndRun "." args >>= either (\e -> putStrLn ("zinc run: " ++ e)) putStr
dispatch (Test _) =
  runTests "." >>= \r -> case r of
    Left e  -> putStrLn ("zinc test: " ++ e)
    Right n -> putStrLn (show n ++ " test suite(s) passed")
dispatch (Repl _) =
  runRepl "." >>= either (\e -> putStrLn ("zinc repl: " ++ e)) (const (pure ()))
dispatch cmd = putStrLn ("zinc: not yet implemented: " ++ show cmd)
