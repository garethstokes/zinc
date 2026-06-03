module Main (main) where

import System.Environment (getArgs)
import Zinc.Add (addInWorkspace)
import Zinc.CLI (Command (..), parseArgs)
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
dispatch cmd = putStrLn ("zinc: not yet implemented: " ++ show cmd)
