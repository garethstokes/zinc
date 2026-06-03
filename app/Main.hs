module Main (main) where

import System.Environment (getArgs)
import Zinc.CLI (parseArgs)

-- | Thin executable shim. Parsing/dispatch logic lives in (and is tested via)
-- "Zinc.CLI"; command implementations arrive in later epics (build driver,
-- resolver, orchestration). For now, parse and report.
main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err  -> putStrLn err
    Right cmd -> putStrLn ("zinc: not yet implemented: " ++ show cmd)
