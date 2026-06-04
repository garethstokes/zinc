module Zinc.CLI
  ( Command (..)
  , parseArgs
  ) where

import Options.Applicative

-- | A parsed zinc command. Mirrors the CLI surface in the design spec (§10).
data Command
  = New String            -- ^ scaffold a new workspace
  | Add String            -- ^ resolve a dependency closure and freeze it
  | Build (Maybe String)  -- ^ build the workspace, or a single member
  | Run [String]          -- ^ build then run an executable, passing through args
  | Repl (Maybe String)   -- ^ ghci for an optional target
  | Test (Maybe String)   -- ^ build and run tests for an optional target
  | Update (Maybe String) -- ^ bump refs for an optional package
  | Clean                 -- ^ remove build artifacts
  | Gc                    -- ^ garbage-collect the shared store
  deriving (Eq, Show)

-- | Pure, testable entry point: parse argv into a 'Command'.
-- Uses 'execParserPure' so it never touches IO or calls @exitFailure@,
-- unlike 'execParser'.
parseArgs :: [String] -> Either String Command
parseArgs args =
  case execParserPure defaultPrefs opts args of
    Success cmd          -> Right cmd
    Failure failure      -> Left (fst (renderFailure failure "zinc"))
    CompletionInvoked _  -> Left "completion invoked"
  where
    opts =
      info
        (commandParser <**> helper)
        (fullDesc <> progDesc "A git-native, Nix-assisted build tool for Haskell")

commandParser :: Parser Command
commandParser =
  subparser $
    mconcat
      [ sub "new"    "Scaffold a new workspace"       (New <$> strArgument (metavar "NAME"))
      , sub "add"    "Add a dependency"               (Add <$> strArgument (metavar "PKG"))
      , sub "build"  "Build the workspace or a member" (Build <$> optional (strArgument (metavar "MEMBER")))
      , sub "run"    "Build then run an executable"   (Run <$> many (strArgument (metavar "ARGS")))
      , sub "repl"   "Open ghci for a target"         (Repl <$> optional (strArgument (metavar "TARGET")))
      , sub "test"   "Build and run tests"            (Test <$> optional (strArgument (metavar "TARGET")))
      , sub "update" "Bump dependency refs to latest" (Update <$> optional (strArgument (metavar "PKG")))
      , sub "clean"  "Remove build artifacts"         (pure Clean)
      , sub "gc"     "Garbage-collect the shared store" (pure Gc)
      ]
  where
    sub name desc p = command name (info (p <**> helper) (progDesc desc))
