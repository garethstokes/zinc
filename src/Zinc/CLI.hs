module Zinc.CLI
  ( Command (..)
  , parseArgs
  ) where

import Options.Applicative
import Zinc.Output (OutputFlags (..))

-- | A parsed zinc command. Output mode (@--json@/@--quiet@) is parsed separately
-- (every subcommand accepts it; see 'parseArgs') and is not encoded here.
data Command
  = New String            -- ^ scaffold a new workspace
  | Add String            -- ^ resolve a dependency closure and freeze it
  | Build (Maybe String)  -- ^ build the workspace, or one member
  | Run (Maybe String) [String] -- ^ build then run an executable: TARGET, then program ARGS
  | Repl (Maybe String)   -- ^ ghci for an optional target
  | Test (Maybe String)   -- ^ build and run tests for an optional target
  | Update (Maybe String) -- ^ bump refs for an optional package
  | Clean                 -- ^ remove build artifacts
  | Gc                    -- ^ garbage-collect the shared store
  | Perf                  -- ^ analyze build performance history
  | Doctor                -- ^ diagnose env/project problems
  | Status                -- ^ workspace status overview
  | Graph                 -- ^ the closure build DAG
  | Explain String        -- ^ why a package is in the build
  | Prime                 -- ^ AI-optimized orientation for this workspace
  | Onboard               -- ^ minimal AGENTS.md/CLAUDE.md snippet
  | Warm                  -- ^ build only the dependency closure
  | Dockerfile            -- ^ emit a multi-stage Docker build recipe
  | Fmt Bool              -- ^ canonically format zinc.toml; Bool = --check
  | Closure String        -- ^ discover a package's non-boot closure + repos
  deriving (Eq, Show)

-- | Pure, testable entry point: parse argv into the output flags + a 'Command'.
-- Uses 'execParserPure' so it never touches IO or calls @exitFailure@.
parseArgs :: [String] -> Either String (OutputFlags, Command)
parseArgs args =
  case execParserPure defaultPrefs opts args of
    Success r           -> Right r
    Failure failure     -> Left (fst (renderFailure failure "zinc"))
    CompletionInvoked _ -> Left "completion invoked"
  where
    opts =
      info
        (commandParser <**> helper)
        (fullDesc <> progDesc "Fast, reproducible Haskell builds that just work.")

commandParser :: Parser (OutputFlags, Command)
commandParser =
  subparser $
    mconcat
      [ sub "new"    "Scaffold a new workspace"       (New <$> strArgument (metavar "NAME"))
      , sub "add"    "Add a dependency"               (Add <$> (yesFlag *> strArgument (metavar "PKG")))
      , sub "build"  "Build the workspace or a member" buildCmd
      , sub "warm"   "Build only the dependency closure (CI/Docker cache)" (pure Warm)
      , sub "run"    "Build then run an executable"   (Run <$> optional (strArgument (metavar "[TARGET]")) <*> many (strArgument (metavar "[-- ARGS...]")))
      , sub "repl"   "Open ghci for a target"         (Repl <$> optional (strArgument (metavar "TARGET")))
      , sub "test"   "Build and run tests"            (Test <$> optional (strArgument (metavar "TARGET")))
      , sub "update" "Bump dependency refs to latest" (Update <$> optional (strArgument (metavar "PKG")))
      , sub "clean"  "Remove build artifacts"         (pure Clean)
      , sub "gc"     "Garbage-collect the shared store" (pure Gc)
      , sub "perf"   "Analyze build performance history" (pure Perf)
      , sub "doctor" "Diagnose environment and project problems" (pure Doctor)
      , sub "status" "Show workspace status (members, closure, drift)" (pure Status)
      , sub "graph"  "Show the closure build DAG"        (pure Graph)
      , sub "explain" "Explain why a package is in the build" (Explain <$> strArgument (metavar "PKG"))
      , sub "prime"  "Print AI-optimized orientation for this workspace" (pure Prime)
      , sub "onboard" "Print an AGENTS.md/CLAUDE.md snippet" (pure Onboard)
      , sub "dockerfile" "Emit a multi-stage Docker build recipe" (pure Dockerfile)
      , sub "fmt"    "Canonically format zinc.toml" (Fmt <$> switch (long "check" <> help "Exit non-zero if not already canonical; write nothing"))
      , sub "closure" "Discover a package's non-boot closure + repos" (Closure <$> strArgument (metavar "PKG"))
      ]
  where
    -- Every subcommand inherits the output flags (--json/--quiet), declared once
    -- here, accepted in the natural `zinc build --json` position.
    sub name desc p = command name (info (((,) <$> outputFlags <*> p) <**> helper) (progDesc desc))
    -- `build --deps-only` is a synonym for `warm` (build just the closure).
    buildCmd =
      (\target depsOnly -> if depsOnly then Warm else Build target)
        <$> optional (strArgument (metavar "MEMBER"))
        <*> switch (long "deps-only" <> help "Build only the dependency closure (alias: zinc warm)")
    -- zinc never prompts; --yes is accepted for forward-compatible scripting and
    -- otherwise ignored (documents the never-prompt contract, spec §3.4).
    yesFlag =
      switch (long "yes" <> short 'y' <> help "Assume yes; never prompt (zinc is non-interactive by default)")
    -- The unified output flags (spec §8): one mode, every command.
    outputFlags =
      OutputFlags
        <$> switch (long "json" <> help "Machine-readable output (JSONL stream + result envelope)")
        <*> switch (long "quiet" <> short 'q' <> help "Suppress progress output")
