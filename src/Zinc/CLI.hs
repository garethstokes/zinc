module Zinc.CLI
  ( Command (..)
  , parseArgs
  ) where

import Options.Applicative

-- | A parsed zinc command. Mirrors the CLI surface in the design spec (§10).
data Command
  = New String            -- ^ scaffold a new workspace
  | Add String            -- ^ resolve a dependency closure and freeze it
  | Build (Maybe String) Bool -- ^ build the workspace (or one member); Bool = --json

  | Run (Maybe String) [String] -- ^ build then run an executable: TARGET (first positional), then program ARGS
  | Repl (Maybe String)   -- ^ ghci for an optional target
  | Test (Maybe String)   -- ^ build and run tests for an optional target
  | Update (Maybe String) -- ^ bump refs for an optional package
  | Clean                 -- ^ remove build artifacts
  | Gc                    -- ^ garbage-collect the shared store
  | Perf Bool             -- ^ analyze build performance history; Bool = --json
  | Doctor Bool           -- ^ diagnose env/project problems; Bool = --json
  | Status Bool           -- ^ workspace status overview; Bool = --json
  | Graph Bool            -- ^ the closure build DAG; Bool = --json
  | Explain String Bool   -- ^ why a package is in the build; Bool = --json
  | Prime                 -- ^ AI-optimized orientation for this workspace
  | Onboard               -- ^ minimal AGENTS.md/CLAUDE.md snippet
  | Warm Bool             -- ^ build only the dependency closure; Bool = --json
  | Dockerfile            -- ^ emit a multi-stage Docker build recipe
  | Fmt Bool              -- ^ canonically format zinc.toml; Bool = --check
  | Closure String Bool   -- ^ discover a package's non-boot closure + repos; Bool = --json
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
        (fullDesc <> progDesc "Fast, reproducible Haskell builds that just work.")

commandParser :: Parser Command
commandParser =
  subparser $
    mconcat
      [ sub "new"    "Scaffold a new workspace"       (New <$> strArgument (metavar "NAME"))
      , sub "add"    "Add a dependency"               (Add <$> (yesFlag *> strArgument (metavar "PKG")))
      , sub "build"  "Build the workspace or a member" buildCmd
      , sub "warm"   "Build only the dependency closure (CI/Docker cache)" (Warm <$> jsonFlag)
      , sub "run"    "Build then run an executable"   (Run <$> optional (strArgument (metavar "[TARGET]")) <*> many (strArgument (metavar "[-- ARGS...]")))
      , sub "repl"   "Open ghci for a target"         (Repl <$> optional (strArgument (metavar "TARGET")))
      , sub "test"   "Build and run tests"            (Test <$> optional (strArgument (metavar "TARGET")))
      , sub "update" "Bump dependency refs to latest" (Update <$> optional (strArgument (metavar "PKG")))
      , sub "clean"  "Remove build artifacts"         (pure Clean)
      , sub "gc"     "Garbage-collect the shared store" (pure Gc)
      , sub "perf"   "Analyze build performance history" (Perf <$> jsonFlag)
      , sub "doctor" "Diagnose environment and project problems" (Doctor <$> jsonFlag)
      , sub "status" "Show workspace status (members, closure, drift)" (Status <$> jsonFlag)
      , sub "graph"  "Show the closure build DAG"        (Graph <$> jsonFlag)
      , sub "explain" "Explain why a package is in the build" (Explain <$> strArgument (metavar "PKG") <*> jsonFlag)
      , sub "prime"  "Print AI-optimized orientation for this workspace" (pure Prime)
      , sub "onboard" "Print an AGENTS.md/CLAUDE.md snippet" (pure Onboard)
      , sub "dockerfile" "Emit a multi-stage Docker build recipe" (pure Dockerfile)
      , sub "fmt"    "Canonically format zinc.toml" (Fmt <$> switch (long "check" <> help "Exit non-zero if not already canonical; write nothing"))
      , sub "closure" "Discover a package's non-boot closure + repos" (Closure <$> strArgument (metavar "PKG") <*> jsonFlag)
      ]
  where
    sub name desc p = command name (info (p <**> helper) (progDesc desc))
    -- `build --deps-only` is a synonym for `warm` (build just the closure).
    buildCmd =
      (\target json depsOnly -> if depsOnly then Warm json else Build target json)
        <$> optional (strArgument (metavar "MEMBER"))
        <*> jsonFlag
        <*> switch (long "deps-only" <> help "Build only the dependency closure (alias: zinc warm)")
    -- zinc never prompts (the confirm flow is a human nicety layered elsewhere),
    -- so --yes is accepted for forward-compatible non-interactive scripting and
    -- otherwise ignored. Documents the never-prompt contract (spec §3.4).
    yesFlag =
      switch (long "yes" <> short 'y' <> help "Assume yes; never prompt (zinc is non-interactive by default)")
    -- Machine-readable structured output (spec §3): the {zinc,command,ok,data,
    -- diagnostics} envelope instead of human text.
    jsonFlag =
      switch (long "json" <> help "Emit a machine-readable JSON report")
