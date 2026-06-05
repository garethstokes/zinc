module Zinc.CLI
  ( Command (..)
  , parseArgs
  , helpOverview
  ) where

import Options.Applicative
import Zinc.Output (OutputFlags (..))

-- | A parsed zinc command. Output mode (@--json@/@--quiet@) is parsed separately
-- (every subcommand accepts it; see 'parseArgs') and is not encoded here.
data Command
  = New String Bool       -- ^ scaffold a new project; Bool = --workspace (multi-member layout)
  | Add String            -- ^ resolve a dependency closure and freeze it
  | Vendor [String]       -- ^ pin no-git deps from their Hackage tarballs
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
  | Version               -- ^ print the zinc version (`--version`/`version`)
  | Help                   -- ^ the friendly no-args / `help` / `--help` overview
  | Outdated Bool          -- ^ report deps with newer versions; Bool = --all (whole closure)
  deriving (Eq, Show)

-- | Pure, testable entry point: parse argv into the output flags + a 'Command'.
-- Uses 'execParserPure' so it never touches IO or calls @exitFailure@.
parseArgs :: [String] -> Either String (OutputFlags, Command)
parseArgs args
  -- `--version`/`-V` are top-level flags (not subcommands), so short-circuit
  -- them before the subparser, which would reject them (zinc-gtv.3). The
  -- `version` subcommand flows through the parser below.
  | args `elem` [["--version"], ["-V"]] = Right (OutputFlags False False, Version)
  -- No args / `help` / `--help` show a friendly overview (exit 0), not
  -- optparse's terse "Missing: COMMAND" (zinc-hw6.6). Per-command help
  -- (`zinc build --help`) still flows through the subparser.
  | args `elem` [[], ["help"], ["--help"], ["-h"]] = Right (OutputFlags False False, Help)
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
      [ sub "new"    "Scaffold a new project (--workspace for a multi-member layout)" (New <$> strArgument (metavar "NAME") <*> switch (long "workspace" <> help "Scaffold a multi-member workspace (packages/<name>/) instead of a flat single-package project"))
      , sub "add"    "Add a dependency"               (Add <$> (yesFlag *> strArgument (metavar "PKG")))
      , sub "vendor" "Pin a no-git dependency from its Hackage tarball" (Vendor <$> (yesFlag *> some (strArgument (metavar "PKG..."))))
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
      , sub "version" "Print the zinc version" (pure Version)
      , sub "outdated" "Report dependencies with newer versions available" (Outdated <$> switch (long "all" <> help "Include the whole closure, not just direct dependencies"))
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

-- | The friendly overview shown for @zinc@ (no args), @zinc help@, and
-- @zinc --help@ (zinc-hw6.6) — a curated, grouped command list, not optparse's
-- terse usage. Per-command detail is still @zinc \<command\> --help@.
helpOverview :: String
helpOverview =
  unlines
    [ "zinc — fast, reproducible Haskell builds that just work."
    , ""
    , "Usage: zinc <command> [options]"
    , ""
    , "Getting started:"
    , "  new <name>         Scaffold a new project (--workspace for multi-member)"
    , "  add <pkg>          Add a dependency and freeze the lock"
    , "  run [target]       Build, then run an executable"
    , ""
    , "Build & test:"
    , "  build [member]     Build the workspace (or one member)"
    , "  test [target]      Build and run tests"
    , "  repl [target]      Open ghci for a target"
    , "  warm               Build only the dependency closure (CI/Docker cache)"
    , ""
    , "Dependencies:"
    , "  add <pkg>          Resolve a package's closure and freeze it"
    , "  vendor <pkg...>    Pin a no-git dependency from its Hackage tarball"
    , "  update [pkg]       Bump dependency refs to latest"
    , ""
    , "Inspect:"
    , "  status             Workspace overview (members, closure, drift)"
    , "  graph              The closure build DAG"
    , "  explain <pkg>      Why a package is in the build"
    , "  closure <pkg>      A package's non-boot closure + repos"
    , "  doctor             Diagnose environment and project problems"
    , ""
    , "Other:"
    , "  fmt                Canonically format zinc.toml"
    , "  clean / gc         Remove build artifacts / collect the shared store"
    , "  dockerfile         Emit a multi-stage Docker build recipe"
    , "  version            Print the zinc version"
    , ""
    , "Run `zinc <command> --help` for command-specific options."
    , "Every command accepts --json (machine output) and --quiet."
    ]
