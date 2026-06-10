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
  | Add String Bool       -- ^ resolve a dependency closure and freeze it; Bool = --dry-run (preview, no write)
  | Vendor [String]       -- ^ pin no-git deps from their Hackage tarballs
  | Build (Maybe String) (Maybe String) (Maybe String) -- ^ build the workspace/member; 2nd = --ghc override, 3rd = --target (native/wasm32-wasi, zinc-9po.3)
  | Run (Maybe String) [String] (Maybe String) -- ^ build then run an executable: TARGET, program ARGS, then --target (native/wasm32-wasi, zinc-9po.4)
  | Repl (Maybe String)   -- ^ ghci for an optional target
  | Test (Maybe String)   -- ^ build and run tests for an optional target
  | Update (Maybe String) Bool -- ^ bump refs for an optional package; Bool = --dry-run
  | Clean                 -- ^ remove build artifacts
  | Gc                    -- ^ garbage-collect the shared store
  | Perf                  -- ^ analyze build performance history
  | Doctor                -- ^ diagnose env/project problems
  | Status                -- ^ workspace status overview
  | Graph                 -- ^ the closure build DAG
  | Explain String        -- ^ why a package is in the build
  | Prime                 -- ^ AI-optimized orientation for this workspace
  | Onboard               -- ^ minimal AGENTS.md/CLAUDE.md snippet
  | Warm (Maybe String) (Maybe String) -- ^ build only the dependency closure; args = --ghc override, --target (zinc-hte)
  | Dockerfile            -- ^ emit a multi-stage Docker build recipe
  | Fmt Bool              -- ^ canonically format zinc.toml; Bool = --check
  | Closure String        -- ^ discover a package's non-boot closure + repos
  | Version               -- ^ print the zinc version (`--version`/`version`)
  | Help                   -- ^ the friendly no-args / `help` / `--help` overview
  | Outdated Bool          -- ^ report deps with newer versions; Bool = --all (whole closure)
  | CachePush              -- ^ `cache push`: publish closure artifacts to the remote cache
  | Package String (Maybe String) (Maybe String) (Maybe String) -- ^ `package <format>`: format, --tag, -o, --to (nix-copy remote)
  | Deploy String (Maybe String) Bool Bool Bool Bool (Maybe Int) String -- ^ `deploy <host>`: host, --service, --init, --rollback, --dry-run, --list, --rollback-to GEN, --strategy (nbk.1/.7/.8)
  | SkillAdd String (Maybe String) -- ^ `skill add <repo> [--ref]`: install a Claude Code skill (dp6.2)
  | SkillList               -- ^ `skill list`: installed skills (dp6.3)
  | SkillRemove String      -- ^ `skill remove <name>`: drop a skill's symlink + lock entry (dp6.3)
  | SkillSync               -- ^ `skill sync`: re-materialize locked skills (dp6.4)
  deriving (Eq, Show)

-- | Pure, testable entry point: parse argv into the output flags + a 'Command'.
-- Uses 'execParserPure' so it never touches IO or calls @exitFailure@.
parseArgs :: [String] -> Either String (OutputFlags, Command)
parseArgs args
  -- `--version`/`-V` are top-level flags (not subcommands), so short-circuit
  -- them before the subparser, which would reject them (zinc-gtv.3). The
  -- `version` subcommand flows through the parser below.
  | args `elem` [["--version"], ["-V"]] = Right (OutputFlags False False False, Version)
  -- No args / `help` / `--help` show a friendly overview (exit 0), not
  -- optparse's terse "Missing: COMMAND" (zinc-hw6.6). Per-command help
  -- (`zinc build --help`) still flows through the subparser.
  | args `elem` [[], ["help"], ["--help"], ["-h"]] = Right (OutputFlags False False False, Help)
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
      , sub "add"    "Add a dependency"               (Add <$> (yesFlag *> strArgument (metavar "PKG")) <*> switch (long "dry-run" <> help "Preview the closure + per-member repo resolvability without touching zinc.toml/zinc.lock"))
      , sub "vendor" "Pin a no-git dependency from its Hackage tarball" (Vendor <$> (yesFlag *> some (strArgument (metavar "PKG..."))))
      , sub "build"  "Build the workspace or a member" buildCmd
      , sub "warm"   "Build only the dependency closure (CI/Docker cache)" (Warm <$> ghcOption <*> targetOption)
      , sub "run"    "Build then run an executable"   (Run <$> optional (strArgument (metavar "[TARGET]")) <*> many (strArgument (metavar "[-- ARGS...]")) <*> targetOption)
      , sub "repl"   "Open ghci for a target"         (Repl <$> optional (strArgument (metavar "TARGET")))
      , sub "test"   "Build and run tests"            (Test <$> optional (strArgument (metavar "TARGET")))
      , sub "update" "Bump dependency refs to latest" (Update <$> optional (strArgument (metavar "PKG")) <*> switch (long "dry-run" <> help "Show the closure delta without writing zinc.lock"))
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
      , sub "package" "Build a deployable artifact (docker/static/bundle/nix)" (Package <$> strArgument (metavar "FORMAT") <*> optional (strOption (long "tag" <> metavar "TAG" <> help "Image tag (docker)")) <*> optional (strOption (long "output" <> short 'o' <> metavar "PATH" <> help "Write the artifact to PATH")) <*> optional (strOption (long "to" <> metavar "STORE-URI" <> help "Copy the Nix closure to a remote store (nix), e.g. ssh://host or s3://bucket")))
      , sub "deploy" "Push and activate a build on a remote NixOS host" (Deploy <$> strArgument (metavar "HOST") <*> optional (strOption (long "service" <> metavar "NAME" <> help "Override the systemd unit name (default: package name)")) <*> switch (long "init" <> help "Generate the host's trusted-users + linger config") <*> switch (long "rollback" <> help "Revert to the previous generation and restart") <*> switch (long "dry-run" <> help "Probe and report without copying or activating") <*> switch (long "list" <> help "List the service's deployed generations (version + timestamp)") <*> optional (option auto (long "rollback-to" <> metavar "GEN" <> help "Switch to a specific generation, then restart + health-check")) <*> strOption (long "strategy" <> metavar "MODE" <> value "recreate" <> help "Deploy strategy: recreate (default) or blue-green (zero-downtime; needs [deploy.*].socket = <port>)"))
      , sub "outdated" "Report dependencies with newer versions available" (Outdated <$> switch (long "all" <> help "Include the whole closure, not just direct dependencies"))
      , command "cache" (info (subparser (sub "push" "Publish built closure artifacts to the remote cache (ZINC_CACHE)" (pure CachePush)) <**> helper) (progDesc "Manage the remote artifact cache"))
      , command "skill" (info (subparser (mconcat
          [ sub "add" "Install a Claude Code skill (git-native, pinned, content-verified)" (SkillAdd <$> strArgument (metavar "REPO") <*> optional (strOption (long "ref" <> metavar "REF" <> help "Pin to a tag, branch, or commit (default: latest)")))
          , sub "list" "List installed skills (name, rev, repo)" (pure SkillList)
          , sub "remove" "Remove an installed skill (symlink + lock entry)" (SkillRemove <$> strArgument (metavar "NAME"))
          , sub "sync" "Re-materialize every locked skill from zinc.lock" (pure SkillSync)
          ]) <**> helper) (progDesc "Install + manage agent skills (no toolchain needed)"))
      ]
  where
    -- Every subcommand inherits the output flags (--json/--quiet), declared once
    -- here, accepted in the natural `zinc build --json` position.
    sub name desc p = command name (info (((,) <$> outputFlags <*> p) <**> helper) (progDesc desc))
    -- `build --deps-only` is a synonym for `warm` (build just the closure).
    buildCmd =
      (\member depsOnly ghc target -> if depsOnly then Warm ghc target else Build member ghc target)
        <$> optional (strArgument (metavar "MEMBER"))
        <*> switch (long "deps-only" <> help "Build only the dependency closure (alias: zinc warm)")
        <*> ghcOption
        <*> targetOption
    -- The compile target (zinc-9po.3): native (default) or wasm32-wasi. Threaded
    -- into the build driver + the provisioned toolchain; the store keys per target.
    targetOption =
      optional (strOption (long "target" <> metavar "TARGET" <> help "Compile target: native (default) or wasm32-wasi"))
    -- A per-build GHC override (ey4): build the whole workspace + closure against
    -- a specific GHC (provisioned via Nix, y03); the store keys on it.
    ghcOption =
      optional (strOption (long "ghc" <> metavar "VERSION" <> help "Build against a specific GHC version (provisioned via Nix); the build is keyed per GHC"))
    -- zinc never prompts; --yes is accepted for forward-compatible scripting and
    -- otherwise ignored (documents the never-prompt contract, spec §3.4).
    yesFlag =
      switch (long "yes" <> short 'y' <> help "Assume yes; never prompt (zinc is non-interactive by default)")
    -- The unified output flags (spec §8): one mode, every command.
    outputFlags =
      OutputFlags
        <$> switch (long "json" <> help "Machine-readable output (JSONL stream + result envelope)")
        <*> switch (long "quiet" <> short 'q' <> help "Suppress progress output")
        -- No -v short form: it reads as --version in too many CLIs (zinc-1sk).
        <*> switch (long "verbose" <> help "Print full tool output on failure (e.g. GHC's complete stderr); ZINC_VERBOSE=1 is the env equivalent")

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
    , "  deploy <host>      Push and activate a build on a remote NixOS host"
    , "  version            Print the zinc version"
    , ""
    , "Run `zinc <command> --help` for command-specific options."
    , "Every command accepts --json (machine output), --quiet, and --verbose (full tool output on failure)."
    ]
