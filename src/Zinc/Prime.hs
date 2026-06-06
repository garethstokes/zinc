-- | Context priming (spec §3.3), the build-tool analog of @bd prime@:
--
--   * @zinc prime@   — AI-optimized orientation for /this/ workspace: toolchain,
--     members, the command surface, and the gotchas an agent must know
--     (zinc drives ghc directly; deps are git-pinned; the store is shared).
--   * @zinc onboard@ — a minimal snippet to paste into @AGENTS.md@/@CLAUDE.md@ so
--     future agents know this project builds with zinc, not cabal/stack.
--
-- Both render from the workspace manifest so they reflect the real project.
module Zinc.Prime
  ( primeText
  , onboardText
  , runPrime
  , runOnboard
  ) where

import Data.List (intercalate)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Zinc.Diagnostic (ZincError (NoZincToml))
import Zinc.Except (Result, failWithError, liftEither, liftIO, runResult)
import Zinc.Manifest (WorkspaceManifest (wsGhc, wsMembers), parseWorkspace)

-- | Read + parse the workspace manifest, failing with 'NoZincToml' if absent.
loadWorkspace :: FilePath -> Result WorkspaceManifest
loadWorkspace wsDir = do
  let wsFile = wsDir </> "zinc.toml"
  present <- liftIO (doesFileExist wsFile)
  if not present
    then failWithError (NoZincToml wsDir)
    else liftIO (readFile wsFile) >>= liftEither . parseWorkspace

memberList :: WorkspaceManifest -> String
memberList ws = case wsMembers ws of
  [] -> "(none)"
  ms -> intercalate ", " ms

-- | The @zinc prime@ orientation for a workspace.
primeText :: WorkspaceManifest -> String
primeText ws =
  unlines
    [ "# zinc workspace orientation"
    , ""
    , "Toolchain: GHC " ++ wsGhc ws ++ " — auto-provisioned via Nix at build time (no manual `nix develop`; it stays as a manual escape hatch)."
    , "Members:   " ++ memberList ws
    , ""
    , "Build / run / test:"
    , "  zinc build              build every member's executables (libraries first)"
    , "  zinc build <member>     build one member"
    , "  zinc build --json       machine-readable report + timing block"
    , "  zinc run -- <args>      build, then run the first executable"
    , "  zinc test               build and run the test suites"
    , ""
    , "Dependencies (git-native, pinned):"
    , "  zinc add <pkg>          add a dep (resolved from [registry], frozen into zinc.lock)"
    , "  zinc update             re-resolve and bump refs"
    , ""
    , "Introspect / diagnose (all support --json):"
    , "  zinc status             toolchain, members, closure (cached?), lock drift"
    , "  zinc graph              the closure build DAG"
    , "  zinc explain <pkg>      why a package is in the build, at which rev"
    , "  zinc doctor             environment + project health checks"
    , "  zinc perf               build performance history (latency, cache, regressions)"
    , ""
    , "Gotchas:"
    , "  - zinc drives `ghc --make` directly. Do NOT use cabal or stack here."
    , "  - Dependencies are git repositories pinned in zinc.lock and mapped in [registry];"
    , "    there is no Hackage resolution — add a repo to [registry] for a new dep."
    , "  - The content-addressed build store is shared at ~/.zinc/store (override: ZINC_STORE)."
    , "  - Commands are non-interactive and emit stable exit codes; pass --json for structured output."
    ]

-- | The @zinc onboard@ snippet for @AGENTS.md@ / @CLAUDE.md@.
onboardText :: WorkspaceManifest -> String
onboardText ws =
  unlines
    [ "## Building (zinc)"
    , ""
    , "This project builds with **zinc** (a git-native, Nix-assisted Haskell build tool) —"
    , "not cabal or stack. Toolchain: GHC " ++ wsGhc ws ++ ", auto-provisioned via Nix at build time."
    , ""
    , "- `zinc build` — build all members (`--json` for a machine report)"
    , "- `zinc test` — run the test suites"
    , "- `zinc run -- <args>` — run an executable"
    , "- `zinc add <pkg>` / `zinc update` — manage git-pinned dependencies (zinc.lock)"
    , "- `zinc status` / `zinc graph` / `zinc explain <pkg>` / `zinc doctor` — orient + diagnose (all `--json`)"
    , ""
    , "Run `zinc prime` for full orientation."
    ]

-- | @zinc prime@: print the orientation for the workspace at @wsDir@.
runPrime :: FilePath -> IO (Either ZincError String)
runPrime wsDir = runResult (primeText <$> loadWorkspace wsDir)

-- | @zinc onboard@: print the AGENTS.md snippet for the workspace at @wsDir@.
runOnboard :: FilePath -> IO (Either ZincError String)
runOnboard wsDir = runResult (onboardText <$> loadWorkspace wsDir)
