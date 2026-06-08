-- | zinc's own build-time version metadata (zinc-b3z). The base version comes
-- from @[package] version@ in zinc's @zinc.toml@ (the single source of truth),
-- and the git tag (commit count, short hash, dirty) is baked in at build time
-- via the synthesized @Paths_zinc@ module (zinc-3x4) — so an installed binary,
-- which has no @.git@, still reports the exact commit it was built from.
--
-- This is a thin leaf module: only the @zinc version@ command depends on it, so
-- the per-commit churn of @Paths_zinc@ doesn't ripple through the library.
module Zinc.BuildInfo
  ( zincBaseVersion
  , zincFullVersion
  ) where

import Data.Version (showVersion)
import Paths_zinc (gitCommitCount, gitDirty, gitHash, version)
import Zinc.Version (gitVersion)

-- | zinc's base version (from @zinc.toml@, via @Paths_zinc@), e.g. @"0.1.0.0"@.
zincBaseVersion :: String
zincBaseVersion = showVersion version

-- | zinc's full, git-derived version, e.g. @"0.1.0.0+267.gabc1234.dirty"@ in a
-- working tree, or just the base for a release/non-git build.
zincFullVersion :: String
zincFullVersion = gitVersion zincBaseVersion gitHash gitCommitCount gitDirty
