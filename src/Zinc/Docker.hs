-- | @zinc dockerfile@ (ephemeral-builds spec §3): emit the multi-stage Docker
-- recipe that makes zinc fast on a fresh filesystem. The dependency closure is
-- its own layer — copy @zinc.toml@ + @zinc.lock@, @zinc build --deps-only@ into
-- a persistent @ZINC_STORE@ cache mount — so an unchanged lock is a layer/cache
-- hit and only fast-changing app source recompiles.
module Zinc.Docker
  ( dockerfileText
  , runDockerfile
  ) where

import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Zinc.Diagnostic (ZincError (NoZincToml))
import Zinc.Except (Result, failWithError, liftEither, liftIO, runResult)
import Zinc.Manifest (WorkspaceManifest (wsGhc), parseWorkspace)

-- | The multi-stage Dockerfile for a workspace pinned to @ghc@. The closure
-- build is split from the member build so they layer/cache independently, and
-- ZINC_STORE is a BuildKit cache mount so the content-addressed store survives
-- across builds even on an ephemeral filesystem.
dockerfileText :: String -> String
dockerfileText ghc =
  unlines
    [ "# syntax=docker/dockerfile:1"
    , "# zinc multi-stage build (ephemeral-builds spec §3)."
    , "# Toolchain: GHC " ++ ghc ++ " — provisioned via the flake dev shell."
    , "# The dependency closure is cached separately from app source: an"
    , "# unchanged zinc.lock makes the closure layer a cache hit, so only"
    , "# members recompile."
    , ""
    , "FROM nixos/nix:latest AS build"
    , "WORKDIR /app"
    , "# Relocatable content-addressed store; mounted as a persistent cache below."
    , "ENV ZINC_STORE=/zinc-store"
    , "# Flakes are required to enter the dev shell that provides the toolchain."
    , "RUN mkdir -p /etc/nix && echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf"
    , ""
    , "# 1. Closure layer: copy ONLY the manifest + lock, then warm the store."
    , "#    Cache key = the content of these two files (Docker layer cache) +"
    , "#    the ZINC_STORE cache mount (keyed in CI on hash(zinc.lock, ghc))."
    , "COPY flake.nix flake.lock* zinc.toml zinc.lock ./"
    , "RUN --mount=type=cache,target=/zinc-store \\"
    , "    nix develop --command zinc build --deps-only"
    , ""
    , "# 2. App layer: copy source and build members. The closure above is reused"
    , "#    from the cache mount when the lock is unchanged."
    , "COPY . ."
    , "RUN --mount=type=cache,target=/zinc-store \\"
    , "    nix develop --command zinc build"
    , ""
    , "# CI note: with no BuildKit cache mount, persist $ZINC_STORE as a cache"
    , "# entry keyed on hash(zinc.lock, ghc-version) and restore it before step 1."
    ]

-- | Read the workspace manifest (failing with 'NoZincToml' if absent) and emit
-- its Dockerfile.
runDockerfile :: FilePath -> IO (Either ZincError String)
runDockerfile wsDir = runResult (dockerfileText . wsGhc <$> loadWorkspace wsDir)

loadWorkspace :: FilePath -> Result WorkspaceManifest
loadWorkspace wsDir = do
  let wsFile = wsDir </> "zinc.toml"
  present <- liftIO (doesFileExist wsFile)
  if not present
    then failWithError (NoZincToml wsDir)
    else liftIO (readFile wsFile) >>= liftEither . parseWorkspace
