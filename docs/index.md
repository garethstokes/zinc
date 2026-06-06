---
title: Home
nav_order: 1
---

# zinc

Fast, reproducible Haskell builds that just work.

zinc is a git-native build tool for Haskell. Dependencies are git
repositories pinned to exact commits. The GHC toolchain and system libraries are
provided by Nix, which zinc manages so you never have to. There is no version
solver: each package name resolves to a single ref, frozen in a lockfile.

```
git pull. zinc build. Done.
```

## Start here

- [Getting started](getting-started.md): install zinc, create a project, build and run it.

## The model

- [The manifest](manifest.md): `zinc.toml`: workspaces, packages, components, dependencies.
- [Dependencies and the lockfile](dependencies.md): git-native deps, one ref per name, `zinc add`, `zinc.lock`, vendoring.
- [Building](building.md): how zinc drives GHC, reads upstream package descriptions, builds the dependency closure, and caches artifacts.
- [The toolchain](toolchain.md): the Nix flake that provides GHC, system libraries, and preprocessors.

## Working with zinc

- [Commands](commands.md): every command and its options.
- [Updating dependencies](updating.md): `zinc update`, `zinc outdated`, and how updates ripple through the closure.
- [Performance](performance.md): `zinc perf`, build timings, cache hit rates.
- [Output](output.md): the human renderer and the machine event stream.
- [The agent surface](agent.md): `--json`, structured diagnostics, stable exit codes, `zinc prime`.

## Targets and deployment

- [WebAssembly targets](wasm.md): `zinc build --target wasm32-wasi`.
- [Packaging and deployment](deploy.md): `zinc package` for Docker images, static binaries, and portable bundles.
- [Skills](skills.md): install agentic skills as a package kind.

## The name

zinc is named after Zinc café in Melbourne, where the idea took shape in a
conversation with Geoff Huntley at the 2026 AI Engineering conference.
