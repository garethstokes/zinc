# zinc guide

Fast, reproducible Haskell builds that just work.

zinc is a git-native, Cargo-like build tool for Haskell. Dependencies are git
repositories pinned to exact commits; the GHC toolchain and system libraries are
provided by Nix, which zinc manages for you; there is no version solver.

## Contents

- [Getting started](getting-started.md) — install zinc, create a project, build and run it.
- [The manifest](manifest.md) — `zinc.toml`: workspace, packages, components, dependencies.
- [Dependencies and the lockfile](dependencies.md) — git-native deps, one ref per name, `zinc add`, `zinc.lock`, vendoring.
- [Building](building.md) — how zinc drives GHC, reads `.cabal` files, builds the closure, and caches artifacts.
- [The toolchain](toolchain.md) — the hidden Nix flake that provides GHC, system libraries, and preprocessors.
- [Commands](commands.md) — every command and its options.
- [The agent surface](agent.md) — `--json`, structured diagnostics, stable exit codes, `zinc prime`.
- [Output](output.md) — the human renderer and the machine event stream.
- [Performance](performance.md) — `zinc perf`, build timings, cache hit rates.
- [Updating dependencies](updating.md) — `zinc update`, `zinc outdated`, and how updates ripple through the closure.
- [WebAssembly targets](wasm.md) — `zinc build --target wasm32-wasi`.
- [Packaging and deployment](deploy.md) — `zinc package` for Docker images, static binaries, and more.
- [Skills](skills.md) — install agentic skills as a package kind.

Each page covers why the feature exists, what it does, how to use it, and worked
examples.
