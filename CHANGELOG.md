# Changelog

All notable changes to zinc are documented here. zinc follows
[semantic versioning](https://semver.org/).

## [0.1.0.0] — unreleased

The first self-hosting, agent-first release.

### Build model

- Git-native dependencies pinned by content hash in `zinc.lock`, mapped via a
  per-project decentralized `[registry]`; resolver walks the transitive closure
  and freezes exact commits + sha256.
- Drives `ghc --make` directly (no cabal/stack); content-addressed build store
  shared at `~/.zinc/store` (relocatable via `ZINC_STORE`), with incremental
  inner-loop rebuilds and bounded-concurrency closure builds.
- **Self-hosting**: zinc builds zinc from its own `zinc.toml`.
- Reads non-zinc dependencies from their `.cabal` (build-depends, extensions,
  ghc-options, include-dirs, **cpp-options, c-sources**); auto-discovers a
  library's modules from its source-dirs (no `exposed`/`other` lists).
- Concurrency-safe shared store (per-key advisory lock) for parallel
  agents/worktrees.

### WebAssembly targets

- `zinc build --target wasm32-wasi` cross-compiles a pure-Haskell workspace to a
  `<name>.wasm` command module, driving GHC's wasm cross-compiler (provisioned
  via the `ghc-wasm-meta` flake). The compile target threads through the build
  driver and keys the store per target, so native + wasm artifacts coexist;
  native builds are byte-identical to before. A closure member that needs C
  sources or system libraries fails fast with `ZINC_WASM_UNSUPPORTED` (the MVP
  is pure-Haskell only).
- `zinc run --target wasm32-wasi` builds for wasm and runs the `.wasm` through
  the Nix-provided `wasmtime`; native `zinc run` execs the binary directly.
- Browser **reactor** flavor: an exe with `wasm-exports = [...]` in its
  `[build.exe.*]` builds (for the wasm target) a reactor module
  (`-no-hs-main -optl-mexec-model=reactor`, with each symbol linker-exported)
  and generates the `ghc_wasm_jsffi.js` glue (via the toolchain's
  `post-link.mjs`) so a browser can bind its `foreign import javascript` calls —
  zero toolchain setup. `hs_init` is exported automatically (the host calls it
  once to start the RTS before any entry point). An empty/absent `wasm-exports`
  stays a WASI command module.

### Agent-native DevEx

- Machine-readable `--json` on every command: a `{ zinc, command, ok, data,
  timing?, diagnostics }` envelope.
- Structured error taxonomy (`ZINC_*` codes) with a `nextAction` and stable
  category exit codes (2 usage · 3 resolution · 4 build · 5 environment ·
  6 integrity).
- Package exposure honours Cabal `reexported-modules:` — an umbrella package
  (e.g. `effectful` re-exporting `Effectful` from `effectful-core`) now lets a
  consumer that depends only on the umbrella `import` the re-exported module.
  zinc reads the reexports, resolves each origin to the dependency that exposes
  it, and emits ghc-pkg's `exposed-modules: New from unit:Orig` syntax in the
  generated `.conf`.
- `ZINC_DEP_BOOT_CONFLICT`: when a dependency fails to compile *because* its
  resolved tag pins a GHC boot library below what the toolchain ships (the
  classic "newest tag is stale" trap — e.g. monad-control's tag pins
  `transformers <0.6` against a 0.6 toolchain), zinc replaces the cryptic
  `ErrorT not in scope` with a named diagnostic — package, boot lib, both
  versions — and probes the dependency's HEAD to suggest the exact forward-pin
  commit. Bounds stay advisory: the check fires only on an actual compile
  failure, so a conservative-but-harmless bound (e.g. a stale `base <4.17`) never
  blocks a build that would otherwise succeed.
- `ZINC_VERBOSE`: on a build failure the concise caret view is summarised; set
  `ZINC_VERBOSE=1` to print GHC's full, untruncated stderr (the `-package-id` /
  module-not-found detail, plus any `-v` output a package's `ghc-options`
  enabled). A hint at the failure points to it. (`ghc-options` already reach the
  member compile — the output was simply hidden before.)
- Introspection: `status`, `graph`, `explain`. Diagnostics: `doctor` (env +
  project health), `perf` (latency p50/p95, cache hit-rate, regressions over a
  rolling baseline). Orientation: `prime`, `onboard`, `dockerfile`.
- Non-interactive contract: never prompts; `--yes` accepted; git runs with
  terminal prompts disabled so missing auth fails fast.
- `zinc run [TARGET] [-- ARGS]` selects an executable target and execs it with
  inherited stdio + exit-code propagation.

### Performance tracking

- A `timing` block (total, per-phase, cache stats, per-package `timeMs`) in the
  build envelope; per-invocation records persisted to `.zinc/metrics.jsonl`
  (survives `zinc clean`); `zinc perf` analyzes the history.

### Distribution

- Nix flake exports `packages.default`, `apps.default`
  (`nix run github:garethstokes/zinc`), and `overlays.default`, alongside the
  dev shell.

### Deploy

- `zinc deploy <host>` (in progress): parses `[user@]host[:port]` / ssh-config
  aliases and probes a NixOS host's deploy preconditions over SSH (Nix daemon
  present, deploy user in `trusted-users`, lingering enabled). Each gap maps to
  a typed diagnostic — `ZINC_DEPLOY_SSH` / `_NO_NIX` / `_NOT_TRUSTED` /
  `_NO_LINGER` — with an actionable `nextAction`. The closure copy + activate
  sequence is landing incrementally.
- `zinc deploy --init <host>` emits the NixOS module snippet that makes a host a
  deploy target (deploy user in `trusted-users` + lingering enabled), resolving
  the user over SSH when no explicit `user@` is given. Printed, never applied
  unprompted.
- Named `[deploy.<name>]` targets in `zinc.toml` (host/service/args/env):
  `zinc deploy <name>` resolves the configured target; an ad-hoc `user@host`
  still works, and `--service` overrides the configured unit name. `zinc fmt`
  preserves the `[deploy.*]` tables.
