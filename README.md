# zinc

**Fast, reproducible Haskell builds that just work.**

zinc is a git-native, Nix-assisted Haskell build tool. Dependencies are **git
repositories pinned by content hash**, the toolchain comes from a **Nix dev
shell**, and zinc drives `ghc --make` **directly** — no cabal, no stack, no
solver. It is **self-hosting** (zinc builds zinc) and **agent-first**: every
command speaks a machine-readable JSON envelope with structured diagnostics.

## Install

zinc is distributed as a Nix flake.

```sh
# Run without installing:
nix run github:garethstokes/zinc -- --help

# Install into your profile:
nix profile install github:garethstokes/zinc

# Build from a checkout:
nix build .#default        # -> ./result/bin/zinc
```

For downstream Nix, the flake also exports `overlays.default` (exposing
`pkgs.zinc`). To hack on zinc itself, enter the dev shell — it provides GHC,
git, and the `alex`/`happy` preprocessors zinc shells out to:

```sh
nix develop
```

> zinc never installs or mutates a toolchain. If `ghc` is not on `PATH` it fails
> fast with guidance (exit code 5) — enter `nix develop` first.

## Quick start

```sh
zinc new myapp     # scaffold a workspace member
zinc build         # build every member (libraries first, then executables)
zinc run -- arg1   # build, then run the executable with inherited stdio
zinc test          # build and run the test suites
zinc add aeson     # add a git-pinned dependency, frozen into zinc.lock
```

## Commands

| Area | Commands |
|---|---|
| Build / run | `build [TARGET] [--json]`, `warm` (deps-only, alias `build --deps-only`), `run [TARGET] [-- ARGS]`, `repl`, `test`, `clean` |
| Dependencies | `add <pkg> [--yes]`, `update`, `gc` |
| Introspection | `status [--json]`, `graph [--json]`, `explain <pkg> [--json]` |
| Diagnostics | `doctor [--json]`, `perf [--json]` |
| Orientation | `prime`, `onboard`, `dockerfile` |
| Scaffold | `new <name>` |

## Agent-friendly by design

- **`--json` everywhere** — commands emit a `{ zinc, command, ok, data, timing?,
  diagnostics }` envelope. Output is **NDJSON**: the **last line is always that
  envelope**, and long build ops (`build`, `warm`) stream one event object per
  line ahead of it (`{ "event": "compile-done", "package": …, "timeMs": …,
  "cached": … }`, plus `resolve-start`, `fetch-start`/`fetch-done`,
  `compile-start`, `register-done`, `finished`). Event lines carry an `event`
  key; the result line carries `zinc`/`ok` and no `event` key — so a streaming
  agent dispatches on `event` while a non-streaming agent just reads the last
  line. Commands with no events (e.g. `status`, `doctor`) emit exactly that one
  envelope line.
- **Structured errors** — every failure carries a stable code (`ZINC_*`), a
  human `nextAction`, and a **category exit code** (2 usage, 3 resolution,
  4 build, 5 environment, 6 integrity) so agents branch without parsing.
- **Non-interactive** — no prompts; `--yes` is accepted for forward-compat.
- **Orientation** — `zinc prime` prints how to build/run/test here;
  `zinc onboard` emits an `AGENTS.md`/`CLAUDE.md` snippet.
- **Performance history** — every build appends a record to
  `.zinc/metrics.jsonl`; `zinc perf` reports latency (p50/p95), cache hit-rate,
  and regressions.

## How it works

- **Dependencies** are git repos pinned in `zinc.lock` and mapped in the
  workspace `[registry]`. Builds are content-addressed
  (`sha256(rev, ghc-version, dep ids, options)`) and cached once per machine in
  a shared store at `~/.zinc/store` (override with `ZINC_STORE`). The store is
  concurrency-safe, so parallel agents/worktrees can share it.
- **The toolchain** is provided by the Nix dev shell; zinc invokes `ghc`,
  `ghc-pkg`, `ar`, and `alex`/`happy`/`hsc2hs` from `PATH`.
- **Ephemeral / CI builds** — `zinc warm` builds just the dependency closure
  into the store, so it can be its own Docker layer / CI cache entry separate
  from fast-changing source. `zinc dockerfile` emits the multi-stage recipe.

## Self-hosting

zinc is described by its own `zinc.toml` and builds itself — `zinc build` on
this repository compiles zinc's dependency closure, library, and executable.
