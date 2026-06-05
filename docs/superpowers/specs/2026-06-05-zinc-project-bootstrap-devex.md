# zinc — Project bootstrap & hidden-Nix devex

**Status:** Design approved 2026-06-05
**Author:** Gareth (with Claude)

## 1. Motivation

Feedback from starting a real project (`crucible`) surfaced first-run friction
that contradicts the "it just works / Nix hidden" thesis:

- **Forced two-level workspace** — even a single app must live under
  `packages/<name>/{src,app,test}`, not the repo root.
- **`flake.nix` hand-authored** — zinc didn't generate one (even though the
  generator already exists: `a6h.1` "Generate flake.nix" is closed).
- **Must run inside `nix develop`** — zinc detects GHC and *points* at
  `nix develop`, but doesn't provision the toolchain itself. So Nix is a manual
  prerequisite — the opposite of "hidden."
- **No `.gitignore` / `git init`.**

Goal: `zinc new myapp && cd myapp && zinc run` Just Works, with zero Nix
knowledge. Most of the machinery exists; the work is **wiring it in**.

## 2. `zinc new`: flat single-package default + full scaffold

- **Flat layout (decided).** `zinc new <name>` scaffolds a **single-package
  project at the repo root**: one `zinc.toml` (`[package]` +
  `[build.exe.<name>]` + `[dependencies]`) with `src/` (and `app/`, `test/`) at
  the root — **no `packages/<name>/` nesting**. A lone package is the implicit
  one-member workspace (design §4), now the default scaffold. `--workspace` (or
  `zinc new --workspace`) produces the multi-member layout for those who want it.
- **Generate `flake.nix`.** Wire the existing `a6h.1` generator into `zinc new`
  so the project ships a zinc-managed flake (GHC + `system-libs` +
  preprocessors). zinc owns/regenerates it when `ghc`/`system-libs` change.
- **`.gitignore`.** Scaffold it: `.zinc/`, `result`, `result-*`, `*.hi`, `*.o`.
- **`git init`.** Initialize the repo (skip if already one); optional initial
  commit.
- **Flake template.** Ship `templates.default` in zinc's own flake so
  `nix flake init -t github:garethstokes/zinc` bootstraps a project (flake +
  `zinc.toml` + `src/` + `.gitignore`) — the Nix-idiomatic entry point,
  complementing `zinc new`.

## 3. Auto-provision the toolchain (realize hidden-Nix)

The single biggest "it just works" win. Today the env-eval machinery exists
(`a6h.2/.3`: `nix print-dev-env`, cached) but is **not plumbed into the build
driver** — Orchestrate notes the "provision phase" as a follow-up — so the user
must `nix develop` by hand.

- **Eval + cache the dev env at build start** (`a6h` `envCacheKey`; re-eval only
  when `ghc`/`system-libs` change).
- **Thread the provisioned env into every toolchain shell-out** — `ghc`,
  `ghc-pkg`, `ar`, `alex`/`happy`/`hsc2hs`, `wasmtime` — across
  `build`/`run`/`test`/`repl`, running them with the provisioned `PATH`/env
  rather than ambient. So `zinc build` works **without** `nix develop`.
- **Fallback:** Nix absent → the existing detect-and-guide preflight (`gtv.2`).
- **Update messaging:** `Prime`/`Diagnostic`/`Docker` currently say "enter
  `nix develop`"; revise to reflect auto-provisioning (and keep
  `nix develop` only as the manual-escape-hatch mention).

## 4. Decomposition

**Epic A — `zinc new` bootstrap:**

| Unit | Notes |
|---|---|
| Flat single-package default (+ `--workspace`) | root `zinc.toml`, `src/app/test` at root |
| Generate `flake.nix` on `new` | wire `a6h.1` |
| `.gitignore` + `git init` | scaffold + init |
| Flake template (`templates.default`) | `nix flake init -t github:garethstokes/zinc` |

**Epic B — Auto-provision the Nix toolchain (hidden-Nix):**

| Unit | Notes | Depends on |
|---|---|---|
| Eval + cache the dev env at build start | use `a6h` machinery | `a6h` (closed) |
| Thread provisioned env into toolchain shell-outs | `ghc`/`ghc-pkg`/`ar`/preprocessors across build/run/test/repl | eval step |
| Revise messaging (no manual `nix develop`) | Prime/Diagnostic/Docker | thread step |

## 5. Gating / priority

Both P2 — directly fix the first-run experience the feedback flagged, and most
prerequisites (`a6h` flake-gen + env-eval) are already closed, so this is
high-leverage wiring. Epic B (auto-provision) is the larger, cross-cutting one
(touches every toolchain process exec) and the bigger thesis win.
