# zinc — CLI output: dual renderer (human delight + machine stream)

**Status:** Design approved 2026-06-04
**Author:** Gareth (with Claude)

## 1. Goal

Improve `zinc`'s output for two audiences that want opposite things from the same
tool:

- **Agents** — structured, stable, parseable, terse, deterministic, no ANSI.
- **Developers (to impress)** — color, live progress, clean alignment, a *felt*
  sense of speed, delightful error messages.

The resolution is **not** two output systems: it's **one structured
event/diagnostic stream with two renderers**, mode-selected. This generalizes the
`ZincError → toDiagnostic → {human, JSON}` split from the diagnostic core
(`rdy.1`) from *errors* to *all output* — progress, resolution tables, results.

## 2. Architecture

```
structured events + diagnostics ──► human renderer  (pretty, color, progress, TTY)
                                └──► machine renderer (JSON envelope / NDJSON stream)

mode: auto (human if stdout is a TTY, else machine) · --json · --quiet · NO_COLOR
```

One source of truth; human and machine output can never drift. The CLI/Main
boundary picks the renderer; everything below emits structured events, never
formatted strings. Builds on `rdy.1` (the `Diagnostic`/`ZincError` model) and
extends it to an output **event** stream (e.g. `ResolveStart`, `FetchDone`,
`CompileStart{pkg}`, `CompileDone{pkg,timeMs,cached}`, `Finished{...}`).

## 3. Human renderer (the "impress developers" half)

Currently untracked (the closed `zinc-unv` covered only error *text*; this
absorbs it into the unified renderer).

- **Cargo-style status verbs, right-aligned:** `Resolving` / `Fetching` /
  `Compiling` / `Building` / `Finished`. Instantly reads as a serious tool.
- **Live progress** for the closure build: `Building [23/47] aeson`, level-by-level.
- **Color + iconography**, `NO_COLOR`-aware, TTY-gated: green ✓, dim secondary
  text, bold package names.
- **Elm/rustc-style errors** — the single biggest delight lever. Render a
  `Diagnostic` with color, a location/caret, and a highlighted `help:`/`try:`
  line straight from `nextAction`. (Elm's compiler is famous purely for this.)
- **Restraint:** clean and fast beats noisy (uv/bun/ruff). Minimal, gated, quiet
  under `--quiet`.

## 4. Speed & cache spectacle (human)

Make zinc's substance *felt*. A final summary:

```
Finished dev in 3.2s · 47 packages (35 cached, 12 built)
```

Leverages `zinc-hbv` (timings) and the build report's cached/built counts. The
"35 cached" line is a flex no other Haskell tool can make — it surfaces the
content-addressed store's build-once property and reproducibility.

## 5. Machine renderer (the agentic half)

- The stable JSON envelope + diagnostics from `rdy`.
- **New: streaming NDJSON events** for long operations — one JSON object per
  event (`{"event":"compile-done","package":"aeson","timeMs":410,"cached":false}`),
  so an agent/CI consumes progress incrementally rather than waiting for a final
  blob. Extends `rdy.2` (final build report) into a live stream; also feeds
  `hbv` perf capture.
- Deterministic: no spinners/ANSI in machine mode; stable field ordering.

## 6. Decomposition

| Unit | Notes | Depends on |
|---|---|---|
| Event model + renderer abstraction + mode selection | the foundation: output event stream, human/machine renderer selection, TTY + `--json`/`--quiet`/`NO_COLOR` | `rdy.1` |
| Human renderer: verbs, color, live progress, elm-style errors | absorbs `unv`'s error rendering | foundation |
| Speed & cache summary (human) | timings + cached/built; flex | foundation; `hbv` |
| Streaming NDJSON machine events | incremental progress for agents/CI | foundation; extends `rdy.2` |

## 7. Relationships

Builds on the diagnostic core (`rdy.1`, `ZincError`/`Diagnostic`); the machine
renderer is the home of `rdy`'s JSON; streaming extends the build report
(`rdy.2`); the speed summary consumes perf data (`hbv`); the human renderer
subsumes the closed human-error work (`unv`). Not gated (MVP done), but the
foundation depends on `rdy.1` landing first.

## 8. Shaping decisions (2026-06-05)

**Current state — build on what exists, don't reinvent.** The diagnostic core
already shipped: `Zinc.Diagnostic` exports `toDiagnostic`, `renderError` (a basic
human renderer), `envelope`, `exitCodeFor`; `Zinc.Json` is the JSON surface; the
build report carries per-package `timeMs`. But `--json` is currently a *bolted-on
per-command `Bool`* (`Build (Maybe String) Bool`, `Doctor Bool`, …) with no TTY
detection, no `--quiet`, no `NO_COLOR`, no event stream, and plain (uncolored)
human output. So `hw6` = **unify the bolted-on `--json` into one mode-selected
dual renderer, add the human-delight layer on top of `renderError`, and introduce
incremental events** — not a from-scratch build.

Decisions:

1. **Unified output mode (replaces per-command `--json` Bool).** Resolve one
   `OutputMode` at the CLI/Main boundary: TTY → human+color; pipe → human plain;
   `--json` → machine; plus `--quiet` and `NO_COLOR`. Drop the per-command `Bool`
   (a CLI.hs + Main.hs dispatch refactor).
2. **Event emission via a `ReaderT` sink over `ExceptT ZincError IO`.** Build code
   calls `emit (CompileDone …)`; the env carries the sink. Composes with the
   existing monad; one plumbing pass enables real streaming + live progress. (This
   is cross-cutting — threads through Orchestrate/Build/Fetch — so sequence it to
   avoid colliding with other in-flight refactors.)
3. **Live rewriting progress line** (human): cargo/bun-style single in-place line,
   `Building [23/47] aeson`, cleared on finish. TTY-only; falls back to
   append-only when not a TTY / under `--quiet`.
4. **Color via raw ANSI** in a tiny internal module — no `ansi-terminal`/`colour`
   dependency (keeps zinc dep-light and dodges the very Safe-Haskell pain those
   packages caused during self-host). `NO_COLOR`-gated.

Elm/rustc-style error rendering (hw6.2) extends the existing `renderError` rather
than replacing it.
