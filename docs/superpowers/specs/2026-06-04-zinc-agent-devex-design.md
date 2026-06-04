# zinc — Agent-native DevEx

**Status:** Design approved 2026-06-04
**Author:** Gareth (with Claude)

## 1. Goal

Make the zinc CLI a **first-class tool for agentic harnesses** (Claude Code and
similar), the way [beads](https://steve-yegge.medium.com/introducing-beads-a-coding-agent-memory-system-637d7d92514a)
is for issue tracking. This is the first post-MVP *feature* initiative.

Aligns with zinc's principles (design §1.1): the tool does the structured
thinking and hands the agent **machine-readable state + actionable next steps**,
so the agent spends tokens on the work, not on parsing prose or inferring build
order.

## 2. What makes a tool agent-native (learned from beads)

From beads' design ([ianbull](https://ianbull.com/posts/beads/), and its command
surface — `prime`, `q`, `ready`, `graph`):

- **Structured data is the primary interface** — every command takes `--json`;
  agents consume state natively, no NLP parsing.
- **The tool owns the graph** — agents don't burn context reasoning about
  ordering; the tool computes and exposes it.
- **The tool says what to do next** — failures carry an actionable fix, not just
  a symptom. A failing run becomes a self-repairing loop.
- **Context recovery** — a `prime`/`onboard` step re-orients an agent after
  compaction or a fresh session.
- **Non-interactive by contract** — never blocks on `$EDITOR`/prompts; stable
  exit codes; agents never stall.
- **Multi-agent safe** — concurrent workers don't corrupt shared state.

**Explicitly NOT copied:** persistent agent *memory* (`remember`/`recall`).
beads owns that; zinc's durable state is its lockfile + content store. Adding a
parallel memory store would fragment — out of scope.

## 3. Architecture — one diagnostic core, three layers

The four requested themes are not peers; they stack on a single foundation.

### 3.1 The diagnostic core (foundation)

A structured-output + diagnostics core that every command emits through.

**Typed errors (decided 2026-06-04 — the internal half).** The `ExceptT String IO`
left by the `8dj` refactor still builds error *messages* at the throw site. Replace
the `String` error with a sum type:

```haskell
data ZincError = RefNotFound {repo, ref} | CloneFailed {repo, detail}
               | ContentHashMismatch {name, expected, got} | DepNoGitRepo {name}
               | BuildTypeCustom {name} | GhcCompile {package, detail}
               | AmbiguousTarget {candidates} | ManifestParse {file, detail}
               | NixAbsent | …
```

Pipelines are `ExceptT ZincError IO`; throw sites construct a **value**
(`throwE (DepNoGitRepo "colour")`), never a message. Compiler-checked
exhaustiveness means a new error kind can't be silently unhandled. This is §1.1
principle 2 made literal — errors are explicit structured state, not strings.

**The bridge.** A single boundary renderer
`toDiagnostic :: ZincError -> Diagnostic` is the *only* place messages/codes are
produced. It is called at the CLI/Main boundary and drives every output:

```
ZincError ──toDiagnostic──► Diagnostic ──► human text (zinc-unv) | JSON | exit code | nextAction
```

- **`Diagnostic` (output shape):** `{ code, severity, title, detail, location?, package?, nextAction? }`.
  The **code** is derived from the `ZincError` constructor.
- **JSON envelope** on every command: `{ zinc, command, ok, data, diagnostics }`.
- **Stable error-code taxonomy** — `ZINC_REF_NOT_FOUND`, `ZINC_SAFE_HASKELL`,
  `ZINC_DEP_NO_GIT_REPO`, `ZINC_NIX_ABSENT`, … — one per `ZincError` constructor.
- **Exit codes** mapped to error categories, so agents branch without parsing.

This **subsumes the old "migrate errors to taxonomy" task (`rdy.3`)**: that
migration is now (a) replacing `String` with `ZincError` across the modules, and
(b) writing the one exhaustive `toDiagnostic` — not hunting string sites.

**Unification with the human diagnostics epic (`zinc-unv`).** The conflict table,
the "colour has no git repo" message, etc. are *renderings* of `Diagnostic`
data. `unv` (human/terminal renderer) and this initiative (JSON renderer) become
**two renderers over one diagnostic model** — not two diagnostic systems. The
core is the shared substrate; build it once.

### 3.2 Build report (foundation)

`zinc build` / `test` / `resolve --json` emit a structured report: per-package
`{ name, ref, status: cached|built|failed|skipped, timeMs, ghcInvocation, diagnostics }`.
The agent sees exactly what happened and why, machine-readably.

### 3.3 Introspection layer

- `zinc status --json` — "where am I": members, resolved closure, lock drift,
  cached vs to-build, toolchain status.
- `zinc graph --json` — the build DAG (closure + members), so the agent never
  infers ordering.
- `zinc explain <pkg>` — why this package is in the build, at this ref:
  provenance (who depends on it) + conflict resolution. Mirrors beads' forensics.
- `zinc prime` — AI-optimized orientation: how to build/run/test here, current
  toolchain, members, gotchas. The build-tool analog of `bd prime`.
- `zinc onboard` — a minimal snippet for `AGENTS.md`/`CLAUDE.md`.

### 3.4 Robustness layer

- `zinc doctor --json` — diagnose env/project problems (Nix present? flakes on?
  lock drift? Custom-Setup deps? unresolvable refs?) each with a `nextAction`.
- **Non-interactive contract** — `zinc add --yes` (the confirm flow is for
  humans), a documented never-prompt guarantee, stable exit codes per category.
- **Concurrency-safe shared store** — parallel agents / worktrees share
  `~/.zinc/store` without corruption (locking around register/write). Independent
  of the core; pairs with parallel closure builds (`zinc-n4r`).

## 4. Decomposition (tracked units)

| Unit | Layer | Depends on |
|---|---|---|
| Diagnostic core: envelope + `Diagnostic` + taxonomy + exit codes | foundation | — |
| Structured build report (`build`/`test`/`resolve --json`) | foundation | core |
| Migrate existing errors → taxonomy with `nextAction` | foundation | core |
| `status` / `graph` / `explain --json` | introspection | core |
| `prime` / `onboard` | introspection | core |
| `doctor --json` | robustness | core |
| Non-interactive contract: `--yes`, never-prompt, exit codes | robustness | core |
| Concurrency-safe shared store | robustness | — |

## 5. Sequencing & gating

- **Gated behind the ExceptT refactor (`zinc-8dj`)**, which is itself gated
  behind self-host (`zinc-576`). Rationale: this adds a structured-output core +
  ~7 new commands; building that on the current right-drift staircase would be
  painful. Clean base first.
- **Top post-MVP *feature* priority** — ahead of distribution (`zinc-gtv`).
- Internal order: the diagnostic core lands first; introspection and robustness
  fan out from it. Store-safety is independent.

## 6. Testing

- Golden-test the JSON envelope + `Diagnostic` shape for representative
  success/failure cases per command.
- Assert stable exit codes per error category.
- `explain`/`graph` golden tests over a fixture closure.
- Concurrency test: parallel `zinc build` invocations against one store leave it
  consistent.

## 7. Out of scope

Persistent agent memory (beads owns it) · a zinc daemon/watch mode (revisit if
the inner loop needs it) · an MCP server wrapping zinc (possible later; the
`--json` surface is the prerequisite either way).
