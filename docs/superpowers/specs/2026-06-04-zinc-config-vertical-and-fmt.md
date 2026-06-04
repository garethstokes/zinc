# zinc — Vertical dependency config + `zinc fmt`

**Status:** Design approved 2026-06-04
**Author:** Gareth (with Claude)

Supersedes the manifest dependency layout in the main design's §4.

## 1. Problem

A dependency's facets are currently spread across three aspect-keyed tables
(*horizontal*): its ref in `[dependencies]`, its repo in `[registry]`, its ghc
flags in `[build-options]`. To read or change one dependency you scan three
tables. We want *vertical*: one block per dependency holding all its facets.

This also converges on Cargo's proven model (`foo = { git=, tag=, features= }`);
zinc's three-table split is the outlier.

## 2. Vertical dependency schema (decided)

One sub-table per dependency at the workspace root:

```toml
[dependencies.aeson]
tag         = "v2.3.0.0"
repo        = "https://github.com/haskell/aeson"   # optional override (see §2.1)
ghc-options = ["-XSafe"]                            # optional; was [build-options]
```

Exactly one ref key per dep: `tag` | `branch` | `rev` (or shorthand, §2.2).

`[registry]` and `[build-options]` are **removed** — their data moves into each
dependency's `repo` and `ghc-options` keys. "Deps live at root" (§4 of the main
design) is unchanged: these blocks live at the workspace root; members still
reference deps by name only.

### 2.1 `repo` is optional in the *manifest* because the *lock* is the record

Decided 2026-06-04 (after review of "is repo optional?"). The repo is **always
explicit — in `zinc.lock`**, not necessarily typed in the manifest:

- `zinc add` resolves each dependency's repo (best-effort discovery from Hackage
  `source-repository`, per §9), **shows the closure with repos for confirmation**,
  and **freezes the repo into `zinc.lock`**. Packages with no resolvable repo are
  flagged for the user to supply a `repo`.
- `zinc build` reads repos **from the lock** and **never infers** — discovery
  happens only at `add` time, under review (principle 4: no build-time inference;
  principle 2: explicit, frozen state).

So a manifest `[dependencies.<name>]` may omit `repo`; it's then resolved+frozen
by `add`. The `repo` key is the explicit **override/pin** — for forks, vendored
or no-upstream-git packages (e.g. `colour`), or to override what discovery would
pick. It is *not* a silent build-time default.

### 2.2 Shorthand for the common case

A bare string is the ref; repo auto-discovered, no flags:

```toml
[dependencies]
aeson = "v2.3.0.0"
hspec = "*"            # "*" = latest release tag
```

So simple deps stay one line and get *shorter* than today; the full
`[dependencies.<name>]` block is opened only for a `repo` override or
`ghc-options`. `zinc fmt` (§3) renders simple deps as shorthand and multi-key
deps as sub-tables.

### 2.3 Parser changes

`parseWorkspace` reads `[dependencies.<name>]` sub-tables **and** bare-string
shorthands under `[dependencies]`. `parseRegistry` and `parseBuildOptions` are
removed; the resolver's repo lookup reads each dep's `repo` (or auto-discovers),
and the build driver reads each dep's `ghc-options`. `addDep` writes a
dependency entry (shorthand when possible).

## 3. `zinc fmt` — one canonical writer

A single canonical writer produces the canonical layout; **both**
`renderWorkspace` (used by `zinc add`/`update`) and `zinc fmt` use it, so
`zinc add` output is already `fmt`-clean — one source of truth for the format.

Canonical form:
- `[workspace]` first, then `[dependencies]`.
- Dependencies sorted by name.
- Simple deps as shorthand (`name = "ref"`); multi-key deps as
  `[dependencies.name]` sub-tables with a stable key order (`tag`/`branch`/`rev`,
  then `repo`, then `ghc-options`).
- Consistent quoting, spacing, array style. Idempotent.

`zinc fmt` rewrites the workspace + member `zinc.toml` files to canonical form.
`zinc fmt --check` writes nothing and exits non-zero if any file isn't already
canonical (CI / agent non-interactive contract, `zinc-rdy.7`).

## 4. Migration

`zinc fmt` doubles as the migration: when it reads a legacy
`[registry]`/`[build-options]` layout, it folds those into per-dependency blocks
and writes the vertical form. Existing manifests to migrate: zinc's own
`zinc.toml`, and the five `test/fixtures/*` workspaces.

## 5. Decomposition

| Unit | Notes | Depends on |
|---|---|---|
| Vertical schema parser | `[dependencies.<name>]` + shorthand; drop registry/build-options; resolver `repo`, builder `ghc-options` | — |
| Canonical writer | one writer; `renderWorkspace` uses it; sorted, shorthand-vs-subtable | schema |
| `zinc fmt` + `--check` | command over the writer; idempotent; migrates legacy layout | writer |
| Migrate existing manifests | zinc.toml + test/fixtures via `zinc fmt` | fmt |

## 6. Coordination

Touches the manifest core (`parseWorkspace`, `parseBuildOptions`, `addDep`,
`renderWorkspace`) — the same area as `8kr` (module discovery), `8uh` (quirks
table), and `49o` (repo discovery). Sequence to avoid collisions. Not gated
(MVP done).
