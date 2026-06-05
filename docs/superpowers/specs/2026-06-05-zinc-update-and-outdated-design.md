# zinc — Updating packages: `update` + `outdated`

**Status:** Design approved 2026-06-05
**Author:** Gareth (with Claude)

## 1. Current state & gaps

`zinc update` exists (`nlx.4`) but is thin:
- It **ignores its `[pkg]` argument** — `zinc update aeson` re-freezes the *whole*
  workspace regardless.
- **No before→after diff** — it rewrites `zinc.lock` silently, violating the
  resolution-transparency principle (§2).
- **No `--dry-run`** (preview without writing).
- **No `zinc outdated`** (what *could* update, read-only).

## 2. `zinc outdated` (read-only)

A non-mutating report: per dependency, current version (from the lock) vs newest
available — **without touching `zinc.lock` or the manifest**.

- **Mechanism:** current = the lock's pinned rev/tag; latest = `git ls-remote
  --tags <repo>` (no clone — cheap, parallelizable) → newest release tag (the
  `"*"`-resolution logic). For vendored/no-git deps (`b1z`), latest comes from
  **Hackage**, not git. Flag major-version jumps.
- **Direct vs transitive (decided):** defaults to **direct deps** (your
  `[dependencies]` — the ones you can actually bump); `--all` includes the whole
  closure (informational — transitives move via their parent or a root override,
  not directly). Boot libs are never reported (they track the GHC version).
- **"Latest available", not "latest compatible":** zinc has no version bounds, so
  `outdated` means "a newer tag exists," not solver-compatible. The major flag is
  the risk signal.
- **vs `update --dry-run`:** `outdated` = "what newer versions exist" (incl.
  pinned deps); `--dry-run` = "what an actual update would change." Different
  questions; both read-only.

## 3. `zinc update`

- **Honor `<pkg>` (decided):** `zinc update aeson` updates just that dep and
  re-resolves the affected sub-closure (fix the dropped arg). Bare `zinc update`
  bumps all movable deps.
- **Closure-delta diff (decided):** print the change as a three-way set delta
  over the closure — **changed** (`aeson 2.2.3.0 → 2.3.0.0`, old→new rev),
  **added**, **removed** — *including ripples* (see §4). This is mandatory
  output, not optional.
- **`--dry-run` (decided):** compute and print the diff **without writing**
  `zinc.lock`.
- **Bump scope (default, revisitable):** bump to the **absolute latest** release
  tag (consistent with `"*"`=latest, no bounds), and the diff **flags major
  crossings loudly** (`aeson 2.x → 3.x — MAJOR`). Pin (`tag = …`) to hold a dep
  back. `--minor`/`--patch` (semver-scoped bumps via tag-filtering, still no
  solver) noted as a possible later convenience — not in scope yet.
- Pinned deps are held by `zinc update`; moving a pin is an explicit re-pin
  (edit the manifest / re-`zinc add`).

## 4. Closure interaction (why this is non-trivial)

zinc's graph is the **closure** (full transitive set, self-describing walk,
lock-pinned), with **one ref per package name** (§2). Consequences:

- **Ripple:** bumping one dep re-walks its manifest, and because a name has a
  *single* ref across the whole closure, a shared transitive can shift for
  *other* packages too. e.g. `zinc update aeson` pulls a newer `scientific` that
  `attoparsec` also depends on → `scientific` bumps for `attoparsec` as a side
  effect. **The diff must surface these ripples**, since zinc resolves by
  one-ref-per-name rather than a per-package solver — the blast radius is wider.
- **Re-walk:** update re-resolves the closure → packages can be added, removed,
  or changed; the diff is a delta over the whole closure, not just the named dep.
- **Direct vs transitive control:** you bump direct deps; transitives move via
  their parent's manifest or a root override.

## 5. Decomposition

| Unit | Notes | Depends on |
|---|---|---|
| `zinc outdated` | read-only current-vs-latest (`ls-remote` + Hackage for vendored); direct default + `--all` closure; major flag | tag-listing (exists) |
| Closure-delta diff | changed/added/removed over the closure, incl. one-ref-per-name ripples | resolver |
| `zinc update <pkg>` (honor the arg) | per-package + sub-closure re-resolve; prints the diff | diff |
| `update --dry-run` | compute + print the diff, no write | diff |

## 6. Gating / priority

P2 — a real devex gap (`update` is broken on its `<pkg>` arg and silent), behind
the active fronts. `outdated` is independent (reuses `ls-remote`); the diff is the
core that `update` + `--dry-run` share.
