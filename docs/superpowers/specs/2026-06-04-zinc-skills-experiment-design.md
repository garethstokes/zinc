# zinc — Skills as a package kind (thin experiment)

**Status:** Design approved 2026-06-04
**Author:** Gareth (with Claude)

## 1. Goal

Let zinc install **agentic "skills"** (Claude Code skills: a directory with a
`SKILL.md`) as a package kind, fetched git-native, pinned, and content-verified —
so an agent's *capabilities* get the same reproducibility and auditability zinc
gives code dependencies.

Scoped as a **thin experiment** (decided 2026-06-04): validate the concept
cheaply with no changes to the Haskell build model. Positioning (whether zinc
becomes a general "agent's package manager") is deferred until the experiment
proves out.

## 2. Why this fits zinc

Strip away the Haskell-specific build step and zinc is a git-native,
content-addressed, lockfile-pinned package fetcher with an agent-native CLI. A
skill needs exactly that machinery; only the final step differs:

| zinc capability | Haskell package | Skill |
|---|---|---|
| resolve (repo + ref) | ✅ reused | ✅ reused |
| fetch → content store | ✅ reused | ✅ reused |
| pin commit + sha256 in lock | ✅ reused | ✅ reused |
| content-hash verification (§8, `c3z.2`) | ✅ reused | ✅ reused |
| **install action** | build with ghc → register | **symlink store tree → `.claude/skills/<name>`** |
| toolchain | Nix GHC | **none — Nix never invoked** |

Skills inherit zinc's properties that matter *more* for skills than libraries:
reproducible + auditable (a skill is executable instructions an agent follows;
pinning + hashing + a visible resolution table is real supply-chain integrity),
and build-once-per-machine via the store.

## 3. Surface — an isolated `zinc skill` subcommand

Deliberately *not* overloading `zinc add`/`build`, so the experiment cannot
destabilize the package model.

- `zinc skill add <repo> [--ref <ref>]` — resolve → clone@ref into the content
  store → verify sha256 → read `SKILL.md` → record in `zinc.lock` → symlink
  `store/<hash>/ → .claude/skills/<name>`.
- `zinc skill list` — installed skills: name, repo, pinned rev. Also surfaced in
  `zinc status`.
- `zinc skill remove <name>` — remove the symlink and the lock entry.
- `zinc skill sync` — re-materialize every locked skill's symlink from
  `zinc.lock`. **The payoff over `git clone`:** commit `zinc.toml` + `zinc.lock`,
  and a teammate or a fresh agent runs `zinc skill sync` to get the exact pinned,
  hash-verified skill set.

The skill path never invokes the Nix preflight (no toolchain needed).

## 4. Manifest + lock (small, reused)

Workspace `zinc.toml` gains a `[skills]` table:
```toml
[skills]
brainstorming = { repo = "https://github.com/org/brainstorming-skill", ref = "v1" }
```
Frozen into `zinc.lock` as `[[skill]]` blocks using the existing freeze
machinery:
```toml
[[skill]]
name   = "brainstorming"
repo   = "https://github.com/org/brainstorming-skill"
rev    = "a1b2c3d…"
sha256 = "sha256:…"
```
The install directory is the skill's own `name` from its `SKILL.md` frontmatter
(self-describing, mirroring how packages declare their identity).

## 5. Validation (thin)

Require a `SKILL.md` at the repo root (or `--ref`'d tree) with `name` +
`description` frontmatter — the Claude Code format, so existing skills work
unchanged. Link the whole tree as-is. No script execution, no skill→skill deps.

## 6. Out of scope (deferred until the experiment proves out)

skill → skill dependencies · skills depending on Haskell packages · script/exec
validation or sandboxing · global (`~/.claude/skills`) install · a `--global`
flag / configurable target · publishing skills · other agent harnesses · auto
`update`. None are precluded; all are additive later.

## 7. Decomposition

| Unit | Notes | Depends on |
|---|---|---|
| Skill manifest + lock + `SKILL.md` reader | `[skills]` parse, `[[skill]]` freeze, frontmatter name/description | — |
| `zinc skill add` | clone@ref → verify → read SKILL.md → lock → symlink | manifest/lock unit |
| `zinc skill list` / `remove` (+ `status`) | inspect + unlink | `add` |
| `zinc skill sync` | re-materialize locked skills on fresh checkout | `add` |

## 8. Gating

Its own small epic. **Technically independent** of the Haskell build path (no
Nix/ghc), so it could be built anytime — but gated behind self-host (`zinc-576`)
to honour "finish the MVP first." Un-gate on request to experiment sooner.
