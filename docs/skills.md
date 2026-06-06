---
title: Skills
nav_order: 14
---

# Skills

## Why

zinc is, underneath, a git-native, content-addressed, lockfile-pinned package
installer with an agent-friendly CLI. Agentic "skills" (capabilities an AI
harness loads) need exactly that: fetched from git, pinned, and verified. So zinc
can install skills the same way it installs code, giving an agent's capabilities
the same reproducibility and auditability as its dependencies.

## What

A skill is a package kind. `zinc skill add` fetches a skill from a git
repository, pins it by commit and content hash in the lockfile, and links it into
the harness's skills directory. The build toolchain is not involved: installing a
skill never invokes Nix or GHC.

A skill is a directory with a manifest (the Claude Code `SKILL.md` format: a name
and description, plus the skill's content). The manifest's name determines where
it installs.

## How

```
zinc skill add <repo> [--ref <ref>]
zinc skill list
zinc skill remove <name>
zinc skill sync
```

`zinc skill add` resolves and clones the skill at its ref into the content store,
verifies its hash, records it in `zinc.lock`, and symlinks it into
`.claude/skills/<name>`. `zinc skill sync` re-materializes every locked skill, so
committing `zinc.toml` and `zinc.lock` lets a teammate or a fresh agent run `zinc
skill sync` and get the exact same, hash-verified set of skills.

## Examples

```
$ zinc skill add https://github.com/org/brainstorming-skill --ref v1
   Added skill 'brainstorming' (pinned a1b2c3d)

$ zinc skill list
brainstorming   https://github.com/org/brainstorming-skill   v1 (a1b2c3d)

$ zinc skill sync          # on a fresh checkout
   Materialized 1 skill from zinc.lock
```

Because skills are pinned and content-verified like any dependency, a skill
cannot change under you, and the set is reproducible from the lockfile.
