---
title: The agent surface
nav_order: 8
---

# The agent surface

## Why

zinc is built to be driven by AI coding agents as well as people. An agent
should not have to parse human prose, guess what a failure means, or infer build
order. So zinc exposes structured, stable output and tells the caller what to do
next when something fails.

## What

- Every command accepts `--json` and emits a stable envelope.
- Failures are structured diagnostics, not opaque strings: each carries a stable
  code, a message, a location where relevant, and an actionable next step.
- Exit codes are stable per error category, so a caller can branch without
  parsing.
- `zinc prime` and `zinc onboard` give an agent context about the project.

Internally, every failure is a typed value rendered once at the boundary, so the
human message, the JSON form, the exit code, and the suggested fix all come from
one source and never disagree.

## How

Ask any command for JSON:

```
zinc build --json
zinc status --json
zinc explain aeson --json
```

The envelope:

```json
{
  "zinc": "0.1.0.0",
  "command": "build",
  "ok": false,
  "data": { "packages": [ { "name": "aeson", "status": "cached", "timeMs": 0 } ] },
  "diagnostics": [
    {
      "code": "ZINC_DEP_NO_GIT_REPO",
      "severity": "error",
      "title": "colour has no upstream git repository",
      "package": "colour",
      "nextAction": "zinc vendor colour"
    }
  ]
}
```

The `nextAction` is the fix, often a command to run, which is what turns a
failing build into a self-repairing loop for an agent.

## How an agent should use it

- Run `zinc prime` on entering a project to learn how to build, run, and test it,
  and the current toolchain.
- Drive commands with `--json`; read `data` for results and `diagnostics` for
  problems.
- On failure, act on `diagnostics[].nextAction`; branch on the exit code.

## Examples

```
$ zinc prime
zinc project: myapp (GHC 9.12.2)
Build: zinc build   Run: zinc run   Test: zinc test
…

$ zinc build --json | jq -r '.diagnostics[] | "\(.code): \(.nextAction)"'
ZINC_DEP_NO_GIT_REPO: zinc vendor colour
```

Persistent project memory is intentionally out of scope; that belongs to your
issue tracker. zinc's durable state is the lockfile and the content store.
