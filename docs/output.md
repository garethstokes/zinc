---
title: Output
nav_order: 9
---

# Output

## Why

The same command serves two audiences that want opposite things. A person at a
terminal wants color, live progress, and readable errors. An agent or CI job
wants stable, parseable, deterministic data. zinc produces both from one source,
so they never drift.

## What

All output flows as a stream of structured events and diagnostics. Two renderers
consume it:

- A human renderer: status verbs, color, a live progress line during the build,
  and compiler-style error formatting. Color is disabled when output is not a
  terminal or when `NO_COLOR` is set.
- A machine renderer: the JSON envelope, and a newline-delimited JSON event
  stream for long operations so a caller can consume progress incrementally.

The mode is selected automatically (colored human output to a terminal, plain
when piped) and can be forced with `--json` or quieted with `--quiet`.

## How

Default, at a terminal:

```
$ zinc build
   Resolving  12 packages
   Building   [9/12] aeson
   Finished   dev in 3.2s · 12 packages (8 cached, 4 built)
```

Piped or in CI (no color, plain):

```
$ zinc build | tee build.log
```

Machine-readable, final result:

```
$ zinc build --json
```

Streaming events for long builds (one JSON object per line):

```
$ zinc build --json-stream
{"event":"compile-start","package":"aeson"}
{"event":"compile-done","package":"aeson","timeMs":410,"cached":false}
{"event":"finished","built":4,"cached":8,"totalMs":3200}
```

Quiet (errors only):

```
$ zinc build --quiet
```

## Examples

An error renders with its location and a suggested fix:

```
$ zinc build
error[ZINC_SAFE_HASKELL]: colour is not certified Safe
  Data.Colour is imported by a Safe module but built without -XSafe.
  try: build colour with -XSafe (see ghc-options for the dependency)
```

The same failure, as data:

```
$ zinc build --json | jq '.diagnostics[0].code'
"ZINC_SAFE_HASKELL"
```
