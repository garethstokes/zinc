---
title: Performance
nav_order: 10
---

# Performance

## Why

Speed is a goal, not an accident — so it should be measured. zinc records how
long each build takes and where the time goes, so you can see whether the cache
is working, which dependencies dominate, and whether a change made things slower.
The data is local; nothing is sent anywhere.

## What

Each build emits a timing block: total time, a breakdown by phase (resolve,
fetch, compile, register, link, member build), per-package compile times, and
cache hit/miss counts. Records are appended to a local `.zinc/metrics.jsonl`.
`zinc perf` reads that history and reports.

## How

Build timings appear in the build summary and in `--json` output. To analyze the
history:

```
zinc perf
```

`zinc perf` reports the slowest dependencies (by cumulative and average compile
time), the cache hit rate over recent builds, per-command latency, and flags a
build that regressed against the rolling baseline. Add `--json` for the data.

## Examples

```
$ zinc perf
Builds analyzed: 24

Slowest dependencies (avg compile)
  aeson         8.1s
  lens          6.4s
  vector        2.2s

Cache hit rate (last 10 builds): 0.86
Build p50 / p95: 1.4s / 31.0s
```

The per-build summary surfaces the cache effect directly:

```
$ zinc build
   Finished dev in 3.2s · 47 packages (43 cached, 4 built)
```

A regression against the baseline is flagged:

```
$ zinc build
   Finished dev in 12.0s · 47 packages (12 cached, 35 built)
   note: 3.7x slower than baseline; cache hit rate dropped 0.9 -> 0.26
```
