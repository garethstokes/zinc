# zinc — Command performance tracking

**Status:** Design approved 2026-06-04
**Author:** Gareth (with Claude)

## 1. Goal

Measure per-command performance and track it over time, to **drive and validate
devex/speed work**. You can't tell whether the inner-loop (`zinc-5ko`) or caching
(`zinc-vwn`) optimizations actually helped without measuring — this is their
feedback loop. Strictly **local; no telemetry / phone-home** (consistent with
zinc's local/explicit ethos).

## 2. Layer 1 — per-command timing (measurement)

A `timing` block in the JSON envelope (the agent-devex diagnostic/report core,
`zinc-rdy.1`) for **every** command:

```
timing: {
  totalMs,
  phases: { resolve, provision, fetch, compile, register, link, member },
  cache:  { hits, misses, pkgsBuilt, pkgsCached }
}
```

Per-package `timeMs` (already in the build report, `zinc-rdy.2`) rolls up into
`compile`. Instrument phases with a monotonic clock; overhead is negligible.
Lives in the same envelope as the rest of the structured output.

## 3. Layer 2 — history + analysis

### 3.1 Persisted history

Append each invocation's record to project-local **`.zinc/metrics.jsonl`**:
the `timing` block plus `{ command, argsSummary, lockHash, ghcVersion,
timestamp }`. Append-only, gitignored, machine-local. It **survives
`zinc clean`** (clean removes `.zinc/build` + the pkgdb, not the root
`.zinc/metrics.jsonl`).

### 3.2 `zinc perf` analyzer

Read `.zinc/metrics.jsonl` and report (with `--json`):

- **Slowest dependencies** — by cumulative / average compile time.
- **Cache hit-rate trend** — hits vs misses over the last N builds.
- **Command latency** — p50 / p95 per command.
- **Regression detection** — current run vs a rolling-median baseline; flag
  slowdowns over a threshold ("build 2× slower than baseline; cache hit rate
  dropped from 0.9 → 0.4").

## 4. Decomposition

| Unit | Layer | Depends on |
|---|---|---|
| Phase timing instrumentation + `timing` block in the envelope | L1 | `rdy.1` (envelope) |
| Persist timing records → `.zinc/metrics.jsonl` | L2 | L1 |
| `zinc perf` analyzer (slowest deps, hit rate, p50/p95, regression) | L2 | persist |

## 5. Relationships & gating

- **Depends on the agent-devex diagnostic core (`zinc-rdy.1`)** for the JSON
  envelope the `timing` block hangs off.
- **Serves `zinc-5ko` (inner-loop incrementality) and `zinc-vwn` (caching)** as
  their measurement/validation layer — measure before/after, catch regressions.
- Not otherwise gated (the MVP / self-host is done).

## 6. Out of scope

Remote/aggregated telemetry · machine-wide cross-project rollup (project-local
first; could aggregate `~/.zinc` later) · flamegraphs / per-module profiling
(phase + per-package granularity is the actionable level for a build tool).
