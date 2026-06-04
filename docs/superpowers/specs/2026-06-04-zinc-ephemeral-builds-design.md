# zinc — Ephemeral / CI / Docker builds

**Status:** Design approved 2026-06-04
**Author:** Gareth (with Claude)

## 1. Goal

Make `zinc build` fast in environments with a **fresh filesystem each build**
(Docker, CI), where zinc's local persistence — the content-addressed
`~/.zinc/store` and the `.zinc/build` outputdir — is thrown away every run.

## 2. The tension, and why zinc is well-suited

zinc's speed model is "build the closure once per machine, cache it." A fresh FS
defeats that: every build is cold (recompile the whole dependency closure +
members). But zinc's artifact model is **content-addressed and deterministic**
(`buildCacheKey = sha256(rev, ghc-version, dep unit-ids, options)`; the lockfile
pins commit + sha256), which is exactly what external caches need. So the answer
is not to make a fresh FS incremental (impossible) but to **externalize the
store** — cacheable locally, then shared remotely. Two layers.

Already shipped and relied on: a relocatable store (`ZINC_STORE`, `zinc-6s4`) and
a working local artifact cache (`zinc-gec`: store-miss → compile → cache → reuse
`.conf` on hit).

## 3. Layer 1 — locally cacheable ephemeral builds (ships first)

- **`zinc build --deps-only`** (alias `zinc warm`): resolve + build/populate the
  dependency *closure* into the store, **without** building workspace members.
  This is the key enabler: it lets the slow-stable closure be its own Docker
  layer / CI cache entry, cleanly separated from fast-changing app source.
- **Docker/CI recipe**: a documented multi-stage pattern —
  1. copy `zinc.toml` + `zinc.lock`,
  2. `zinc build --deps-only` into a `ZINC_STORE` cache mount
     (`--mount=type=cache` / CI cache keyed on `hash(zinc.lock, ghc-version)`),
  3. copy source, build members.
  Unchanged lock → the closure layer is a cache hit; only members recompile.
  Optionally a `zinc dockerfile` command emitting this snippet.

Reproducibility is a free correctness win: lock-pinned + content-addressed means
same lock → same closure → same cache key, no "works on my machine."

## 4. Layer 2 — remote shared artifact cache (sequenced after L1)

The remote tier of the local artifact cache (`gec`): build the closure **once
globally**, every Docker/CI run pulls prebuilt artifacts instead of compiling.

- **`CacheBackend` interface**, first impl **HTTP content-addressed**: pull =
  `GET <url>/<key>` for the artifact + its `.conf`; write = `PUT` / a publish
  step. Works behind any static host / bucket + proxy (the Nix binary-cache
  model). S3 / OCI backends drop in behind the interface later.
- **Pull integration** (the one surgical change): the closure builder's cache
  check (`Orchestrate.produceOne`) lookup order becomes **local store → remote
  cache → compile**. On a remote hit: fetch artifact + `.conf` into the local
  store, **verify content hash**, register. On miss: compile as today.
- **Push**: `zinc cache push` / a CI publish step uploads built artifacts under
  their key. Not every local build — an explicit publish.
- **Config + trust**: cache read-URL(s) + optional write-URL via `ZINC_CACHE_URL`
  / a `[cache]` table. **Private caches only** (ones you control) + content-hash
  verification on every fetch. A verification hook is left in place for signing.

### Trust ordering (decided)

Content-addressing makes the cache **safe-by-construction against corruption /
tampering-in-transit** (verify the fetched blob's hash). The only thing signing
adds is protection against a *maliciously poisoned* cache serving bad output for
a legitimate input key — which only matters for untrusted/public caches. Hence:
private caches + hash-verify now; **Nix-style signed caches deferred** as a later
layer for public/shared use.

## 5. Decomposition

| Unit | Layer | Notes | Depends on |
|---|---|---|---|
| `zinc build --deps-only` / `warm` | L1 | closure-only build | build orchestration (`nlx`) |
| Docker/CI recipe (+ optional `zinc dockerfile`) | L1 | doc + cache-key convention | `--deps-only` |
| `CacheBackend` interface + HTTP impl (pull) | L2 | content-addressed GET | local cache (`gec`) |
| Remote pull integration (store → remote → compile) | L2 | hooks `produceOne`; hash-verify on fetch | backend, `gec` |
| Remote push (`zinc cache push` / CI publish) | L2 | upload by key | backend |
| Cache config + trust (URLs, private-only, verify) | L2 | signing hook stubbed | backend |
| Signed cache (public/shared) | deferred | later layer | config/trust |

## 6. Sequencing & gating

L1 first (immediate value from shipped primitives) → L2. Gated behind self-host
(`zinc-576`) like other post-MVP work. Relationships: extends the local artifact
cache (`zinc-gec`); relates to `c3z.3` (Nix-NAR-compatible hashing — relevant if
Nix-cache interop is ever wanted), `zinc-6s4` (relocatable store ✅), and
`zinc-rdy.8` (store concurrency, for parallel CI).

## 7. Out of scope

Making a genuinely fresh FS *incremental* (needs persistence — that's what the
cache layers externalize) · caching the per-member `.zinc/build` outputdir
(changes every commit; the closure cache is the win) · signed/public caches
(deferred, §4) · S3/OCI backends (interface accommodates them; HTTP ships first).
