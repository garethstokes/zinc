# zinc — WebAssembly targets (GHC wasm32-wasi)

**Status:** Design approved 2026-06-05
**Author:** Gareth (with Claude)

## 1. Goal

`zinc build --target wasm32-wasi` — compile a Haskell workspace to WebAssembly
with **zero toolchain setup**. The normally-painful part (provisioning the GHC
wasm cross-compiler) is exactly what zinc already does well, so this is high
leverage on the existing model. WASI command modules first; browser
reactor + JS-FFI as a tracked follow-on in the same epic. Native stays the
default; `--target` is an explicit override.

## 2. What GHC WASM is (research, 2026-06)

- A cross-compiler targeting **`wasm32-wasi`** (mature in GHC 9.10–9.15; still a
  "tech preview"). Invoked as `wasm32-wasi-ghc` / `-ghc-pkg` / `-hsc2hs`
  (cross-prefix convention). [GHC WASM guide](https://downloads.haskell.org/ghc/latest/docs/users_guide/wasm.html)
- **Delivered as a Nix flake — [`ghc-wasm-meta`](https://github.com/haskell-wasm/ghc-wasm-meta).**
  This is the linchpin: the wasm toolchain is a Nix input.
- **Template Haskell + GHCi work** via a Node.js external interpreter GHC
  launches automatically (needs `node` on PATH).
- **Two flavors**: a `wasm32-wasi` **command module** (CLI/server, run via
  `wasmtime`/`node` — the simple default) and a **reactor module + JavaScript
  FFI** for the browser (`-optl-mexec-model=reactor`, explicit exports,
  `foreign import javascript`, no host filesystem).

## 3. Why it fits zinc

| zinc mechanism | WASM target |
|---|---|
| Nix provides the toolchain (hidden) | the generated flake adds `ghc-wasm-meta` + `node` + `wasmtime` and selects `wasm32-wasi-ghc`; the user never sets up the wasm toolchain |
| Drive `ghc --make` directly | drive `wasm32-wasi-ghc --make`; the build driver becomes **target-parameterized** (binary prefix + flags) |
| Content-addressed store | the **target triple joins the cache key** → build-once per `(machine, target)`; native + wasm closures coexist |
| git-native lockfile | target-independent — same deps; only toolchain + flags + cache key differ |

## 4. Architecture

- **Target abstraction.** A `Target` (`native | wasm32-wasi`) parameterizes the
  build driver: the ghc binary (`wasm32-wasi-ghc`), `ghc-pkg`/`hsc2hs` prefixes,
  and target-specific flags. Native is the default.
- **Toolchain provisioning.** Extend the hidden-Nix flake generation (`a6h`): when
  a wasm target is requested, add `ghc-wasm-meta` as a flake input + `node`
  (for TH/GHCi) + `wasmtime` (to run), and resolve the cross toolchain. Cached
  per the env key like the native toolchain.
- **Cache/store.** Add the target triple to the build cache key (`gec`) so wasm
  artifacts never collide with native; `~/.zinc/store` holds both.
- **`zinc run --target wasm32-wasi`** runs the produced `.wasm` via the
  Nix-provided `wasmtime`.
- **TH** works via the node external interpreter (provisioned in the env); the
  build driver must preserve GHC's external-interpreter path, not bypass it.

## 5. The limit — C FFI / system-libs

Pure-Haskell closures cross-compile cleanly. Packages with C sources or
`extra-libraries`/`system-libs` need wasi-sdk-built libs, and `nixpkgs` attrs →
wasm cross-libs largely don't exist. So the WASM MVP is **pure-Haskell
closures**; a closure member needing C/`system-libs` is **flagged
unsupported-for-wasm** with a clear diagnostic (`ZINC_WASM_UNSUPPORTED`) — same
shape as the `build-type: Custom` casualty, not a silent failure.

## 6. Browser flavor (follow-on, same epic)

Emit a **reactor module** (`-optl-mexec-model=reactor`) with explicit `--export`
names (linker dead-code-elims unexported symbols), support `foreign
import/export javascript`, and optionally emit JS glue. Browser context: no host
filesystem, browser globals only. Pursued after WASI command modules work.

## 7. Decomposition

| Unit | Notes | Depends on |
|---|---|---|
| Target abstraction + `ghc-wasm-meta` toolchain provisioning | `Target` param + flake input (ghc-wasm-meta/node/wasmtime) + cross-prefix driver | `a6h` (Nix env) |
| Target-keyed cache/store | target triple in the build key | foundation; `gec` |
| `zinc build --target wasm32-wasi` | CLI flag + drive wasm-ghc + `.wasm` output + flag C-dep unsupported | foundation, cache |
| `zinc run --target wasm32-wasi` (wasmtime) | run the `.wasm` | build |
| Browser: reactor module + JS FFI (+ glue) | follow-on | build |

## 8. Gating / priority

Post-MVP, P3 — a forward-looking differentiator, not on a current critical
path. Builds on the Nix env (`a6h`), the artifact cache (`gec`), and the build
driver. The WASI core is the valuable first slice; the browser flavor is the
flashier follow-on.

## 9. Latest dev-team notes (researched 2026-06, GHC 9.14.1)

Refinements from the GHC team's recent updates ([9.14.1 release](https://www.haskell.org/ghc/blog/20251219-ghc-9.14.1-released.html),
[wasm guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html)):

- **GHCi + Template Haskell now evaluate in wasm** (9.14+), via a custom
  dynamic-linking + node external-interpreter mechanism — including `foreign
  import javascript` from the browser. TH is solidly supported; `zinc repl
  --target wasm32-wasi` becomes feasible as a later add.
- **No native threads.** WASM/WASI has no multithreading (`wasi-threads` is only
  a proposal). The wasm build driver **must not pass `-threaded` /
  `-with-rtsopts=-N`** — note: zinc's *native* build does. Target-specific link
  flags, not shared.
- **Recent-runtime requirement.** The wasm module uses post-MVP extensions
  (multi-value, …) needing a recent `wasmtime`/browser. The Nix-pinned
  `wasmtime` (from the flake) satisfies the CLI runner.
- Still a **tech preview**, not in official bindists — via `ghc-wasm-meta`.
