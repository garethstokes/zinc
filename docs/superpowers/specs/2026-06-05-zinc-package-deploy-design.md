# zinc — `zinc package`: deployable artifacts

**Status:** Design approved 2026-06-05
**Author:** Gareth (with Claude)

## 1. Goal

`zinc package <format>` turns a build into a **deployable artifact**, with zero
Nix knowledge — leveraging the flake zinc already generates and hides. Formats:
`docker` (OCI image), `static` (musl binary), `bundle` (portable single-file),
`nix` (closure via `nix copy`).

## 2. Disambiguation (three different "Docker/deploy" things)

- **`vwn` — build *inside* Docker/CI** (cache the store as a layer for fast
  fresh-FS builds). *Input side.*
- **`gtv` — distribute *zinc itself*** (the flake / `nix run github:…/zinc`).
- **This (`zinc package`) — deploy *the user's app*** built with zinc. *Output
  side.* New and distinct.

## 3. Nix is the packaging engine

zinc already hides Nix; Nix is a first-class packaging tool. From the generated
flake we get every deploy format for free:

| Format | Nix mechanism (hidden) | Deploy story | Reality |
|---|---|---|---|
| **docker** | `dockerTools.buildLayeredImage` | registry → k8s/containers | daemonless, reproducible, minimal (binary + runtime libs); broadest target |
| **static** | `pkgsStatic` / musl | `scp` and run anywhere | one self-contained file; **fiddliest** — C deps / not all packages link static |
| **bundle** | `nix bundle` (AppImage/arx) | one portable file | cheapest — wraps the existing derivation |
| **nix** | `nix copy` / `packages.default` | to Nix hosts | basically free; niche (target must run Nix) |

## 4. UX & mechanism

- **A distinct `zinc package <format>` verb** — *not* a `--target` value.
  `--target` is the *compile architecture* (native / wasm32-wasi); `package` is
  the *deploy format*. They compose (e.g. package a native build). `zinc package
  docker --tag myapp:1.0`, `zinc package static -o ./dist/myapp`, etc.
- **Mechanism:** the generated flake (`a6h.1`) gains deploy **outputs**
  (`dockerImage`, `static`, `bundle`). `zinc package <fmt>` resolves + builds the
  app, then drives the corresponding Nix output and emits the artifact — Nix
  hidden, auto-provisioned (`y03`). Builds on the build driver + the generated
  flake; no Dockerfile, no daemon, no `nix` literacy required.

## 5. Per-format notes

- **docker** — `buildLayeredImage` with the app as entrypoint + its runtime
  closure; `--tag`, and load (`docker load`) / push to a registry. Reproducible
  digests.
- **static** — `pkgsStatic`/musl static link. Honest about the limit: packages
  with C FFI / `system-libs` may not link static; surface a clear diagnostic
  (`ZINC_STATIC_UNSUPPORTED`) rather than a cryptic linker error — same shape as
  the wasm/Custom casualties.
- **bundle** — `nix bundle` the flake app → one portable executable. Lowest
  effort; broadest "copy and run".
- **nix** — `nix copy` the derivation to a Nix host (or just hand over the flake
  `packages.default`). Nearly free.

## 6. Decomposition

| Unit | Notes | Depends on |
|---|---|---|
| `zinc package` verb + flake deploy-output scaffolding | the verb; generated flake gains deploy outputs; wire the built app in | generated flake (`a6h.1`/`6hf.2`), `y03` |
| `package docker` (dockerTools) | OCI image, `--tag`, load/push | foundation |
| `package static` (pkgsStatic) | musl static + unsupported diagnostic | foundation |
| `package bundle` (nix bundle) | portable single-file | foundation |
| `package nix` (`nix copy`) | closure to a Nix host | foundation |

## 7. Gating / priority

P3 — a forward-looking deploy capability, after the active fronts. `docker` is
the headline (broadest reach); `bundle`/`nix` are cheap adds; `static` is the
fiddliest. Builds on the generated flake (`a6h.1`, wired by `6hf.2`) and
auto-provisioning (`y03`).
