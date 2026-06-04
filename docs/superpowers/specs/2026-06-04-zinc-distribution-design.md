# zinc — Distribution

**Status:** Design approved 2026-06-04
**Author:** Gareth (with Claude)

## 1. Goal & audience

Distribute zinc to **Haskell early-adopters** — people happy to live in a
Nix/flake workflow and tolerate light friction (e.g. "install Nix, then
`nix run`"). This is *not* the public-mass-adoption tier; install polish such as
`curl | sh` installers, a Homebrew tap, or a nixpkgs submission is explicitly
out of scope for now (see §7).

## 2. The core constraint

zinc shells out to Nix at runtime (`nix print-dev-env`, via `Zinc.Env`) to get
the GHC toolchain + system libraries. So "distribute zinc" is two stacked
problems: get the `zinc` binary onto a machine, **and** ensure Nix is present —
without the user feeling they had to learn Nix (zinc's "hidden Nix" thesis).

For the chosen audience this largely collapses: the primary channel is **the
flake itself**, and you cannot `nix run` a flake without Nix. If the channel
works, Nix is by definition present. The binary-without-Nix case is handled by a
preflight (§4), not by bundling Nix.

## 3. Channels (decided)

- **A — Flake-first (v1, now).** The install *is* the flake.
- **B — GitHub release binaries (fast-follow, deferred).** Prebuilt binaries
  attached to tagged releases for users who want `zinc` on `PATH` without
  `nix profile install`.
- **C — Full polish (deferred indefinitely).** `curl | sh` installer,
  nixpkgs/Homebrew. This is the public-adoption tier and is not pursued now.

Decision: **ship A now, track B as a documented fast-follow, defer C.**

## 4. Approach A — flake-first (v1)

### 4.1 Flake outputs — the install surface

`flake.nix` today is **devShell-only**. Extend it with:

- `packages.<system>.default` — the zinc derivation, built from the cabal
  project against the same Nix-provided GHC the devShell already pins (so the
  release toolchain matches the dev toolchain — dogfoods zinc's thesis).
- `apps.<system>.default` — `{ type = "app"; program = ".../bin/zinc"; }`, so
  `nix run github:<owner>/zinc` works.
- `overlays.default` — exposes `zinc` so other flakes / nixpkgs configs pull it
  in.
- keep `devShells` unchanged.

The existing systems list (`x86_64-linux`, `aarch64-linux`, `x86_64-darwin`,
`aarch64-darwin`) already covers all targets, including B's binary matrix.

Resulting install paths:
```
nix run github:<owner>/zinc                 # one-shot
nix profile install github:<owner>/zinc     # put zinc on PATH
github:<owner>/zinc/v0.1.0                   # pin an exact release
```

### 4.2 Nix preflight — the one code change

Before any command that shells out to Nix (`build`, `run`, `repl`, `test`,
`add`, `update` — the `Zinc.Env.nixPrintDevEnv` path), zinc checks that `nix` is
on `PATH` and that flakes are usable. If Nix is absent or unusable, zinc prints
a clear message naming the prerequisite and a one-line install command (the
Determinate Systems installer) and exits non-zero. **zinc never installs or
mutates Nix itself** — detect and guide only.

This is the only product code change for distribution. It matters mainly for the
B path (binary users may lack Nix) but is cheap and improves first-run UX
everywhere.

### 4.3 Versioning

`zinc --version` (existing `Zinc.Version`) reports the version, sourced from the
cabal package version. A release is a git tag `vX.Y.Z` matching that version.
Optionally stamp the short git commit alongside the version. Because flake refs
are pinnable, the tag *is* the release artifact for A.

### 4.4 Docs

A README install section covering: the `nix run` / `nix profile install`
commands, the Nix prerequisite + installer link, the `overlays.default` snippet,
and the pinnable tag ref. Keep a `CHANGELOG`.

### 4.5 Release process (light, no CI)

Cut a release by tagging `vX.Y.Z`. No CI is required for A — the flake ref is
the distributable. B introduces CI.

## 5. Approach B — release binaries (fast-follow, deferred)

A GitHub Actions workflow `nix build`s zinc across the four systems and attaches
the resulting binaries to the GitHub Release for the tag. Reuses A's preflight
(§4.2), since downloaded-binary users are the ones most likely to lack Nix.
Cost: CI setup, macOS runners, and the standing reality that the binary still
needs Nix at runtime. Pursue when there is actual demand for a PATH binary.

## 6. Testing

- `nix build .#packages.<system>.default` produces a runnable `zinc`.
- `nix flake check` passes.
- Preflight: unit-test the Nix-detection logic; golden-test the absent-Nix
  guidance message.

## 7. Deferred / out of scope

`curl | sh` installer · Homebrew tap · nixpkgs submission · self-update
(`zinc self-update`) · Windows (no Nix; already out of scope project-wide).

## 8. Dogfood north-star (noted, unscheduled)

Once zinc reaches its self-hosting MVP, the release artifact could eventually be
produced by `zinc build` itself rather than `nix build`/cabal — the ultimate
dogfood. Recorded as direction, not scheduled work.
