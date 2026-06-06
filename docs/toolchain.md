---
title: The toolchain
nav_order: 6
---

# The toolchain

## Why

Getting a working GHC and the right system libraries is a common source of
friction. zinc removes it by provisioning the toolchain with Nix, while you never
write or run Nix yourself. The compiler version is pinned per workspace, so every
machine and every CI run builds with the same GHC.

## What

Each workspace has a generated `flake.nix` that pins nixpkgs, selects the GHC
chosen in `[workspace] ghc`, adds the union of all `system-libs` declared across
the build, and adds the preprocessors zinc may need (alex, happy; hsc2hs ships
with GHC). zinc evaluates this once and caches the result, re-evaluating only
when the GHC version or `system-libs` change. The compiler, `ghc-pkg`, and
preprocessors come from that environment.

`system-libs` entries are nixpkgs attribute names. Naming the attribute directly
(for example `zlib`) sidesteps the problem of mapping an `extra-libraries` name
to a system package.

## How

The compiler is set in the manifest:

```toml
[workspace]
ghc = "9.6.5"
```

System libraries are declared per build component and Nix supplies them:

```toml
[build.lib]
source-dirs = ["src"]
system-libs = ["zlib", "pkg-config"]
```

zinc generates and owns `flake.nix`; it regenerates when the GHC version or
`system-libs` set changes. You can still enter the environment manually with
`nix develop` if you want a shell with the same compiler.

## Examples

Pin a specific compiler for the whole workspace:

```toml
[workspace]
members = ["packages/myapp"]
ghc = "9.8.2"
```

Add a C library a dependency needs:

```toml
[build.lib]
source-dirs = ["src"]
system-libs = ["zlib"]
```

Limitation: because the toolchain comes from Nix, zinc targets macOS and Linux.
Windows is out of scope.
