---
title: Dependencies and the lockfile
nav_order: 4
---

# Dependencies and the lockfile

## Why

cabal's pain is concentrated in the solver and the Hackage version dance. zinc
removes both. Every dependency is a git repository at an exact ref; there is one
ref per package name across the whole workspace; the resolved commit is frozen
in a lockfile. No solver runs, so there are no solver errors.

## What

A dependency is `name = <ref>` plus a repository. The ref is a tag, branch, or
commit (`"*"` means the latest release tag). The repository is resolved at `zinc
add` time and frozen, so you usually only write the name and ref.

GHC boot libraries (`base`, `text`, `bytestring`, `containers`, and so on) ship
with the Nix-provided compiler and are never fetched.

The dependency graph is the transitive closure, discovered by reading each
dependency's own manifest (zinc-native) or its `.cabal` file. The workspace lists
only its direct dependencies; transitive ones are discovered. Across the closure,
each package name resolves to exactly one ref — if two packages ask for different
refs, the workspace-root override wins, otherwise the latest referenced ref.

## How

Add a dependency. zinc discovers the full transitive closure, prints it for
review, and freezes it:

```
zinc add aeson
```

`zinc add` walks the closure with `ghc-pkg` and Hackage `source-repository`
metadata to find each package's git repository, then writes the refs into
`zinc.toml` and the pinned commits and content hashes into `zinc.lock`. Resolution
is always shown — the resolved closure prints as a table, and any conflict (where
the one-ref-per-name rule chose a ref) is called out.

`zinc.lock` pins every package in the closure:

```toml
[[locked]]
name   = "aeson"
repo   = "https://github.com/haskell/aeson"
rev    = "a1b2c3d…"              # the resolved commit, not the tag
sha256 = "sha256:…"             # verifies the fetch; part of the cache key
depends = ["scientific", "…"]   # flattened for fast graph load
```

Commit `zinc.toml` and `zinc.lock`; a clean checkout reproduces the exact same
closure.

## Vendoring packages with no git repository

A few packages have no upstream git repository (darcs-era ones such as `colour`
or `tf-random`). When `zinc add` finds one, it flags it rather than failing into
a dead end, and tells you the one command to recover:

```
zinc vendor colour
```

`zinc vendor` fetches the package's Hackage tarball into the content store, pins
it by `sha256`, and records it as a vendored source — after which `zinc add`
proceeds. Build never resolves through Hackage; it reads the pinned source from
the store.

## Examples

Pin an exact version, then build:

```
$ zinc add aeson           # resolves the closure, freezes zinc.lock
$ zinc build
```

Override a dependency's repository (a fork) in `zinc.toml`:

```toml
[dependencies.aeson]
tag  = "v2.3.0.0"
repo = "https://github.com/myorg/aeson"
```

Reproduce a project's dependencies on a fresh checkout:

```
$ git clone … && cd …
$ zinc build               # builds the exact closure from zinc.lock
```
