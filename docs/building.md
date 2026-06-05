---
title: Building
nav_order: 5
---

# Building

## Why

zinc builds Haskell by driving GHC directly, rather than delegating to cabal's
build machinery. This keeps the build predictable, lets zinc cache artifacts by
content, and gives a fast edit-build loop. Speed and reproducibility are the
goals: a dependency compiles once per machine, and switching branches reuses the
result.

## What

`zinc build` resolves the dependency closure from the lockfile, provisions the
toolchain, builds the closure in dependency order, then builds your workspace
members. Two kinds of code are handled differently:

- Dependencies (the closure) are content-addressed and cached. A dependency is
  built once per machine, keyed by its commit, the GHC version, its own
  dependencies, and its build options. A cache hit registers the prebuilt
  artifact and skips compilation.
- Workspace members (your code) build into `.zinc/build` and rely on GHC's own
  recompilation checking, so only changed modules — and the call sites whose
  imported interfaces actually changed — recompile.

A dependency describes itself in one of two ways. A zinc-native package has a
`[build]` block in its `zinc.toml`. An upstream package is read from its
`.cabal` file: zinc uses the Cabal library as a parser only (never its builder)
to derive the build, synthesizing `Paths_<pkg>.hs` and `cabal_macros.h` and
running preprocessors (alex, happy, hsc2hs) as needed.

## How

Build the whole workspace:

```
zinc build
```

Build a single member:

```
zinc build mymember
```

Build only the dependency closure, without your members — useful as a cacheable
layer in CI or Docker:

```
zinc build --deps-only
```

The content store lives at `~/.zinc/store` (override with `ZINC_STORE`). It is
shared across projects and branches, so a dependency built for one checkout is
reused by another. `zinc clean` removes a project's build output (`.zinc/`) while
keeping the store; `zinc gc` garbage-collects the store.

## Examples

A cold build compiles the closure once; a warm rebuild reuses it:

```
$ zinc build
   Compiling aeson, scientific, … (12 packages)
   Finished in 41.2s · 12 packages (0 cached, 12 built)

$ zinc build           # nothing changed
   Finished in 0.3s · 12 packages (12 cached, 0 built)
```

Edit one module and rebuild — only the affected modules recompile:

```
$ zinc build
   Compiling Myapp.Core
   Finished in 1.1s
```

Build the closure as its own step (the slow, stable part), then your source (the
fast-changing part):

```
$ zinc build --deps-only
$ zinc build
```

Known limitation: packages with `build-type: Custom` (a real `Setup.hs`) are not
built, because zinc drives GHC directly rather than running Setup scripts.
