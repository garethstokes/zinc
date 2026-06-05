---
title: Getting started
nav_order: 2
---

# Getting started

## Why

Starting a Haskell project usually means choosing a build tool, writing a cabal
file, picking dependency bounds, and hoping the solver finds a plan. zinc removes
those steps. You create a project, add dependencies by name, and build. The
toolchain is provisioned for you; dependencies resolve to exact commits with no
solver.

## What

zinc installs as a single binary. It requires Nix (used internally to provide
GHC and system libraries — you do not write or run Nix yourself). A new project
is a workspace with one package, a `zinc.toml` manifest, and a generated
`flake.nix` that pins the compiler.

## How

Install zinc from its flake:

```
nix profile install github:garethstokes/zinc
```

Or run it without installing:

```
nix run github:garethstokes/zinc -- --help
```

Create a project, then build and run it:

```
zinc new myapp
cd myapp
zinc build
zinc run
```

`zinc new` scaffolds a single-package project, a managed `flake.nix`, a
`.gitignore`, and initializes a git repository. For a multi-package layout, pass
`--workspace`.

## Examples

A fresh project:

```
$ zinc new myapp
$ cd myapp
$ zinc run
Hello from myapp!
```

The scaffolded layout:

```
myapp/
  zinc.toml          # package, build components, dependencies
  flake.nix          # managed toolchain (GHC + system libs)
  .gitignore
  src/
  app/Main.hs
  test/
```

Add a dependency and build against it:

```
$ zinc add aeson
$ zinc build
```

See [The manifest](manifest.md) for the structure of `zinc.toml`, and
[Dependencies and the lockfile](dependencies.md) for how `zinc add` resolves and
pins.
