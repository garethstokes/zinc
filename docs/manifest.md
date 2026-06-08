---
title: The manifest
nav_order: 3
---

# The manifest (`zinc.toml`)

## Why

A build tool needs to know what to compile and what it depends on. zinc keeps
that in one readable TOML file per package, plus a workspace file that lists
members and shared dependencies. There are no separate package files to
hand-edit and no version bounds to maintain.

## What

A workspace root declares its members and the dependencies shared across them. A
package declares its identity and its build components (a library, executables,
test suites). A single-package project is a workspace with one implicit member,
so a lone `zinc.toml` can carry both the workspace and the package sections.

Modules are discovered automatically from the source directories. There is no
exposed/other module distinction, and every module is importable by dependents.

## How

Workspace root:

```toml
[workspace]
members = ["packages/myapp", "packages/mylib"]
ghc = "9.6.5"                      # Nix pins exactly this compiler

[dependencies]
aeson = "v2.3.0.0"                 # a ref; the repo is resolved and frozen into zinc.lock
hspec = "*"                        # "*" = latest release tag

[dependencies.colour]              # full form: a repo override and/or ghc flags
repo        = "https://github.com/garethstokes/color.git"
ghc-options = ["-XSafe"]
```

A dependency is one entry. The common case is a one-line ref (`aeson =
"v2.3.0.0"` or `"*"`); the repo is resolved at `zinc add` time and frozen in the
lockfile. Open a `[dependencies.<name>]` block only to override the repo or pass
per-dependency ghc flags.

A package (a member, or the same file in a single-package project):

```toml
[package]
name = "myapp"
version = "0.1.0"

[build.lib]
source-dirs = ["src"]              # every module under here is discovered and exposed
extensions  = ["OverloadedStrings", "LambdaCase"]
ghc-options = ["-Wall"]
depends     = ["aeson", "mylib"]   # external deps and workspace siblings, by name
system-libs = ["zlib"]             # nixpkgs attribute names; Nix supplies them

[build.exe.myapp]
source-dirs = ["app"]
main        = "Main.hs"
depends     = ["myapp"]

[build.test.spec]
source-dirs = ["test"]
main        = "Spec.hs"
depends     = ["myapp", "hspec"]
```

### Git-derived versions

`version` is normally a literal (`"0.1.0"`). Setting it to the sentinel
`"git"` opts the package into git-derived versioning instead:

```toml
[package]
name = "myapp"
version = "git"
```

The base version is then read from the nearest git tag reachable from `HEAD`
(`git describe`-style — a leading `v` and any `<pkg>-`/`<pkg>/` scope prefix are
stripped, so `v1.2.0` and `myapp-1.2.0` both yield `1.2.0`). The synthesized
`Paths_<pkg>` module appends the commit metadata, so `Paths_<pkg>.fullVersion`
reads e.g. `1.2.0+12.gabc1234` (base + commit count + short hash, `.dirty` when
the tree has uncommitted changes) — the same format `zinc version` reports for
zinc itself. An untagged checkout falls back to base `0` (the commit still rides
along in `fullVersion`). A dependency built from source picks up its version the
same way, derived from its locked commit.

## Examples

A minimal single-package executable:

```toml
[workspace]
members = ["."]
ghc = "9.6.5"

[dependencies]

[package]
name = "myapp"
version = "0.1.0"

[build.exe.myapp]
source-dirs = ["app"]
main = "Main.hs"
depends = []
```

A member depends on a sibling by name; the sibling's library is built and
registered first:

```toml
[build.exe.myapp]
source-dirs = ["app"]
main = "Main.hs"
depends = ["mylib"]              # mylib is another workspace member
```

`system-libs` names nixpkgs attributes directly, so a C dependency like `zlib`
is supplied by the toolchain without mapping an `extra-libraries` name to a
system package.

Run `zinc fmt` to rewrite manifests into the canonical layout (sorted
dependencies, shorthand where possible). `zinc add` and `zinc update` already
write in that layout.
