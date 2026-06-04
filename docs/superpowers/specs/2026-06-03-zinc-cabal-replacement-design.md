# zinc — A git-native, Nix-assisted build tool for Haskell

**Status:** Design approved 2026-06-03
**Author:** Gareth (with Claude)

## 1. Motivation & thesis

`cabal-install` bundles seven jobs: describe the project, **solve dependency
versions**, fetch packages, provide the toolchain, build the world, build your
code, and run/publish. The pain is concentrated in the solver and the
Hackage-version dance. zinc keeps the good parts and replaces the painful ones.

**Thesis: ergonomics.** zinc is a Cargo-like front-end where Nix is a hidden
implementation detail the user never has to touch. Speed (prebuilt/cached
artifacts, fast inner loop) and "it just works" (no solver errors, ever) are the
goal.

### 1.1 Principles (revised 2026-06-04)

These were once framed as two co-equal "north stars" — *explicit / no-magic* and
*it just works* — which appeared to conflict (does an awkward build flag for a
deep transitive dependency get stated explicitly, or hidden to keep things
effortless?). The conflict was an artifact of wording: it only arises if
*explicit* is read as *user-authored*. It isn't. zinc already discovers
transitive deps from self-describing manifests and auto-freezes the lockfile —
explicit in the system, effortless for the user. So the principles are layered,
not competing:

1. **Ergonomics is the goal; explicitness is the mechanism.** The user
   experiences "it just works" *because* the tool makes every decision explicit
   and frozen — not in spite of it.
2. **Explicit state, not explicit effort.** Every resolve/build decision is
   recorded as inspectable, reproducible data (manifest, lockfile, declared
   build facts). The tool produces and freezes it; the user never hand-maintains
   it.
3. **Declared once, at the source, inherited transitively.** Any fact a
   dependency needs to build — ref, ghc flags, `system-libs`, a Safe-Haskell
   assertion — lives *with that dependency* (or in zinc's own inspectable data
   for it), never re-stated by consumers.
4. **Reject inference, not automation.** No hidden solving, no version-fragile
   heuristics (the unpredictable part of cabal). Deterministic automation that
   yields frozen, auditable state is the whole point.

The user is concise (states direct intent); the *system* is explicit (records
and freezes everything else). "Magic" zinc rejects = decisions you can't see or
reproduce. "Automation" zinc embraces = the tool producing explicit state for
you.

**Division of labour:**

| Concern | Owner |
|---|---|
| Haskell source dependencies | **git** (plain clone at a ref) |
| GHC toolchain + system/native libs (zlib, pkg-config) | **Nix** |
| Resolve / build / run / UX | **zinc** (a Haskell CLI) |

## 2. Dependency model (decided)

- **Whole-graph git.** Every non-boot package is a `repo + ref`
  (`hash | tag | branch`), fetched with plain `git`. No Hackage at build time.
- **GHC boot libraries** (`base`, `text`, `bytestring`, `containers`, …) ship
  with the Nix-provided GHC and are never fetched.
- **One git ref per package name** across the whole workspace (GHC strongly
  prefers a single version of each package). **No solver, ever.**
- **Version selection:** latest release tag by default; the user overrides any
  package's ref by hand; cabal-style version bounds are **ignored** (at most a
  warning). The resolved commit is frozen in the lockfile.
- **Self-describing graph.** Because zinc-native packages declare their deps
  *with* repos in their own `zinc.toml`, the graph is self-describing (like Go's
  `go.mod`): the workspace lists only its **direct** deps' repos; transitive
  repos are discovered by cloning each dep and reading *its* manifest.
- **Conflict rule:** if multiple packages reference the same name at different
  refs, the workspace-root override wins; otherwise the latest referenced ref;
  default latest tag. There is only ever one ref per name in the final build.
- **Resolution transparency (decided 2026-06-03):** because the thesis is "no
  surprises," resolution is never silent. `zinc add` and `zinc build` print the
  full resolved closure as a table (name → repo → chosen ref), and a conflict —
  where the rule picked one ref over another a package asked for — is called out
  explicitly in that table. The table is shown on closure resolution only, not
  on the member-only inner loop, so edit→build stays quiet.

## 3. Build model (decided)

zinc drives the **GHC toolchain directly** (`ghc --make`, `ghc-pkg`) — it does
**not** execute Cabal's build machinery (no `Setup.hs`, no `configure/build`
delegation).

- **Opt 1 — zinc-native:** a package describes itself with a `[build]`
  block in its `zinc.toml`. zinc reads that block and drives ghc. This is the
  path for your own code.
- **Opt 2 — `.cabal` reader (in MVP):** zinc *reads* `.cabal` files (using the
  `Cabal` library as a parser only — never its builder) to auto-derive the
  `[build]` block, so arbitrary upstream packages can be pulled from git
  unmodified. **Decision (2026-06-03): Opt 2 is MVP scope, not deferred** — the
  MVP must depend on real Hackage packages (it is the only way `zinc add aeson`,
  a real-leaf integration test, and self-hosting can work), so the build driver
  and the `.cabal`-fidelity layer land together.

**What driving ghc directly requires (Opt 2 fidelity work, all MVP):**
- Synthesize `Paths_<pkg>.hs` (many packages `import Paths_foo`).
- Emit `cabal_macros.h` (`MIN_VERSION_<pkg>(x,y,z)` CPP macros).
- Run preprocessors (alex/happy/hsc2hs/c2hs) before ghc.
- Discover repos for unknown packages from Hackage `source-repository` metadata
  (seeds `zinc add` for packages not yet in `[registry]`).

**Known casualty:** `build-type: Custom` / `Setup.hs` packages remain
unsupported until specially handled (deferred — see §13).

**Per-package build facts / quirks (decided 2026-06-04).** Some upstreams need a
ghc flag that is neither in their `.cabal` nor derivable by inference — the
canonical case is a package that must be built `-XTrustworthy` so a
`{-# LANGUAGE Safe #-}` dependent can import it (cabal achieves this via an
implicit, version-fragile Safe-detection heuristic zinc deliberately does *not*
replicate — §1.1 principle 4). Per principles 2–3, such a fact is **zinc-authored
data attached to the package** — a small, version-controlled quirks table
shipped *with zinc* (e.g. `colour → -XTrustworthy`), or the package's wrapped
manifest — applied automatically when zinc builds that dependency and frozen
like any other build input. The end user never states it, even when the package
is a deep transitive dependency. A workspace `[build-options]` table is the
user-facing override for first-party packages and the long tail; it is the
escape hatch, not the mechanism. This is a *permanent* need, not an Opt-2
stopgap: the fact is absent from the `.cabal`, so the reader can never surface
it — but it stays small, since Safe-importing packages are rare.

## 4. Manifest — `zinc.toml`

Workspaces are first-class from the MVP. A workspace root declares members,
shared dependencies, and the registry; each member declares its components.

**Workspace root:**
```toml
[workspace]
members = ["packages/myapp", "packages/mylib"]
ghc = "9.8.2"                      # Nix pins exactly this compiler

[dependencies]                     # shared across the workspace; one ref per name
aeson = { tag = "v2.2.3.0" }
hspec = "*"                        # "*" = latest release tag, frozen in lock

[registry]                         # repos for DIRECT deps; transitives self-describe
aeson = "https://github.com/haskell/aeson"
hspec = "https://github.com/hspec/hspec"
```

**Member package (`packages/myapp/zinc.toml`):**
```toml
[package]
name = "myapp"
version = "0.1.0"

[build.lib]                        # optional library component
source-dirs     = ["src"]
exposed-modules = ["Myapp", "Myapp.Core"]
other-modules   = ["Myapp.Internal"]
extensions      = ["OverloadedStrings", "LambdaCase"]
ghc-options     = ["-Wall"]
depends         = ["aeson", "mylib"]   # workspace siblings allowed
system-libs     = ["zlib"]             # nixpkgs attr names → Nix supplies them

[build.exe.myapp]                  # named executable
source-dirs = ["app"]
main = "Main.hs"
depends = ["myapp"]

[build.test.spec]                  # test component
source-dirs = ["test"]
main = "Spec.hs"
depends = ["myapp", "hspec"]
```

`system-libs` naming nixpkgs attrs directly sidesteps the
`.cabal extra-libraries → nixpkgs` mapping problem in Opt 1.

**External deps live at the workspace root (decided 2026-06-03).** All external
dependencies are declared once in the root `[dependencies]` + `[registry]`
(consistent with "one ref per name across the workspace", §2). A member's
`depends = […]` only *references* names already declared at the root — siblings
or shared external deps — and never introduces a new external repo of its own.
Consequence: `zinc add` always edits the **root** manifest + lockfile, never a
member's. A member's `zinc.toml` therefore holds only `[package]` + `[build.*]`,
no `[dependencies]`/`[registry]`. (A lone single-package project is just a
workspace with one implicit member and needs no `[workspace]` boilerplate; the
resolver reads its `[dependencies]`/`[registry]` directly — see §4 parsing.)

## 5. Lockfile — `zinc.lock`

Pins every package in the closure to an exact commit + content hash. Lives at
the workspace root.
```toml
[[locked]]
name = "aeson"
repo = "https://github.com/haskell/aeson"
rev = "a1b2c3d…"          # resolved commit, not the tag
sha256 = "sha256-Xk9…"    # validates the fetch; part of the cache key
depends = ["scientific", "witherable"]   # flattened for fast graph load
```

## 6. The hidden Nix env (the only Nix in the project)

zinc generates and manages one `flake.nix` pinning `nixpkgs` + selecting
`haskell.compiler.ghc982` (**just the compiler, not nixpkgs' package set**) +
the union of all `system-libs` in the closure + preprocessors (alex/happy). It
evaluates this **once** (`nix print-dev-env`, cached) to get
`ghc`/`ghc-pkg`/`hsc2hs` and lib paths. The user never writes Nix; `flake.lock`
pins it. Nix is re-consulted **only** when the GHC version or `system-libs`
change — it never touches the inner loop.

**Limitation:** Nix ⇒ macOS/Linux only. Windows is out of scope.

## 7. GHC build driver

Topo-sort the closure; for each non-boot package:
1. Fetch source at locked rev → `~/.zinc/store/src/<name>-<rev>`, verify `sha256`.
2. **Cache check:** key = `hash(rev, ghc-version, dep unit-ids, options)`.
   Hit → `ghc-pkg register` the cached build, skip compile.
3. Miss → run preprocessors → `ghc --make -hide-all-packages
   -package-db <store> -package <dep…> -i<srcdirs> -this-unit-id <name>-<ver>
   -O <modules>` → build archive → write `.conf` → register into
   `~/.zinc/store/pkg/<key>`.
4. **Workspace member packages** build into `.zinc/build/` directly, relying on
   ghc's own recompilation avoidance — never cache-keyed, so the edit→build loop
   stays fast.

## 8. Artifact cache (decided: yes)

Content-addressed `~/.zinc/store`. A dep compiles **once per machine**;
branch-switching reuses builds. This is what makes "pure git, no shared binary
cache" tolerable.

## 9. `zinc add` flow

`zinc add <pkg>` resolves the full transitive closure (walking self-describing
manifests; in Opt 2, seeding unknown repos once from Hackage `source-repository`
metadata), prints the whole closure with proposed repos and resolved refs, and
**requires confirmation** before freezing into `[registry]` + `zinc.lock`.
Packages with no resolvable repo are flagged for the user to supply one.

## 10. CLI surface (Cargo-like)

```
zinc new <name>     scaffold a workspace (zinc.toml + packages/)
zinc add <pkg>      resolve closure → confirm → freeze registry+lock
zinc build          resolve → provision → build closure + workspace members
zinc run [exe --…]  build then run an executable component
zinc repl [target]  ghci with project package-db + local modules
zinc test [target]  build + run test components
zinc update [pkg]   bump ref(s) to latest, rewrite lock
zinc clean          drop .zinc/build (keep the store)
```

## 11. Implementation language

**Haskell.** Dogfoods the ecosystem; the `Cabal` library needed for the
Opt-2 `.cabal` reader is a Haskell library imported directly (no
reimplementation). `typed-process` drives ghc; `ghc-paths`/Nix locate it.

## 12. Testing strategy

- **Unit:** manifest/lock TOML parsers, resolver graph walk, ghc-command
  construction, `flake.nix` generation (golden tests).
- **Integration (a test ladder, decided 2026-06-03):** three rungs of
  increasing realism, each a gate on the next:
  1. **Synthetic** zinc-native workspace (lib + exe + test, hand-written
     `[build]`, no upstream) builds end-to-end and the exe runs.
  2. **Real leaf** — the same, now depending on a small real Hackage package
     (e.g. `integer-logarithms`) pulled from git and built via the Opt-2
     `.cabal` reader.
  3. **Self-hosting — the MVP definition of done:** `zinc build` builds zinc
     itself from a clean checkout, resolving and building zinc's own real
     dependencies (`typed-process`, a TOML parser, `optparse-applicative`, …).
     This single test exercises the resolver, Opt-2 reader, Nix env, build
     driver, cache, and orchestration against genuine real-world packages at
     once. **The MVP is not complete until this passes.**

## 13. Deferred (YAGNI / roadmap)

*(Opt 2 — `.cabal` reader + `Paths_`/`cabal_macros.h` synthesis — was here; it
is now MVP scope, see §3.)*

`build-type: Custom`/`Setup.hs` · benchmarks · Haddock · sdist/Hackage publish ·
profiling builds · HLS/`hie-bios` cradle (important for real editor use, but
post-MVP) · store garbage collection · parallel closure builds ·
cross-compilation · Windows.
