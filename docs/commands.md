---
title: Commands
nav_order: 7
---

# Commands

## Why

zinc's command surface is small and predictable: a focused set of verbs for the
everyday loop, plus introspection and diagnostics commands designed to be driven by
scripts and agents as well as people.

## What

Every command accepts `--json` for machine-readable output and exits with a
stable code per outcome. With no arguments, `zinc` prints an overview of the
commands.

## Project lifecycle

| Command | Description |
|---|---|
| `zinc new <name>` | Scaffold a new project (`--workspace` for a multi-package layout). |
| `zinc add <pkg>` | Resolve a dependency's closure, show it, and freeze it into the lockfile. |
| `zinc vendor <pkg…>` | Fetch a no-git-repo package's tarball into the store and pin it. |
| `zinc build [member]` | Build the workspace, or one member. `--deps-only` builds just the closure. |
| `zinc run [target] [-- args]` | Build, then run an executable. |
| `zinc repl [target]` | Open ghci with the project's package database and modules. |
| `zinc test [target]` | Build and run test components. |
| `zinc update [pkg]` | Re-resolve refs and rewrite the lockfile. `--dry-run` previews. |
| `zinc clean` | Remove build artifacts (`.zinc/`), keeping the content store. |
| `zinc gc` | Garbage-collect the content store. |
| `zinc fmt` | Rewrite `zinc.toml` into the canonical layout. `--check` for CI. |

## Inspect and diagnose

| Command | Description |
|---|---|
| `zinc status` | Workspace overview: members, resolved closure, lock drift, toolchain. |
| `zinc graph` | The dependency build DAG. |
| `zinc explain <pkg>` | Why a package is in the build, at its ref. |
| `zinc outdated` | Dependencies with newer release tags available. |
| `zinc doctor` | Diagnose environment and project problems, with fixes. |
| `zinc perf` | Build performance history: slowest dependencies, cache hit rate. |
| `zinc prime` | An orientation summary for an agent landing in the project. |

## Targets and deployment

| Command | Description |
|---|---|
| `zinc build --target wasm32-wasi` | Build to WebAssembly. |
| `zinc package <format>` | Produce a deployable artifact (`docker`, `static`, `bundle`, `nix`). |
| `zinc skill <add\|list\|remove\|sync>` | Manage installed agentic skills. |

## How

Most commands operate on the current workspace. Pass `--json` to any command for
structured output; pass `--quiet` to suppress progress; `NO_COLOR` disables
color.

## Examples

```
$ zinc new myapp && cd myapp
$ zinc add aeson
$ zinc build
$ zinc run -- --port 8080
$ zinc test
$ zinc outdated
$ zinc build --json | jq '.diagnostics'
```
