---
title: Updating dependencies
nav_order: 11
---

# Updating dependencies

## Why

Dependencies move. You want to see what is available, bump what you choose, and
understand the blast radius before you commit — without a solver deciding for
you. Because zinc keeps one ref per package name, bumping one dependency can
shift others, so updates are shown explicitly.

## What

- `zinc outdated` is a read-only report of what could be updated: for each
  dependency, the version you have versus the newest release tag available. It
  changes nothing.
- `zinc update` re-resolves refs and rewrites the lockfile, printing a
  before/after diff of the closure — changed, added, and removed packages,
  including ripples caused by the one-ref-per-name rule. `--dry-run` previews the
  diff without writing.

There are no version bounds, so an update goes to the latest release tag, and a
major-version jump is flagged. Pin a dependency (`tag = …`) to hold it back.

## How

See what is available, without touching anything:

```
zinc outdated
```

By default `zinc outdated` reports your direct dependencies (the ones you can
bump directly); `--all` includes the whole closure. Transitive packages move
when their parent moves or when you add a root override.

Update, with a preview:

```
zinc update --dry-run        # show the diff
zinc update                  # apply it
zinc update aeson            # update just one dependency and its sub-closure
```

## Closure ripples

Because each name has a single ref across the closure, updating one dependency
can move a shared transitive for others. For example, updating `aeson` may pull a
newer `scientific` that `attoparsec` also depends on, so `scientific` bumps for
`attoparsec` too. The update diff shows these moves, not just the package you
named.

## Examples

```
$ zinc outdated
aeson        2.2.3.0  ->  2.3.0.0
scientific   0.3.7.0  ->  0.3.8.1   (transitive, via aeson)

$ zinc update aeson --dry-run
Would update:
  aeson       2.2.3.0 -> 2.3.0.0
  scientific  0.3.7.0 -> 0.3.8.1   (ripple: shared with attoparsec)
+ added:   text-iso8601 0.1.0.0
  no removals

$ zinc update aeson
Updated 2 packages, 1 added. zinc.lock rewritten.
```

To revert an update, restore the lockfile from git:

```
git checkout zinc.lock
```
