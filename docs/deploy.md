---
title: Packaging and deployment
nav_order: 13
---

# Packaging and deployment

## Why

Building a binary is half the job; shipping it is the other half. Because zinc
already manages a Nix flake, and Nix is a capable packaging tool, zinc can turn a
build into a deployable artifact (a container image, a static binary, a portable
file) with no Nix knowledge on your part.

## What

`zinc package <format>` produces a deployable artifact from the build. This is a
deployment format, distinct from `--target` (which selects a compile
architecture). Formats:

- `docker`: a reproducible, minimal OCI image, built without a Docker daemon.
- `static`: a self-contained statically linked (musl) binary.
- `bundle`: a single portable executable.
- `nix`: the build as a Nix closure, deployable to Nix hosts.

## How

```
zinc package docker --tag myapp:1.0
zinc package static -o ./dist/myapp
zinc package bundle
zinc package nix
```

`zinc package docker` builds a layered image containing your executable and its
runtime closure, ready to load locally or push to a registry. `zinc package
static` produces a binary you can copy to a server and run directly.

## Examples

Build and load a container image:

```
$ zinc package docker --tag myapp:1.0
   Built image myapp:1.0 (18 MB, 3 layers)
$ docker run myapp:1.0
Hello from myapp!
```

Produce a static binary and run it on a bare host:

```
$ zinc package static -o ./dist/myapp
$ scp ./dist/myapp server:/usr/local/bin/myapp
$ ssh server myapp
Hello from myapp!
```

Limitations: a static binary uses musl, so packages depending on C libraries
without a static build, or that use runtime dynamic loading, cannot be linked
statically. Such a closure member is reported as unsupported rather than failing
with a linker error. Container images and bundles have no such constraint.
