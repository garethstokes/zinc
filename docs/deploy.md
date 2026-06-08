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

## Deploying to a host

`zinc deploy <host>` takes the next step: it builds the Nix closure, copies it
to a remote NixOS host over SSH, installs it into a dedicated per-service Nix
profile, writes a user-systemd unit, and health-checks the activation —
auto-rolling-back if the new version fails to come up. No imperative steps, no
Nix knowledge on the host side beyond a one-time setup.

```
zinc deploy myhost                 # build → copy → activate (recreate)
zinc deploy myhost --init          # print the one-time host setup snippet
zinc deploy myhost --list          # the release history (generations)
zinc deploy myhost --rollback      # revert to the previous generation
zinc deploy myhost --rollback-to 3 # switch to a specific generation
```

`<host>` is a `[user@]host[:port]` address or an ssh-config alias.

### What the host needs

A deploy target is an ordinary NixOS host. Three preconditions must hold; before
every deploy zinc probes them over SSH and, if one is missing, stops with a
typed diagnostic that names the gap rather than failing partway through:

| Requirement | Why | If missing |
|---|---|---|
| **SSH access** to the host (key-based; zinc never prompts for a password) | every step runs over SSH | `ZINC_DEPLOY_SSH` |
| **Nix** installed (a working `nix` with the daemon) | the closure is copied and activated with Nix | `ZINC_DEPLOY_NO_NIX` |
| The **deploy user in `nix.settings.trusted-users`** (directly, via a `@group`, or `*`) | so the host accepts the pushed store paths from `nix copy` | `ZINC_DEPLOY_NOT_TRUSTED` |
| **User lingering** enabled for the deploy user | so the user-level systemd service keeps running without an active login | `ZINC_DEPLOY_NO_LINGER` |

You never hand-edit Nix config by guesswork. Run `--init` first — it resolves
the deploy user (the explicit `user@`, else the host's own `id -un` over SSH)
and prints the exact NixOS module to add to that host's configuration:

```
$ zinc deploy prod --init
{
  nix.settings.trusted-users = [ "deploy" ];
  users.users.deploy.linger  = true;
}
```

zinc never silently mutates a remote system, so the snippet is **printed, not
applied** — add it to the host's `configuration.nix` (or a module it imports),
`nixos-rebuild switch` once, and the host is a permanent deploy target. No agent
or daemon is installed on the host; everything zinc needs is stock Nix +
systemd-user.

### How a deploy works

Each `zinc deploy` runs the same sequence; any step's failure stops with a typed
diagnostic and leaves the running service untouched:

1. **Probe** the host's preconditions (above).
2. **Build** the executable's Nix closure locally (the same closure as
   `zinc package nix`).
3. **Copy** the closure to the host with `nix copy` (signatures unchecked — the
   trust is the SSH channel + `trusted-users`).
4. **Install** it into a dedicated per-service profile,
   `~/.local/state/nix/profiles/zinc-<service>`, with `nix-env --set` — which
   makes the profile contain *exactly* this closure as one new **generation**, so
   a redeploy cleanly replaces the previous version and every release is
   retained for rollback. The app version is stamped against the generation in a
   sidecar so `--list` can label each release.
5. **Activate** a user-systemd unit `zinc-<service>.service` whose `ExecStart`
   points at the profile's `bin/` (a fixed path) — so a rollback only re-points
   the profile and restarts, never rewrites the unit. `args`/`env` from the
   target become the unit's command line and `Environment=` lines.
6. **Health-check**: restart the unit, wait for it to become `active`, then
   confirm after a short settle window that it stayed up (`NRestarts = 0`). If
   the new version crashes or crash-loops, zinc **auto-rolls-back** to the
   previous generation, restarts, and reports the failure — the host is never
   left on a broken release.

### Named targets

Configure a target once in `zinc.toml` and deploy by name:

```toml
[deploy.prod]
host    = "deploy@prod.example.com"
service = "myapp"            # the systemd unit name (defaults to the package)
args    = ["--port", "8080"] # passed to the executable
socket  = 8080               # listening port, for zero-downtime cutovers (below)

[deploy.prod.env]
RUST_LOG = "info"
```

```
zinc deploy prod
```

### Generations and rollback

Each deploy is a Nix profile generation stamped with its app version, so the
host keeps the full release timeline. A successful deploy reports the generation
the release became, so its identity is clear at deploy time:

```
$ zinc deploy prod
Deployed myapp 1.4.0 (generation 7) to prod.example.com — /nix/store/…
  systemd unit zinc-myapp is active.

$ zinc deploy prod --list
  gen 5  1.3.0  2026-06-01 09:14:02
  gen 6  1.3.1  2026-06-04 11:02:55
  gen 7  1.4.0  2026-06-08 20:30:03  (current)
```

`--rollback` reverts to the previous generation; `--rollback-to <N>` switches to
any past generation. Both restart and health-check the service, auto-rolling
back if the target generation fails to come up. A redeploy of an unchanged
binary is a no-op: the closure is content-addressed, so it maps to the same
store path and the same generation (the version stamp still refreshes).

### Zero-downtime cutovers

With a `socket` configured, `--strategy blue-green` performs a socket-activated
cutover: a persistent systemd socket owns the listening port and buffers
incoming connections while the service is swapped between blue/green profiles,
so a successful cutover drops zero connections. A new version that fails its
health-check is rolled back to the live color.

```
zinc deploy prod --strategy blue-green
```
