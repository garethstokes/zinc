# zinc — `zinc deploy <host>`: push a build to a NixOS host over the LAN

**Status:** Design approved 2026-06-06
**Author:** Gareth (with Claude)

## 1. Goal

`zinc deploy <host>` takes a build and runs it as a **managed service** on a
remote NixOS host, over SSH, with no Nix literacy required. Where `zinc package`
*produces* a deployable artifact, `zinc deploy` *delivers and activates* one. v1
is deliberately narrow: a single NixOS host, a user-systemd service, and
profile-based rollback.

## 2. Disambiguation

- **`zinc package <format>` (7m6)** — *produces* an artifact (docker/static/
  bundle/nix). Output side, stops at the artifact.
- **`gtv` — distribute *zinc itself*.**
- **This (`zinc deploy`)** — *delivers + activates* the user's app on a remote
  host. Builds directly on `zinc package nix` (7m6.5): the closure is the payload.

## 3. Why NixOS-first

A NixOS target already runs the Nix daemon, so the artifact is a **closure copied
over SSH** (`nix copy`) — no rebuild on the target, content-addressed dedup, and
the runtime closure (GHC libs) travels with it. The host is two lines of config
away from being a deploy target. Non-Nix hosts (static binary + `scp`) are a
deferred follow-up, not a v1 concern.

## 4. Command surface

```
zinc deploy <host> [--service <name>] [--init] [--rollback] [--dry-run] [--json]
```

- `<host>` = `[user@]host[:port]`, an `~/.ssh/config` alias, or a named target.
- `--service <name>` overrides the unit name (default: package name).
- `--init` generates (and optionally applies) the host's NixOS config.
- `--rollback` reverts to the previous generation and restarts.
- Runs as a **user systemd service** by default — no root, no sudo.

Named targets in `zinc.toml` (ad-hoc `user@host` also works with no config):

```toml
[deploy.homelab]
host    = "gareth@nixos-box"
service = "myapp"
args    = ["--port", "8080"]
env     = { RUST_LOG = "info" }
```

## 5. Deploy sequence

1. **Build** the closure (the `zinc package nix` step, 7m6.5).
2. **Probe** over SSH: Nix daemon present, user in `trusted-users`, lingering
   enabled. Each missing precondition → a structured diagnostic with `nextAction`
   (see §7), never a cryptic failure.
3. **Copy:** `nix copy --to ssh-ng://host <store-path>` — only missing paths
   transfer.
4. **GC-root via a profile:** `nix profile install` into a dedicated profile
   (`~/.local/state/nix/profiles/zinc-<service>`). Pins against GC *and* gives
   generations for free.
5. **Install/refresh the unit:** write `~/.config/systemd/user/zinc-<service>.service`
   with `ExecStart` pointing at the profile `bin/`, then `systemctl --user
   daemon-reload && restart`.
6. **Health-check:** wait (condition-based, not a sleep) for the unit to reach
   `active`. On failure → **auto-roll back** to the previous generation, restart,
   and report.
7. **Report:** new store path, bytes copied, generation number, service status;
   plus the `--json` envelope for agents.

Generated unit (deliberately boring):

```ini
[Unit]
Description=zinc service myapp
[Service]
ExecStart=%h/.local/state/nix/profiles/zinc-myapp/bin/myapp
Restart=on-failure
[Install]
WantedBy=default.target
```

## 6. Rollback

Generations *are* the rollback mechanism — the prior closure is still on the host,
so rollback is instant (no copy):

```
zinc deploy --rollback homelab     # nix profile rollback + restart
```

Keep the last N generations (default 5); older ones are GC-eligible.

## 7. Host requirements + `--init`

For v1 (NixOS), exactly four things:

1. Key-based SSH for the deploy user.
2. Nix daemon running (automatic on NixOS).
3. Deploy user in `trusted-users` — so `nix copy` is accepted (the classic
   gotcha; trusted-users bypasses the copied-path signature check).
4. Lingering enabled — so the user service runs without an active login.

`zinc deploy --init <host>` generates the snippet (and applies it if it can
sudo), reusing the auto-provision pattern from `y03` — you never hand-roll Nix:

```nix
{
  nix.settings.trusted-users = [ "gareth" ];
  users.users.gareth.linger  = true;
}
```

The probe maps each gap to an actionable, typed diagnostic:

| Code | Meaning | nextAction |
|---|---|---|
| `ZINC_DEPLOY_SSH` | connect/auth failed | check key auth / host reachability |
| `ZINC_DEPLOY_NO_NIX` | no Nix daemon on host | install Nix / confirm it is NixOS |
| `ZINC_DEPLOY_NOT_TRUSTED` | user not in `trusted-users` | add user to `nix.settings.trusted-users` (`zinc deploy --init` prints it) |
| `ZINC_DEPLOY_NO_LINGER` | lingering disabled | set `users.users.<u>.linger = true` |
| `ZINC_DEPLOY_ACTIVATE` | unit failed health-check | rolled back; show `journalctl --user -u zinc-<svc>` |

## 8. Decomposition

| Unit | Notes | Depends on |
|---|---|---|
| `deploy` verb + SSH probe + diagnostics | target parsing, capability probe, typed errors | `zinc package nix` (7m6.5), agent-devex diagnostics core |
| nix-copy + profile GC-root | `nix copy` the closure, `nix profile install` | verb |
| user-systemd unit + activate + health-check | write/refresh unit, restart, wait-for-active | profile step |
| `--rollback` (profile generations) | `nix profile rollback` + restart | unit step |
| `--init` host bootstrap | generate/apply trusted-users + linger snippet | verb; reuses `y03` pattern |
| named `[deploy.*]` targets in zinc.toml | config-driven targets | verb |

## 9. Scope

**In v1:** single NixOS host, user-systemd managed service, profile-based
rollback, `--init` bootstrap, `--dry-run`, `--json`.

**Deferred (follow-up beads):** non-Nix static-binary fallback; system-level
services / sudo; privileged ports + reverse proxy; multi-host fan-out; the
declarative `nixos-rebuild --target-host` / deploy-rs path; secrets management.

## 10. Gating / priority

P3 — a forward-looking capability that completes the deploy story (`zinc package`
produces; `zinc deploy` delivers). Builds on the already-delivered `zinc package
nix` (7m6.5) and the auto-provision pattern (`y03`).
