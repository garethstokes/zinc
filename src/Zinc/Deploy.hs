-- | @zinc deploy \<host\>@ — the verb, the SSH capability probe, and the typed
-- diagnostics that map each missing host precondition to an actionable next
-- step (deploy-host spec §4,5,7; zinc-nbk.1).
--
-- This unit establishes the foundation the rest of the @deploy@ epic builds on:
-- parsing the target (@[user@]host[:port]@ / an @~\/.ssh\/config@ alias), an
-- SSH probe of the three NixOS preconditions (Nix daemon present, deploy user
-- in @trusted-users@, lingering enabled), and one 'ZincError' per gap. The
-- closure copy + profile GC-root (nbk.2), the user-systemd unit + health-check
-- (nbk.3), @--rollback@ (nbk.4) and @--init@ (nbk.5) layer on top.
--
-- The shell-out boundary ('probeHost') is thin: command construction
-- ('sshArgs', 'probeScript'), output parsing ('parseProbeOutput') and the
-- diagnostic ordering ('interpretProbe') are all pure and tested.
module Zinc.Deploy
  ( DeployHost (..)
  , ProbeChecks (..)
  , ProbeOutcome (..)
  , ResolvedDeploy (..)
  , resolveDeploy
  , parseDeployHost
  , renderDeployTarget
  , sshArgs
  , probeScript
  , parseProbeOutput
  , interpretProbe
  , probeHost
  , runDeploy
  , deployReadyJson
  , initSnippet
  , runInit
  , nixCopyStoreUri
  , nixCopyArgs
  , nixCopyEnv
  , profileName
  , profileInstallScript
  , runNixCopy
  , runProfileInstall
  , unitFile
  , systemdUnit
  , activateScript
  , rollbackScript
  , runActivate
  , runRollback
  , Generation (..)
  , listGenerationsScript
  , parseGenerations
  , switchGenerationScript
  , runDeployList
  , runSwitchGeneration
  , generationsJson
  , renderGenerations
  , socketUnitFile
  , colorProfileName
  , socketUnit
  , blueGreenScript
  , runBlueGreen
  , deployGenMarker
  , parseDeployGen
  ) where

import Data.Char (isDigit)
import Data.List (find)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Zinc.Diagnostic
  ( ZincError (DeployActivate, DeployCopy, DeployNoLinger, DeployNoNix, DeployNotTrusted, DeploySsh)
  )
import Zinc.Json (Json (..), object)
import Zinc.Manifest (DeployTarget (..))

-- | A parsed deploy target: an optional user, a host (or @~\/.ssh\/config@
-- alias), and an optional port. SSH resolves an alias, so a bare token is left
-- as the host unchanged.
data DeployHost = DeployHost
  { dhUser :: Maybe String
  , dhHost :: String
  , dhPort :: Maybe Int
  }
  deriving (Eq, Show)

-- | A deploy target after resolving the CLI argument against the manifest's
-- @[deploy.*]@ tables (zinc-nbk.6): the effective host plus the service / args /
-- env that the activate step (nbk.3) will use.
data ResolvedDeploy = ResolvedDeploy
  { rdHost    :: DeployHost
  , rdService :: Maybe String
  , rdArgs    :: [String]
  , rdEnv     :: [(String, String)]
  , rdSocket  :: Maybe Int -- ^ listening port for socket-activated zero-downtime deploys (zinc-nbk.8)
  }
  deriving (Eq, Show)

-- | Resolve the @zinc deploy \<arg\>@ argument: if @arg@ names a @[deploy.*]@
-- target use its host/service/args/env/socket; otherwise treat @arg@ as an
-- ad-hoc @[user@]host[:port]@ / ssh alias. A @--service@ override always wins
-- over the configured service.
resolveDeploy :: [DeployTarget] -> String -> Maybe String -> ResolvedDeploy
resolveDeploy targets arg svcOverride = case find ((== arg) . dtName) targets of
  Just t  -> ResolvedDeploy (parseDeployHost (dtHost t)) (svcOverride `orConfig` dtService t) (dtArgs t) (dtEnv t) (dtSocket t)
  Nothing -> ResolvedDeploy (parseDeployHost arg) svcOverride [] [] Nothing
  where
    orConfig (Just s) _ = Just s
    orConfig Nothing  c = c

-- | The three NixOS preconditions the probe reports, as booleans (spec §7).
data ProbeChecks = ProbeChecks
  { pcNix     :: Bool  -- ^ a Nix daemon is present on the host
  , pcTrusted :: Bool  -- ^ the deploy user is in @nix.settings.trusted-users@
  , pcLinger  :: Bool  -- ^ user lingering is enabled (services run without a login)
  }
  deriving (Eq, Show)

-- | The outcome of an SSH probe: either the connection itself failed (auth /
-- reachability) or it succeeded and we read the three checks back.
data ProbeOutcome
  = SshUnreachable String  -- ^ connect/auth failed; detail (first stderr line)
  | Probed ProbeChecks
  deriving (Eq, Show)

-- | Parse a @[user@]host[:port]@ target. A trailing @:NNN@ is only treated as a
-- port when it is all digits, so a host that happens to contain a colon is left
-- intact. A bare token (no @\@@, no numeric port) is the host / ssh alias.
parseDeployHost :: String -> DeployHost
parseDeployHost s = DeployHost user host port
  where
    (user, hostPort) = case break (== '@') s of
      (u, '@' : r) -> (Just u, r)
      _            -> (Nothing, s)
    (host, port) = splitPort hostPort

-- | Split a trailing @:port@ off a host, honouring only an all-digit suffix
-- after the LAST colon (so @host:weird@ stays whole).
splitPort :: String -> (String, Maybe Int)
splitPort hp = case break (== ':') (reverse hp) of
  (revPort, ':' : revHost)
    | not (null revPort) && all isDigit revPort ->
        (reverse revHost, Just (read (reverse revPort)))
  _ -> (hp, Nothing)

-- | The @[user@]host@ string SSH is invoked with (the port goes through @-p@).
renderDeployTarget :: DeployHost -> String
renderDeployTarget h = maybe "" (++ "@") (dhUser h) ++ dhHost h

-- | The @ssh@ argv for running @cmd@ on the host. @BatchMode=yes@ makes a
-- missing key fail fast instead of prompting (AGENTS.md non-interactive rule);
-- a non-default port is passed via @-p@.
sshArgs :: DeployHost -> [String] -> [String]
sshArgs h cmd =
  ["-o", "BatchMode=yes"]
    ++ maybe [] (\p -> ["-p", show p]) (dhPort h)
    ++ [renderDeployTarget h]
    ++ cmd

-- | The remote shell script the probe pipes to @sh -s@. It prints one
-- @key=yes|no@ line per precondition; all decision logic stays on the host (we
-- only parse the booleans), and it always exits 0 so a clean run is
-- distinguishable from an SSH-level failure (exit 255).
probeScript :: String
probeScript =
  unlines
    [ "u=$(id -un)"
    , "if command -v nix >/dev/null 2>&1; then echo nix=yes; else echo nix=no; fi"
    , "tu=$(nix config show 2>/dev/null | sed -n 's/^trusted-users = //p')"
    , "if [ -z \"$tu\" ]; then tu=$(nix show-config 2>/dev/null | sed -n 's/^trusted-users = //p'); fi"
    , "trusted=no"
    , "for t in $tu; do"
    , "  case \"$t\" in"
    , "    \"$u\"|'*') trusted=yes ;;"
    , "    @*) g=${t#@}; if id -nG \"$u\" 2>/dev/null | tr ' ' '\\n' | grep -qx \"$g\"; then trusted=yes; fi ;;"
    , "  esac"
    , "done"
    , "echo trusted=$trusted"
    , "if [ \"$(loginctl show-user \"$u\" -p Linger --value 2>/dev/null)\" = yes ]; then echo linger=yes; else echo linger=no; fi"
    ]

-- | Parse the probe's @key=value@ output; a missing key reads as @False@.
parseProbeOutput :: String -> ProbeChecks
parseProbeOutput out = ProbeChecks (yes "nix") (yes "trusted") (yes "linger")
  where
    kvs = [(k, drop 1 v) | ln <- lines out, let (k, v) = break (== '=') ln]
    yes k = lookup k kvs == Just "yes"

-- | Map a probe outcome to the first blocking gap, in deploy order: reachable →
-- Nix present → user trusted → lingering on. 'Right' @()@ means the host is
-- ready to receive a deploy.
interpretProbe :: DeployHost -> ProbeOutcome -> Either ZincError ()
interpretProbe h outcome = case outcome of
  SshUnreachable detail -> Left (DeploySsh (dhHost h) detail)
  Probed c
    | not (pcNix c)     -> Left (DeployNoNix (dhHost h))
    | not (pcTrusted c) -> Left (DeployNotTrusted user)
    | not (pcLinger c)  -> Left (DeployNoLinger user)
    | otherwise         -> Right ()
  where
    user = maybe (dhHost h) id (dhUser h)

-- | Run the SSH probe against a host. Pipes 'probeScript' to @sh -s@; ssh's own
-- exit 255 (connect/auth failure) becomes 'SshUnreachable', anything else is a
-- successful login whose stdout we parse.
probeHost :: DeployHost -> IO ProbeOutcome
probeHost h = do
  (ec, out, err) <- readProcessWithExitCode "ssh" (sshArgs h ["sh", "-s"]) probeScript
  pure $ case ec of
    ExitFailure 255 -> SshUnreachable (firstLine err)
    _               -> Probed (parseProbeOutput out)

-- | The first line of an ssh stderr (CR-trimmed) for a diagnostic detail.
firstLine :: String -> String
firstLine s = case lines s of
  (l : _) -> takeWhile (/= '\r') l
  []      -> "ssh connection failed"

-- | The @zinc deploy@ probe step for nbk.1: probe a (already-resolved) host over
-- SSH and report readiness. The copy + activate sequence (nbk.2/.3) and
-- @--rollback@ (nbk.4) layer on top of this probe. Returns the host + its checks
-- so the caller can render a readiness summary.
runDeploy :: DeployHost -> IO (Either ZincError (DeployHost, ProbeChecks))
runDeploy h = do
  outcome <- probeHost h
  pure $ case interpretProbe h outcome of
    Left e  -> Left e
    Right () -> case outcome of
      Probed c -> Right (h, c)
      -- interpretProbe only returns Right () for a Probed outcome.
      SshUnreachable d -> Left (DeploySsh (dhHost h) d)

-- | The NixOS module snippet that makes a host a valid deploy target (spec §7):
-- the deploy user goes in @trusted-users@ (so @nix copy@ is accepted) and gets
-- lingering (so the user service runs without an active login). The user never
-- hand-rolls Nix — reusing the auto-provision philosophy of y03.
initSnippet :: String -> String
initSnippet user =
  unlines
    [ "{"
    , "  nix.settings.trusted-users = [ \"" ++ user ++ "\" ];"
    , "  users.users." ++ user ++ ".linger  = true;"
    , "}"
    ]

-- | @zinc deploy --init \<host\>@ (nbk.5): resolve the deploy user (the explicit
-- @user\@@ if given, else the host's own @id -un@ over SSH) and return the
-- NixOS 'initSnippet' for it. zinc never silently mutates a remote system's
-- config, so this prints the snippet for the operator to add to their NixOS
-- configuration rather than applying it unprompted.
runInit :: DeployHost -> IO (Either ZincError String)
runInit h =
  case dhUser h of
    Just u  -> pure (Right (initSnippet u))
    Nothing -> do
      (ec, out, err) <- readProcessWithExitCode "ssh" (sshArgs h ["id", "-un"]) ""
      pure $ case ec of
        ExitFailure 255 -> Left (DeploySsh (dhHost h) (firstLine err))
        _ -> case words out of
          (u : _) -> Right (initSnippet u)
          []      -> Left (DeploySsh (dhHost h) "could not determine the remote username")

-- | The @ssh-ng://@ Nix store URI for a host (deploy-host spec §5 step 3). The
-- port is NOT embedded here — Nix's @ssh-ng@ shells out to @ssh@, which takes
-- the port via @NIX_SSHOPTS@ (see 'nixCopyEnv'), not the URI.
nixCopyStoreUri :: DeployHost -> String
nixCopyStoreUri h = "ssh-ng://" ++ maybe "" (++ "@") (dhUser h) ++ dhHost h

-- | The @nix copy@ argv that pushes a store path's closure to the host. Only
-- store paths the host is missing transfer (content-addressed dedup, spec §5).
-- @--no-check-sigs@: the closure was just built locally (trusted by
-- construction) but is unsigned, and @ssh-ng@ otherwise rejects unsigned paths
-- even for a trusted remote user ("lacks a signature by a trusted key");
-- verified against a real host (zinc-nbk.2).
nixCopyArgs :: DeployHost -> FilePath -> [String]
nixCopyArgs h path =
  ["--extra-experimental-features", "nix-command flakes", "copy", "--no-check-sigs", "--to", nixCopyStoreUri h, path]

-- | Extra env for the @nix copy@ process: a non-default port reaches Nix's
-- underlying @ssh@ via @NIX_SSHOPTS@ (the URI carries no port).
nixCopyEnv :: DeployHost -> [(String, String)]
nixCopyEnv h = maybe [] (\p -> [("NIX_SSHOPTS", "-p " ++ show p)]) (dhPort h)

-- | The dedicated user-profile name for a service (spec §5 step 4):
-- @\~\/.local\/state\/nix\/profiles\/zinc-\<service\>@ pins the closure against
-- GC and yields generations (the rollback substrate, nbk.4) for free.
profileName :: String -> String
profileName service = "zinc-" ++ service

-- | The remote shell script that points the service's user profile at a
-- (already-copied) store path — the GC-root + generation step. Uses
-- @nix-env --set@, not @nix profile install@: @--set@ makes the profile contain
-- EXACTLY this closure as one new generation, so a redeploy cleanly REPLACES the
-- prior app (whereas @nix profile install@ errors on the conflicting @bin/@ of
-- the previous version) and rollback (nbk.4) reverts to the prior generation.
-- Verified against a real host (zinc-nbk.2). Runs on the host via SSH, so
-- @$HOME@ expands there.
profileInstallScript :: String -> String -> FilePath -> String
profileInstallScript service version path =
  unlines
    [ "set -e"
    , "prof=\"$HOME/.local/state/nix/profiles/" ++ profileName service ++ "\""
    , "mkdir -p \"$(dirname \"$prof\")\""
    , "nix-env --profile \"$prof\" --set " ++ path
    , -- Stamp the app version against the new generation so `deploy --list`
      -- (nbk.7) can label each release; the sidecar maps gen number -> version
      -- (the timestamp comes from nix-env --list-generations itself).
      "gen=$(nix-env --profile \"$prof\" --list-generations | sed -n 's/^ *\\([0-9][0-9]*\\).*(current).*/\\1/p')"
    , "[ -n \"$gen\" ] && printf '%s\\t%s\\n' \"$gen\" '" ++ version ++ "' >> \"$prof.zinc-versions\""
    , -- Report the assigned generation back to the deploy caller (zinc-zp7) so
      -- the release's identity (version @ generation N) is surfaced at deploy
      -- time, not only retrospectively via `deploy --list`.
      "[ -n \"$gen\" ] && echo \"" ++ deployGenMarker ++ " $gen\""
    ]

-- | Push a built store closure to the host with @nix copy@ (deploy sequence
-- step 3). NOTE (nbk.2): the command construction is unit-tested, but this IO
-- path is UNVERIFIED without a real NixOS host — see the issue's discovery notes.
runNixCopy :: DeployHost -> FilePath -> IO (Either ZincError ())
runNixCopy h path = do
  base <- getEnvironment
  (code, _out, err) <-
    readCreateProcessWithExitCode
      (proc "nix" (nixCopyArgs h path)) {env = Just (base ++ nixCopyEnv h)}
      ""
  pure $ case code of
    ExitSuccess   -> Right ()
    ExitFailure _ -> Left (DeployCopy (dhHost h) (firstLine err))

-- | Install a copied store path into the service's user profile over SSH
-- (deploy sequence step 4). On success returns the generation number the
-- release became (zinc-zp7; 'Nothing' on a pre-zp7 host or if the profile had
-- no current generation). NOTE (nbk.2): unit-tested command construction; the
-- IO path is UNVERIFIED without a real NixOS host.
runProfileInstall :: DeployHost -> String -> String -> FilePath -> IO (Either ZincError (Maybe Int))
runProfileInstall h service version path = do
  (code, out, err) <-
    readProcessWithExitCode "ssh" (sshArgs h ["sh", "-s"]) (profileInstallScript service version path)
  pure $ case code of
    ExitSuccess   -> Right (parseDeployGen out)
    ExitFailure _ -> Left (DeployCopy (dhHost h) (firstLine err))

-- | The user-systemd unit file name for a service: @zinc-\<service\>.service@
-- (spec §5 step 5).
unitFile :: String -> String
unitFile service = profileName service ++ ".service"

-- | The generated user-systemd unit (deploy-host spec §5; zinc-nbk.3). The
-- @ExecStart@ points at the dedicated PROFILE @bin/@, not a concrete store path,
-- so @nix profile rollback@ swaps the running closure under a fixed path without
-- rewriting the unit. Deliberately boring: @Restart=on-failure@, an optional
-- @Environment=@ per configured var, started under the default target. @args@
-- are appended to @ExecStart@ (v1: simple flags; no shell quoting).
systemdUnit :: String -> [String] -> [(String, String)] -> String
systemdUnit service args env =
  unlines $
    [ "[Unit]"
    , "Description=zinc service " ++ service
    , ""
    , "[Service]"
    , "ExecStart=%h/.local/state/nix/profiles/" ++ profileName service ++ "/bin/" ++ service ++ concatMap (' ' :) args
    , "Restart=on-failure"
    ]
      ++ ["Environment=" ++ k ++ "=" ++ v | (k, v) <- env]
      ++ [ ""
         , "[Install]"
         , "WantedBy=default.target"
         ]

-- | The remote activation script (deploy-host spec §5 steps 5–6; zinc-nbk.3):
-- write the unit, @daemon-reload@, @restart@, then wait — condition-based, not a
-- fixed sleep — for the unit to report @active@ (capped at ~10s). On failure (or
-- @failed@) it AUTO-ROLLS-BACK: @nix profile rollback@ to the previous
-- generation, restart the (unchanged-path) unit, and @exit 1@ so the caller
-- reports 'DeployActivate'. Runs on the host via SSH, so @$HOME@ expands there.
activateScript :: String -> String -> String
activateScript service unit =
  unlines
    [ "set -e"
    , "unitdir=\"$HOME/.config/systemd/user\""
    , "mkdir -p \"$unitdir\""
    , "cat > \"$unitdir/" ++ unitFile service ++ "\" <<'ZINC_UNIT_EOF'"
    , dropTrailingNewline unit
    , "ZINC_UNIT_EOF"
    , "systemctl --user daemon-reload"
    , "systemctl --user enable " ++ unitFile service ++ " >/dev/null 2>&1 || true"
    , "systemctl --user restart " ++ unitFile service
    , "prof=\"$HOME/.local/state/nix/profiles/" ++ profileName service ++ "\""
    , "unit=" ++ unitFile service
    , "# Wait (condition-based) for the unit to first reach active or fail."
    , "up=no"
    , "for _ in $(seq 1 50); do"
    , "  state=$(systemctl --user is-active \"$unit\" 2>/dev/null || true)"
    , "  if [ \"$state\" = active ]; then up=yes; break; fi"
    , "  if [ \"$state\" = failed ]; then break; fi"
    , "  sleep 0.2"
    , "done"
    , "# Settle, then confirm it STAYED up without restarting — a crash-looping"
    , "# service (Restart=on-failure) flashes 'active' between crashes, so a single"
    , "# is-active poll isn't enough; require active + NRestarts=0 after a window."
    , "if [ \"$up\" = yes ]; then"
    , "  sleep 1.5"
    , "  state=$(systemctl --user is-active \"$unit\" 2>/dev/null || true)"
    , "  nr=$(systemctl --user show -p NRestarts --value \"$unit\" 2>/dev/null || echo 0)"
    , "  if [ \"$state\" = active ] && [ \"${nr:-0}\" = 0 ]; then echo ZINC_ACTIVE; exit 0; fi"
    , "fi"
    , "# health-check failed: roll back to the previous generation and restart."
    , "# reset-failed first: a crash-loop trips systemd's start-limit, which would"
    , "# otherwise reject the restart ('start request repeated too quickly')."
    , "nix-env --profile \"$prof\" --rollback || true"
    , "systemctl --user reset-failed \"$unit\" 2>/dev/null || true"
    , "systemctl --user restart \"$unit\" || true"
    , "echo ZINC_ROLLED_BACK >&2"
    , "exit 1"
    ]
  where
    dropTrailingNewline s = case reverse s of '\n' : r -> reverse r; _ -> s

-- | The remote rollback script (deploy-host spec §6; zinc-nbk.4): revert the
-- service's profile to the previous generation and restart the unit. Instant —
-- the prior closure is still on the host, so no copy is needed.
rollbackScript :: String -> String
rollbackScript service =
  unlines
    [ "set -e"
    , "prof=\"$HOME/.local/state/nix/profiles/" ++ profileName service ++ "\""
    , "nix-env --profile \"$prof\" --rollback"
    , "systemctl --user reset-failed " ++ unitFile service ++ " 2>/dev/null || true"
    , "systemctl --user restart " ++ unitFile service
    ]

-- | Install/refresh the user-systemd unit and health-check it over SSH
-- (deploy-host spec §5 steps 5–6; zinc-nbk.3). A failed activation has already
-- been rolled back on the host by 'activateScript'; the error reports that.
runActivate :: DeployHost -> String -> [String] -> [(String, String)] -> IO (Either ZincError ())
runActivate h service args env = do
  (code, _out, err) <-
    readProcessWithExitCode "ssh" (sshArgs h ["sh", "-s"]) (activateScript service (systemdUnit service args env))
  pure $ case code of
    ExitSuccess   -> Right ()
    ExitFailure _ -> Left (DeployActivate (dhHost h) (firstLine err))

-- | @zinc deploy --rollback \<host\>@ (deploy-host spec §6; zinc-nbk.4): revert
-- the service to its previous generation and restart, over SSH.
runRollback :: DeployHost -> String -> IO (Either ZincError ())
runRollback h service = do
  (code, _out, err) <-
    readProcessWithExitCode "ssh" (sshArgs h ["sh", "-s"]) (rollbackScript service)
  pure $ case code of
    ExitSuccess   -> Right ()
    ExitFailure _ -> Left (DeployActivate (dhHost h) (firstLine err))

-- | The persistent @.socket@ unit name that owns a service's listening port
-- (zinc-nbk.8). It matches the service unit's prefix (@zinc-\<svc\>@) so systemd
-- hands its listening fd to @zinc-\<svc\>.service@ via socket activation.
socketUnitFile :: String -> String
socketUnitFile service = "zinc-" ++ service ++ ".socket"

-- | The per-color profile name (zinc-nbk.8): each color is its own nix profile
-- generation chain, so the versioning + rollback (nbk.7) compose per color.
colorProfileName :: String -> String -> String
colorProfileName service color = profileName service ++ "-" ++ color

-- | The @.socket@ unit that owns the listening port (zinc-nbk.8). It is started
-- ONCE and never stopped during a deploy, so it holds the port and buffers
-- incoming connections in its listen backlog while the service is swapped to a
-- new version — the source of the zero-dropped-connections guarantee.
socketUnit :: String -> Int -> String
socketUnit service port =
  unlines
    [ "[Unit]"
    , "Description=zinc socket for " ++ service
    , ""
    , "[Socket]"
    , "ListenStream=" ++ show port
    , ""
    , "[Install]"
    , "WantedBy=sockets.target"
    ]

-- | The socket-activated, color-aware blue/green cutover script (zinc-nbk.8).
-- Run on the host after the closure is copied. It: picks the INACTIVE color,
-- points that color's profile at the new closure (+ stamps its version, nbk.7),
-- (re)writes the persistent @.socket@ (port owner) and the @.service@ whose
-- @ExecStart@ is the inactive color's profile bin, then restarts the service —
-- the socket keeps the port open across the swap, so connections QUEUE rather
-- than drop. It then health-checks the new color (active + @NRestarts=0@ after a
-- settle window); on success it records the new active color, on failure it
-- repoints the service at the PREVIOUS color and restarts (instant — that
-- color's profile is untouched) and exits non-zero ('DeployActivate').
--
-- GUARANTEE + LIMIT (verified on a real host): a SUCCESSFUL cutover drops zero
-- connections (the socket buffers across the swap). On FAILURE the new version
-- never successfully serves and the live color is restored, but a crashing new
-- version briefly stalls the socket during crash-detection, so a few in-flight
-- connections can drop in that ~2s window. Truly-zero-drop on failure would need
-- to validate the new color BEFORE the socket forwards to it, which a
-- socket-activated binary can't do standalone (it needs the inherited fd) — an
-- out-of-band validation port / app cooperation is the future refinement.
blueGreenScript :: String -> Int -> String -> [String] -> [(String, String)] -> FilePath -> String
blueGreenScript service port version args env' path =
  unlines $
    [ "set -e"
    , "unitdir=\"$HOME/.config/systemd/user\""
    , "mkdir -p \"$unitdir\""
    , "sock=" ++ socketUnitFile service
    , "unit=" ++ unitFile service
    , "marker=\"$unitdir/zinc-" ++ service ++ ".color\""
    , "active=$(cat \"$marker\" 2>/dev/null || echo '')"
    , "if [ \"$active\" = blue ]; then target=green; else target=blue; fi"
    , "prof=\"$HOME/.local/state/nix/profiles/" ++ profileName service ++ "-$target\""
    , "mkdir -p \"$(dirname \"$prof\")\""
    , "nix-env --profile \"$prof\" --set " ++ path
    , "gen=$(nix-env --profile \"$prof\" --list-generations | sed -n 's/^ *\\([0-9][0-9]*\\).*(current).*/\\1/p')"
    , "[ -n \"$gen\" ] && printf '%s\\t%s\\n' \"$gen\" '" ++ version ++ "' >> \"$prof.zinc-versions\""
    , "[ -n \"$gen\" ] && echo \"" ++ deployGenMarker ++ " $gen\"" -- report the generation to the caller (zinc-zp7)
    , -- the persistent socket (owns the port; never stopped → buffers the swap)
      "cat > \"$unitdir/$sock\" <<'ZINC_SOCK_EOF'"
    , dropTrailingNewline (socketUnit service port)
    , "ZINC_SOCK_EOF"
    , -- the socket-activated service, ExecStart = the TARGET color's profile bin
      "cat > \"$unitdir/$unit\" <<ZINC_SVC_EOF"
    , "[Unit]"
    , "Description=zinc service " ++ service ++ " ($target)"
    , "Requires=$sock"
    , "After=$sock"
    , ""
    , "[Service]"
    , "ExecStart=$prof/bin/" ++ service ++ concatMap (' ' :) args
    , "Restart=on-failure"
    ]
      ++ ["Environment=" ++ k ++ "=" ++ v | (k, v) <- env']
      ++ [ "ZINC_SVC_EOF"
         , "systemctl --user daemon-reload"
         , "systemctl --user enable \"$sock\" >/dev/null 2>&1 || true"
         , "systemctl --user start \"$sock\"" -- hold the port open across the swap
         , "systemctl --user reset-failed \"$unit\" 2>/dev/null || true"
         , "systemctl --user restart \"$unit\"" -- socket buffers connections here
         , "up=no"
         , "for _ in $(seq 1 50); do"
         , "  state=$(systemctl --user is-active \"$unit\" 2>/dev/null || true)"
         , "  if [ \"$state\" = active ]; then up=yes; break; fi"
         , "  if [ \"$state\" = failed ]; then break; fi"
         , "  sleep 0.2"
         , "done"
         , "if [ \"$up\" = yes ]; then"
         , "  sleep 1.5"
         , "  state=$(systemctl --user is-active \"$unit\" 2>/dev/null || true)"
         , "  nr=$(systemctl --user show -p NRestarts --value \"$unit\" 2>/dev/null || echo 0)"
         , "  if [ \"$state\" = active ] && [ \"${nr:-0}\" = 0 ]; then printf '%s' \"$target\" > \"$marker\"; echo ZINC_ACTIVE; exit 0; fi"
         , "fi"
         , -- health-check failed: repoint the service at the previous color (its
           -- profile is untouched) and restart, so the live version is restored.
           "if [ -n \"$active\" ]; then"
         , "  oldprof=\"$HOME/.local/state/nix/profiles/" ++ profileName service ++ "-$active\""
         , "  sed -i \"s#^ExecStart=.*#ExecStart=$oldprof/bin/" ++ service ++ concatMap (' ' :) args ++ "#\" \"$unitdir/$unit\""
         , "  systemctl --user daemon-reload"
         , "  systemctl --user reset-failed \"$unit\" 2>/dev/null || true"
         , "  systemctl --user restart \"$unit\" || true"
         , "fi"
         , "echo ZINC_ROLLED_BACK >&2"
         , "exit 1"
         ]
  where
    dropTrailingNewline s = case reverse s of '\n' : r -> reverse r; _ -> s

-- | @zinc deploy --strategy blue-green@ (zinc-nbk.8): the socket-activated,
-- color-swapping, zero-dropped-connection cutover over SSH. A failed health-check
-- has already been rolled back on the host by 'blueGreenScript'.
runBlueGreen :: DeployHost -> String -> Int -> String -> [String] -> [(String, String)] -> FilePath -> IO (Either ZincError (Maybe Int))
runBlueGreen h service port version args env' path = do
  (code, out, err) <-
    readProcessWithExitCode "ssh" (sshArgs h ["sh", "-s"]) (blueGreenScript service port version args env' path)
  pure $ case code of
    ExitSuccess   -> Right (parseDeployGen out)
    ExitFailure _ -> Left (DeployActivate (dhHost h) (firstLine err))

-- | The marker line a deploy script prints to report the generation it created,
-- so 'runProfileInstall' / 'runBlueGreen' can return it (zinc-zp7).
deployGenMarker :: String
deployGenMarker = "ZINC_GEN"

-- | The generation number a deploy script reported via 'deployGenMarker', taken
-- from the LAST such line (a no-op redeploy may print several). 'Nothing' if no
-- well-formed marker is present (e.g. a pre-zp7 host).
parseDeployGen :: String -> Maybe Int
parseDeployGen out =
  case [n | l <- lines out, [m, g] <- [words l], m == deployGenMarker, [(n, "")] <- [reads g]] of
    [] -> Nothing
    ns -> Just (last ns)

-- | One deployed release: a profile generation, its app version (from the
-- version sidecar, 'Nothing' for a pre-nbk.7 deploy), its timestamp, and whether
-- it is the currently-active generation (zinc-nbk.7).
data Generation = Generation
  { genNum       :: Int
  , genTimestamp :: String
  , genCurrent   :: Bool
  , genVersion   :: Maybe String
  }
  deriving (Eq, Show)

-- | The remote script that dumps a service's generation history (zinc-nbk.7):
-- @nix-env --list-generations@ for the numbers + timestamps + the current
-- marker, then the @.zinc-versions@ sidecar (gen -> app version), separated by a
-- marker line for 'parseGenerations'. No profile yet → both empty (exit 0).
listGenerationsScript :: String -> String
listGenerationsScript service =
  unlines
    [ "prof=\"$HOME/.local/state/nix/profiles/" ++ profileName service ++ "\""
    , "nix-env --profile \"$prof\" --list-generations 2>/dev/null || true"
    , "echo '" ++ generationsMarker ++ "'"
    , "cat \"$prof.zinc-versions\" 2>/dev/null || true"
    ]

-- | The line separating @list-generations@ output from the version sidecar in
-- 'listGenerationsScript' output.
generationsMarker :: String
generationsMarker = "---ZINC-VERSIONS---"

-- | Parse 'listGenerationsScript' output into the generation history (nbk.7),
-- joining each @nix-env --list-generations@ row (gen, timestamp, current) with
-- the sidecar's @gen\\tversion@ lines.
parseGenerations :: String -> [Generation]
parseGenerations out =
  [ Generation n ts cur (lookup n vers)
  | l <- genLines
  , Just (n, ts, cur) <- [parseGenLine l]
  ]
  where
    (genLines, rest) = break (== generationsMarker) (lines out)
    -- Reverse so a gen restamped on a no-op redeploy (identical closure → no new
    -- generation, but a fresh version stamp) shows its LATEST version.
    vers = reverse [(n, v) | l <- drop 1 rest, Just (n, v) <- [parseVerLine l]]
    parseGenLine line = case words line of
      (g : ws) | [(n, "")] <- reads g ->
        Just (n, unwords (filter (/= "(current)") ws), "(current)" `elem` ws)
      _ -> Nothing
    parseVerLine line = case break (== '\t') line of
      (g, '\t' : v) | [(n, "")] <- reads g -> Just (n, v)
      _ -> Nothing

-- | The remote script that pins a service's profile to a SPECIFIC generation
-- (zinc-nbk.7 @--rollback-to \<N\>@): @nix-env --switch-generation@ — works
-- backward AND forward, unlike the single-step @--rollback@. The caller then
-- re-runs the activate + health-check path (so a bad target auto-rolls-back).
switchGenerationScript :: String -> Int -> String
switchGenerationScript service n =
  unlines
    [ "set -e"
    , "prof=\"$HOME/.local/state/nix/profiles/" ++ profileName service ++ "\""
    , "nix-env --profile \"$prof\" --switch-generation " ++ show n
    ]

-- | @zinc deploy --list \<host\>@ (nbk.7): the service's generation history.
runDeployList :: DeployHost -> String -> IO (Either ZincError [Generation])
runDeployList h service = do
  (code, out, err) <-
    readProcessWithExitCode "ssh" (sshArgs h ["sh", "-s"]) (listGenerationsScript service)
  pure $ case code of
    ExitSuccess   -> Right (parseGenerations out)
    ExitFailure _ -> Left (DeploySsh (dhHost h) (firstLine err))

-- | @zinc deploy --rollback-to \<N\> \<host\>@ (nbk.7): switch the profile to
-- generation @N@. The caller follows with 'runActivate' to restart + health-check
-- (auto-rolling-back on failure, like a normal deploy).
runSwitchGeneration :: DeployHost -> String -> Int -> IO (Either ZincError ())
runSwitchGeneration h service n = do
  (code, _out, err) <-
    readProcessWithExitCode "ssh" (sshArgs h ["sh", "-s"]) (switchGenerationScript service n)
  pure $ case code of
    ExitSuccess   -> Right ()
    ExitFailure _ -> Left (DeployActivate (dhHost h) (firstLine err))

-- | The @--json@ data block for @deploy --list@ (nbk.7).
generationsJson :: [Generation] -> Json
generationsJson gens =
  JObject
    [ ( "generations"
      , JArray
          [ JObject
              [ ("generation", JInt (genNum g))
              , ("version", maybe JNull JString (genVersion g))
              , ("timestamp", JString (genTimestamp g))
              , ("current", JBool (genCurrent g))
              ]
          | g <- gens
          ]
      )
    ]

-- | The human rendering for @deploy --list@ (nbk.7): one release per line —
-- @gen N  vX.Y.Z  \<timestamp\>  (current)@.
renderGenerations :: [Generation] -> String
renderGenerations [] = "No deployed generations.\n"
renderGenerations gens = unlines (map row gens)
  where
    row g =
      "  gen " ++ show (genNum g)
        ++ "  " ++ maybe "(unversioned)" id (genVersion g)
        ++ "  " ++ genTimestamp g
        ++ (if genCurrent g then "  (current)" else "")

-- | The @--json@ data block for a ready host.
deployReadyJson :: DeployHost -> ProbeChecks -> Json
deployReadyJson h c =
  object
    [ ("host", Just (JString (dhHost h)))
    , ("user", JString <$> dhUser h)
    , ("port", JInt <$> dhPort h)
    , ("ready", Just (JBool True))
    , ( "checks"
      , Just
          ( JObject
              [ ("nix", JBool (pcNix c))
              , ("trusted", JBool (pcTrusted c))
              , ("linger", JBool (pcLinger c))
              ]
          )
      )
    ]
