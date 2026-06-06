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
  ) where

import Data.Char (isDigit)
import Data.List (find)
import System.Exit (ExitCode (ExitFailure))
import System.Process (readProcessWithExitCode)
import Zinc.Diagnostic
  ( ZincError (DeployNoLinger, DeployNoNix, DeployNotTrusted, DeploySsh)
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
  }
  deriving (Eq, Show)

-- | Resolve the @zinc deploy \<arg\>@ argument: if @arg@ names a @[deploy.*]@
-- target use its host/service/args/env; otherwise treat @arg@ as an ad-hoc
-- @[user@]host[:port]@ / ssh alias. A @--service@ override always wins over the
-- configured service.
resolveDeploy :: [DeployTarget] -> String -> Maybe String -> ResolvedDeploy
resolveDeploy targets arg svcOverride = case find ((== arg) . dtName) targets of
  Just t  -> ResolvedDeploy (parseDeployHost (dtHost t)) (svcOverride `orConfig` dtService t) (dtArgs t) (dtEnv t)
  Nothing -> ResolvedDeploy (parseDeployHost arg) svcOverride [] []
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
