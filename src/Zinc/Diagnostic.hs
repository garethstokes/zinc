-- | The diagnostic core (agent-devex spec §3.1): one structured error model that
-- every command emits through. Pipelines fail with a 'ZincError' /value/ (never
-- a message string); the single boundary renderer 'toDiagnostic' is the only
-- place codes\/messages\/next-actions are produced, and it feeds the human
-- renderer, the JSON 'envelope', and the process 'exitCodeFor'.
module Zinc.Diagnostic
  ( ZincError (..)
  , Severity (..)
  , Diagnostic (..)
  , toDiagnostic
  , renderError
  , errorCode
  , exitCodeFor
  , diagnosticJson
  , envelope
  , zincVersion
  ) where

import System.Exit (ExitCode (..))
import Zinc.Json (Json (..), object)

-- | This zinc's version, surfaced in the JSON envelope.
zincVersion :: String
zincVersion = "0.1.0.0"

-- | A structured failure. Throw sites construct a value (e.g.
-- @throwE (DepNoGitRepo "colour")@), never a formatted message — so a new error
-- kind cannot be silently unhandled, and rendering is centralised. Fields are
-- positional; see each constructor's comment.
data ZincError
  = RefNotFound String String         -- ^ repo, ref
  | CloneFailed String String         -- ^ repo, detail
  | GitAuth String                    -- ^ repo (private/auth-required fetch)
  | ContentHashMismatch String String String -- ^ name, expected, got
  | DepNoGitRepo String               -- ^ name (no upstream git repo)
  | BuildTypeCustom String            -- ^ name (Custom Setup.hs unsupported)
  | GhcCompile String String          -- ^ package, detail
  | AmbiguousTarget [String]          -- ^ candidate exe names
  | ManifestParse String String       -- ^ file, detail
  | NixAbsent
  | ToolchainMissing String            -- ^ a required build tool (e.g. ghc) is not on PATH
  | NoZincToml String                 -- ^ directory
  | NoRepoInRegistry String String    -- ^ name, requiring parent
  | OtherError String                 -- ^ escape hatch for not-yet-migrated messages
  deriving (Eq, Show)

data Severity = SError | SWarning | SInfo
  deriving (Eq, Show)

-- | The agent-facing shape of a rendered error (spec §3.1). Optional fields are
-- omitted from JSON when absent.
data Diagnostic = Diagnostic
  { diagCode       :: String
  , diagSeverity   :: Severity
  , diagTitle      :: String
  , diagDetail     :: Maybe String
  , diagLocation   :: Maybe String
  , diagPackage    :: Maybe String
  , diagNextAction :: Maybe String
  }
  deriving (Eq, Show)

-- | The stable error-code taxonomy — one code per 'ZincError' constructor.
errorCode :: ZincError -> String
errorCode e = case e of
  RefNotFound {}         -> "ZINC_REF_NOT_FOUND"
  CloneFailed {}         -> "ZINC_CLONE_FAILED"
  GitAuth {}             -> "ZINC_GIT_AUTH"
  ContentHashMismatch {} -> "ZINC_CONTENT_HASH_MISMATCH"
  DepNoGitRepo {}        -> "ZINC_DEP_NO_GIT_REPO"
  BuildTypeCustom {}     -> "ZINC_BUILD_TYPE_CUSTOM"
  GhcCompile {}          -> "ZINC_GHC_COMPILE"
  AmbiguousTarget {}     -> "ZINC_AMBIGUOUS_TARGET"
  ManifestParse {}       -> "ZINC_MANIFEST_PARSE"
  NixAbsent              -> "ZINC_NIX_ABSENT"
  ToolchainMissing {}    -> "ZINC_TOOLCHAIN_MISSING"
  NoZincToml {}          -> "ZINC_NO_ZINC_TOML"
  NoRepoInRegistry {}    -> "ZINC_NO_REPO_IN_REGISTRY"
  OtherError {}          -> "ZINC_ERROR"

-- | The single boundary renderer: 'ZincError' to the agent-facing 'Diagnostic'.
-- The /only/ place messages and next-actions are produced.
toDiagnostic :: ZincError -> Diagnostic
toDiagnostic e =
  Diagnostic
    { diagCode = errorCode e
    , diagSeverity = SError
    , diagTitle = title
    , diagDetail = detail
    , diagLocation = location
    , diagPackage = package
    , diagNextAction = nextAction
    }
  where
    (title, detail, location, package, nextAction) = case e of
      RefNotFound repo ref ->
        ( "could not resolve ref", Just (ref ++ " in " ++ repo), Nothing, Nothing
        , Just "check the tag/branch/rev exists in the repo, or use a different ref" )
      CloneFailed repo d ->
        ( "git clone failed", Just (repo ++ ": " ++ d), Nothing, Nothing
        , Just "check the repo URL and network access" )
      GitAuth repo ->
        ( "git authentication required", Just repo, Nothing, Nothing
        , Just "configure git credentials (SSH key or token) for this private repo" )
      ContentHashMismatch name expected got ->
        ( "dependency content hash mismatch", Just ("expected " ++ expected ++ ", got " ++ got), Nothing, Just name
        , Just "run `zinc update` to refresh the lock, or verify the source has not been tampered with" )
      DepNoGitRepo name ->
        ( "dependency has no git repository", Just (name ++ " is not available from any git repo"), Nothing, Just name
        , Just ("vendor " ++ name ++ " into a git mirror and map it in [registry]") )
      BuildTypeCustom name ->
        ( "Custom build-type is not supported", Just (name ++ " uses a Setup.hs (build-type: Custom)"), Nothing, Just name
        , Just "pin a version with build-type: Simple, or vendor a Simple-built variant" )
      GhcCompile pkg d ->
        ( "compilation failed", Just d, Nothing, Just pkg
        , Just "fix the reported compile error; for dep-specific flags use [build-options]" )
      AmbiguousTarget cands ->
        ( "ambiguous run target", Just ("candidates: " ++ unwords cands), Nothing, Nothing
        , Just "name the executable: `zinc run <exe>` (or member:exe)" )
      ManifestParse file d ->
        ( "manifest parse error", Just d, Just file, Nothing
        , Just "fix the TOML in the manifest" )
      NixAbsent ->
        ( "Nix is not available", Nothing, Nothing, Nothing
        , Just "install Nix (flakes enabled) or enter the dev shell with `nix develop`" )
      ToolchainMissing tool ->
        ( "required toolchain not found", Just (tool ++ " is not on PATH"), Nothing, Nothing
        , Just "enter the dev shell with `nix develop` (it provides GHC), or install GHC onto PATH" )
      NoZincToml dir ->
        ( "no zinc.toml found", Just ("expected a workspace manifest in " ++ dir), Nothing, Nothing
        , Just "run `zinc new <name>` to scaffold a workspace, or cd into one" )
      NoRepoInRegistry name parent ->
        ( "no repo in [registry] for dependency", Just (name ++ " (required by " ++ parent ++ ")"), Nothing, Just name
        , Just ("add `" ++ name ++ " = \"<git-url>\"` to the workspace [registry]") )
      OtherError msg ->
        ( msg, Nothing, Nothing, Nothing, Nothing )

-- | A human one-line rendering of an error, for the plain-text CLI surface and
-- test failure messages: @title@ (plus @detail@ when present). The next-action
-- and code are part of the richer 'Diagnostic'/JSON surface, not this line.
renderError :: ZincError -> String
renderError e =
  let d = toDiagnostic e
   in diagTitle d ++ maybe "" (\x -> ": " ++ x) (diagDetail d)

-- | The process exit code for an error category, so agents branch without
-- parsing. 2 = usage/manifest, 3 = resolution/fetch, 4 = build, 5 = environment,
-- 6 = integrity, 1 = other.
exitCodeFor :: ZincError -> ExitCode
exitCodeFor e = ExitFailure $ case e of
  NoZincToml {}          -> 2
  ManifestParse {}       -> 2
  AmbiguousTarget {}     -> 2
  RefNotFound {}         -> 3
  CloneFailed {}         -> 3
  GitAuth {}             -> 3
  DepNoGitRepo {}        -> 3
  NoRepoInRegistry {}    -> 3
  GhcCompile {}          -> 4
  BuildTypeCustom {}     -> 4
  NixAbsent              -> 5
  ToolchainMissing {}    -> 5
  ContentHashMismatch {} -> 6
  OtherError {}          -> 1

-- | A 'Diagnostic' as JSON (optional fields omitted when absent).
diagnosticJson :: Diagnostic -> Json
diagnosticJson d =
  object
    [ ("code", Just (JString (diagCode d)))
    , ("severity", Just (JString (severityText (diagSeverity d))))
    , ("title", Just (JString (diagTitle d)))
    , ("detail", JString <$> diagDetail d)
    , ("location", JString <$> diagLocation d)
    , ("package", JString <$> diagPackage d)
    , ("nextAction", JString <$> diagNextAction d)
    ]

severityText :: Severity -> String
severityText SError = "error"
severityText SWarning = "warning"
severityText SInfo = "info"

-- | The JSON envelope every @--json@ command emits:
-- @{ zinc, command, ok, data, timing?, diagnostics }@. The @timing@ block
-- (perf spec §2) is included only when measured; 'envelope' omits it otherwise.
envelope :: String -> Bool -> Maybe Json -> Maybe Json -> [Diagnostic] -> Json
envelope command ok dat timing diags =
  JObject $
    [ ("zinc", JString zincVersion)
    , ("command", JString command)
    , ("ok", JBool ok)
    , ("data", maybe JNull id dat)
    ]
      ++ maybe [] (\t -> [("timing", t)]) timing
      ++ [("diagnostics", JArray (map diagnosticJson diags))]
