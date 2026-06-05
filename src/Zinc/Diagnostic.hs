-- | The diagnostic core (agent-devex spec §3.1): one structured error model that
-- every command emits through. Pipelines fail with a 'ZincError' /value/ (never
-- a message string); the single boundary renderer 'toDiagnostic' is the only
-- place codes\/messages\/next-actions are produced, and it feeds the human
-- renderer, the JSON 'envelope', and the process 'exitCodeFor'.
module Zinc.Diagnostic
  ( ZincError (..)
  , Severity (..)
  , Diagnostic (..)
  , SourceLocation (..)
  , ghcLocation
  , tomlLocation
  , toDiagnostic
  , renderError
  , humanError
  , errorCode
  , exitCodeFor
  , diagnosticJson
  , envelope
  , zincVersion
  ) where

import Data.Char (isDigit)
import Data.List (find, intercalate, isInfixOf, stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe)
import Zinc.Ansi (cyan, dim, red, redBold)
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
  | DepNoGitRepo String               -- ^ space-separated package names with no upstream git repo (vendor targets)
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

-- | A structured source location for a diagnostic (hw6.5): enough to draw an
-- elm-style caret under the offending span. Line/col/span/excerpt are all
-- optional so a location can degrade to a bare file when a tool gives no
-- position. Columns are 1-based, matching GHC and toml-parser.
data SourceLocation = SourceLocation
  { locFile    :: String
  , locLine    :: Maybe Int
  , locCol     :: Maybe Int
  , locEndLine :: Maybe Int
  , locEndCol  :: Maybe Int
  , locExcerpt :: Maybe String  -- ^ the offending source line, for caret rendering
  }
  deriving (Eq, Show)

-- | A bare-file location (no position), the graceful floor when a tool reports
-- only which file failed.
fileLocation :: String -> SourceLocation
fileLocation f = SourceLocation f Nothing Nothing Nothing Nothing Nothing

-- | Extract a 'SourceLocation' from GHC @--make@ stderr: the first
-- @path:line:col:@ header, the matching @\<n\> | \<source\>@ gutter line as the
-- excerpt, and the @^^^^@ caret run (when present) as the span end-column.
-- Best-effort: 'Nothing' if no header is recognizable.
ghcLocation :: String -> Maybe SourceLocation
ghcLocation out = listToMaybe (mapMaybe header ls)
  where
    ls = lines out
    header ln = do
      (file, l, c) <- parseHeader ln
      let excerpt = gutterFor l
          endC = (\n -> c + n) <$> caretWidth
      pure (SourceLocation file (Just l) (Just c) Nothing endC excerpt)
    -- "path:line:col:" — path is everything up to the first ":<digit". Require a
    -- non-empty, non-numeric path so a "1:8:" toml position isn't read as a file.
    parseHeader ln = case break (== ':') ln of
      (file, ':' : rest1)
        | not (null file) && not (all isDigit file) ->
            case spanDigits rest1 of
              (l@(_ : _), ':' : rest2) ->
                case spanDigits rest2 of
                  (c@(_ : _), ':' : _) -> Just (file, read l, read c)
                  _ -> Nothing
              _ -> Nothing
      _ -> Nothing
    -- The "<n> | <source>" line GHC prints for line n; excerpt is <source>.
    gutterFor n =
      let pfx = show n ++ " | "
       in stripPrefix pfx . dropWhile (== ' ') =<< find (isInfixOf pfx) ls
    -- Width of the "^^^^" caret run GHC underlines the span with, if any.
    caretWidth = case filter (\l -> '^' `elem` l && all (`elem` " |^") l) ls of
      (l : _) -> Just (length (filter (== '^') l))
      _       -> Nothing

-- | Extract a 'SourceLocation' from a toml-parser error for a known file: the
-- leading @line:col:@ position toml-parser prefixes its message with. Falls
-- back to a bare-file location when no position is present.
tomlLocation :: String -> String -> SourceLocation
tomlLocation file detail =
  case spanDigits (dropWhile (== ' ') detail) of
    (l@(_ : _), ':' : rest) ->
      case spanDigits rest of
        (c@(_ : _), ':' : _) -> (fileLocation file) {locLine = Just (read l), locCol = Just (read c)}
        _ -> fileLocation file
    _ -> fileLocation file

-- | Split a leading run of digits off a string (like 'span' 'isDigit').
spanDigits :: String -> (String, String)
spanDigits = span isDigit

-- | The agent-facing shape of a rendered error (spec §3.1). Optional fields are
-- omitted from JSON when absent.
data Diagnostic = Diagnostic
  { diagCode       :: String
  , diagSeverity   :: Severity
  , diagTitle      :: String
  , diagDetail     :: Maybe String
  , diagLocation   :: Maybe SourceLocation
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
      DepNoGitRepo names ->
        ( "dependency has no git repository", Just (names ++ " not available from any git repo (darcs-era / no source-repository)"), Nothing, Just names
        , Just ("run `zinc vendor " ++ names ++ "` to pin it from its Hackage tarball (sha256)") )
      BuildTypeCustom name ->
        ( "Custom build-type is not supported", Just (name ++ " uses a Setup.hs (build-type: Custom)"), Nothing, Just name
        , Just "pin a version with build-type: Simple, or vendor a Simple-built variant" )
      GhcCompile pkg d ->
        ( "compilation failed", Just d, ghcLocation d, Just pkg
        , Just "fix the reported compile error; for dep-specific flags use [build-options]" )
      AmbiguousTarget cands ->
        ( "ambiguous run target", Just ("candidates: " ++ unwords cands), Nothing, Nothing
        , Just "name the executable: `zinc run <exe>` (or member:exe)" )
      ManifestParse file d ->
        ( "manifest parse error", Just d, Just (tomlLocation file d), Nothing
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

-- | The rich, multi-line human rendering of a 'Diagnostic' (hw6.2, minimal
-- style): a status line, an indented source block with a caret under the
-- offending span when the diagnostic carries a 'SourceLocation' with an
-- excerpt (e.g. GHC compile errors via hw6.5), and a @help:@ footer from the
-- next action. Extends the one-line 'renderError'; @color@ gates all ANSI.
humanError :: Bool -> Diagnostic -> String
humanError color d = intercalate "\n" (statusLine : body)
  where
    statusLine = red color "\10007" ++ " " ++ redBold color (diagTitle d) ++ locSuffix
    locSuffix = maybe "" (\l -> "  " ++ dim color (renderLoc l)) (diagLocation d)
    body = sourceOrDetail ++ helpLines
    -- With a caret-able location (excerpt + column) draw the source block; the
    -- primary GHC "•" message annotates the caret. Otherwise fall back to the
    -- detail text (toml message, generic errors) indented under the status.
    sourceOrDetail = case diagLocation d of
      Just l
        | Just ex <- locExcerpt l
        , Just c <- locCol l ->
            "" : sourceBlock color l ex c (primaryMessage =<< diagDetail d)
      _ -> maybe [] (\dt -> ["", indentLines dt]) (diagDetail d)
    helpLines = maybe [] (\h -> ["", "   " ++ cyan color "help" ++ ": " ++ h]) (diagNextAction d)
    indentLines = intercalate "\n" . map ("   " ++) . lines

-- | @file:line:col@ for a status line (degrades to file, or file:line).
renderLoc :: SourceLocation -> String
renderLoc l = locFile l ++ case locLine l of
  Nothing -> ""
  Just ln -> ":" ++ show ln ++ maybe "" ((":" ++) . show) (locCol l)

-- | The two-line source block: the numbered excerpt and a caret run under the
-- offending span (columns @col@..@endCol@), optionally annotated with the
-- primary message. The gutter pipe on line 2 aligns under line 1's.
sourceBlock :: Bool -> SourceLocation -> String -> Int -> Maybe String -> [String]
sourceBlock color l ex c msg =
  [ "   " ++ lnStr ++ " " ++ pipe ++ " " ++ ex
  , "   " ++ replicate (length lnStr) ' ' ++ " " ++ pipe ++ " " ++ replicate (c - 1) ' ' ++ carets ++ inline
  ]
  where
    lnStr = maybe "" show (locLine l)
    pipe = dim color "\9474"                                   -- │
    width = maybe 1 (\e -> max 1 (e - c)) (locEndCol l)
    carets = red color (replicate width '^')
    inline = maybe "" (" " ++) msg

-- | The first GHC @•@-bulleted line (its primary cause) for the inline caret
-- annotation; 'Nothing' when the detail has no bullet (non-GHC diagnostics).
primaryMessage :: String -> Maybe String
primaryMessage detail =
  listToMaybe [dropWhile (== ' ') (drop 1 (dropWhile (/= '\8226') ln)) | ln <- lines detail, '\8226' `elem` ln]

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
    , ("location", locationJson <$> diagLocation d)
    , ("package", JString <$> diagPackage d)
    , ("nextAction", JString <$> diagNextAction d)
    ]

-- | A 'SourceLocation' as a JSON object (absent position fields omitted), so a
-- machine consumer can position a caret without re-parsing tool output.
locationJson :: SourceLocation -> Json
locationJson l =
  object
    [ ("file", Just (JString (locFile l)))
    , ("line", JInt <$> locLine l)
    , ("col", JInt <$> locCol l)
    , ("endLine", JInt <$> locEndLine l)
    , ("endCol", JInt <$> locEndCol l)
    , ("excerpt", JString <$> locExcerpt l)
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
