-- | @zinc doctor@ (spec §3.4): diagnose environment and project problems, each
-- finding carrying a 'nextAction', emitted through the diagnostic core. Checks
-- are local and fast (no network/fetch): is Nix present, are flakes enabled, is
-- there a workspace here, is the lock in sync. Fetch-requiring checks
-- (Custom-Setup deps, unresolvable refs) are a follow-up.
module Zinc.Doctor
  ( runDoctor
  , doctorOk
  , doctorJson
  , renderDoctor
  , lockDriftDiagnostic
  , flakesOffDiagnostic
  ) where

import Data.List (intercalate)
import Data.Maybe (catMaybes)
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Zinc.Diagnostic (Diagnostic (..), Severity (..), ZincError (NixAbsent, NoZincToml), envelope, toDiagnostic)
import Zinc.Json (Json)
import Zinc.Orchestrate (checkLockDrift)

-- | Run all checks for the workspace at @wsDir@, returning the problems found
-- (an empty list means a clean bill of health).
runDoctor :: FilePath -> IO [Diagnostic]
runDoctor wsDir =
  catMaybes
    <$> sequence
      [ checkNix
      , checkFlakes
      , checkWorkspace wsDir
      , checkDrift wsDir
      ]

-- | Is @nix@ on PATH? zinc uses it to provision the toolchain.
checkNix :: IO (Maybe Diagnostic)
checkNix = do
  mnix <- findExecutable "nix"
  pure $ maybe (Just (toDiagnostic NixAbsent)) (const Nothing) mnix

-- | Are flakes (and the new CLI) enabled? Probed with @nix flake --help@, which
-- errors when the @nix-command@/@flakes@ experimental features are off. Skipped
-- when Nix is absent (that is already reported by 'checkNix').
checkFlakes :: IO (Maybe Diagnostic)
checkFlakes = do
  mnix <- findExecutable "nix"
  case mnix of
    Nothing -> pure Nothing
    Just _ -> do
      (code, _, _) <- readProcessWithExitCode "nix" ["flake", "--help"] ""
      pure $ case code of
        ExitSuccess -> Nothing
        _ -> Just flakesOffDiagnostic

-- | The flakes-disabled finding.
flakesOffDiagnostic :: Diagnostic
flakesOffDiagnostic =
  Diagnostic
    { diagCode = "ZINC_NIX_FLAKES_OFF"
    , diagSeverity = SWarning
    , diagTitle = "Nix flakes are not enabled"
    , diagDetail = Just "the nix-command and flakes experimental features are off"
    , diagLocation = Nothing
    , diagPackage = Nothing
    , diagNextAction = Just "add `experimental-features = nix-command flakes` to your nix.conf"
    }

-- | Is there a workspace manifest here at all?
checkWorkspace :: FilePath -> IO (Maybe Diagnostic)
checkWorkspace wsDir = do
  present <- doesFileExist (wsDir </> "zinc.toml")
  pure $ if present then Nothing else Just (toDiagnostic (NoZincToml wsDir))

-- | Does the lockfile cover every declared dependency?
checkDrift :: FilePath -> IO (Maybe Diagnostic)
checkDrift wsDir = do
  present <- doesFileExist (wsDir </> "zinc.toml")
  if not present
    then pure Nothing
    else lockDriftDiagnostic <$> checkLockDrift wsDir

-- | The lock-drift finding for a set of uncovered dependency names ('Nothing'
-- when the lock is in sync).
lockDriftDiagnostic :: [String] -> Maybe Diagnostic
lockDriftDiagnostic [] = Nothing
lockDriftDiagnostic ds =
  Just
    Diagnostic
      { diagCode = "ZINC_LOCK_DRIFT"
      , diagSeverity = SWarning
      , diagTitle = "zinc.lock is out of date"
      , diagDetail = Just ("not in the lock: " ++ intercalate ", " ds)
      , diagLocation = Nothing
      , diagPackage = Nothing
      , diagNextAction = Just "run `zinc add` (or `zinc update`) to refresh the lock"
      }

-- | A clean bill of health: no error-severity findings (warnings are tolerated).
doctorOk :: [Diagnostic] -> Bool
doctorOk = not . any ((== SError) . diagSeverity)

-- | The doctor report as the standard JSON envelope (findings as diagnostics).
doctorJson :: [Diagnostic] -> Json
doctorJson diags = envelope "doctor" (doctorOk diags) Nothing Nothing diags

-- | A compact human rendering.
renderDoctor :: [Diagnostic] -> String
renderDoctor [] = "All checks passed. No problems found.\n"
renderDoctor diags = unlines (map line diags)
  where
    line d =
      sev (diagSeverity d)
        ++ diagTitle d
        ++ maybe "" (\x -> ": " ++ x) (diagDetail d)
        ++ maybe "" (\x -> "\n    -> " ++ x) (diagNextAction d)
    sev SError = "error: "
    sev SWarning = "warning: "
    sev SInfo = "info: "
