-- | Human-facing rendering for diagnostics & error UX (epic zinc-unv) plus the
-- machine-readable build report (spec §3.2): the resolution table shown on
-- @resolve@ / @build@ / @add@, and the structured per-package build outcome
-- emitted on @zinc build --json@.
module Zinc.Report
  ( renderResolution
  , PackageStatus (..)
  , PackageReport (..)
  , BuildOutcome (..)
  , statusText
  , packageReportJson
  , buildDataJson
  ) where

import Zinc.Json (Json (..), object)
import Zinc.Manifest (Ref (..))
import Zinc.Resolve (ResolvedDep (..))

-- | What happened to a dependency in a build (spec §3.2). 'Cached' reused a
-- content-addressed store build; 'Built' compiled from source; 'Skipped' ships
-- no library to build; 'Failed' errored (reserved — a failure currently aborts
-- the whole build, surfaced as the envelope's diagnostic rather than per-pkg).
data PackageStatus = Cached | Built | Skipped | Failed
  deriving (Eq, Show)

-- | A dependency's line in the build report. @timeMs@ / @ghcInvocation@ /
-- per-package @diagnostics@ (spec §3.2) are layered on by later beads (timing:
-- hbv.1); this carries the always-available identity + status.
data PackageReport = PackageReport
  { prName   :: String
  , prRef    :: String
  , prStatus :: PackageStatus
  }
  deriving (Eq, Show)

-- | The outcome of a build: the executables produced plus the per-package
-- closure report. Returned by the report-bearing build entry point and emitted
-- as the @data@ block of the @zinc build --json@ envelope.
data BuildOutcome = BuildOutcome
  { boExes     :: [FilePath]
  , boPackages :: [PackageReport]
  }
  deriving (Eq, Show)

-- | The stable wire string for a status.
statusText :: PackageStatus -> String
statusText Cached  = "cached"
statusText Built   = "built"
statusText Skipped = "skipped"
statusText Failed  = "failed"

-- | A 'PackageReport' as JSON: @{ name, ref, status }@.
packageReportJson :: PackageReport -> Json
packageReportJson p =
  object
    [ ("name", Just (JString (prName p)))
    , ("ref", Just (JString (prRef p)))
    , ("status", Just (JString (statusText (prStatus p))))
    ]

-- | The @data@ block for the build envelope: @{ executables, packages }@.
buildDataJson :: BuildOutcome -> Json
buildDataJson o =
  JObject
    [ ("executables", JArray (map JString (boExes o)))
    , ("packages", JArray (map packageReportJson (boPackages o)))
    ]

-- | Render the resolved closure as an aligned @package / ref / repo@ table.
renderResolution :: [ResolvedDep] -> String
renderResolution [] = "(no dependencies)\n"
renderResolution rds = unlines (header : map row rds)
  where
    nameW = maximum (length "package" : map (length . rdName) rds)
    refW = maximum (length "ref" : map (length . renderRef . rdRef) rds)
    pad w s = s ++ replicate (w - length s) ' '
    header = pad nameW "package" ++ "  " ++ pad refW "ref" ++ "  repo"
    row d = pad nameW (rdName d) ++ "  " ++ pad refW (renderRef (rdRef d)) ++ "  " ++ rdRepo d

-- | Compact display form of a ref.
renderRef :: Ref -> String
renderRef (Tag t)    = t
renderRef (Branch b) = b
renderRef (Rev r)    = take 8 r
renderRef Latest     = "*"
