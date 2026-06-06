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
  , CacheStats (..)
  , Timing (..)
  , cacheStatsOf
  , timingJson
  , buildSummaryLine
  , buildBreakdownLine
  , fmtMs
  ) where

import Zinc.Ansi (dim, greenBold)
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
  , prTimeMs :: Maybe Int -- ^ wall-clock build time (ms); 'Nothing' if unmeasured
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
    , ("timeMs", JInt <$> prTimeMs p)
    ]

-- | The @data@ block for the build envelope: @{ executables, packages }@.
buildDataJson :: BuildOutcome -> Json
buildDataJson o =
  JObject
    [ ("executables", JArray (map JString (boExes o)))
    , ("packages", JArray (map packageReportJson (boPackages o)))
    ]

-- | Cache effectiveness for a build (perf spec §2): closure deps reused from the
-- content-addressed store (@hits@ / @pkgsCached@) vs compiled from source
-- (@misses@ / @pkgsBuilt@). Library-less deps ('Skipped') count as neither.
data CacheStats = CacheStats
  { csHits       :: Int
  , csMisses     :: Int
  , csPkgsBuilt  :: Int
  , csPkgsCached :: Int
  }
  deriving (Eq, Show)

-- | The @timing@ block (perf spec §2): wall-clock total, per-phase durations
-- (ms, in build order), and cache stats. Hangs off the JSON envelope so perf
-- work (5ko inner-loop, vwn caching) has a measurement/validation feedback loop.
data Timing = Timing
  { tiTotalMs   :: Int
  , tiPhases    :: [(String, Int)] -- ^ coarse wall-clock phases (closure/member), ordered
  , tiBreakdown :: [(String, Int)] -- ^ finer CUMULATIVE per-phase work (fetch/compile/register/link), summed across parallel builds — NOT wall-clock, kept separate to avoid mixing units (zinc-nti.3)
  , tiCache     :: CacheStats
  }
  deriving (Eq, Show)

-- | Derive cache stats from the per-package report (no extra measurement).
cacheStatsOf :: [PackageReport] -> CacheStats
cacheStatsOf pkgs =
  CacheStats
    { csHits = cached
    , csMisses = built
    , csPkgsBuilt = built
    , csPkgsCached = cached
    }
  where
    cached = count Cached
    built = count Built
    count s = length (filter ((== s) . prStatus) pkgs)

-- | The hw6.3 human build-finish line: a cargo-style speed + cache summary,
-- e.g. @Finished in 3.2s · 22 packages (22 cached, 0 built)@. Surfaces zinc's
-- build-once content-addressed store — most of a warm closure is reused, not
-- rebuilt — and reproducibility (the same inputs hit the same cache). @color@
-- gates ANSI; the verb is right-aligned in the same 12-col gutter as progress.
buildSummaryLine :: Bool -> Timing -> String
buildSummaryLine color t =
  greenBold color (pad "Finished")
    ++ " in "
    ++ fmtMs (tiTotalMs t)
    ++ " "
    ++ dim color ("\183 " ++ show n ++ " package" ++ (if n == 1 then "" else "s") ++ " (" ++ show cached ++ " cached, " ++ show built ++ " built)")
  where
    pad s = replicate (max 0 (12 - length s)) ' ' ++ s
    cached = csPkgsCached (tiCache t)
    built = csPkgsBuilt (tiCache t)
    n = cached + built

-- | The optional finer-phase line shown under the build summary (zinc-nti.3):
-- @  breakdown · fetch 1.2s · compile 8.4s · register 0.3s · link 2.1s (cumulative)@.
-- 'Nothing' when no instrumented phase ran (a no-op build), so trivial builds
-- keep their clean one-line summary. A fully dep-cached build still shows the
-- member recompile time (the workspace members re-run @ghc --make@ regardless of
-- the cached closure). Cumulative across the parallel closure builds, so the sum
-- exceeds wall-clock — labelled as such to keep the units honest.
buildBreakdownLine :: Bool -> Timing -> Maybe String
buildBreakdownLine color t
  | null phs  = Nothing
  | otherwise = Just (dim color ("  breakdown" ++ concatMap part phs ++ " (cumulative)"))
  where
    phs = [(p, ms) | (p, ms) <- tiBreakdown t, ms > 0]
    part (p, ms) = " \183 " ++ p ++ " " ++ fmtMs ms

-- | Render a millisecond duration compactly: @3.2s@ at or above a second
-- (one decimal), else @450ms@.
fmtMs :: Int -> String
fmtMs ms
  | ms >= 1000 = show (ms `div` 1000) ++ "." ++ show (ms `mod` 1000 `div` 100) ++ "s"
  | otherwise  = show ms ++ "ms"

-- | A 'Timing' as JSON: @{ totalMs, phases:{…}, breakdown:{…}, cache:{…} }@.
-- @breakdown@ (cumulative per-phase work; zinc-nti.3) is omitted when empty.
timingJson :: Timing -> Json
timingJson t =
  JObject $
    [ ("totalMs", JInt (tiTotalMs t))
    , ("phases", JObject [(p, JInt ms) | (p, ms) <- tiPhases t])
    ]
      ++ [("breakdown", JObject [(p, JInt ms) | (p, ms) <- tiBreakdown t]) | not (null (tiBreakdown t))]
      ++ [ ( "cache"
      , JObject
          [ ("hits", JInt (csHits c))
          , ("misses", JInt (csMisses c))
          , ("pkgsBuilt", JInt (csPkgsBuilt c))
          , ("pkgsCached", JInt (csPkgsCached c))
          ]
      )
    ]
  where
    c = tiCache t

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
renderRef (Tag t)      = t
renderRef (Branch b)   = b
renderRef (Rev r)      = take 8 r
renderRef Latest       = "*"
renderRef (Vendored v) = v ++ " (vendored)"
