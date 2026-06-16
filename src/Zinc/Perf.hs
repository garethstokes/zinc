-- | The @zinc perf@ analyzer (perf spec §3.2, L2). Reads the project-local
-- @.zinc/metrics.jsonl@ history written by "Zinc.Metrics" and reports command
-- latency (p50/p95), cache hit-rate, and a regression check of the latest run
-- against a rolling-median baseline. This closes the perf feedback loop for the
-- inner-loop (5ko) and caching (vwn) work: measure -> persist -> analyze.
--
-- "Slowest dependencies" (perf spec §3.2) ranks packages by cumulative compile
-- @timeMs@. Cache hits aren't compiles (0ms), so a fully-cached project reports
-- no slowest deps rather than a list of 0ms entries.
module Zinc.Perf
  ( PerfRecord (..)
  , CommandStats (..)
  , Regression (..)
  , PerfSummary (..)
  , decodeRecord
  , percentile
  , summarize
  , perfSummaryJson
  , renderPerf
  , readMetrics
  , runPerf
  ) where

import Data.List (sortBy, sort)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (Down), comparing)
import System.Directory (doesFileExist)
import Zinc.Json (Json (..), parseJson)
import Zinc.Metrics (metricsPath)
import Zinc.Report (fmtMs)

-- | The slice of a metrics record the analyzer needs (one build invocation).
data PerfRecord = PerfRecord
  { perfCommand  :: String
  , perfTotalMs  :: Int
  , perfHits     :: Int
  , perfMisses   :: Int
  , perfPackages :: [(String, Int)] -- ^ per-dependency build time (name -> ms)
  }
  deriving (Eq, Show)

-- | Latency stats for one command over the whole history.
data CommandStats = CommandStats
  { csCommand :: String
  , csCount   :: Int
  , csP50Ms   :: Int
  , csP95Ms   :: Int
  }
  deriving (Eq, Show)

-- | A detected slowdown: the latest run for a command vs the median of its
-- prior runs. @regSlowdown@ flags a regression over the threshold.
data Regression = Regression
  { regCommand    :: String
  , regBaselineMs :: Int
  , regCurrentMs  :: Int
  , regSlowdown   :: Bool
  }
  deriving (Eq, Show)

-- | The full analysis.
data PerfSummary = PerfSummary
  { sumRecords    :: Int
  , sumCommands   :: [CommandStats]
  , sumCacheHits  :: Int
  , sumCacheMiss  :: Int
  , sumRegression :: Maybe Regression
  , sumSlowest    :: [(String, Int, Int)] -- ^ (dependency, cumulative ms, build count), slowest first
  }
  deriving (Eq, Show)

-- | The slowest dependencies across all recorded builds, ranked by cumulative
-- compile time (perf spec §3.2). Returns up to the top N as
-- @(name, cumulativeMs, compiles)@. Cache hits aren't compiles (they record
-- 0ms), so they are excluded outright: a dep's count reflects how many times it
-- was actually compiled, not how many builds it appeared in, and a fully-cached
-- project reports no slowest deps rather than a list of 0ms entries.
slowestDeps :: [PerfRecord] -> [(String, Int, Int)]
slowestDeps recs =
  let byName = Map.fromListWith (\(t1, c1) (t2, c2) -> (t1 + t2, c1 + c2)) [(n, (ms, 1 :: Int)) | r <- recs, (n, ms) <- perfPackages r, ms > 0]
      ranked = sortBy (comparing (Down . (\(_, (t, _)) -> t))) (Map.toList byName)
   in [(n, t, c) | (n, (t, c)) <- take 5 ranked]

-- | Flag a regression when the latest run is at least this many times the
-- baseline median (perf spec §3.2: "build 2× slower than baseline").
regressionThreshold :: Double
regressionThreshold = 1.5

-- Small accessors over the parsed 'Json'.
jLookup :: String -> Json -> Maybe Json
jLookup k (JObject kvs) = lookup k kvs
jLookup _ _ = Nothing

jInt :: Json -> Maybe Int
jInt (JInt n) = Just n
jInt _ = Nothing

jStr :: Json -> Maybe String
jStr (JString s) = Just s
jStr _ = Nothing

-- | Decode one metrics record's analyzer-relevant fields, or 'Nothing' if the
-- shape doesn't match (forward/backward-compatible: unknown records are
-- skipped rather than failing the whole report).
decodeRecord :: Json -> Maybe PerfRecord
decodeRecord j = do
  cmd <- jLookup "command" j >>= jStr
  timing <- jLookup "timing" j
  total <- jLookup "totalMs" timing >>= jInt
  cache <- jLookup "cache" timing
  hits <- jLookup "hits" cache >>= jInt
  misses <- jLookup "misses" cache >>= jInt
  -- packages is optional (older records / non-build commands omit it).
  let pkgs = case jLookup "packages" j of
        Just (JArray xs) -> [(n, ms) | p <- xs, Just n <- [jLookup "name" p >>= jStr], Just ms <- [jLookup "timeMs" p >>= jInt]]
        _ -> []
  pure (PerfRecord cmd total hits misses pkgs)

-- | Nearest-rank percentile (1..100) of a list of samples.
percentile :: Int -> [Int] -> Int
percentile _ [] = 0
percentile p xs =
  let sorted = sort xs
      n = length sorted
      rank = max 1 (ceiling (fromIntegral p / 100 * fromIntegral n :: Double))
   in sorted !! (min n rank - 1)

-- | Compute the summary from a chronological list of records.
summarize :: [PerfRecord] -> PerfSummary
summarize recs =
  PerfSummary
    { sumRecords = length recs
    , sumCommands = map statsFor (distinct (map perfCommand recs))
    , sumCacheHits = sum (map perfHits recs)
    , sumCacheMiss = sum (map perfMisses recs)
    , sumRegression = regressionOf recs
    , sumSlowest = slowestDeps recs
    }
  where
    statsFor cmd =
      let ms = [perfTotalMs r | r <- recs, perfCommand r == cmd]
       in CommandStats cmd (length ms) (percentile 50 ms) (percentile 95 ms)

    -- The latest record's command, compared to the median of its prior runs.
    regressionOf [] = Nothing
    regressionOf rs =
      let latest = last rs
          cmd = perfCommand latest
          prior = [perfTotalMs r | r <- init rs, perfCommand r == cmd]
       in if null prior
            then Nothing
            else
              let base = percentile 50 prior
               in Just (Regression cmd base (perfTotalMs latest) (fromIntegral (perfTotalMs latest) >= regressionThreshold * fromIntegral base))

    distinct = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- | Cache hit-rate as a rounded percentage (0 when there is no cache activity).
hitRatePct :: PerfSummary -> Int
hitRatePct s =
  let total = sumCacheHits s + sumCacheMiss s
   in if total == 0 then 0 else round (100 * fromIntegral (sumCacheHits s) / fromIntegral total :: Double)

-- | The summary as JSON (the @--json@ surface).
perfSummaryJson :: PerfSummary -> Json
perfSummaryJson s =
  JObject
    [ ("records", JInt (sumRecords s))
    , ("commands", JArray (map commandJson (sumCommands s)))
    , ( "cache"
      , JObject
          [ ("hits", JInt (sumCacheHits s))
          , ("misses", JInt (sumCacheMiss s))
          , ("hitRatePct", JInt (hitRatePct s))
          ]
      )
    , ("regression", maybe JNull regressionJson (sumRegression s))
    , ("slowestDeps", JArray [JObject [("name", JString n), ("cumulativeMs", JInt t), ("builds", JInt c)] | (n, t, c) <- sumSlowest s])
    ]
  where
    commandJson c =
      JObject
        [ ("command", JString (csCommand c))
        , ("count", JInt (csCount c))
        , ("p50Ms", JInt (csP50Ms c))
        , ("p95Ms", JInt (csP95Ms c))
        ]
    regressionJson r =
      JObject
        [ ("command", JString (regCommand r))
        , ("baselineMs", JInt (regBaselineMs r))
        , ("currentMs", JInt (regCurrentMs r))
        , ("slowdown", JBool (regSlowdown r))
        ]

-- | A compact human rendering of the summary.
renderPerf :: PerfSummary -> String
renderPerf s
  | sumRecords s == 0 = "No build metrics yet (run a build; history lands in .zinc/metrics.jsonl).\n"
  | otherwise = unlines (header : map cmdLine (sumCommands s) ++ cacheLine : regLines ++ slowLines)
  where
    header = show (sumRecords s) ++ " build(s) recorded:"
    cmdLine c = "  " ++ csCommand c ++ ": " ++ show (csCount c) ++ " run(s), p50 " ++ fmtMs (csP50Ms c) ++ ", p95 " ++ fmtMs (csP95Ms c)
    cacheLine = "  cache: " ++ show (hitRatePct s) ++ "% hit-rate (" ++ show (sumCacheHits s) ++ " hits, " ++ show (sumCacheMiss s) ++ " misses)"
    slowLines = case sumSlowest s of
      [] -> []
      ds -> "  slowest deps (cumulative):" : ["    " ++ n ++ ": " ++ fmtMs t ++ " over " ++ show c ++ " compile(s)" | (n, t, c) <- ds]
    regLines = case sumRegression s of
      Nothing -> []
      Just r
        | regSlowdown r -> ["  REGRESSION: " ++ regCommand r ++ " " ++ fmtMs (regCurrentMs r) ++ " vs " ++ fmtMs (regBaselineMs r) ++ " baseline"]
        | otherwise -> ["  latest " ++ regCommand r ++ ": " ++ fmtMs (regCurrentMs r) ++ " vs " ++ fmtMs (regBaselineMs r) ++ " baseline (ok)"]

-- | Read and decode the workspace's metrics history (chronological). Missing
-- file or unparseable lines yield no records rather than an error.
readMetrics :: FilePath -> IO [PerfRecord]
readMetrics wsDir = do
  let path = metricsPath wsDir
  present <- doesFileExist path
  if not present
    then pure []
    else do
      ls <- lines <$> readFile path
      pure (mapMaybe decodeLine ls)
  where
    decodeLine l = case parseJson l of
      Right j -> decodeRecord j
      Left _  -> Nothing

-- | @zinc perf@: read the metrics history and summarize it.
runPerf :: FilePath -> IO PerfSummary
runPerf wsDir = summarize <$> readMetrics wsDir
