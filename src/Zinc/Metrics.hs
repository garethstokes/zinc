-- | Project-local performance history (perf spec §3.1, L2). Each invocation's
-- 'Timing' plus light context is appended as one JSON line to
-- @\<workspace\>/.zinc/metrics.jsonl@ — append-only, gitignored, machine-local,
-- and preserved across @zinc clean@. The @zinc perf@ analyzer (hbv.3) reads it
-- back; this module owns the record shape, its JSON line, and the append.
module Zinc.Metrics
  ( MetricsRecord (..)
  , metricsRecordJson
  , metricsLine
  , appendMetrics
  , metricsPath
  , recordBuild
  ) where

import Control.Exception (SomeException, handle)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import Zinc.Json (Json (..), renderJson)
import Zinc.Manifest (parseWorkspace, wsGhc)
import Zinc.Report (Timing, timingJson)
import Zinc.Store (hashString)

-- | One recorded invocation: the timing block plus the context needed to
-- compare like with like over time (perf spec §3.1).
data MetricsRecord = MetricsRecord
  { mrCommand     :: String     -- ^ e.g. "build"
  , mrArgsSummary :: String     -- ^ compact arg summary, e.g. "" or a member name
  , mrLockHash    :: String     -- ^ fingerprint of zinc.lock (dependency set)
  , mrGhcVersion  :: String     -- ^ workspace GHC version
  , mrTimestamp   :: String     -- ^ ISO-8601 wall-clock time (supplied by caller)
  , mrTiming      :: Timing
  }
  deriving (Eq, Show)

-- | The record as JSON.
metricsRecordJson :: MetricsRecord -> Json
metricsRecordJson r =
  JObject
    [ ("timestamp", JString (mrTimestamp r))
    , ("command", JString (mrCommand r))
    , ("argsSummary", JString (mrArgsSummary r))
    , ("lockHash", JString (mrLockHash r))
    , ("ghcVersion", JString (mrGhcVersion r))
    , ("timing", timingJson (mrTiming r))
    ]

-- | The record as a single newline-terminated JSONL line.
metricsLine :: MetricsRecord -> String
metricsLine r = renderJson (metricsRecordJson r) ++ "\n"

-- | The metrics file for a workspace: @\<wsDir\>/.zinc/metrics.jsonl@.
metricsPath :: FilePath -> FilePath
metricsPath wsDir = wsDir </> ".zinc" </> "metrics.jsonl"

-- | Append a record to the workspace's metrics file (creating @.zinc@ if
-- needed). Append-only; never rewrites prior history.
appendMetrics :: FilePath -> MetricsRecord -> IO ()
appendMetrics wsDir r = do
  let path = metricsPath wsDir
  createDirectoryIfMissing True (takeDirectory path)
  appendFile path (metricsLine r)

-- | Record a completed build: gather context (timestamp, GHC version from the
-- manifest, a fingerprint of the lockfile) and append a metrics line. Best
-- effort — a metrics write failure must never fail the build, so all
-- exceptions are swallowed.
recordBuild :: FilePath -> String -> Maybe String -> Timing -> IO ()
recordBuild wsDir command target timing = handle ignore $ do
  ts <- iso8601Show <$> getCurrentTime
  wsSrc <- readFile (wsDir </> "zinc.toml")
  let ghc = either (const "?") wsGhc (parseWorkspace wsSrc)
  hasLock <- doesFileExist (wsDir </> "zinc.lock")
  lockHash <- if hasLock then hashString <$> readFile (wsDir </> "zinc.lock") else pure "none"
  appendMetrics wsDir (MetricsRecord command (maybe "" id target) lockHash ghc ts timing)
  where
    ignore :: SomeException -> IO ()
    ignore _ = pure ()
