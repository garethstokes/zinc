-- | The introspection layer (spec §3.3): read-only, machine-readable views over
-- a workspace so an agent never has to infer structure or ordering.
--
--   * @zinc status@  — where am I: toolchain, members, the resolved closure
--     (cached vs to-build), and lock drift.
--   * @zinc graph@   — the build DAG (closure nodes + dependency edges + the
--     topological levels the closure builds in).
--   * @zinc explain@ — why a package is in the build: who requires it, at which
--     resolved revision.
module Zinc.Introspect
  ( DepStatus (..)
  , statusJson
  , graphJson
  , explainJson
  , runStatus
  , runGraph
  , runExplain
  , renderStatus
  , renderGraph
  , renderExplain
  ) where

import Data.List (intercalate, nub)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Zinc.Cache (BuildKey (..), buildCacheKey, storeConfPath)
import Zinc.Diagnostic (ZincError (NoZincToml))
import Zinc.Except (Result, failWithError, liftEither, liftIO, runResult)
import Zinc.Json (Json (..))
import Zinc.Lock (LockedPackage (..), lockRepo, lockRev, parseLock)
import Zinc.Manifest (Ref (Latest), WorkspaceManifest (wsDependencies, wsGhc, wsMembers), depName, depGhcOptionsOf, parseWorkspace)
import Zinc.Resolve (ResolvedDep (..), topoLevels)
import Zinc.Store (resolveStoreRoot)

-- | A dependency's line in @zinc status@: its resolved revision and whether a
-- build is already cached in the content-addressed store.
data DepStatus = DepStatus
  { dsName   :: String
  , dsRef    :: String
  , dsCached :: Bool
  }
  deriving (Eq, Show)

-- Shared plumbing -----------------------------------------------------------

-- | Read + parse the workspace manifest, failing with 'NoZincToml' if absent.
loadWorkspace :: FilePath -> Result (String, WorkspaceManifest)
loadWorkspace wsDir = do
  let wsFile = wsDir </> "zinc.toml"
  present <- liftIO (doesFileExist wsFile)
  if not present
    then failWithError (NoZincToml wsDir)
    else do
      src <- liftIO (readFile wsFile)
      ws <- liftEither (parseWorkspace src)
      pure (src, ws)

-- | Read the lockfile (empty when absent or unparseable).
loadLocks :: FilePath -> IO [LockedPackage]
loadLocks wsDir = do
  let lockFile = wsDir </> "zinc.lock"
  present <- doesFileExist lockFile
  if not present
    then pure []
    else either (const []) id . parseLock <$> readFile lockFile

-- | Drifted direct deps: names declared in the manifest but absent from the lock.
driftOf :: WorkspaceManifest -> [LockedPackage] -> [String]
driftOf ws locks =
  [depName d | d <- wsDependencies ws, depName d `notElem` map lockName locks]

-- status --------------------------------------------------------------------

-- | @zinc status@: gather toolchain, members, per-dep cache status, and drift.
-- Returns the pieces so the caller can render either JSON or human text.
runStatus :: FilePath -> IO (Either ZincError (String, [String], [DepStatus], [String]))
runStatus wsDir = runResult $ do
  (_, ws) <- loadWorkspace wsDir
  locks <- liftIO (loadLocks wsDir)
  storeRoot <- liftIO resolveStoreRoot
  let opts = depGhcOptionsOf ws
  deps <- liftIO (mapM (depStatus storeRoot (wsGhc ws) opts) locks)
  pure (wsGhc ws, wsMembers ws, deps, driftOf ws locks)
  where
    depStatus storeRoot ghc opts l = do
      let key = buildCacheKey (BuildKey (lockRev l) ghc (lockDepends l) (fromMaybe [] (lookup (lockName l) opts)))
      cached <- doesFileExist (storeConfPath storeRoot key)
      pure (DepStatus (lockName l) (lockRev l) cached)

statusJson :: String -> [String] -> [DepStatus] -> [String] -> Json
statusJson ghc members deps drift =
  JObject
    [ ("ghc", JString ghc)
    , ("members", JArray (map JString members))
    , ("dependencies", JArray (map depJson deps))
    , ("drift", JArray (map JString drift))
    ]
  where
    depJson d =
      JObject
        [ ("name", JString (dsName d))
        , ("ref", JString (dsRef d))
        , ("cached", JBool (dsCached d))
        ]

renderStatus :: String -> [String] -> [DepStatus] -> [String] -> String
renderStatus ghc members deps drift =
  unlines $
    ["GHC " ++ ghc, "members: " ++ list members, show (length deps) ++ " dependency(ies):"]
      ++ map depLine deps
      ++ ["lock drift: " ++ (if null drift then "none" else list drift)]
  where
    list xs = if null xs then "(none)" else intercalate ", " xs
    depLine d = "  " ++ dsName d ++ " @ " ++ take 8 (dsRef d) ++ (if dsCached d then " (cached)" else " (to build)")

-- graph ---------------------------------------------------------------------

-- | @zinc graph@: the closure build DAG (returns the locks for rendering).
runGraph :: FilePath -> IO (Either ZincError [LockedPackage])
runGraph wsDir = runResult $ do
  _ <- loadWorkspace wsDir
  liftIO (loadLocks wsDir)

graphJson :: [LockedPackage] -> Json
graphJson locks =
  JObject
    [ ("nodes", JArray (map JString names))
    , ("edges", JArray [edge (lockName l) d | l <- locks, d <- lockDepends l, d `elem` names])
    , ("levels", JArray (map (JArray . map JString) levels))
    ]
  where
    names = map lockName locks
    toResolved l = ResolvedDep (lockName l) (lockRepo l) Latest (lockDepends l)
    levels = either (const []) (map (map rdName)) (topoLevels (map toResolved locks))
    edge from to = JObject [("from", JString from), ("to", JString to)]

renderGraph :: [LockedPackage] -> String
renderGraph locks
  | null locks = "(empty closure)\n"
  | otherwise = unlines [lockName l ++ " -> " ++ deps (lockDepends l) | l <- locks]
  where
    deps ds = if null ds then "(no deps)" else intercalate ", " ds

-- explain -------------------------------------------------------------------

-- | @zinc explain \<pkg\>@: provenance for one package (returns the locks; the
-- caller already holds the package name).
runExplain :: FilePath -> IO (Either ZincError [LockedPackage])
runExplain wsDir = runResult $ do
  _ <- loadWorkspace wsDir
  liftIO (loadLocks wsDir)

explainJson :: String -> [LockedPackage] -> Json
explainJson pkg locks =
  JObject
    [ ("package", JString pkg)
    , ("inClosure", JBool (pkg `elem` map lockName locks))
    , ("ref", maybe JNull (JString . lockRev) (lookupLock pkg locks))
    , ("requiredBy", JArray (map JString (requiredBy pkg locks)))
    ]

lookupLock :: String -> [LockedPackage] -> Maybe LockedPackage
lookupLock pkg = foldr (\l acc -> if lockName l == pkg then Just l else acc) Nothing

-- | Direct dependents of @pkg@ within the closure (the workspace itself is the
-- implicit root for direct deps and is not listed).
requiredBy :: String -> [LockedPackage] -> [String]
requiredBy pkg locks = nub [lockName l | l <- locks, pkg `elem` lockDepends l]

renderExplain :: String -> [LockedPackage] -> String
renderExplain pkg locks =
  unlines $
    [pkg ++ (if inClosure then "" else " (NOT in the closure)")]
      ++ ["  ref: " ++ maybe "?" lockRev (lookupLock pkg locks) | inClosure]
      ++ ["  required by: " ++ (if null rb then "(direct dependency of the workspace)" else intercalate ", " rb)]
  where
    inClosure = pkg `elem` map lockName locks
    rb = requiredBy pkg locks
