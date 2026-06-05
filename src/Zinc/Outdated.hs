-- | @zinc outdated@ (zinc-90j.1): a read-only report of each dependency's
-- current pin vs. the newest version available upstream. Direct deps by
-- default; @--all@ widens to the whole resolved closure (informational). Newest
-- means /latest available/ (no bounds, no compatibility solve) — git tags via
-- @git ls-remote@ (no clone), or the Hackage version for a vendored dep. Boot
-- libraries are excluded. Distinct from @update --dry-run@: this never resolves
-- or writes anything.
module Zinc.Outdated
  ( Status (..)
  , OutdatedDep (..)
  , classify
  , runOutdated
  , outdatedJson
  , renderOutdated
  ) where

import Data.List (sortOn)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Zinc.Diagnostic (ZincError (NoZincToml))
import Zinc.Except (Result, failWithError, liftEither, liftIO, runResult)
import Zinc.Git (listTags, splitRepoSubdir)
import Zinc.Hackage (hackageLatestVersion)
import Zinc.Json (Json (..), object)
import Zinc.Lock (LockedPackage (..), Source (..), lockRepo, parseLock)
import Zinc.Manifest (Dependency (..), Ref (..), WorkspaceManifest (wsDependencies), parseWorkspace)
import Zinc.Resolve (isBootLib)
import Zinc.Version (newestTagFor, parseVersion)
import Data.Maybe (isJust)

-- | A dependency's currency relative to upstream.
data Status
  = UpToDate          -- ^ current is the newest (or newer than) available
  | Behind Bool       -- ^ a newer version exists; 'True' if it's a major jump
  | Unknown           -- ^ can't compare (current is a bare commit, or no upstream version)
  deriving (Eq, Show)

-- | One row of the report.
data OutdatedDep = OutdatedDep
  { odName    :: String
  , odCurrent :: String        -- ^ the current pin (tag/version, or a short commit)
  , odNewest  :: Maybe String  -- ^ newest available upstream ('Nothing' if undiscoverable)
  , odStatus  :: Status
  }
  deriving (Eq, Show)

-- | Compare a current pin to the newest available version. Both must parse as
-- dotted versions to judge currency; a bare commit (or missing upstream) is
-- 'Unknown'. A differing leading component is flagged as a major jump.
classify :: String -> Maybe String -> Status
classify current newest = case newest of
  Nothing -> Unknown
  Just nv -> case (parseVersion current, parseVersion nv) of
    (Just c, Just n)
      | n > c     -> Behind (major c n)
      | otherwise -> UpToDate
    _ -> Unknown
  where
    major (c : _) (n : _) = c /= n
    major _ _ = False

-- | What a dependency needs queried: its name, current pin label, and source.
data Target = Target
  { tName    :: String
  , tCurrent :: String
  , tSource  :: TargetSource
  }

data TargetSource = GitRepo String | Vendored' -- repo (with optional #subdir) | Hackage

-- | @zinc outdated@: build the report. @allClosure@ widens from direct deps to
-- the whole locked closure. Reads the manifest + lock; queries upstream for the
-- newest version of each non-boot dep (the one network step, read-only).
runOutdated :: Bool -> FilePath -> IO (Either ZincError [OutdatedDep])
runOutdated allClosure wsDir = runResult $ do
  let wsFile = wsDir </> "zinc.toml"
  present <- liftIO (doesFileExist wsFile)
  if not present then failWithError (NoZincToml wsDir) else pure ()
  ws <- liftEither . parseWorkspace =<< liftIO (readFile wsFile)
  locks <- liftIO (loadLocks wsDir)
  let byName = [(lockName l, l) | l <- locks]
      targets
        | allClosure = [closureTarget l | l <- locks, not (isBootLib (lockName l))]
        | otherwise  = [directTarget byName d | d <- wsDependencies ws, not (isBootLib (depName d))]
  liftIO (mapM resolveTarget (sortOn tName targets))
  where
    loadLocks dir = do
      let lf = dir </> "zinc.lock"
      there <- doesFileExist lf
      if not there then pure [] else either (const []) id . parseLock <$> readFile lf

    -- A direct dep: source from its declared repo, else the lock's resolved repo.
    directTarget byName d =
      let ml = lookup (depName d) byName
       in case depRef d of
            Vendored v -> Target (depName d) v Vendored'
            Tag t      -> Target (depName d) t (GitRepo (repoFor d ml))
            Rev r      -> Target (depName d) (short r) (GitRepo (repoFor d ml))
            Branch b   -> Target (depName d) b (GitRepo (repoFor d ml))
            Latest     -> Target (depName d) (maybe "*" (short . lockRevOf) ml) (GitRepo (repoFor d ml))

    repoFor d ml = case depRepo d of
      Just r  -> r
      Nothing -> maybe "" lockRepo ml

    -- A closure (lock) entry: vendored -> Hackage; otherwise the git repo at its rev.
    closureTarget l = case lockSource l of
      TarballSource v   -> Target (lockName l) v Vendored'
      GitSource repo rev -> Target (lockName l) (short rev) (GitRepo repo)

    lockRevOf l = case lockSource l of GitSource _ r -> r; TarballSource v -> v

    resolveTarget t = do
      newest <- newestFor t
      pure (OutdatedDep (tName t) (tCurrent t) newest (classify (tCurrent t) newest))

    newestFor t = case tSource t of
      Vendored'      -> hackageLatestVersion (tName t)
      GitRepo ""     -> pure Nothing
      GitRepo repo   -> do
        let (base, msub) = splitRepoSubdir repo
        tags <- listTags base
        pure $ case tags of
          Left _   -> Nothing
          Right ts -> newestTagFor (Just (tName t)) (isJust msub) ts

    short r = if length r > 7 && all (`elem` ("0123456789abcdef" :: String)) r then take 7 r else r

-- | The report as JSON.
outdatedJson :: [OutdatedDep] -> Json
outdatedJson deps = JObject [("dependencies", JArray (map one deps))]
  where
    one d =
      object
        [ ("name", Just (JString (odName d)))
        , ("current", Just (JString (odCurrent d)))
        , ("newest", JString <$> odNewest d)
        , ("status", Just (JString (statusText (odStatus d))))
        , ("major", Just (JBool (isMajor (odStatus d))))
        ]
    isMajor (Behind m) = m
    isMajor _            = False

statusText :: Status -> String
statusText UpToDate        = "up-to-date"
statusText (Behind True) = "outdated-major"
statusText (Behind False) = "outdated"
statusText Unknown         = "unknown"

-- | A compact human table.
renderOutdated :: [OutdatedDep] -> String
renderOutdated [] = "All dependencies are up to date.\n"
renderOutdated deps =
  unlines (header : map row deps ++ ["", summary])
  where
    nameW = maximum (4 : map (length . odName) deps)
    curW  = maximum (7 : map (length . odCurrent) deps)
    pad w s = s ++ replicate (max 0 (w - length s)) ' '
    header = pad nameW "name" ++ "  " ++ pad curW "current" ++ "  newest"
    row d =
      pad nameW (odName d)
        ++ "  "
        ++ pad curW (odCurrent d)
        ++ "  "
        ++ maybe "?" id (odNewest d)
        ++ marker (odStatus d)
    marker (Behind True)  = "  (major)"
    marker (Behind False) = "  (outdated)"
    marker _                = ""
    nOut = length [() | d <- deps, isOut (odStatus d)]
    isOut (Behind _) = True
    isOut _            = False
    summary =
      show nOut ++ " of " ++ show (length deps) ++ " dependenc"
        ++ (if length deps == 1 then "y" else "ies") ++ " outdated."
