-- | The closure delta (zinc-90j.2): the before->after diff over a resolved
-- closure that @zinc update@ (and @update --dry-run@) must always show
-- (resolution-transparency, design s2). Because it diffs the WHOLE locked
-- closure — not just the directly-bumped deps — it captures one-ref-per-name
-- ripples: bumping one dependency can shift a shared transitive that other
-- packages also build against, and that shift shows up here as a change.
module Zinc.Delta
  ( Change (..)
  , ClosureDelta (..)
  , closureDelta
  , isEmptyDelta
  , deltaJson
  , renderDelta
  ) where

import Zinc.Json (Json (..))
import Zinc.Lock (LockedPackage (..), lockRev)
import Zinc.Version (parseVersion)

-- | A package present before and after whose pinned rev/version changed.
data Change = Change
  { chName  :: String
  , chOld   :: String  -- ^ previous rev (git) or version (vendored)
  , chNew   :: String
  , chMajor :: Bool    -- ^ both parse as versions and the leading component differs
  }
  deriving (Eq, Show)

-- | The closure diff: changed pins, plus packages added to / removed from the
-- closure. (Added/removed carried as @(name, rev\/version)@.)
data ClosureDelta = ClosureDelta
  { cdChanged :: [Change]
  , cdAdded   :: [(String, String)]
  , cdRemoved :: [(String, String)]
  }
  deriving (Eq, Show)

-- | Diff two lockfiles (old -> new) by package name, comparing each package's
-- rev/version ('lockRev', which is the commit for a git source or the version
-- for a vendored one).
closureDelta :: [LockedPackage] -> [LockedPackage] -> ClosureDelta
closureDelta old new =
  ClosureDelta
    { cdChanged =
        [ Change n ov nv (major ov nv)
        | (n, ov) <- oldVer
        , Just nv <- [lookup n newVer]
        , ov /= nv
        ]
    , cdAdded   = [(n, v) | (n, v) <- newVer, n `notElem` map fst oldVer]
    , cdRemoved = [(n, v) | (n, v) <- oldVer, n `notElem` map fst newVer]
    }
  where
    oldVer = [(lockName l, lockRev l) | l <- old]
    newVer = [(lockName l, lockRev l) | l <- new]
    major a b = case (parseVersion a, parseVersion b) of
      (Just (x : _), Just (y : _)) -> x /= y
      _                            -> False

-- | True when nothing changed, was added, or was removed.
isEmptyDelta :: ClosureDelta -> Bool
isEmptyDelta d = null (cdChanged d) && null (cdAdded d) && null (cdRemoved d)

-- | The delta as JSON.
deltaJson :: ClosureDelta -> Json
deltaJson d =
  JObject
    [ ("changed", JArray [changeJson c | c <- cdChanged d])
    , ("added", JArray [pairJson n v | (n, v) <- cdAdded d])
    , ("removed", JArray [pairJson n v | (n, v) <- cdRemoved d])
    ]
  where
    changeJson c =
      JObject
        [ ("name", JString (chName c))
        , ("old", JString (chOld c))
        , ("new", JString (chNew c))
        , ("major", JBool (chMajor c))
        ]
    pairJson n v = JObject [("name", JString n), ("rev", JString v)]

-- | A compact human rendering. @dryRun@ only changes the framing line.
renderDelta :: Bool -> ClosureDelta -> String
renderDelta dryRun d
  | isEmptyDelta d = (if dryRun then "Dry run: " else "") ++ "closure unchanged.\n"
  | otherwise =
      unlines $
        [headline]
          ++ ["  ~ " ++ chName c ++ " " ++ short (chOld c) ++ " -> " ++ short (chNew c) ++ (if chMajor c then "  (major)" else "") | c <- cdChanged d]
          ++ ["  + " ++ n ++ " " ++ short v | (n, v) <- cdAdded d]
          ++ ["  - " ++ n ++ " " ++ short v | (n, v) <- cdRemoved d]
  where
    headline =
      (if dryRun then "Dry run — closure delta " else "Closure delta ")
        ++ "(" ++ counts ++ "):"
    counts =
      show (length (cdChanged d)) ++ " changed, "
        ++ show (length (cdAdded d)) ++ " added, "
        ++ show (length (cdRemoved d)) ++ " removed"
    -- Shorten a git commit for display; leave tags/versions intact.
    short r = if length r > 7 && all (`elem` ("0123456789abcdef" :: String)) r then take 7 r else r
