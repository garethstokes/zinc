-- | Minimal semver-aware tag selection, used to resolve a @"*"@ (Latest)
-- dependency to the newest release tag (spec §2).
module Zinc.Version
  ( newestTag
  , newestTagFor
  , parseVersion
  , gitVersion
  ) where

import Control.Applicative ((<|>))
import Data.List (maximumBy, stripPrefix)
import Data.Ord (comparing)
import Text.Read (readMaybe)

-- | Pick the highest version tag from a list, comparing numerically (so
-- @v1.10.0 > v1.2.0@). Tags that don't parse as a dotted version (optionally
-- @v@-prefixed) are ignored. 'Nothing' if none parse.
newestTag :: [String] -> Maybe String
newestTag = newestTagFor Nothing False

-- | As 'newestTag', but package- and monorepo-aware. A package name (the subdir
-- of a @repo#subdir@ dependency, or the dep name) lets tags scoped to that
-- package — @\<pkg\>-1.2.3@, @\<pkg\>\/1.2.3@, @\<pkg\>_1.2.3@ — be recognised.
--
-- @isSubdir@ says whether the dependency is a subdir of a multi-package monorepo
-- (@repo#subdir@). The two cases differ in what a /bare/ version tag (@v1.2.3@)
-- means:
--
--   * Subdir package: bare/global tags belong to a SIBLING or a pre-split repo
--     state (haskell/vector carries @vector-stream-0.1.0.1@ alongside stale
--     global @v0.12.3.1@); picking one checks out a commit where the subdir may
--     not exist. So prefer scoped tags, falling back to bare only when the
--     package has no scoped tag at all (zinc-ffm.3).
--   * Standalone repo (no subdir): the package IS the whole repo, so its
--     releases may be tagged EITHER scoped (@strict-1.5@) OR bare (@v1.5.1.0@,
--     e.g. hashable, which also carries one stale @hashable-1.3.2.0@). Consider
--     both and take the newest — never let a stale scoped tag mask newer bare
--     releases (zinc-myx).
newestTagFor :: Maybe String -> Bool -> [String] -> Maybe String
newestTagFor mpkg isSubdir tags = snd <$> newestPairFor mpkg isSubdir tags

-- | The (version, tag) the selection picks.
newestPairFor :: Maybe String -> Bool -> [String] -> Maybe ([Int], String)
newestPairFor mpkg isSubdir tags
  | isSubdir  = maybePick (if null scoped then bare else scoped)
  | otherwise = maybePick (scoped ++ bare)
  where
    maybePick [] = Nothing
    maybePick xs = Just (maximumBy (comparing fst) xs)
    bare = [(v, t) | t <- tags, Just v <- [parseVersion t]]
    scoped = case mpkg of
      Nothing  -> []
      Just pkg -> [(v, t) | t <- tags, Just rest <- [stripScope pkg t], Just v <- [parseVersion rest]]
    -- The version part after a "<pkg>" + separator scope prefix.
    stripScope pkg t =
      stripPrefix (pkg ++ "-") t <|> stripPrefix (pkg ++ "/") t <|> stripPrefix (pkg ++ "_") t

-- | A git-derived full version string (zinc-b3z): the base version, plus
-- @+\<commitCount\>.g\<shortHash\>@ when a commit hash is known, plus @.dirty@
-- when the working tree has uncommitted changes. With no hash (a non-git build)
-- it is just the base — the deterministic fallback. Pure, so the format is
-- testable without a build.
gitVersion :: String -> String -> Int -> Bool -> String
gitVersion base hash count dirty
  | null hash = base
  | otherwise = base ++ "+" ++ show count ++ ".g" ++ take 7 hash ++ (if dirty then ".dirty" else "")

parseVersion :: String -> Maybe [Int]
parseVersion raw =
  let parts = splitOn '.' (dropWhile (== 'v') raw)
   in if not (null parts) && all (not . null) parts
        then traverse readMaybe parts
        else Nothing

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, [])       -> [a]
  (a, _ : rest) -> a : splitOn c rest
