-- | Minimal semver-aware tag selection, used to resolve a @"*"@ (Latest)
-- dependency to the newest release tag (spec §2).
module Zinc.Version
  ( newestTag
  , newestTagFor
  ) where

import Control.Applicative ((<|>))
import Data.List (maximumBy, stripPrefix)
import Data.Ord (comparing)
import Text.Read (readMaybe)

-- | Pick the highest version tag from a list, comparing numerically (so
-- @v1.10.0 > v1.2.0@). Tags that don't parse as a dotted version (optionally
-- @v@-prefixed) are ignored. 'Nothing' if none parse.
newestTag :: [String] -> Maybe String
newestTag = newestTagFor Nothing

-- | As 'newestTag', but monorepo-aware: when a package name is given (the
-- subdir of a @repo#subdir@ dependency), PREFER tags scoped to that package —
-- @\<pkg\>-1.2.3@, @\<pkg\>\/1.2.3@, or @\<pkg\>_1.2.3@ — over bare version
-- tags. A monorepo like haskell/vector carries both per-package tags
-- (@vector-stream-0.1.0.1@) and stale global ones (@v0.12.3.1@); picking the
-- latter checks out a commit where the package's subdir does not yet exist.
-- Falls back to bare version tags only when no package-scoped tag is present.
newestTagFor :: Maybe String -> [String] -> Maybe String
newestTagFor mpkg tags =
  case scoped of
    (_ : _) -> Just (pick scoped)
    []      -> case bare of
      []        -> Nothing
      (_ : _)   -> Just (pick bare)
  where
    pick = snd . maximumBy (comparing fst)
    bare = [(v, t) | t <- tags, Just v <- [parseVersion t]]
    scoped = case mpkg of
      Nothing  -> []
      Just pkg -> [(v, t) | t <- tags, Just rest <- [stripScope pkg t], Just v <- [parseVersion rest]]
    -- The version part after a "<pkg>" + separator scope prefix.
    stripScope pkg t =
      stripPrefix (pkg ++ "-") t <|> stripPrefix (pkg ++ "/") t <|> stripPrefix (pkg ++ "_") t

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
