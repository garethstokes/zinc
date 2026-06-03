-- | Minimal semver-aware tag selection, used to resolve a @"*"@ (Latest)
-- dependency to the newest release tag (spec §2).
module Zinc.Version
  ( newestTag
  ) where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Text.Read (readMaybe)

-- | Pick the highest version tag from a list, comparing numerically (so
-- @v1.10.0 > v1.2.0@). Tags that don't parse as a dotted version (optionally
-- @v@-prefixed) are ignored. 'Nothing' if none parse.
newestTag :: [String] -> Maybe String
newestTag tags =
  case [(v, t) | t <- tags, Just v <- [parseVersion t]] of
    []  -> Nothing
    vts -> Just (snd (maximumBy (comparing fst) vts))

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
