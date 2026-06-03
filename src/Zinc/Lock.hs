-- | The workspace lockfile @zinc.lock@ (spec §5): every package in the
-- resolved closure pinned to an exact commit + content hash.
module Zinc.Lock
  ( LockedPackage (..)
  , parseLock
  , renderLock
  ) where

import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Toml
import Toml.Value (Value (..))
import Zinc.TOML (optStringArray, stringField)

-- | One @[[locked]]@ entry.
data LockedPackage = LockedPackage
  { lockName    :: String
  , lockRepo    :: String
  , lockRev     :: String   -- ^ resolved commit (not the tag/branch)
  , lockSha256  :: String   -- ^ content hash; validates the fetch
  , lockDepends :: [String] -- ^ flattened dep names, for fast graph load
  }
  deriving (Eq, Show)

-- | Parse a lockfile. An absent @[[locked]]@ array means no packages.
parseLock :: String -> Either String [LockedPackage]
parseLock src = do
  top <- Toml.parse src
  case Map.lookup "locked" top of
    Nothing         -> Right []
    Just (Array xs) -> mapM toLocked xs
    Just _          -> Left "expected an array of [[locked]] tables"
  where
    toLocked (Table t) =
      LockedPackage
        <$> stringField "name" t
        <*> stringField "repo" t
        <*> stringField "rev" t
        <*> stringField "sha256" t
        <*> optStringArray "depends" t
    toLocked _ = Left "expected a table in the [[locked]] array"

-- | Render locked packages back to TOML. Round-trips with 'parseLock'.
-- Minimal emitter: our values (names, URLs, hex revs, hashes) contain no
-- characters needing TOML escaping.
renderLock :: [LockedPackage] -> String
renderLock = intercalate "\n" . map renderOne
  where
    renderOne p =
      unlines
        [ "[[locked]]"
        , "name = " ++ str (lockName p)
        , "repo = " ++ str (lockRepo p)
        , "rev = " ++ str (lockRev p)
        , "sha256 = " ++ str (lockSha256 p)
        , "depends = [" ++ intercalate ", " (map str (lockDepends p)) ++ "]"
        ]
    str s = "\"" ++ s ++ "\""
