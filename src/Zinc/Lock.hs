-- | The workspace lockfile @zinc.lock@ (spec §5): every package in the
-- resolved closure pinned to an exact commit + content hash.
module Zinc.Lock
  ( LockedPackage (..)
  , Source (..)
  , lockRepo
  , lockRev
  , srcKey
  , parseLock
  , renderLock
  ) where

import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Toml
import Toml.Value (Value (..))
import Zinc.TOML (optStringArray, stringField)

-- | Where a locked package's source comes from (b1z, design s2): a git repo at
-- an exact commit, or a Hackage sdist tarball pinned by version. Both are
-- content-addressed by 'lockSha256'; the source kind only decides /how/ the
-- pinned bytes are fetched (git clone vs. tarball download). A new source kind
-- cannot be silently mis-fetched: every fetch site pattern-matches this.
data Source
  = GitSource String String -- ^ repo, resolved commit
  | TarballSource String    -- ^ version (Hackage sdist; the name is 'lockName')
  deriving (Eq, Show)

-- | One @[[locked]]@ entry.
data LockedPackage = LockedPackage
  { lockName    :: String
  , lockSource  :: Source   -- ^ git repo+rev OR vendored tarball version
  , lockSha256  :: String   -- ^ content hash; validates the fetch
  , lockDepends :: [String] -- ^ flattened dep names, for fast graph load
  }
  deriving (Eq, Show)

-- | The git repo of a locked package, or @""@ for a vendored tarball (which has
-- no repo). A display/identifier helper so cache-key, report, and store-path
-- code need not branch on the source kind — only the actual fetch does.
lockRepo :: LockedPackage -> String
lockRepo p = case lockSource p of
  GitSource repo _ -> repo
  TarballSource _  -> ""

-- | The store/cache identifier of a locked package: the resolved git commit, or
-- the vendored tarball's version. Stable per pinned source, so it keys the
-- content-addressed source dir and the build cache the same way for both kinds.
lockRev :: LockedPackage -> String
lockRev p = case lockSource p of
  GitSource _ rev   -> rev
  TarballSource ver -> ver

-- | What identifies a package's SOURCE checkout (zinc-qln): the git repo (so the
-- sub-packages of one monorepo share a single clone — keyed by repo, not name),
-- or the package name for a vendored tarball (each tarball is its own source).
-- The @#subdir@ is kept here and stripped when slugged into the store path, so
-- @effectful@ and @effectful-core@ (same repo) map to one checkout while a
-- tarball keeps its per-name checkout. Pairs with 'lockRev' to name the dir.
srcKey :: LockedPackage -> String
srcKey p = case lockSource p of
  GitSource repo _ -> repo
  TarballSource _  -> lockName p

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
        <*> sourceOf t
        <*> stringField "sha256" t
        <*> optStringArray "depends" t
    toLocked _ = Left "expected a table in the [[locked]] array"
    -- A @vendored@ key marks a Hackage tarball (no repo/rev); otherwise the
    -- entry is a git source with @repo@ + @rev@.
    sourceOf t = case Map.lookup "vendored" t of
      Just (String ver) -> Right (TarballSource ver)
      Just _            -> Left "expected a string for 'vendored'"
      Nothing           -> GitSource <$> stringField "repo" t <*> stringField "rev" t

-- | Render locked packages back to TOML. Round-trips with 'parseLock'.
-- Minimal emitter: our values (names, URLs, hex revs, hashes) contain no
-- characters needing TOML escaping.
renderLock :: [LockedPackage] -> String
renderLock = intercalate "\n" . map renderOne
  where
    renderOne p =
      unlines $
        ["[[locked]]", "name = " ++ str (lockName p)]
          ++ sourceLines (lockSource p)
          ++ [ "sha256 = " ++ str (lockSha256 p)
             , "depends = [" ++ intercalate ", " (map str (lockDepends p)) ++ "]"
             ]
    sourceLines (GitSource repo rev) = ["repo = " ++ str repo, "rev = " ++ str rev]
    sourceLines (TarballSource ver)  = ["vendored = " ++ str ver]
    str s = "\"" ++ s ++ "\""
