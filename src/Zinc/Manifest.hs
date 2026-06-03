module Zinc.Manifest
  ( WorkspaceManifest (..)
  , MemberManifest (..)
  , Dependency (..)
  , Ref (..)
  , parseWorkspace
  , parseMember
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Toml
import Toml.Value (Value (..))
import Zinc.TOML (stringArrayField, stringField, subTable, tableField)

-- | How a dependency is pinned. There is exactly one ref per package name
-- across a workspace, and bounds are ignored (spec §2).
data Ref
  = Tag String     -- ^ @{ tag = "v2.2.3.0" }@
  | Branch String  -- ^ @{ branch = "main" }@
  | Rev String     -- ^ @{ rev = "a1b2c3d" }@
  | Latest         -- ^ @"*"@ — latest release tag, resolved at @add@ time
  deriving (Eq, Show)

-- | A direct dependency: a package name plus how it is pinned.
data Dependency = Dependency
  { depName :: String
  , depRef  :: Ref
  }
  deriving (Eq, Show)

-- | The workspace-root @zinc.toml@ (spec §4). Member @[build.*]@ parsing is a
-- separate concern (task zinc-th0.3); this covers what the resolver needs.
data WorkspaceManifest = WorkspaceManifest
  { wsMembers      :: [FilePath]
  , wsGhc          :: String
  , wsDependencies :: [Dependency]
  , wsRegistry     :: [(String, String)] -- ^ package name → git repo URL
  }
  deriving (Eq, Show)

-- | A member package's @[package]@ identity. The @[build.*]@ component model
-- is parsed separately (task zinc-th0.3).
data MemberManifest = MemberManifest
  { pkgName    :: String
  , pkgVersion :: String
  }
  deriving (Eq, Show)

-- | Parse a member manifest's @[package]@ identity from TOML source.
parseMember :: String -> Either String MemberManifest
parseMember src = do
  top     <- Toml.parse src
  pkg     <- tableField "package" top
  name    <- stringField "name" pkg
  version <- stringField "version" pkg
  pure MemberManifest {pkgName = name, pkgVersion = version}

-- | Parse a workspace-root manifest from TOML source.
parseWorkspace :: String -> Either String WorkspaceManifest
parseWorkspace src = do
  top      <- Toml.parse src
  wsTbl    <- tableField "workspace" top
  members  <- stringArrayField "members" wsTbl
  ghc      <- stringField "ghc" wsTbl
  pure
    WorkspaceManifest
      { wsMembers      = members
      , wsGhc          = ghc
      , wsDependencies = parseDeps (subTable "dependencies" top)
      , wsRegistry     = parseRegistry (subTable "registry" top)
      }

parseDeps :: Map String Value -> [Dependency]
parseDeps = map (\(name, val) -> Dependency name (refOf val)) . Map.toList
  where
    refOf (Table t)
      | Just (String s) <- Map.lookup "tag" t    = Tag s
      | Just (String s) <- Map.lookup "branch" t = Branch s
      | Just (String s) <- Map.lookup "rev" t    = Rev s
    refOf _ = Latest -- bare "*" (or anything unrecognised) → latest

parseRegistry :: Map String Value -> [(String, String)]
parseRegistry = foldr keep [] . Map.toList
  where
    keep (name, String url) acc = (name, url) : acc
    keep _                  acc = acc
