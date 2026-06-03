module Zinc.Manifest
  ( WorkspaceManifest (..)
  , MemberManifest (..)
  , Component (..)
  , ComponentKind (..)
  , Dependency (..)
  , Ref (..)
  , parseWorkspace
  , parseMember
  , parseDependencies
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Toml
import Toml.Value (Value (..))
import Zinc.TOML (optStringArray, stringArrayField, stringField, subTable, tableField)

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

-- | A buildable component within a member package (spec §4).
data ComponentKind = Library | Executable | TestSuite
  deriving (Eq, Show)

-- | One @[build.*]@ component. Absent fields default to empty.
data Component = Component
  { compKind           :: ComponentKind
  , compName           :: String       -- ^ @"lib"@, or the exe/test name
  , compSourceDirs     :: [String]
  , compExposedModules :: [String]     -- ^ library only
  , compOtherModules   :: [String]
  , compMain           :: Maybe String -- ^ executable/test entrypoint
  , compExtensions     :: [String]
  , compGhcOptions     :: [String]
  , compDepends        :: [String]
  , compSystemLibs     :: [String]     -- ^ nixpkgs attr names
  }
  deriving (Eq, Show)

-- | A member package's @[package]@ identity plus its @[build.*]@ components.
data MemberManifest = MemberManifest
  { pkgName       :: String
  , pkgVersion    :: String
  , pkgComponents :: [Component]
  }
  deriving (Eq, Show)

-- | Parse a member manifest (identity + build components) from TOML source.
parseMember :: String -> Either String MemberManifest
parseMember src = do
  top     <- Toml.parse src
  pkg     <- tableField "package" top
  name    <- stringField "name" pkg
  version <- stringField "version" pkg
  pure
    MemberManifest
      { pkgName = name
      , pkgVersion = version
      , pkgComponents = parseBuild top
      }

-- | Parse the @[build]@ table into components: one @lib@, plus each named
-- @exe.\<name\>@ and @test.\<name\>@.
parseBuild :: Map String Value -> [Component]
parseBuild top = lib ++ named Executable "exe" ++ named TestSuite "test"
  where
    build = subTable "build" top
    lib = case Map.lookup "lib" build of
      Just (Table t) -> [mkComponent Library "lib" t]
      _              -> []
    named kind key =
      [mkComponent kind name t | (name, Table t) <- Map.toList (subTable key build)]

mkComponent :: ComponentKind -> String -> Map String Value -> Component
mkComponent kind name t =
  Component
    { compKind = kind
    , compName = name
    , compSourceDirs = strs "source-dirs"
    , compExposedModules = strs "exposed-modules"
    , compOtherModules = strs "other-modules"
    , compMain = str "main"
    , compExtensions = strs "extensions"
    , compGhcOptions = strs "ghc-options"
    , compDepends = strs "depends"
    , compSystemLibs = strs "system-libs"
    }
  where
    strs k = either (const []) id (optStringArray k t)
    str k = case Map.lookup k t of
      Just (String s) -> Just s
      _               -> Nothing

-- | Parse a workspace-root manifest from TOML source.
parseWorkspace :: String -> Either String WorkspaceManifest
parseWorkspace src = do
  top      <- Toml.parse src
  wsTbl    <- tableField "workspace" top
  members  <- stringArrayField "members" wsTbl
  ghc      <- stringField "ghc" wsTbl
  let (deps, reg) = depsAndRegistry top
  pure
    WorkspaceManifest
      { wsMembers      = members
      , wsGhc          = ghc
      , wsDependencies = deps
      , wsRegistry     = reg
      }

-- | Read just @[dependencies]@ + @[registry]@ from any package manifest
-- (no @[workspace]@ required). This is what the resolver reads from each
-- fetched dependency to discover the self-describing graph (spec §2).
parseDependencies :: String -> Either String ([Dependency], [(String, String)])
parseDependencies src = depsAndRegistry <$> Toml.parse src

depsAndRegistry :: Map String Value -> ([Dependency], [(String, String)])
depsAndRegistry top =
  (parseDeps (subTable "dependencies" top), parseRegistry (subTable "registry" top))

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
