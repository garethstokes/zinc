module Zinc.Manifest
  ( WorkspaceManifest (..)
  , MemberManifest (..)
  , Component (..)
  , ComponentKind (..)
  , Dependency (..)
  , Ref (..)
  , isVendored
  , depRepos
  , depGhcOptionsOf
  , parseWorkspace
  , parseMember
  , parseDependencies
  , renderWorkspace
  , renderDependencies
  , addDep
  ) where

import Data.List (intercalate, sortOn)
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
  | Vendored String -- ^ @{ vendored = "2.3.6" }@ — a pinned Hackage tarball, not a git ref (b1z)
  deriving (Eq, Show)

-- | Whether a pin is a vendored Hackage tarball (no git repo) rather than a git
-- ref — the one place the resolver/freeze/render branch on source kind.
isVendored :: Ref -> Bool
isVendored (Vendored _) = True
isVendored _            = False

-- | A direct dependency, vertically: a name, how it is pinned, and (optionally)
-- its repo override and extra ghc flags — all of a dependency's facets in one
-- place (spec: vertical config). @depRepo@ is 'Nothing' when the repo is left to
-- @add@-time discovery + the lock; it is the explicit override/pin otherwise.
data Dependency = Dependency
  { depName       :: String
  , depRef        :: Ref
  , depRepo       :: Maybe String -- ^ optional repo override/pin (was @[registry]@)
  , depGhcOptions :: [String]     -- ^ optional extra ghc flags (was @[build-options]@)
  }
  deriving (Eq, Show)

-- | The workspace-root @zinc.toml@ (spec §4). Member @[build.*]@ parsing is a
-- separate concern (task zinc-th0.3); this covers what the resolver needs.
data WorkspaceManifest = WorkspaceManifest
  { wsMembers      :: [FilePath]
  , wsGhc          :: String
  , wsDependencies :: [Dependency]
  }
  deriving (Eq, Show)

-- | The repos a workspace pins, keyed by dependency name — the resolver's view,
-- derived from each dependency's @repo@ (replaces the old @[registry]@ table).
depRepos :: WorkspaceManifest -> [(String, String)]
depRepos ws = [(depName d, r) | d <- wsDependencies ws, Just r <- [depRepo d]]

-- | Per-dependency extra ghc flags, keyed by name (derived from each
-- dependency's @ghc-options@; replaces the old @[build-options]@ table).
depGhcOptionsOf :: WorkspaceManifest -> [(String, [String])]
depGhcOptionsOf ws = [(depName d, depGhcOptions d) | d <- wsDependencies ws, not (null (depGhcOptions d))]

-- | A buildable component within a member package (spec §4).
data ComponentKind = Library | Executable | TestSuite
  deriving (Eq, Show)

-- | One @[build.*]@ component. Absent fields default to empty.
data Component = Component
  { compKind           :: ComponentKind
  , compName           :: String       -- ^ @"lib"@, or the exe/test name
  , compSourceDirs     :: [String]
  , compModules        :: [String]     -- ^ explicit modules; empty = auto-discover from source-dirs (spec §4, no module hiding)
  , compMain           :: Maybe String -- ^ executable/test entrypoint
  , compExtensions     :: [String]
  , compGhcOptions     :: [String]
  , compDepends        :: [String]
  , compSystemLibs     :: [String]     -- ^ nixpkgs attr names
  , compIncludeDirs    :: [String]     -- ^ C-header search dirs for CPP, relative to the package
  , compCppOptions     :: [String]     -- ^ CPP @-D@ defines (cabal cpp-options), passed via @-optP@
  , compCSources       :: [String]     -- ^ C sources to compile + archive (cabal c-sources), relative to the package
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
    , compModules = strs "modules"
    , compMain = str "main"
    , compExtensions = strs "extensions"
    , compGhcOptions = strs "ghc-options"
    , compDepends = strs "depends"
    , compSystemLibs = strs "system-libs"
    , compIncludeDirs = strs "include-dirs"
    , compCppOptions = strs "cpp-options"
    , compCSources = strs "c-sources"
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
  pure
    WorkspaceManifest
      { wsMembers      = members
      , wsGhc          = ghc
      , wsDependencies = parseDeps (subTable "dependencies" top)
      }

-- | Read just @[dependencies]@ from any package manifest (no @[workspace]@
-- required) — what the resolver reads from each fetched dependency to discover
-- the self-describing graph. Returns the deps plus their repos (derived from
-- each dependency's @repo@) for the resolver's registry view.
parseDependencies :: String -> Either String ([Dependency], [(String, String)])
parseDependencies src = (\deps -> (deps, [(depName d, r) | d <- deps, Just r <- [depRepo d]])) . parseDeps . subTable "dependencies" <$> Toml.parse src

-- | Parse the @[dependencies]@ table: each entry is either a bare-string
-- shorthand (@name = "v1.2.3"@ → a tag; @"*"@ → latest) or a sub-table
-- (@[dependencies.name]@) with one of @tag@/@branch@/@rev@, an optional @repo@
-- override, and optional @ghc-options@.
parseDeps :: Map String Value -> [Dependency]
parseDeps = map dep . Map.toList
  where
    dep (name, Table t) = Dependency name (refOf t) (strOf "repo" t) (arrOf "ghc-options" t)
    dep (name, String "*") = Dependency name Latest Nothing []
    dep (name, String s) = Dependency name (Tag s) Nothing []
    dep (name, _) = Dependency name Latest Nothing []

    refOf t
      | Just (String s) <- Map.lookup "vendored" t = Vendored s -- a pinned Hackage tarball
      | Just (String "*") <- Map.lookup "tag" t  = Latest -- canonical sub-table form of Latest
      | Just (String s) <- Map.lookup "tag" t    = Tag s
      | Just (String s) <- Map.lookup "branch" t = Branch s
      | Just (String s) <- Map.lookup "rev" t    = Rev s
      | otherwise                                = Latest
    strOf k t = case Map.lookup k t of Just (String s) -> Just s; _ -> Nothing
    arrOf k t = case Map.lookup k t of Just (Array xs) -> [s | String s <- xs]; _ -> []

-- | The canonical workspace-manifest writer (spec §3): @[workspace]@ then
-- @[dependencies]@, deps sorted by name, each rendered as a one-line shorthand
-- when it has only a ref, or a @[dependencies.name]@ sub-table (stable key
-- order: ref, repo, ghc-options) when it has a repo or ghc flags. Idempotent;
-- shared by @zinc add@/@update@ and @zinc fmt@. Round-trips with 'parseWorkspace'.
renderWorkspace :: WorkspaceManifest -> String
renderWorkspace w =
  unlines $
    [ "[workspace]"
    , "members = [" ++ intercalate ", " (map quote (wsMembers w)) ++ "]"
    , "ghc = " ++ quote (wsGhc w)
    , ""
    ]
      ++ renderDependencies (wsDependencies w)
  where
    quote s = "\"" ++ s ++ "\""

-- | Render the canonical @[dependencies]@ block: deps sorted by name, each as a
-- one-line shorthand (ref only) or a @[dependencies.name]@ sub-table (ref, then
-- repo, then ghc-options). Shared by 'renderWorkspace' and @zinc fmt@.
renderDependencies :: [Dependency] -> [String]
renderDependencies deps = "[dependencies]" : concatMap renderDep (sortOn depName deps)
  where
    quote s = "\"" ++ s ++ "\""
    refStr (Tag t)      = ("tag", t)
    refStr (Branch b)   = ("branch", b)
    refStr (Rev v)      = ("rev", v)
    refStr Latest       = ("tag", "*")
    refStr (Vendored v) = ("vendored", v)
    -- A dep with no repo override and no ghc flags renders as one-line shorthand
    -- (the bare ref string); otherwise a [dependencies.name] sub-table. A
    -- vendored pin always uses the sub-table form: its bare value would parse
    -- back as a git tag, losing the source kind.
    renderDep d
      | Nothing <- depRepo d, null (depGhcOptions d), not (isVendored (depRef d)) =
          [depName d ++ " = " ++ quote (snd (refStr (depRef d)))]
      | otherwise =
          let (k, v) = refStr (depRef d)
           in [ ""
              , "[dependencies." ++ depName d ++ "]"
              , k ++ " = " ++ quote v
              ]
                ++ maybe [] (\r -> ["repo = " ++ quote r]) (depRepo d)
                ++ [ "ghc-options = [" ++ intercalate ", " (map quote (depGhcOptions d)) ++ "]"
                   | not (null (depGhcOptions d))
                   ]

-- | Add (or replace) a direct dependency with its repo override, keeping the
-- list sorted by name (the canonical writer re-sorts anyway).
addDep :: WorkspaceManifest -> String -> Ref -> String -> WorkspaceManifest
addDep w name ref repo =
  w
    { wsDependencies =
        sortOn depName (Dependency name ref (Just repo) [] : filter ((/= name) . depName) (wsDependencies w))
    }
