-- | Build orchestration (spec §7): the @zinc build@ command. Builds each
-- workspace member, ordering them so a member's sibling-library dependencies
-- are built and registered (into a workspace package db) before it.
--
-- Note: compiling the git /dependency closure/ from source is a separate,
-- larger concern (tracked as a follow-up); this builds the workspace members
-- and their sibling links.
module Zinc.Orchestrate
  ( runBuild
  , orderMembers
  ) where

import qualified Data.Map as Map
import System.FilePath ((</>))
import Zinc.Build (LibBuild (..), MemberBuild (..), buildLib, buildMember, initPackageDb)
import Zinc.Manifest
  ( Component (compDepends, compKind)
  , ComponentKind (Executable, Library)
  , MemberManifest (pkgComponents, pkgName, pkgVersion)
  , WorkspaceManifest (wsMembers)
  , parseMember
  , parseWorkspace
  )

-- | Build every member's executables in a workspace, building sibling
-- libraries first. Returns the built executable paths, or the first error.
runBuild :: FilePath -> IO (Either String [FilePath])
runBuild wsDir = do
  wsSrc <- readFile (wsDir </> "zinc.toml")
  case parseWorkspace wsSrc of
    Left err -> pure (Left err)
    Right ws -> do
      loaded <- loadMembers [] (wsMembers ws)
      case loaded of
        Left err -> pure (Left err)
        Right members -> do
          let wsDb = wsDir </> ".zinc" </> "pkgdb"
          ready <- initPackageDb wsDb
          case ready of
            Left err -> pure (Left err)
            Right () -> buildAll wsDb [] (orderMembers members)
  where
    loadMembers acc [] = pure (Right (reverse acc))
    loadMembers acc (member : rest) = do
      let dir = wsDir </> member
      src <- readFile (dir </> "zinc.toml")
      case parseMember src of
        Left err -> pure (Left (member ++ ": " ++ err))
        Right mem -> loadMembers ((dir, mem) : acc) rest

    buildAll _ acc [] = pure (Right acc)
    buildAll wsDb acc ((dir, mem) : rest) = do
      libResult <- buildMemberLib wsDb dir mem
      case libResult of
        Left err -> pure (Left err)
        Right () -> do
          exeResult <- buildExes wsDb dir [] (executables mem)
          case exeResult of
            Left err    -> pure (Left err)
            Right paths -> buildAll wsDb (acc ++ paths) rest

    buildMemberLib wsDb dir mem =
      case filter ((== Library) . compKind) (pkgComponents mem) of
        []        -> pure (Right ())
        (lib : _) -> buildLib (LibBuild dir (dir </> ".zinc" </> "lib") wsDb (pkgName mem) (pkgVersion mem) lib)

    executables mem = filter ((== Executable) . compKind) (pkgComponents mem)

    buildExes _ _ acc [] = pure (Right (reverse acc))
    buildExes wsDb dir acc (comp : rest) = do
      result <- buildMember (MemberBuild dir (dir </> ".zinc" </> "build") (Just wsDb) comp)
      case result of
        Left err  -> pure (Left err)
        Right exe -> buildExes wsDb dir (exe : acc) rest

-- | Topologically order members so a member is preceded by the sibling
-- members it depends on (so their libraries are registered first).
orderMembers :: [(FilePath, MemberManifest)] -> [(FilePath, MemberManifest)]
orderMembers members = map (byName Map.!) (reverse ordered)
  where
    names = map (pkgName . snd) members
    byName = Map.fromList [(pkgName m, e) | e@(_, m) <- members]
    depsOf name =
      maybe
        []
        (filter (`elem` names) . concatMap compDepends . pkgComponents . snd)
        (Map.lookup name byName)
    (_, ordered) = foldl visit ([], []) names
    visit (visited, order) name
      | name `elem` visited = (visited, order)
      | otherwise =
          let (visited', order') = foldl visit (name : visited, order) (depsOf name)
           in (visited', name : order')
