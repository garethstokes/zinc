-- | Build orchestration (spec §7, §10): the @zinc build@ / @run@ / @test@
-- commands. Builds each workspace member, ordering them so a member's
-- sibling-library dependencies are built and registered (into a workspace
-- package db) before it.
--
-- Note: compiling the git /dependency closure/ from source is a separate,
-- larger concern (tracked as a follow-up); this builds the workspace members
-- and their sibling links.
module Zinc.Orchestrate
  ( runBuild
  , buildAndRun
  , runTests
  , orderMembers
  ) where

import qualified Data.Map as Map
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcess, readProcessWithExitCode)
import Zinc.Build (LibBuild (..), MemberBuild (..), buildLib, buildMember, initPackageDb)
import Zinc.Manifest
  ( Component (compDepends, compKind)
  , ComponentKind (Executable, Library, TestSuite)
  , MemberManifest (pkgComponents, pkgName, pkgVersion)
  , WorkspaceManifest (wsMembers)
  , parseMember
  , parseWorkspace
  )

-- | Build a workspace: each member's library (so siblings can link) plus every
-- component whose kind satisfies @keep@, returned as built executable paths.
buildWorkspace :: FilePath -> (ComponentKind -> Bool) -> IO (Either String [FilePath])
buildWorkspace wsDir keep = do
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
          exeResult <- buildComps wsDb dir [] (wanted mem)
          case exeResult of
            Left err    -> pure (Left err)
            Right paths -> buildAll wsDb (acc ++ paths) rest

    buildMemberLib wsDb dir mem =
      case filter ((== Library) . compKind) (pkgComponents mem) of
        []        -> pure (Right ())
        (lib : _) -> buildLib (LibBuild dir (dir </> ".zinc" </> "lib") wsDb (pkgName mem) (pkgVersion mem) lib)

    wanted mem = filter (keep . compKind) (pkgComponents mem)

    buildComps _ _ acc [] = pure (Right (reverse acc))
    buildComps wsDb dir acc (comp : rest) = do
      result <- buildMember (MemberBuild dir (dir </> ".zinc" </> "build") (Just wsDb) comp)
      case result of
        Left err  -> pure (Left err)
        Right exe -> buildComps wsDb dir (exe : acc) rest

-- | @zinc build@: build every member's executables (libraries first).
runBuild :: FilePath -> IO (Either String [FilePath])
runBuild wsDir = buildWorkspace wsDir (== Executable)

-- | @zinc run@: build, then run the first executable with the given args,
-- returning its stdout.
buildAndRun :: FilePath -> [String] -> IO (Either String String)
buildAndRun wsDir args = do
  built <- runBuild wsDir
  case built of
    Left err        -> pure (Left err)
    Right []        -> pure (Left "no executable to run")
    Right (exe : _) -> Right <$> readProcess exe args ""

-- | @zinc test@: build and run all test-suite components, returning how many
-- passed. Fails on the first non-zero exit.
runTests :: FilePath -> IO (Either String Int)
runTests wsDir = do
  built <- buildWorkspace wsDir (== TestSuite)
  case built of
    Left err   -> pure (Left err)
    Right exes -> runEach 0 exes
  where
    runEach n [] = pure (Right n)
    runEach n (exe : rest) = do
      (code, _, _) <- readProcessWithExitCode exe [] ""
      case code of
        ExitSuccess   -> runEach (n + 1) rest
        ExitFailure _ -> pure (Left (exe ++ ": test suite failed"))

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
