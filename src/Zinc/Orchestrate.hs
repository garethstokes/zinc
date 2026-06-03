-- | Build orchestration (spec §7): the @zinc build@ command. Reads the
-- workspace, then builds each member's executable components.
--
-- Note: compiling the git /dependency closure/ from source (fetch →
-- preprocess → ghc → archive → register, with caching) is a separate, larger
-- concern; this builds the workspace members themselves (the dependency-light
-- happy path, incl. a fresh @zinc new@ project).
module Zinc.Orchestrate
  ( runBuild
  ) where

import System.FilePath ((</>))
import Zinc.Build (MemberBuild (..), buildMember)
import Zinc.Manifest
  ( Component (compKind)
  , ComponentKind (Executable)
  , MemberManifest (pkgComponents)
  , WorkspaceManifest (wsMembers)
  , parseMember
  , parseWorkspace
  )

-- | Build every member's executables in a workspace. Returns the built
-- executable paths, or the first error.
runBuild :: FilePath -> IO (Either String [FilePath])
runBuild wsDir = do
  wsSrc <- readFile (wsDir </> "zinc.toml")
  case parseWorkspace wsSrc of
    Left err -> pure (Left err)
    Right ws -> buildMembers [] (wsMembers ws)
  where
    buildMembers acc [] = pure (Right acc)
    buildMembers acc (member : rest) = do
      let memberDir = wsDir </> member
      msrc <- readFile (memberDir </> "zinc.toml")
      case parseMember msrc of
        Left err -> pure (Left (member ++ ": " ++ err))
        Right mem -> do
          built <- buildExes memberDir (filter ((== Executable) . compKind) (pkgComponents mem)) []
          case built of
            Left err    -> pure (Left err)
            Right paths -> buildMembers (acc ++ paths) rest

    buildExes _ [] acc = pure (Right (reverse acc))
    buildExes memberDir (comp : rest) acc = do
      result <- buildMember (MemberBuild memberDir (memberDir </> ".zinc" </> "build") Nothing comp)
      case result of
        Left err  -> pure (Left err)
        Right exe -> buildExes memberDir rest (exe : acc)
