module Main (main) where

import Control.Monad (when)
import Data.Either (isLeft)
import Data.List (find, isInfixOf)
import Data.Maybe (isJust)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  )
import Test.Hspec
import Zinc.CLI (Command (..), parseArgs)
import Zinc.Manifest
  ( Dependency (..)
  , MemberManifest (..)
  , Ref (..)
  , WorkspaceManifest (..)
  , parseMember
  , parseWorkspace
  )
import Zinc.Scaffold (FileSpec (..), materialize, scaffoldNew)

-- | Body of the generated file at the given path, if present.
bodyOf :: FilePath -> [FileSpec] -> Maybe String
bodyOf p = fmap specBody . find ((== p) . specPath)

main :: IO ()
main = hspec $ do
  describe "parseArgs" $ do
    it "parses the `build` subcommand" $
      parseArgs ["build"] `shouldBe` Right Build

    it "parses `new <name>` with its argument" $
      parseArgs ["new", "myapp"] `shouldBe` Right (New "myapp")

    it "parses `add <pkg>` with its argument" $
      parseArgs ["add", "aeson"] `shouldBe` Right (Add "aeson")

    it "parses `clean`" $
      parseArgs ["clean"] `shouldBe` Right Clean

    it "parses `repl` with no target" $
      parseArgs ["repl"] `shouldBe` Right (Repl Nothing)

    it "parses `repl <target>`" $
      parseArgs ["repl", "mylib"] `shouldBe` Right (Repl (Just "mylib"))

    it "parses `test` with no target" $
      parseArgs ["test"] `shouldBe` Right (Test Nothing)

    it "parses `update` with no package" $
      parseArgs ["update"] `shouldBe` Right (Update Nothing)

    it "parses `run` and passes through args after --" $
      parseArgs ["run", "--", "a", "b"] `shouldBe` Right (Run ["a", "b"])

    it "rejects an unknown subcommand" $
      parseArgs ["frobnicate"] `shouldSatisfy` isLeft

  describe "scaffoldNew" $ do
    let files = scaffoldNew "myapp"

    it "writes a workspace manifest that lists the member" $
      (isInfixOf "members = [\"packages/myapp\"]" <$> bodyOf "zinc.toml" files)
        `shouldBe` Just True

    it "writes a member manifest naming the package" $
      (isInfixOf "name = \"myapp\"" <$> bodyOf "packages/myapp/zinc.toml" files)
        `shouldBe` Just True

    it "writes a Main.hs entrypoint for the member" $
      bodyOf "packages/myapp/app/Main.hs" files `shouldSatisfy` isJust

  describe "parseWorkspace" $ do
    let sample =
          unlines
            [ "[workspace]"
            , "members = [\"packages/myapp\", \"packages/mylib\"]"
            , "ghc = \"9.6.5\""
            , ""
            , "[dependencies]"
            , "aeson = { tag = \"v2.2.3.0\" }"
            , "hspec = \"*\""
            , ""
            , "[registry]"
            , "aeson = \"https://github.com/haskell/aeson\""
            ]
        parsed = parseWorkspace sample
        refOf name = lookup name . map (\d -> (depName d, depRef d)) . wsDependencies

    it "reads the workspace members" $
      (wsMembers <$> parsed) `shouldBe` Right ["packages/myapp", "packages/mylib"]

    it "reads the ghc version" $
      (wsGhc <$> parsed) `shouldBe` Right "9.6.5"

    it "reads a tag-pinned dependency" $
      (refOf "aeson" <$> parsed) `shouldBe` Right (Just (Tag "v2.2.3.0"))

    it "reads a `*` dependency as Latest" $
      (refOf "hspec" <$> parsed) `shouldBe` Right (Just Latest)

    it "reads registry entries" $
      ((lookup "aeson" . wsRegistry) <$> parsed)
        `shouldBe` Right (Just "https://github.com/haskell/aeson")

    it "fails on a missing [workspace] table" $
      parseWorkspace "[dependencies]\n" `shouldSatisfy` isLeft

  describe "parseMember" $ do
    let sample =
          unlines
            [ "[package]"
            , "name = \"myapp\""
            , "version = \"0.2.3\""
            , ""
            , "[build.exe.myapp]"
            , "main = \"Main.hs\""
            ]

    it "reads the package name" $
      (pkgName <$> parseMember sample) `shouldBe` Right "myapp"

    it "reads the package version" $
      (pkgVersion <$> parseMember sample) `shouldBe` Right "0.2.3"

    it "parses the member manifest produced by scaffoldNew" $
      let body = maybe "" id (bodyOf "packages/demo/zinc.toml" (scaffoldNew "demo"))
       in (pkgName <$> parseMember body) `shouldBe` Right "demo"

  describe "materialize" $
    it "writes every FileSpec under the given root, creating parent dirs" $ do
      let root = "/tmp/zinc-scaffold-test"
      stale <- doesDirectoryExist root
      when stale $ removeDirectoryRecursive root
      materialize root (scaffoldNew "demo")
      wsExists <- doesFileExist (root ++ "/zinc.toml")
      mainExists <- doesFileExist (root ++ "/packages/demo/app/Main.hs")
      memberBody <- readFile (root ++ "/packages/demo/zinc.toml")
      (wsExists, mainExists, "name = \"demo\"" `isInfixOf` memberBody)
        `shouldBe` (True, True, True)
