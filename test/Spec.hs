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
