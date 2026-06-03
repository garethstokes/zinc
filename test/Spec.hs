module Main (main) where

import Control.Monad (forM_, when)
import Data.Char (isSpace)
import Data.Either (isLeft)
import Data.Functor.Identity (runIdentity)
import Data.List (find, isInfixOf, sort)
import Data.Maybe (isJust)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , removeDirectoryRecursive
  )
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcess)
import Test.Hspec
import Zinc.CLI (Command (..), parseArgs)
import Zinc.Git (cloneAt)
import Zinc.Store (contentHash, storeSrcPath, verifyContent)
import Zinc.Manifest
  ( Component (..)
  , ComponentKind (..)
  , Dependency (..)
  , MemberManifest (..)
  , Ref (..)
  , WorkspaceManifest (..)
  , parseMember
  , parseWorkspace
  )
import Zinc.Resolve (DepManifest (..), ResolvedDep (..), resolve)
import Zinc.Lock (LockedPackage (..), parseLock, renderLock)
import Zinc.Scaffold (FileSpec (..), materialize, scaffoldNew)

-- | Body of the generated file at the given path, if present.
bodyOf :: FilePath -> [FileSpec] -> Maybe String
bodyOf p = fmap specBody . find ((== p) . specPath)

trimStr :: String -> String
trimStr = f . f where f = reverse . dropWhile isSpace

-- | Write a fresh directory tree from (relative path, contents) pairs.
writeTree :: FilePath -> [(FilePath, String)] -> IO ()
writeTree root files = do
  stale <- doesDirectoryExist root
  when stale $ removeDirectoryRecursive root
  forM_ files $ \(rel, content) -> do
    let full = root </> rel
    createDirectoryIfMissing True (takeDirectory full)
    writeFile full content

-- | Build a throwaway local git repo: commit c1 (tagged v1.0), then commit c2
-- on the default branch plus a `feature` branch at c2. Returns the repo path
-- and the two commit SHAs. No network involved.
setupGitFixture :: IO (FilePath, String, String)
setupGitFixture = do
  let base = "/tmp/zinc-git-fixture"
      repo = base ++ "/repo"
  stale <- doesDirectoryExist base
  when stale $ removeDirectoryRecursive base
  createDirectoryIfMissing True repo
  let git args = readProcess "git" ("-C" : repo : args) ""
  _ <- git ["init", "--quiet"]
  _ <- git ["config", "user.email", "t@example.com"]
  _ <- git ["config", "user.name", "Test"]
  writeFile (repo ++ "/a.txt") "a"
  _ <- git ["add", "."]
  _ <- git ["commit", "--quiet", "-m", "c1"]
  c1 <- trimStr <$> git ["rev-parse", "HEAD"]
  _ <- git ["tag", "v1.0"]
  writeFile (repo ++ "/b.txt") "b"
  _ <- git ["add", "."]
  _ <- git ["commit", "--quiet", "-m", "c2"]
  c2 <- trimStr <$> git ["rev-parse", "HEAD"]
  _ <- git ["branch", "feature"]
  pure (repo, c1, c2)

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

  describe "Zinc.Lock" $ do
    let pkgs =
          [ LockedPackage
              { lockName = "aeson"
              , lockRepo = "https://github.com/haskell/aeson"
              , lockRev = "a1b2c3d"
              , lockSha256 = "sha256-Xk9"
              , lockDepends = ["scientific", "witherable"]
              }
          , LockedPackage
              { lockName = "scientific"
              , lockRepo = "https://github.com/basvandijk/scientific"
              , lockRev = "f4e5d6"
              , lockSha256 = "sha256-Yz1"
              , lockDepends = []
              }
          ]
        sample =
          unlines
            [ "[[locked]]"
            , "name = \"aeson\""
            , "repo = \"https://github.com/haskell/aeson\""
            , "rev = \"a1b2c3d\""
            , "sha256 = \"sha256-Xk9\""
            , "depends = [\"scientific\", \"witherable\"]"
            , ""
            , "[[locked]]"
            , "name = \"scientific\""
            , "repo = \"https://github.com/basvandijk/scientific\""
            , "rev = \"f4e5d6\""
            , "sha256 = \"sha256-Yz1\""
            , "depends = []"
            ]

    it "parses a lockfile into locked packages, preserving order" $
      parseLock sample `shouldBe` Right pkgs

    it "round-trips: parse . render == id" $
      parseLock (renderLock pkgs) `shouldBe` Right pkgs

    it "treats an empty/absent [[locked]] array as no packages" $
      parseLock "" `shouldBe` Right []

  describe "cloneAt" $ do
    (repo, c1, c2) <- runIO setupGitFixture

    it "resolves a tag to its commit" $ do
      r <- cloneAt repo "v1.0" "/tmp/zinc-git-fixture/co-tag"
      r `shouldBe` Right c1

    it "resolves a branch to its commit" $ do
      r <- cloneAt repo "feature" "/tmp/zinc-git-fixture/co-branch"
      r `shouldBe` Right c2

    it "resolves an explicit commit SHA" $ do
      r <- cloneAt repo c1 "/tmp/zinc-git-fixture/co-rev"
      r `shouldBe` Right c1

    it "checks out the worktree at the requested ref" $ do
      _ <- cloneAt repo "v1.0" "/tmp/zinc-git-fixture/co-wt"
      hasA <- doesFileExist "/tmp/zinc-git-fixture/co-wt/a.txt"
      hasB <- doesFileExist "/tmp/zinc-git-fixture/co-wt/b.txt"
      (hasA, hasB) `shouldBe` (True, False)

    it "fails on an unknown ref" $ do
      r <- cloneAt repo "no-such-ref" "/tmp/zinc-git-fixture/co-bad"
      r `shouldSatisfy` isLeft

  describe "Zinc.Store" $ do
    let base = "/tmp/zinc-store-test"

    it "computes the canonical store source path" $
      storeSrcPath "/store" "aeson" "abc123" `shouldBe` "/store/src/aeson-abc123"

    it "hashes a tree deterministically" $ do
      let d = base ++ "/det"
      writeTree d [("a.hs", "module A"), ("sub/b.hs", "module B")]
      h1 <- contentHash d
      h2 <- contentHash d
      h1 `shouldBe` h2

    it "ignores the .git directory when hashing" $ do
      let d1 = base ++ "/nogit"
          d2 = base ++ "/withgit"
      writeTree d1 [("a.hs", "x")]
      writeTree d2 [("a.hs", "x"), (".git/HEAD", "ref: refs/heads/main"), (".git/config", "junk")]
      ha <- contentHash d1
      hb <- contentHash d2
      ha `shouldBe` hb

    it "changes the hash when a file's contents change" $ do
      let d1 = base ++ "/one"
          d2 = base ++ "/two"
      writeTree d1 [("a.hs", "one")]
      writeTree d2 [("a.hs", "two")]
      h1 <- contentHash d1
      h2 <- contentHash d2
      h1 `shouldNotBe` h2

    it "verifies matching content and rejects a mismatch" $ do
      let d = base ++ "/verify"
      writeTree d [("a.hs", "hello")]
      h <- contentHash d
      ok <- verifyContent d h
      bad <- verifyContent d "sha256:deadbeef"
      (ok, bad) `shouldBe` (True, False)

  describe "parseMember [build.*] components" $ do
    let sample =
          unlines
            [ "[package]"
            , "name = \"myapp\""
            , "version = \"0.1.0\""
            , ""
            , "[build.lib]"
            , "source-dirs = [\"src\"]"
            , "exposed-modules = [\"Myapp\", \"Myapp.Core\"]"
            , "other-modules = [\"Myapp.Internal\"]"
            , "extensions = [\"OverloadedStrings\"]"
            , "ghc-options = [\"-Wall\"]"
            , "depends = [\"aeson\"]"
            , "system-libs = [\"zlib\"]"
            , ""
            , "[build.exe.myapp]"
            , "source-dirs = [\"app\"]"
            , "main = \"Main.hs\""
            , "depends = [\"myapp\"]"
            , ""
            , "[build.test.spec]"
            , "source-dirs = [\"test\"]"
            , "main = \"Spec.hs\""
            , "depends = [\"myapp\", \"hspec\"]"
            ]
        comps = either (const []) pkgComponents (parseMember sample)
        byName n = find ((== n) . compName) comps

    it "parses the library component with all its fields" $
      byName "lib"
        `shouldBe` Just
          Component
            { compKind = Library
            , compName = "lib"
            , compSourceDirs = ["src"]
            , compExposedModules = ["Myapp", "Myapp.Core"]
            , compOtherModules = ["Myapp.Internal"]
            , compMain = Nothing
            , compExtensions = ["OverloadedStrings"]
            , compGhcOptions = ["-Wall"]
            , compDepends = ["aeson"]
            , compSystemLibs = ["zlib"]
            }

    it "parses a named executable component" $
      (\c -> (compKind c, compMain c, compSourceDirs c, compDepends c)) <$> byName "myapp"
        `shouldBe` Just (Executable, Just "Main.hs", ["app"], ["myapp"])

    it "parses a named test component" $
      (\c -> (compKind c, compMain c, compDepends c)) <$> byName "spec"
        `shouldBe` Just (TestSuite, Just "Spec.hs", ["myapp", "hspec"])

    it "yields no components when [build] is absent" $
      (pkgComponents <$> parseMember "[package]\nname = \"x\"\nversion = \"1\"")
        `shouldBe` Right []

  describe "resolve (graph walk)" $ do
    let dep n r = Dependency n r
        boot = (`elem` ["base", "text", "bytestring", "containers"])
        fetchFrom fix n _ _ = pure (maybe (Left ("missing: " ++ n)) Right (lookup n fix))
        run fix rootDeps rootReg =
          runIdentity (resolve boot (fetchFrom fix) rootDeps rootReg)
        findRD n r = either (const Nothing) (find ((== n) . rdName)) r

        fixture =
          [ ("aeson", DepManifest [dep "scientific" Latest, dep "base" Latest] [("scientific", "r/sci")])
          , ("scientific", DepManifest [dep "integer-logarithms" Latest] [("integer-logarithms", "r/il")])
          , ("integer-logarithms", DepManifest [dep "base" Latest] [])
          ]
        rootDeps = [dep "aeson" (Tag "v2.2.3.0")]
        rootReg = [("aeson", "r/aeson")]
        result = run fixture rootDeps rootReg

    it "resolves the full transitive closure (non-boot)" $
      (sort . map rdName <$> result)
        `shouldBe` Right ["aeson", "integer-logarithms", "scientific"]

    it "carries each node's repo and ref" $
      (\d -> (rdRepo d, rdRef d)) <$> findRD "aeson" result
        `shouldBe` Just ("r/aeson", Tag "v2.2.3.0")

    it "records non-boot direct deps and drops boot libs" $ do
      (rdDepends <$> findRD "aeson" result) `shouldBe` Just ["scientific"]
      (rdDepends <$> findRD "integer-logarithms" result) `shouldBe` Just []

    it "lets a root pin win over a transitive pin for the same name" $ do
      let fix =
            [ ("aeson", DepManifest [dep "scientific" Latest] [("scientific", "r/sci")])
            , ("scientific", DepManifest [] [])
            ]
          rds = [dep "aeson" (Tag "v2"), dep "scientific" (Tag "root-pin")]
          reg = [("aeson", "r/aeson"), ("scientific", "r/sci")]
          r = run fix rds reg
      (rdRef <$> findRD "scientific" r) `shouldBe` Just (Tag "root-pin")

    it "errors when a dependency has no repo in any registry" $ do
      let fix =
            [ ("aeson", DepManifest [dep "scientific" Latest] []) -- no repo for scientific
            ]
          r = run fix [dep "aeson" (Tag "v2")] [("aeson", "r/aeson")]
      r `shouldSatisfy` isLeft

    it "terminates on dependency cycles" $ do
      let fix =
            [ ("a", DepManifest [dep "b" Latest] [("b", "r/b")])
            , ("b", DepManifest [dep "a" Latest] [("a", "r/a")])
            ]
          r = run fix [dep "a" Latest] [("a", "r/a")]
      (sort . map rdName <$> r) `shouldBe` Right ["a", "b"]

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
