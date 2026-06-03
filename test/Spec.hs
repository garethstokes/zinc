module Main (main) where

import Control.Monad (forM_, when)
import Data.Char (isSpace)
import Data.Either (isLeft)
import Data.Functor.Identity (runIdentity)
import Data.IORef (modifyIORef', newIORef, readIORef)
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
import Zinc.Git (cloneAt, listTags)
import Zinc.Hackage (hackageCabalUrl, sourceRepoOf)
import Zinc.Store (contentHash, storeSrcPath, verifyContent)
import Zinc.Manifest
  ( Component (..)
  , ComponentKind (..)
  , Dependency (..)
  , MemberManifest (..)
  , Ref (..)
  , WorkspaceManifest (..)
  , addDep
  , parseDependencies
  , parseMember
  , parseWorkspace
  , renderWorkspace
  )
import Zinc.Fetch (gitFetchManifest)
import Zinc.Add (freezeClosure, lockEntry, runAdd)
import Zinc.Build (GhcInvocation (..), PackageConf (..), archiveArgs, ghcMakeArgs, preprocessorFor, registerPackage, renderConf, runPreprocessor)
import Zinc.Cache (BuildKey (..), buildCacheKey, cacheHit, storeConfPath, storePkgPath, writeCachedConf)
import Zinc.Cabal (parseCabalComponents)
import Zinc.Env (envCacheKey, nixPrintDevEnv, provisionEnv)
import Zinc.Macros (emitCabalMacros)
import Zinc.Nix (generateFlake)
import Zinc.Paths (pathsModuleName, synthesizePaths)
import Zinc.Report (renderResolution)
import Zinc.SysLibs (toNixpkgs)
import Zinc.Resolve (DepManifest (..), ResolvedDep (..), isBootLib, resolve, topoSort)
import Zinc.Version (newestTag)
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

-- | Build a throwaway git repo whose zinc.toml declares deps + a registry,
-- tagged v1. Returns the repo path.
setupDepRepo :: IO FilePath
setupDepRepo = do
  let base = "/tmp/zinc-fetch-fixture"
      repo = base ++ "/dep"
  stale <- doesDirectoryExist base
  when stale $ removeDirectoryRecursive base
  createDirectoryIfMissing True repo
  let git args = readProcess "git" ("-C" : repo : args) ""
  _ <- git ["init", "--quiet"]
  _ <- git ["config", "user.email", "t@example.com"]
  _ <- git ["config", "user.name", "Test"]
  writeFile
    (repo ++ "/zinc.toml")
    ( unlines
        [ "[package]"
        , "name = \"dep\""
        , "version = \"1\""
        , "[dependencies]"
        , "aeson = { tag = \"v2\" }"
        , "[registry]"
        , "aeson = \"r/aeson\""
        ]
    )
  _ <- git ["add", "."]
  _ <- git ["commit", "--quiet", "-m", "c1"]
  _ <- git ["tag", "v1"]
  _ <- git ["tag", "v2"]
  pure repo

-- | Build a workspace + a local zinc-native leaf dep repo for end-to-end
-- `zinc add`. Returns (workspace zinc.toml path, store root, leaf repo path).
setupAddFixture :: IO (FilePath, FilePath, FilePath)
setupAddFixture = do
  let baseD = "/tmp/zinc-add-fixture"
  stale <- doesDirectoryExist baseD
  when stale $ removeDirectoryRecursive baseD
  let leaf = baseD ++ "/leaf"
  createDirectoryIfMissing True leaf
  let g d args = readProcess "git" ("-C" : d : args) ""
  _ <- g leaf ["init", "--quiet"]
  _ <- g leaf ["config", "user.email", "t@example.com"]
  _ <- g leaf ["config", "user.name", "Test"]
  writeFile (leaf ++ "/zinc.toml") "[package]\nname = \"leaf\"\nversion = \"1.0\"\n"
  _ <- g leaf ["add", "."]
  _ <- g leaf ["commit", "--quiet", "-m", "c1"]
  _ <- g leaf ["tag", "v1"]
  let wsDir = baseD ++ "/ws"
      wsFile = wsDir ++ "/zinc.toml"
  createDirectoryIfMissing True wsDir
  writeFile
    wsFile
    (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "leaf" (Tag "v1")] [("leaf", leaf)]))
  pure (wsFile, baseD ++ "/store", leaf)

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

  describe "topoSort" $ do
    let rd n ds = ResolvedDep n ("r/" ++ n) Latest ds

    it "orders dependencies before dependents (linear chain)" $
      (map rdName <$> topoSort [rd "aeson" ["scientific"], rd "scientific" ["il"], rd "il" []])
        `shouldBe` Right ["il", "scientific", "aeson"]

    it "produces a valid order for a diamond" $
      (map rdName <$> topoSort [rd "a" ["b", "c"], rd "b" ["d"], rd "c" ["d"], rd "d" []])
        `shouldBe` Right ["d", "b", "c", "a"]

    it "rejects a dependency cycle" $
      topoSort [rd "a" ["b"], rd "b" ["a"]] `shouldSatisfy` isLeft

    it "handles an empty closure" $
      topoSort [] `shouldBe` Right []

  describe "parseDependencies" $ do
    it "reads [dependencies] and [registry] without requiring [workspace]" $
      parseDependencies
        ( unlines
            [ "[package]"
            , "name = \"foo\""
            , "version = \"1\""
            , "[dependencies]"
            , "aeson = { tag = \"v2\" }"
            , "scientific = \"*\""
            , "[registry]"
            , "aeson = \"r/aeson\""
            , "scientific = \"r/sci\""
            ]
        )
        `shouldBe` Right
          ( [Dependency "aeson" (Tag "v2"), Dependency "scientific" Latest]
          , [("aeson", "r/aeson"), ("scientific", "r/sci")]
          )

    it "defaults to empty when the sections are absent" $
      parseDependencies "[package]\nname = \"x\"\nversion = \"1\"" `shouldBe` Right ([], [])

  describe "gitFetchManifest" $ do
    repo <- runIO setupDepRepo

    it "clones a dep at a ref and parses its manifest" $ do
      r <- gitFetchManifest "/tmp/zinc-fetch-store" "dep" repo (Tag "v1")
      r `shouldBe` Right (DepManifest [Dependency "aeson" (Tag "v2")] [("aeson", "r/aeson")])

    it "resolves a Latest ref to the newest tag and parses its manifest" $ do
      r <- gitFetchManifest "/tmp/zinc-fetch-store" "dep" repo Latest
      r `shouldBe` Right (DepManifest [Dependency "aeson" (Tag "v2")] [("aeson", "r/aeson")])

  describe "newestTag" $ do
    it "picks the highest semver tag (numeric, not lexical)" $
      newestTag ["v1.0.0", "v1.2.0", "v1.10.0", "v2.0.0"] `shouldBe` Just "v2.0.0"

    it "orders 1.10 above 1.2" $
      newestTag ["v1.2.0", "v1.10.0"] `shouldBe` Just "v1.10.0"

    it "ignores non-version tags" $
      newestTag ["nightly", "v1.0.0", "latest"] `shouldBe` Just "v1.0.0"

    it "is Nothing when there are no version tags" $
      newestTag ["nightly", "HEAD"] `shouldBe` Nothing

  describe "listTags" $ do
    repo <- runIO setupDepRepo
    it "lists the repo's tags" $ do
      r <- listTags repo
      (sort <$> r) `shouldBe` Right ["v1", "v2"]

  describe "generateFlake" $ do
    let flake = generateFlake "9.6.5" ["zlib", "pcre"]

    it "selects the pinned GHC compiler with dots stripped" $
      ("haskell.compiler.ghc965" `isInfixOf` flake) `shouldBe` True

    it "includes each system library as a nixpkgs attr" $
      all (`isInfixOf` flake) ["pkgs.zlib", "pkgs.pcre"] `shouldBe` True

    it "includes the alex/happy preprocessors" $
      all (`isInfixOf` flake) ["alex", "happy"] `shouldBe` True

    it "pins nixpkgs and exposes a devShell" $
      all (`isInfixOf` flake) ["nixpkgs.url", "devShells"] `shouldBe` True

    it "works with no system libraries" $
      ("haskell.compiler.ghc965" `isInfixOf` generateFlake "9.6.5" []) `shouldBe` True

  describe "dev env provisioning" $ do
    it "key is order-independent for system libs" $
      envCacheKey "9.6.5" ["a", "b"] `shouldBe` envCacheKey "9.6.5" ["b", "a"]

    it "key changes with the ghc version" $
      (envCacheKey "9.6.5" [] == envCacheKey "9.8.2" []) `shouldBe` False

    it "evaluates on a cache miss and reuses on a hit" $ do
      let root = "/tmp/zinc-env-test/hit"
      stale <- doesDirectoryExist root
      when stale $ removeDirectoryRecursive root
      counter <- newIORef (0 :: Int)
      let evalr _ _ = modifyIORef' counter (+ 1) >> pure (Right "ENVDATA")
      a <- provisionEnv evalr root "9.6.5" ["zlib"]
      b <- provisionEnv evalr root "9.6.5" ["zlib"]
      n <- readIORef counter
      (a, b, n) `shouldBe` (Right "ENVDATA", Right "ENVDATA", 1)

    it "re-evaluates when the inputs change" $ do
      let root = "/tmp/zinc-env-test/change"
      stale <- doesDirectoryExist root
      when stale $ removeDirectoryRecursive root
      counter <- newIORef (0 :: Int)
      let evalr _ libs = modifyIORef' counter (+ 1) >> pure (Right (unwords libs))
      _ <- provisionEnv evalr root "9.6.5" ["zlib"]
      _ <- provisionEnv evalr root "9.6.5" ["zlib", "pcre"]
      n <- readIORef counter
      n `shouldBe` 2

  describe "parseCabalComponents" $ do
    let cabal =
          unlines
            [ "cabal-version: 2.4"
            , "name: demo"
            , "version: 0.1"
            , "library"
            , "  hs-source-dirs: src"
            , "  exposed-modules: Demo Demo.Core"
            , "  other-modules: Demo.Internal"
            , "  default-extensions: OverloadedStrings"
            , "  ghc-options: -Wall"
            , "  build-depends: base, aeson"
            , "  extra-libraries: z pthread"
            , "executable demo-exe"
            , "  main-is: Main.hs"
            , "  hs-source-dirs: app"
            , "  build-depends: base, demo"
            , "test-suite spec"
            , "  type: exitcode-stdio-1.0"
            , "  main-is: Spec.hs"
            , "  hs-source-dirs: test"
            , "  build-depends: base, demo, hspec"
            ]
        comps = either (const []) id (parseCabalComponents cabal)
        byName n = find ((== n) . compName) comps

    it "derives the library component with all fields" $
      byName "lib"
        `shouldBe` Just
          Component
            { compKind = Library
            , compName = "lib"
            , compSourceDirs = ["src"]
            , compExposedModules = ["Demo", "Demo.Core"]
            , compOtherModules = ["Demo.Internal"]
            , compMain = Nothing
            , compExtensions = ["OverloadedStrings"]
            , compGhcOptions = ["-Wall"]
            , compDepends = ["aeson", "base"] -- finalizePD normalizes build-depends order
            , compSystemLibs = ["zlib"]
            }

    it "derives an executable component" $
      (\c -> (compKind c, compMain c, compSourceDirs c, compDepends c)) <$> byName "demo-exe"
        `shouldBe` Just (Executable, Just "Main.hs", ["app"], ["base", "demo"])

    it "derives a test-suite component" $
      (\c -> (compKind c, compMain c, compDepends c)) <$> byName "spec"
        `shouldBe` Just (TestSuite, Just "Spec.hs", ["base", "demo", "hspec"])

    it "errors on malformed cabal input" $
      parseCabalComponents "library\n  exposed-modules: =bad=" `shouldSatisfy` isLeft

    it "resolves flag conditionals (fast defaults True -> -O2)" $
      let c =
            unlines
              [ "cabal-version: 2.4"
              , "name: c"
              , "version: 1"
              , "flag fast"
              , "  default: True"
              , "library"
              , "  build-depends: base"
              , "  exposed-modules: M"
              , "  if flag(fast)"
              , "    ghc-options: -O2"
              ]
          libc = either (const Nothing) (find ((== "lib") . compName)) (parseCabalComponents c)
       in (compGhcOptions <$> libc) `shouldBe` Just ["-O2"]

  describe "synthesizePaths" $ do
    let src = synthesizePaths "my-pkg" [0, 1, 0]

    it "munges dashes to underscores in the module name" $
      pathsModuleName "my-pkg" `shouldBe` "Paths_my_pkg"

    it "declares the Paths_ module and exports the common API" $
      all (`isInfixOf` src) ["module Paths_my_pkg", "version", "getDataFileName"]
        `shouldBe` True

    it "encodes the version via makeVersion" $
      ("makeVersion [0,1,0]" `isInfixOf` src) `shouldBe` True

  describe "emitCabalMacros" $ do
    let h = emitCabalMacros [("base", [4, 18, 2, 1]), ("my-dep", [1, 2])]

    it "defines VERSION_ with the full version string" $
      ("#define VERSION_base \"4.18.2.1\"" `isInfixOf` h) `shouldBe` True

    it "defines MIN_VERSION_ using the first three components" $
      all
        (`isInfixOf` h)
        [ "#define MIN_VERSION_base(major1,major2,minor)"
        , "(major1) <  4"
        , "(major2) <  18"
        , "(minor) <= 2"
        ]
        `shouldBe` True

    it "munges dashes in macro identifiers" $
      all (`isInfixOf` h) ["VERSION_my_dep", "MIN_VERSION_my_dep(major1,major2,minor)"]
        `shouldBe` True

    it "pads short versions with zeros" $
      ("(minor) <= 0" `isInfixOf` h) `shouldBe` True

  describe "renderResolution" $ do
    let rds =
          [ ResolvedDep "aeson" "https://github.com/haskell/aeson" (Tag "v2.2.3.0") ["scientific"]
          , ResolvedDep "scientific" "https://github.com/basvandijk/scientific" Latest []
          ]
        out = renderResolution rds

    it "lists each package with its ref and repo" $
      all
        (`isInfixOf` out)
        ["aeson", "v2.2.3.0", "scientific", "*", "https://github.com/haskell/aeson"]
        `shouldBe` True

    it "includes a header row" $
      all (`isInfixOf` out) ["package", "ref", "repo"] `shouldBe` True

    it "reports an empty closure" $
      renderResolution [] `shouldBe` "(no dependencies)\n"

  describe "toNixpkgs" $ do
    it "maps known C lib names to nixpkgs attrs" $ do
      toNixpkgs "z" `shouldBe` Just "zlib"
      toNixpkgs "crypto" `shouldBe` Just "openssl"

    it "drops libc-provided system libs" $
      toNixpkgs "pthread" `shouldBe` Nothing

    it "falls back to identity for unknown libs" $
      toNixpkgs "ncurses" `shouldBe` Just "ncurses"

  describe "freeze engine" $ do
    repo <- runIO setupDepRepo

    it "lockEntry maps a resolved dep + rev + sha to a LockedPackage" $
      lockEntry (ResolvedDep "aeson" "r/aeson" (Tag "v2") ["scientific"]) "abc123" "sha256:xyz"
        `shouldBe` LockedPackage
          { lockName = "aeson"
          , lockRepo = "r/aeson"
          , lockRev = "abc123"
          , lockSha256 = "sha256:xyz"
          , lockDepends = ["scientific"]
          }

    it "freezeClosure clones each dep and records its commit + content hash" $ do
      let rd = ResolvedDep "dep" repo (Tag "v1") ["aeson"]
      r <- freezeClosure "/tmp/zinc-freeze-store" [rd]
      case r of
        Right [lp] -> do
          (lockName lp, lockRepo lp, lockDepends lp) `shouldBe` ("dep", repo, ["aeson"])
          length (lockRev lp) `shouldBe` 40 -- git sha1 hex
          take 7 (lockSha256 lp) `shouldBe` "sha256:"
        other -> expectationFailure ("unexpected freeze result: " ++ show other)

  describe "nixPrintDevEnv" $
    it "evaluates a generated flake and exposes the pinned ghc" $ do
      let dir = "/tmp/zinc-devenv-test"
      stale <- doesDirectoryExist dir
      when stale $ removeDirectoryRecursive dir
      r <- nixPrintDevEnv dir "9.6.5" []
      case r of
        Right out -> ("ghc-9.6.5" `isInfixOf` out) `shouldBe` True
        Left err  -> expectationFailure err

  describe "ghcMakeArgs" $ do
    it "builds a fully-specified ghc --make invocation" $
      ghcMakeArgs
        GhcInvocation
          { giUnitId = "myapp-0.1.0"
          , giPackageDb = "/store/pkgdb"
          , giDeps = ["aeson", "scientific"]
          , giSourceDirs = ["src"]
          , giModules = ["Myapp", "Myapp.Core"]
          , giExtensions = ["OverloadedStrings"]
          , giGhcOptions = ["-Wall"]
          , giOutputDir = ".zinc/build/myapp"
          }
        `shouldBe` [ "--make"
                   , "-hide-all-packages"
                   , "-package-db", "/store/pkgdb"
                   , "-package", "aeson"
                   , "-package", "scientific"
                   , "-isrc"
                   , "-this-unit-id", "myapp-0.1.0"
                   , "-outputdir", ".zinc/build/myapp"
                   , "-O"
                   , "-XOverloadedStrings"
                   , "-Wall"
                   , "Myapp", "Myapp.Core"
                   ]

    it "always isolates packages even with no deps/extensions/options" $
      ghcMakeArgs
        GhcInvocation
          { giUnitId = "leaf-1.0"
          , giPackageDb = "/db"
          , giDeps = []
          , giSourceDirs = ["."]
          , giModules = ["Leaf"]
          , giExtensions = []
          , giGhcOptions = []
          , giOutputDir = "out"
          }
        `shouldBe` [ "--make"
                   , "-hide-all-packages"
                   , "-package-db", "/db"
                   , "-i."
                   , "-this-unit-id", "leaf-1.0"
                   , "-outputdir", "out"
                   , "-O"
                   , "Leaf"
                   ]

  describe "package conf + register" $ do
    it "archiveArgs builds the ar command" $
      archiveArgs "/lib" "myapp-0.1.0" ["A.o", "B.o"]
        `shouldBe` ["rcs", "/lib/libHSmyapp-0.1.0.a", "A.o", "B.o"]

    it "renderConf emits the key fields" $
      let out =
            renderConf
              PackageConf
                { confName = "myapp"
                , confVersion = "0.1.0"
                , confId = "myapp-0.1.0-abc"
                , confExposedModules = ["Myapp", "Myapp.Core"]
                , confImportDirs = ["/hi"]
                , confLibraryDirs = ["/lib"]
                , confHsLibraries = ["HSmyapp-0.1.0-abc"]
                , confDepends = []
                }
       in all
            (`isInfixOf` out)
            [ "name: myapp"
            , "id: myapp-0.1.0-abc"
            , "exposed-modules: Myapp Myapp.Core"
            , "hs-libraries: HSmyapp-0.1.0-abc"
            ]
            `shouldBe` True

    it "registers a synthesized conf (accepted by ghc-pkg)" $ do
      let base = "/tmp/zinc-pkgdb-test"
          db = base ++ "/db"
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      let conf =
            renderConf
              PackageConf
                { confName = "demo"
                , confVersion = "1.0"
                , confId = "demo-1.0-deadbeef"
                , confExposedModules = ["Demo"]
                , confImportDirs = [base ++ "/hi"]
                , confLibraryDirs = [base ++ "/lib"]
                , confHsLibraries = ["HSdemo-1.0-deadbeef"]
                , confDepends = []
                }
      r <- registerPackage db conf
      r `shouldBe` Right ()

  describe "preprocessors" $ do
    it "maps source extensions to their preprocessor command" $ do
      preprocessorFor "Lexer.x" `shouldBe` Just ("alex", ["Lexer.x", "-o", "Lexer.hs"])
      preprocessorFor "Parser.y" `shouldBe` Just ("happy", ["Parser.y", "-o", "Parser.hs"])
      preprocessorFor "Foo.hsc" `shouldBe` Just ("hsc2hs", ["Foo.hsc", "-o", "Foo.hs"])

    it "leaves plain .hs files alone" $
      preprocessorFor "Plain.hs" `shouldBe` Nothing

    it "runPreprocessor runs hsc2hs and produces the .hs" $ do
      let dir = "/tmp/zinc-pp-test"
      stale <- doesDirectoryExist dir
      when stale $ removeDirectoryRecursive dir
      createDirectoryIfMissing True dir
      writeFile (dir ++ "/Foo.hsc") "module Foo where\nanswer :: Int\nanswer = 42\n"
      r <- runPreprocessor (dir ++ "/Foo.hsc")
      produced <- doesFileExist (dir ++ "/Foo.hs")
      (r, produced) `shouldBe` (Right (), True)

  describe "build cache key" $ do
    let key deps opts = buildCacheKey (BuildKey "abc" "9.6.5" deps opts)
        k1 = key ["base-4", "aeson-2"] ["-O2"]

    it "is deterministic" $
      key ["base-4", "aeson-2"] ["-O2"] `shouldBe` k1

    it "is order-independent for deps and options" $
      key ["aeson-2", "base-4"] ["-O2"] `shouldBe` k1

    it "changes with the resolved rev" $
      (buildCacheKey (BuildKey "xyz" "9.6.5" ["base-4", "aeson-2"] ["-O2"]) == k1) `shouldBe` False

    it "changes with the ghc version" $
      (buildCacheKey (BuildKey "abc" "9.8.2" ["base-4", "aeson-2"] ["-O2"]) == k1) `shouldBe` False

    it "lays out the package store path" $
      storePkgPath "/store" "deadbeef" `shouldBe` "/store/pkg/deadbeef"

  describe "artifact cache hit/miss" $ do
    it "misses when absent, hits after caching, and stores the conf" $ do
      let root = "/tmp/zinc-cache-test"
          key = "abc123"
      stale <- doesDirectoryExist root
      when stale $ removeDirectoryRecursive root
      miss <- cacheHit root key
      writeCachedConf root key "name: demo\n"
      hit <- cacheHit root key
      conf <- readFile (storeConfPath root key)
      (miss, hit, conf) `shouldBe` (False, True, "name: demo\n")

  describe "workspace write-back" $ do
    let ws =
          WorkspaceManifest
            { wsMembers = ["packages/a", "packages/b"]
            , wsGhc = "9.6.5"
            , wsDependencies = [Dependency "aeson" (Tag "v2"), Dependency "hspec" Latest]
            , wsRegistry = [("aeson", "r/aeson"), ("hspec", "r/hspec")]
            }

    it "renderWorkspace round-trips through parseWorkspace" $
      parseWorkspace (renderWorkspace ws) `shouldBe` Right ws

    it "addDep inserts a new dependency + registry entry (sorted)" $
      let w = addDep (WorkspaceManifest ["packages/a"] "9.6.5" [] []) "aeson" (Tag "v2") "r/aeson"
       in (wsDependencies w, wsRegistry w)
            `shouldBe` ([Dependency "aeson" (Tag "v2")], [("aeson", "r/aeson")])

    it "addDep replaces an existing dependency in place" $
      let w0 = addDep (WorkspaceManifest [] "9.6.5" [] []) "aeson" (Tag "v2") "r/aeson"
          w1 = addDep w0 "aeson" Latest "r/aeson2"
       in (wsDependencies w1, wsRegistry w1)
            `shouldBe` ([Dependency "aeson" Latest], [("aeson", "r/aeson2")])

  describe "isBootLib" $ do
    it "recognises GHC boot libraries" $
      all isBootLib ["base", "text", "bytestring", "containers"] `shouldBe` True

    it "does not flag ordinary packages" $
      any isBootLib ["aeson", "scientific", "hspec"] `shouldBe` False

  describe "runAdd (end-to-end)" $ do
    (wsFile, store, leafRepo) <- runIO setupAddFixture

    it "resolves, freezes, and writes the lock + manifest" $ do
      r <- runAdd wsFile store "leaf" (Tag "v1") leafRepo
      lockText <- readFile (takeDirectory wsFile </> "zinc.lock")
      case r of
        Right summary ->
          (("leaf" `isInfixOf` summary), ("leaf" `isInfixOf` lockText)) `shouldBe` (True, True)
        Left err -> expectationFailure err

  describe "Hackage repo discovery" $ do
    it "extracts the source-repository head location from a .cabal" $
      sourceRepoOf
        ( unlines
            [ "cabal-version: 2.4"
            , "name: demo"
            , "version: 0.1"
            , "source-repository head"
            , "  type: git"
            , "  location: https://github.com/x/demo.git"
            , "library"
            , "  build-depends: base"
            ]
        )
        `shouldBe` Just "https://github.com/x/demo.git"

    it "returns Nothing when there is no source-repository" $
      sourceRepoOf "cabal-version: 2.4\nname: demo\nversion: 0.1\n" `shouldBe` Nothing

    it "builds the Hackage .cabal URL" $
      hackageCabalUrl "aeson" `shouldBe` "https://hackage.haskell.org/package/aeson/aeson.cabal"

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
