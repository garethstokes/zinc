module Main (main) where

import Control.Monad (forM_, when)
import Data.Char (isSpace)
import Data.Either (isLeft, isRight)
import Data.Functor.Identity (runIdentity)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, try)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, isInfixOf, sort)
import Data.Maybe (isJust)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getHomeDirectory
  , removeDirectoryRecursive
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcess)
import Test.Hspec
import System.Exit (ExitCode (..))
import Zinc.CLI (Command (..), parseArgs)
import Zinc.Diagnostic (Diagnostic (..), Severity (..), ZincError (..), diagnosticJson, envelope, errorCode, exitCodeFor, renderError, toDiagnostic)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Zinc.Closure (discoverRepos, parseDependsField, pkgNameOf)
import Zinc.Output (OutputEvent (..), OutputFlags (..), OutputMode (..), Sink (..), eventJson, nullSink, withRenderer)
import Zinc.Docker (dockerfileText)
import Zinc.Fmt (canonicalizeManifest)
import Zinc.Doctor (doctorJson, doctorOk, flakesOffDiagnostic, lockDriftDiagnostic, renderDoctor, runDoctor)
import Zinc.Introspect (DepStatus (..), explainJson, graphJson, statusJson)
import Zinc.Prime (onboardText, primeText)
import Zinc.Json (Json (..), parseJson, renderJson)
import Zinc.Git (cloneAt, gitEnv, listTags, splitRepoSubdir)
import Zinc.Hackage (hackageCabalUrl, sourceRepoOf)
import Zinc.Store (contentHash, resolveStoreRoot, storeSrcPath, verifyContent, withStoreLock)
import Zinc.Manifest
  ( Component (..)
  , ComponentKind (..)
  , Dependency (..)
  , MemberManifest (..)
  , Ref (..)
  , WorkspaceManifest (..)
  , addDep
  , depRepos
  , depGhcOptionsOf
  , parseDependencies
  , parseMember
  , parseWorkspace
  , renderWorkspace
  )
import Zinc.Fetch (gitFetchManifest)
import Zinc.GC (GCRoot (..), gcStore, runGc)
import Zinc.Add (enrichWithRepos, freezeClosure, lockEntry, runAdd, runUpdate)
import Zinc.Build (GhcInvocation (..), MemberBuild (..), PackageConf (..), archiveArgs, buildMember, ghcMakeArgs, installedVersions, preprocessorFor, registerPackage, renderConf, replArgs, runPreprocessor, writeFileIfChanged)
import Zinc.Cache (BuildKey (..), buildCacheKey, cacheHit, storeConfPath, storePkgPath, writeCachedConf)
import Zinc.Cabal (cabalBuildType, cabalVersion, parseCabalComponents, parseCabalComponentsForGhc)
import Zinc.Env (envCacheKey, nixPrintDevEnv, provisionEnv)
import Zinc.Macros (emitCabalMacros)
import Zinc.Nix (generateFlake)
import Zinc.Orchestrate (buildAndRun, lockDrift, orderMembers, parMapBounded, resolveTarget, runBuild, runBuildMember, runClean, runTests, runWarm)
import Zinc.Paths (pathsModuleName, synthesizePaths)
import Zinc.Report (BuildOutcome (..), CacheStats (..), PackageReport (..), PackageStatus (..), Timing (..), buildDataJson, cacheStatsOf, packageReportJson, renderResolution, statusText, timingJson)
import Zinc.SysLibs (toNixpkgs)
import Zinc.Resolve (DepManifest (..), ResolvedDep (..), isBootLib, resolve, topoLevels, topoSort)
import Zinc.Version (newestTag)
import Zinc.Lock (LockedPackage (..), parseLock, renderLock)
import Zinc.Metrics (MetricsRecord (..), appendMetrics, metricsLine, metricsPath)
import Zinc.Perf (CommandStats (..), PerfRecord (..), Regression (..), PerfSummary (..), decodeRecord, percentile, perfSummaryJson, renderPerf, summarize)
import Zinc.Scaffold (FileSpec (..), materialize, scaffoldNew)

-- | Body of the generated file at the given path, if present.
bodyOf :: FilePath -> [FileSpec] -> Maybe String
bodyOf p = fmap specBody . find ((== p) . specPath)

trimStr :: String -> String
trimStr = f . f where f = reverse . dropWhile isSpace

-- | Write a file, creating parent directories first.
writeFileIn :: FilePath -> String -> IO ()
writeFileIn path content = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path content

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
        , "[dependencies.aeson]"
        , "tag = \"v2\""
        , "repo = \"r/aeson\""
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
    (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "leaf" (Tag "v1") (Just leaf) []]))
  pure (wsFile, baseD ++ "/store", leaf)

-- | A throwaway store shared by the build end-to-end tests, kept out of the
-- real @~\/.zinc@ via the @ZINC_STORE@ override.
testStoreDir :: FilePath
testStoreDir = "/tmp/zinc-test-store"

main :: IO ()
main = hspec $ do
  describe "diagnostic core (rdy.1)" $ do
    it "maps each error to its stable taxonomy code" $ do
      errorCode (DepNoGitRepo "colour") `shouldBe` "ZINC_DEP_NO_GIT_REPO"
      errorCode NixAbsent `shouldBe` "ZINC_NIX_ABSENT"
      errorCode (ContentHashMismatch "p" "a" "b") `shouldBe` "ZINC_CONTENT_HASH_MISMATCH"

    it "toDiagnostic carries code, package, and an actionable nextAction" $ do
      let d = toDiagnostic (DepNoGitRepo "colour")
      diagCode d `shouldBe` "ZINC_DEP_NO_GIT_REPO"
      diagPackage d `shouldBe` Just "colour"
      diagNextAction d `shouldSatisfy` isJust

    it "renders a Diagnostic to JSON, omitting absent optional fields" $
      renderJson (diagnosticJson (toDiagnostic (ManifestParse "f.toml" "bad")))
        `shouldBe` "{\"code\":\"ZINC_MANIFEST_PARSE\",\"severity\":\"error\",\"title\":\"manifest parse error\",\"detail\":\"bad\",\"location\":\"f.toml\",\"nextAction\":\"fix the TOML in the manifest\"}"

    it "wraps output in the standard envelope (timing omitted when absent)" $
      renderJson (envelope "build" True (Just (JObject [("built", JInt 1)])) Nothing [])
        `shouldBe` "{\"zinc\":\"0.1.0.0\",\"command\":\"build\",\"ok\":true,\"data\":{\"built\":1},\"diagnostics\":[]}"

    it "renders the toolchain-missing guidance (gtv.2 preflight)" $ do
      let d = toDiagnostic (ToolchainMissing "ghc")
      diagCode d `shouldBe` "ZINC_TOOLCHAIN_MISSING"
      exitCodeFor (ToolchainMissing "ghc") `shouldBe` ExitFailure 5
      diagNextAction d `shouldSatisfy` maybe False (isInfixOf "nix develop")

    it "assigns stable exit codes per category" $ do
      exitCodeFor (NoZincToml ".") `shouldBe` ExitFailure 2
      exitCodeFor (DepNoGitRepo "colour") `shouldBe` ExitFailure 3
      exitCodeFor (GhcCompile "p" "boom") `shouldBe` ExitFailure 4
      exitCodeFor NixAbsent `shouldBe` ExitFailure 5
      exitCodeFor (ContentHashMismatch "p" "a" "b") `shouldBe` ExitFailure 6

    it "escapes JSON strings" $
      renderJson (JString "a\"b\nc") `shouldBe` "\"a\\\"b\\nc\""

  describe "build report (rdy.2)" $ do
    it "maps each status to its stable wire string" $
      map statusText [Cached, Built, Skipped, Failed]
        `shouldBe` ["cached", "built", "skipped", "failed"]

    it "renders a per-package report as JSON" $
      renderJson (packageReportJson (PackageReport "colour" "a1b2c3" Cached Nothing))
        `shouldBe` "{\"name\":\"colour\",\"ref\":\"a1b2c3\",\"status\":\"cached\"}"

    it "includes timeMs in a per-package report when measured" $
      renderJson (packageReportJson (PackageReport "colour" "a1b2c3" Built (Just 1234)))
        `shouldBe` "{\"name\":\"colour\",\"ref\":\"a1b2c3\",\"status\":\"built\",\"timeMs\":1234}"

    it "renders the build data block (executables + packages)" $
      renderJson (buildDataJson (BuildOutcome ["/w/.zinc/build/app"] [PackageReport "colour" "a1b2c3" Built Nothing]))
        `shouldBe` "{\"executables\":[\"/w/.zinc/build/app\"],\"packages\":[{\"name\":\"colour\",\"ref\":\"a1b2c3\",\"status\":\"built\"}]}"

    it "wraps a build outcome in the standard envelope" $
      renderJson (envelope "build" True (Just (buildDataJson (BuildOutcome [] []))) Nothing [])
        `shouldBe` "{\"zinc\":\"0.1.0.0\",\"command\":\"build\",\"ok\":true,\"data\":{\"executables\":[],\"packages\":[]},\"diagnostics\":[]}"

    it "build in a non-workspace dir fails structurally (ZINC_NO_ZINC_TOML), not a crash" $ do
      let d = "/tmp/zinc-test-noworkspace"
      createDirectoryIfMissing True d
      r <- runBuild d
      either errorCode (const "built") r `shouldBe` "ZINC_NO_ZINC_TOML"

  describe "command timing (hbv.1)" $ do
    it "derives cache stats from per-package statuses" $ do
      let pkgs = [PackageReport "a" "r" Cached Nothing, PackageReport "b" "r" Built Nothing, PackageReport "c" "r" Cached Nothing, PackageReport "d" "r" Skipped Nothing]
          c = cacheStatsOf pkgs
      (csHits c, csMisses c, csPkgsBuilt c, csPkgsCached c) `shouldBe` (2, 1, 1, 2)

    it "renders the timing block (totalMs, phases, cache)" $
      renderJson (timingJson (Timing 1234 [("closure", 900), ("member", 300)] (CacheStats 2 1 1 2)))
        `shouldBe` "{\"totalMs\":1234,\"phases\":{\"closure\":900,\"member\":300},\"cache\":{\"hits\":2,\"misses\":1,\"pkgsBuilt\":1,\"pkgsCached\":2}}"

    it "includes the timing block in the envelope when present" $
      renderJson (envelope "build" True (Just (buildDataJson (BuildOutcome [] []))) (Just (timingJson (Timing 5 [] (CacheStats 0 0 0 0)))) [])
        `shouldBe` "{\"zinc\":\"0.1.0.0\",\"command\":\"build\",\"ok\":true,\"data\":{\"executables\":[],\"packages\":[]},\"timing\":{\"totalMs\":5,\"phases\":{},\"cache\":{\"hits\":0,\"misses\":0,\"pkgsBuilt\":0,\"pkgsCached\":0}},\"diagnostics\":[]}"

  describe "Zinc.Json parser (hbv.3)" $ do
    it "round-trips every rendered value shape" $ do
      let samples =
            [ JNull, JBool True, JBool False, JInt 0, JInt (-7)
            , JString "hi\n\"x\"\t/", JArray [JInt 1, JString "a", JBool False]
            , JObject [("k", JInt 5), ("nested", JObject [("a", JArray [])])]
            ]
      map (parseJson . renderJson) samples `shouldBe` map Right samples

    it "parses a nested metrics-shaped object (whitespace-tolerant)" $
      parseJson "{ \"totalMs\": 7, \"phases\": {\"closure\": 4}, \"ok\": true }"
        `shouldBe` Right (JObject [("totalMs", JInt 7), ("phases", JObject [("closure", JInt 4)]), ("ok", JBool True)])

    it "rejects malformed input" $ do
      parseJson "{" `shouldSatisfy` isLeft
      parseJson "[1,2" `shouldSatisfy` isLeft
      parseJson "tru" `shouldSatisfy` isLeft
      parseJson "{\"k\" 1}" `shouldSatisfy` isLeft

  describe "run target selection (tci.1)" $ do
    it "selects the sole executable when no target is given" $
      resolveTarget Nothing [("app", "/w/app")] `shouldBe` Right "/w/app"

    it "errors when there is no executable" $
      either errorCode (const "ok") (resolveTarget Nothing []) `shouldBe` "ZINC_ERROR"

    it "is ambiguous (lists candidates) when many exes and no target" $
      either errorCode (const "ok") (resolveTarget Nothing [("a", "/a"), ("b", "/b")]) `shouldBe` "ZINC_AMBIGUOUS_TARGET"

    it "selects a named target" $
      resolveTarget (Just "b") [("a", "/a"), ("b", "/b")] `shouldBe` Right "/b"

    it "accepts a member:exe qualifier (matches the exe part)" $
      resolveTarget (Just "mylib:b") [("a", "/a"), ("b", "/b")] `shouldBe` Right "/b"

    it "lists candidates for an unknown target" $
      either errorCode (const "ok") (resolveTarget (Just "ghost") [("a", "/a")]) `shouldBe` "ZINC_AMBIGUOUS_TARGET"

  describe "output foundation (hw6.1)" $ do
    it "renders an event as a tagged JSONL object (stable field order)" $ do
      renderJson (eventJson (CompileDone "aeson" 410 False))
        `shouldBe` "{\"event\":\"compile-done\",\"package\":\"aeson\",\"timeMs\":410,\"cached\":false}"
      renderJson (eventJson ResolveStart) `shouldBe` "{\"event\":\"resolve-start\"}"

    it "Sink fans out to every consumer (Monoid) and nullSink drops events" $ do
      r1 <- newTVarIO []
      r2 <- newTVarIO []
      let rec ref = Sink (\e -> modifyTVar' ref (e :))
          s = rec r1 <> nullSink <> rec r2
      atomically (runSink s (CompileDone "aeson" 5 False))
      atomically (runSink s ResolveStart)
      v1 <- readTVarIO r1
      v2 <- readTVarIO r2
      (reverse v1, reverse v2)
        `shouldBe` ([CompileDone "aeson" 5 False, ResolveStart], [CompileDone "aeson" 5 False, ResolveStart])

    it "withRenderer runs the body, drains events, and returns without hanging" $ do
      r <- withRenderer (Human False True) $ \sink -> do
        atomically (runSink sink (CompileStart "x"))
        atomically (runSink sink (CompileDone "x" 1 True))
        pure (42 :: Int)
      r `shouldBe` 42

    it "OutputFlags has the expected shape" $
      (ofJson (OutputFlags True False), ofQuiet (OutputFlags True False)) `shouldBe` (True, False)

  describe "closure discovery (49o)" $ do
    it "parses the `closure` subcommand (+ --json)" $ do
      parseArgs ["closure", "aeson"] `shouldBe` Right (OutputFlags False False, Closure "aeson")
      parseArgs ["closure", "aeson", "--json"] `shouldBe` Right (OutputFlags True False, Closure "aeson")

    it "extracts the package name from an installed unit-id" $ do
      pkgNameOf "aeson-2.2.3.0-abc123" `shouldBe` "aeson"
      pkgNameOf "data-default-class-0.1.2.0" `shouldBe` "data-default-class"
      pkgNameOf "base-4.18.2.1" `shouldBe` "base"
      pkgNameOf "rts" `shouldBe` "rts"

    it "parses ghc-pkg `depends` output (multi-line) into unit-ids" $
      parseDependsField "depends: array-0.5.6.0 base-4.18.2.1\n         bytestring-0.11.5.3"
        `shouldBe` ["array-0.5.6.0", "base-4.18.2.1", "bytestring-0.11.5.3"]

    it "partitions a closure into discovered repos and needs-vendoring" $ do
      let discover n = pure (if n == "colour" then Nothing else Just ("https://example/" ++ n))
      (found, missing) <- discoverRepos discover ["aeson", "colour", "scientific"]
      found `shouldBe` [("aeson", "https://example/aeson"), ("scientific", "https://example/scientific")]
      missing `shouldBe` ["colour"]

    it "enrichWithRepos pins discovered repos but a hand-supplied override wins" $ do
      let ws0 = WorkspaceManifest [] "9.6.5" [Dependency "aeson" (Tag "v2") Nothing [], Dependency "pp" Latest (Just "r/mono#pp") []]
          ws1 = enrichWithRepos ws0 [("aeson", "r/aeson"), ("pp", "r/mono"), ("scientific", "r/sci")]
      wsDependencies ws1
        `shouldBe` [ Dependency "aeson" (Tag "v2") (Just "r/aeson") [] -- discovered (no prior repo)
                   , Dependency "pp" Latest (Just "r/mono#pp") []      -- override kept (not clobbered by "r/mono")
                   , Dependency "scientific" Latest (Just "r/sci") []  -- discovered
                   ]

  describe "zinc fmt (8n6.3)" $ do
    it "parses fmt and fmt --check" $ do
      parseArgs ["fmt"] `shouldBe` Right (OutputFlags False False, Fmt False)
      parseArgs ["fmt", "--check"] `shouldBe` Right (OutputFlags False False, Fmt True)

    it "canonicalizes deps (sorted, shorthand) and is idempotent" $ do
      let src = unlines ["[workspace]", "members = []", "ghc = \"9.6.5\"", "[dependencies]", "zebra = { tag = \"v2\" }", "alpha = \"*\""]
          out1 = either error id (canonicalizeManifest src)
          out2 = either error id (canonicalizeManifest out1)
      out1 `shouldBe` out2
      all (`isInfixOf` out1) ["alpha = \"*\"", "zebra = \"v2\""] `shouldBe` True

    it "preserves [package]/[build.*] while rewriting deps" $ do
      let src = unlines ["[workspace]", "members = [\".\"]", "ghc = \"9.6.5\"", "[package]", "name = \"z\"", "[build.lib]", "source-dirs = [\"src\"]", "[dependencies]", "x = \"v1\""]
          out = either error id (canonicalizeManifest src)
      all (`isInfixOf` out) ["[package]", "[build.lib]", "source-dirs = [\"src\"]", "x = \"v1\""] `shouldBe` True

  describe "dockerfile recipe (vwn.2)" $ do
    it "parses the `dockerfile` subcommand" $
      parseArgs ["dockerfile"] `shouldBe` Right (OutputFlags False False, Dockerfile)

    it "emits the multi-stage closure-cached recipe" $ do
      let t = dockerfileText "9.6.5"
      all (`isInfixOf` t)
        [ "GHC 9.6.5"
        , "ZINC_STORE"
        , "COPY flake.nix flake.lock* zinc.toml zinc.lock"
        , "zinc build --deps-only"
        , "--mount=type=cache,target=/zinc-store"
        , "zinc build"
        ]
        `shouldBe` True

  describe "warm / build --deps-only (vwn.1)" $ do
    it "parses warm and build --deps-only to the same closure-only command" $ do
      parseArgs ["warm"] `shouldBe` Right (OutputFlags False False, Warm)
      parseArgs ["warm", "--json"] `shouldBe` Right (OutputFlags True False, Warm)
      parseArgs ["build", "--deps-only"] `shouldBe` Right (OutputFlags False False, Warm)
      parseArgs ["build", "--deps-only", "--json"] `shouldBe` Right (OutputFlags True False, Warm)

    it "builds the closure only (empty for a depless workspace)" $ do
      let d = "/tmp/zinc-warm-test"
      createDirectoryIfMissing True d
      writeFileIn (d </> "zinc.toml") (renderWorkspace (WorkspaceManifest [] "9.6.5" []))
      r <- runWarm d
      r `shouldBe` Right []

    it "fails with NoZincToml outside a workspace" $ do
      let d = "/tmp/zinc-warm-nows"
      createDirectoryIfMissing True d
      r <- runWarm d
      either errorCode (const "ok") r `shouldBe` "ZINC_NO_ZINC_TOML"

  describe "writeFileIfChanged (zinc-k2i regression)" $ do
    it "overwrites changed content without locking the file" $ do
      -- Regression: lazy readFile left the read handle open when (==)
      -- short-circuited on content that differs at byte 0, so the rewrite hit
      -- "resource busy (file is locked)". Strict readFile' closes it first.
      let dir = "/tmp/zinc-wfic-test"
          p = dir </> "f.txt"
      createDirectoryIfMissing True dir
      _ <- writeFileIfChanged p "original content goes here"
      wrote <- writeFileIfChanged p "totally different content" -- differs at byte 0
      contents <- readFile p
      (wrote, contents) `shouldBe` (True, "totally different content")

    it "reports no write (and preserves the file) when content is unchanged" $ do
      let dir = "/tmp/zinc-wfic-test"
          p = dir </> "g.txt"
      createDirectoryIfMissing True dir
      _ <- writeFileIfChanged p "same"
      wrote <- writeFileIfChanged p "same"
      wrote `shouldBe` False

  describe "concurrency-safe store (rdy.8)" $ do
    it "runs the action and releases the per-key lock afterward" $ do
      let root = "/tmp/zinc-lock-test1"
      createDirectoryIfMissing True root
      r <- withStoreLock root "k1" (pure (42 :: Int))
      held <- doesDirectoryExist (root </> "locks" </> "k1")
      (r, held) `shouldBe` (42, False)

    it "releases the lock even when the action throws" $ do
      let root = "/tmp/zinc-lock-test2"
      createDirectoryIfMissing True root
      _ <- (try (withStoreLock root "k" (ioError (userError "boom"))) :: IO (Either IOException ()))
      doesDirectoryExist (root </> "locks" </> "k") `shouldReturn` False

    it "serializes concurrent critical sections (no lost updates)" $ do
      let root = "/tmp/zinc-lock-test3"
          n = 12
      createDirectoryIfMissing True root
      ref <- newIORef (0 :: Int)
      done <- newEmptyMVar
      forM_ [1 .. n] $ \_ ->
        forkIO $ do
          withStoreLock root "shared" $ do
            v <- readIORef ref
            threadDelay 1000 -- 1ms: without the lock this read/write interleaves and loses updates
            writeIORef ref (v + 1)
          putMVar done ()
      forM_ [1 .. n] (const (takeMVar done))
      readIORef ref `shouldReturn` n

  describe "context priming (rdy.5)" $ do
    it "parses prime / onboard" $ do
      parseArgs ["prime"] `shouldBe` Right (OutputFlags False False, Prime)
      parseArgs ["onboard"] `shouldBe` Right (OutputFlags False False, Onboard)

    it "prime reflects toolchain, members, and the no-cabal gotcha" $ do
      let t = primeText (WorkspaceManifest ["packages/app"] "9.6.5" [])
      all (`isInfixOf` t) ["GHC 9.6.5", "packages/app", "zinc build", "Do NOT use cabal"] `shouldBe` True

    it "onboard is a paste-ready AGENTS.md snippet" $ do
      let t = onboardText (WorkspaceManifest [] "9.6.5" [])
      all (`isInfixOf` t) ["## Building (zinc)", "zinc build", "9.6.5"] `shouldBe` True

  describe "introspection (rdy.4)" $ do
    it "parses status / graph / explain (+ --json)" $ do
      parseArgs ["status"] `shouldBe` Right (OutputFlags False False, Status)
      parseArgs ["graph", "--json"] `shouldBe` Right (OutputFlags True False, Graph)
      parseArgs ["explain", "aeson"] `shouldBe` Right (OutputFlags False False, Explain "aeson")
      parseArgs ["explain", "aeson", "--json"] `shouldBe` Right (OutputFlags True False, Explain "aeson")

    it "renders status as JSON" $
      renderJson (statusJson "9.6.5" ["packages/app"] [DepStatus "colour" "abc1234" True] ["aeson"])
        `shouldBe` "{\"ghc\":\"9.6.5\",\"members\":[\"packages/app\"],\"dependencies\":[{\"name\":\"colour\",\"ref\":\"abc1234\",\"cached\":true}],\"drift\":[\"aeson\"]}"

    it "renders the closure graph: nodes, edges, topo levels" $ do
      let locks = [LockedPackage "a" "r/a" "ra" "sha256:x" ["b"], LockedPackage "b" "r/b" "rb" "sha256:y" []]
      renderJson (graphJson locks)
        `shouldBe` "{\"nodes\":[\"a\",\"b\"],\"edges\":[{\"from\":\"a\",\"to\":\"b\"}],\"levels\":[[\"b\"],[\"a\"]]}"

    it "explains a package's provenance (who requires it, at which rev)" $ do
      let locks = [LockedPackage "a" "r/a" "ra" "sha256:x" ["b"], LockedPackage "b" "r/b" "rb" "sha256:y" []]
      renderJson (explainJson "b" locks)
        `shouldBe` "{\"package\":\"b\",\"inClosure\":true,\"ref\":\"rb\",\"requiredBy\":[\"a\"]}"

    it "explains a package outside the closure" $
      renderJson (explainJson "ghost" [])
        `shouldBe` "{\"package\":\"ghost\",\"inClosure\":false,\"ref\":null,\"requiredBy\":[]}"

  describe "doctor (rdy.6)" $ do
    it "parses the `doctor` subcommand (+ --json)" $ do
      parseArgs ["doctor"] `shouldBe` Right (OutputFlags False False, Doctor)
      parseArgs ["doctor", "--json"] `shouldBe` Right (OutputFlags True False, Doctor)

    it "reports lock drift as a warning with a nextAction" $ do
      lockDriftDiagnostic [] `shouldBe` Nothing
      let d = maybe (error "expected drift") id (lockDriftDiagnostic ["aeson", "text"])
      (diagCode d, diagSeverity d) `shouldBe` ("ZINC_LOCK_DRIFT", SWarning)
      diagNextAction d `shouldSatisfy` isJust

    it "treats warnings as ok, error-severity findings as not-ok" $ do
      doctorOk [flakesOffDiagnostic] `shouldBe` True
      doctorOk [toDiagnostic (NoZincToml ".")] `shouldBe` False

    it "renders a clean bill of health" $
      renderDoctor [] `shouldSatisfy` isInfixOf "No problems found"

    it "emits the doctor envelope with ok reflecting health" $
      renderJson (doctorJson []) `shouldSatisfy` isInfixOf "\"command\":\"doctor\",\"ok\":true"

    it "flags a missing workspace as a NoZincToml error (not-ok)" $ do
      let d = "/tmp/zinc-doctor-nows"
      createDirectoryIfMissing True d
      diags <- runDoctor d
      any ((== "ZINC_NO_ZINC_TOML") . diagCode) diags `shouldBe` True
      doctorOk diags `shouldBe` False

  describe "perf analyzer (hbv.3)" $ do
    it "parses the `perf` subcommand (+ --json)" $ do
      parseArgs ["perf"] `shouldBe` Right (OutputFlags False False, Perf)
      parseArgs ["perf", "--json"] `shouldBe` Right (OutputFlags True False, Perf)

    it "decodes a metrics record's analyzer-relevant fields" $ do
      let j = either (error "parse") id (parseJson "{\"command\":\"build\",\"timing\":{\"totalMs\":42,\"cache\":{\"hits\":3,\"misses\":1}}}")
      decodeRecord j `shouldBe` Just (PerfRecord "build" 42 3 1 [])

    it "computes nearest-rank percentiles" $ do
      percentile 50 [100, 110, 300] `shouldBe` 110
      percentile 95 [100, 110, 300] `shouldBe` 300
      percentile 50 ([] :: [Int]) `shouldBe` 0

    it "summarizes latency, cache, and a regression vs the prior median" $ do
      let s = summarize [PerfRecord "build" 100 1 0 [], PerfRecord "build" 110 2 1 [], PerfRecord "build" 300 0 1 []]
      sumRecords s `shouldBe` 3
      (sumCacheHits s, sumCacheMiss s) `shouldBe` (3, 2)
      map (\c -> (csCommand c, csCount c, csP50Ms c, csP95Ms c)) (sumCommands s) `shouldBe` [("build", 3, 110, 300)]
      sumRegression s `shouldBe` Just (Regression "build" 100 300 True)

    it "renders empty history gracefully" $
      renderPerf (summarize []) `shouldSatisfy` isInfixOf "No build metrics yet"

    it "emits a JSON summary" $
      renderJson (perfSummaryJson (summarize []))
        `shouldBe` "{\"records\":0,\"commands\":[],\"cache\":{\"hits\":0,\"misses\":0,\"hitRatePct\":0},\"regression\":null,\"slowestDeps\":[]}"

    it "ranks slowest dependencies by cumulative build time across records (nti)" $ do
      let recs =
            [ PerfRecord "build" 100 0 1 [("alpha", 900), ("beta", 100)]
            , PerfRecord "build" 50 1 0 [("alpha", 0), ("beta", 100)]
            ]
      sumSlowest (summarize recs) `shouldBe` [("alpha", 900, 2), ("beta", 200, 2)]

  describe "metrics persistence (hbv.2)" $ do
    it "renders a metrics record as one JSON line (packages omitted when empty)" $
      metricsLine (MetricsRecord "build" "" "sha256:abc" "9.6.5" "2026-06-04T00:00:00Z" (Timing 7 [("closure", 4)] (CacheStats 1 0 0 1)) [])
        `shouldBe` "{\"timestamp\":\"2026-06-04T00:00:00Z\",\"command\":\"build\",\"argsSummary\":\"\",\"lockHash\":\"sha256:abc\",\"ghcVersion\":\"9.6.5\",\"timing\":{\"totalMs\":7,\"phases\":{\"closure\":4},\"cache\":{\"hits\":1,\"misses\":0,\"pkgsBuilt\":0,\"pkgsCached\":1}}}\n"

    it "includes per-package timing in the record when present (nti)" $
      metricsLine (MetricsRecord "build" "" "h" "9.6.5" "t" (Timing 5 [] (CacheStats 0 1 1 0)) [("alpha", 900)])
        `shouldBe` "{\"timestamp\":\"t\",\"command\":\"build\",\"argsSummary\":\"\",\"lockHash\":\"h\",\"ghcVersion\":\"9.6.5\",\"timing\":{\"totalMs\":5,\"phases\":{},\"cache\":{\"hits\":0,\"misses\":1,\"pkgsBuilt\":1,\"pkgsCached\":0}},\"packages\":[{\"name\":\"alpha\",\"timeMs\":900}]}\n"

    it "appends (never rewrites) records to .zinc/metrics.jsonl" $ do
      let d = "/tmp/zinc-metrics-test"
          rec n = MetricsRecord "build" n "h" "9.6.5" "t" (Timing 1 [] (CacheStats 0 0 0 0)) []
      stale <- doesDirectoryExist d
      when stale $ removeDirectoryRecursive d
      createDirectoryIfMissing True d
      appendMetrics d (rec "one")
      appendMetrics d (rec "two")
      ls <- lines <$> readFile (metricsPath d)
      length ls `shouldBe` 2

  describe "non-interactive contract (rdy.7)" $ do
    it "accepts (and ignores) --yes on add: zinc never prompts" $ do
      parseArgs ["add", "--yes", "aeson"] `shouldBe` Right (OutputFlags False False, Add "aeson")
      parseArgs ["add", "-y", "aeson"] `shouldBe` Right (OutputFlags False False, Add "aeson")

    it "gitEnv sets the non-interactive guards so git can't block on a tty" $ do
      let e = gitEnv [("PATH", "/usr/bin"), ("GIT_TERMINAL_PROMPT", "1")]
      lookup "GIT_TERMINAL_PROMPT" e `shouldBe` Just "0"
      lookup "GIT_SSH_COMMAND" e `shouldBe` Just "ssh -o BatchMode=yes"

    it "gitEnv preserves the ambient environment (e.g. PATH) without duplicating overrides" $ do
      let e = gitEnv [("PATH", "/usr/bin"), ("GIT_TERMINAL_PROMPT", "1")]
      lookup "PATH" e `shouldBe` Just "/usr/bin"
      length (filter ((== "GIT_TERMINAL_PROMPT") . fst) e) `shouldBe` 1

  -- Isolate every build end-to-end test from the real ~/.zinc by pointing the
  -- shared store at a throwaway dir (exercises the ZINC_STORE override).
  runIO $ do
    stale <- doesDirectoryExist testStoreDir
    when stale $ removeDirectoryRecursive testStoreDir
    setEnv "ZINC_STORE" testStoreDir
  describe "parseArgs" $ do
    it "parses the `build` subcommand" $
      parseArgs ["build"] `shouldBe` Right (OutputFlags False False, Build Nothing)

    it "parses `build <member>` with a target" $
      parseArgs ["build", "mylib"] `shouldBe` Right (OutputFlags False False, Build (Just "mylib"))

    it "parses `build --json` (machine surface)" $ do
      parseArgs ["build", "--json"] `shouldBe` Right (OutputFlags True False, Build Nothing)
      parseArgs ["build", "mylib", "--json"] `shouldBe` Right (OutputFlags True False, Build (Just "mylib"))

    it "parses `new <name>` with its argument" $
      parseArgs ["new", "myapp"] `shouldBe` Right (OutputFlags False False, New "myapp")

    it "parses `add <pkg>` with its argument" $
      parseArgs ["add", "aeson"] `shouldBe` Right (OutputFlags False False, Add "aeson")

    it "parses `clean`" $
      parseArgs ["clean"] `shouldBe` Right (OutputFlags False False, Clean)

    it "parses `gc`" $
      parseArgs ["gc"] `shouldBe` Right (OutputFlags False False, Gc)

    it "parses `repl` with no target" $
      parseArgs ["repl"] `shouldBe` Right (OutputFlags False False, Repl Nothing)

    it "parses `repl <target>`" $
      parseArgs ["repl", "mylib"] `shouldBe` Right (OutputFlags False False, Repl (Just "mylib"))

    it "parses `test` with no target" $
      parseArgs ["test"] `shouldBe` Right (OutputFlags False False, Test Nothing)

    it "parses `update` with no package" $
      parseArgs ["update"] `shouldBe` Right (OutputFlags False False, Update Nothing)

    it "parses `run [TARGET] [-- ARGS]` (target first, then program args)" $ do
      parseArgs ["run"] `shouldBe` Right (OutputFlags False False, Run Nothing [])
      parseArgs ["run", "web"] `shouldBe` Right (OutputFlags False False, Run (Just "web") [])
      parseArgs ["run", "web", "--", "a", "b"] `shouldBe` Right (OutputFlags False False, Run (Just "web") ["a", "b"])

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
            , "hspec = \"*\""
            , ""
            , "[dependencies.aeson]"
            , "tag = \"v2.2.3.0\""
            , "repo = \"https://github.com/haskell/aeson\""
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

    it "derives a dependency's repo from its `repo` key" $
      ((lookup "aeson" . depRepos) <$> parsed)
        `shouldBe` Right (Just "https://github.com/haskell/aeson")

    it "fails on a missing [workspace] table" $
      parseWorkspace "[dependencies]\n" `shouldSatisfy` isLeft

  describe "dependency ghc-options (vertical schema)" $ do
    it "reads per-dependency extra ghc flags from each dep's ghc-options" $ do
      let src = unlines ["[workspace]", "members = []", "ghc = \"9.6.5\"", "[dependencies.colour]", "tag = \"v1\"", "ghc-options = [\"-XSafe\"]"]
      (sort . depGhcOptionsOf <$> parseWorkspace src) `shouldBe` Right [("colour", ["-XSafe"])]

    it "is empty when no dependency sets ghc-options" $
      (depGhcOptionsOf <$> parseWorkspace "[workspace]\nmembers = []\nghc = \"9.6.5\"\n") `shouldBe` Right []

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
            , "modules = [\"Myapp\", \"Myapp.Core\", \"Myapp.Internal\"]"
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
            , compModules = ["Myapp", "Myapp.Core", "Myapp.Internal"]
            , compMain = Nothing
            , compExtensions = ["OverloadedStrings"]
            , compGhcOptions = ["-Wall"]
            , compDepends = ["aeson"]
            , compSystemLibs = ["zlib"]
            , compIncludeDirs = []
            , compCppOptions = []
            , compCSources = []
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
    let dep n r = Dependency n r Nothing []
        boot = (`elem` ["base", "text", "bytestring", "containers"])
        fetchFrom fix n _ _ = pure (maybe (Left (OtherError ("missing: " ++ n))) Right (lookup n fix))
        run fix deps reg =
          runIdentity (resolve boot (fetchFrom fix) deps reg)
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

    it "falls back to the root registry for a transitive dep's repo" $ do
      -- a real upstream 'a' declares dep 'b' but carries no registry of its own;
      -- b's repo is supplied by the root workspace registry.
      let fix =
            [ ("a", DepManifest [dep "b" Latest] [])
            , ("b", DepManifest [] [])
            ]
          r = run fix [dep "a" Latest] [("a", "r/a"), ("b", "r/b")]
      (sort . map rdName <$> r) `shouldBe` Right ["a", "b"]

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

  describe "topoLevels" $ do
    let rd n ds = ResolvedDep n ("r/" ++ n) Latest ds

    it "puts each node a level after all its in-closure deps (linear chain)" $
      (map (map rdName) <$> topoLevels [rd "aeson" ["scientific"], rd "scientific" ["il"], rd "il" []])
        `shouldBe` Right [["il"], ["scientific"], ["aeson"]]

    it "groups mutually-independent siblings into one level" $
      (map (map rdName) <$> topoLevels [rd "a" ["b", "c"], rd "b" ["d"], rd "c" ["d"], rd "d" []])
        `shouldBe` Right [["d"], ["b", "c"], ["a"]]

    it "puts every root dep at level 0 when there are no edges" $
      (map (map rdName) <$> topoLevels [rd "x" [], rd "y" [], rd "z" []])
        `shouldBe` Right [["x", "y", "z"]]

    it "rejects a dependency cycle" $
      topoLevels [rd "a" ["b"], rd "b" ["a"]] `shouldSatisfy` isLeft

    it "flattening the levels yields a valid topo order" $
      (concatMap (map rdName) <$> topoLevels [rd "a" ["b", "c"], rd "b" ["d"], rd "c" ["d"], rd "d" []])
        `shouldBe` (map rdName <$> topoSort [rd "a" ["b", "c"], rd "b" ["d"], rd "c" ["d"], rd "d" []])

    it "handles an empty closure" $
      topoLevels [] `shouldBe` Right []

  describe "parMapBounded (bounded-concurrency map)" $ do
    it "returns results in input order regardless of bound" $ do
      r <- parMapBounded 4 (\x -> pure (Right (x * 2 :: Int))) [1 .. 10 :: Int]
      r `shouldBe` map (Right . (* 2)) [1 .. 10]

    it "runs every task even when the bound is 1" $ do
      r <- parMapBounded 1 (\x -> pure (Right x)) [1 .. 5 :: Int]
      r `shouldBe` map Right [1 .. 5 :: Int]

    it "surfaces a task's Left without dropping the others" $ do
      r <- parMapBounded 3 (\x -> pure (if even x then Left (OtherError ("bad " ++ show x)) else Right x)) [1 .. 4 :: Int]
      r `shouldBe` [Right 1, Left (OtherError "bad 2"), Right 3, Left (OtherError "bad 4")]

  describe "parseDependencies" $ do
    it "reads vertical [dependencies.<name>] (ref + repo) without requiring [workspace]" $
      parseDependencies
        ( unlines
            [ "[package]"
            , "name = \"foo\""
            , "version = \"1\""
            , "[dependencies.aeson]"
            , "tag = \"v2\""
            , "repo = \"r/aeson\""
            , "[dependencies.scientific]"
            , "repo = \"r/sci\""
            ]
        )
        `shouldBe` Right
          ( [Dependency "aeson" (Tag "v2") (Just "r/aeson") [], Dependency "scientific" Latest (Just "r/sci") []]
          , [("aeson", "r/aeson"), ("scientific", "r/sci")]
          )

    it "defaults to empty when the sections are absent" $
      parseDependencies "[package]\nname = \"x\"\nversion = \"1\"" `shouldBe` Right ([], [])

  describe "gitFetchManifest" $ do
    repo <- runIO setupDepRepo

    it "clones a dep at a ref and parses its manifest" $ do
      r <- gitFetchManifest "/tmp/zinc-fetch-store" "9.6.5" "dep" repo (Tag "v1")
      r `shouldBe` Right (DepManifest [Dependency "aeson" (Tag "v2") (Just "r/aeson") []] [("aeson", "r/aeson")])

    it "resolves a Latest ref to the newest tag and parses its manifest" $ do
      r <- gitFetchManifest "/tmp/zinc-fetch-store" "9.6.5" "dep" repo Latest
      r `shouldBe` Right (DepManifest [Dependency "aeson" (Tag "v2") (Just "r/aeson") []] [("aeson", "r/aeson")])

    it "derives deps from a .cabal when a real upstream has no zinc.toml" $ do
      let cabalRepo = "/tmp/zinc-fetch-cabal-dep"
      stale <- doesDirectoryExist cabalRepo
      when stale $ removeDirectoryRecursive cabalRepo
      writeFileIn (cabalRepo ++ "/up.cabal") (unlines ["cabal-version: 2.4", "name: up", "version: 1.0", "library", "  build-depends: base, containers, prettyprinter", "  exposed-modules: Up"])
      let git args = readProcess "git" ("-C" : cabalRepo : args) ""
      _ <- git ["init", "--quiet"]
      _ <- git ["config", "user.email", "t@e"]
      _ <- git ["config", "user.name", "T"]
      _ <- git ["add", "."]
      _ <- git ["commit", "--quiet", "-m", "up"]
      rev <- trimStr <$> git ["rev-parse", "HEAD"]
      r <- gitFetchManifest "/tmp/zinc-fetch-store" "9.6.5" "up" cabalRepo (Rev rev)
      -- repos come from the root registry, so the derived manifest carries none
      r `shouldBe` Right (DepManifest [Dependency "base" Latest Nothing [], Dependency "containers" Latest Nothing [], Dependency "prettyprinter" Latest Nothing []] [])

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
            , compModules = ["Demo", "Demo.Core", "Demo.Internal"]
            , compMain = Nothing
            , compExtensions = ["OverloadedStrings"]
            , compGhcOptions = ["-Wall"]
            , compDepends = ["aeson", "base"] -- finalizePD normalizes build-depends order
            , compSystemLibs = ["zlib"]
            , compIncludeDirs = []
            , compCppOptions = []
            , compCSources = []
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

    it "changes with the build options (per-dep overrides)" $
      (key ["base-4", "aeson-2"] ["-XSafe"] == k1) `shouldBe` False

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

  describe "workspace write-back (vertical schema)" $ do
    let ws =
          WorkspaceManifest
            { wsMembers = ["packages/a", "packages/b"]
            , wsGhc = "9.6.5"
            , wsDependencies =
                [ Dependency "aeson" (Tag "v2") (Just "r/aeson") []
                , Dependency "hspec" Latest (Just "r/hspec") ["-XSafe"]
                ]
            }

    it "renderWorkspace round-trips through parseWorkspace" $
      parseWorkspace (renderWorkspace ws) `shouldBe` Right ws

    it "renders a simple (ref-only) dependency as one-line shorthand" $ do
      let simple = WorkspaceManifest [] "9.6.5" [Dependency "text" (Tag "v2.1") Nothing []]
      ("text = \"v2.1\"" `isInfixOf` renderWorkspace simple) `shouldBe` True

    it "addDep inserts a new dependency with its repo (sorted, fmt-clean)" $
      let w = addDep (WorkspaceManifest ["packages/a"] "9.6.5" []) "aeson" (Tag "v2") "r/aeson"
       in (wsDependencies w, depRepos w)
            `shouldBe` ([Dependency "aeson" (Tag "v2") (Just "r/aeson") []], [("aeson", "r/aeson")])

    it "addDep replaces an existing dependency in place" $
      let w0 = addDep (WorkspaceManifest [] "9.6.5" []) "aeson" (Tag "v2") "r/aeson"
          w1 = addDep w0 "aeson" Latest "r/aeson2"
       in (wsDependencies w1, depRepos w1)
            `shouldBe` ([Dependency "aeson" Latest (Just "r/aeson2") []], [("aeson", "r/aeson2")])

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
        Left err -> expectationFailure (renderError err)

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

    it "normalizes git:// to https and appends a monorepo subdir (49o)" $ do
      sourceRepoOf (unlines ["name: x", "version: 1", "source-repository head", "  type: git", "  location: git://github.com/o/r"])
        `shouldBe` Just "https://github.com/o/r"
      sourceRepoOf (unlines ["name: x", "version: 1", "source-repository head", "  type: git", "  location: https://github.com/o/mono", "  subdir: pkg"])
        `shouldBe` Just "https://github.com/o/mono#pkg"

    it "builds the Hackage .cabal URL" $
      hackageCabalUrl "aeson" `shouldBe` "https://hackage.haskell.org/package/aeson/aeson.cabal"

  describe "buildMember (real compile)" $
    it "compiles and links a hello-world member, which runs" $ do
      let dir = "/tmp/zinc-member-build"
      stale <- doesDirectoryExist dir
      when stale $ removeDirectoryRecursive dir
      createDirectoryIfMissing True (dir ++ "/app")
      writeFile (dir ++ "/app/Main.hs") "module Main where\nmain :: IO ()\nmain = putStrLn \"hello from zinc\"\n"
      let comp =
            Component
              { compKind = Executable
              , compName = "demo"
              , compSourceDirs = ["app"]
              , compModules = []
              , compMain = Just "Main.hs"
              , compExtensions = []
              , compGhcOptions = []
              , compDepends = []
              , compSystemLibs = []
              , compIncludeDirs = []
              , compCppOptions = []
              , compCSources = []
              }
      r <- buildMember (MemberBuild dir (dir ++ "/build") Nothing comp)
      case r of
        Right exe -> do
          out <- readProcess exe [] ""
          out `shouldBe` "hello from zinc\n"
        Left err -> expectationFailure err

  describe "runBuild (scaffold -> build -> run)" $
    it "builds a scaffolded workspace member that runs" $ do
      let dir = "/tmp/zinc-build-test"
      stale <- doesDirectoryExist dir
      when stale $ removeDirectoryRecursive dir
      createDirectoryIfMissing True dir
      materialize dir (scaffoldNew "demo")
      r <- runBuild dir
      case r of
        Right (exe : _) -> do
          out <- readProcess exe [] ""
          out `shouldBe` "Hello from demo!\n"
        Right [] -> expectationFailure "no executable built"
        Left err -> expectationFailure (renderError err)

  describe "orderMembers" $
    it "orders a member after the siblings it depends on" $ do
      let comp deps =
            Component Library "x" [] [] Nothing [] [] deps [] [] [] []
          core = ("packages/core", MemberManifest "core" "1.0" [comp []])
          app = ("packages/app", MemberManifest "app" "1.0" [comp ["core"]])
      map (pkgName . snd) (orderMembers [app, core]) `shouldBe` ["core", "app"]

  describe "runBuild sibling linking (end-to-end)" $
    it "builds a lib member and an exe member that links it" $ do
      let d = "/tmp/zinc-sibling-ws"
      stale <- doesDirectoryExist d
      when stale $ removeDirectoryRecursive d
      writeFileIn (d ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/core", "packages/app"] "9.6.5" []))
      writeFileIn (d ++ "/packages/core/zinc.toml") (unlines ["[package]", "name = \"core\"", "version = \"1.0\"", "[build.lib]", "source-dirs = [\"src\"]", "exposed-modules = [\"Core\"]"])
      writeFileIn (d ++ "/packages/core/src/Core.hs") "module Core (greeting) where\ngreeting :: String\ngreeting = \"hi from core\"\n"
      writeFileIn (d ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"core\"]"])
      writeFileIn (d ++ "/packages/app/app/Main.hs") "module Main where\nimport Core (greeting)\nmain :: IO ()\nmain = putStrLn greeting\n"
      r <- runBuild d
      case r of
        Right (exe : _) -> do
          out <- readProcess exe [] ""
          out `shouldBe` "hi from core\n"
        Right [] -> expectationFailure "no executable built"
        Left err -> expectationFailure (renderError err)

  describe "buildAndRun (zinc run)" $
    it "builds and runs the member executable" $ do
      let dir = "/tmp/zinc-run-test"
      stale <- doesDirectoryExist dir
      when stale $ removeDirectoryRecursive dir
      createDirectoryIfMissing True dir
      materialize dir (scaffoldNew "demo")
      r <- buildAndRun dir []
      r `shouldBe` Right "Hello from demo!\n"

  describe "runTests (zinc test)" $
    it "builds and runs a passing test component" $ do
      let dir = "/tmp/zinc-test-test"
      stale <- doesDirectoryExist dir
      when stale $ removeDirectoryRecursive dir
      writeFileIn (dir ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/t"] "9.6.5" []))
      writeFileIn (dir ++ "/packages/t/zinc.toml") (unlines ["[package]", "name = \"t\"", "version = \"1.0\"", "[build.test.spec]", "source-dirs = [\"test\"]", "main = \"Spec.hs\""])
      writeFileIn (dir ++ "/packages/t/test/Spec.hs") "module Main where\nmain :: IO ()\nmain = putStrLn \"tests ok\"\n"
      r <- runTests dir
      r `shouldBe` Right 1

  describe "git dependency build (end-to-end)" $
    it "fetches a git dep, builds its library, and links a member against it" $ do
      let base = "/tmp/zinc-closure-ws"
          greet = base ++ "/greet-repo"
          ws = base ++ "/ws"
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      -- a zinc-native git library dependency
      writeFileIn (greet ++ "/zinc.toml") (unlines ["[package]", "name = \"greet\"", "version = \"1.0\"", "[build.lib]", "source-dirs = [\"src\"]", "exposed-modules = [\"Greet\"]"])
      writeFileIn (greet ++ "/src/Greet.hs") "module Greet (hello) where\nhello :: String\nhello = \"hi from greet\"\n"
      let git args = readProcess "git" ("-C" : greet : args) ""
      _ <- git ["init", "--quiet"]
      _ <- git ["config", "user.email", "t@example.com"]
      _ <- git ["config", "user.name", "Test"]
      _ <- git ["add", "."]
      _ <- git ["commit", "--quiet", "-m", "greet"]
      rev <- trimStr <$> git ["rev-parse", "HEAD"]
      -- workspace whose member depends on the git dep
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "greet" (Rev rev) (Just greet) []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "greet" greet rev "sha256:x" []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"greet\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Greet (hello)\nmain :: IO ()\nmain = putStrLn hello\n"
      r <- buildAndRun ws []
      r `shouldBe` Right "hi from greet\n"

  describe "lockDrift" $ do
    let ws = WorkspaceManifest [] "9.6.5" [Dependency "aeson" (Tag "v2") Nothing [], Dependency "hspec" Latest Nothing []]
        lk n = LockedPackage n "r" "rev" "sha" []

    it "reports manifest deps missing from the lock" $
      lockDrift ws [lk "aeson"] `shouldBe` ["hspec"]

    it "reports no drift when every dep is locked" $
      lockDrift ws [lk "aeson", lk "hspec"] `shouldBe` []

  describe "replArgs" $ do
    let exeComp =
          Component Executable "app" ["app"] [] (Just "Main.hs") [] [] [] [] [] [] []

    it "builds ghci args loading the member's main" $
      replArgs (Just "/db") "/m" exeComp
        `shouldBe` ["-package-db", "/db", "-hide-all-packages", "-package", "base", "-i/m/app", "/m/app/Main.hs"]

    it "loads a scaffolded member in ghci (ghci -e main)" $ do
      let dir = "/tmp/zinc-repl-test"
      stale <- doesDirectoryExist dir
      when stale $ removeDirectoryRecursive dir
      createDirectoryIfMissing True dir
      materialize dir (scaffoldNew "demo")
      let memberDir = dir ++ "/packages/demo"
          comp = Component Executable "demo" ["app"] [] (Just "Main.hs") [] [] [] [] [] [] []
      out <- readProcess "ghci" (replArgs Nothing memberDir comp ++ ["-e", "main"]) ""
      out `shouldBe` "Hello from demo!\n"

  describe "runBuildMember (target selection)" $
    it "builds only the named member's executable" $ do
      let d = "/tmp/zinc-target-ws"
      stale <- doesDirectoryExist d
      when stale $ removeDirectoryRecursive d
      writeFileIn (d ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/a", "packages/b"] "9.6.5" []))
      let member n =
            do
              writeFileIn (d ++ "/packages/" ++ n ++ "/zinc.toml") (unlines ["[package]", "name = \"" ++ n ++ "\"", "version = \"1.0\"", "[build.exe." ++ n ++ "]", "source-dirs = [\"app\"]", "main = \"Main.hs\""])
              writeFileIn (d ++ "/packages/" ++ n ++ "/app/Main.hs") ("module Main where\nmain :: IO ()\nmain = putStrLn \"" ++ n ++ "\"\n")
      member "a"
      member "b"
      one <- runBuildMember d (Just "a")
      allB <- runBuildMember d Nothing
      (length <$> one, length <$> allB) `shouldBe` (Right 1, Right 2)

  describe "parseCabalComponentsForGhc" $ do
    it "resolves impl(ghc) conditionals using the supplied version" $ do
      let c =
            unlines
              [ "cabal-version: 2.4"
              , "name: c"
              , "version: 1"
              , "library"
              , "  build-depends: base"
              , "  exposed-modules: M"
              , "  if impl(ghc >= 9.8)"
              , "    other-modules: NewGhc"
              ]
          libFor v = either (const Nothing) (find ((== "lib") . compName)) (parseCabalComponentsForGhc v c)
      (compModules <$> libFor "9.8.2", compModules <$> libFor "9.6.5")
        `shouldBe` (Just ["M", "NewGhc"], Just ["M"])

    it "extracts cpp-options and c-sources from a library (zinc-izy)" $ do
      let c =
            unlines
              [ "cabal-version: 2.4"
              , "name: d"
              , "version: 1"
              , "library"
              , "  build-depends: base"
              , "  exposed-modules: M"
              , "  cpp-options: -DUSE_C"
              , "  c-sources: cbits/init.c"
              ]
          libFor = either (const Nothing) (find ((== "lib") . compName)) (parseCabalComponents c)
      (compCppOptions <$> libFor, compCSources <$> libFor)
        `shouldBe` (Just ["-DUSE_C"], Just ["cbits/init.c"])

  describe "git dependency build from .cabal (end-to-end)" $
    it "builds a non-zinc-native git dep (only a .cabal) and links a member" $ do
      let base = "/tmp/zinc-cabal-dep-ws"
          dep = base ++ "/greet-repo"
          ws = base ++ "/ws"
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      -- a plain Hackage-style package: only a .cabal, no zinc.toml
      writeFileIn (dep ++ "/greet.cabal") (unlines ["cabal-version: 2.4", "name: greet", "version: 1.0", "library", "  hs-source-dirs: src", "  exposed-modules: Greet", "  build-depends: base"])
      writeFileIn (dep ++ "/src/Greet.hs") "module Greet (hello) where\nhello :: String\nhello = \"hi from cabal dep\"\n"
      let git args = readProcess "git" ("-C" : dep : args) ""
      _ <- git ["init", "--quiet"]
      _ <- git ["config", "user.email", "t@example.com"]
      _ <- git ["config", "user.name", "Test"]
      _ <- git ["add", "."]
      _ <- git ["commit", "--quiet", "-m", "greet"]
      rev <- trimStr <$> git ["rev-parse", "HEAD"]
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "greet" (Rev rev) (Just dep) []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "greet" dep rev "sha256:x" []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"greet\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Greet (hello)\nmain :: IO ()\nmain = putStrLn hello\n"
      r <- buildAndRun ws []
      r `shouldBe` Right "hi from cabal dep\n"

  describe "non-base boot-lib linking (end-to-end)" $
    it "links a dep that uses a boot lib base does not pull (array)" $ do
      let base = "/tmp/zinc-bootlink-ws"
          dep = base ++ "/boxed-repo"
          ws = base ++ "/ws"
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      -- a zinc-native dep whose library uses Data.Array (boot lib 'array',
      -- which 'base' does not transitively provide at link time)
      writeFileIn (dep ++ "/zinc.toml") (unlines ["[package]", "name = \"boxed\"", "version = \"1.0\"", "[build.lib]", "source-dirs = [\"src\"]", "exposed-modules = [\"Boxed\"]", "depends = [\"array\"]"])
      writeFileIn (dep ++ "/src/Boxed.hs") "module Boxed (firstElem) where\nimport Data.Array (listArray, (!))\nfirstElem :: Int\nfirstElem = listArray (0, 2 :: Int) [10, 20, 30] ! (0 :: Int)\n"
      let git args = readProcess "git" ("-C" : dep : args) ""
      _ <- git ["init", "--quiet"]
      _ <- git ["config", "user.email", "t@example.com"]
      _ <- git ["config", "user.name", "Test"]
      _ <- git ["add", "."]
      _ <- git ["commit", "--quiet", "-m", "boxed"]
      rev <- trimStr <$> git ["rev-parse", "HEAD"]
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "boxed" (Rev rev) (Just dep) []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "boxed" dep rev "sha256:x" []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"boxed\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Boxed (firstElem)\nmain :: IO ()\nmain = print firstElem\n"
      r <- buildAndRun ws []
      r `shouldBe` Right "10\n"

  describe "closure builder runs alex preprocessor (end-to-end)" $
    it "builds a git dep whose library ships an alex .x lexer" $ do
      let base = "/tmp/zinc-alex-ws"
          dep = base ++ "/lexdep-repo"
          ws = base ++ "/ws"
          alexSrc =
            unlines
              [ "{"
              , "module Lexer (firstWord) where"
              , "}"
              , "%wrapper \"basic\""
              , "tokens :-"
              , "  $white+ ;"
              , "  [A-Za-z]+ { \\s -> s }"
              , "{"
              , "firstWord :: String"
              , "firstWord = head (alexScanTokens \"hello world\")"
              , "}"
              ]
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      writeFileIn (dep ++ "/zinc.toml") (unlines ["[package]", "name = \"lexdep\"", "version = \"1.0\"", "[build.lib]", "source-dirs = [\"src\"]", "exposed-modules = [\"Lexer\"]", "depends = [\"array\"]"])
      writeFileIn (dep ++ "/src/Lexer.x") alexSrc
      let git args = readProcess "git" ("-C" : dep : args) ""
      _ <- git ["init", "--quiet"]
      _ <- git ["config", "user.email", "t@example.com"]
      _ <- git ["config", "user.name", "Test"]
      _ <- git ["add", "."]
      _ <- git ["commit", "--quiet", "-m", "lexdep"]
      rev <- trimStr <$> git ["rev-parse", "HEAD"]
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "lexdep" (Rev rev) (Just dep) []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "lexdep" dep rev "sha256:x" []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"lexdep\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Lexer (firstWord)\nmain :: IO ()\nmain = putStrLn firstWord\n"
      r <- buildAndRun ws []
      r `shouldBe` Right "hello\n"

  describe "per-dependency build overrides (end-to-end)" $
    it "applies extra ghc flags from [build-options] to a closure dep" $ do
      let base = "/tmp/zinc-override-ws"
          dep = base ++ "/extdep-repo"
          ws = base ++ "/ws"
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      -- the dep's library uses a tuple section, which needs -XTupleSections —
      -- supplied ONLY via the workspace [build-options], not the dep's manifest.
      writeFileIn (dep ++ "/zinc.toml") (unlines ["[package]", "name = \"extdep\"", "version = \"1.0\"", "[build.lib]", "source-dirs = [\"src\"]", "exposed-modules = [\"Ext\"]"])
      writeFileIn (dep ++ "/src/Ext.hs") "module Ext (tag) where\ntag :: a -> (Int, a)\ntag = (1,)\n"
      let git args = readProcess "git" ("-C" : dep : args) ""
      _ <- git ["init", "--quiet"]
      _ <- git ["config", "user.email", "t@example.com"]
      _ <- git ["config", "user.name", "Test"]
      _ <- git ["add", "."]
      _ <- git ["commit", "--quiet", "-m", "extdep"]
      rev <- trimStr <$> git ["rev-parse", "HEAD"]
      -- workspace manifest hand-written so it can carry a [build-options] table
      writeFileIn (ws ++ "/zinc.toml") (unlines ["[workspace]", "members = [\"packages/app\"]", "ghc = \"9.6.5\"", "[dependencies.extdep]", "rev = \"" ++ rev ++ "\"", "repo = \"" ++ dep ++ "\"", "ghc-options = [\"-XTupleSections\"]"])
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "extdep" dep rev "sha256:x" []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"extdep\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Ext (tag)\nmain :: IO ()\nmain = print (fst (tag \"x\"))\n"
      r <- buildAndRun ws []
      r `shouldBe` Right "1\n"

  describe "splitRepoSubdir" $ do
    it "returns the whole spec and no subdir when there is no # suffix" $
      splitRepoSubdir "https://github.com/a/b.git" `shouldBe` ("https://github.com/a/b.git", Nothing)

    it "splits a url#subdir suffix into url and subdir" $
      splitRepoSubdir "https://github.com/quchen/prettyprinter#prettyprinter" `shouldBe` ("https://github.com/quchen/prettyprinter", Just "prettyprinter")

    it "supports a nested subdir path" $
      splitRepoSubdir "/local/repo#pkgs/core" `shouldBe` ("/local/repo", Just "pkgs/core")

  describe "git dependency in a repo subdirectory (end-to-end)" $
    it "builds a package located in a monorepo subdir and links a member" $ do
      let base = "/tmp/zinc-subdir-ws"
          repo = base ++ "/mono-repo"
          ws = base ++ "/ws"
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      -- a monorepo: the package lives under pkgs/greet, not at the root
      writeFileIn (repo ++ "/README.md") "monorepo root\n"
      writeFileIn (repo ++ "/pkgs/greet/zinc.toml") (unlines ["[package]", "name = \"greet\"", "version = \"1.0\"", "[build.lib]", "source-dirs = [\"src\"]", "exposed-modules = [\"Greet\"]"])
      writeFileIn (repo ++ "/pkgs/greet/src/Greet.hs") "module Greet (hi) where\nhi :: String\nhi = \"hi from subdir\"\n"
      let git args = readProcess "git" ("-C" : repo : args) ""
      _ <- git ["init", "--quiet"]
      _ <- git ["config", "user.email", "t@example.com"]
      _ <- git ["config", "user.name", "Test"]
      _ <- git ["add", "."]
      _ <- git ["commit", "--quiet", "-m", "mono"]
      rev <- trimStr <$> git ["rev-parse", "HEAD"]
      let repoSpec = repo ++ "#pkgs/greet"
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "greet" (Rev rev) (Just repoSpec) []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "greet" repoSpec rev "sha256:x" []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"greet\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Greet (hi)\nmain :: IO ()\nmain = putStrLn hi\n"
      r <- buildAndRun ws []
      r `shouldBe` Right "hi from subdir\n"

  describe "closure builder passes cabal include-dirs to CPP (end-to-end)" $
    it "builds a dep whose module #includes a header from its include-dirs" $ do
      let base = "/tmp/zinc-incdir-ws"
          dep = base ++ "/hdrdep-repo"
          ws = base ++ "/ws"
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      writeFileIn (dep ++ "/zinc.toml") (unlines ["[package]", "name = \"hdrdep\"", "version = \"1.0\"", "[build.lib]", "source-dirs = [\"src\"]", "exposed-modules = [\"Hdr\"]", "include-dirs = [\"include\"]"])
      writeFileIn (dep ++ "/include/myconst.h") "#define MY_CONST 7\n"
      writeFileIn (dep ++ "/src/Hdr.hs") "{-# LANGUAGE CPP #-}\nmodule Hdr (val) where\n#include \"myconst.h\"\nval :: Int\nval = MY_CONST\n"
      let git args = readProcess "git" ("-C" : dep : args) ""
      _ <- git ["init", "--quiet"]
      _ <- git ["config", "user.email", "t@example.com"]
      _ <- git ["config", "user.name", "Test"]
      _ <- git ["add", "."]
      _ <- git ["commit", "--quiet", "-m", "hdrdep"]
      rev <- trimStr <$> git ["rev-parse", "HEAD"]
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "hdrdep" (Rev rev) (Just dep) []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "hdrdep" dep rev "sha256:x" []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"hdrdep\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Hdr (val)\nmain :: IO ()\nmain = print val\n"
      r <- buildAndRun ws []
      r `shouldBe` Right "7\n"

  -- Test ladder rung 2 (spec §12): a REAL Hackage leaf pulled from git and
  -- built via the Opt-2 .cabal reader. Network-gated so the default suite
  -- stays hermetic; run with ZINC_NET_TESTS=1 (the capability is also covered
  -- hermetically by the boot-lib and .cabal-dep e2e tests above).
  describe "real Hackage leaf via Opt-2 reader (rung 2, network)" $
    it "builds integer-logarithms from git and links a member" $ do
      net <- lookupEnv "ZINC_NET_TESTS"
      case net of
        Nothing -> pendingWith "network test; set ZINC_NET_TESTS=1 to run"
        Just _ -> do
          let base = "/tmp/zinc-rung2-ws"
              dep = base ++ "/integer-logarithms"
              ws = base ++ "/ws"
          stale <- doesDirectoryExist base
          when stale $ removeDirectoryRecursive base
          createDirectoryIfMissing True base
          _ <- readProcess "git" ["clone", "--depth", "1", "https://github.com/Bodigrim/integer-logarithms.git", dep] ""
          rev <- trimStr <$> readProcess "git" ["-C", dep, "rev-parse", "HEAD"] ""
          writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "integer-logarithms" (Rev rev) (Just dep) []]))
          writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "integer-logarithms" dep rev "sha256:x" []])
          writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"integer-logarithms\"]"])
          writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Math.NumberTheory.Logarithms (integerLog2)\nmain :: IO ()\nmain = putStrLn (\"log2(1000)=\" ++ show (integerLog2 1000))\n"
          r <- buildAndRun ws []
          r `shouldBe` Right "log2(1000)=9\n"

  -- A deeper real closure (rung ~2.5): toml-parser pulls prettyprinter (a
  -- monorepo subdir) and runs alex/happy + CPP include-dirs — exercising the
  -- full closure machinery on genuine packages. Network-gated.
  describe "real multi-package closure: toml-parser + prettyprinter (network)" $
    it "builds toml-parser with its prettyprinter dep and parses TOML" $ do
      net <- lookupEnv "ZINC_NET_TESTS"
      case net of
        Nothing -> pendingWith "network test; set ZINC_NET_TESTS=1 to run"
        Just _ -> do
          let base = "/tmp/zinc-rung25"
              tomlDep = base ++ "/toml-parser"
              ppDep = base ++ "/prettyprinter"
              ws = base ++ "/ws"
          stale <- doesDirectoryExist base
          when stale $ removeDirectoryRecursive base
          createDirectoryIfMissing True base
          _ <- readProcess "git" ["clone", "--depth", "1", "https://github.com/glguy/toml-parser.git", tomlDep] ""
          _ <- readProcess "git" ["clone", "--depth", "1", "https://github.com/quchen/prettyprinter.git", ppDep] ""
          tRev <- trimStr <$> readProcess "git" ["-C", tomlDep, "rev-parse", "HEAD"] ""
          pRev <- trimStr <$> readProcess "git" ["-C", ppDep, "rev-parse", "HEAD"] ""
          let ppSpec = ppDep ++ "#prettyprinter"
          writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "toml-parser" (Rev tRev) (Just tomlDep) [], Dependency "prettyprinter" Latest (Just ppSpec) []]))
          writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "prettyprinter" ppSpec pRev "sha256:x" [], LockedPackage "toml-parser" tomlDep tRev "sha256:x" ["prettyprinter"]])
          writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"toml-parser\"]"])
          writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Toml (parse)\nmain :: IO ()\nmain = putStrLn (either (const \"err\") (const \"parsed-ok\") (parse \"x = 1\\n\"))\n"
          r <- buildAndRun ws []
          r `shouldBe` Right "parsed-ok\n"

  -- The full self-host pipeline on real packages: resolve a real upstream's
  -- closure from its .cabal + the workspace registry, freeze it (real content
  -- hashes), then build/run — zinc generating its own lock, not a hand-authored
  -- one. Network-gated.
  describe "self-host rehearsal: resolve+freeze+build a real closure (network)" $
    it "runUpdate auto-generates toml-parser's lock, which then builds and runs" $ do
      net <- lookupEnv "ZINC_NET_TESTS"
      case net of
        Nothing -> pendingWith "network test; set ZINC_NET_TESTS=1 to run"
        Just _ -> do
          let base = "/tmp/zinc-rehearsal"
              tomlDep = base ++ "/toml-parser"
              ppDep = base ++ "/prettyprinter"
              ws = base ++ "/ws"
              store = base ++ "/store"
          stale <- doesDirectoryExist base
          when stale $ removeDirectoryRecursive base
          createDirectoryIfMissing True base
          _ <- readProcess "git" ["clone", "--depth", "1", "https://github.com/glguy/toml-parser.git", tomlDep] ""
          _ <- readProcess "git" ["clone", "--depth", "1", "https://github.com/quchen/prettyprinter.git", ppDep] ""
          tRev <- trimStr <$> readProcess "git" ["-C", tomlDep, "rev-parse", "HEAD"] ""
          pRev <- trimStr <$> readProcess "git" ["-C", ppDep, "rev-parse", "HEAD"] ""
          writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "toml-parser" (Rev tRev) (Just tomlDep) [], Dependency "prettyprinter" (Rev pRev) (Just (ppDep ++ "#prettyprinter")) []]))
          writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"toml-parser\"]"])
          writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Toml (parse)\nmain :: IO ()\nmain = putStrLn (either (const \"err\") (const \"parsed-ok\") (parse \"x = 1\\n\"))\n"
          -- zinc resolves the real closure (.cabal + registry) and freezes a lock
          upd <- runUpdate (ws ++ "/zinc.toml") store
          upd `shouldSatisfy` isRight
          lockSrc <- readFile (ws ++ "/zinc.lock")
          (isInfixOf "toml-parser" lockSrc && isInfixOf "prettyprinter" lockSrc) `shouldBe` True
          r <- buildAndRun ws []
          r `shouldBe` Right "parsed-ok\n"

  describe "content-hash verification on build (spec §8)" $
    it "rejects a fetched dep whose content hash does not match the lock" $ do
      let base = "/tmp/zinc-tamper-ws"
          dep = base ++ "/greet-repo"
          ws = base ++ "/ws"
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      writeFileIn (dep ++ "/zinc.toml") (unlines ["[package]", "name = \"greet\"", "version = \"1.0\"", "[build.lib]", "source-dirs = [\"src\"]", "exposed-modules = [\"Greet\"]"])
      writeFileIn (dep ++ "/src/Greet.hs") "module Greet (hello) where\nhello :: String\nhello = \"hi\"\n"
      let git args = readProcess "git" ("-C" : dep : args) ""
      _ <- git ["init", "--quiet"]
      _ <- git ["config", "user.email", "t@example.com"]
      _ <- git ["config", "user.name", "Test"]
      _ <- git ["add", "."]
      _ <- git ["commit", "--quiet", "-m", "greet"]
      rev <- trimStr <$> git ["rev-parse", "HEAD"]
      -- a real-shaped sha256 that deliberately does NOT match the source tree
      let wrongSha = "sha256:" ++ replicate 64 '0'
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "greet" (Rev rev) (Just dep) []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "greet" dep rev wrongSha []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"greet\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Greet (hello)\nmain :: IO ()\nmain = putStrLn hello\n"
      r <- buildAndRun ws []
      r `shouldSatisfy` either ((== "ZINC_CONTENT_HASH_MISMATCH") . errorCode) (const False)

  describe "dependency build with Paths_ (end-to-end)" $ do
    it "synthesizes Paths_<pkg> so a dep importing it builds + links" $ do
      let base = "/tmp/zinc-paths-dep-ws"
          dep = base ++ "/greet-repo"
          ws = base ++ "/ws"
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      writeFileIn (dep ++ "/zinc.toml") (unlines ["[package]", "name = \"greet\"", "version = \"1.2\"", "[build.lib]", "source-dirs = [\"src\"]", "exposed-modules = [\"Greet\"]"])
      writeFileIn (dep ++ "/src/Greet.hs") "module Greet (hello) where\nimport Paths_greet (version)\nimport Data.Version (showVersion)\nhello :: String\nhello = \"greet \" ++ showVersion version\n"
      let git args = readProcess "git" ("-C" : dep : args) ""
      _ <- git ["init", "--quiet"]
      _ <- git ["config", "user.email", "t@example.com"]
      _ <- git ["config", "user.name", "Test"]
      _ <- git ["add", "."]
      _ <- git ["commit", "--quiet", "-m", "greet"]
      rev <- trimStr <$> git ["rev-parse", "HEAD"]
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "greet" (Rev rev) (Just dep) []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "greet" dep rev "sha256:x" []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"greet\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Greet (hello)\nmain :: IO ()\nmain = putStrLn hello\n"
      r <- buildAndRun ws []
      r `shouldBe` Right "greet 1.2\n"

    it "does not double-define Paths_<pkg> when the dep also lists it as a module" $ do
      let base = "/tmp/zinc-paths-dup-ws"
          dep = base ++ "/greet-repo"
          ws = base ++ "/ws"
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      -- the dep explicitly lists Paths_greet in other-modules (as cabal
      -- autogen-modules do) — zinc must not collide with its synthesized copy
      writeFileIn (dep ++ "/zinc.toml") (unlines ["[package]", "name = \"greet\"", "version = \"2.0\"", "[build.lib]", "source-dirs = [\"src\"]", "exposed-modules = [\"Greet\"]", "other-modules = [\"Paths_greet\"]"])
      writeFileIn (dep ++ "/src/Greet.hs") "module Greet (hello) where\nimport Paths_greet (version)\nimport Data.Version (showVersion)\nhello :: String\nhello = \"greet \" ++ showVersion version\n"
      let git args = readProcess "git" ("-C" : dep : args) ""
      _ <- git ["init", "--quiet"]
      _ <- git ["config", "user.email", "t@example.com"]
      _ <- git ["config", "user.name", "Test"]
      _ <- git ["add", "."]
      _ <- git ["commit", "--quiet", "-m", "greet"]
      rev <- trimStr <$> git ["rev-parse", "HEAD"]
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "greet" (Rev rev) (Just dep) []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "greet" dep rev "sha256:x" []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"greet\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Greet (hello)\nmain :: IO ()\nmain = putStrLn hello\n"
      r <- buildAndRun ws []
      r `shouldBe` Right "greet 2.0\n"

  describe "deep closure + artifact cache (end-to-end)" $
    it "builds a 2-level git-dep closure, then rebuilds from cache after sources are gone" $ do
      let base = "/tmp/zinc-deep-ws"
          repoB = base ++ "/b-repo"
          repoA = base ++ "/a-repo"
          ws = base ++ "/ws"
          gitIn d args = readProcess "git" ("-C" : d : args) ""
          mkRepo d files = do
            mapM_ (uncurry writeFileIn) [(d ++ "/" ++ p, c) | (p, c) <- files]
            _ <- gitIn d ["init", "--quiet"]
            _ <- gitIn d ["config", "user.email", "t@e"]
            _ <- gitIn d ["config", "user.name", "T"]
            _ <- gitIn d ["add", "."]
            _ <- gitIn d ["commit", "--quiet", "-m", "c"]
            trimStr <$> gitIn d ["rev-parse", "HEAD"]
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      revB <- mkRepo repoB
        [ ("zinc.toml", unlines ["[package]", "name = \"b\"", "version = \"1.0\"", "[build.lib]", "source-dirs = [\"src\"]", "exposed-modules = [\"BMod\"]"])
        , ("src/BMod.hs", "module BMod (vb) where\nvb :: String\nvb = \"B\"\n")
        ]
      revA <- mkRepo repoA
        [ ("zinc.toml", unlines ["[package]", "name = \"a\"", "version = \"1.0\"", "[build.lib]", "source-dirs = [\"src\"]", "exposed-modules = [\"AMod\"]", "depends = [\"b\"]"])
        , ("src/AMod.hs", "module AMod (va) where\nimport BMod (vb)\nva :: String\nva = \"A+\" ++ vb\n")
        ]
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "a" (Rev revA) (Just repoA) [], Dependency "b" Latest (Just repoB) []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "a" repoA revA "sha256:a" ["b"], LockedPackage "b" repoB revB "sha256:b" []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"a\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport AMod (va)\nmain :: IO ()\nmain = putStrLn va\n"
      first <- buildAndRun ws []
      -- destroy the dependency sources; a correct cache must rebuild without them
      removeDirectoryRecursive repoA
      removeDirectoryRecursive repoB
      removeDirectoryRecursive (testStoreDir ++ "/src")
      second <- buildAndRun ws []
      (first, second) `shouldBe` (Right "A+B\n", Right "A+B\n")

  describe "cabalBuildType" $ do
    it "reads a Simple build-type" $
      cabalBuildType (unlines ["cabal-version: 2.4", "name: d", "version: 1", "build-type: Simple", "library", "  build-depends: base"])
        `shouldBe` Right "Simple"

    it "reads a Custom build-type" $
      cabalBuildType (unlines ["cabal-version: 2.4", "name: d", "version: 1", "build-type: Custom", "custom-setup", "  setup-depends: base, Cabal", "library", "  build-depends: base"])
        `shouldBe` Right "Custom"

  describe "cabalVersion" $
    it "reads the declared package version" $
      cabalVersion (unlines ["cabal-version: 2.4", "name: colour", "version: 2.3.6", "library", "  build-depends: base"])
        `shouldBe` Right "2.3.6"

  describe "runClean" $
    it "removes build artifacts but keeps the store and metrics" $ do
      let d = "/tmp/zinc-clean-test"
      stale <- doesDirectoryExist d
      when stale $ removeDirectoryRecursive d
      writeFileIn (d ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/a"] "9.6.5" []))
      writeFileIn (d ++ "/packages/a/zinc.toml") "[package]\nname = \"a\"\nversion = \"1.0\"\n"
      writeFileIn (d ++ "/.zinc/store/keep.txt") "cached"
      writeFileIn (d ++ "/.zinc/metrics.jsonl") "{\"command\":\"build\"}\n"
      writeFileIn (d ++ "/.zinc/pkgdb/x") "db"
      writeFileIn (d ++ "/packages/a/.zinc/build/x.o") "obj"
      runClean d
      store <- doesDirectoryExist (d ++ "/.zinc/store")
      pkgdb <- doesDirectoryExist (d ++ "/.zinc/pkgdb")
      metrics <- doesFileExist (d ++ "/.zinc/metrics.jsonl")
      buildGone <- not <$> doesDirectoryExist (d ++ "/packages/a/.zinc/build")
      (store, pkgdb, metrics, buildGone) `shouldBe` (True, False, True, True)

  describe "gcStore (store garbage collection)" $
    it "sweeps unreferenced pkg/ and src/ entries, keeping live ones" $ do
      let root = "/tmp/zinc-gc-store"
          ghc = "9.6.5"
          liveLock = LockedPackage "a" "r/a" "rev-a" "sha256:x" []
          liveKey = buildCacheKey (BuildKey "rev-a" ghc [] [])
          deadKey = buildCacheKey (BuildKey "rev-z" ghc [] [])
      stale <- doesDirectoryExist root
      when stale $ removeDirectoryRecursive root
      writeFileIn (root ++ "/pkg/" ++ liveKey ++ "/package.conf") "live"
      writeFileIn (root ++ "/pkg/" ++ deadKey ++ "/package.conf") "dead"
      writeFileIn (root ++ "/src/a-rev-a/x.hs") "live"
      writeFileIn (root ++ "/src/b-rev-b/x.hs") "dead"
      (rmPkg, rmSrc) <- gcStore root [GCRoot ghc [liveLock]]
      livePkg <- doesDirectoryExist (root ++ "/pkg/" ++ liveKey)
      deadPkg <- doesDirectoryExist (root ++ "/pkg/" ++ deadKey)
      liveSrc <- doesDirectoryExist (root ++ "/src/a-rev-a")
      deadSrc <- doesDirectoryExist (root ++ "/src/b-rev-b")
      (livePkg, deadPkg, liveSrc, deadSrc, rmPkg, rmSrc)
        `shouldBe` (True, False, True, False, [deadKey], ["b-rev-b"])

  describe "runGc (workspace GC entry)" $
    it "collects store entries not referenced by the current workspace lock" $ do
      let dir = "/tmp/zinc-gc-ws"
          gcRoot = "/tmp/zinc-gc-ws-store"
          ghc = "9.6.5"
          liveKey = buildCacheKey (BuildKey "rev-a" ghc [] [])
          deadKey = buildCacheKey (BuildKey "rev-z" ghc [] [])
      mapM_ (\p -> doesDirectoryExist p >>= \e -> when e (removeDirectoryRecursive p)) [dir, gcRoot]
      setEnv "ZINC_STORE" gcRoot
      writeFileIn (dir ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] ghc [Dependency "a" (Rev "rev-a") (Just "r/a") []]))
      writeFileIn (dir ++ "/zinc.lock") (renderLock [LockedPackage "a" "r/a" "rev-a" "sha256:x" []])
      writeFileIn (gcRoot ++ "/pkg/" ++ liveKey ++ "/package.conf") "live"
      writeFileIn (gcRoot ++ "/pkg/" ++ deadKey ++ "/package.conf") "dead"
      writeFileIn (gcRoot ++ "/src/a-rev-a/x.hs") "live"
      writeFileIn (gcRoot ++ "/src/zombie-rev-z/x.hs") "dead"
      r <- runGc dir
      setEnv "ZINC_STORE" testStoreDir -- restore shared isolation
      r `shouldBe` Right ([deadKey], ["zombie-rev-z"])

  describe "runUpdate" $ do
    (wsFile, store, _leafRepo) <- runIO setupAddFixture

    it "re-resolves and rewrites the lockfile" $ do
      r <- runUpdate wsFile store
      lockText <- readFile (takeDirectory wsFile </> "zinc.lock")
      case r of
        Right _ -> ("leaf" `isInfixOf` lockText) `shouldBe` True
        Left err -> expectationFailure (renderError err)

  describe "full workspace lifecycle (integration, rung 1)" $
    it "scaffolds, builds, runs, then cleans a synthetic workspace" $ do
      let d = "/tmp/zinc-lifecycle"
      stale <- doesDirectoryExist d
      when stale $ removeDirectoryRecursive d
      createDirectoryIfMissing True d
      materialize d (scaffoldNew "demo") -- new
      built <- runBuild d -- build
      ran <- buildAndRun d [] -- run
      runClean d -- clean
      artifactsGone <- not <$> doesDirectoryExist (d ++ "/packages/demo/.zinc/build")
      (fmap length built, ran, artifactsGone)
        `shouldBe` (Right 1, Right "Hello from demo!\n", True)

  describe "installedVersions" $
    it "reports the real version of a boot library (base)" $ do
      vs <- installedVersions
      case lookup "base" vs of
        Just (major : _) -> (major >= 4) `shouldBe` True
        _                -> expectationFailure "base not found in ghc-pkg"

  describe "real cabal_macros versions (end-to-end)" $
    it "a dep guarded on MIN_VERSION_base compiles the modern branch" $ do
      let base = "/tmp/zinc-macro-ver-ws"
          dep = base ++ "/greet-repo"
          ws = base ++ "/ws"
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      writeFileIn (dep ++ "/zinc.toml") (unlines ["[package]", "name = \"greet\"", "version = \"1.0\"", "[build.lib]", "source-dirs = [\"src\"]", "exposed-modules = [\"Greet\"]"])
      writeFileIn (dep ++ "/src/Greet.hs") "{-# LANGUAGE CPP #-}\nmodule Greet (hello) where\nhello :: String\n#if MIN_VERSION_base(4,0,0)\nhello = \"modern\"\n#else\nhello = \"old\"\n#endif\n"
      let git args = readProcess "git" ("-C" : dep : args) ""
      _ <- git ["init", "--quiet"]
      _ <- git ["config", "user.email", "t@e"]
      _ <- git ["config", "user.name", "T"]
      _ <- git ["add", "."]
      _ <- git ["commit", "--quiet", "-m", "c"]
      rev <- trimStr <$> git ["rev-parse", "HEAD"]
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "greet" (Rev rev) (Just dep) []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "greet" dep rev "sha256:x" []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"greet\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Greet (hello)\nmain :: IO ()\nmain = putStrLn hello\n"
      r <- buildAndRun ws []
      r `shouldBe` Right "modern\n"

  describe "resolveStoreRoot" $ do
    it "honours the ZINC_STORE override when set" $ do
      setEnv "ZINC_STORE" "/tmp/zinc-custom-store"
      r <- resolveStoreRoot
      setEnv "ZINC_STORE" testStoreDir -- restore isolation for any later tests
      r `shouldBe` "/tmp/zinc-custom-store"

    it "falls back to ~/.zinc/store when ZINC_STORE is unset" $ do
      unsetEnv "ZINC_STORE"
      home <- getHomeDirectory
      r <- resolveStoreRoot
      setEnv "ZINC_STORE" testStoreDir -- restore isolation for any later tests
      r `shouldBe` home </> ".zinc" </> "store"

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
