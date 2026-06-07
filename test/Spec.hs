module Main (main) where

import Control.Monad (forM_, when)
import Data.Char (isSpace)
import Data.Either (isLeft, isRight)
import Data.Functor.Identity (runIdentity)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, try)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Maybe (isJust)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , removePathForcibly
  , getHomeDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Process (readProcess)
import Test.Hspec
import System.Exit (ExitCode (..))
import Zinc.CLI (Command (..), helpOverview, parseArgs)
import Zinc.Diagnostic (Diagnostic (..), Severity (..), SourceLocation (..), ZincError (..), diagnosticJson, envelope, errorCode, exitCodeFor, ghcLocation, humanError, rawToolOutput, renderError, toDiagnostic, tomlLocation, zincVersion, zincVersionLine)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Zinc.Closure (discoverRepos, parseDependsField, pkgNameOf)
import Zinc.Ansi (greenBold, style)
import Zinc.Output (OutputEvent (..), OutputFlags (..), OutputMode (..), RProg (..), Sink (..), eventJson, nullSink, progressLine, verb, withRenderer)
import Zinc.Docker (dockerfileText)
import Zinc.Deploy
  ( DeployHost (..)
  , ProbeChecks (..)
  , ProbeOutcome (..)
  , ResolvedDeploy (..)
  , initSnippet
  , interpretProbe
  , nixCopyArgs
  , nixCopyEnv
  , nixCopyStoreUri
  , profileInstallScript
  , profileName
  , parseDeployHost
  , parseProbeOutput
  , probeScript
  , resolveDeploy
  , sshArgs
  )
import Zinc.Fmt (canonicalizeManifest, mergeManifestDependencies, setManifestDependencies)
import Zinc.Doctor (doctorJson, doctorOk, flakesOffDiagnostic, lockDriftDiagnostic, renderDoctor, runDoctor)
import Zinc.Introspect (DepStatus (..), explainJson, graphJson, runStatus, statusJson)
import Zinc.Prime (onboardText, primeText)
import Zinc.Json (Json (..), parseJson, renderJson)
import Zinc.Git (cloneAt, gitEnv, gitInitIfNeeded, isInsideRepo, listTags, splitRepoSubdir)
import Zinc.Hackage (hackageCabalUrl, hackageTarballUrl, sourceRepoOf)
import Zinc.Outdated (OutdatedDep (..), Status (..), classify)
import Zinc.Quirks (quirkGhcOptions)
import Zinc.Package (PackageFormat (..), dockerImageRef, formatName, packagingFlake, parsePackageFormat, storePathRefs)
import Zinc.Delta (Change (..), ClosureDelta (..), closureDelta, isEmptyDelta)
import Zinc.CacheBackend (CacheBackend (..), CacheConfig (..), PullOutcome (..), artifactUrl, curlOutcome, httpBackend, parseCacheTable, resolveCacheConfig)
import Zinc.Store (contentHash, resolveStoreRoot, srcDirName, storeSrcPath, verifyContent, withStoreLock)
import Zinc.Manifest
  ( Component (..)
  , ComponentKind (..)
  , Dependency (..)
  , DeployTarget (..)
  , MemberManifest (..)
  , Ref (..)
  , WorkspaceManifest (..)
  , addDep
  , addVendored
  , depRepos
  , depGhcOptionsOf
  , depFlagsOf
  , parseDependencies
  , parseDeployTargets
  , parseMember
  , parseWorkspace
  , renderWorkspace
  )
import Zinc.Fetch (gitFetchManifest, isHpackOnly, namedCabal, packageDirIn)
import Zinc.GC (GCRoot (..), gcStore, runGc)
import Zinc.Add (enrichWithRepos, freezeClosure, lockEntry, runAdd, runUpdate, runVendor, splitNameVersion)
import Zinc.Build (GhcInvocation (..), LibBuild (..), MemberBuild (..), PackageConf (..), archiveArgs, buildLib, buildMember, discoverModules, ghcMakeArgs, initPackageDb, installedVersions, memberBuildDir, packageFlags, ppCommand, preprocessorFor, reactorLinkFlags, registeredExposedMatches, registerPackage, renderConf, replArgs, runPreprocessor, wasmSupported, writeFileIfChanged, zincBuiltUnitIds)
import Zinc.Cache (BuildKey (..), buildCacheKey, buildCacheKeyFor, cacheHit, storeConfPath, storePkgPath, writeCachedConf)
import Zinc.Cabal (bootConflicts, cabalBuildType, cabalVersion, parseCabalComponents, parseCabalComponentsForGhc, parseCabalComponentsForPlatform)
import Distribution.System (Arch (Wasm32), OS (Wasi), Platform (Platform), buildPlatform)
import Zinc.Env (devEnvVars, envCacheKey, envCacheKeyFor, nixPrintDevEnv, provisionEnv, toolchainPath, toolchainVars)
import Zinc.Macros (emitCabalMacros)
import Zinc.Nix (generateFlake, generateFlakeFor)
import Zinc.Target (Target (..), ghcFor, ghcPkgFor, hsc2hsFor, isWasm, parseTarget, targetTriple, toolPrefix)
import Zinc.Orchestrate (buildAndRun, lockDrift, orderMembers, parMapBounded, resolveTarget, runBuild, runBuildMember, runClean, runTests, runWarm)
import Zinc.Paths (pathsModuleName, synthesizePaths)
import Zinc.Report (BuildOutcome (..), CacheStats (..), PackageReport (..), PackageStatus (..), Timing (..), buildBreakdownLine, buildDataJson, buildSummaryLine, cacheStatsOf, fmtMs, packageReportJson, renderResolution, statusText, timingJson)
import Zinc.SysLibs (toNixpkgs)
import Zinc.Resolve (DepManifest (..), ResolvedDep (..), isBootLib, resolve, topoLevels, topoSort)
import Zinc.Version (newestTag, newestTagFor)
import Zinc.Lock (LockedPackage (..), Source (..), lockRepo, lockRev, parseLock, renderLock, srcKey)
import Zinc.Skill (LockedSkill (..), SkillDep (..), parseSkillLock, parseSkills, readSkillFrontmatter, renderSkillLock)
import Zinc.SkillCmd (renderSkillList, runSkillAdd, runSkillList, runSkillRemove, runSkillSync, skillRepoName, writeSkillLockEntry)
import Zinc.Metrics (MetricsRecord (..), appendMetrics, metricsLine, metricsPath)
import Zinc.Perf (CommandStats (..), PerfRecord (..), Regression (..), PerfSummary (..), decodeRecord, percentile, perfSummaryJson, renderPerf, summarize)
import Zinc.Scaffold (FileSpec (..), materialize, scaffoldNew, scaffoldWorkspace)

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
    (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "leaf" (Tag "v1") (Just leaf) [] []]))
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

    it "no-git diagnostic's nextAction is a copy-paste `zinc vendor` command (b1z.3)" $ do
      diagNextAction (toDiagnostic (DepNoGitRepo "colour"))
        `shouldSatisfy` maybe False (isInfixOf "zinc vendor colour")
      -- multiple targets stay one space-separated command
      diagNextAction (toDiagnostic (DepNoGitRepo "colour tf-random"))
        `shouldSatisfy` maybe False (isInfixOf "zinc vendor colour tf-random")

    it "renders a Diagnostic to JSON, omitting absent optional fields" $
      renderJson (diagnosticJson (toDiagnostic (ManifestParse "f.toml" "bad")))
        `shouldBe` "{\"code\":\"ZINC_MANIFEST_PARSE\",\"severity\":\"error\",\"title\":\"manifest parse error\",\"detail\":\"bad\",\"location\":{\"file\":\"f.toml\"},\"nextAction\":\"fix the TOML in the manifest\"}"

    it "tomlLocation lifts a leading line:col position onto the known file (hw6.5)" $ do
      tomlLocation "zinc.toml" "12:5: unexpected key"
        `shouldBe` SourceLocation "zinc.toml" (Just 12) (Just 5) Nothing Nothing Nothing
      -- No position in the detail -> degrade to a bare-file location.
      tomlLocation "zinc.toml" "malformed" `shouldBe` SourceLocation "zinc.toml" Nothing Nothing Nothing Nothing Nothing

    it "ghcLocation parses file:line:col, excerpt, and caret span from ghc stderr (hw6.5)" $ do
      let stderr =
            unlines
              [ "src/Foo.hs:10:7: error: [GHC-83865]"
              , "    Variable not in scope: bar"
              , "   |"
              , "10 |   foo = bar"
              , "   |         ^^^"
              ]
      ghcLocation stderr
        `shouldBe` Just (SourceLocation "src/Foo.hs" (Just 10) (Just 7) Nothing (Just 10) (Just "  foo = bar"))

    it "ghcLocation returns Nothing when stderr has no recognizable location (hw6.5)" $
      ghcLocation "ghc: panic! the impossible happened" `shouldBe` Nothing

    it "wraps output in the standard envelope (timing omitted when absent)" $
      renderJson (envelope "build" True (Just (JObject [("built", JInt 1)])) Nothing [])
        `shouldBe` "{\"zinc\":\"0.1.0.0\",\"command\":\"build\",\"ok\":true,\"data\":{\"built\":1},\"diagnostics\":[]}"

    it "renders the toolchain-missing guidance (auto-provision, gtv.2/y03.3)" $ do
      let d = toDiagnostic (ToolchainMissing "ghc")
      diagCode d `shouldBe` "ZINC_TOOLCHAIN_MISSING"
      exitCodeFor (ToolchainMissing "ghc") `shouldBe` ExitFailure 5
      -- now guides at Nix auto-provisioning, with `nix develop` only as the escape hatch
      diagNextAction d `shouldSatisfy` maybe False (isInfixOf "auto-provisions")

    it "assigns stable exit codes per category" $ do
      exitCodeFor (NoZincToml ".") `shouldBe` ExitFailure 2
      exitCodeFor (DepNoGitRepo "colour") `shouldBe` ExitFailure 3
      exitCodeFor (GhcCompile "p" "boom") `shouldBe` ExitFailure 4
      exitCodeFor NixAbsent `shouldBe` ExitFailure 5
      exitCodeFor (ContentHashMismatch "p" "a" "b") `shouldBe` ExitFailure 6

    it "escapes JSON strings" $
      renderJson (JString "a\"b\nc") `shouldBe` "\"a\\\"b\\nc\""

  describe "mergeManifestDependencies (minimal-diff add, zinc-91n.2)" $ do
    let manifest = unlines
          [ "[workspace]"
          , "members = [\".\"]"
          , "ghc = \"9.6.5\""
          , ""
          , "[dependencies]"
          , "# Each dependency vertically: rev pin + repo override."
          , ""
          , "[dependencies.zed]"
          , "rev = \"z1\""
          , "repo = \"https://example/zed.git\""
          , ""
          , "[dependencies.alpha]"
          , "vendored = \"1.0\""
          , "# -XSafe is applied automatically by the quirks table."
          ]
        zed   = Dependency "zed" (Rev "z1") (Just "https://example/zed.git") [] []
        alpha = Dependency "alpha" (Vendored "1.0") Nothing [] []
        mid   = Dependency "mid" (Rev "m1") (Just "https://example/mid.git") [] []

    it "is idempotent: re-writing the same deps preserves comments and ordering byte-for-byte" $
      -- The canonical writer would alphabetise (alpha before zed) and drop both
      -- comments; the minimal-diff editor must leave the file untouched.
      mergeManifestDependencies manifest [zed, alpha] `shouldBe` Right manifest

    it "appends a new dependency, keeping existing blocks (and their comments) verbatim" $ do
      let Right out = mergeManifestDependencies manifest [zed, alpha, mid]
      out `shouldSatisfy` ("# -XSafe is applied automatically by the quirks table." `isInfixOf`)
      out `shouldSatisfy` ("[dependencies.mid]" `isInfixOf`)
      -- author's ordering preserved: zed before alpha, new dep last
      let idx s = length (takeWhile (not . (s `isInfixOf`)) (lines out))
      idx "[dependencies.zed]" < idx "[dependencies.alpha]" `shouldBe` True
      idx "[dependencies.alpha]" < idx "[dependencies.mid]" `shouldBe` True

    it "drops a dependency removed from the desired set" $ do
      let Right out = mergeManifestDependencies manifest [alpha]
      out `shouldSatisfy` (not . ("[dependencies.zed]" `isInfixOf`))
      out `shouldSatisfy` ("[dependencies.alpha]" `isInfixOf`)

  describe "memberBuildDir (zinc-91n.8)" $ do
    it "collapses a flat (member \".\") workspace to a clean build path" $
      -- wsDir </> member == "." </> "." == "./." (System.FilePath keeps the dots),
      -- which un-normalised joins to "././.zinc/build". Must read as ".zinc/build".
      memberBuildDir ("." </> ".") `shouldBe` ".zinc/build"

    it "leaves a nested member path untouched" $
      memberBuildDir ("packages" </> "foo") `shouldBe` "packages/foo/.zinc/build"

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

    it "renders the timing block with the finer breakdown (totalMs, phases, breakdown, cache; nti.3)" $
      renderJson (timingJson (Timing 1234 [("closure", 900), ("member", 300)] [("fetch", 120), ("compile", 1500), ("link", 200)] (CacheStats 2 1 1 2)))
        `shouldBe` "{\"totalMs\":1234,\"phases\":{\"closure\":900,\"member\":300},\"breakdown\":{\"fetch\":120,\"compile\":1500,\"link\":200},\"cache\":{\"hits\":2,\"misses\":1,\"pkgsBuilt\":1,\"pkgsCached\":2}}"

    it "omits the breakdown object when no finer phases were measured (nti.3)" $
      renderJson (timingJson (Timing 1234 [("closure", 900)] [] (CacheStats 2 1 1 2)))
        `shouldBe` "{\"totalMs\":1234,\"phases\":{\"closure\":900},\"cache\":{\"hits\":2,\"misses\":1,\"pkgsBuilt\":1,\"pkgsCached\":2}}"

    it "renders the cumulative breakdown line, dropping zero phases (nti.3)" $ do
      buildBreakdownLine False (Timing 9 [] [("fetch", 0), ("compile", 8400), ("link", 2100)] (CacheStats 0 5 5 0))
        `shouldBe` Just "  breakdown \183 compile 8.4s \183 link 2.1s (cumulative)"
      buildBreakdownLine False (Timing 9 [] [] (CacheStats 5 0 0 5)) `shouldBe` Nothing

    it "includes the timing block in the envelope when present" $
      renderJson (envelope "build" True (Just (buildDataJson (BuildOutcome [] []))) (Just (timingJson (Timing 5 [] [] (CacheStats 0 0 0 0)))) [])
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
      r <- withRenderer (Human False False True) $ \sink -> do
        atomically (runSink sink (CompileStart "x"))
        atomically (runSink sink (CompileDone "x" 1 True))
        pure (42 :: Int)
      r `shouldBe` 42

    it "OutputFlags has the expected shape" $
      (ofJson (OutputFlags True False), ofQuiet (OutputFlags True False)) `shouldBe` (True, False)

    it "Plan is a tagged JSONL object carrying the closure total (hw6.2)" $
      renderJson (eventJson (Plan 47)) `shouldBe` "{\"event\":\"plan\",\"total\":47}"

  describe "human renderer (hw6.2)" $ do
    it "raw-ANSI style wraps when color is on and is a no-op when off" $ do
      style True [1, 32] "x" `shouldBe` "\ESC[1;32mx\ESC[0m"
      style False [1, 32] "x" `shouldBe` "x"
      greenBold False "Compiling" `shouldBe` "Compiling"

    it "verb right-aligns the status word in a 12-col gutter (plain when no color)" $ do
      verb False "Compiling" `shouldBe` "   Compiling"
      verb False "Resolving" `shouldBe` "   Resolving"

    it "progressLine shows [done/total] while the closure builds (hw6.2)" $
      progressLine False (RProg 47 23 "aeson") `shouldBe` "   Compiling aeson [23/47]"

    it "progressLine switches to Building once the closure is complete (members)" $
      progressLine False (RProg 47 47 "zinc") `shouldBe` "    Building zinc"

    it "humanError renders a minimal status line, caret block, and help (hw6.2)" $ do
      let d =
            Diagnostic
              { diagCode = "ZINC_GHC_COMPILE"
              , diagSeverity = SError
              , diagTitle = "compilation failed"
              , diagDetail = Just "app/Main.hs:3:22: error:\n    \8226 Couldn't match type 'Bool' with '[Char]'\n"
              , diagLocation = Just (SourceLocation "app/Main.hs" (Just 3) (Just 22) Nothing (Just 26) (Just "main = putStrLn (1 + True)"))
              , diagPackage = Just "demo"
              , diagNextAction = Just "fix the type"
              }
          out = humanError False d
      out `shouldSatisfy` isInfixOf "\10007 compilation failed  app/Main.hs:3:22"
      out `shouldSatisfy` isInfixOf "3 \9474 main = putStrLn (1 + True)"
      -- caret width = endCol(26) - col(22) = 4, annotated with the primary cause
      out `shouldSatisfy` isInfixOf "^^^^ Couldn't match type 'Bool' with '[Char]'"
      out `shouldSatisfy` isInfixOf "help: fix the type"

    it "rawToolOutput exposes the full compiler stderr for a build failure (rxa)" $ do
      rawToolOutput (GhcCompile "demo" "line1\nCould not find module 'Foo'\n  it is a member of the hidden package 'bar'\nfull -v dump")
        `shouldBe` Just "line1\nCould not find module 'Foo'\n  it is a member of the hidden package 'bar'\nfull -v dump"
    it "rawToolOutput has no raw tool output for non-compiler errors (rxa)" $
      rawToolOutput (NoZincToml "/x") `shouldBe` Nothing

    it "humanError degrades to status + detail when there is no source excerpt" $ do
      let out = humanError False (toDiagnostic (DepNoGitRepo "colour"))
      out `shouldSatisfy` isInfixOf "\10007 dependency has no git repository"
      out `shouldSatisfy` isInfixOf "help: "

    it "fmtMs renders sub-second as ms and >=1s as one-decimal seconds (hw6.3)" $ do
      fmtMs 450 `shouldBe` "450ms"
      fmtMs 3200 `shouldBe` "3.2s"
      fmtMs 1000 `shouldBe` "1.0s"

    it "buildSummaryLine is a cargo-style speed + cache summary (hw6.3)" $
      buildSummaryLine False (Timing 3200 [] [] (CacheStats 35 12 12 35))
        `shouldBe` "    Finished in 3.2s \183 47 packages (35 cached, 12 built)"

    it "buildSummaryLine singularizes a one-package closure (hw6.3)" $
      buildSummaryLine False (Timing 800 [] [] (CacheStats 1 0 0 1))
        `shouldBe` "    Finished in 800ms \183 1 package (1 cached, 0 built)"

  describe "closure discovery (49o)" $ do
    it "parses the `closure` subcommand (+ --json)" $ do
      parseArgs ["closure", "aeson"] `shouldBe` Right (OutputFlags False False, Closure "aeson")
      parseArgs ["closure", "aeson", "--json"] `shouldBe` Right (OutputFlags True False, Closure "aeson")

    it "parses the `vendor` subcommand with one or more packages (b1z.2)" $ do
      parseArgs ["vendor", "colour"] `shouldBe` Right (OutputFlags False False, Vendor ["colour"])
      parseArgs ["vendor", "colour", "tf-random"] `shouldBe` Right (OutputFlags False False, Vendor ["colour", "tf-random"])
      parseArgs ["vendor"] `shouldSatisfy` isLeft -- at least one package required

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
      let ws0 = WorkspaceManifest [] "9.6.5" [Dependency "aeson" (Tag "v2") Nothing [] [], Dependency "pp" Latest (Just "r/mono#pp") [] []]
          ws1 = enrichWithRepos ws0 [("aeson", "r/aeson"), ("pp", "r/mono"), ("scientific", "r/sci")]
      wsDependencies ws1
        `shouldBe` [ Dependency "aeson" (Tag "v2") (Just "r/aeson") [] [] -- discovered (no prior repo)
                   , Dependency "pp" Latest (Just "r/mono#pp") [] []      -- override kept (not clobbered by "r/mono")
                   , Dependency "scientific" Latest (Just "r/sci") [] []  -- discovered
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

    it "setManifestDependencies keeps a flat project's [package]/[build.*] when add rewrites deps (lnh)" $ do
      -- the exact shape `zinc new <name>` scaffolds (flat single-package), then
      -- `zinc add` injects a new [dependencies.dep]; package/build must survive.
      let src = unlines ["[workspace]", "members = [\".\"]", "ghc = \"9.6.5\"", "", "[package]", "name = \"app\"", "version = \"0.1.0\"", "", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "", "[dependencies]"]
          out = either error id (setManifestDependencies src [Dependency "dep" Latest (Just "r/dep") [] []])
      all (`isInfixOf` out)
        ["[package]", "name = \"app\"", "[build.exe.app]", "main = \"Main.hs\"", "[dependencies.dep]", "r/dep"]
        `shouldBe` True
      -- and the result round-trips: still parses, with the new dep present
      (depName <$> either (const []) wsDependencies (parseWorkspace out)) `shouldBe` ["dep"]

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
      parseArgs ["warm"] `shouldBe` Right (OutputFlags False False, Warm Nothing)
      parseArgs ["warm", "--json"] `shouldBe` Right (OutputFlags True False, Warm Nothing)
      parseArgs ["build", "--deps-only"] `shouldBe` Right (OutputFlags False False, Warm Nothing)
      parseArgs ["build", "--deps-only", "--json"] `shouldBe` Right (OutputFlags True False, Warm Nothing)

    it "builds the closure only (empty for a depless workspace)" $ do
      let d = "/tmp/zinc-warm-test"
      createDirectoryIfMissing True d
      writeFileIn (d </> "zinc.toml") (renderWorkspace (WorkspaceManifest [] "9.6.5" []))
      r <- runWarm nullSink d Nothing
      r `shouldBe` Right []

    it "fails with NoZincToml outside a workspace" $ do
      let d = "/tmp/zinc-warm-nows"
      createDirectoryIfMissing True d
      r <- runWarm nullSink d Nothing
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

    it "renders status as JSON (incl. installed skills; lmm)" $
      renderJson (statusJson "9.6.5" ["packages/app"] [DepStatus "colour" "abc1234" True] ["aeson"] ["brainstorming"])
        `shouldBe` "{\"ghc\":\"9.6.5\",\"members\":[\"packages/app\"],\"dependencies\":[{\"name\":\"colour\",\"ref\":\"abc1234\",\"cached\":true}],\"drift\":[\"aeson\"],\"skills\":[\"brainstorming\"]}"

    it "runStatus surfaces installed skill names from the lock (lmm)" $ do
      let d = "/tmp/zinc-status-skills"
      stale <- doesDirectoryExist d
      when stale (removeDirectoryRecursive d)
      createDirectoryIfMissing True d
      writeFile (d </> "zinc.toml") "[workspace]\nmembers = [\".\"]\nghc = \"9.6.5\"\n"
      writeFile (d </> "zinc.lock") (renderSkillLock [LockedSkill "brainstorming" "r/b" "rev" "sha256:x"])
      r <- runStatus d
      either (const []) (\(_, _, _, _, sk) -> sk) r `shouldBe` ["brainstorming"]

    it "renders the closure graph: nodes, edges, topo levels" $ do
      let locks = [LockedPackage "a" (GitSource "r/a" "ra") "sha256:x" ["b"] [] [], LockedPackage "b" (GitSource "r/b" "rb") "sha256:y" [] [] []]
      renderJson (graphJson locks)
        `shouldBe` "{\"nodes\":[\"a\",\"b\"],\"edges\":[{\"from\":\"a\",\"to\":\"b\"}],\"levels\":[[\"b\"],[\"a\"]]}"

    it "explains a package's provenance (who requires it, at which rev)" $ do
      let locks = [LockedPackage "a" (GitSource "r/a" "ra") "sha256:x" ["b"] [] [], LockedPackage "b" (GitSource "r/b" "rb") "sha256:y" [] [] []]
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
      metricsLine (MetricsRecord "build" "" "sha256:abc" "9.6.5" "2026-06-04T00:00:00Z" (Timing 7 [("closure", 4)] [] (CacheStats 1 0 0 1)) [])
        `shouldBe` "{\"timestamp\":\"2026-06-04T00:00:00Z\",\"command\":\"build\",\"argsSummary\":\"\",\"lockHash\":\"sha256:abc\",\"ghcVersion\":\"9.6.5\",\"timing\":{\"totalMs\":7,\"phases\":{\"closure\":4},\"cache\":{\"hits\":1,\"misses\":0,\"pkgsBuilt\":0,\"pkgsCached\":1}}}\n"

    it "includes per-package timing in the record when present (nti)" $
      metricsLine (MetricsRecord "build" "" "h" "9.6.5" "t" (Timing 5 [] [] (CacheStats 0 1 1 0)) [("alpha", 900)])
        `shouldBe` "{\"timestamp\":\"t\",\"command\":\"build\",\"argsSummary\":\"\",\"lockHash\":\"h\",\"ghcVersion\":\"9.6.5\",\"timing\":{\"totalMs\":5,\"phases\":{},\"cache\":{\"hits\":0,\"misses\":1,\"pkgsBuilt\":1,\"pkgsCached\":0}},\"packages\":[{\"name\":\"alpha\",\"timeMs\":900}]}\n"

    it "appends (never rewrites) records to .zinc/metrics.jsonl" $ do
      let d = "/tmp/zinc-metrics-test"
          rec n = MetricsRecord "build" n "h" "9.6.5" "t" (Timing 1 [] [] (CacheStats 0 0 0 0)) []
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
      parseArgs ["build"] `shouldBe` Right (OutputFlags False False, Build Nothing Nothing Nothing)

    it "parses `build <member>` with a target" $
      parseArgs ["build", "mylib"] `shouldBe` Right (OutputFlags False False, Build (Just "mylib") Nothing Nothing)

    it "parses `build --json` (machine surface)" $ do
      parseArgs ["build", "--json"] `shouldBe` Right (OutputFlags True False, Build Nothing Nothing Nothing)
      parseArgs ["build", "mylib", "--json"] `shouldBe` Right (OutputFlags True False, Build (Just "mylib") Nothing Nothing)

    it "parses `build --target wasm32-wasi` (zinc-9po.3)" $ do
      parseArgs ["build", "--target", "wasm32-wasi"] `shouldBe` Right (OutputFlags False False, Build Nothing Nothing (Just "wasm32-wasi"))
      parseArgs ["build", "mylib", "--target", "native"] `shouldBe` Right (OutputFlags False False, Build (Just "mylib") Nothing (Just "native"))

    it "parses the `--ghc <version>` override on build and warm (ey4)" $ do
      parseArgs ["build", "--ghc", "9.10"] `shouldBe` Right (OutputFlags False False, Build Nothing (Just "9.10") Nothing)
      parseArgs ["build", "mylib", "--ghc", "9.8.2"] `shouldBe` Right (OutputFlags False False, Build (Just "mylib") (Just "9.8.2") Nothing)
      parseArgs ["warm", "--ghc", "9.10"] `shouldBe` Right (OutputFlags False False, Warm (Just "9.10"))
      parseArgs ["build", "--deps-only", "--ghc", "9.10"] `shouldBe` Right (OutputFlags False False, Warm (Just "9.10"))

    it "parses `new <name>` (flat default) and `new --workspace <name>`" $ do
      parseArgs ["new", "myapp"] `shouldBe` Right (OutputFlags False False, New "myapp" False)
      parseArgs ["new", "--workspace", "myapp"] `shouldBe` Right (OutputFlags False False, New "myapp" True)

    it "parses `add <pkg>` with its argument" $
      parseArgs ["add", "aeson"] `shouldBe` Right (OutputFlags False False, Add "aeson")

    it "parses `clean`" $
      parseArgs ["clean"] `shouldBe` Right (OutputFlags False False, Clean)

    it "parses `gc`" $
      parseArgs ["gc"] `shouldBe` Right (OutputFlags False False, Gc)

    it "parses the version command and the --version/-V flags (gtv.3)" $ do
      parseArgs ["version"] `shouldBe` Right (OutputFlags False False, Version)
      parseArgs ["--version"] `shouldBe` Right (OutputFlags False False, Version)
      parseArgs ["-V"] `shouldBe` Right (OutputFlags False False, Version)

    it "zincVersionLine reads `zinc <semver>` (plus optional commit/date)" $
      zincVersionLine `shouldSatisfy` isInfixOf ("zinc " ++ zincVersion)

    it "parses `outdated` and `outdated --all` (90j.1)" $ do
      parseArgs ["outdated"] `shouldBe` Right (OutputFlags False False, Outdated False)
      parseArgs ["outdated", "--all"] `shouldBe` Right (OutputFlags False False, Outdated True)

    it "parses the nested `cache push` (vwn.5)" $
      parseArgs ["cache", "push"] `shouldBe` Right (OutputFlags False False, CachePush)

    it "no args / help / --help parse to the friendly Help overview (hw6.6)" $ do
      parseArgs [] `shouldBe` Right (OutputFlags False False, Help)
      parseArgs ["help"] `shouldBe` Right (OutputFlags False False, Help)
      parseArgs ["--help"] `shouldBe` Right (OutputFlags False False, Help)

    it "helpOverview lists the common commands grouped" $
      all (`isInfixOf` helpOverview) ["Usage: zinc", "new", "build", "run", "Getting started:"]
        `shouldBe` True

    it "parses `repl` with no target" $
      parseArgs ["repl"] `shouldBe` Right (OutputFlags False False, Repl Nothing)

    it "parses `repl <target>`" $
      parseArgs ["repl", "mylib"] `shouldBe` Right (OutputFlags False False, Repl (Just "mylib"))

    it "parses `test` with no target" $
      parseArgs ["test"] `shouldBe` Right (OutputFlags False False, Test Nothing)

    it "parses `update` (with --dry-run)" $ do
      parseArgs ["update"] `shouldBe` Right (OutputFlags False False, Update Nothing False)
      parseArgs ["update", "--dry-run"] `shouldBe` Right (OutputFlags False False, Update Nothing True)

    it "parses `run [TARGET] [-- ARGS]` (target first, then program args)" $ do
      parseArgs ["run"] `shouldBe` Right (OutputFlags False False, Run Nothing [] Nothing)
      parseArgs ["run", "web"] `shouldBe` Right (OutputFlags False False, Run (Just "web") [] Nothing)
      parseArgs ["run", "web", "--", "a", "b"] `shouldBe` Right (OutputFlags False False, Run (Just "web") ["a", "b"] Nothing)
      parseArgs ["run", "--target", "wasm32-wasi"] `shouldBe` Right (OutputFlags False False, Run Nothing [] (Just "wasm32-wasi"))

    it "rejects an unknown subcommand" $
      parseArgs ["frobnicate"] `shouldSatisfy` isLeft

  describe "scaffoldNew (flat single-package, 6hf.1)" $ do
    let files = scaffoldNew "myapp"

    it "writes a flat root manifest: implicit one-member workspace (member \".\")" $
      (isInfixOf "members = [\".\"]" <$> bodyOf "zinc.toml" files)
        `shouldBe` Just True

    it "puts [package] in the root zinc.toml — no packages/<name>/ nesting" $ do
      (isInfixOf "name = \"myapp\"" <$> bodyOf "zinc.toml" files) `shouldBe` Just True
      bodyOf "packages/myapp/zinc.toml" files `shouldBe` Nothing

    it "writes app/Main.hs at the root" $ do
      bodyOf "app/Main.hs" files `shouldSatisfy` isJust
      bodyOf "packages/myapp/app/Main.hs" files `shouldBe` Nothing

    it "scaffolds a .gitignore covering .zinc/ and Nix result symlinks (6hf.3)" $ do
      let gi = maybe "" id (bodyOf ".gitignore" files)
      all (`isInfixOf` gi) [".zinc/", "result", "*.hi", "*.o"] `shouldBe` True

    it "ships a zinc-managed flake.nix: pinned GHC + preprocessors (6hf.2)" $ do
      let fl = maybe "" id (bodyOf "flake.nix" files)
      all (`isInfixOf` fl) ["ghc965", "alex", "happy", "devShells"] `shouldBe` True

  describe "scaffoldWorkspace (--workspace multi-member, 6hf.1)" $ do
    let files = scaffoldWorkspace "myapp"

    it "writes a workspace manifest that lists the nested member" $
      (isInfixOf "members = [\"packages/myapp\"]" <$> bodyOf "zinc.toml" files)
        `shouldBe` Just True

    it "writes a nested member manifest + Main.hs" $ do
      (isInfixOf "name = \"myapp\"" <$> bodyOf "packages/myapp/zinc.toml" files) `shouldBe` Just True
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

    it "reads a vendored dependency as a Vendored pin (b1z)" $ do
      let src = unlines ["[workspace]", "members = []", "ghc = \"9.6.5\"", "[dependencies.colour]", "vendored = \"2.3.6\""]
      (refOf "colour" <$> parseWorkspace src) `shouldBe` Right (Just (Vendored "2.3.6"))

    it "round-trips a vendored dependency through render . parse (b1z)" $ do
      -- a vendored pin must NOT collapse to the bare-string shorthand: that
      -- would parse back as a git tag, silently losing the source kind.
      let ws = WorkspaceManifest [] "9.6.5" [Dependency "colour" (Vendored "2.3.6") Nothing [] []]
      (wsDependencies <$> parseWorkspace (renderWorkspace ws)) `shouldBe` Right (wsDependencies ws)

    it "addVendored preserves a dep's existing ghc-options when re-pinning (mvj)" $ do
      -- re-pinning colour (git, -XSafe) as vendored must keep its -XSafe override.
      let ws0 = WorkspaceManifest [] "9.6.5" [Dependency "colour" (Rev "abc") (Just "r/colour") ["-XSafe"] []]
          ws1 = addVendored ws0 "colour" "2.3.6"
      (depRef <$> find ((== "colour") . depName) (wsDependencies ws1)) `shouldBe` Just (Vendored "2.3.6")
      (depGhcOptions <$> find ((== "colour") . depName) (wsDependencies ws1)) `shouldBe` Just ["-XSafe"]
      (depRepo <$> find ((== "colour") . depName) (wsDependencies ws1)) `shouldBe` Just Nothing

    it "fails on a missing [workspace] table" $
      parseWorkspace "[dependencies]\n" `shouldSatisfy` isLeft

  describe "dependency ghc-options (vertical schema)" $ do
    it "reads per-dependency extra ghc flags from each dep's ghc-options" $ do
      let src = unlines ["[workspace]", "members = []", "ghc = \"9.6.5\"", "[dependencies.colour]", "tag = \"v1\"", "ghc-options = [\"-XSafe\"]"]
      (sort . depGhcOptionsOf <$> parseWorkspace src) `shouldBe` Right [("colour", ["-XSafe"])]

    it "is empty when no dependency sets ghc-options" $
      (depGhcOptionsOf <$> parseWorkspace "[workspace]\nmembers = []\nghc = \"9.6.5\"\n") `shouldBe` Right []

  describe "dependency manual cabal flags (zinc-iaj.2)" $ do
    it "parses a dep's flags inline table into depFlags" $ do
      let src = unlines ["[workspace]", "members = []", "ghc = \"9.6.5\"", "[dependencies.postgresql-libpq]", "tag = \"v1\"", "flags = { use-pkg-config = true }"]
          flagsOfDep = fmap (depFlags <$>) . fmap (find ((== "postgresql-libpq") . depName) . wsDependencies)
      flagsOfDep (parseWorkspace src) `shouldBe` Right (Just [("use-pkg-config", True)])

    it "derives depFlagsOf from each dep's flags table (false flags kept)" $ do
      let src = unlines ["[workspace]", "members = []", "ghc = \"9.6.5\"", "[dependencies.foo]", "tag = \"v1\"", "flags = { a = true, b = false }"]
      (map (fmap sort) . depFlagsOf <$> parseWorkspace src) `shouldBe` Right [("foo", [("a", True), ("b", False)])]

    it "is empty when no dependency sets flags" $
      (depFlagsOf <$> parseWorkspace "[workspace]\nmembers = []\nghc = \"9.6.5\"\n") `shouldBe` Right []

    it "round-trips a dependency's flags through render . parse" $ do
      let ws = WorkspaceManifest [] "9.6.5" [Dependency "postgresql-libpq" (Tag "v1") Nothing [] [("use-pkg-config", True)]]
      (wsDependencies <$> parseWorkspace (renderWorkspace ws)) `shouldBe` Right (wsDependencies ws)

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

    it "parses the [package] in the flat manifest produced by scaffoldNew" $
      let body = maybe "" id (bodyOf "zinc.toml" (scaffoldNew "demo"))
       in (pkgName <$> parseMember body) `shouldBe` Right "demo"

  describe "Zinc.Lock" $ do
    let pkgs =
          [ LockedPackage
              { lockName = "aeson"
              , lockSource = GitSource "https://github.com/haskell/aeson" "a1b2c3d"
              , lockSha256 = "sha256-Xk9"
              , lockDepends = ["scientific", "witherable"]
              , lockFlags = []
              , lockSystemLibs = []
              }
          , LockedPackage
              { lockName = "scientific"
              , lockSource = GitSource "https://github.com/basvandijk/scientific" "f4e5d6"
              , lockSha256 = "sha256-Yz1"
              , lockDepends = []
              , lockFlags = []
              , lockSystemLibs = []
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

    it "parses and renders a vendored (tarball) entry, round-tripping (b1z)" $ do
      let vp = [LockedPackage "colour" (TarballSource "2.3.6") "sha256:abc" ["base"] [] []]
          toml =
            unlines
              [ "[[locked]]"
              , "name = \"colour\""
              , "vendored = \"2.3.6\""
              , "sha256 = \"sha256:abc\""
              , "depends = [\"base\"]"
              ]
      parseLock toml `shouldBe` Right vp
      parseLock (renderLock vp) `shouldBe` Right vp
      (lockRepo (head vp), lockRev (head vp)) `shouldBe` ("", "2.3.6")

    it "round-trips a package's manual flags through render . parse (zinc-iaj.2)" $ do
      let fp = [LockedPackage "postgresql-libpq" (GitSource "r/pq" "rev") "sha256:x" ["base"] [("use-pkg-config", True)] []]
      parseLock (renderLock fp) `shouldBe` Right fp

    it "renders a package's flags as a flags inline table" $ do
      let fp = [LockedPackage "pq" (GitSource "r/pq" "rev") "sha256:x" [] [("use-pkg-config", True)] []]
      ("flags = { use-pkg-config = true }" `isInfixOf` renderLock fp) `shouldBe` True

    it "round-trips + renders a package's system-libs (zinc-389)" $ do
      let sp = [LockedPackage "postgresql-libpq" (GitSource "r/pq" "rev") "sha256:x" ["base"] [] ["postgresql"]]
      parseLock (renderLock sp) `shouldBe` Right sp
      ("system-libs = [\"postgresql\"]" `isInfixOf` renderLock sp) `shouldBe` True

    it "treats an empty/absent [[locked]] array as no packages" $
      parseLock "" `shouldBe` Right []

  describe "gitInitIfNeeded (6hf.3)" $
    it "inits a repo in a fresh dir, then is a no-op when already inside one" $ do
      let d = "/tmp/zinc-gitinit-test"
      stale <- doesDirectoryExist d
      when stale $ removeDirectoryRecursive d
      createDirectoryIfMissing True d
      before <- isInsideRepo d
      r1 <- gitInitIfNeeded d
      hasGit <- doesDirectoryExist (d </> ".git")
      inside <- isInsideRepo d
      r2 <- gitInitIfNeeded d -- already a repo: no-op
      (before, isRight r1, hasGit, inside, isRight r2) `shouldBe` (False, True, True, True, True)

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

    it "computes the canonical store source path (repo-keyed slug; qln)" $ do
      -- the path is <root>/src/<basename>-<hash>-<rev>: greppable basename, a
      -- short hash for uniqueness, then the rev.
      let p = storeSrcPath "/store" "https://github.com/o/effectful.git" "abc123"
      ("/store/src/effectful.git-" `isPrefixOf` p) `shouldBe` True
      ("-abc123" `isSuffixOf` p) `shouldBe` True

    it "shares one src checkout across a monorepo's sub-packages, splits distinct repos (qln)" $ do
      let repo = "https://github.com/haskell-effectful/effectful.git"
          -- effectful + effectful-core: same repo (one with a #subdir), same rev
          dirOf k = takeFileName (storeSrcPath "/store" k "rr")
      -- the #subdir is stripped before keying, so sub-packages collapse to one dir
      dirOf repo `shouldBe` dirOf (repo ++ "#effectful-core")
      -- a different repo at the same rev keys to a different checkout
      (dirOf repo == dirOf "https://github.com/o/other.git") `shouldBe` False

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
            , compExtraLibs = []
            , compIncludeDirs = []
            , compCppOptions = []
            , compCSources = []
            , compReexports = []
            , compWasmExports = []
            , compFromCabal = False
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
    let dep n r = Dependency n r Nothing [] []
        boot = (`elem` ["base", "text", "bytestring", "containers"])
        fetchFrom fix n _ _ = pure (maybe (Left (OtherError ("missing: " ++ n))) Right (lookup n fix))
        noDiscover _ = pure Nothing
        run fix deps reg =
          runIdentity (resolve boot (fetchFrom fix) noDiscover [] deps reg)
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

    it "drops a package's self-reference from its closure deps (sublibraries, ffm)" $ do
      -- a real .cabal can list its own name (internal sub-library, e.g.
      -- attoparsec); that self-edge must not enter the closure or topo sort.
      let fix =
            [ ("a", DepManifest [dep "a" Latest, dep "b" Latest] [("b", "r/b")])
            , ("b", DepManifest [] [])
            ]
          r = run fix [dep "a" Latest] [("a", "r/a")]
      (rdDepends <$> findRD "a" r) `shouldBe` Just ["b"]
      (sort . map rdName <$> r) `shouldBe` Right ["a", "b"]

    it "discovers a transitive dep's repo via the injected fallback (Hackage, ffm)" $ do
      -- 'a' (root-pinned) declares 'b' with NO registry anywhere; the discovery
      -- function (production: Hackage source-repository) supplies b's repo, so
      -- the closure resolves without b being hand-listed.
      let fix =
            [ ("a", DepManifest [dep "b" Latest] [])
            , ("b", DepManifest [] [])
            ]
          discover n = pure (lookup n [("b", "hackage/b")])
          r = runIdentity (resolve boot (fetchFrom fix) discover [] [dep "a" Latest] [("a", "r/a")])
      (sort . map rdName <$> r) `shouldBe` Right ["a", "b"]
      (rdRepo <$> (r >>= maybe (Left (OtherError "no b")) Right . find ((== "b") . rdName)))
        `shouldBe` Right "hackage/b"

    it "resolves a vendored dep with no repo, skipping registry/Hackage discovery (b1z)" $ do
      -- 'colour' is vendored: it is in no registry and discovery returns Nothing,
      -- yet it still resolves (its source is a pinned Hackage tarball, not a git
      -- repo). Without the short-circuit this would fail as NoRepoInRegistry.
      let fix =
            [ ("a", DepManifest [dep "colour" (Vendored "2.3.6")] [])
            , ("colour", DepManifest [dep "base" Latest] [])
            ]
          r = run fix [dep "a" Latest] [("a", "r/a")]
      (sort . map rdName <$> r) `shouldBe` Right ["a", "colour"]
      ((\d -> (rdRepo d, rdRef d)) <$> findRD "colour" r) `shouldBe` Just ("", Vendored "2.3.6")

    it "soft-pins a walked name to its pinned ref (90j.3)" $ do
      -- 'a' (root) declares 'b'; a soft pin holds 'b' at a specific rev.
      let fix = [("a", DepManifest [dep "b" Latest] [("b", "r/b")]), ("b", DepManifest [] [])]
          r = runIdentity (resolve boot (fetchFrom fix) noDiscover [("b", Rev "pinned-rev")] [dep "a" Latest] [("a", "r/a")])
      (rdRef <$> findRD "b" r) `shouldBe` Just (Rev "pinned-rev")

    it "a soft pin never forces an unrequired dep into the closure (90j.3)" $ do
      -- 'a' depends on nothing; pinning 'gone' must NOT pull it in (so deps a new
      -- version drops fall out under `update <pkg>`).
      let fix = [("a", DepManifest [] [])]
          r = runIdentity (resolve boot (fetchFrom fix) noDiscover [("gone", Rev "x")] [dep "a" Latest] [("a", "r/a")])
      (sort . map rdName <$> r) `shouldBe` Right ["a"]

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
          ( [Dependency "aeson" (Tag "v2") (Just "r/aeson") [] [], Dependency "scientific" Latest (Just "r/sci") [] []]
          , [("aeson", "r/aeson"), ("scientific", "r/sci")]
          )

    it "defaults to empty when the sections are absent" $
      parseDependencies "[package]\nname = \"x\"\nversion = \"1\"" `shouldBe` Right ([], [])

  describe "gitFetchManifest" $ do
    repo <- runIO setupDepRepo

    it "clones a dep at a ref and parses its manifest" $ do
      r <- gitFetchManifest "/tmp/zinc-fetch-store" "9.6.5" [] "dep" repo (Tag "v1")
      r `shouldBe` Right (DepManifest [Dependency "aeson" (Tag "v2") (Just "r/aeson") [] []] [("aeson", "r/aeson")])

    it "resolves a Latest ref to the newest tag and parses its manifest" $ do
      r <- gitFetchManifest "/tmp/zinc-fetch-store" "9.6.5" [] "dep" repo Latest
      r `shouldBe` Right (DepManifest [Dependency "aeson" (Tag "v2") (Just "r/aeson") [] []] [("aeson", "r/aeson")])

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
      r <- gitFetchManifest "/tmp/zinc-fetch-store" "9.6.5" [] "up" cabalRepo (Rev rev)
      -- repos come from the root registry, so the derived manifest carries none
      r `shouldBe` Right (DepManifest [Dependency "base" Latest Nothing [] [], Dependency "containers" Latest Nothing [] [], Dependency "prettyprinter" Latest Nothing [] []] [])

    it "finalizes the closure with the dep's manual flags so a flag-toggled build-depends picks the flag-selected provider (iaj.2)" $ do
      -- A .cabal whose library build-depends is gated on a manual flag, exactly
      -- like postgresql-libpq's use-pkg-config (false -> *-configure, true ->
      -- *-pkgconfig). The closure MUST resolve the same provider the build will,
      -- or zinc fetches/builds the wrong (here, unbuildable) dependency.
      let flagRepo = "/tmp/zinc-fetch-flag-dep"
      stale <- doesDirectoryExist flagRepo
      when stale $ removeDirectoryRecursive flagRepo
      writeFileIn (flagRepo ++ "/pglibpq.cabal") (unlines
        [ "cabal-version: 2.4"
        , "name: pglibpq"
        , "version: 1.0"
        , "flag use-pkg-config"
        , "  default: False"
        , "  manual: True"
        , "library"
        , "  exposed-modules: PgLibPQ"
        , "  if flag(use-pkg-config)"
        , "    build-depends: base, pglibpq-pkgconfig"
        , "  else"
        , "    build-depends: base, pglibpq-configure"
        ])
      let git args = readProcess "git" ("-C" : flagRepo : args) ""
      _ <- git ["init", "--quiet"]
      _ <- git ["config", "user.email", "t@e"]
      _ <- git ["config", "user.name", "T"]
      _ <- git ["add", "."]
      _ <- git ["commit", "--quiet", "-m", "pglibpq"]
      rev <- trimStr <$> git ["rev-parse", "HEAD"]
      -- Without the flag: default (False) -> the *-configure provider.
      off <- gitFetchManifest "/tmp/zinc-fetch-store" "9.6.5" [] "pglibpq" flagRepo (Rev rev)
      fmap (map depName . dmDeps) off `shouldBe` Right ["base", "pglibpq-configure"]
      -- With use-pkg-config=true threaded in (keyed by package name) -> the
      -- *-pkgconfig provider, the same one the build's finalize selects.
      on <- gitFetchManifest "/tmp/zinc-fetch-store" "9.6.5" [("pglibpq", [("use-pkg-config", True)])] "pglibpq" flagRepo (Rev rev)
      fmap (map depName . dmDeps) on `shouldBe` Right ["base", "pglibpq-pkgconfig"]

  describe "newestTag" $ do
    it "picks the highest semver tag (numeric, not lexical)" $
      newestTag ["v1.0.0", "v1.2.0", "v1.10.0", "v2.0.0"] `shouldBe` Just "v2.0.0"

    it "orders 1.10 above 1.2" $
      newestTag ["v1.2.0", "v1.10.0"] `shouldBe` Just "v1.10.0"

    it "ignores non-version tags" $
      newestTag ["nightly", "v1.0.0", "latest"] `shouldBe` Just "v1.0.0"

    it "is Nothing when there are no version tags" $
      newestTag ["nightly", "HEAD"] `shouldBe` Nothing

    it "prefers a package-scoped tag over stale global tags in a monorepo subdir (ffm.3)" $ do
      -- haskell/vector carries both vector-stream-* and pre-split v0.12.* tags;
      -- Latest for the vector-stream SUBDIR must pick its own newest scoped tag,
      -- not the global one (which belongs to a sibling / pre-split state).
      let tags = ["v0.12.3.1", "vector-0.13.2.0", "vector-stream-0.1.0.0", "vector-stream-0.1.0.1"]
      newestTagFor (Just "vector-stream") True tags `shouldBe` Just "vector-stream-0.1.0.1"
      newestTagFor (Just "vector") True tags `shouldBe` Just "vector-0.13.2.0"

    it "a monorepo subdir falls back to bare version tags when it has no scoped tag (ffm.3)" $
      newestTagFor (Just "vector-stream") True ["v0.12.3.1", "v0.13.0.0"] `shouldBe` Just "v0.13.0.0"

    it "a standalone repo considers BOTH scoped and bare tags, newest wins (myx)" $ do
      -- hashable's repo tags its releases bare (v1.5.1.0) but also carries one
      -- stale scoped tag (hashable-1.3.2.0). As a standalone (non-subdir) repo,
      -- the newest across both must win — not the stale scoped tag, which would
      -- pick a pre-text-2.0 hashable incompatible with the boot text.
      let tags = ["v1.4.7.0", "hashable-1.3.2.0", "v1.5.0.0", "v1.5.1.0"]
      newestTagFor (Just "hashable") False tags `shouldBe` Just "v1.5.1.0"

    it "newestTag is newestTagFor with no package scope, non-subdir" $
      newestTagFor Nothing False ["v1.2.0", "v1.10.0"] `shouldBe` Just "v1.10.0"

  describe "Zinc.Outdated.classify (90j.1)" $ do
    it "flags a newer version, distinguishing minor from major jumps" $ do
      classify "v1.2.0" (Just "v1.3.0") `shouldBe` Behind False
      classify "v1.2.0" (Just "v2.0.0") `shouldBe` Behind True

    it "is up-to-date when current is the newest or newer" $ do
      classify "v1.2.0" (Just "v1.2.0") `shouldBe` UpToDate
      classify "v1.5.0" (Just "v1.2.0") `shouldBe` UpToDate

    it "is unknown for a bare commit or a missing upstream version" $ do
      classify "a1b2c3d" (Just "v1.2.0") `shouldBe` Unknown
      classify "v1.2.0" Nothing `shouldBe` Unknown

  describe "Zinc.Delta.closureDelta (90j.2)" $ do
    let mk n v = LockedPackage n (GitSource ("r/" ++ n) v) "sha256:x" [] [] []
    it "reports changed (with major flag), added, and removed — incl. ripples" $ do
      let old = [mk "a" "v1.0.0", mk "b" "v2.0.0", mk "gone" "v1.0.0"]
          new = [mk "a" "v1.1.0", mk "b" "v3.0.0", LockedPackage "new" (TarballSource "0.5") "sha256:x" [] [] []]
          d = closureDelta old new
      map chName (cdChanged d) `shouldBe` ["a", "b"]
      map chMajor (cdChanged d) `shouldBe` [False, True]
      map fst (cdAdded d) `shouldBe` ["new"]
      map fst (cdRemoved d) `shouldBe` ["gone"]

    it "is empty when the closure is unchanged" $
      isEmptyDelta (closureDelta [mk "a" "v1"] [mk "a" "v1"]) `shouldBe` True

  describe "Zinc.CacheBackend (vwn.3)" $ do
    it "builds the content-addressed artifact URL (trailing slash normalised)" $ do
      artifactUrl "https://cache.example.com" "abc123" `shouldBe` "https://cache.example.com/abc123.tar.gz"
      artifactUrl "https://cache.example.com/" "abc123" `shouldBe` "https://cache.example.com/abc123.tar.gz"

    it "classifies a curl exit: success / Miss(4xx) / transport error" $ do
      curlOutcome ExitSuccess "" `shouldBe` Right ()
      curlOutcome (ExitFailure 22) "" `shouldBe` Left Miss
      case curlOutcome (ExitFailure 7) "refused" of
        Left (PullFailed _) -> pure ()
        other -> expectationFailure ("expected PullFailed, got " ++ show other)

    it "pulls + unpacks an artifact over file:// into pkg/<key>/, miss otherwise" $ do
      let base = "/tmp/zinc-cb"; store = "/tmp/zinc-cb-store"; key = "deadbeefkey"
      forM_ [base, store] $ \d -> doesDirectoryExist d >>= \e -> when e (removeDirectoryRecursive d)
      createDirectoryIfMissing True (base ++ "/art")
      writeFile (base ++ "/art/package.conf") "name: demo\n"
      _ <- readProcess "tar" ["-czf", base ++ "/" ++ key ++ ".tar.gz", "-C", base ++ "/art", "."] ""
      let backend = httpBackend ("file://" ++ base)
      out <- cbPull backend key store
      got <- readFile (storePkgPath store key </> "package.conf")
      miss <- cbPull backend "no-such-key" store
      (out, "name: demo" `isInfixOf` got, miss /= Pulled) `shouldBe` (Pulled, True, True)

    it "round-trips an artifact: push to file:// then pull into a fresh store (vwn.5)" $ do
      let base = "/tmp/zinc-cb-rt"; store = base ++ "/store"; store2 = base ++ "/store2"
          cache = base ++ "/cache"; key = "roundtripkey"
      doesDirectoryExist base >>= \e -> when e (removeDirectoryRecursive base)
      createDirectoryIfMissing True (storePkgPath store key)
      writeFile (storePkgPath store key </> "package.conf") "name: rt\n"
      createDirectoryIfMissing True cache -- a file:// upload target dir must exist
      let be = httpBackend ("file://" ++ cache)
      pushed <- cbPush be key store
      out <- cbPull be key store2
      got <- readFile (storePkgPath store2 key </> "package.conf")
      (pushed, out, "name: rt" `isInfixOf` got) `shouldBe` (Right (), Pulled, True)

    it "parses a [cache] table: urls, write-url, private-only (vwn.6)" $ do
      let src = unlines ["[cache]", "urls = [\"https://a/z\", \"https://b/z\"]", "write-url = \"https://w/z\"", "private-only = true"]
      parseCacheTable src `shouldBe` Just (CacheConfig ["https://a/z", "https://b/z"] (Just "https://w/z") True)
      parseCacheTable "[workspace]\nmembers = []\nghc = \"9.6.5\"\n" `shouldBe` Nothing

    it "resolveCacheConfig prefers [cache], falls back to the env var (vwn.6)" $ do
      let dir = "/tmp/zinc-cachecfg"
      doesDirectoryExist dir >>= \e -> when e (removeDirectoryRecursive dir)
      createDirectoryIfMissing True dir
      setEnv "ZINC_CACHE" "https://env/z"
      writeFile (dir </> "zinc.toml") (unlines ["[cache]", "urls = [\"https://file/z\"]"])
      fromTable <- resolveCacheConfig dir
      writeFile (dir </> "zinc.toml") "[workspace]\nmembers = []\nghc = \"9.6.5\"\n"
      fromEnv <- resolveCacheConfig dir
      unsetEnv "ZINC_CACHE"
      (ccReadUrls fromTable, ccReadUrls fromEnv, ccWriteUrl fromEnv)
        `shouldBe` (["https://file/z"], ["https://env/z"], Just "https://env/z")

  describe "Zinc.Package (7m6.1)" $ do
    it "parses the deploy formats and rejects unknown ones" $ do
      map parsePackageFormat ["docker", "static", "bundle", "nix"]
        `shouldBe` map Right [Docker, Static, Bundle, NixClosure]
      parsePackageFormat "rpm" `shouldSatisfy` isLeft
      map formatName [Docker, Static, Bundle, NixClosure] `shouldBe` ["docker", "static", "bundle", "nix"]

    it "parses the `package <format>` verb with --tag, -o and --to" $ do
      parseArgs ["package", "docker"] `shouldBe` Right (OutputFlags False False, Package "docker" Nothing Nothing Nothing)
      parseArgs ["package", "docker", "--tag", "app:1.0"] `shouldBe` Right (OutputFlags False False, Package "docker" (Just "app:1.0") Nothing Nothing)
      parseArgs ["package", "nix", "-o", "./dist"] `shouldBe` Right (OutputFlags False False, Package "nix" Nothing (Just "./dist") Nothing)
      parseArgs ["package", "nix", "--to", "ssh://build-host"] `shouldBe` Right (OutputFlags False False, Package "nix" Nothing Nothing (Just "ssh://build-host"))

    it "generates a packaging flake: packages.default wraps the binary, plus a docker image (7m6.2)" $ do
      let fl = packagingFlake "myapp" "myapp" "1.0" ["/nix/store/s8q3rch0wd3shdnznz9bcj8mj6pvz1gr-gmp-with-cxx-6.3.0"]
      all (`isInfixOf` fl)
        [ "packages = forAll", "default = app", "install -Dm755 ${./myapp}", "apps = forAll"
        , "dockerTools.buildLayeredImage", "name = \"myapp\"", "tag = \"1.0\"", "config.Cmd = [ \"/bin/myapp\" ]"
        -- runtime deps are pinned so Nix captures the full closure (7m6.2/.4)
        , "map builtins.storePath", "\"/nix/store/s8q3rch0wd3shdnznz9bcj8mj6pvz1gr-gmp-with-cxx-6.3.0\""
        ]
        `shouldBe` True

    it "storePathRefs extracts top-level store deps from a binary's bytes (7m6.2)" $ do
      -- RPATH-style bytes: full paths, sub-paths, and a repeat all collapse to roots
      let bytes = "\0/nix/store/s8q3rch0wd3shdnznz9bcj8mj6pvz1gr-gmp-with-cxx-6.3.0/lib:"
                ++ "/nix/store/gniy4ab9wcijxjpcciddgpzdwq3v3dnb-libffi-3.4.6/lib\0junk"
                ++ "/nix/store/s8q3rch0wd3shdnznz9bcj8mj6pvz1gr-gmp-with-cxx-6.3.0/lib/libgmp.so.10"
      storePathRefs bytes `shouldBe`
        [ "/nix/store/s8q3rch0wd3shdnznz9bcj8mj6pvz1gr-gmp-with-cxx-6.3.0"
        , "/nix/store/gniy4ab9wcijxjpcciddgpzdwq3v3dnb-libffi-3.4.6"
        ]

    it "resolves the docker image name:tag from --tag (7m6.2)" $ do
      dockerImageRef "hello" Nothing `shouldBe` ("hello", "latest")
      dockerImageRef "hello" (Just "app:1.0") `shouldBe` ("app", "1.0")
      dockerImageRef "hello" (Just "app") `shouldBe` ("app", "latest")

    it "static packaging surfaces a clear ZINC_STATIC_UNSUPPORTED with alternatives (7m6.3)" $ do
      let d = toDiagnostic (StaticUnsupported "myapp")
      errorCode (StaticUnsupported "myapp") `shouldBe` "ZINC_STATIC_UNSUPPORTED"
      exitCodeFor (StaticUnsupported "myapp") `shouldBe` ExitFailure 4
      diagNextAction d `shouldSatisfy` maybe False (\a -> "docker" `isInfixOf` a && "bundle" `isInfixOf` a)

  describe "build quirks table (8uh)" $ do
    it "applies -XSafe to colour automatically (no manifest escape hatch)" $
      quirkGhcOptions "colour" `shouldBe` ["-XSafe"]

    it "has no quirk for an ordinary package" $
      quirkGhcOptions "containers" `shouldBe` []

  describe "manifest parse diagnostics on read-only paths (szn)" $
    it "a malformed zinc.toml yields ZINC_MANIFEST_PARSE, not a generic error" $ do
      let d = "/tmp/zinc-badmanifest"
      stale <- doesDirectoryExist d
      when stale $ removeDirectoryRecursive d
      createDirectoryIfMissing True d
      writeFile (d </> "zinc.toml") "[workspace]\nmembers =\n"
      r <- runStatus d
      (errorCode <$> either Just (const Nothing) r) `shouldBe` Just "ZINC_MANIFEST_PARSE"

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

  describe "Zinc.Target (9po.1)" $ do
    it "parses native + wasm targets (with a wasm alias), rejects unknown" $ do
      map parseTarget ["native", "wasm32-wasi", "wasm"] `shouldBe` map Right [Native, Wasm32Wasi, Wasm32Wasi]
      parseTarget "arm64" `shouldSatisfy` isLeft

    it "resolves the cross-tool prefix + binary names per target" $ do
      (toolPrefix Native, toolPrefix Wasm32Wasi) `shouldBe` ("", "wasm32-wasi-")
      map ($ Native) [ghcFor, ghcPkgFor, hsc2hsFor] `shouldBe` ["ghc", "ghc-pkg", "hsc2hs"]
      map ($ Wasm32Wasi) [ghcFor, ghcPkgFor, hsc2hsFor] `shouldBe` ["wasm32-wasi-ghc", "wasm32-wasi-ghc-pkg", "wasm32-wasi-hsc2hs"]
      map targetTriple [Native, Wasm32Wasi] `shouldBe` ["native", "wasm32-wasi"]
      map isWasm [Native, Wasm32Wasi] `shouldBe` [False, True]

  describe "wasm toolchain provisioning (9po.1)" $ do
    it "the native flake is byte-identical to generateFlakeFor Native (no regression)" $
      generateFlakeFor Native "9.6.5" ["zlib"] `shouldBe` generateFlake "9.6.5" ["zlib"]

    it "the wasm flake adds ghc-wasm-meta + node + wasmtime (and not the native ghc)" $ do
      let wf = generateFlakeFor Wasm32Wasi "9.6.5" ["zlib"]
      all (`isInfixOf` wf)
        [ "ghc-wasm-meta.url", "ghc-wasm-meta.packages.${system}.default"
        , "ghc-wasm-meta.packages.${system}.nodejs", "ghc-wasm-meta.packages.${system}.wasmtime", "devShells"
        ]
        `shouldBe` True
      -- pure-Haskell MVP: native ghc attr + the (unavailable) wasm system libs are absent
      any (`isInfixOf` wf) ["haskell.compiler.ghc965", "pkgs.zlib"] `shouldBe` False

    it "keys the dev-env cache per target: native is unchanged, wasm differs (9po.1)" $ do
      envCacheKeyFor Native "9.6.5" ["zlib"] `shouldBe` envCacheKey "9.6.5" ["zlib"] -- native byte-identical
      (envCacheKeyFor Wasm32Wasi "9.6.5" ["zlib"] == envCacheKey "9.6.5" ["zlib"]) `shouldBe` False

  describe "toolchain env provisioning (y03)" $ do
    let sampleJson =
          "{\"bashFunctions\":{\"f\":\"...\"},\"variables\":{"
            ++ "\"PATH\":{\"type\":\"exported\",\"value\":\"/nix/x/bin\"},"
            ++ "\"HOME\":{\"type\":\"exported\",\"value\":\"/homeless-shelter\"},"
            ++ "\"HOSTTYPE\":{\"type\":\"var\",\"value\":\"x86_64\"},"
            ++ "\"PKG_CONFIG_PATH\":{\"type\":\"exported\",\"value\":\"/nix/pc\"}}}"

    it "extracts exported vars from `nix print-dev-env --json` (not type:var / functions)" $ do
      let vs = devEnvVars sampleJson
      lookup "PATH" vs `shouldBe` Just "/nix/x/bin"
      lookup "PKG_CONFIG_PATH" vs `shouldBe` Just "/nix/pc"
      lookup "HOSTTYPE" vs `shouldBe` Nothing

    it "prepends the dev PATH to the ambient one (toolchain wins, user tools kept)" $
      toolchainPath [("PATH", "/nix/x/bin")] "/usr/bin:/bin" `shouldBe` Just "/nix/x/bin:/usr/bin:/bin"

    it "applies only a build whitelist — never sandbox vars like HOME" $ do
      ("HOME" `elem` toolchainVars) `shouldBe` False
      ("PKG_CONFIG_PATH" `elem` toolchainVars) `shouldBe` True

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
            , compExtraLibs = ["z", "pthread"]
            , compIncludeDirs = []
            , compCppOptions = []
            , compCSources = []
            , compReexports = []
            , compWasmExports = []
            , compFromCabal = True
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

    it "flattens an internal sub-library into the one library unit (ffm.2)" $ do
      let c =
            unlines
              [ "cabal-version: 2.4"
              , "name: p"
              , "version: 1"
              , "library p-internal"
              , "  hs-source-dirs: internal"
              , "  build-depends: base, array"
              , "  exposed-modules: P.Internal"
              , "library"
              , "  hs-source-dirs: src"
              , "  build-depends: base, bytestring, p-internal"
              , "  exposed-modules: P"
              ]
          libc = either (const Nothing) (find ((== "lib") . compName)) (parseCabalComponents c)
      -- one merged unit: both source dirs and both module sets ...
      (compSourceDirs <$> libc) `shouldBe` Just ["src", "internal"]
      (sort . compModules <$> libc) `shouldBe` Just ["P", "P.Internal"]
      -- ... and the sub-library name dropped from build-depends (it is this unit)
      (sort . compDepends <$> libc) `shouldBe` Just ["array", "base", "bytestring"]

    it "does NOT flatten a sub-library the main library doesn't depend on (ffm.5)" $ do
      -- vector ships a public `library benchmarks-O2` used only by its benchmark
      -- stanza, carrying bench-only deps (random, tasty). The main library does
      -- not depend on it, so it must not be folded in — else those deps leak into
      -- the library's build-depends and the build passes `-package random`.
      let c =
            unlines
              [ "cabal-version: 2.4"
              , "name: p"
              , "version: 1"
              , "library"
              , "  hs-source-dirs: src"
              , "  build-depends: base, deepseq"
              , "  exposed-modules: P"
              , "library bench-only"
              , "  hs-source-dirs: bench"
              , "  build-depends: base, p, random, tasty"
              , "  exposed-modules: P.Bench"
              ]
          libc = either (const Nothing) (find ((== "lib") . compName)) (parseCabalComponents c)
      (sort . compDepends <$> libc) `shouldBe` Just ["base", "deepseq"]
      (sort . compModules <$> libc) `shouldBe` Just ["P"]

    it "captures default-language as the LEADING -X flag, before extensions (ffm.5)" $ do
      -- Without -XHaskell2010, GHC's default poly-kinds a phantom type variable
      -- (s :: k instead of s :: *), breaking packages like vector that rely on
      -- Haskell2010 kind defaulting. The language must precede extensions so it
      -- sets the base edition rather than resetting an extension after the fact.
      let c =
            unlines
              [ "cabal-version: 2.4"
              , "name: p"
              , "version: 1"
              , "library"
              , "  build-depends: base"
              , "  default-language: Haskell2010"
              , "  default-extensions: BangPatterns"
              , "  exposed-modules: P"
              ]
          libc = either (const Nothing) (find ((== "lib") . compName)) (parseCabalComponents c)
      (compExtensions <$> libc) `shouldBe` Just ["Haskell2010", "BangPatterns"]

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

    it "maps libpq / pq to pkgs.postgresql (no pkgs.libpq in nixpkgs; zinc-389)" $ do
      toNixpkgs "libpq" `shouldBe` Just "postgresql"
      toNixpkgs "pq" `shouldBe` Just "postgresql"

  describe "freeze engine" $ do
    repo <- runIO setupDepRepo

    it "lockEntry maps a resolved dep + rev + sha to a LockedPackage" $
      lockEntry [] [] (ResolvedDep "aeson" "r/aeson" (Tag "v2") ["scientific"]) "abc123" "sha256:xyz"
        `shouldBe` LockedPackage
          { lockName = "aeson"
          , lockSource = GitSource "r/aeson" "abc123"
          , lockSha256 = "sha256:xyz"
          , lockDepends = ["scientific"]
          , lockFlags = []
          , lockSystemLibs = []
          }

    it "lockEntry records the manifest's manual flags + the package's system libs (zinc-iaj.2 / zinc-389)" $
      lockEntry [("postgresql-libpq", [("use-pkg-config", True)])] ["postgresql"] (ResolvedDep "postgresql-libpq" "r/pq" (Tag "v1") []) "abc123" "sha256:xyz"
        `shouldBe` LockedPackage
          { lockName = "postgresql-libpq"
          , lockSource = GitSource "r/pq" "abc123"
          , lockSha256 = "sha256:xyz"
          , lockDepends = []
          , lockFlags = [("use-pkg-config", True)]
          , lockSystemLibs = ["postgresql"]
          }

    it "splitNameVersion separates a trailing version, respecting dashes in names (b1z.2)" $ do
      splitNameVersion "colour-2.3.6" `shouldBe` Just ("colour", "2.3.6")
      splitNameVersion "tf-random-0.5" `shouldBe` Just ("tf-random", "0.5")
      splitNameVersion "colour" `shouldBe` Nothing     -- bare name → version from the env
      splitNameVersion "tf-random" `shouldBe` Nothing

    it "lockEntry records a vendored dep as a tarball source, ignoring the git rev (b1z)" $
      lockEntry [] [] (ResolvedDep "colour" "" (Vendored "2.3.6") ["base"]) "unused-rev" "sha256:abc"
        `shouldBe` LockedPackage
          { lockName = "colour"
          , lockSource = TarballSource "2.3.6"
          , lockSha256 = "sha256:abc"
          , lockDepends = ["base"]
          , lockFlags = []
          , lockSystemLibs = []
          }

    it "freezeClosure clones each dep and records its commit + content hash" $ do
      let rd = ResolvedDep "dep" repo (Tag "v1") ["aeson"]
      r <- freezeClosure "/tmp/zinc-freeze-store" [] [rd]
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
                , confReexports = []
                , confExtraLibraries = []
                }
       in all
            (`isInfixOf` out)
            [ "name: myapp"
            , "id: myapp-0.1.0-abc"
            , "exposed-modules: Myapp Myapp.Core"
            , "hs-libraries: HSmyapp-0.1.0-abc"
            ]
            `shouldBe` True

    it "renderConf emits reexports inline in exposed-modules as `New from unit:Orig` (jdf)" $
      let out =
            renderConf
              PackageConf
                { confName = "facade"
                , confVersion = "1.0"
                , confId = "facade"
                , confExposedModules = ["Own"]
                , confImportDirs = ["/d"]
                , confLibraryDirs = ["/d"]
                , confHsLibraries = ["HSfacade"]
                , confDepends = ["effectful-core"]
                , confReexports = [("Effectful", "effectful-core", "Effectful")]
                , confExtraLibraries = []
                }
       in ("exposed-modules: Own Effectful from effectful-core:Effectful" `isInfixOf` out) `shouldBe` True

    it "renderConf emits extra-libraries so GHC auto-links external C libs for dependents (zinc-389)" $
      let out =
            renderConf
              PackageConf
                { confName = "postgresql-libpq"
                , confVersion = "0.11"
                , confId = "postgresql-libpq"
                , confExposedModules = ["Database.PostgreSQL.LibPQ"]
                , confImportDirs = ["/d"]
                , confLibraryDirs = ["/d"]
                , confHsLibraries = ["HSpostgresql-libpq"]
                , confDepends = ["bytestring"]
                , confReexports = []
                , confExtraLibraries = ["pq"]
                }
       in ("extra-libraries: pq" `isInfixOf` out) `shouldBe` True

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
                , confReexports = []
                , confExtraLibraries = []
                }
      r <- registerPackage db conf
      r `shouldBe` Right ()

    -- zinc-0k7: a persisted workspace db can carry a STALE conf after a zinc
    -- upgrade adds content (jdf taught the builder to emit Cabal reexports into
    -- exposed-modules). isRegistered only checks the pkg dir, so the closure
    -- builder must additionally re-register when the registered exposed-modules
    -- drift from the conf it holds — otherwise the reexports stay invisible and
    -- a consumer's `import <reexported>` fails. This guards that drift detection.
    it "registeredExposedMatches detects a conf whose reexports drifted (0k7)" $ do
      let base = "/tmp/zinc-stale-conf-test"
          db = base ++ "/db"
          mk own reexs =
            renderConf
              PackageConf
                { confName = "umbrella"
                , confVersion = "1.0"
                , confId = "umbrella"
                , confExposedModules = own
                , confImportDirs = [base ++ "/d"]
                , confLibraryDirs = [base ++ "/d"]
                , confHsLibraries = []
                , confDepends = ["origin"]
                , confReexports = reexs
                , confExtraLibraries = []
                }
          oldConf = mk ["Own"] [] -- pre-jdf: no reexports
          newConf = mk ["Own"] [("Effectful", "origin", "Effectful")] -- post-jdf
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      -- origin must exist so ghc-pkg accepts the reexport's `from origin:...`.
      _ <- registerPackage db (renderConf (PackageConf "origin" "1.0" "origin" ["Effectful"] [base ++ "/d"] [base ++ "/d"] [] [] [] []))
      _ <- registerPackage db oldConf
      -- The registered (old) conf does NOT match the new conf → must re-register.
      drifted <- registeredExposedMatches db "umbrella" newConf
      -- After re-registering the new conf, it matches itself → skip is safe.
      _ <- registerPackage db newConf
      current <- registeredExposedMatches db "umbrella" newConf
      (drifted, current) `shouldBe` (False, True)

  describe "preprocessors" $ do
    it "maps source extensions to their preprocessor command" $ do
      preprocessorFor "Lexer.x" `shouldBe` Just ("alex", ["Lexer.x", "-o", "Lexer.hs"])
      preprocessorFor "Parser.y" `shouldBe` Just ("happy", ["Parser.y", "-o", "Parser.hs"])
      preprocessorFor "Foo.hsc" `shouldBe` Just ("hsc2hs", ["Foo.hsc", "-o", "Foo.hs"])

    it "leaves plain .hs files alone" $
      preprocessorFor "Plain.hs" `shouldBe` Nothing

    -- zinc-bxw.1: hsc2hs compiles a generated _hsc_make.c with cc, so it must see
    -- the package's bundled headers (e.g. network's HsNet.h). The include-dirs
    -- ride in as --cflag=-I...; alex/happy emit pure Haskell and ignore them.
    it "passes include-dir cflags to hsc2hs (and not to alex/happy)" $ do
      let cflags = ["-I/pkg/include", "-I/pkg/other"]
      ppCommand cflags "Net.hsc" "Net.hs"
        `shouldBe` Just ("hsc2hs", ["Net.hsc", "-o", "Net.hs", "--cflag=-I/pkg/include", "--cflag=-I/pkg/other"])
      ppCommand cflags "Lexer.x" "Lexer.hs" `shouldBe` Just ("alex", ["Lexer.x", "-o", "Lexer.hs"])
      ppCommand cflags "Parser.y" "Parser.hs" `shouldBe` Just ("happy", ["Parser.y", "-o", "Parser.hs"])

    it "runPreprocessor runs hsc2hs and produces the .hs" $ do
      let dir = "/tmp/zinc-pp-test"
      stale <- doesDirectoryExist dir
      when stale $ removeDirectoryRecursive dir
      createDirectoryIfMissing True dir
      writeFile (dir ++ "/Foo.hsc") "module Foo where\nanswer :: Int\nanswer = 42\n"
      r <- runPreprocessor (dir ++ "/Foo.hsc")
      produced <- doesFileExist (dir ++ "/Foo.hs")
      (r, produced) `shouldBe` (Right (), True)

  describe "Zinc.Skill foundation (dp6.1)" $ do
    it "parses the [skills] table into SkillDeps (repo + ref heuristics)" $ do
      let src = unlines
            [ "[workspace]", "members = [\".\"]", "ghc = \"9.6.5\""
            , "[skills]"
            , "brainstorming = { repo = \"https://github.com/o/brainstorming\", ref = \"v1\" }"
            , "debugging = { repo = \"https://github.com/o/debugging\" }"
            , "research = { repo = \"https://github.com/o/research\", ref = \"*\" }"
            ]
      case parseSkills src of
        Left e -> expectationFailure e
        Right sks -> do
          map skName sks `shouldBe` ["brainstorming", "debugging", "research"]
          map skRepo sks `shouldBe` ["https://github.com/o/brainstorming", "https://github.com/o/debugging", "https://github.com/o/research"]
          map skRef sks `shouldBe` [Tag "v1", Latest, Latest]

    it "treats a full hex ref as a commit (rev), a name as a tag (dp6.1)" $ do
      let mk r = "[workspace]\nmembers=[\".\"]\nghc=\"9.6.5\"\n[skills]\ns = { repo = \"r\", ref = \"" ++ r ++ "\" }\n"
      (skRef . head <$> parseSkills (mk "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2")) `shouldBe` Right (Rev "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2")
      (skRef . head <$> parseSkills (mk "v2.0")) `shouldBe` Right (Tag "v2.0")

    it "returns no skills when there is no [skills] table" $
      parseSkills "[workspace]\nmembers = [\".\"]\nghc = \"9.6.5\"\n" `shouldBe` Right []

    it "round-trips [[skill]] lock blocks through render . parse" $ do
      let sks = [LockedSkill "brainstorming" "https://github.com/o/b" "a1b2c3d" "sha256:deadbeef", LockedSkill "debugging" "https://github.com/o/d" "f00ba12" "sha256:cafe"]
      parseSkillLock (renderSkillLock sks) `shouldBe` Right sks

    it "reads SKILL.md frontmatter (name + description), lenient on CRLF + quotes" $ do
      readSkillFrontmatter (unlines ["---", "name: brainstorming", "description: Helps brainstorm ideas", "---", "# Brainstorming", "body"])
        `shouldBe` Right ("brainstorming", "Helps brainstorm ideas")
      readSkillFrontmatter "---\r\nname: \"debugging\"\r\ndescription: 'find root causes'\r\n---\r\nbody\r\n"
        `shouldBe` Right ("debugging", "find root causes")

    it "rejects SKILL.md missing frontmatter or a required field (dp6.1)" $ do
      readSkillFrontmatter "# no frontmatter\nbody\n" `shouldSatisfy` isLeft
      readSkillFrontmatter (unlines ["---", "name: x", "---"]) `shouldSatisfy` isLeft -- no description

  describe "zinc skill add (dp6.2)" $ do
    it "parses `skill add <repo> [--ref]`" $ do
      parseArgs ["skill", "add", "https://github.com/o/s"]
        `shouldBe` Right (OutputFlags False False, SkillAdd "https://github.com/o/s" Nothing)
      parseArgs ["skill", "add", "https://github.com/o/s", "--ref", "v1"]
        `shouldBe` Right (OutputFlags False False, SkillAdd "https://github.com/o/s" (Just "v1"))

    it "skillRepoName drops a .git suffix and trailing slash" $ do
      skillRepoName "https://github.com/o/brainstorming.git" `shouldBe` "brainstorming"
      skillRepoName "https://github.com/o/research/" `shouldBe` "research"

    it "writeSkillLockEntry adds a [[skill]], preserves [[locked]], replaces by name" $ do
      let f = "/tmp/zinc-skill-lock-test.lock"
      writeFile f (renderLock [LockedPackage "p" (GitSource "r/p" "rev") "sha256:p" [] [] []])
      writeSkillLockEntry f (LockedSkill "brainstorming" "r/b" "rb1" "sha256:b1")
      writeSkillLockEntry f (LockedSkill "brainstorming" "r/b" "rb2" "sha256:b2") -- same name -> replace
      src <- readFile f
      parseLock src `shouldBe` Right [LockedPackage "p" (GitSource "r/p" "rev") "sha256:p" [] [] []]
      parseSkillLock src `shouldBe` Right [LockedSkill "brainstorming" "r/b" "rb2" "sha256:b2"]

    it "installs a skill from a local git repo: clone, lock, symlink (e2e)" $ do
      let base = "/tmp/zinc-skill-e2e"
          repo = base ++ "/brainstorm-skill"
          ws = base ++ "/ws"
      stale <- doesDirectoryExist base
      when stale (removeDirectoryRecursive base)
      writeFileIn (repo ++ "/SKILL.md") (unlines ["---", "name: brainstorming", "description: Helps brainstorm", "---", "# Brainstorming", "do the thing"])
      writeFileIn (repo ++ "/extra.md") "supporting file\n"
      let git args = readProcess "git" ("-C" : repo : args) ""
      _ <- git ["init", "--quiet"]
      _ <- git ["config", "user.email", "t@example.com"]
      _ <- git ["config", "user.name", "Test"]
      _ <- git ["add", "."]
      _ <- git ["commit", "--quiet", "-m", "skill"]
      _ <- git ["tag", "v1"]
      createDirectoryIfMissing True ws
      r <- runSkillAdd repo Nothing ws -- ref=latest -> resolves the v1 tag
      -- the install symlinks the store tree to .claude/skills/<frontmatter-name>
      let link = ws ++ "/.claude/skills/brainstorming"
      linkOk <- doesFileExist (link ++ "/SKILL.md")
      lockSrc <- readFile (ws ++ "/zinc.lock")
      ( either Left (Right . map lskName) (parseSkillLock lockSrc)
        , linkOk
        , either (const "ERR") id r )
        `shouldBe` (Right ["brainstorming"], True, "Installed skill brainstorming (Helps brainstorm) \8594 .claude/skills/brainstorming")

  describe "zinc skill list / remove / sync (dp6.3, dp6.4)" $ do
    it "parses `skill list`, `skill remove <name>`, `skill sync`" $ do
      parseArgs ["skill", "list"] `shouldBe` Right (OutputFlags False False, SkillList)
      parseArgs ["skill", "remove", "brainstorming"] `shouldBe` Right (OutputFlags False False, SkillRemove "brainstorming")
      parseArgs ["skill", "sync"] `shouldBe` Right (OutputFlags False False, SkillSync)

    it "renders an installed-skills table (and an empty-state line)" $ do
      renderSkillList [] `shouldSatisfy` isInfixOf "No skills installed"
      renderSkillList [LockedSkill "brainstorming" "https://github.com/o/b" "abcdef1234567" "sha256:x"]
        `shouldSatisfy` (\s -> "brainstorming" `isInfixOf` s && "abcdef123456" `isInfixOf` s)

    it "add -> list -> sync -> remove round-trips a skill end-to-end" $ do
      let base = "/tmp/zinc-skill-lifecycle"
          repo = base ++ "/skill-repo"
          ws = base ++ "/ws"
          link = ws ++ "/.claude/skills/myskill"
      stale <- doesDirectoryExist base
      when stale (removeDirectoryRecursive base)
      writeFileIn (repo ++ "/SKILL.md") (unlines ["---", "name: myskill", "description: A skill", "---", "body"])
      let git args = readProcess "git" ("-C" : repo : args) ""
      _ <- git ["init", "--quiet"]; _ <- git ["config", "user.email", "t@e.com"]; _ <- git ["config", "user.name", "T"]
      _ <- git ["add", "."]; _ <- git ["commit", "--quiet", "-m", "s"]; _ <- git ["tag", "v1"]
      createDirectoryIfMissing True ws
      _ <- runSkillAdd repo (Just "v1") ws
      listed <- runSkillList ws
      -- remove the symlink, then sync re-materializes it from the lock
      removePathForcibly link
      syncGone <- doesFileExist (link ++ "/SKILL.md")
      synced <- runSkillSync ws
      syncBack <- doesFileExist (link ++ "/SKILL.md")
      -- remove drops the symlink AND the lock entry
      removed <- runSkillRemove "myskill" ws
      afterRemoveList <- runSkillList ws
      afterRemoveLink <- doesPathExist link
      ( fmap (map lskName) listed
        , syncGone, fmap id synced, syncBack
        , removed, fmap (map lskName) afterRemoveList, afterRemoveLink )
        `shouldBe`
        ( Right ["myskill"]
        , False, Right ["myskill"], True
        , Right "Removed skill myskill", Right [], False )

  describe "build cache key" $ do
    let keyF deps opts flags = buildCacheKey (BuildKey "pkg" "abc" "9.6.5" deps opts flags)
        key deps opts = keyF deps opts []
        k1 = key ["base-4", "aeson-2"] ["-O2"]

    it "is deterministic" $
      key ["base-4", "aeson-2"] ["-O2"] `shouldBe` k1

    it "is order-independent for deps and options" $
      key ["aeson-2", "base-4"] ["-O2"] `shouldBe` k1

    it "changes with the resolved rev" $
      (buildCacheKey (BuildKey "pkg" "xyz" "9.6.5" ["base-4", "aeson-2"] ["-O2"] []) == k1) `shouldBe` False

    it "changes with the ghc version" $
      (buildCacheKey (BuildKey "pkg" "abc" "9.8.2" ["base-4", "aeson-2"] ["-O2"] []) == k1) `shouldBe` False

    it "changes with the build options (per-dep overrides)" $
      (key ["base-4", "aeson-2"] ["-XSafe"] == k1) `shouldBe` False

    it "changes with the manual cabal flags — flipping a flag invalidates the artifact (iaj.2)" $ do
      -- Two builds of the same rev with different flag assignments must NOT
      -- share a store entry, or flipping e.g. use-pkg-config false->true would
      -- serve the artifact built with the OLD flag.
      let kOff = keyF ["base-4"] ["-O2"] [("use-pkg-config", False)]
          kOn  = keyF ["base-4"] ["-O2"] [("use-pkg-config", True)]
      (kOff == kOn) `shouldBe` False
      -- A flag assignment also differs from no flags at all.
      (kOff == key ["base-4"] ["-O2"]) `shouldBe` False

    it "is order-independent for flags" $
      keyF ["base-4"] ["-O2"] [("a", True), ("b", False)]
        `shouldBe` keyF ["base-4"] ["-O2"] [("b", False), ("a", True)]

    it "changes with the package name — monorepo siblings sharing a commit don't collide (qln)" $
      (buildCacheKey (BuildKey "other" "abc" "9.6.5" ["base-4", "aeson-2"] ["-O2"] []) == k1) `shouldBe` False

    it "is target-keyed: native is byte-identical, wasm differs (9po.2)" $ do
      let bk = BuildKey "pkg" "abc" "9.6.5" ["base-4"] ["-O2"] []
      buildCacheKeyFor Native bk `shouldBe` buildCacheKey bk -- native: no cache invalidation
      (buildCacheKeyFor Wasm32Wasi bk == buildCacheKey bk) `shouldBe` False -- wasm never collides with native

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
                [ Dependency "aeson" (Tag "v2") (Just "r/aeson") [] []
                , Dependency "hspec" Latest (Just "r/hspec") ["-XSafe"] []
                ]
            }

    it "renderWorkspace round-trips through parseWorkspace" $
      parseWorkspace (renderWorkspace ws) `shouldBe` Right ws

    it "renders a simple (ref-only) dependency as one-line shorthand" $ do
      let simple = WorkspaceManifest [] "9.6.5" [Dependency "text" (Tag "v2.1") Nothing [] []]
      ("text = \"v2.1\"" `isInfixOf` renderWorkspace simple) `shouldBe` True

    it "addDep inserts a new dependency with its repo (sorted, fmt-clean)" $
      let w = addDep (WorkspaceManifest ["packages/a"] "9.6.5" []) "aeson" (Tag "v2") "r/aeson"
       in (wsDependencies w, depRepos w)
            `shouldBe` ([Dependency "aeson" (Tag "v2") (Just "r/aeson") [] []], [("aeson", "r/aeson")])

    it "addDep replaces an existing dependency in place" $
      let w0 = addDep (WorkspaceManifest [] "9.6.5" []) "aeson" (Tag "v2") "r/aeson"
          w1 = addDep w0 "aeson" Latest "r/aeson2"
       in (wsDependencies w1, depRepos w1)
            `shouldBe` ([Dependency "aeson" Latest (Just "r/aeson2") [] []], [("aeson", "r/aeson2")])

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

    it "falls back to a git-host homepage when source-repository is absent (ffm.2/strict)" $ do
      sourceRepoOf (unlines ["name: strict", "version: 0.5.1", "homepage: https://github.com/haskell-strict/strict", "library", "  build-depends: base"])
        `shouldBe` Just "https://github.com/haskell-strict/strict"
      -- a non-git-host homepage is not assumed to be a repo
      sourceRepoOf (unlines ["name: x", "version: 1", "homepage: https://example.com/docs", "library", "  build-depends: base"])
        `shouldBe` Nothing

    it "normalizes git:// to https and appends a monorepo subdir (49o)" $ do
      sourceRepoOf (unlines ["name: x", "version: 1", "source-repository head", "  type: git", "  location: git://github.com/o/r"])
        `shouldBe` Just "https://github.com/o/r"
      sourceRepoOf (unlines ["name: x", "version: 1", "source-repository head", "  type: git", "  location: https://github.com/o/mono", "  subdir: pkg"])
        `shouldBe` Just "https://github.com/o/mono#pkg"

    it "normalizes a forge tree-URL into clone-url#subdir, from location or homepage (qln)" $ do
      -- a source-repository location that is a GitHub browser tree URL
      sourceRepoOf (unlines ["name: x", "version: 1", "source-repository head", "  type: git", "  location: https://github.com/o/mono/tree/master/pkg"])
        `shouldBe` Just "https://github.com/o/mono#pkg"
      -- GitLab's /-/tree/ variant
      sourceRepoOf (unlines ["name: x", "version: 1", "source-repository head", "  type: git", "  location: https://gitlab.com/o/mono/-/tree/main/sub/pkg"])
        `shouldBe` Just "https://gitlab.com/o/mono#sub/pkg"
      -- the unliftio case: NO source-repository, homepage is a tree URL with an #readme anchor
      sourceRepoOf (unlines ["name: unliftio", "version: 0.2", "homepage: https://github.com/fpco/unliftio/tree/master/unliftio#readme", "library", "  build-depends: base"])
        `shouldBe` Just "https://github.com/fpco/unliftio#unliftio"
      -- a plain git-host homepage with an HTML anchor: the anchor is dropped, no bogus subdir
      sourceRepoOf (unlines ["name: x", "version: 1", "homepage: https://github.com/o/r#readme", "library", "  build-depends: base"])
        `shouldBe` Just "https://github.com/o/r"
      -- an explicit subdir: field still wins over anything derived from the URL
      sourceRepoOf (unlines ["name: x", "version: 1", "source-repository head", "  type: git", "  location: https://github.com/o/mono/tree/master/wrong", "  subdir: right"])
        `shouldBe` Just "https://github.com/o/mono#right"

    it "builds the Hackage .cabal URL" $
      hackageCabalUrl "aeson" `shouldBe` "https://hackage.haskell.org/package/aeson/aeson.cabal"

    it "builds a package's Hackage sdist tarball URL (b1z)" $
      hackageTarballUrl "colour" "2.3.6" `shouldBe` "https://hackage.haskell.org/package/colour-2.3.6/colour-2.3.6.tar.gz"

  describe "monorepo cabal selection (ffm.6/ffm.7)" $ do
    it "namedCabal picks <name>.cabal out of a multi-cabal dir, case-insensitively" $ do
      namedCabal "text-iso8601" ["aeson.cabal", "text-iso8601.cabal"] `shouldBe` Just "text-iso8601.cabal"
      namedCabal "QuickCheck" ["quickcheck.cabal"] `shouldBe` Just "quickcheck.cabal"
      namedCabal "x" ["a.cabal", "b.cabal"] `shouldBe` Nothing

    it "packageDirIn finds the dir holding THIS package's <name>.cabal" $ do
      let base = "/tmp/zinc-pkgdir-test"
      stale <- doesDirectoryExist base
      when stale (removeDirectoryRecursive base)
      -- an explicit url#subdir always wins
      createDirectoryIfMissing True (base </> "a" </> "sub")
      packageDirIn (base </> "a") "repo#sub" "a" >>= (`shouldBe` (base </> "a" </> "sub"))
      -- the package's own cabal at the root -> the root
      createDirectoryIfMissing True (base </> "b")
      writeFile (base </> "b" </> "b.cabal") "name: b\n"
      packageDirIn (base </> "b") "repo" "b" >>= (`shouldBe` (base </> "b"))
      -- no root manifest, but a <name>/ subdir has one (strict-style monorepo)
      createDirectoryIfMissing True (base </> "c" </> "strict")
      writeFile (base </> "c" </> "strict" </> "strict.cabal") "name: strict\n"
      packageDirIn (base </> "c") "repo" "strict" >>= (`shouldBe` (base </> "c" </> "strict"))
      -- a monorepo root holding only a SIBLING's cabal -> still descend to
      -- <name>/ (the over-inclusion fix: don't read the sibling's deps)
      createDirectoryIfMissing True (base </> "d" </> "pkg")
      writeFile (base </> "d" </> "sibling.cabal") "name: sibling\n"
      writeFile (base </> "d" </> "pkg" </> "pkg.cabal") "name: pkg\n"
      packageDirIn (base </> "d") "repo" "pkg" >>= (`shouldBe` (base </> "d" </> "pkg"))

    it "isHpackOnly detects a package.yaml with no committed .cabal (pzu)" $ do
      let base = "/tmp/zinc-hpack-test"
      stale <- doesDirectoryExist base
      when stale (removeDirectoryRecursive base)
      -- package.yaml + no .cabal -> hpack (must vendor from Hackage)
      createDirectoryIfMissing True (base </> "hp")
      writeFile (base </> "hp" </> "package.yaml") "name: hp\n"
      isHpackOnly (base </> "hp") >>= (`shouldBe` True)
      -- package.yaml AND a committed .cabal -> not hpack-only (read the cabal)
      createDirectoryIfMissing True (base </> "both")
      writeFile (base </> "both" </> "package.yaml") "name: both\n"
      writeFile (base </> "both" </> "both.cabal") "name: both\n"
      isHpackOnly (base </> "both") >>= (`shouldBe` False)
      -- a plain cabal package -> not hpack
      createDirectoryIfMissing True (base </> "cab")
      writeFile (base </> "cab" </> "cab.cabal") "name: cab\n"
      isHpackOnly (base </> "cab") >>= (`shouldBe` False)

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
              , compExtraLibs = []
              , compIncludeDirs = []
              , compCppOptions = []
              , compCSources = []
              , compReexports = []
              , compWasmExports = []
              , compFromCabal = False
              }
      r <- buildMember (MemberBuild dir (dir ++ "/build") Nothing comp)
      case r of
        Right exe -> do
          out <- readProcess exe [] ""
          out `shouldBe` "hello from zinc\n"
        Left err -> expectationFailure (renderError err)

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
            Component Library "x" [] [] Nothing [] [] deps [] [] [] [] [] [] [] False
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

  -- zinc-jdf regression fixture (no committed e2e existed): a consumer that
  -- declares ONLY an umbrella whose conf re-exports a module from an origin
  -- package (`Origin from origin:Origin`) must build + run. GHC follows the
  -- reexport into the registered-but-hidden origin, so the consumer needs neither
  -- to declare nor to expose the origin. Built directly (not via a manifest)
  -- since zinc-native zinc.toml can't express reexported-modules — only cabal can.
  -- Guards that reexport rendering + resolution stay wired end-to-end.
  describe "umbrella-only reexport consumer (real compile, jdf)" $
    it "builds + runs a consumer that declares only the umbrella, importing a re-exported symbol" $ do
      let base = "/tmp/zinc-umbrella-consumer"
          db = base ++ "/db/pkg.db"
          lib nm = Component Library nm ["src"] [] Nothing [] [] ["base"] [] [] [] [] [] [] [] False
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      _ <- initPackageDb db
      -- origin package: exposes a module with a real symbol the consumer uses.
      createDirectoryIfMissing True (base ++ "/origin/src")
      writeFile (base ++ "/origin/src/Origin.hs") "module Origin (secret) where\nsecret :: Int\nsecret = 42\n"
      origR <- buildLib (LibBuild (base ++ "/origin") (base ++ "/origin/dist") db "origin" "1.0" (lib "origin"))
      -- umbrella: no own modules, re-exports Origin from the origin package. Built
      -- as a hand-registered conf (the reexport form a cabal umbrella produces).
      -- Its import/library dirs are its OWN (empty) dir, NOT the origin's — so the
      -- ONLY way the consumer can resolve `Origin` is by following the reexport
      -- into the origin package (which the fix makes visible).
      createDirectoryIfMissing True (base ++ "/umbrella/dist")
      let umbrella =
            renderConf
              PackageConf
                { confName = "umbrella"
                , confVersion = "1.0"
                , confId = "umbrella"
                , confExposedModules = []
                , confImportDirs = [base ++ "/umbrella/dist"]
                , confLibraryDirs = [base ++ "/umbrella/dist"]
                , confHsLibraries = []
                , confDepends = ["origin"]
                , confReexports = [("Origin", "origin", "Origin")]
                , confExtraLibraries = []
                }
      umbR <- registerPackage db umbrella
      -- consumer executable declaring ONLY the umbrella (not origin).
      createDirectoryIfMissing True (base ++ "/app/app")
      writeFile (base ++ "/app/app/Main.hs") "module Main where\nimport Origin (secret)\nmain :: IO ()\nmain = print secret\n"
      let consumer = (lib "app") {compKind = Executable, compName = "app", compSourceDirs = ["app"], compMain = Just "Main.hs", compDepends = ["umbrella"]}
      r <- buildMember (MemberBuild (base ++ "/app") (base ++ "/app/build") (Just db) consumer)
      (origR, umbR) `shouldBe` (Right (), Right ())
      case r of
        Right exe -> do
          out <- readProcess exe [] ""
          out `shouldBe` "42\n"
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
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "greet" (Rev rev) (Just greet) [] []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "greet" (GitSource greet rev) "sha256:x" [] [] []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"greet\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Greet (hello)\nmain :: IO ()\nmain = putStrLn hello\n"
      r <- buildAndRun ws []
      r `shouldBe` Right "hi from greet\n"

  describe "remote artifact cache pull (vwn.4, end-to-end)" $
    it "pulls a closure artifact from a file:// cache instead of recompiling" $ do
      let base = "/tmp/zinc-cache-pull-ws"
          greet = base ++ "/greet-repo"
          ws = base ++ "/ws"
          cacheDir = base ++ "/cache"
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      writeFileIn (greet ++ "/zinc.toml") (unlines ["[package]", "name = \"greet\"", "version = \"1.0\"", "[build.lib]", "source-dirs = [\"src\"]", "exposed-modules = [\"Greet\"]"])
      writeFileIn (greet ++ "/src/Greet.hs") "module Greet (hello) where\nhello :: String\nhello = \"hi from cache\"\n"
      let git args = readProcess "git" ("-C" : greet : args) ""
      _ <- git ["init", "--quiet"]
      _ <- git ["config", "user.email", "t@example.com"]
      _ <- git ["config", "user.name", "Test"]
      _ <- git ["add", "."]
      _ <- git ["commit", "--quiet", "-m", "greet"]
      rev <- trimStr <$> git ["rev-parse", "HEAD"]
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "greet" (Rev rev) (Just greet) [] []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "greet" (GitSource greet rev) "sha256:x" [] [] []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"greet\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Greet (hello)\nmain :: IO ()\nmain = putStrLn hello\n"
      -- 1) build locally; greet's artifact lands in the content-addressed store
      r1 <- buildAndRun ws []
      -- 2) harvest greet's pkg artifact into a file:// cache, keyed by its build key
      let key = buildCacheKey (BuildKey "greet" rev "9.6.5" [] [] [])
          pkgDir = storePkgPath testStoreDir key
      createDirectoryIfMissing True cacheDir
      _ <- readProcess "tar" ["-czf", cacheDir ++ "/" ++ key ++ ".tar.gz", "-C", pkgDir, "."] ""
      -- 3) wipe greet's LOCAL store entry (pkg + src) to force a miss
      removeDirectoryRecursive pkgDir
      removeDirectoryRecursive (storeSrcPath testStoreDir greet rev)
      -- 4) rebuild with the remote cache: greet must be PULLED, not recompiled
      --    (its source is never re-fetched, so the src dir stays absent)
      setEnv "ZINC_CACHE" ("file://" ++ cacheDir)
      r2 <- buildAndRun ws []
      unsetEnv "ZINC_CACHE"
      srcReFetched <- doesDirectoryExist (storeSrcPath testStoreDir greet rev)
      (r1, r2, srcReFetched) `shouldBe` (Right "hi from cache\n", Right "hi from cache\n", False)

  describe "lockDrift" $ do
    let ws = WorkspaceManifest [] "9.6.5" [Dependency "aeson" (Tag "v2") Nothing [] [], Dependency "hspec" Latest Nothing [] []]
        lk n = LockedPackage n (GitSource "r" "rev") "sha" [] [] []

    it "reports manifest deps missing from the lock" $
      lockDrift ws [lk "aeson"] `shouldBe` ["hspec"]

    it "reports no drift when every dep is locked" $
      lockDrift ws [lk "aeson", lk "hspec"] `shouldBe` []

  describe "replArgs" $ do
    let exeComp =
          Component Executable "app" ["app"] [] (Just "Main.hs") [] [] [] [] [] [] [] [] [] [] False

    it "builds ghci args loading the member's main" $
      replArgs (Just "/db") "/m" exeComp
        `shouldBe` ["-package-db", "/db", "-hide-all-packages", "-package", "base", "-i/m/app", "/m/app/Main.hs"]

    it "loads a scaffolded member in ghci (ghci -e main)" $ do
      let dir = "/tmp/zinc-repl-test"
      stale <- doesDirectoryExist dir
      when stale $ removeDirectoryRecursive dir
      createDirectoryIfMissing True dir
      materialize dir (scaffoldNew "demo")
      -- flat scaffold: the member is the repo root itself (member "."), source at app/
      let memberDir = dir
          comp = Component Executable "demo" ["app"] [] (Just "Main.hs") [] [] [] [] [] [] [] [] [] [] False
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
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "greet" (Rev rev) (Just dep) [] []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "greet" (GitSource dep rev) "sha256:x" [] [] []])
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
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "boxed" (Rev rev) (Just dep) [] []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "boxed" (GitSource dep rev) "sha256:x" [] [] []])
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
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "lexdep" (Rev rev) (Just dep) [] []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "lexdep" (GitSource dep rev) "sha256:x" [] [] []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"lexdep\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Lexer (firstWord)\nmain :: IO ()\nmain = putStrLn firstWord\n"
      r <- buildAndRun ws []
      r `shouldBe` Right "hello\n"
      -- c3g: the generated .hs must NOT land in the content-addressed src tree
      -- (it would change the tree's hash and fail the lock sha256 on the next
      -- build); preprocessor output goes to the dist dir instead.
      polluted <- doesFileExist (storeSrcPath testStoreDir dep rev </> "src" </> "Lexer.hs")
      polluted `shouldBe` False

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
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "extdep" (GitSource dep rev) "sha256:x" [] [] []])
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
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "greet" (Rev rev) (Just repoSpec) [] []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "greet" (GitSource repoSpec rev) "sha256:x" [] [] []])
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
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "hdrdep" (Rev rev) (Just dep) [] []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "hdrdep" (GitSource dep rev) "sha256:x" [] [] []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"hdrdep\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Hdr (val)\nmain :: IO ()\nmain = print val\n"
      r <- buildAndRun ws []
      r `shouldBe` Right "7\n"

  describe "closure builder compiles + links a dependency's C sources (i98)" $
    it "archives a dep's cabal c-sources object so a dependent links its foreign symbol" $ do
      -- A dep with c-sources (e.g. primitive's cbits/primitive-memops.c) must
      -- have its C object archived into libHS<pkg>.a, or a dependent that
      -- references the foreign symbol fails to link (undefined reference).
      let base = "/tmp/zinc-csrc-ws"
          dep = base ++ "/csym-repo"
          ws = base ++ "/ws"
      stale <- doesDirectoryExist base
      when stale $ removeDirectoryRecursive base
      writeFileIn (dep ++ "/zinc.toml") (unlines ["[package]", "name = \"csym\"", "version = \"1.0\"", "[build.lib]", "source-dirs = [\"src\"]", "exposed-modules = [\"CSym\"]", "c-sources = [\"cbits/foo.c\"]"])
      writeFileIn (dep ++ "/cbits/foo.c") "int zinc_csym(void) { return 7; }\n"
      writeFileIn (dep ++ "/src/CSym.hs") "{-# LANGUAGE ForeignFunctionInterface #-}\nmodule CSym (csym) where\nforeign import ccall unsafe \"zinc_csym\" csym :: Int\n"
      let git args = readProcess "git" ("-C" : dep : args) ""
      _ <- git ["init", "--quiet"]
      _ <- git ["config", "user.email", "t@example.com"]
      _ <- git ["config", "user.name", "Test"]
      _ <- git ["add", "."]
      _ <- git ["commit", "--quiet", "-m", "csym"]
      rev <- trimStr <$> git ["rev-parse", "HEAD"]
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "csym" (Rev rev) (Just dep) [] []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "csym" (GitSource dep rev) "sha256:x" [] [] []])
      writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"csym\"]"])
      writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport CSym (csym)\nmain :: IO ()\nmain = print csym\n"
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
          writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "integer-logarithms" (Rev rev) (Just dep) [] []]))
          writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "integer-logarithms" (GitSource dep rev) "sha256:x" [] [] []])
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
          writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "toml-parser" (Rev tRev) (Just tomlDep) [] [], Dependency "prettyprinter" Latest (Just ppSpec) [] []]))
          writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "prettyprinter" (GitSource ppSpec pRev) "sha256:x" [] [] [], LockedPackage "toml-parser" (GitSource tomlDep tRev) "sha256:x" ["prettyprinter"] [] []])
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
          writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "toml-parser" (Rev tRev) (Just tomlDep) [] [], Dependency "prettyprinter" (Rev pRev) (Just (ppDep ++ "#prettyprinter")) [] []]))
          writeFileIn (ws ++ "/packages/app/zinc.toml") (unlines ["[package]", "name = \"app\"", "version = \"1.0\"", "[build.exe.app]", "source-dirs = [\"app\"]", "main = \"Main.hs\"", "depends = [\"toml-parser\"]"])
          writeFileIn (ws ++ "/packages/app/app/Main.hs") "module Main where\nimport Toml (parse)\nmain :: IO ()\nmain = putStrLn (either (const \"err\") (const \"parsed-ok\") (parse \"x = 1\\n\"))\n"
          -- zinc resolves the real closure (.cabal + registry) and freezes a lock
          upd <- runUpdate Nothing False (ws ++ "/zinc.toml") store
          upd `shouldSatisfy` isRight
          lockSrc <- readFile (ws ++ "/zinc.lock")
          (isInfixOf "toml-parser" lockSrc && isInfixOf "prettyprinter" lockSrc) `shouldBe` True
          r <- buildAndRun ws []
          r `shouldBe` Right "parsed-ok\n"

  -- b1z: the vendoring recovery path on a real no-git package. colour is
  -- darcs-era (no upstream git repo), so it can only enter the closure via a
  -- vendored Hackage tarball. runVendor must fetch + unpack + pin it, write a
  -- [[locked]] entry with `vendored = ...` and a real sha256, and a [dependencies]
  -- entry with `vendored = ...`. Network-gated.
  describe "vendoring a no-git package from Hackage (b1z, network)" $
    it "runVendor pins colour as a vendored tarball with a real content hash" $ do
      net <- lookupEnv "ZINC_NET_TESTS"
      case net of
        Nothing -> pendingWith "network test; set ZINC_NET_TESTS=1 to run"
        Just _ -> do
          let base = "/tmp/zinc-vendor-b1z"
              ws = base ++ "/ws"
              store = base ++ "/store"
          stale <- doesDirectoryExist base
          when stale $ removeDirectoryRecursive base
          createDirectoryIfMissing True base
          -- a workspace with no git deps; we vendor colour into it by version
          writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" []))
          r <- runVendor (ws ++ "/zinc.toml") store ["colour-2.3.6"]
          r `shouldSatisfy` isRight
          lockSrc <- readFile (ws ++ "/zinc.lock")
          locks <- either (fail . ("lock parse: " ++)) pure (parseLock lockSrc)
          case find ((== "colour") . lockName) locks of
            Nothing -> expectationFailure "colour missing from the lock"
            Just lp -> do
              lockSource lp `shouldBe` TarballSource "2.3.6"
              take 7 (lockSha256 lp) `shouldBe` "sha256:"
              length (drop 7 (lockSha256 lp)) `shouldBe` 64 -- real content hash, not a placeholder
          manifestSrc <- readFile (ws ++ "/zinc.toml")
          manifestSrc `shouldSatisfy` isInfixOf "vendored = \"2.3.6\""

  -- ffm: the committed real-package fixtures exercise
  -- resolve->fetch->build-closure->link->run on escalating closure depth, each
  -- with a committed zinc.lock generated via the Hackage auto-discovery
  -- resolver. Network-gated like the rehearsals above. (Deeper fixtures —
  -- vector's monorepo-sibling subdirs, attoparsec/aeson's internal
  -- sub-libraries — are tracked as separate capability gaps.)
  describe "real-package fixtures (ffm, network)" $ do
    let fixtureRunsTo name expected =
          it ("builds and runs the " ++ name ++ " fixture") $ do
            net <- lookupEnv "ZINC_NET_TESTS"
            case net of
              Nothing -> pendingWith "network test; set ZINC_NET_TESTS=1 to run"
              Just _  -> buildAndRun ("test/fixtures/" ++ name) [] >>= (`shouldBe` Right expected)
    fixtureRunsTo "hashable" "42\n"
    fixtureRunsTo "scientific" "3.14\n"
    fixtureRunsTo "attoparsec" "Right 42\n"
    -- vector: monorepo subdir + a public bench-only sub-library (benchmarks-O2)
    -- that must NOT be flattened in, and a default-language the build must honour
    -- (else the Storable MVector phantom is poly-kinded) — zinc-ffm.5.
    fixtureRunsTo "vector" "55\n"
    -- aeson: the deepest closure (40 packages). Exercises Latest tag selection
    -- across a large transitive graph (zinc-myx) and C-source linking through
    -- primitive/splitmix (zinc-i98). `print (encode (object ["zinc" .= 1]))`.
    fixtureRunsTo "aeson" "\"{\\\"zinc\\\":1}\"\n"

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
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "greet" (Rev rev) (Just dep) [] []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "greet" (GitSource dep rev) wrongSha [] [] []])
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
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "greet" (Rev rev) (Just dep) [] []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "greet" (GitSource dep rev) "sha256:x" [] [] []])
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
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "greet" (Rev rev) (Just dep) [] []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "greet" (GitSource dep rev) "sha256:x" [] [] []])
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
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "a" (Rev revA) (Just repoA) [] [], Dependency "b" Latest (Just repoB) [] []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "a" (GitSource repoA revA) "sha256:a" ["b"] [] [], LockedPackage "b" (GitSource repoB revB) "sha256:b" [] [] []])
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
          liveLock = LockedPackage "a" (GitSource "r/a" "rev-a") "sha256:x" [] [] []
          liveKey = buildCacheKey (BuildKey "a" "rev-a" ghc [] [] [])
          deadKey = buildCacheKey (BuildKey "a" "rev-z" ghc [] [] [])
          -- the src dir the builder would create for the live lock (repo-keyed slug; qln)
          liveSrcDir = srcDirName (srcKey liveLock) (lockRev liveLock)
          deadSrcDir = "b-rev-b" -- arbitrary unreferenced entry
      stale <- doesDirectoryExist root
      when stale $ removeDirectoryRecursive root
      writeFileIn (root ++ "/pkg/" ++ liveKey ++ "/package.conf") "live"
      writeFileIn (root ++ "/pkg/" ++ deadKey ++ "/package.conf") "dead"
      writeFileIn (root ++ "/src/" ++ liveSrcDir ++ "/x.hs") "live"
      writeFileIn (root ++ "/src/" ++ deadSrcDir ++ "/x.hs") "dead"
      (rmPkg, rmSrc) <- gcStore root [GCRoot ghc [liveLock]]
      livePkg <- doesDirectoryExist (root ++ "/pkg/" ++ liveKey)
      deadPkg <- doesDirectoryExist (root ++ "/pkg/" ++ deadKey)
      liveSrc <- doesDirectoryExist (root ++ "/src/" ++ liveSrcDir)
      deadSrc <- doesDirectoryExist (root ++ "/src/" ++ deadSrcDir)
      (livePkg, deadPkg, liveSrc, deadSrc, rmPkg, rmSrc)
        `shouldBe` (True, False, True, False, [deadKey], [deadSrcDir])

  describe "runGc (workspace GC entry)" $
    it "collects store entries not referenced by the current workspace lock" $ do
      let dir = "/tmp/zinc-gc-ws"
          gcRoot = "/tmp/zinc-gc-ws-store"
          ghc = "9.6.5"
          liveKey = buildCacheKey (BuildKey "a" "rev-a" ghc [] [] [])
          deadKey = buildCacheKey (BuildKey "a" "rev-z" ghc [] [] [])
          liveLock = LockedPackage "a" (GitSource "r/a" "rev-a") "sha256:x" [] [] []
          liveSrcDir = srcDirName (srcKey liveLock) (lockRev liveLock)
      mapM_ (\p -> doesDirectoryExist p >>= \e -> when e (removeDirectoryRecursive p)) [dir, gcRoot]
      setEnv "ZINC_STORE" gcRoot
      writeFileIn (dir ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] ghc [Dependency "a" (Rev "rev-a") (Just "r/a") [] []]))
      writeFileIn (dir ++ "/zinc.lock") (renderLock [liveLock])
      writeFileIn (gcRoot ++ "/pkg/" ++ liveKey ++ "/package.conf") "live"
      writeFileIn (gcRoot ++ "/pkg/" ++ deadKey ++ "/package.conf") "dead"
      writeFileIn (gcRoot ++ "/src/" ++ liveSrcDir ++ "/x.hs") "live"
      writeFileIn (gcRoot ++ "/src/zombie-rev-z/x.hs") "dead"
      r <- runGc dir
      setEnv "ZINC_STORE" testStoreDir -- restore shared isolation
      r `shouldBe` Right ([deadKey], ["zombie-rev-z"])

  describe "runUpdate" $ do
    (wsFile, store, _leafRepo) <- runIO setupAddFixture

    it "re-resolves and rewrites the lockfile" $ do
      r <- runUpdate Nothing False wsFile store
      lockText <- readFile (takeDirectory wsFile </> "zinc.lock")
      case r of
        Right _ -> ("leaf" `isInfixOf` lockText) `shouldBe` True
        Left err -> expectationFailure (renderError err)

    it "--dry-run computes the delta but does not write the lock (90j.2)" $ do
      let lockFile = takeDirectory wsFile </> "zinc.lock"
      stale <- doesFileExist lockFile
      when stale $ removeFile lockFile
      r <- runUpdate Nothing True wsFile store
      wrote <- doesFileExist lockFile
      (isRight r, wrote) `shouldBe` (True, False)

    it "update <pkg> targets a single dep and still rewrites the lock (90j.3)" $ do
      r <- runUpdate (Just "leaf") False wsFile store
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
      writeFileIn (ws ++ "/zinc.toml") (renderWorkspace (WorkspaceManifest ["packages/app"] "9.6.5" [Dependency "greet" (Rev rev) (Just dep) [] []]))
      writeFileIn (ws ++ "/zinc.lock") (renderLock [LockedPackage "greet" (GitSource dep rev) "sha256:x" [] [] []])
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
      mainExists <- doesFileExist (root ++ "/app/Main.hs")
      manifestBody <- readFile (root ++ "/zinc.toml")
      (wsExists, mainExists, "name = \"demo\"" `isInfixOf` manifestBody)
        `shouldBe` (True, True, True)

  describe "Zinc.Deploy host parsing (nbk.1)" $ do
    it "parses [user@]host[:port] forms" $ do
      parseDeployHost "nixos-box" `shouldBe` DeployHost Nothing "nixos-box" Nothing
      parseDeployHost "gareth@nixos-box" `shouldBe` DeployHost (Just "gareth") "nixos-box" Nothing
      parseDeployHost "gareth@nixos-box:2222" `shouldBe` DeployHost (Just "gareth") "nixos-box" (Just 2222)
      parseDeployHost "nixos-box:22" `shouldBe` DeployHost Nothing "nixos-box" (Just 22)

    it "treats a bare token as a host / ssh-config alias" $
      parseDeployHost "homelab" `shouldBe` DeployHost Nothing "homelab" Nothing

    it "does not split a non-numeric suffix as a port" $
      parseDeployHost "host:weird" `shouldBe` DeployHost Nothing "host:weird" Nothing

  describe "Zinc.Deploy ssh args (nbk.1)" $ do
    it "renders user@host, inserts -p for a port, and forces BatchMode" $ do
      sshArgs (DeployHost (Just "gareth") "box" (Just 2222)) ["echo", "hi"]
        `shouldBe` ["-o", "BatchMode=yes", "-p", "2222", "gareth@box", "echo", "hi"]
      sshArgs (DeployHost Nothing "box" Nothing) ["true"]
        `shouldBe` ["-o", "BatchMode=yes", "box", "true"]

  describe "Zinc.Deploy probe output parsing (nbk.1)" $ do
    it "parses key=value probe lines, defaulting missing keys to False" $ do
      parseProbeOutput "nix=yes\ntrusted=no\nlinger=yes\n"
        `shouldBe` ProbeChecks True False True
      parseProbeOutput "" `shouldBe` ProbeChecks False False False

    it "ships a remote script that checks nix, trusted-users and lingering" $
      all (`isInfixOf` probeScript) ["nix=", "trusted=", "linger=", "loginctl"] `shouldBe` True

  describe "Zinc.Deploy probe interpretation (nbk.1)" $ do
    let h = DeployHost (Just "gareth") "box" Nothing
        code = either (Just . errorCode) (const Nothing)
    it "maps an unreachable host to ZINC_DEPLOY_SSH" $
      code (interpretProbe h (SshUnreachable "connection refused")) `shouldBe` Just "ZINC_DEPLOY_SSH"
    it "reports the first missing precondition, nix → trusted → linger" $ do
      code (interpretProbe h (Probed (ProbeChecks False False False))) `shouldBe` Just "ZINC_DEPLOY_NO_NIX"
      code (interpretProbe h (Probed (ProbeChecks True False False))) `shouldBe` Just "ZINC_DEPLOY_NOT_TRUSTED"
      code (interpretProbe h (Probed (ProbeChecks True True False))) `shouldBe` Just "ZINC_DEPLOY_NO_LINGER"
    it "passes a fully-provisioned host" $
      code (interpretProbe h (Probed (ProbeChecks True True True))) `shouldBe` Nothing

  describe "deploy diagnostics taxonomy (nbk.1)" $ do
    let errs = [DeploySsh "box" "x", DeployNoNix "box", DeployNotTrusted "gareth", DeployNoLinger "gareth"]
    it "assigns stable ZINC_DEPLOY_* codes" $
      map errorCode errs
        `shouldBe` ["ZINC_DEPLOY_SSH", "ZINC_DEPLOY_NO_NIX", "ZINC_DEPLOY_NOT_TRUSTED", "ZINC_DEPLOY_NO_LINGER"]
    it "categorises every deploy precondition as an environment failure (exit 5)" $
      map exitCodeFor errs `shouldBe` replicate 4 (ExitFailure 5)
    it "gives every deploy error an actionable nextAction" $
      all (isJust . diagNextAction . toDiagnostic) errs `shouldBe` True

  describe "deploy verb parsing (nbk.1)" $ do
    it "parses `deploy <host>` with the v1 flag surface" $ do
      parseArgs ["deploy", "gareth@box"]
        `shouldBe` Right (OutputFlags False False, Deploy "gareth@box" Nothing False False False)
      parseArgs ["deploy", "homelab", "--service", "myapp", "--dry-run"]
        `shouldBe` Right (OutputFlags False False, Deploy "homelab" (Just "myapp") False False True)
      parseArgs ["deploy", "box", "--init"]
        `shouldBe` Right (OutputFlags False False, Deploy "box" Nothing True False False)
      parseArgs ["deploy", "box", "--rollback"]
        `shouldBe` Right (OutputFlags False False, Deploy "box" Nothing False True False)

  describe "Zinc.Deploy --init snippet (nbk.5)" $ do
    it "generates a NixOS trusted-users + linger snippet for the deploy user" $ do
      let s = initSnippet "gareth"
      all
        (`isInfixOf` s)
        [ "nix.settings.trusted-users = [ \"gareth\" ]"
        , "users.users.gareth.linger"
        , "= true;"
        ]
        `shouldBe` True

    it "wraps the snippet in a NixOS module attrset" $ do
      let s = initSnippet "deployer"
      (head (lines s), last (filter (not . null) (lines s))) `shouldBe` ("{", "}")

  describe "Zinc.Manifest [deploy.*] targets (nbk.6)" $ do
    it "parses a named deploy target's host, service, args and env" $ do
      let toml =
            unlines
              [ "[workspace]", "members = [\".\"]", "ghc = \"9.6.5\""
              , "[deploy.homelab]"
              , "host = \"gareth@nixos-box\""
              , "service = \"myapp\""
              , "args = [\"--port\", \"8080\"]"
              , "env = { RUST_LOG = \"info\" }"
              ]
      parseDeployTargets toml
        `shouldBe` Right [DeployTarget "homelab" "gareth@nixos-box" (Just "myapp") ["--port", "8080"] [("RUST_LOG", "info")]]

    it "returns no targets when there is no [deploy] section" $
      parseDeployTargets "[workspace]\nmembers = []\nghc = \"9.6.5\"\n" `shouldBe` Right []

  describe "Zinc.Deploy target resolution (nbk.6)" $ do
    let t = DeployTarget "homelab" "gareth@nixos-box:2200" (Just "myapp") ["--port", "8080"] [("RUST_LOG", "info")]
    it "resolves a named target to its host, service, args and env" $
      resolveDeploy [t] "homelab" Nothing
        `shouldBe` ResolvedDeploy (DeployHost (Just "gareth") "nixos-box" (Just 2200)) (Just "myapp") ["--port", "8080"] [("RUST_LOG", "info")]

    it "falls back to an ad-hoc user@host when no named target matches" $
      resolveDeploy [t] "deploy@other-box" Nothing
        `shouldBe` ResolvedDeploy (DeployHost (Just "deploy") "other-box" Nothing) Nothing [] []

    it "lets --service override the configured service" $
      rdService (resolveDeploy [t] "homelab" (Just "override")) `shouldBe` Just "override"

  describe "Zinc.Deploy nix copy + profile (nbk.2)" $ do
    it "builds the ssh-ng store URI from the target (port goes via NIX_SSHOPTS, not the URI)" $ do
      nixCopyStoreUri (DeployHost (Just "gareth") "box" Nothing) `shouldBe` "ssh-ng://gareth@box"
      nixCopyStoreUri (DeployHost Nothing "box" (Just 2222)) `shouldBe` "ssh-ng://box"

    it "builds the nix copy argv with experimental features enabled" $
      nixCopyArgs (DeployHost (Just "gareth") "box" Nothing) "/nix/store/abc-app"
        `shouldBe` ["--extra-experimental-features", "nix-command flakes", "copy", "--to", "ssh-ng://gareth@box", "/nix/store/abc-app"]

    it "passes a non-default port to nix's ssh via NIX_SSHOPTS" $ do
      nixCopyEnv (DeployHost Nothing "box" (Just 2222)) `shouldBe` [("NIX_SSHOPTS", "-p 2222")]
      nixCopyEnv (DeployHost Nothing "box" Nothing) `shouldBe` []

    it "names the per-service profile and installs into it (GC-root + generations)" $ do
      profileName "myapp" `shouldBe` "zinc-myapp"
      let s = profileInstallScript "myapp" "/nix/store/abc-app"
      all
        (`isInfixOf` s)
        [ ".local/state/nix/profiles/zinc-myapp"
        , "profile install --profile"
        , "/nix/store/abc-app"
        ]
        `shouldBe` True

  describe "Zinc.Cabal reexported-modules (jdf)" $ do
    it "reads bare and renamed reexports from a library .cabal" $ do
      let cabal =
            unlines
              [ "cabal-version: 2.4"
              , "name: facade"
              , "version: 1.0"
              , "library"
              , "  build-depends: base"
              , "  reexported-modules: Effectful, Orig as Renamed"
              , "  default-language: Haskell2010"
              ]
      fmap (concatMap compReexports . filter ((== Library) . compKind)) (parseCabalComponents cabal)
        `shouldBe` Right [("Effectful", Nothing, "Effectful"), ("Renamed", Nothing, "Orig")]

  describe "Zinc.Cabal external C libraries (zinc-389)" $
    -- extra-libraries are already link names; a pkgconfig-depends module is
    -- normalized to its link name by dropping a leading 'lib' (libpq -> pq). Both
    -- land in compExtraLibs (the conf's extra-libraries:), and the nixpkgs attr
    -- (postgresql) in compSystemLibs (the env flake).
    it "reads extra-libraries + pkgconfig-depends into link names (libpq -> pq)" $ do
      let cabal =
            unlines
              [ "cabal-version: 2.4"
              , "name: pglib"
              , "version: 1.0"
              , "library"
              , "  build-depends: base"
              , "  extra-libraries: pq"
              , "  pkgconfig-depends: libpq"
              , "  default-language: Haskell2010"
              ]
          libOf f = either (const []) (concatMap f . filter ((== Library) . compKind)) (parseCabalComponents cabal)
      (sort (libOf compExtraLibs), libOf compSystemLibs) `shouldBe` (["pq"], ["postgresql"])

  describe "Zinc.Cabal platform finalization (xum)" $
    -- A wasm build must finalize against Platform Wasm32 Wasi so arch(wasm32)
    -- conditionals pick a package's wasm variant (e.g. miso's ffi/wasm +
    -- ghc-experimental), not the host's vanilla one.
    it "resolves arch(wasm32) conditionals for the wasm platform only" $ do
      let cabal =
            unlines
              [ "cabal-version: 2.2"
              , "name: p"
              , "version: 1.0"
              , "library"
              , "  build-depends: base"
              , "  if arch(wasm32)"
              , "    build-depends: ghc-experimental"
              , "  default-language: Haskell2010"
              ]
          deps plat = either (const []) (concatMap compDepends . filter ((== Library) . compKind)) (parseCabalComponentsForPlatform plat [] "9.6.5" cabal)
      ("ghc-experimental" `elem` deps buildPlatform, "ghc-experimental" `elem` deps (Platform Wasm32 Wasi))
        `shouldBe` (False, True)

  describe "Zinc.Build packageFlags zinc-built vs toolchain (iaj merge regression)" $ do
    -- A dep can be BOTH zinc-built (in the workspace db, bare unit-id) AND present
    -- in the toolchain's global db with a hashed id (e.g. ansi-terminal-types,
    -- which the flake's hspec drags in). zinc's own build must win: -package-id by
    -- bare name, checked before the toolchain -package branch — else GHC may link
    -- the wrong instance and the conf's reexport origin won't match its depends.
    it "pins a zinc-built dep with -package-id even when it is also toolchain-provided" $
      packageFlags ["ansi-terminal-types"] ["ansi-terminal-types", "base"] ["ansi-terminal-types"]
        `shouldBe` ["-package", "base", "-package-id", "ansi-terminal-types"]
    it "exposes a toolchain-only dep by name (-package), not -package-id" $
      packageFlags [] ["optparse-applicative", "base"] ["optparse-applicative"]
        `shouldBe` ["-package", "base", "-package", "optparse-applicative"]
    it "zincBuiltUnitIds lists the bare ids zinc registered into a db" $ do
      let dir = "/tmp/zinc-builtids-test"
      stale <- doesDirectoryExist dir
      when stale $ removeDirectoryRecursive dir
      createDirectoryIfMissing True dir
      writeFile (dir ++ "/ansi-terminal-types.conf") "name: ansi-terminal-types\n"
      writeFile (dir ++ "/colour.conf") "name: colour\n"
      writeFile (dir ++ "/package.cache") "" -- not a .conf; must be ignored
      ids <- zincBuiltUnitIds dir
      sort ids `shouldBe` ["ansi-terminal-types", "colour"]

  describe "Zinc.Cabal manual flag assignments → finalizePD (zinc-iaj.2)" $ do
    -- A manual flag (use-pkg-config) gates which build-depends the library gets.
    -- Default (false) → the configure provider's dep; setting it true must pick
    -- the pkg-config provider's dep, proving the flag reaches finalizePD. This is
    -- the postgresql-libpq shape that unblocks the Manifest project.
    let cabal =
          unlines
            [ "cabal-version: 2.2"
            , "name: pq"
            , "version: 1.0"
            , "flag use-pkg-config"
            , "  default: False"
            , "  manual: True"
            , "library"
            , "  build-depends: base"
            , "  if flag(use-pkg-config)"
            , "    build-depends: pkgconfig-provider"
            , "  else"
            , "    build-depends: configure-provider"
            , "  default-language: Haskell2010"
            ]
        deps flags = either (const []) (concatMap compDepends . filter ((== Library) . compKind)) (parseCabalComponentsForPlatform buildPlatform flags "9.6.5" cabal)

    it "takes the automatic-flag default when no manual flag is given" $ do
      ("configure-provider" `elem` deps [], "pkgconfig-provider" `elem` deps [])
        `shouldBe` (True, False)

    it "honors a manual flag, flipping which provider dep is selected" $ do
      ("configure-provider" `elem` deps [("use-pkg-config", True)], "pkgconfig-provider" `elem` deps [("use-pkg-config", True)])
        `shouldBe` (False, True)

  describe "Zinc.Build wasm support gate (9po.3, 90t)" $ do
    let pureLib =
          Component
            { compKind = Library
            , compName = "lib"
            , compSourceDirs = ["src"]
            , compModules = []
            , compMain = Nothing
            , compExtensions = []
            , compGhcOptions = []
            , compDepends = ["base"]
            , compSystemLibs = []
            , compExtraLibs = []
            , compIncludeDirs = []
            , compCppOptions = []
            , compCSources = []
            , compReexports = []
            , compWasmExports = []
            , compFromCabal = False
            }
        code = either (Just . errorCode) (const Nothing)
    it "passes a pure-Haskell component for both native and wasm" $
      (code (wasmSupported Native pureLib), code (wasmSupported Wasm32Wasi pureLib)) `shouldBe` (Nothing, Nothing)
    it "allows C sources for wasm (the toolchain's clang cross-compiles them; 90t)" $ do
      -- C sources used to be rejected (9po.3 MVP); the wasm toolchain compiles
      -- portable C (e.g. miso's cbits/foreign.c), so they are now supported.
      let withC = pureLib {compCSources = ["cbits/x.c"]}
      (code (wasmSupported Native withC), code (wasmSupported Wasm32Wasi withC))
        `shouldBe` (Nothing, Nothing)
    it "still rejects system libraries (extra-libraries) for wasm only" $
      (code (wasmSupported Native (pureLib {compSystemLibs = ["zlib"]})), code (wasmSupported Wasm32Wasi (pureLib {compSystemLibs = ["zlib"]})))
        `shouldBe` (Nothing, Just "ZINC_WASM_UNSUPPORTED")

  describe "Zinc.Build reactor link flags (9po.5)" $ do
    it "emits no-hs-main, reactor exec-model, an auto hs_init export, and one --export per symbol" $
      reactorLinkFlags ["hs_start", "myFunc"]
        `shouldBe` ["-no-hs-main", "-optl-mexec-model=reactor", "-optl-Wl,--export=hs_init", "-optl-Wl,--export=hs_start", "-optl-Wl,--export=myFunc"]
    it "does not double-export hs_init when the user lists it" $
      reactorLinkFlags ["hs_init", "hs_start"]
        `shouldBe` ["-no-hs-main", "-optl-mexec-model=reactor", "-optl-Wl,--export=hs_init", "-optl-Wl,--export=hs_start"]

  describe "Zinc.Cabal.bootConflicts (sib)" $ do
    let isBoot = (`elem` ["transformers", "base"])
        toolchain = [("transformers", [0, 6, 1, 0]), ("base", [4, 18, 0, 0])]
        cabalWith dep =
          unlines
            [ "cabal-version: 2.4"
            , "name: demo"
            , "version: 1.0"
            , "library"
            , "  build-depends: base, " ++ dep
            , "  default-language: Haskell2010"
            ]
    it "flags a boot-lib bound that excludes the toolchain version" $
      fmap (map (\(n, _, t) -> (n, t))) (bootConflicts isBoot toolchain "9.6.5" (cabalWith "transformers >=0.2 && <0.6"))
        `shouldBe` Right [("transformers", "0.6.1.0")]
    it "passes when the bound admits the toolchain version" $
      bootConflicts isBoot toolchain "9.6.5" (cabalWith "transformers >=0.2 && <0.7") `shouldBe` Right []
    it "ignores a version-pinned non-boot dep" $
      bootConflicts isBoot toolchain "9.6.5" (cabalWith "regex-base <0.1") `shouldBe` Right []

  describe "ZINC_DEP_BOOT_CONFLICT diagnostic (sib)" $ do
    let e = DepBootConflict "monad-control" "transformers" ">=0.2 && <0.6" "0.6.1.0" Nothing
        d = toDiagnostic e
    it "has the stable code, resolution exit category, package and a nextAction" $
      (errorCode e, exitCodeFor e, diagPackage d, isJust (diagNextAction d))
        `shouldBe` ("ZINC_DEP_BOOT_CONFLICT", ExitFailure 3, Just "monad-control", True)
    it "names the exact forward commit when the HEAD-probe found one" $
      (diagNextAction (toDiagnostic (DepBootConflict "monad-control" "transformers" "<0.6" "0.6.1.0" (Just "3785240")))
        >>= \na -> if "3785240" `isInfixOf` na then Just () else Nothing)
        `shouldBe` Just ()

  describe "zinc-iaj.1 (don't auto-sweep cabal deps; exclude Setup.hs)" $ do
    it "discoverModules excludes Setup.hs (and Main)" $ do
      let dir = "/tmp/zinc-iaj1-discover"
      createDirectoryIfMissing True dir
      writeFile (dir </> "A.hs") "module A where\n"
      writeFile (dir </> "Setup.hs") "import Distribution.Simple\nmain = defaultMain\n"
      writeFile (dir </> "Main.hs") "main = pure ()\n"
      mods <- discoverModules [dir]
      mods `shouldContain` ["A"]
      mods `shouldNotContain` ["Setup"]
      mods `shouldNotContain` ["Main"]

    it "a native [build.lib] component has compFromCabal == False" $ do
      let src = unlines
            [ "[package]"
            , "name = \"native\""
            , "version = \"1.0\""
            , "[build.lib]"
            , "source-dirs = [\"src\"]"
            ]
      case parseMember src of
        Right mm ->
          fmap compFromCabal (find ((== Library) . compKind) (pkgComponents mm))
            `shouldBe` Just False
        Left err -> expectationFailure err

    it "a cabal-derived library component has compFromCabal == True" $ do
      let cabal = unlines
            [ "cabal-version: 2.4"
            , "name: demo"
            , "version: 1.0"
            , ""
            , "library"
            , "  exposed-modules: Demo"
            , "  hs-source-dirs: src"
            , "  build-depends: base"
            , "  default-language: Haskell2010"
            ]
      case parseCabalComponents cabal of
        Right comps ->
          fmap compFromCabal (find ((== Library) . compKind) comps)
            `shouldBe` Just True
        Left err -> expectationFailure err
