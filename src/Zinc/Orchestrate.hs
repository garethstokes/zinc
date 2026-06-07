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
  , runBuildMember
  , runBuildReport
  , runWarm
  , buildAndRun
  , resolveRunTarget
  , resolveRunTargetFor
  , resolveTarget
  , runTests
  , orderMembers
  , lockDrift
  , checkLockDrift
  , runRepl
  , runClean
  , runCachePush
  , runPackage
  , parMapBounded
  ) where

import Control.Concurrent (forkIO, getNumCapabilities)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import GHC.Clock (getMonotonicTime)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (SomeException, finally, try)
import Control.Monad (filterM, forM, forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Bifunctor (first)
import Data.Char (isHexDigit)
import Data.List (isSuffixOf, stripPrefix)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe)
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, findExecutable, listDirectory, makeAbsolute, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, takeFileName, (</>))
import System.Process (callProcess, readProcess, readProcessWithExitCode)
import Zinc.Build (LibBuild (..), MemberBuild (..), buildLibArtifactsFor, buildLibFor, buildMemberFor, initPackageDb, initPackageDbFor, installedVersionsFor, isRegistered, memberBuildDir, registeredExposedMatches, registerPackage, replArgs)
import Zinc.Cabal (bootConflicts, cabalBuildType, cabalVersion, parseCabalComponentsForPlatform)
import Zinc.Cache (BuildKey (..), buildCacheKey, buildCacheKeyFor, storeConfPath, storePkgPath)
import Zinc.Configure (configureComponent)
import Zinc.Target (Target (Native), isWasm)
import Distribution.System (Arch (Wasm32), OS (Wasi), Platform (Platform), buildPlatform)
import Zinc.CacheBackend (CacheBackend (cbPull, cbPush), CacheConfig (ccReadUrls, ccWriteUrl), PullOutcome (Pulled), httpBackend, resolveCacheConfig)
import Zinc.Quirks (quirkGhcOptions)
import Zinc.Fetch (packageDirIn)
import Zinc.Git (cloneAt)
import Zinc.Hackage (fetchHackageTarball)
import Zinc.Lock (LockedPackage (..), Source (..), lockRepo, lockRev, parseLock, srcKey)
import Zinc.Manifest
  ( Component (compDepends, compGhcOptions, compKind)
  , ComponentKind (Executable, Library, TestSuite)
  , Dependency (depName)
  , MemberManifest (pkgComponents, pkgName, pkgVersion)
  , Ref (Latest)
  , WorkspaceManifest (wsDependencies, wsGhc, wsMembers)
  , parseMember
  , parseWorkspace
  , depGhcOptionsOf
  , depFlagsOf
  )
import Zinc.Diagnostic (ZincError (AmbiguousTarget, ContentHashMismatch, DepBootConflict, ManifestParse, NixAbsent, NoZincToml, OtherError, StaticUnsupported, ToolchainMissing))
import Zinc.Package (PackageFormat (..), dockerImageRef, packagingFlake, storePathRefs)
import Zinc.Except (Result, failWith, failWithError, liftEither, liftEitherE, liftIO, orFail, orFailE, runResult)
import Zinc.Output (OutputEvent (..), Sink, emit, nullSink)
import Zinc.Report (BuildOutcome (..), PackageReport (..), PackageStatus (..), Timing (..), cacheStatsOf)
import Zinc.Resolve (ResolvedDep (..), isBootLib, topoLevels)
import Zinc.Store (contentHash, resolveStoreRoot, storeSrcPath, withStoreLock)

-- | Build a workspace: each member's library (so siblings can link) plus every
-- component whose kind satisfies @keep@, returned as built executable paths.
-- @target@ (when 'Just') restricts which member's @keep@-components are built;
-- libraries are always built so dependencies remain available.
-- | Fail with a clear, categorized diagnostic if the GHC toolchain zinc shells
-- out to is not on PATH (spec §4.2: detect + guide before invoking it), rather
-- than a raw "ghc: command not found". zinc never installs a toolchain — it
-- points the user at `nix develop`. Guards build/run/test/repl (via
-- 'buildWorkspaceReport') and warm.
ensureToolchain :: Result ()
ensureToolchain = do
  ghc <- liftIO (findExecutable "ghc")
  when (isNothing ghc) (failWithError (ToolchainMissing "ghc"))

buildWorkspace :: FilePath -> Maybe String -> (ComponentKind -> Bool) -> IO (Either ZincError [FilePath])
buildWorkspace wsDir member keep = fmap (fmap (\(o, _, _) -> boExes o)) (buildWorkspaceReport nullSink Native wsDir member Nothing keep)

-- | As 'buildWorkspace', but also returns the per-package closure report (spec
-- §3.2) and per-phase wall-clock timings (perf spec §2) for the structured
-- @--json@ surface. 'buildWorkspace' is the thin exes-only projection.
buildWorkspaceReport :: Sink -> Target -> FilePath -> Maybe String -> Maybe String -> (ComponentKind -> Bool) -> IO (Either ZincError (BuildOutcome, [(String, Int)], [(String, Int)]))
buildWorkspaceReport sink target wsDir member ghcOverride keep = runResult $ do
  ensureToolchain
  let wsFile = wsDir </> "zinc.toml"
  present <- liftIO (doesFileExist wsFile)
  when (not present) $ failWithError (NoZincToml wsDir)
  wsSrc <- liftIO $ readFile wsFile
  ws <- liftEitherE (first (ManifestParse wsFile) (parseWorkspace wsSrc))
  -- One GHC per build (ey4): a @--ghc@ override replaces the workspace's pinned
  -- GHC for the WHOLE closure + members (the store keys on it, so each version
  -- builds once); the toolchain is provisioned for it up front (y03).
  let effectiveGhc = fromMaybe (wsGhc ws) ghcOverride
  members <- traverse loadMember (wsMembers ws)
  let wsDb = wsDir </> ".zinc" </> "pkgdb"
  -- Keep the package db across builds (inner-loop incrementality): registration
  -- is idempotent (ghc-pkg register --force) and the closure builder skips deps
  -- already registered at their current key-addressed pkg dir.
  orFail (initPackageDbFor target wsDb)
  storeRoot <- liftIO resolveStoreRoot
  -- Coarse phases (perf spec §2): the dependency-closure build (fetch + compile
  -- + register of deps) and the workspace-member build (compile + link). Finer
  -- breakdown (resolve/provision/fetch/register/link split) is a follow-up.
  -- Shared accumulator for the finer cumulative per-phase breakdown (nti.3),
  -- written from the parallel closure builds and the member builds alike.
  acc <- liftIO (newIORef Map.empty)
  (pkgs, closureMs) <- timed (orFailE (buildClosure sink target wsDir storeRoot wsDb effectiveGhc (depGhcOptionsOf ws) (depFlagsOf ws) (Just acc)))
  (exes, memberMs) <- timed (concat <$> traverse (buildMemberAll (Just acc) wsDb) (orderMembers members))
  breakdownMap <- liftIO (readIORef acc)
  -- Present in build order, only the phases that actually ran.
  let breakdown = [(p, ms) | p <- ["fetch", "compile", "register", "link"], Just ms <- [Map.lookup p breakdownMap]]
  pure (BuildOutcome exes pkgs, [("closure", closureMs), ("member", memberMs)], breakdown)
  where
    loadMember member = do
      let dir = wsDir </> member
      src <- liftIO $ readFile (dir </> "zinc.toml")
      mem <- liftEitherE (first (ManifestParse (dir </> "zinc.toml")) (parseMember src))
      pure (dir, mem)

    -- Build a member's library (so siblings/exes can link it), then every
    -- @keep@-selected component, returning the executable paths. Brackets the
    -- member's compile with CompileStart/Done events (the visible "build zinc"
    -- step) carrying its own wall-clock.
    -- @acc@ collects the finer breakdown (nti.3): the member library counts as
    -- @compile@, its executables as @link@ (the final ghc --make against the libs).
    buildMemberAll acc wsDb (dir, mem) = do
      liftIO (emit sink (CompileStart (pkgName mem)))
      t0 <- liftIO getMonotonicTime
      case filter ((== Library) . compKind) (pkgComponents mem) of
        []        -> pure ()
        (lib : _) -> do
          -- Absolute lib dir so the registered library-dirs is absolute: ghc-pkg
          -- rejects relative paths, and (with the db now persisted) a relative
          -- entry can't be cleanly re-registered across builds.
          libDir <- liftIO (makeAbsolute (dir </> ".zinc" </> "lib"))
          orFailE (accuminto acc "compile" (buildLibFor target (LibBuild dir libDir wsDb (pkgName mem) (pkgVersion mem) lib)))
      exes <- traverse (\comp -> orFailE (accuminto acc "link" (buildMemberFor target (MemberBuild dir (memberBuildDir dir) (Just wsDb) comp)))) (wanted mem)
      t1 <- liftIO getMonotonicTime
      liftIO (emit sink (CompileDone (pkgName mem) (round ((t1 - t0) * 1000) :: Int) False))
      pure exes

    wanted mem
      | maybe True (== pkgName mem) member = filter (keep . compKind) (pkgComponents mem)
      | otherwise = []

-- | @zinc build@: build every member's executables (libraries first).
runBuild :: FilePath -> IO (Either ZincError [FilePath])
runBuild wsDir = buildWorkspace wsDir Nothing (== Executable)

-- | @zinc build \<member\>@: build only the named member's executables.
runBuildMember :: FilePath -> Maybe String -> IO (Either ZincError [FilePath])
runBuildMember wsDir target = buildWorkspace wsDir target (== Executable)

-- | @zinc build --deps-only@ / @zinc warm@: resolve + build the dependency
-- closure into the store WITHOUT building workspace members, so the
-- slow-stable closure can be its own Docker layer / CI cache entry, separate
-- from fast-changing source (ephemeral-builds spec §3). Returns the per-package
-- closure report.
runWarm :: Sink -> Target -> FilePath -> Maybe String -> IO (Either ZincError [PackageReport])
runWarm sink target wsDir ghcOverride = runResult $ do
  ensureToolchain
  let wsFile = wsDir </> "zinc.toml"
  present <- liftIO (doesFileExist wsFile)
  when (not present) $ failWithError (NoZincToml wsDir)
  wsSrc <- liftIO (readFile wsFile)
  ws <- liftEitherE (first (ManifestParse wsFile) (parseWorkspace wsSrc))
  let wsDb = wsDir </> ".zinc" </> "pkgdb"
  -- Build the closure for the SELECTED target (zinc-hte): --deps-only/warm used
  -- to hardcode Native, so `--target wasm32-wasi` silently produced native
  -- objects. The pkgdb + store keys are target-scoped, like the full build.
  orFail (initPackageDbFor target wsDb)
  storeRoot <- liftIO resolveStoreRoot
  orFailE (buildClosure sink target wsDir storeRoot wsDb (fromMaybe (wsGhc ws) ghcOverride) (depGhcOptionsOf ws) (depFlagsOf ws) Nothing)

-- | @zinc build [member] --json@: build, returning the structured outcome
-- (executables + per-package closure report) and the 'Timing' block (total
-- wall-clock, per-phase durations, cache stats) for the machine surface.
runBuildReport :: Sink -> Target -> FilePath -> Maybe String -> Maybe String -> IO (Either ZincError (BuildOutcome, Timing))
runBuildReport sink target wsDir member ghcOverride = do
  t0 <- getMonotonicTime
  r <- buildWorkspaceReport sink target wsDir member ghcOverride (== Executable)
  t1 <- getMonotonicTime
  let totalMs = round ((t1 - t0) * 1000) :: Int
  pure $ fmap (\(o, phases, breakdown) -> (o, Timing totalMs phases breakdown (cacheStatsOf (boPackages o)))) r

-- | Time an IO step and add its duration (ms) to a named bucket in a shared
-- accumulator, for the cumulative per-phase breakdown (zinc-nti.3). Safe to call
-- from the parallel 'produceOne' threads — 'atomicModifyIORef'' serialises the
-- updates. 'Nothing' (e.g. @warm@) is a no-op, so instrumented call sites stay
-- uniform whether or not a breakdown is being collected.
accuminto :: Maybe (IORef (Map.Map String Int)) -> String -> IO a -> IO a
accuminto Nothing _ act = act
accuminto (Just ref) name act = do
  t0 <- getMonotonicTime
  a <- act
  t1 <- getMonotonicTime
  let ms = round ((t1 - t0) * 1000) :: Int
  atomicModifyIORef' ref (\m -> (Map.insertWith (+) name ms m, ()))
  pure a

-- | Run a pipeline step, returning its wall-clock duration in milliseconds.
timed :: Result a -> Result (a, Int)
timed act = do
  t0 <- liftIO getMonotonicTime
  a <- act
  t1 <- liftIO getMonotonicTime
  pure (a, round ((t1 - t0) * 1000))

-- | @zinc run@: build, then run the first executable with the given args,
-- returning its stdout. (Retained for the test surface; the CLI uses
-- 'runTarget' for proper target selection.)
buildAndRun :: FilePath -> [String] -> IO (Either ZincError String)
buildAndRun wsDir args = runResult $ do
  built <- orFailE (runBuild wsDir)
  case built of
    []        -> failWith "no executable to run"
    (exe : _) -> liftIO (readProcess exe args "")

-- | Resolve a @zinc run@ TARGET against the workspace's built executables
-- (exe name -> path). 'Nothing' selects the sole exe (error if zero, ambiguous
-- if many); a 'Just' name matches by exe name, or the @exe@ part of a
-- @member:exe@ qualifier. An unknown or ambiguous target yields a structured
-- 'AmbiguousTarget' listing the candidates (spec §6 taxonomy).
resolveTarget :: Maybe String -> [(String, FilePath)] -> Either ZincError FilePath
resolveTarget Nothing exes = case exes of
  []       -> Left (OtherError "no executable to run")
  [(_, e)] -> Right e
  _        -> Left (AmbiguousTarget (map fst exes))
resolveTarget (Just t) exes =
  maybe (Left (AmbiguousTarget (map fst exes))) Right (lookup (exeName t) exes)
  where
    exeName s = case break (== ':') s of
      (_, ':' : e) -> e -- member:exe -> exe
      _            -> s

-- | @zinc run [TARGET]@: build the workspace and resolve TARGET to a single
-- executable path. The caller execs it (inheriting stdio, propagating the exit
-- code) — building is separated from running so @run@ has live, interactive I/O.
resolveRunTarget :: FilePath -> Maybe String -> IO (Either ZincError FilePath)
resolveRunTarget = resolveRunTargetFor Native

-- | As 'resolveRunTarget', for an explicit 'Target' (zinc-9po.4): builds the
-- workspace for the target and resolves the selector to one artifact. A wasm
-- artifact is @\<name\>.wasm@, so the selector matches on the name with any
-- @.wasm@ suffix stripped. Native is byte-identical.
resolveRunTargetFor :: Target -> FilePath -> Maybe String -> IO (Either ZincError FilePath)
resolveRunTargetFor target wsDir sel = runResult $ do
  (outcome, _, _) <- orFailE (buildWorkspaceReport nullSink target wsDir Nothing Nothing (== Executable))
  liftEitherE (resolveTarget sel [(runName e, e) | e <- boExes outcome])
  where
    runName e = let n = takeFileName e in if ".wasm" `isSuffixOf` n then take (length n - 5) n else n

-- | @zinc test@: build and run all test-suite components, returning how many
-- passed. Fails on the first non-zero exit.
runTests :: FilePath -> IO (Either ZincError Int)
runTests wsDir = runResult $ do
  exes <- orFailE (buildWorkspace wsDir Nothing (== TestSuite))
  mapM_ runOne exes
  pure (length exes)
  where
    runOne exe = do
      (code, _, _) <- liftIO (readProcessWithExitCode exe [] "")
      case code of
        ExitSuccess   -> pure ()
        ExitFailure _ -> failWith (exe ++ ": test suite failed")

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

-- | Map an effectful, fallible action over a list with bounded concurrency
-- (at most @n@ in flight). Each task runs on its own thread, gated by a
-- semaphore; results come back in input order. Exceptions are reflected as
-- @Left@ rather than killing the batch. Under the non-threaded RTS this is
-- effectively serial (capabilities = 1), which keeps behaviour identical to a
-- sequential build; the -threaded zinc binary gets real overlap.
parMapBounded :: Int -> (a -> IO (Either ZincError b)) -> [a] -> IO [Either ZincError b]
parMapBounded n f xs = do
  sem <- newQSem (max 1 n)
  mvars <- mapM (spawn sem) xs
  mapM takeMVar mvars
  where
    spawn sem x = do
      mv <- newEmptyMVar
      _ <- forkIO $ do
        waitQSem sem
        r <- try (f x) `finally` signalQSem sem
        putMVar mv (either (\e -> Left (OtherError (show (e :: SomeException)))) id r)
      pure mv

-- | Build the resolved git-dependency closure (from @zinc.lock@) from source
-- into the workspace package db, in dependency order, so members can link it.
-- Each locked package is fetched at its exact commit, its zinc.toml read, and
-- its library compiled + registered. (Compiling arbitrary upstream packages
-- with Setup.hs / Template Haskell / deep closures is a further follow-up;
-- this handles zinc-native git library deps.)
buildClosure :: Sink -> Target -> FilePath -> FilePath -> FilePath -> String -> [(String, [String])] -> [(String, [(String, Bool)])] -> Maybe (IORef (Map.Map String Int)) -> IO (Either ZincError [PackageReport])
buildClosure sink target wsDir storeRoot wsDb ghcVersion buildOpts depFlagsMap mAcc = runResult $ do
  let lockFile = wsDir </> "zinc.lock"
  present <- liftIO (doesFileExist lockFile)
  if not present
    then pure []
    else do
      liftIO (emit sink ResolveStart)
      locks <- liftEither . parseLock =<< liftIO (readFile lockFile)
      liftIO (emit sink (Plan (length locks)))
      levels <- liftEitherE (topoLevels (map toResolved locks))
      let byName = Map.fromList [(lockName l, l) | l <- locks]
      concat <$> traverse (buildLevel . map ((byName Map.!) . rdName)) levels
  where
    toResolved l = ResolvedDep (lockName l) (lockRepo l) Latest (lockDepends l)

    -- Build the closure level by level (spec §7). Nodes within a level are
    -- mutually independent, so compile them concurrently; then register their
    -- confs serially — ghc-pkg register on the shared wsDb is not
    -- concurrency-safe, and the next level's compiles need this level
    -- registered first. Concurrency is bounded by the capability count, so the
    -- non-threaded test harness stays correct-but-serial while the -threaded
    -- zinc binary actually overlaps the compiles.
    -- Compile a level's nodes concurrently, then register their confs serially,
    -- skipping deps already registered at their current key-addressed pkg dir.
    buildLevel level = do
      n <- liftIO getNumCapabilities
      produced <- liftIO (parMapBounded n produceOne level)
      results <- liftEitherE (sequence produced)
      mapM_ registerNeeded (mapMaybe snd results)
      pure (map fst results)

    registerNeeded (unitId, pkgOut, conf) = do
      done <- liftIO (isRegistered wsDb unitId pkgOut)
      -- Re-register even when the pkg dir is unchanged if the registered conf has
      -- drifted from the one we hold — a persisted wsDb can carry a stale conf
      -- after a zinc upgrade adds content (e.g. jdf's reexports), which would
      -- otherwise stay invisible to consumers (zinc-0k7).
      fresh <- liftIO (if done then registeredExposedMatches wsDb unitId conf else pure False)
      when (not done || not fresh) (orFail (accuminto mAcc "register" (registerPackage wsDb conf)) >> liftIO (emit sink (RegisterDone unitId)))

    -- Content-addressed cache key from data available without the source, so a
    -- cached build is reused without even fetching. Includes the dep's
    -- [build-options] override so changing an override invalidates the cache.
    cacheKeyOf l = buildCacheKeyFor target (BuildKey (lockName l) (lockRev l) ghcVersion (lockDepends l) (overrideFor l) (flagsFor l))

    -- A dep's effective ghc-option override: the built-in quirk for the package
    -- (zinc-8uh) PLUS any workspace [build-options]. The quirk leads so a known
    -- fix (e.g. colour -XSafe) applies even when the workspace lists nothing.
    overrideFor l = quirkGhcOptions (lockName l) ++ fromMaybe [] (lookup (lockName l) buildOpts)

    -- Produce a closure node's library WITHOUT registering it: returns the conf
    -- text to register (@Just@), or @Nothing@ when the dep ships no library.
    -- No shared-state writes, so this is safe to run concurrently across the
    -- independent nodes of one level.
    -- Fast path: a cached conf is reused without locking. Otherwise take the
    -- per-key store lock (so parallel agents/worktrees can't write pkg/<key>
    -- concurrently), re-check the cache inside the lock — a peer may have built
    -- it while we waited — and only then fetch + compile.
    produceOne l = do
      let key = cacheKeyOf l
          confPath = storeConfPath storeRoot key
      cached <- doesFileExist confPath
      if cached
        then reuseCached l key
        else withStoreLock storeRoot key $ do
          nowCached <- doesFileExist confPath
          pulled <- if nowCached then pure False else tryRemotePull (lockName l) key confPath
          if nowCached || pulled
            then reuseCached l key
            else do
              -- Per-package wall-clock build time (perf spec §3.2). Stamped onto
              -- whatever report buildNode produces (built or skipped).
              t0 <- getMonotonicTime
              r <- runResult (buildNode l key)
              t1 <- getMonotonicTime
              let ms = round ((t1 - t0) * 1000) :: Int
              case r of
                Right (rep, _) | prStatus rep == Built -> emit sink (CompileDone (lockName l) ms False)
                _ -> pure ()
              pure (fmap (\(rep, m) -> (rep {prTimeMs = Just ms}, m)) r)

    -- L2 remote cache (vwn.4): on a local miss, try the configured backend
    -- (ZINC_CACHE) before compiling. A hit unpacks the artifact into the local
    -- store; we accept it only when it carries BOTH the .conf and the library
    -- archive — a light integrity check; full signing/hash-verify is vwn.6.
    -- Any miss/error falls back to a local compile, so the build always
    -- succeeds offline and is unchanged when no remote is configured.
    tryRemotePull name key confPath = do
      cfg <- resolveCacheConfig wsDir
      let verified = do
            confOk <- doesFileExist confPath
            aOk <- doesFileExist (storePkgPath storeRoot key </> ("libHS" ++ name ++ ".a"))
            pure (confOk && aOk)
          tryUrls [] = pure False
          tryUrls (url : rest) = do
            outcome <- cbPull (httpBackend url) key storeRoot
            case outcome of
              Pulled  -> verified >>= \ok -> if ok then pure True else tryUrls rest
              _       -> tryUrls rest
      tryUrls (ccReadUrls cfg)

    reuseCached l key = do
      conf <- readFile (storeConfPath storeRoot key)
      emit sink (CompileDone (lockName l) 0 True)
      pure (Right (PackageReport (lockName l) (lockRev l) Cached (Just 0), Just (lockName l, storePkgPath storeRoot key, conf)))

    buildNode l key = do
      let pkgOut = storePkgPath storeRoot key
          report st = PackageReport (lockName l) (lockRev l) st Nothing
          -- Keyed by repo (srcKey), so a monorepo's sub-packages share ONE clone
          -- instead of each re-cloning the identical tree (zinc-qln).
          dest = storeSrcPath storeRoot (srcKey l) (lockRev l)
          -- A clone-scoped lock so sibling sub-packages — which hold distinct
          -- build-key locks — can't race to clone the same checkout. Re-checked
          -- inside the lock (a peer may have just cloned it).
          srcLockKey = "src-" ++ takeFileName dest
      fetched <- liftIO $ withStoreLock storeRoot srcLockKey $ do
        exists <- doesDirectoryExist dest
        if exists
          then pure (Right ())
          else do
            emit sink (FetchStart (lockName l))
            -- Fetch the pinned source by its kind: a git clone, or a Hackage sdist
            -- tarball for a vendored pin (b1z). Either way the bytes are verified
            -- against the lock's sha256 below, so build never resolves a version —
            -- it only retrieves the already-pinned content.
            let fetchLocked = case lockSource l of
                  GitSource repo rev -> fmap (const ()) <$> cloneAt repo rev dest
                  TarballSource ver  -> fmap (const ()) <$> fetchHackageTarball (lockName l) ver dest
            r <- accuminto mAcc "fetch" (first (("fetch " ++ lockName l ++ ": ") ++) <$> fetchLocked)
            case r of
              Right _ -> emit sink (FetchDone (lockName l)) >> pure (Right ())
              Left e  -> pure (Left e)
      orFail (pure fetched)
      orFailE (verifyFetched l dest)
      -- The package may live in a subdirectory of the repo (monorepo) — an
      -- explicit #subdir, or a <name>/ dir auto-detected for metadata-poor
      -- monorepos. Resolve it the same way the resolver did.
      pkgDir <- liftIO (packageDirIn dest (lockRepo l) (lockName l))
      comps <- liftIO (loadDepComponents (flagsFor l) pkgDir)
      (version, components) <- liftEither (first ((lockName l ++ ": ") ++) comps)
      case filter ((== Library) . compKind) components of
        []        -> pure (report Skipped, Nothing) -- no library to build
        (lib : _) -> do
          -- Apply any per-dependency build overrides (extra ghc flags,
          -- e.g. -XSafe) from the workspace [build-options].
          let lib' = lib {compGhcOptions = compGhcOptions lib ++ overrideFor l}
          -- bxw.2: a build-type:Configure dep (e.g. network) needs ./configure
          -- run to generate its system-probed headers (HsNetworkConfig.h) before
          -- the .hsc preprocess. Run it from the Hackage sdist (the git checkout
          -- ships only configure.ac) and fold the generated include dir into the
          -- component. Skipped for wasm, where a Configure/C dep is unsupported.
          lib2 <-
            if isWasm target
              then pure lib'
              else orFail (configureComponent (lockName l) version pkgDir pkgOut lib')
          liftIO (emit sink (CompileStart (lockName l)))
          built <- liftIO (accuminto mAcc "compile" (buildLibArtifactsFor target (LibBuild pkgDir pkgOut wsDb (lockName l) version lib2)))
          case built of
            Right (conf, _) -> pure (report Built, Just (lockName l, pkgOut, conf))
            -- sib: cabal version bounds are ADVISORY (GHC ignores them), so we
            -- never block a build that would succeed. Only when the compile
            -- actually FAILS do we check whether a stale boot-library bound
            -- explains it — and if so, replace the cryptic GHC error (e.g.
            -- "ErrorT not in scope") with a typed ZINC_DEP_BOOT_CONFLICT naming
            -- the package, the boot lib + both versions, and the forward-pin.
            Left e -> do
              conflict <- liftIO (checkBootConflict l pkgDir)
              orFailE (pure (Left (either id (const e) conflict)))

    -- sib: for a cabal-based dep, fail with a typed ZINC_DEP_BOOT_CONFLICT if its
    -- declared bound on a GHC boot library excludes the toolchain's version (a
    -- stale tag). zinc-native deps (no .cabal) carry no such bounds — skip.
    -- On conflict, probe the repo's default HEAD: if HEAD relaxes the bound, name
    -- that exact commit so the nextAction is a copy-paste forward-pin.
    checkBootConflict l pkgDir = do
      entries <- listDirectory pkgDir
      case filter ((== ".cabal") . takeExtension) entries of
        []          -> pure (Right ())
        (cabal : _) -> do
          src <- readFile (pkgDir </> cabal)
          installed <- installedVersionsFor target
          case bootConflicts isBootLib installed ghcVersion src of
            Right ((bootLib, range, ver) : _) -> do
              suggested <- probeHeadFix l installed
              pure (Left (DepBootConflict (lockName l) bootLib range ver suggested))
            _ -> pure (Right ())

    -- HEAD-probe (sib): clone the dep's default branch, and if its .cabal no
    -- longer conflicts with the toolchain, return that HEAD commit as the
    -- suggested forward-pin. Best-effort + on the (rare) error path: any failure
    -- degrades to the generic suggestion (Nothing). The bound stays advisory.
    probeHeadFix l installed = do
      let tmp = storeRoot </> "boot-probe" </> lockName l
      stale <- doesDirectoryExist tmp
      when stale (removeDirectoryRecursive tmp)
      cloned <- cloneAt (lockRepo l) "HEAD" tmp
      result <- case cloned of
        Left _ -> pure Nothing
        Right sha -> do
          headPkgDir <- packageDirIn tmp (lockRepo l) (lockName l)
          headEntries <- listDirectory headPkgDir
          case filter ((== ".cabal") . takeExtension) headEntries of
            (cabal : _) -> do
              headSrc <- readFile (headPkgDir </> cabal)
              pure $ case bootConflicts isBootLib installed ghcVersion headSrc of
                Right [] -> Just sha -- HEAD admits the toolchain version
                _        -> Nothing
            [] -> pure Nothing
      done <- doesDirectoryExist tmp
      when done (removeDirectoryRecursive tmp)
      pure result

    -- Tamper detection (spec §8): a fetched tree's content hash must match the
    -- lock's recorded sha256. Only enforced for real-shaped hashes so that
    -- placeholder shas (fixtures, pre-freeze locks) don't block the build.
    verifyFetched l dest
      | looksRealSha (lockSha256 l) = do
          got <- contentHash dest
          pure $
            if got == lockSha256 l
              then Right ()
              else Left (ContentHashMismatch (lockName l) (lockSha256 l) got)
      | otherwise = pure (Right ())

    looksRealSha s = case stripPrefix "sha256:" s of
      Just h  -> length h == 64 && all isHexDigit h
      _       -> False

    -- Per-dependency manual cabal flags (zinc-iaj.2): the workspace's
    -- [dependencies.<name>].flags, keyed by package name, so finalizePD can
    -- select a non-default flavor (e.g. postgresql-libpq's pkg-config provider).
    flagsFor l = fromMaybe [] (lookup (lockName l) depFlagsMap)

    -- A dependency's components come from its zinc.toml ([build] block) if it
    -- is zinc-native, else from its .cabal via the Opt-2 reader.
    loadDepComponents flags dest = do
      hasZinc <- doesFileExist (dest </> "zinc.toml")
      if hasZinc
        then do
          src <- readFile (dest </> "zinc.toml")
          pure $ case parseMember src of
            Left err  -> Left err
            Right mem -> Right (pkgVersion mem, pkgComponents mem)
        else do
          entries <- listDirectory dest
          case filter ((== ".cabal") . takeExtension) entries of
            (cabal : _) -> do
              src <- readFile (dest </> cabal)
              pure $ case cabalBuildType src of
                Right "Custom" -> Left "build-type: Custom (Setup.hs) is not supported yet"
                -- Finalize against the build TARGET's platform so arch(wasm32)
                -- conditionals resolve for a wasm build (zinc-xum) — a host-only
                -- finalize would pick a dep's vanilla (non-wasm) variant.
                _ -> case parseCabalComponentsForPlatform (if isWasm target then Platform Wasm32 Wasi else buildPlatform) flags ghcVersion src of
                  Left err -> Left err
                  Right cs -> Right (either (const "0") id (cabalVersion src), cs)
            [] -> pure (Left "no zinc.toml or .cabal")

-- | @zinc cache push@ (vwn.5): upload each locally-built closure artifact to the
-- configured remote cache (@ZINC_CACHE@), keyed by its 'buildCacheKey'. An
-- explicit CI publish step — only artifacts present in the local store are
-- pushed (an unbuilt one is skipped); an upload error fails. The build key is
-- computed exactly as the build does, so a pushed artifact is pull-hit later.
-- Returns the pushed dependency names.
runCachePush :: FilePath -> IO (Either ZincError [String])
runCachePush wsDir = runResult $ do
  cfg <- liftIO (resolveCacheConfig wsDir)
  be <- maybe (failWith "no remote cache write-url configured (set [cache].write-url, ZINC_CACHE_URL, or ZINC_CACHE)") (pure . httpBackend) (ccWriteUrl cfg)
  let lockFile = wsDir </> "zinc.lock"
  haveLock <- liftIO (doesFileExist lockFile)
  locks <- if haveLock then liftEither . parseLock =<< liftIO (readFile lockFile) else pure []
  let wsFile = wsDir </> "zinc.toml"
  wsSrc <- liftIO (readFile wsFile)
  ws <- liftEitherE (first (ManifestParse wsFile) (parseWorkspace wsSrc))
  storeRoot <- liftIO resolveStoreRoot
  let ghc = wsGhc ws
      opts = depGhcOptionsOf ws
      optsFor l = fromMaybe [] (lookup (lockName l) opts)
      flags = depFlagsOf ws
      flagsFor l = fromMaybe [] (lookup (lockName l) flags)
  fmap catMaybes $ forM locks $ \l -> do
    let key = buildCacheKey (BuildKey (lockName l) (lockRev l) ghc (lockDepends l) (optsFor l) (flagsFor l))
        pkgDir = storePkgPath storeRoot key
    there <- liftIO (doesDirectoryExist pkgDir)
    if not there
      then pure Nothing
      else do
        res <- liftIO (cbPush be key storeRoot)
        either (\e -> failWith ("cache push " ++ lockName l ++ ": " ++ e)) (const (pure (Just (lockName l)))) res

-- | @zinc package \<format\>@ foundation (zinc-7m6.1): build the app, stage a
-- packaging flake (its @packages.default@ wraps the built binary into a Nix
-- store derivation), and emit the artifact. The @nix@ format is available now
-- (the foundational closure); @docker@/@static@/@bundle@ extend the flake in
-- 7m6.2/.3/.4. Nix is auto-provisioned (y03); a clear diagnostic when absent.
runPackage :: PackageFormat -> Maybe String -> Maybe String -> Maybe String -> FilePath -> IO (Either ZincError String)
runPackage fmt tag out to wsDir = runResult $ do
  haveNix <- liftIO (findExecutable "nix")
  when (isNothing haveNix) (failWithError NixAbsent)
  exe <- orFailE (resolveRunTarget wsDir Nothing) -- builds the app + resolves its executable
  let name = takeFileName exe
      pkgDir = wsDir </> ".zinc" </> "package"
      (imageName, imageTag) = dockerImageRef name tag
  liftIO $ do
    stale <- doesDirectoryExist pkgDir
    when stale (removeDirectoryRecursive pkgDir)
    createDirectoryIfMissing True pkgDir
    copyFile exe (pkgDir </> name)
    -- Scan the prebuilt binary for the store paths it needs at runtime and pin
    -- them in the flake; otherwise Nix's reference scanner (limited to the
    -- input closure) omits them and the artifact can't find libgmp etc.
    bin <- BS.readFile (pkgDir </> name)
    deps <- filterM doesPathExist (storePathRefs (BS8.unpack bin))
    writeFile (pkgDir </> "flake.nix") (packagingFlake name imageName imageTag deps)
    -- flakes only see tracked files; stage the binary + flake into a throwaway repo.
    _ <- readProcessWithExitCode "git" ["-C", pkgDir, "init", "-q"] ""
    _ <- readProcessWithExitCode "git" ["-C", pkgDir, "add", "."] ""
    pure ()
  case fmt of
    NixClosure -> do
      path <- orFail (nixBuildAttr pkgDir "default")
      liftIO $ forM_ out $ \o -> readProcessWithExitCode "cp" ["-rfL", path, o] "" >> pure ()
      -- nix copy the closure to a remote store (zinc-7m6.5); niche but free given
      -- the local closure is already realized — the target must run Nix.
      copied <- case to of
        Nothing -> pure ""
        Just dest -> orFail (nixCopyTo path dest) >> pure (" → " ++ dest ++ " (nix copy)")
      pure ("Packaged " ++ name ++ " as a Nix closure: " ++ path ++ maybe "" (\o -> " (copied to " ++ o ++ ")") out ++ copied)
    Docker -> do
      tarball <- orFail (nixBuildAttr pkgDir "dockerImage")
      let ref = imageName ++ ":" ++ imageTag
      case out of
        Just o -> do
          liftIO (readProcessWithExitCode "cp" ["-fL", tarball, o] "" >> pure ())
          pure ("Built OCI image " ++ ref ++ " -> " ++ o)
        Nothing -> do
          haveDocker <- liftIO (findExecutable "docker")
          case haveDocker of
            Just _ -> do
              loaded <- liftIO (readProcessWithExitCode "docker" ["load", "-i", tarball] "")
              let (lc, _, _) = loaded
              pure $ case lc of
                ExitSuccess   -> "Built + loaded OCI image " ++ ref
                ExitFailure _ -> "Built OCI image " ++ ref ++ " -> " ++ tarball ++ " (run `docker load -i " ++ tarball ++ "`)"
            Nothing -> pure ("Built OCI image " ++ ref ++ " -> " ++ tarball ++ " (run `docker load -i " ++ tarball ++ "`)")
    Bundle -> do
      -- `nix bundle` wraps the app + its closure into one self-extracting
      -- executable (zinc-7m6.4). -o makes a symlink into the store; resolve it
      -- to the real file so -o/the reported path is a copyable artifact.
      let link = pkgDir </> "bundle-result"
      real <- orFail (nixBundle pkgDir link)
      case out of
        Just o -> do
          liftIO (readProcessWithExitCode "cp" ["-fL", real, o] "" >> pure ())
          pure ("Bundled " ++ name ++ " -> " ++ o ++ " (portable self-extracting executable)")
        Nothing -> pure ("Bundled " ++ name ++ " -> " ++ real ++ " (portable self-extracting executable)")
    -- zinc builds against the dynamic GHC; fully-static (musl) re-linking of GHC
    -- binaries is not supported. Surface a clear diagnostic with the practical
    -- alternatives (docker/bundle) rather than a cryptic linker error (zinc-7m6.3).
    Static -> failWithError (StaticUnsupported name)
  where
    -- --impure: the flake pins the binary's runtime deps via builtins.storePath
    -- (see Zinc.Package.packagingFlake), which pure flake eval forbids.
    nixBuildAttr dir attr = do
      (code, o, e) <-
        readProcessWithExitCode
          "nix"
          ["--extra-experimental-features", "nix-command flakes", "build", "--impure", dir ++ "#" ++ attr, "--no-link", "--print-out-paths"]
          ""
      pure $ case code of
        ExitSuccess   -> Right (reverse (dropWhile (`elem` ("\n\r \t" :: String)) (reverse o)))
        ExitFailure _ -> Left (if null e then "nix build failed" else e)
    -- `nix bundle` must target the package (which carries pname), not apps.default
    -- (whose drvToBundle has no pname); resolve the current system for the attr.
    nixBundle dir link = do
      sys <- currentSystem
      let target = dir ++ "#packages." ++ sys ++ ".default"
      (code, _, e) <-
        readProcessWithExitCode
          "nix"
          ["--extra-experimental-features", "nix-command flakes", "bundle", "--impure", target, "-o", link]
          ""
      case code of
        ExitFailure _ -> pure (Left (if null e then "nix bundle failed" else e))
        ExitSuccess   -> Right <$> canonicalizePath link
    currentSystem = do
      (c, o, _) <- readProcessWithExitCode "nix" ["--extra-experimental-features", "nix-command flakes", "eval", "--impure", "--raw", "--expr", "builtins.currentSystem"] ""
      pure $ case c of
        ExitSuccess | not (null (trim o)) -> trim o
        _                                 -> "x86_64-linux"
    trim = reverse . dropWhile (`elem` ("\n\r \t" :: String)) . reverse
    nixCopyTo path dest = do
      (code, _, e) <-
        readProcessWithExitCode
          "nix"
          ["--extra-experimental-features", "nix-command flakes", "copy", "--to", dest, path]
          ""
      pure $ case code of
        ExitSuccess   -> Right ()
        ExitFailure _ -> Left (if null e then "nix copy failed" else e)

-- | Direct dependencies declared in the manifest but absent from the lockfile
-- — i.e. names that need (re-)resolving via @zinc add@. Empty means the lock
-- covers every direct dependency.
lockDrift :: WorkspaceManifest -> [LockedPackage] -> [String]
lockDrift ws locks =
  [depName d | d <- wsDependencies ws, depName d `notElem` map lockName locks]

-- | Read a workspace's manifest + lock and report drifted direct deps (names
-- in zinc.toml not covered by zinc.lock). Empty if files are missing/unparsable.
checkLockDrift :: FilePath -> IO [String]
checkLockDrift wsDir = do
  hasWs <- doesFileExist (wsDir </> "zinc.toml")
  hasLock <- doesFileExist (wsDir </> "zinc.lock")
  if not (hasWs && hasLock)
    then pure []
    else do
      ws <- parseWorkspace <$> readFile (wsDir </> "zinc.toml")
      lk <- parseLock <$> readFile (wsDir </> "zinc.lock")
      pure $ case (ws, lk) of
        (Right w, Right ls) -> lockDrift w ls
        _                   -> []

-- | @zinc repl@: build the workspace (so deps are registered), then launch an
-- interactive ghci loading the first member's first component.
runRepl :: FilePath -> IO (Either ZincError ())
runRepl wsDir = runResult $ do
  _ <- orFailE (runBuild wsDir)
  wsSrc <- liftIO $ readFile (wsDir </> "zinc.toml")
  ws <- liftEitherE (first (ManifestParse (wsDir </> "zinc.toml")) (parseWorkspace wsSrc))
  case wsMembers ws of
    [] -> failWith "no members to load"
    (member : _) -> do
      let dir = wsDir </> member
      msrc <- liftIO $ readFile (dir </> "zinc.toml")
      mem <- liftEitherE (first (ManifestParse (dir </> "zinc.toml")) (parseMember msrc))
      case pkgComponents mem of
        [] -> failWith (member ++ ": no components to load")
        (comp : _) ->
          liftIO $ callProcess "ghci" (replArgs (Just (wsDir </> ".zinc" </> "pkgdb")) dir comp)

-- | @zinc clean@: remove build artifacts (members' .zinc dirs + the workspace
-- package db) while keeping the content-addressed store (spec §10).
runClean :: FilePath -> IO ()
runClean wsDir = do
  hasWs <- doesFileExist (wsDir </> "zinc.toml")
  members <-
    if hasWs
      then either (const []) wsMembers . parseWorkspace <$> readFile (wsDir </> "zinc.toml")
      else pure []
  -- Remove build artifacts only (each member's compiled output + the workspace
  -- pkgdb), preserving the content-addressed store and .zinc/metrics.jsonl —
  -- the latter must survive clean (perf spec §3.1). Targeting build/lib (not the
  -- whole .zinc) keeps metrics even for a member="." workspace (zinc itself).
  mapM_ (\m -> rm (wsDir </> m </> ".zinc" </> "build") >> rm (wsDir </> m </> ".zinc" </> "lib")) members
  rm (wsDir </> ".zinc" </> "pkgdb")
  where
    rm p = do
      there <- doesDirectoryExist p
      when there (removeDirectoryRecursive p)
