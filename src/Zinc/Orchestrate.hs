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
  , runTests
  , orderMembers
  , lockDrift
  , checkLockDrift
  , runRepl
  , runClean
  , parMapBounded
  ) where

import Control.Concurrent (forkIO, getNumCapabilities)
import GHC.Clock (getMonotonicTime)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (SomeException, finally, try)
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Char (isHexDigit)
import Data.List (stripPrefix)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, (</>))
import System.Process (callProcess, readProcess, readProcessWithExitCode)
import Zinc.Build (LibBuild (..), MemberBuild (..), buildLib, buildLibArtifacts, buildMember, initPackageDb, isRegistered, registerPackage, replArgs)
import Zinc.Cabal (cabalBuildType, cabalVersion, parseCabalComponentsForGhc)
import Zinc.Cache (BuildKey (..), buildCacheKey, storeConfPath, storePkgPath)
import Zinc.Git (cloneAt, splitRepoSubdir)
import Zinc.Lock (LockedPackage (..), parseLock)
import Zinc.Manifest
  ( Component (compDepends, compGhcOptions, compKind)
  , ComponentKind (Executable, Library, TestSuite)
  , Dependency (depName)
  , MemberManifest (pkgComponents, pkgName, pkgVersion)
  , Ref (Latest)
  , WorkspaceManifest (wsDependencies, wsGhc, wsMembers)
  , parseMember
  , parseWorkspace
  , parseBuildOptions
  )
import Zinc.Diagnostic (ZincError (ContentHashMismatch, NoZincToml, OtherError))
import Zinc.Except (Result, failWith, failWithError, liftEither, liftEitherE, liftIO, orFail, orFailE, runResult)
import Zinc.Report (BuildOutcome (..), PackageReport (..), PackageStatus (..), Timing (..), cacheStatsOf)
import Zinc.Resolve (ResolvedDep (..), topoLevels)
import Zinc.Store (contentHash, resolveStoreRoot, storeSrcPath, withStoreLock)

-- | Build a workspace: each member's library (so siblings can link) plus every
-- component whose kind satisfies @keep@, returned as built executable paths.
-- @target@ (when 'Just') restricts which member's @keep@-components are built;
-- libraries are always built so dependencies remain available.
buildWorkspace :: FilePath -> Maybe String -> (ComponentKind -> Bool) -> IO (Either ZincError [FilePath])
buildWorkspace wsDir target keep = fmap (fmap (boExes . fst)) (buildWorkspaceReport wsDir target keep)

-- | As 'buildWorkspace', but also returns the per-package closure report (spec
-- §3.2) and per-phase wall-clock timings (perf spec §2) for the structured
-- @--json@ surface. 'buildWorkspace' is the thin exes-only projection.
buildWorkspaceReport :: FilePath -> Maybe String -> (ComponentKind -> Bool) -> IO (Either ZincError (BuildOutcome, [(String, Int)]))
buildWorkspaceReport wsDir target keep = runResult $ do
  let wsFile = wsDir </> "zinc.toml"
  present <- liftIO (doesFileExist wsFile)
  when (not present) $ failWithError (NoZincToml wsDir)
  wsSrc <- liftIO $ readFile wsFile
  ws <- liftEither (parseWorkspace wsSrc)
  members <- traverse loadMember (wsMembers ws)
  let wsDb = wsDir </> ".zinc" </> "pkgdb"
  -- Keep the package db across builds (inner-loop incrementality): registration
  -- is idempotent (ghc-pkg register --force) and the closure builder skips deps
  -- already registered at their current key-addressed pkg dir.
  orFail (initPackageDb wsDb)
  storeRoot <- liftIO resolveStoreRoot
  -- Coarse phases (perf spec §2): the dependency-closure build (fetch + compile
  -- + register of deps) and the workspace-member build (compile + link). Finer
  -- breakdown (resolve/provision/fetch/register/link split) is a follow-up.
  (pkgs, closureMs) <- timed (orFailE (buildClosure wsDir storeRoot wsDb (wsGhc ws) (parseBuildOptions wsSrc)))
  (exes, memberMs) <- timed (concat <$> traverse (buildMemberAll wsDb) (orderMembers members))
  pure (BuildOutcome exes pkgs, [("closure", closureMs), ("member", memberMs)])
  where
    loadMember member = do
      let dir = wsDir </> member
      src <- liftIO $ readFile (dir </> "zinc.toml")
      mem <- liftEither (first ((member ++ ": ") ++) (parseMember src))
      pure (dir, mem)

    -- Build a member's library (so siblings/exes can link it), then every
    -- @keep@-selected component, returning the executable paths.
    buildMemberAll wsDb (dir, mem) = do
      case filter ((== Library) . compKind) (pkgComponents mem) of
        []        -> pure ()
        (lib : _) -> do
          -- Absolute lib dir so the registered library-dirs is absolute: ghc-pkg
          -- rejects relative paths, and (with the db now persisted) a relative
          -- entry can't be cleanly re-registered across builds.
          libDir <- liftIO (makeAbsolute (dir </> ".zinc" </> "lib"))
          orFailE (buildLib (LibBuild dir libDir wsDb (pkgName mem) (pkgVersion mem) lib))
      traverse (\comp -> orFail (buildMember (MemberBuild dir (dir </> ".zinc" </> "build") (Just wsDb) comp))) (wanted mem)

    wanted mem
      | maybe True (== pkgName mem) target = filter (keep . compKind) (pkgComponents mem)
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
runWarm :: FilePath -> IO (Either ZincError [PackageReport])
runWarm wsDir = runResult $ do
  let wsFile = wsDir </> "zinc.toml"
  present <- liftIO (doesFileExist wsFile)
  when (not present) $ failWithError (NoZincToml wsDir)
  wsSrc <- liftIO (readFile wsFile)
  ws <- liftEither (parseWorkspace wsSrc)
  let wsDb = wsDir </> ".zinc" </> "pkgdb"
  orFail (initPackageDb wsDb)
  storeRoot <- liftIO resolveStoreRoot
  orFailE (buildClosure wsDir storeRoot wsDb (wsGhc ws) (parseBuildOptions wsSrc))

-- | @zinc build [member] --json@: build, returning the structured outcome
-- (executables + per-package closure report) and the 'Timing' block (total
-- wall-clock, per-phase durations, cache stats) for the machine surface.
runBuildReport :: FilePath -> Maybe String -> IO (Either ZincError (BuildOutcome, Timing))
runBuildReport wsDir target = do
  t0 <- getMonotonicTime
  r <- buildWorkspaceReport wsDir target (== Executable)
  t1 <- getMonotonicTime
  let totalMs = round ((t1 - t0) * 1000) :: Int
  pure $ fmap (\(o, phases) -> (o, Timing totalMs phases (cacheStatsOf (boPackages o)))) r

-- | Run a pipeline step, returning its wall-clock duration in milliseconds.
timed :: Result a -> Result (a, Int)
timed act = do
  t0 <- liftIO getMonotonicTime
  a <- act
  t1 <- liftIO getMonotonicTime
  pure (a, round ((t1 - t0) * 1000))

-- | @zinc run@: build, then run the first executable with the given args,
-- returning its stdout.
buildAndRun :: FilePath -> [String] -> IO (Either ZincError String)
buildAndRun wsDir args = runResult $ do
  built <- orFailE (runBuild wsDir)
  case built of
    []        -> failWith "no executable to run"
    (exe : _) -> liftIO (readProcess exe args "")

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
buildClosure :: FilePath -> FilePath -> FilePath -> String -> [(String, [String])] -> IO (Either ZincError [PackageReport])
buildClosure wsDir storeRoot wsDb ghcVersion buildOpts = runResult $ do
  let lockFile = wsDir </> "zinc.lock"
  present <- liftIO (doesFileExist lockFile)
  if not present
    then pure []
    else do
      locks <- liftEither . parseLock =<< liftIO (readFile lockFile)
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
      when (not done) (orFail (registerPackage wsDb conf))

    -- Content-addressed cache key from data available without the source, so a
    -- cached build is reused without even fetching. Includes the dep's
    -- [build-options] override so changing an override invalidates the cache.
    cacheKeyOf l = buildCacheKey (BuildKey (lockRev l) ghcVersion (lockDepends l) (overrideFor l))

    overrideFor l = fromMaybe [] (lookup (lockName l) buildOpts)

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
          if nowCached then reuseCached l key else runResult (buildNode l key)

    reuseCached l key = do
      conf <- readFile (storeConfPath storeRoot key)
      pure (Right (PackageReport (lockName l) (lockRev l) Cached, Just (lockName l, storePkgPath storeRoot key, conf)))

    buildNode l key = do
      let pkgOut = storePkgPath storeRoot key
          report st = PackageReport (lockName l) (lockRev l) st
          dest = storeSrcPath storeRoot (lockName l) (lockRev l)
      exists <- liftIO (doesDirectoryExist dest)
      when (not exists) $
        orFail (first (("fetch " ++ lockName l ++ ": ") ++) <$> cloneAt (lockRepo l) (lockRev l) dest) >> pure ()
      orFailE (verifyFetched l dest)
      -- The package may live in a subdirectory of the repo (monorepo);
      -- read its manifest/sources from there.
      let pkgDir = maybe dest (dest </>) (snd (splitRepoSubdir (lockRepo l)))
      comps <- liftIO (loadDepComponents pkgDir)
      (version, components) <- liftEither (first ((lockName l ++ ": ") ++) comps)
      case filter ((== Library) . compKind) components of
        []        -> pure (report Skipped, Nothing) -- no library to build
        (lib : _) -> do
          -- Apply any per-dependency build overrides (extra ghc flags,
          -- e.g. -XSafe) from the workspace [build-options].
          let lib' = lib {compGhcOptions = compGhcOptions lib ++ overrideFor l}
          (conf, _) <- orFailE (buildLibArtifacts (LibBuild pkgDir pkgOut wsDb (lockName l) version lib'))
          pure (report Built, Just (lockName l, pkgOut, conf))

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

    -- A dependency's components come from its zinc.toml ([build] block) if it
    -- is zinc-native, else from its .cabal via the Opt-2 reader.
    loadDepComponents dest = do
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
                _ -> case parseCabalComponentsForGhc ghcVersion src of
                  Left err -> Left err
                  Right cs -> Right (either (const "0") id (cabalVersion src), cs)
            [] -> pure (Left "no zinc.toml or .cabal")

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
  ws <- liftEither (parseWorkspace wsSrc)
  case wsMembers ws of
    [] -> failWith "no members to load"
    (member : _) -> do
      let dir = wsDir </> member
      msrc <- liftIO $ readFile (dir </> "zinc.toml")
      mem <- liftEither (parseMember msrc)
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
