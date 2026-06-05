-- | The GHC build driver (spec §7): zinc drives @ghc --make@ directly rather
-- than executing Cabal's builder. This module constructs the invocation.
module Zinc.Build
  ( GhcInvocation (..)
  , ghcMakeArgs
  , PackageConf (..)
  , renderConf
  , archiveArgs
  , registerPackage
  , isRegistered
  , preprocessorFor
  , runPreprocessor
  , MemberBuild (..)
  , buildMember
  , replArgs
  , LibBuild (..)
  , buildLib
  , buildLibArtifacts
  , writeFileIfChanged
  , discoverModules
  , initPackageDb
  , installedVersions
  ) where

import Data.Maybe (fromMaybe, isJust)
import Control.Monad (unless, when)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, nub)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, makeRelative, takeDirectory, takeExtension, (-<.>), (<.>), (</>))
import System.IO (readFile')
import System.Process (readProcessWithExitCode)
import Zinc.Diagnostic (ZincError (GhcCompile))
import Zinc.Except (liftIO, orFail, orFailE, runResult)
import Zinc.Macros (emitCabalMacros)
import Zinc.Manifest (Component (..))
import Zinc.Paths (pathsModuleName, synthesizePaths)
import Zinc.Resolve (isBootLib)

-- | Everything needed to compile one component with @ghc --make@.
data GhcInvocation = GhcInvocation
  { giUnitId     :: String     -- ^ @-this-unit-id@ (e.g. @myapp-0.1.0@)
  , giPackageDb  :: FilePath   -- ^ accumulated store package db
  , giDeps       :: [String]   -- ^ dependency packages to expose (@-package@)
  , giSourceDirs :: [FilePath] -- ^ @-i@ search roots
  , giModules    :: [String]   -- ^ modules to compile
  , giExtensions :: [String]   -- ^ language extensions (without the @-X@)
  , giGhcOptions :: [String]   -- ^ extra ghc options, verbatim
  , giOutputDir  :: FilePath   -- ^ @-outputdir@ for .hi/.o
  }
  deriving (Eq, Show)

-- | Build the @ghc --make@ argument list. @-hide-all-packages@ + explicit
-- @-package@ flags give exact control over what's visible — no ambient leakage.
ghcMakeArgs :: GhcInvocation -> [String]
ghcMakeArgs gi =
  ["--make", "-hide-all-packages", "-package-db", giPackageDb gi]
    ++ concatMap (\d -> ["-package", d]) (giDeps gi)
    ++ map ("-i" ++) (giSourceDirs gi)
    ++ ["-this-unit-id", giUnitId gi]
    ++ ["-outputdir", giOutputDir gi]
    ++ ["-O"]
    ++ map ("-X" ++) (giExtensions gi)
    ++ giGhcOptions gi
    ++ giModules gi

-- | A synthesized installed-package description (@.conf@) — the metadata
-- @ghc-pkg register@ records so later compiles can @-package@ this build.
data PackageConf = PackageConf
  { confName           :: String
  , confVersion        :: String
  , confId             :: String     -- ^ unit-id
  , confExposedModules :: [String]
  , confImportDirs     :: [FilePath]
  , confLibraryDirs    :: [FilePath]
  , confHsLibraries    :: [String]
  , confDepends        :: [String]   -- ^ dependency unit-ids
  }
  deriving (Eq, Show)

-- | Render a 'PackageConf' to ghc-pkg's @.conf@ format.
renderConf :: PackageConf -> String
renderConf c =
  unlines
    [ "name: " ++ confName c
    , "version: " ++ confVersion c
    , "id: " ++ confId c
    , "key: " ++ confId c
    , "exposed: True"
    , "exposed-modules: " ++ unwords (confExposedModules c)
    , "import-dirs: " ++ unwords (confImportDirs c)
    , "library-dirs: " ++ unwords (confLibraryDirs c)
    , "hs-libraries: " ++ unwords (confHsLibraries c)
    , "depends: " ++ unwords (confDepends c)
    ]

-- | @ar@ arguments to assemble a static library archive from object files.
archiveArgs :: FilePath -> String -> [FilePath] -> [String]
archiveArgs libDir unitId objs =
  ["rcs", libDir </> ("libHS" ++ unitId ++ ".a")] ++ objs

-- | Register a rendered @.conf@ into the given package db (creating it if
-- needed). Uses @--force@ so a freshly-built package registers even before
-- ghc-pkg can re-validate every path.
registerPackage :: FilePath -> String -> IO (Either String ())
registerPackage db confText = do
  initResult <- initPackageDb db
  case initResult of
    Left err -> pure (Left err)
    Right () -> do
      let confFile = takeDirectory db </> "register.conf"
          pkgId = maybe "" (drop 4) (find ("id: " `isPrefixOf`) (lines confText))
      writeFile confFile confText
      -- Unregister any prior copy first so re-registration over a persisted db
      -- (inner-loop incrementality) is a clean overwrite: ghc-pkg register
      -- --force is unreliable at replacing a package that has dependencies.
      -- Ignore failure (the package may not be registered yet).
      _ <- readProcessWithExitCode "ghc-pkg" ["--package-db", db, "unregister", "--force", pkgId] ""
      runUnit "ghc-pkg" ["--package-db", db, "register", "--force", confFile]

-- | Create an empty package db (no-op if it already exists).
initPackageDb :: FilePath -> IO (Either String ())
initPackageDb db = do
  createDirectoryIfMissing True (takeDirectory db)
  exists <- doesDirectoryExist db
  if exists then pure (Right ()) else runUnit "ghc-pkg" ["init", db]

runUnit :: String -> [String] -> IO (Either String ())
runUnit cmd args = do
  (code, _out, err) <- readProcessWithExitCode cmd args ""
  pure $ case code of
    ExitSuccess   -> Right ()
    ExitFailure _ -> Left (cmd ++ ": " ++ err)

-- | Run @ghc@ for one package, surfacing a failure as a structured
-- 'GhcCompile' carrying GHC's RAW stderr (no @ghc:@ prefix) so the diagnostic
-- renderer can parse a file:line:col location and draw a caret (hw6.5).
runGhc :: String -> [String] -> IO (Either ZincError ())
runGhc pkg args = do
  (code, _out, err) <- readProcessWithExitCode "ghc" args ""
  pure $ case code of
    ExitSuccess   -> Right ()
    ExitFailure _ -> Left (GhcCompile pkg err)

-- | Compile a component's cabal @c-sources@ into the dist dir (each object
-- mirroring its source path, e.g. @\<distDir\>\/cbits\/foo.o@), with the
-- package's @include-dirs@ on the C search path. A separate @ghc -c@ step, so
-- the objects land in the dist dir and get archived into the library — see the
-- call site for why @ghc --make@ cannot place them there (zinc-i98).
compileCSources :: LibBuild -> Component -> IO (Either ZincError ())
compileCSources lb comp = runResult (mapM_ one (compCSources comp))
  where
    incs = ["-I" ++ (lbMemberDir lb </> d) | d <- compIncludeDirs comp]
    one c = do
      let src = lbMemberDir lb </> c
          obj = lbDistDir lb </> (c -<.> "o")
      liftIO (createDirectoryIfMissing True (takeDirectory obj))
      orFailE (runGhc (lbName lb) (["-c", src, "-o", obj] ++ incs))

-- | The preprocessor command for a source @file@ writing its generated @.hs@ to
-- @out@ (alex/happy/hsc2hs), or 'Nothing' for a plain @.hs@.
ppCommand :: FilePath -> FilePath -> Maybe (String, [String])
ppCommand file out = case takeExtension file of
  ".x"   -> Just ("alex", [file, "-o", out])
  ".y"   -> Just ("happy", [file, "-o", out])
  ".hsc" -> Just ("hsc2hs", [file, "-o", out])
  _      -> Nothing

-- | The preprocessor command for a source file, or 'Nothing' for plain .hs.
-- Each turns @file.<ext>@ into the sibling @file.hs@.
preprocessorFor :: FilePath -> Maybe (String, [String])
preprocessorFor file = ppCommand file (file -<.> "hs")

-- | Run the preprocessor for a source file (no-op for plain .hs). The tools
-- (alex/happy/hsc2hs) come from the Nix-provided toolchain.
runPreprocessor :: FilePath -> IO (Either String ())
runPreprocessor file = case preprocessorFor file of
  Nothing            -> pure (Right ())
  Just (prog, args)  -> runUnit prog args

-- | Inputs to build one workspace member (spec §7): the member's source dir,
-- a build output dir (kept stable so ghc's recompilation avoidance gives a
-- fast inner loop), an optional project package-db for deps/siblings, and the
-- component to build.
data MemberBuild = MemberBuild
  { mbMemberDir :: FilePath
  , mbBuildDir  :: FilePath
  , mbPackageDb :: Maybe FilePath
  , mbComponent :: Component
  }

-- | @-package@ flags for a component's dependencies (plus the always-present
-- base). zinc-built deps are pinned by their exact unit-id via @-package-id@
-- (their unit-id is the bare package name), so a same-named package in GHC's
-- global db — e.g. a Nix-provided @ansi-terminal-types@ — cannot shadow the
-- version zinc actually built. Boot libs resolve from the global db by name.
packageFlags :: [String] -> [String]
packageFlags deps = concatMap flag (nub ("base" : deps))
  where
    flag d
      | isBootLib d = ["-package", d]
      | otherwise = ["-package-id", d]

-- | Compile + link a member executable with @ghc --make@, returning the
-- executable path. Isolation via @-hide-all-packages@ + explicit @-package@
-- (base is always available).
buildMember :: MemberBuild -> IO (Either ZincError FilePath)
buildMember mb = do
  createDirectoryIfMissing True (mbBuildDir mb)
  let comp = mbComponent mb
      srcDirs = if null (compSourceDirs comp) then ["."] else compSourceDirs comp
      exe = mbBuildDir mb </> compName comp
      mainFile = mbMemberDir mb </> head srcDirs </> maybe "Main.hs" id (compMain comp)
      args =
        ["--make", "-j"]
          ++ maybe [] (\db -> ["-package-db", db]) (mbPackageDb mb)
          ++ ["-hide-all-packages"]
          ++ packageFlags (compDepends comp)
          ++ map (\d -> "-i" ++ (mbMemberDir mb </> d)) srcDirs
          ++ map ("-X" ++) (compExtensions comp)
          ++ compGhcOptions comp
          ++ ["-outputdir", mbBuildDir mb, mainFile, "-o", exe]
  result <- runGhc (compName comp) args
  pure (fmap (const exe) result)

-- | Inputs to build a member's library so siblings can link against it.
data LibBuild = LibBuild
  { lbMemberDir :: FilePath
  , lbDistDir   :: FilePath  -- ^ holds .hi/.o and the archive; also the lib dir
  , lbPackageDb :: FilePath  -- ^ workspace db to register into / resolve sibling deps
  , lbName      :: String
  , lbVersion   :: String
  , lbComponent :: Component  -- ^ the library component
  }

-- | Compile a library component, archive it, and register it into the
-- workspace package db so sibling members can @-package@ it.
buildLib :: LibBuild -> IO (Either ZincError ())
buildLib lb = runResult $ do
  (conf, confChanged) <- orFailE (buildLibArtifacts lb)
  -- Skip re-registration on a persisted db when the conf is unchanged and the
  -- lib is already registered at this dir (inner-loop incrementality).
  reg <- liftIO (isRegistered (lbPackageDb lb) (lbName lb) (lbDistDir lb))
  when (confChanged || not reg) (orFail (registerPackage (lbPackageDb lb) conf))

-- | Compile and archive a library and persist its @package.conf@ into the
-- store, returning the conf text — but /without/ registering it into the
-- workspace db. Registration is split out so independent closure libraries can
-- be compiled concurrently and then registered serially (ghc-pkg register on a
-- shared db is not concurrency-safe).
buildLibArtifacts :: LibBuild -> IO (Either ZincError (String, Bool))
buildLibArtifacts lb = runResult $ do
  liftIO $ createDirectoryIfMissing True (lbDistDir lb)
  -- Synthesize the Cabal-autogen files (Paths_<pkg>, cabal_macros.h) into a
  -- generated-source dir so the package's own modules can import/use them.
  let gen = lbDistDir lb </> "zinc-gen"
      -- Generated .hs from preprocessors (alex/happy/hsc2hs) go here, off the
      -- content-addressed src tree (zinc-c3g), and onto ghc's -i path below.
      ppGen = lbDistDir lb </> "zinc-pp"
  liftIO $ createDirectoryIfMissing True gen
  liftIO $ createDirectoryIfMissing True ppGen
  let comp = lbComponent lb
      -- One ref per package name (spec §2), so the unit-id is just the name;
      -- this also lets dependents reference it by name in their conf depends.
      unitId = lbName lb
      pathsMod = pathsModuleName (lbName lb)
      macrosHeader = gen </> "cabal_macros.h"
  _ <- liftIO $ writeFileIfChanged (gen </> pathsMod <.> "hs") (synthesizePaths (lbName lb) (versionInts (lbVersion lb)))
  installed <- liftIO installedVersions
  let depVersion d = fromMaybe [0] (lookup d installed)
      -- A direct dep's id for the conf's @depends@ (drives a dependent's
      -- linking): zinc-built deps by bare name (their unit-id); non-base boot
      -- libs by their real installed id (e.g. @array-0.5.6.0@) so the linker
      -- pulls them in. base is omitted — it is always linked via -package base.
      depConfId d
        | isBootLib d = d ++ "-" ++ intercalate "." (map show (depVersion d))
        | otherwise = d
  -- Emit cabal_macros.h for the package's OWN version only. GHC 8.0+
  -- auto-generates VERSION_<dep>/MIN_VERSION_<dep> for every -package dep, so
  -- emitting our own (from the GLOBAL ghc-pkg, which can lag a closure-built
  -- dep — e.g. primitive 0.8.0.0 vs the built 0.9.1.0) only causes a CPP
  -- redefinition conflict with GHC's correct value. Self macros stay (GHC does
  -- not define them for the home unit). (zinc-ffm.4)
  _ <- liftIO $ writeFileIfChanged macrosHeader $
    emitCabalMacros [(lbName lb, versionInts (lbVersion lb))]
  let srcDirs = if null (compSourceDirs comp) then ["."] else compSourceDirs comp
  -- A library's modules: the explicit list, or auto-discovered by walking its
  -- source dirs (spec §4 "no module hiding" — zinc-native packages list none;
  -- cabal deps carry their .cabal module list). Every discovered module is
  -- exposed.
  discovered <-
    if null (compModules comp)
      then liftIO (discoverModules [lbMemberDir lb </> d | d <- srcDirs])
      else pure (compModules comp)
  -- nub so a package that already lists Paths_<pkg> doesn't collide with the
  -- Paths_ module zinc synthesizes.
  let modules = nub (discovered ++ [pathsMod])
      compileArgs =
        ["--make", "-j", "-hide-all-packages", "-package-db", lbPackageDb lb]
          ++ packageFlags (compDepends comp)
          ++ map (\d -> "-i" ++ (lbMemberDir lb </> d)) srcDirs
          ++ ["-i" ++ gen, "-i" ++ ppGen, "-optP-include", "-optP" ++ macrosHeader]
          -- C-header search dirs (cabal include-dirs) so CPP #include of the
          -- package's own headers (e.g. version-compatibility-macros.h) resolves.
          ++ concatMap (\d -> let p = lbMemberDir lb </> d in ["-I" ++ p, "-optP-I" ++ p]) (compIncludeDirs comp)
          ++ ["-this-unit-id", unitId, "-outputdir", lbDistDir lb]
          ++ map ("-X" ++) (compExtensions comp)
          -- CPP -D defines (cabal cpp-options) for conditionally-compiled source.
          ++ map ("-optP" ++) (compCppOptions comp)
          ++ compGhcOptions comp
          ++ modules
  -- Generate sources from any .x/.y/.hsc the dep ships (e.g. toml-parser's
  -- alex/happy lexer+parser) so ghc --make finds the resulting .hs modules,
  -- then compile and archive. orFail short-circuits on the first failure.
  orFail (runPreprocessorsTo ppGen (map (lbMemberDir lb </>) srcDirs))
  orFailE (runGhc (lbName lb) compileArgs)
  -- C sources (cabal c-sources, e.g. primitive's cbits/primitive-memops.c) are
  -- compiled in a SEPARATE `ghc -c` step into the dist dir, NOT via `ghc --make`:
  -- --make writes a C object next to its (absolute) source — outside -outputdir
  -- and into the content-addressed src tree — so findObjs would never archive it
  -- and a dependent linking the library hits "undefined reference" (zinc-i98:
  -- hsprimitive_memset_*, splitmix_init). Built with the package's include-dirs.
  orFailE (compileCSources lb comp)
  objs <- liftIO (findObjs (lbDistDir lb))
  -- Re-archive only when an object is newer than the archive: ghc --make keeps
  -- objects incremental, so an unchanged lib's .a (and the exe linking it)
  -- need not be rebuilt.
  let aPath = lbDistDir lb </> ("libHS" ++ unitId ++ ".a")
  stale <- liftIO (archiveStale aPath objs)
  when stale $ orFail (runUnit "ar" (archiveArgs (lbDistDir lb) unitId objs))
  let confText =
        renderConf
          PackageConf
            { confName = lbName lb
            , confVersion = lbVersion lb
            , confId = unitId
            , confExposedModules = discovered
            , confImportDirs = [lbDistDir lb]
            , confLibraryDirs = [lbDistDir lb]
            , confHsLibraries = ["HS" ++ unitId]
            , -- Direct deps as installed unit-ids so dependents link them:
              -- zinc deps by bare name, non-base boot libs by real id.
              confDepends = [depConfId d | d <- nub (compDepends comp), d /= "base"]
            }
  -- Persist the conf alongside the build (the artifact cache re-registers it
  -- without recompiling); report whether it changed so a sibling lib can skip
  -- re-registration on the persisted db.
  confChanged <- liftIO (writeFileIfChanged (lbDistDir lb </> "package.conf") confText)
  pure (confText, confChanged)

-- | Discover a library's modules by walking its source dirs: every
-- @.hs\/.lhs\/.hsc\/.x\/.y@ file becomes a dotted module name (its path under the
-- source dir, @\/@ -> @.@, extension dropped), excluding @Main@. This backs the
-- "no module hiding" model (spec §4): a zinc-native package lists no modules and
-- every one it ships is compiled and exposed.
discoverModules :: [FilePath] -> IO [String]
discoverModules dirs = nub . concat <$> mapM fromDir dirs
  where
    fromDir dir = do
      exists <- doesDirectoryExist dir
      if not exists
        then pure []
        else do
          files <- listFilesRec dir
          pure
            [ m
            | f <- files
            , takeExtension f `elem` [".hs", ".lhs", ".hsc", ".x", ".y"]
            , let m = toModule (makeRelative dir f)
            , m /= "Main"
            ]
    toModule = map (\c -> if c == '/' then '.' else c) . dropExtension

-- | Recursively list the files under a directory (relative paths joined to it).
listFilesRec :: FilePath -> IO [FilePath]
listFilesRec dir = do
  entries <- listDirectory dir
  fmap concat $ mapM (\e -> let p = dir </> e in doesDirectoryExist p >>= \isDir -> if isDir then listFilesRec p else pure [p]) entries

-- | Write @content@ to @path@ only if it differs from the current contents,
-- preserving the mtime when unchanged so ghc --make does not needlessly
-- recompile modules that depend on a regenerated autogen file (Paths_/macros).
-- Returns whether it actually wrote.
writeFileIfChanged :: FilePath -> String -> IO Bool
writeFileIfChanged path content = do
  exists <- doesFileExist path
  -- NB: readFile' (strict) closes the handle before the writeFile below;
  -- lazy readFile leaves the handle open when (==) short-circuits on changed
  -- content, causing "resource busy (file is locked)" on the write.
  same <- if exists then (== content) <$> readFile' path else pure False
  unless same (writeFile path content)
  pure (not same)

-- | Is @unitId@ registered in @db@ with @pkgDir@ among its library-dirs? Used
-- to decide whether a (sibling) lib still needs (re-)registering on a persisted
-- db. A changed rev yields a different pkg dir, so this re-registers correctly.
isRegistered :: FilePath -> String -> FilePath -> IO Bool
isRegistered db unitId pkgDir = do
  (code, out, _) <- readProcessWithExitCode "ghc-pkg" ["--package-db", db, "field", unitId, "library-dirs"] ""
  pure (code == ExitSuccess && pkgDir `isInfixOf` out)

-- | Does the archive need rebuilding — i.e. it is missing or some object is
-- newer than it?
archiveStale :: FilePath -> [FilePath] -> IO Bool
archiveStale aPath objs = do
  exists <- doesFileExist aPath
  if not exists
    then pure True
    else do
      aTime <- getModificationTime aPath
      oTimes <- mapM getModificationTime objs
      pure (any (> aTime) oTimes)

-- | Recursively list object files under a directory.
findObjs :: FilePath -> IO [FilePath]
findObjs root = do
  exists <- doesDirectoryExist root
  if not exists then pure [] else go root
  where
    go dir = do
      entries <- listDirectory dir
      fmap concat $ mapM (classify dir) entries
    classify dir e = do
      let p = dir </> e
      isDir <- doesDirectoryExist p
      if isDir then go p else pure [p | takeExtension p == ".o"]

-- | Recursively list files under a directory that need a preprocessor
-- (.x/.y/.hsc), as absolute paths.
preprocessableUnder :: FilePath -> IO [FilePath]
preprocessableUnder root = do
  exists <- doesDirectoryExist root
  if not exists then pure [] else go root
  where
    go dir = do
      entries <- listDirectory dir
      fmap concat $ mapM (classify dir) entries
    classify dir e = do
      let p = dir </> e
      isDir <- doesDirectoryExist p
      if isDir then go p else pure [p | isJust (preprocessorFor p)]

-- | Run alex/happy/hsc2hs over every preprocessable source under the given
-- dirs, writing each generated @.hs@ into @genRoot@
-- (mirroring the file's path under its source dir) instead of beside the
-- source. The fetched source tree is content-addressed; generating into it
-- would change its hash and fail the lock's sha256 check on the next build
-- (zinc-c3g). @genRoot@ must be on ghc's @-i@ search path so the generated
-- modules are found. First failure wins.
runPreprocessorsTo :: FilePath -> [FilePath] -> IO (Either String ())
runPreprocessorsTo genRoot dirs = do
  pairs <- concat <$> mapM (\d -> map ((,) d) <$> preprocessableUnder d) dirs
  go pairs
  where
    go [] = pure (Right ())
    go ((dir, f) : rest) =
      let out = genRoot </> (makeRelative dir f -<.> "hs")
       in case ppCommand f out of
            Nothing -> go rest
            Just (prog, args) -> do
              createDirectoryIfMissing True (takeDirectory out)
              runUnit prog args >>= either (pure . Left) (const (go rest))

-- | ghci argument list to load a component for @zinc repl@: the package db,
-- isolation flags + exposed deps, source roots, and the targets to load (the
-- main file for an executable, or the exposed modules for a library).
replArgs :: Maybe FilePath -> FilePath -> Component -> [String]
replArgs packageDb memberDir comp =
  maybe [] (\db -> ["-package-db", db]) packageDb
    ++ ["-hide-all-packages"]
    ++ packageFlags (compDepends comp)
    ++ map (\d -> "-i" ++ (memberDir </> d)) srcDirs
    ++ map ("-X" ++) (compExtensions comp)
    ++ compGhcOptions comp
    ++ targets
  where
    srcDirs = if null (compSourceDirs comp) then ["."] else compSourceDirs comp
    targets = case compMain comp of
      Just m  -> [memberDir </> head srcDirs </> m]
      Nothing -> compModules comp

-- | Parse a dotted version string into integer components (non-numeric -> 0).
versionInts :: String -> [Int]
versionInts = map readInt . splitDots
  where
    splitDots s = case break (== '.') s of
      (a, [])    -> [a]
      (a, _ : r) -> a : splitDots r
    readInt x = case reads x of
      [(n, "")] -> n
      _         -> 0

-- | Versions of packages currently visible to ghc-pkg (boot libs + already
-- registered deps), for emitting correct MIN_VERSION_* CPP macros.
installedVersions :: IO [(String, [Int])]
installedVersions = do
  (_, out, _) <- readProcessWithExitCode "ghc-pkg" ["list", "--simple-output"] ""
  pure [(name, versionInts ver) | pid <- words out, Just (name, ver) <- [splitNameVer pid]]
  where
    splitNameVer pid = case reverse (splitOnDash pid) of
      (v : rest@(_ : _)) | isVersion v -> Just (intercalate "-" (reverse rest), v)
      _                                -> Nothing
    isVersion v = not (null v) && all (`elem` ("0123456789." :: String)) v
    splitOnDash s = case break (== '-') s of
      (a, [])    -> [a]
      (a, _ : r) -> a : splitOnDash r
