-- | The GHC build driver (spec §7): zinc drives @ghc --make@ directly rather
-- than executing Cabal's builder. This module constructs the invocation.
module Zinc.Build
  ( GhcInvocation (..)
  , ghcMakeArgs
  , PackageConf (..)
  , renderConf
  , archiveArgs
  , registerPackage
  , preprocessorFor
  , runPreprocessor
  , MemberBuild (..)
  , buildMember
  , replArgs
  , LibBuild (..)
  , buildLib
  , buildLibArtifacts
  , initPackageDb
  , installedVersions
  ) where

import Data.List (find, intercalate, isPrefixOf, nub)
import Data.Maybe (fromMaybe, isJust)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeExtension, (-<.>), (<.>), (</>))
import System.Process (readProcessWithExitCode)
import Zinc.Except (liftIO, orFail, runResult)
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

-- | The preprocessor command for a source file, or 'Nothing' for plain .hs.
-- Each turns @file.<ext>@ into the sibling @file.hs@.
preprocessorFor :: FilePath -> Maybe (String, [String])
preprocessorFor file = case takeExtension file of
  ".x"   -> Just ("alex", [file, "-o", file -<.> "hs"])
  ".y"   -> Just ("happy", [file, "-o", file -<.> "hs"])
  ".hsc" -> Just ("hsc2hs", [file, "-o", file -<.> "hs"])
  _      -> Nothing

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
buildMember :: MemberBuild -> IO (Either String FilePath)
buildMember mb = do
  createDirectoryIfMissing True (mbBuildDir mb)
  let comp = mbComponent mb
      srcDirs = if null (compSourceDirs comp) then ["."] else compSourceDirs comp
      exe = mbBuildDir mb </> compName comp
      mainFile = mbMemberDir mb </> head srcDirs </> maybe "Main.hs" id (compMain comp)
      args =
        ["--make"]
          ++ maybe [] (\db -> ["-package-db", db]) (mbPackageDb mb)
          ++ ["-hide-all-packages"]
          ++ packageFlags (compDepends comp)
          ++ map (\d -> "-i" ++ (mbMemberDir mb </> d)) srcDirs
          ++ map ("-X" ++) (compExtensions comp)
          ++ compGhcOptions comp
          ++ ["-outputdir", mbBuildDir mb, mainFile, "-o", exe]
  result <- runUnit "ghc" args
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
buildLib :: LibBuild -> IO (Either String ())
buildLib lb = runResult $ do
  conf <- orFail (buildLibArtifacts lb)
  orFail (registerPackage (lbPackageDb lb) conf)

-- | Compile and archive a library and persist its @package.conf@ into the
-- store, returning the conf text — but /without/ registering it into the
-- workspace db. Registration is split out so independent closure libraries can
-- be compiled concurrently and then registered serially (ghc-pkg register on a
-- shared db is not concurrency-safe).
buildLibArtifacts :: LibBuild -> IO (Either String String)
buildLibArtifacts lb = runResult $ do
  liftIO $ createDirectoryIfMissing True (lbDistDir lb)
  -- Synthesize the Cabal-autogen files (Paths_<pkg>, cabal_macros.h) into a
  -- generated-source dir so the package's own modules can import/use them.
  let gen = lbDistDir lb </> "zinc-gen"
  liftIO $ createDirectoryIfMissing True gen
  let comp = lbComponent lb
      -- One ref per package name (spec §2), so the unit-id is just the name;
      -- this also lets dependents reference it by name in their conf depends.
      unitId = lbName lb
      pathsMod = pathsModuleName (lbName lb)
      macrosHeader = gen </> "cabal_macros.h"
  liftIO $ writeFile (gen </> pathsMod <.> "hs") (synthesizePaths (lbName lb) (versionInts (lbVersion lb)))
  installed <- liftIO installedVersions
  let depVersion d = fromMaybe [0] (lookup d installed)
      -- A direct dep's id for the conf's @depends@ (drives a dependent's
      -- linking): zinc-built deps by bare name (their unit-id); non-base boot
      -- libs by their real installed id (e.g. @array-0.5.6.0@) so the linker
      -- pulls them in. base is omitted — it is always linked via -package base.
      depConfId d
        | isBootLib d = d ++ "-" ++ intercalate "." (map show (depVersion d))
        | otherwise = d
  liftIO $ writeFile macrosHeader $
    emitCabalMacros ((lbName lb, versionInts (lbVersion lb)) : [(d, depVersion d) | d <- compDepends comp])
  let srcDirs = if null (compSourceDirs comp) then ["."] else compSourceDirs comp
      -- nub so a package that already lists Paths_<pkg> in its (other-)modules
      -- doesn't collide with the Paths_ module zinc synthesizes.
      modules = nub (compExposedModules comp ++ compOtherModules comp ++ [pathsMod])
      compileArgs =
        ["--make", "-hide-all-packages", "-package-db", lbPackageDb lb]
          ++ packageFlags (compDepends comp)
          ++ map (\d -> "-i" ++ (lbMemberDir lb </> d)) srcDirs
          ++ ["-i" ++ gen, "-optP-include", "-optP" ++ macrosHeader]
          -- C-header search dirs (cabal include-dirs) so CPP #include of the
          -- package's own headers (e.g. version-compatibility-macros.h) resolves.
          ++ concatMap (\d -> let p = lbMemberDir lb </> d in ["-I" ++ p, "-optP-I" ++ p]) (compIncludeDirs comp)
          ++ ["-this-unit-id", unitId, "-outputdir", lbDistDir lb]
          ++ map ("-X" ++) (compExtensions comp)
          ++ compGhcOptions comp
          ++ modules
  -- Generate sources from any .x/.y/.hsc the dep ships (e.g. toml-parser's
  -- alex/happy lexer+parser) so ghc --make finds the resulting .hs modules,
  -- then compile and archive. orFail short-circuits on the first failure.
  orFail (runPreprocessorsIn (map (lbMemberDir lb </>) srcDirs))
  orFail (runUnit "ghc" compileArgs)
  objs <- liftIO (findObjs (lbDistDir lb))
  orFail (runUnit "ar" (archiveArgs (lbDistDir lb) unitId objs))
  let confText =
        renderConf
          PackageConf
            { confName = lbName lb
            , confVersion = lbVersion lb
            , confId = unitId
            , confExposedModules = compExposedModules comp
            , confImportDirs = [lbDistDir lb]
            , confLibraryDirs = [lbDistDir lb]
            , confHsLibraries = ["HS" ++ unitId]
            , -- Direct deps as installed unit-ids so dependents link them:
              -- zinc deps by bare name, non-base boot libs by real id.
              confDepends = [depConfId d | d <- nub (compDepends comp), d /= "base"]
            }
  -- Persist the conf alongside the build so the artifact cache can
  -- re-register it without recompiling.
  liftIO $ writeFile (lbDistDir lb </> "package.conf") confText
  pure confText

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
-- directories so .x/.y/.hsc become .hs before compilation. First failure wins.
runPreprocessorsIn :: [FilePath] -> IO (Either String ())
runPreprocessorsIn dirs = do
  files <- concat <$> mapM preprocessableUnder dirs
  go files
  where
    go [] = pure (Right ())
    go (f : fs) = runPreprocessor f >>= either (pure . Left) (const (go fs))

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
      Nothing -> compExposedModules comp

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
