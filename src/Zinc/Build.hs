-- | The GHC build driver (spec §7): zinc drives @ghc --make@ directly rather
-- than executing Cabal's builder. This module constructs the invocation.
module Zinc.Build
  ( GhcInvocation (..)
  , ghcMakeArgs
  , externalInterpFlags
  , packageFlags
  , zincBuiltUnitIds
  , PackageConf (..)
  , renderConf
  , archiveArgs
  , registerPackage
  , registerPackageFor
  , isRegistered
  , registeredExposedMatches
  , registeredExposedMatchesFor
  , preprocessorFor
  , ppCommand
  , runPreprocessor
  , MemberBuild (..)
  , memberBuildDir
  , buildMember
  , buildMemberFor
  , wasmSupported
  , reactorLinkFlags
  , replArgs
  , LibBuild (..)
  , buildLib
  , buildLibFor
  , buildLibArtifacts
  , buildLibArtifactsFor
  , writeFileIfChanged
  , discoverModules
  , initPackageDb
  , initPackageDbFor
  , installedVersions
  , installedVersionsFor
  , installedUnitIdsFor
  ) where

import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Control.Monad (unless, when)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, nub, sort)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getModificationTime, listDirectory, makeAbsolute)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, makeRelative, normalise, splitDirectories, takeDirectory, takeExtension, (-<.>), (<.>), (</>))
import System.IO (readFile')
import System.Process (CreateProcess (cwd), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Zinc.Diagnostic (ZincError (GhcCompile, OtherError, WasmUnsupported))
import Zinc.Except (liftIO, orFail, orFailE, runResult)
import Zinc.Macros (emitCabalMacros)
import Zinc.Manifest (Component (..))
import Zinc.Paths (pathsModuleName, synthesizePaths)
import Zinc.Resolve (isBootLib)
import Zinc.Target (Target (Native), ghcFor, ghcPkgFor, hsc2hsFor, isWasm)

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

-- | TemplateHaskell splices run in GHC's interpreter. On a dynamically-linked
-- GHC (the nixpkgs default) the *internal* interpreter loads each package
-- dependency as a shared object (@libHS<pkg>.so@) to evaluate a splice. zinc
-- builds dependencies as static archives only, so a splice that calls into
-- another package — e.g. @th-lift@'s @deriveLiftMany@, reached transitively via
-- aeson — fails with "libHSth-lift.so: cannot open shared object file". Routing
-- splices through the *external* interpreter (the vanilla @ghc-iserv@) makes GHC
-- load the static @.a@ via the RTS object linker instead, so TH works against
-- zinc's static deps with no @.so@ needed. GHC spawns iserv only when a module
-- actually has a splice, so TH-free modules are unaffected. WASM cross-compiles
-- wire up their own iserv via the toolchain, so this is native-only (zinc-1wa).
externalInterpFlags :: Target -> [String]
externalInterpFlags t = if isWasm t then [] else ["-fexternal-interpreter"]

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
  , confReexports      :: [(String, String, String)] -- ^ resolved reexports: (newName, originUnitId, originName) — emitted inline in exposed-modules (zinc-jdf)
  , confExtraLibraries :: [String]   -- ^ external C library link names; GHC auto-adds @-l<name>@ for a dependent (zinc-389)
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
    , -- Reexports ride in exposed-modules using ghc-pkg's @New from unit:Orig@
      -- syntax, so a consumer that depends only on this package can import a
      -- module the package re-exports from a dependency (zinc-jdf).
      "exposed-modules: " ++ unwords (confExposedModules c ++ [new ++ " from " ++ unit ++ ":" ++ orig | (new, unit, orig) <- confReexports c])
    , "import-dirs: " ++ unwords (confImportDirs c)
    , "library-dirs: " ++ unwords (confLibraryDirs c)
    , "hs-libraries: " ++ unwords (confHsLibraries c)
    , -- External C libraries (cabal extra-libraries / pkgconfig-depends): GHC
      -- auto-emits @-l<name>@ when a dependent links, so a consumer of a package
      -- that FFIs into e.g. libpq doesn't hit "undefined reference" (zinc-389).
      "extra-libraries: " ++ unwords (confExtraLibraries c)
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
registerPackage = registerPackageFor Native

-- | As 'registerPackage', for an explicit 'Target' (zinc-9po.3): uses the
-- target's @ghc-pkg@ (e.g. @wasm32-wasi-ghc-pkg@). Native is byte-identical.
registerPackageFor :: Target -> FilePath -> String -> IO (Either String ())
registerPackageFor target db confText = do
  initResult <- initPackageDbFor target db
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
      _ <- readProcessWithExitCode (ghcPkgFor target) ["--package-db", db, "unregister", "--force", pkgId] ""
      runUnit (ghcPkgFor target) ["--package-db", db, "register", "--force", confFile]

-- | Create an empty package db (no-op if it already exists).
initPackageDb :: FilePath -> IO (Either String ())
initPackageDb = initPackageDbFor Native

-- | As 'initPackageDb', for an explicit 'Target': the db is created by the
-- target's @ghc-pkg@ so its package.cache matches that compiler (zinc-9po.3).
initPackageDbFor :: Target -> FilePath -> IO (Either String ())
initPackageDbFor target db = do
  createDirectoryIfMissing True (takeDirectory db)
  exists <- doesDirectoryExist db
  if exists then pure (Right ()) else runUnit (ghcPkgFor target) ["init", db]

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
runGhc = runGhcFor Native

-- | As 'runGhc', for an explicit 'Target': invokes the target's @ghc@
-- (e.g. @wasm32-wasi-ghc@). Native is byte-identical (zinc-9po.3).
runGhcFor :: Target -> String -> [String] -> IO (Either ZincError ())
runGhcFor = runGhcInFor Nothing

-- | As 'runGhcFor', but run @ghc@ with an explicit working directory. A package
-- built from its source root lets Template-Haskell file splices
-- (@evalFile@/@embedFile@/@addDependentFile@) resolve PACKAGE-RELATIVE paths —
-- e.g. miso's @$(evalFile "js/miso.js")@ — exactly as Cabal does (it runs the
-- compiler in the package dir). All other compile paths zinc passes are
-- absolute, so the cwd only affects this relative file access (zinc-90t).
runGhcInFor :: Maybe FilePath -> Target -> String -> [String] -> IO (Either ZincError ())
runGhcInFor mcwd target pkg args = do
  (code, _out, err) <- readCreateProcessWithExitCode (proc (ghcFor target) args) {cwd = mcwd} ""
  pure $ case code of
    ExitSuccess   -> Right ()
    ExitFailure _ -> Left (GhcCompile pkg err)

-- | Compile a component's cabal @c-sources@ into the dist dir (each object
-- mirroring its source path, e.g. @\<distDir\>\/cbits\/foo.o@), with the
-- package's @include-dirs@ on the C search path. A separate @ghc -c@ step, so
-- the objects land in the dist dir and get archived into the library — see the
-- call site for why @ghc --make@ cannot place them there (zinc-i98).
compileCSources :: Target -> LibBuild -> Component -> IO (Either ZincError ())
compileCSources target lb comp = runResult (mapM_ one (compCSources comp))
  where
    incs = ["-I" ++ (lbMemberDir lb </> d) | d <- compIncludeDirs comp]
    one c = do
      let src = lbMemberDir lb </> c
          obj = lbDistDir lb </> (c -<.> "o")
      liftIO (createDirectoryIfMissing True (takeDirectory obj))
      orFailE (runGhcFor target (lbName lb) (["-c", src, "-o", obj] ++ incs))

-- | The preprocessor command for a source @file@ writing its generated @.hs@ to
-- @out@ (alex/happy/hsc2hs), or 'Nothing' for a plain @.hs@. @cflags@ are extra
-- C-compiler flags (the package's @-I<include-dir>@) handed to hsc2hs via
-- @--cflag@: hsc2hs generates and compiles a @_hsc_make.c@ with cc, so it must
-- see the package's own bundled headers (e.g. network's @HsNet.h@), exactly as
-- the later real compile already gets them (zinc-bxw.1). alex/happy emit pure
-- Haskell and ignore @cflags@.
ppCommand :: [String] -> FilePath -> FilePath -> Maybe (String, [String])
ppCommand cflags file out = case takeExtension file of
  ".x"   -> Just ("alex", [file, "-o", out])
  ".y"   -> Just ("happy", [file, "-o", out])
  ".hsc" -> Just ("hsc2hs", [file, "-o", out] ++ ["--cflag=" ++ c | c <- cflags])
  _      -> Nothing

-- | The preprocessor command for a source file (no extra cflags), or 'Nothing'
-- for a plain .hs. Each turns @file.<ext>@ into the sibling @file.hs@. Used for
-- detection and the simple (header-free) member path.
preprocessorFor :: FilePath -> Maybe (String, [String])
preprocessorFor file = ppCommand [] file (file -<.> "hs")

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

-- | A member's build-output directory: @\<memberDir\>\/.zinc\/build@. The path
-- is normalised so a flat single-member workspace (member @"."@, the self-host
-- layout) doesn't surface a redundant @.\/.\/@ prefix in the printed artifact
-- path — e.g. @"./."@ would otherwise join to @"././.zinc/build/zinc"@ (zinc-91n.8).
memberBuildDir :: FilePath -> FilePath
memberBuildDir dir = normalise (dir </> ".zinc" </> "build")

-- | @-package@ flags for a component's dependencies (plus the always-present
-- base). A dep zinc itself built (registered in the workspace db, @zincBuilt@) is
-- pinned by its exact unit-id via @-package-id@ — its unit-id is the bare package
-- name — so a same-named package in GHC's global db cannot shadow the version
-- zinc built. This is checked FIRST: a package can be BOTH zinc-built and present
-- in the toolchain's global db (e.g. @ansi-terminal-types@, which the flake's
-- @hspec@ drags in transitively), and zinc's own build must win (zinc-iaj merge).
-- Otherwise, a dep the toolchain provides resolves from the global db by name
-- (@-package@): boot libs, and any bundled library whose unit-id is HASHED — e.g.
-- the wasm GHC's @ghc-experimental@, where @-package-id ghc-experimental@ (bare)
-- cannot match @ghc-experimental-<ver>-<hash>@ (zinc-xum). @toolchain@ is the
-- toolchain's package names (from 'installedVersionsFor'); both lists empty falls
-- back to boot-list-only detection (the native repl, where bare ids hold).
packageFlags :: [String] -> [String] -> [String] -> [String]
packageFlags zincBuilt toolchain deps = concatMap flag (nub ("base" : deps))
  where
    flag d
      | d `elem` zincBuilt = ["-package-id", d]
      | isBootLib d || d `elem` toolchain = ["-package", d]
      | otherwise = ["-package-id", d]

-- | Compile + link a member executable with @ghc --make@, returning the
-- executable path. Isolation via @-hide-all-packages@ + explicit @-package@
-- (base is always available).
buildMember :: MemberBuild -> IO (Either ZincError FilePath)
buildMember = buildMemberFor Native

-- | As 'buildMember', for an explicit 'Target' (zinc-9po.3). For @wasm32-wasi@:
-- the output is a @\<name\>.wasm@ command module, the compiler is the wasm
-- cross-@ghc@, and a member needing C sources / system-libs is rejected up front
-- with 'WasmUnsupported' (the MVP is pure-Haskell only). Native is byte-identical.
buildMemberFor :: Target -> MemberBuild -> IO (Either ZincError FilePath)
buildMemberFor target mb = runResult $ do
  let comp = mbComponent mb
  orFailE (pure (wasmSupported target comp))
  liftIO (createDirectoryIfMissing True (mbBuildDir mb))
  -- Toolchain-provided packages (boot + bundled, e.g. the wasm GHC's
  -- ghc-experimental) resolve by name; a dep zinc itself built (registered in the
  -- member's package db) takes -package-id, even if the same name also lives in
  -- the toolchain's global db (zinc-iaj merge).
  installed <- liftIO (installedVersionsFor target)
  zincBuilt <- liftIO (maybe (pure []) zincBuiltUnitIds (mbPackageDb mb))
  -- A wasm exe with a non-empty wasm-exports is a browser REACTOR module
  -- (zinc-9po.5): no hs-main, the reactor exec-model, and the listed symbols
  -- exported (the linker dead-code-elims anything unexported). Otherwise it is a
  -- plain WASI command module (9po.3) and native is unaffected.
  let reactor = isWasm target && not (null (compWasmExports comp))
      srcDirs = if null (compSourceDirs comp) then ["."] else compSourceDirs comp
      exe = mbBuildDir mb </> compName comp ++ (if isWasm target then ".wasm" else "")
      mainFile = mbMemberDir mb </> head srcDirs </> maybe "Main.hs" id (compMain comp)
      args =
        ["--make", "-j"]
          ++ maybe [] (\db -> ["-package-db", db]) (mbPackageDb mb)
          ++ ["-hide-all-packages"]
          ++ packageFlags zincBuilt (map fst installed) (compDepends comp)
          ++ map (\d -> "-i" ++ (mbMemberDir mb </> d)) srcDirs
          ++ externalInterpFlags target
          ++ map ("-X" ++) (compExtensions comp)
          ++ compGhcOptions comp
          ++ (if reactor then reactorLinkFlags (compWasmExports comp) else [])
          ++ ["-outputdir", mbBuildDir mb, mainFile, "-o", exe]
  orFailE (runGhcFor target (compName comp) args)
  -- A reactor needs the JS-FFI glue (ghc_wasm_jsffi.js) generated from the
  -- linked module by the toolchain's post-link.mjs, so a browser can bind its
  -- `foreign import javascript` calls (zinc-9po.5).
  when reactor (orFailE (generateJsffiGlue target exe (mbBuildDir mb)))
  pure exe

-- | The extra @ghc@ flags that turn a wasm executable into a browser reactor
-- module exporting @exports@ (zinc-9po.5): no Haskell @main@, the @reactor@
-- exec-model, and an explicit linker @--export@ per symbol so wasm-ld keeps them.
-- @hs_init@ is always exported (a reactor's host must call it once to start the
-- Haskell RTS before any other export, else @newBoundTask: RTS is not
-- initialised@) — so the user lists only their own entry points.
reactorLinkFlags :: [String] -> [String]
reactorLinkFlags exports =
  ["-no-hs-main", "-optl-mexec-model=reactor"]
    ++ ["-optl-Wl,--export=" ++ e | e <- nub ("hs_init" : exports)]

-- | Generate the @ghc_wasm_jsffi.js@ glue beside a linked reactor module, via
-- the toolchain's @post-link.mjs@ (run with the provisioned @node@). The glue
-- binds the module's @foreign import javascript@ imports for a browser host.
generateJsffiGlue :: Target -> FilePath -> FilePath -> IO (Either ZincError ())
generateJsffiGlue target wasmPath outDir = do
  (_, libdirOut, _) <- readProcessWithExitCode (ghcFor target) ["--print-libdir"] ""
  let libdir = takeWhile (/= '\n') libdirOut
      postLink = libdir </> "post-link.mjs"
      glue = outDir </> "ghc_wasm_jsffi.js"
  (code, _, err) <- readProcessWithExitCode "node" [postLink, "--input", wasmPath, "--output", glue] ""
  pure $ case code of
    ExitSuccess   -> Right ()
    ExitFailure _ -> Left (OtherError ("post-link JS-FFI glue generation failed: " ++ err))

-- | Reject a component that cannot build for a wasm target. Cabal @c-sources@
-- ARE supported (zinc-90t): the wasm toolchain ships a clang that cross-compiles
-- portable C to wasm, and zinc compiles them with the target @ghc@ like any
-- object (e.g. miso's @cbits/foreign.c@). What still can't cross-compile is a
-- dependency on a prebuilt system library (@extra-libraries@): there is rarely a
-- wasm build of an external C library to link against, so that stays an up-front
-- 'WasmUnsupported' rather than a cryptic link failure (zinc-9po.3 / spec §5).
-- Non-portable C (Linux-only headers/syscalls) still fails, but at compile time
-- with the toolchain's own error, exactly as it would natively. Native passes.
wasmSupported :: Target -> Component -> Either ZincError ()
wasmSupported target comp
  | isWasm target, not (null (compSystemLibs comp)) =
      Left (WasmUnsupported (compName comp) "needs system libraries (extra-libraries); wasm32-wasi has no prebuilt wasm build of an external C library to link")
  | otherwise = Right ()

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
buildLib = buildLibFor Native

-- | As 'buildLib', for an explicit 'Target' (zinc-9po.3): artifacts + registration
-- go through the target's toolchain. Native is byte-identical.
buildLibFor :: Target -> LibBuild -> IO (Either ZincError ())
buildLibFor target lb = runResult $ do
  (conf, confChanged) <- orFailE (buildLibArtifactsFor target lb)
  -- Skip re-registration on a persisted db when the conf is unchanged and the
  -- lib is already registered at this dir (inner-loop incrementality).
  reg <- liftIO (isRegisteredFor target (lbPackageDb lb) (lbName lb) (lbDistDir lb))
  when (confChanged || not reg) (orFail (registerPackageFor target (lbPackageDb lb) conf))

-- | Compile and archive a library and persist its @package.conf@ into the
-- store, returning the conf text — but /without/ registering it into the
-- workspace db. Registration is split out so independent closure libraries can
-- be compiled concurrently and then registered serially (ghc-pkg register on a
-- shared db is not concurrency-safe).
buildLibArtifacts :: LibBuild -> IO (Either ZincError (String, Bool))
buildLibArtifacts = buildLibArtifactsFor Native

-- | As 'buildLibArtifacts', for an explicit 'Target' (zinc-9po.3): compiles +
-- archives with the target's toolchain. A wasm target rejects a C-source /
-- system-lib library up front ('WasmUnsupported'). Native is byte-identical.
buildLibArtifactsFor :: Target -> LibBuild -> IO (Either ZincError (String, Bool))
buildLibArtifactsFor target lb = runResult $ do
  orFailE (pure (wasmSupported target (lbComponent lb)))
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
  installed <- liftIO (installedVersionsFor target)
  bootUnitIds <- liftIO (installedUnitIdsFor target)
  zincBuilt <- liftIO (zincBuiltUnitIds (lbPackageDb lb))
  let depVersion d = fromMaybe [0] (lookup d installed)
      -- A direct dep's id for the conf's @depends@ (drives a dependent's
      -- linking). A dep zinc itself built is recorded by its bare unit-id (its
      -- name) — checked FIRST, so a dep that is BOTH zinc-built AND in the
      -- toolchain's global db (e.g. @ansi-terminal-types@ via the flake's
      -- @hspec@) is recorded as zinc's own, matching what 'resolveReexports'
      -- resolves the reexport origin to (also queried against this db). Otherwise
      -- a TOOLCHAIN-provided dep takes its real installed unit-id: a stock GHC
      -- hashes those (the wasm cross GHC's @bytestring@ is
      -- @bytestring-0.12.2.0-2834@), so a synthesized @name-version@ won't match
      -- (zinc-90t/zinc-xum). Fall back to @name-version@ when the lookup misses.
      -- base is omitted — it is always linked via -package base.
      depConfId d
        | d `elem` zincBuilt = d
        | otherwise = fromMaybe (d ++ "-" ++ intercalate "." (map show (depVersion d))) (lookup d bootUnitIds)
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
  -- Only zinc-native components auto-discover on an empty module list; a
  -- cabal-sourced component (compFromCabal) trusts its .cabal list even when
  -- empty — e.g. a build-type: Configure dep — so we never sweep its root and
  -- mistakenly compile Setup.hs (zinc-iaj.1).
  discovered <-
    if null (compModules comp) && not (compFromCabal comp)
      then liftIO (discoverModules [lbMemberDir lb </> d | d <- srcDirs])
      else pure (compModules comp)
  -- nub so a package that already lists Paths_<pkg> doesn't collide with the
  -- Paths_ module zinc synthesizes.
  -- The compile runs with cwd=lbMemberDir (for package-relative TH file splices),
  -- so the package-db — which zinc holds relative to the workspace — must be
  -- absolute or ghc can't find it from the package dir (zinc-90t).
  absDb <- liftIO (makeAbsolute (lbPackageDb lb))
  let modules = nub (discovered ++ [pathsMod])
      compileArgs =
        ["--make", "-j", "-hide-all-packages", "-package-db", absDb]
          ++ packageFlags zincBuilt (map fst installed) (compDepends comp)
          ++ map (\d -> "-i" ++ (lbMemberDir lb </> d)) srcDirs
          ++ ["-i" ++ gen, "-i" ++ ppGen, "-optP-include", "-optP" ++ macrosHeader]
          -- C-header search dirs (cabal include-dirs) so CPP #include of the
          -- package's own headers (e.g. version-compatibility-macros.h) resolves.
          ++ concatMap (\d -> let p = lbMemberDir lb </> d in ["-I" ++ p, "-optP-I" ++ p]) (compIncludeDirs comp)
          ++ ["-this-unit-id", unitId, "-outputdir", lbDistDir lb]
          ++ externalInterpFlags target
          ++ map ("-X" ++) (compExtensions comp)
          -- CPP -D defines (cabal cpp-options) for conditionally-compiled source.
          ++ map ("-optP" ++) (compCppOptions comp)
          ++ compGhcOptions comp
          ++ modules
  -- Generate sources from any .x/.y/.hsc the dep ships (e.g. toml-parser's
  -- alex/happy lexer+parser) so ghc --make finds the resulting .hs modules,
  -- then compile and archive. orFail short-circuits on the first failure.
  -- hsc2hs compiles a C program that may use MIN_VERSION_<dep>/VERSION_<dep> CPP
  -- macros (e.g. network's Flag.hsc: @#if !(MIN_VERSION_base(4,11,0))@). GHC
  -- auto-generates these for @ghc --make@, but the preprocessor's cc step has no
  -- such help, so emit a macros header covering this package's deps and -include
  -- it ahead of every .hsc compile (zinc-bxw.4).
  let hscMacros = gen </> "hsc_macros.h"
  _ <- liftIO $ writeFileIfChanged hscMacros $
    emitCabalMacros [(d, depVersion d) | d <- nub ("base" : compDepends comp)]
  orFail (runPreprocessorsTo target (["-I" ++ (lbMemberDir lb </> d) | d <- compIncludeDirs comp] ++ ["-include", hscMacros]) (if null (compModules comp) then Nothing else Just modules) ppGen (map (lbMemberDir lb </>) srcDirs))
  -- Run in the package source root so package-relative TH file splices resolve.
  orFailE (runGhcInFor (Just (lbMemberDir lb)) target (lbName lb) compileArgs)
  -- C sources (cabal c-sources, e.g. primitive's cbits/primitive-memops.c) are
  -- compiled in a SEPARATE `ghc -c` step into the dist dir, NOT via `ghc --make`:
  -- --make writes a C object next to its (absolute) source — outside -outputdir
  -- and into the content-addressed src tree — so findObjs would never archive it
  -- and a dependent linking the library hits "undefined reference" (zinc-i98:
  -- hsprimitive_memset_*, splitmix_init). Built with the package's include-dirs.
  orFailE (compileCSources target lb comp)
  objs <- liftIO (findObjs (lbDistDir lb))
  -- Re-archive only when an object is newer than the archive: ghc --make keeps
  -- objects incremental, so an unchanged lib's .a (and the exe linking it)
  -- need not be rebuilt.
  let aPath = lbDistDir lb </> ("libHS" ++ unitId ++ ".a")
  stale <- liftIO (archiveStale aPath objs)
  when stale $ orFail (runUnit "ar" (archiveArgs (lbDistDir lb) unitId objs))
  -- Resolve cabal reexported-modules to (new, originUnitId, orig) by finding
  -- which already-registered dependency exposes each origin module (zinc-jdf),
  -- so the conf can carry `New from unit:Orig` and a consumer importing the
  -- re-exported module needs only this package on its -package list.
  reexports <- liftIO (resolveReexports target (lbPackageDb lb) (nub (compDepends comp)) (compReexports comp))
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
            , confReexports = reexports
            , confExtraLibraries = compExtraLibs comp
            }
  -- Persist the conf alongside the build (the artifact cache re-registers it
  -- without recompiling); report whether it changed so a sibling lib can skip
  -- re-registration on the persisted db.
  confChanged <- liftIO (writeFileIfChanged (lbDistDir lb </> "package.conf") confText)
  pure (confText, confChanged)

-- | Discover a library's modules by walking its source dirs: every
-- @.hs\/.lhs\/.hsc\/.x\/.y@ file becomes a dotted module name (its path under the
-- source dir, @\/@ -> @.@, extension dropped), excluding @Main@ and @Setup@ (a
-- cabal build driver). This backs the
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
            , m /= "Setup" -- Setup.hs/.lhs is a cabal build driver, not a module (zinc-iaj.1)
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
isRegistered = isRegisteredFor Native

-- | As 'isRegistered', querying the target's @ghc-pkg@ (zinc-9po.3).
isRegisteredFor :: Target -> FilePath -> String -> FilePath -> IO Bool
isRegisteredFor target db unitId pkgDir = do
  (code, out, _) <- readProcessWithExitCode (ghcPkgFor target) ["--package-db", db, "field", unitId, "library-dirs"] ""
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
-- @mAllow@ restricts preprocessing to files whose derived module name is in the
-- list (the component's finalized modules). A cabal dependency selects its
-- modules by platform (@network@'s @Network.Socket.Win32.*@ live only under
-- @if os(windows)@), so on Linux those @.hsc@ must NOT be preprocessed — hsc2hs
-- would choke on Win32-only identifiers (zinc-bxw.3). @Nothing@ preprocesses
-- everything (a zinc-native package auto-discovers .hs and lists no modules).
runPreprocessorsTo :: Target -> [String] -> Maybe [String] -> FilePath -> [FilePath] -> IO (Either String ())
runPreprocessorsTo target cflags mAllow genRoot dirs = do
  pairs <- concat <$> mapM (\d -> map ((,) d) <$> preprocessableUnder d) dirs
  go (filter inBuild pairs)
  where
    inBuild (dir, f) = case mAllow of
      Nothing    -> True
      Just allow -> moduleNameOf dir f `elem` allow
    moduleNameOf dir f = intercalate "." (splitDirectories (dropExtension (makeRelative dir f)))
    go [] = pure (Right ())
    go ((dir, f) : rest) =
      let out = genRoot </> (makeRelative dir f -<.> "hs")
       in case ppCommand cflags f out of
            Nothing -> go rest
            Just (prog, args) -> do
              createDirectoryIfMissing True (takeDirectory out)
              -- alex/happy are host code generators (stay native); only hsc2hs
              -- is the cross-prefixed tool for a wasm target (zinc-9po.3).
              let prog' = if prog == "hsc2hs" then hsc2hsFor target else prog
              runUnit prog' args >>= either (pure . Left) (const (go rest))

-- | ghci argument list to load a component for @zinc repl@: the package db,
-- isolation flags + exposed deps, source roots, and the targets to load (the
-- main file for an executable, or the exposed modules for a library).
replArgs :: Maybe FilePath -> FilePath -> Component -> [String]
replArgs packageDb memberDir comp =
  maybe [] (\db -> ["-package-db", db]) packageDb
    ++ ["-hide-all-packages"]
    ++ packageFlags [] [] (compDepends comp)
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
-- | Resolve cabal reexports to @(newName, originUnitId, originName)@ for the
-- @.conf@ (zinc-jdf). A reexport that names its origin package uses it directly
-- (zinc's unit-id is the bare package name); a bare reexport is resolved by
-- finding which dependency already registered in @db@ exposes the origin module.
-- Unresolvable reexports are dropped (rather than emit a conf ghc-pkg rejects).
resolveReexports :: Target -> FilePath -> [String] -> [(String, Maybe String, String)] -> IO [(String, String, String)]
resolveReexports _ _ _ [] = pure []
resolveReexports target db deps reexs = do
  exposedByDep <- mapM (\d -> (,) d <$> exposedModulesOf target db d) deps
  let originOf orig = listToMaybe [d | (d, mods) <- exposedByDep, orig `elem` mods]
  pure [(new, unit, orig) | (new, mPkg, orig) <- reexs, Just unit <- [maybe (originOf orig) Just mPkg]]

-- | The bare unit-ids zinc itself has registered into a workspace package db:
-- the basenames of the @\<id\>.conf@ files ghc-pkg writes there. zinc registers
-- everything it builds with @id == package name@, and a workspace db holds ONLY
-- zinc-built packages (the global db is separate, stacked at use time), so this
-- is exactly the set of zinc-built deps. Used to pin a dep by its zinc-built bare
-- id even when the SAME name also lives in the toolchain's global db with a
-- hashed id (e.g. @ansi-terminal-types@, pulled in transitively by the flake's
-- @hspec@) — there, zinc's own build must win, both on the @-package-id@ flag and
-- in the conf @depends@, or the conf's reexport origin won't match its depends
-- and ghc-pkg marks the package broken (zinc-iaj merge regression).
zincBuiltUnitIds :: FilePath -> IO [String]
zincBuiltUnitIds db = do
  exists <- doesDirectoryExist db
  if not exists
    then pure []
    else do
      fs <- listDirectory db
      pure [dropExtension f | f <- fs, takeExtension f == ".conf"]

-- | The module names a registered package exposes (own + its own reexports),
-- via @ghc-pkg field <pkg> exposed-modules@. Reexport @from@ annotations are
-- left as separate tokens (harmless for membership lookup).
exposedModulesOf :: Target -> FilePath -> String -> IO [String]
exposedModulesOf target db pkg = do
  (_, out, _) <- readProcessWithExitCode (ghcPkgFor target) ["--package-db", db, "field", pkg, "exposed-modules"] ""
  let body = drop 1 (dropWhile (/= ':') out) -- after the "exposed-modules:" label
  pure (words (map (\ch -> if ch == ',' then ' ' else ch) body))

-- | Whether @unitId@'s currently-registered @exposed-modules@ (own modules plus
-- the inline @New from unit:Orig@ reexports) match those declared in @confText@.
-- 'isRegistered' only checks that the pkg dir is registered, so it cannot see a
-- conf whose /content/ drifted at the same rev/pkg-dir — e.g. a zinc upgrade that
-- taught the builder to emit Cabal reexports (zinc-jdf) into a package that was
-- already registered in a persisted workspace db. Without re-registering, that
-- db keeps the pre-upgrade conf and the reexported modules stay invisible to a
-- consumer, so @import \<reexported\>@ fails (zinc-0k7). Comparing exposed-modules
-- catches exactly that drift; library-dirs drift is already caught by the pkg-dir
-- check, and the other conf fields are pinned by the build key.
registeredExposedMatches :: FilePath -> String -> String -> IO Bool
registeredExposedMatches = registeredExposedMatchesFor Native

-- | As 'registeredExposedMatches', querying the target's @ghc-pkg@.
registeredExposedMatchesFor :: Target -> FilePath -> String -> String -> IO Bool
registeredExposedMatchesFor target db unitId confText = do
  (_, out, _) <- readProcessWithExitCode (ghcPkgFor target) ["--package-db", db, "field", unitId, "exposed-modules"] ""
  let declaredLine = fromMaybe "" (find ("exposed-modules:" `isPrefixOf`) (lines confText))
  pure (toks out == toks declaredLine)
  where
    -- The set of tokens after the "exposed-modules:" label, whitespace/comma
    -- and order insensitive (ghc-pkg comma-separates + line-wraps; the conf
    -- space-separates on one line — both yield the same word multiset).
    toks raw = sort (words (map (\ch -> if ch == ',' then ' ' else ch) (drop 1 (dropWhile (/= ':') raw))))

-- | The installed unit-id (@ghc-pkg field <pkg> id@) of each package the target
-- toolchain provides, e.g. @[("bytestring","bytestring-0.12.2.0-2834"), …]@. A
-- stock GHC HASHES boot-lib unit-ids (the wasm cross GHC does), so a dependent's
-- conf @depends@ must name the toolchain's ACTUAL id, not a synthesized
-- @name-version@, or the package is "unusable due to missing dependencies"
-- (zinc-90t). Parsed from @ghc-pkg field '*' name,id@ (alternating lines).
installedUnitIdsFor :: Target -> IO [(String, String)]
installedUnitIdsFor target = do
  (_, out, _) <- readProcessWithExitCode (ghcPkgFor target) ["field", "*", "name,id"] ""
  pure (pair (mapMaybe value (lines out)))
  where
    value l = case break (== ':') l of
      (k, ':' : v) | k `elem` ["name", "id"] -> Just (dropWhile (== ' ') v)
      _                                       -> Nothing
    pair (n : i : rest) = (n, i) : pair rest
    pair _              = []

installedVersions :: IO [(String, [Int])]
installedVersions = installedVersionsFor Native

-- | As 'installedVersions', for the target's @ghc-pkg@ (zinc-9po.3): a wasm
-- build reads the wasm cross-compiler's boot-library versions.
installedVersionsFor :: Target -> IO [(String, [Int])]
installedVersionsFor target = do
  (_, out, _) <- readProcessWithExitCode (ghcPkgFor target) ["list", "--simple-output"] ""
  pure [(name, versionInts ver) | pid <- words out, Just (name, ver) <- [splitNameVer pid]]
  where
    splitNameVer pid = case reverse (splitOnDash pid) of
      (v : rest@(_ : _)) | isVersion v -> Just (intercalate "-" (reverse rest), v)
      _                                -> Nothing
    isVersion v = not (null v) && all (`elem` ("0123456789." :: String)) v
    splitOnDash s = case break (== '-') s of
      (a, [])    -> [a]
      (a, _ : r) -> a : splitOnDash r
