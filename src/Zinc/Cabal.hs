-- | Opt-2 @.cabal@ reader (spec §3): parse a Hackage @.cabal@ file with the
-- @Cabal@ library (as a parser only — never its builder) and derive the same
-- 'Component' model zinc-native @[build]@ blocks produce. This lets zinc build
-- arbitrary upstream packages pulled from git without zinc-ifying them.
--
-- Conditionals (@if flag(...)/os(...)/impl(ghc ...)@) are resolved with
-- 'finalizePD' against default flags + the current platform + a GHC compiler.
module Zinc.Cabal
  ( parseCabalComponents
  , parseCabalComponentsForGhc
  , parseCabalComponentsForPlatform
  , cabalBuildType
  , cabalVersion
  , cabalJsSources
  , bootConflicts
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Foldable (toList)
import Data.List (intercalate, nub)
import Data.Maybe (mapMaybe)
import Distribution.Compiler
  ( AbiTag (NoAbiTag)
  , CompilerFlavor (GHC)
  , CompilerId (CompilerId)
  , unknownCompilerInfo
  )
import Distribution.PackageDescription
  ( BuildInfo
  , Executable (buildInfo, exeName, modulePath)
  , Library
  , PackageDescription (executables, library, package, subLibraries, testSuites)
  , TestSuite (testBuildInfo, testInterface, testName)
  , TestSuiteInterface (TestSuiteExeV10)
  , allBuildInfo
  , buildType
  , cSources
  , jsSources
  , cppOptions
  , defaultExtensions
  , defaultLanguage
  , packageDescription
  , exposedModules
  , extraLibs
  , hcOptions
  , hsSourceDirs
  , includeDirs
  , libBuildInfo
  , otherModules
  , pkgconfigDepends
  , targetBuildDepends
  )
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Pretty (prettyShow)
import Distribution.System (Platform, buildPlatform)
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec (ComponentRequestedSpec))
import Distribution.Types.Flag (mkFlagAssignment, mkFlagName)
import Distribution.Types.Dependency (depLibraries, depPkgName, depVerRange)
import Distribution.Types.Library (libName, reexportedModules)
import Distribution.Types.ModuleReexport (ModuleReexport (moduleReexportName, moduleReexportOriginalName, moduleReexportOriginalPackage))
import Distribution.Types.LibraryName (LibraryName (LSubLibName))
import Distribution.Types.PackageId (pkgName, pkgVersion)
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.PkgconfigDependency (PkgconfigDependency (PkgconfigDependency))
import Distribution.Types.PkgconfigName (unPkgconfigName)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Version (mkVersion, withinRange)
import Zinc.Manifest (Component (..), ComponentKind (..))
import Zinc.SysLibs (toNixpkgs)

-- | Derive zinc 'Component's from @.cabal@ source, resolving conditionals
-- against a recent default GHC. See 'parseCabalComponentsForGhc'.
parseCabalComponents :: String -> Either String [Component]
parseCabalComponents = parseCabalComponentsForGhc "9.6.5"

-- | Like 'parseCabalComponents', but resolve @impl(ghc ...)@ conditionals
-- against the given GHC version (e.g. the workspace's @ghc@), for the HOST
-- platform.
parseCabalComponentsForGhc :: String -> String -> Either String [Component]
parseCabalComponentsForGhc = parseCabalComponentsForPlatform buildPlatform []

-- | Like 'parseCabalComponentsForGhc', but resolve @os(...)/arch(...)@
-- conditionals against an explicit 'Platform' (zinc-xum). A wasm build must
-- finalize against @Platform Wasm32 Wasi@, not the host: otherwise @arch(wasm32)@
-- is false and a package like miso builds its VANILLA FFI variant (ffi/ghc,
-- @-DVANILLA@) instead of the wasm one (ffi/wasm, @-DWASM@, @Miso.DSL.TH@,
-- @ghc-experimental@). The host platform is the right default elsewhere.
parseCabalComponentsForPlatform :: Platform -> [(String, Bool)] -> String -> String -> Either String [Component]
parseCabalComponentsForPlatform platform flags ghcVersion src =
  case snd (runParseResult (parseGenericPackageDescription (BS.pack src))) of
    Left err -> Left ("cabal parse error: " ++ show err)
    Right gpd ->
      -- Request the LIBRARY (+exes) only, NOT tests/benchmarks: zinc builds a
      -- dependency's library, and finalizePD's automatic-flag resolution can
      -- otherwise flip a shared flag to keep a test-suite buildable, dragging
      -- test-only deps (QuickCheck, tasty -> ansi-terminal -> colour) into the
      -- library's build-depends and the closure (zinc-ffm.7).
      --
      -- Manual flags (zinc-iaj.2) PIN the listed flags (e.g.
      -- @use-pkg-config = true@ to select postgresql-libpq's pkg-config
      -- provider); finalizePD's automatic resolution still fills the rest, so
      -- manual wins and automatic covers the gaps.
      case finalizePD flagAssignment (ComponentRequestedSpec False False) (const True) platform ghc [] gpd of
        Left missing -> Left ("cabal finalize error: unsatisfied " ++ show (map prettyShow missing))
        Right (pd, _flags) ->
          Right (libraryComponent pd ++ executableComponents pd ++ testComponents pd)
  where
    ghc = unknownCompilerInfo (CompilerId GHC (mkVersion (versionInts ghcVersion))) NoAbiTag
    flagAssignment = mkFlagAssignment [(mkFlagName n, v) | (n, v) <- flags]

-- | Every @js-sources@ file declared across a .cabal's components (zinc-gdk),
-- best-effort ([] on a parse/finalize miss). Finalized against @platform@ so a
-- wasm-gated @js-sources@ (e.g. miso ships @js/miso.js@ for its wasm reactor) is
-- seen; the paths are relative to the package root. Used to surface a wasm
-- dependency's JS runtime next to the built @.wasm@ so a browser page can load
-- it, for frameworks that don't self-embed their JS into the wasm.
cabalJsSources :: Platform -> String -> [String]
cabalJsSources platform src =
  case snd (runParseResult (parseGenericPackageDescription (BS.pack src))) of
    Left _ -> []
    Right gpd ->
      case finalizePD (mkFlagAssignment []) (ComponentRequestedSpec False False) (const True) platform ghc [] gpd of
        Left _        -> []
        Right (pd, _) -> nub (concatMap jsSources (allBuildInfo pd))
  where
    ghc = unknownCompilerInfo (CompilerId GHC (mkVersion [9, 6, 5])) NoAbiTag

-- | Boot-library version conflicts in a @.cabal@ (zinc-sib): each
-- @build-depends@ on a GHC boot library whose declared version range EXCLUDES
-- the toolchain's installed version. This is the structured detection behind the
-- @ZINC_DEP_BOOT_CONFLICT@ diagnostic — it turns a cryptic downstream
-- @ErrorT not in scope@ GHC failure (a stale tag pinning e.g. @transformers <0.6@
-- against a 0.6 toolchain) into a named conflict. Returns @(bootLib,
-- declaredRange, toolchainVersion)@ per offending dependency.
--
-- PRINCIPLE GUARD (spec line 81): the bound is read for DIAGNOSTICS ONLY and is
-- never fed back into resolution — zinc still pins by tag, not by solving bounds.
bootConflicts
  :: (String -> Bool)   -- ^ is this a GHC boot library?
  -> [(String, [Int])]  -- ^ toolchain installed versions (from 'Zinc.Build.installedVersions')
  -> String             -- ^ GHC version (to resolve @impl(ghc ...)@ conditionals)
  -> String             -- ^ @.cabal@ source
  -> Either String [(String, String, String)]
bootConflicts isBoot toolchain ghcVersion src =
  case snd (runParseResult (parseGenericPackageDescription (BS.pack src))) of
    Left err -> Left ("cabal parse error: " ++ show err)
    Right gpd ->
      case finalizePD mempty (ComponentRequestedSpec False False) (const True) buildPlatform ghc [] gpd of
        Left missing -> Left ("cabal finalize error: unsatisfied " ++ show (map prettyShow missing))
        Right (pd, _flags) ->
          Right $
            nub
              [ (name, prettyShow range, intercalate "." (map show ver))
              | bi <- maybe [] (pure . libBuildInfo) (library pd)
              , d <- targetBuildDepends bi
              , let name = unPackageName (depPkgName d)
              , isBoot name
              , Just ver <- [lookup name toolchain]
              , let range = depVerRange d
              , not (withinRange (mkVersion ver) range)
              ]
  where
    ghc = unknownCompilerInfo (CompilerId GHC (mkVersion (versionInts ghcVersion))) NoAbiTag

-- | Parse a dotted version string into integer components (e.g. @"9.6.5"@).
versionInts :: String -> [Int]
versionInts = map read . splitDots
  where
    splitDots s = case break (== '.') s of
      (a, [])    -> [a]
      (a, _ : r) -> a : splitDots r

-- | The package's single library unit: the main library with every INTERNAL
-- sub-library it (transitively) depends on (private @library \<name\>@ stanzas,
-- e.g. attoparsec's @attoparsec-internal@) FLATTENED in — their source dirs,
-- modules, deps and flags merged, and the sub-library names dropped from
-- build-depends (they are this same unit now, not external packages). zinc
-- models one library per package and hides no modules (spec §2, §4), so an
-- internal library is just more source folded into the one unit; the external
-- view (the exposed API + package name a dependent links) is unchanged.
-- (zinc-ffm.2)
--
-- Only sub-libraries the main library actually depends on are folded in: a
-- package can also ship a /public/ sub-library used solely by its own
-- benchmarks/tests (e.g. vector's @benchmarks-O2@), and merging that would drag
-- its bench/test-only deps (random, tasty) into the library's build-depends,
-- making the build pass @-package random@ for a package not in the closure
-- (zinc-ffm.5).
libraryComponent :: PackageDescription -> [Component]
libraryComponent pd = case library pd of
  Nothing   -> []
  Just main ->
    let subs      = subLibraries pd
        subNames  = mapMaybe subLibName subs
        -- The sub-libraries reachable from the main library's build-depends.
        needed    = closure subNames subs (subDepsOf subNames (libBuildInfo main))
        neededSubs = [s | s <- subs, maybe False (`elem` needed) (subLibName s)]
        -- Cabal records a build-depends on an internal sub-library as a
        -- dependency on the PACKAGE ITSELF (so it reads as the package name),
        -- and some forms as the bare sub-library name. Both are this one unit —
        -- drop every sub-library name (a library never -packages itself; leaving
        -- the self name in yields "cannot satisfy -package-id <self>").
        selfNames = unPackageName (pkgName (package pd)) : subNames
        merged    = foldr (mergeLib . fromLibrary) (fromLibrary main) neededSubs
     in [merged {compDepends = filter (`notElem` selfNames) (compDepends merged)}]
  where
    subLibName lib = case libName lib of
      LSubLibName n -> Just (unUnqualComponentName n)
      _             -> Nothing

-- | Names of THIS package's sub-libraries referenced by a build-info — both the
-- modern @pkg:sublib@ form (via @depLibraries@) and the legacy bare
-- sub-library-name form (a dep whose package name is one of our sub-libraries).
subDepsOf :: [String] -> BuildInfo -> [String]
subDepsOf subNames bi =
  [unUnqualComponentName n | d <- deps, LSubLibName n <- toList (depLibraries d)]
    ++ [p | d <- deps, let p = unPackageName (depPkgName d), p `elem` subNames]
  where
    deps = targetBuildDepends bi

-- | Transitive closure of the sub-libraries needed, starting from a seed set and
-- following each needed sub-library's own sub-library deps.
closure :: [String] -> [Library] -> [String] -> [String]
closure subNames subs = go []
  where
    byName = [(n, l) | l <- subs, LSubLibName n' <- [libName l], let n = unUnqualComponentName n']
    go seen [] = seen
    go seen (x : xs)
      | x `elem` seen = go seen xs
      | otherwise = case lookup x byName of
          Just l  -> go (x : seen) (xs ++ subDepsOf subNames (libBuildInfo l))
          Nothing -> go seen xs

-- | Fold a sub-library's build inputs into the accumulating library component
-- (union of source dirs, modules, deps and flags); the kind/name/main stay the
-- main library's.
mergeLib :: Component -> Component -> Component
mergeLib sub acc =
  acc
    { compSourceDirs  = nub (compSourceDirs acc ++ compSourceDirs sub)
    , compModules     = nub (compModules acc ++ compModules sub)
    , compDepends     = nub (compDepends acc ++ compDepends sub)
    , compExtensions  = nub (compExtensions acc ++ compExtensions sub)
    , compGhcOptions  = compGhcOptions acc ++ compGhcOptions sub
    , compIncludeDirs = nub (compIncludeDirs acc ++ compIncludeDirs sub)
    , compCppOptions  = compCppOptions acc ++ compCppOptions sub
    , compCSources    = nub (compCSources acc ++ compCSources sub)
    , compSystemLibs  = nub (compSystemLibs acc ++ compSystemLibs sub)
    , compExtraLibs   = nub (compExtraLibs acc ++ compExtraLibs sub)
    , compPkgconfig   = nub (compPkgconfig acc ++ compPkgconfig sub)
    , compReexports   = nub (compReexports acc ++ compReexports sub)
    , compWasmExports = nub (compWasmExports acc ++ compWasmExports sub)
    }

executableComponents :: PackageDescription -> [Component]
executableComponents pd =
  [ (fromBuildInfo Executable (unUnqualComponentName (exeName exe)) (buildInfo exe))
      { compMain = Just (modulePath exe) }
  | exe <- executables pd
  ]

testComponents :: PackageDescription -> [Component]
testComponents pd =
  [ (fromBuildInfo TestSuite (unUnqualComponentName (testName ts)) (testBuildInfo ts))
      { compMain = testMain ts }
  | ts <- testSuites pd
  ]

testMain :: TestSuite -> Maybe String
testMain ts = case testInterface ts of
  TestSuiteExeV10 _ path -> Just path
  _                      -> Nothing

fromLibrary :: Library -> Component
fromLibrary lib =
  let c = fromBuildInfo Library "lib" (libBuildInfo lib)
   in c
        { compModules = map prettyShow (exposedModules lib) ++ compModules c
        , -- Cabal reexported-modules (zinc-jdf): a bare reexport has no origin
          -- package (Nothing — resolved at conf time from the deps that expose
          -- it); @Orig as New@ / @pkg:Orig as New@ name it explicitly.
          compReexports =
            [ ( prettyShow (moduleReexportName r)
              , unPackageName <$> moduleReexportOriginalPackage r
              , prettyShow (moduleReexportOriginalName r)
              )
            | r <- reexportedModules lib
            ]
        }

-- | The fields common to every component, pulled from a 'BuildInfo'.
fromBuildInfo :: ComponentKind -> String -> BuildInfo -> Component
fromBuildInfo kind name bi =
  Component
    { compKind = kind
    , compName = name
    , compSourceDirs = map getSymbolicPath (hsSourceDirs bi)
    , compModules = map prettyShow (otherModules bi)
    , compMain = Nothing
    , -- The component's @default-language@ (e.g. @Haskell2010@) leads the @-X@
      -- flags so it sets the base language edition before any extension. Cabal
      -- always applies one; dropping it lets GHC's default poly-kind a phantom
      -- type variable (@s :: k@ vs @s :: *@), breaking packages that rely on
      -- Haskell2010 kind defaulting — e.g. vector's @Data.Vector.*.Mutable@
      -- (zinc-ffm.5). zinc-native components declare none, so are unchanged.
      compExtensions = maybe [] (\l -> [prettyShow l]) (defaultLanguage bi) ++ map prettyShow (defaultExtensions bi)
    , compGhcOptions = hcOptions GHC bi
    , compDepends = map (unPackageName . depPkgName) (targetBuildDepends bi)
    , compSystemLibs = nub (mapMaybe toNixpkgs (extraLibs bi ++ pkgconfigNames bi))
    , -- C library LINK names for the conf's @extra-libraries:@ (zinc-389): cabal
      -- @extra-libraries@ are already link names. @pkgconfig-depends@ MODULES are
      -- kept separate ('compPkgconfig') and resolved to link names via
      -- @pkg-config --libs@ at build time, because a module name is not the lib
      -- name (pkgconfig @zlib@ -> @-lz@, not @-lzlib@; zinc-mmx).
      compExtraLibs = nub (extraLibs bi)
    , compPkgconfig = nub (pkgconfigNames bi)
    , compIncludeDirs = includeDirs bi
    , compCppOptions = cppOptions bi
    , compCSources = cSources bi
    , compReexports = [] -- set by 'fromLibrary' (only libraries reexport); exes/tests have none
    , compWasmExports = [] -- wasm reactor exports are a zinc.toml exe setting, not a cabal field (zinc-9po.5)
    , compFromCabal = True -- cabal-sourced: trust the .cabal module list even when empty (Configure deps), never auto-discover (zinc-iaj.1)
    }
  where
    pkgconfigNames b = [unPkgconfigName n | PkgconfigDependency n _ <- pkgconfigDepends b]

-- | The declared @build-type@ of a @.cabal@ (e.g. @"Simple"@, @"Custom"@).
-- zinc only builds Simple-ish packages directly; Custom (Setup.hs) deps are
-- rejected with a clear message by the build pipeline.
cabalBuildType :: String -> Either String String
cabalBuildType src =
  case snd (runParseResult (parseGenericPackageDescription (BS.pack src))) of
    Left err  -> Left ("cabal parse error: " ++ show err)
    Right gpd -> Right (prettyShow (buildType (packageDescription gpd)))

-- | The declared @version@ of a @.cabal@ (e.g. @"2.3.6"@). Used so a fetched
-- dependency registers with its real version — keeping zinc's synthesized
-- @cabal_macros.h@ in step with GHC's own @VERSION_<pkg>@/@MIN_VERSION_<pkg>@
-- macros (a mismatch is a CPP redefinition error).
cabalVersion :: String -> Either String String
cabalVersion src =
  case snd (runParseResult (parseGenericPackageDescription (BS.pack src))) of
    Left err  -> Left ("cabal parse error: " ++ show err)
    Right gpd -> Right (prettyShow (pkgVersion (package (packageDescription gpd))))
