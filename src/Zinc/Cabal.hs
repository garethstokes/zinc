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
  , cabalBuildType
  , cabalVersion
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.List (nub)
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
  , PackageDescription (executables, library, package, testSuites)
  , TestSuite (testBuildInfo, testInterface, testName)
  , TestSuiteInterface (TestSuiteExeV10)
  , buildType
  , defaultExtensions
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
import Distribution.System (buildPlatform)
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec (ComponentRequestedSpec))
import Distribution.Types.Dependency (depPkgName)
import Distribution.Types.PackageId (pkgVersion)
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.PkgconfigDependency (PkgconfigDependency (PkgconfigDependency))
import Distribution.Types.PkgconfigName (unPkgconfigName)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Version (mkVersion)
import Zinc.Manifest (Component (..), ComponentKind (..))
import Zinc.SysLibs (toNixpkgs)

-- | Derive zinc 'Component's from @.cabal@ source, resolving conditionals
-- against a recent default GHC. See 'parseCabalComponentsForGhc'.
parseCabalComponents :: String -> Either String [Component]
parseCabalComponents = parseCabalComponentsForGhc "9.6.5"

-- | Like 'parseCabalComponents', but resolve @impl(ghc ...)@ conditionals
-- against the given GHC version (e.g. the workspace's @ghc@).
parseCabalComponentsForGhc :: String -> String -> Either String [Component]
parseCabalComponentsForGhc ghcVersion src =
  case snd (runParseResult (parseGenericPackageDescription (BS.pack src))) of
    Left err -> Left ("cabal parse error: " ++ show err)
    Right gpd ->
      case finalizePD mempty (ComponentRequestedSpec True True) (const True) buildPlatform ghc [] gpd of
        Left missing -> Left ("cabal finalize error: unsatisfied " ++ show (map prettyShow missing))
        Right (pd, _flags) ->
          Right (libraryComponent pd ++ executableComponents pd ++ testComponents pd)
  where
    ghc = unknownCompilerInfo (CompilerId GHC (mkVersion (versionInts ghcVersion))) NoAbiTag

-- | Parse a dotted version string into integer components (e.g. @"9.6.5"@).
versionInts :: String -> [Int]
versionInts = map read . splitDots
  where
    splitDots s = case break (== '.') s of
      (a, [])    -> [a]
      (a, _ : r) -> a : splitDots r

libraryComponent :: PackageDescription -> [Component]
libraryComponent pd = maybe [] (\l -> [fromLibrary l]) (library pd)

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
  (fromBuildInfo Library "lib" (libBuildInfo lib))
    { compExposedModules = map prettyShow (exposedModules lib) }

-- | The fields common to every component, pulled from a 'BuildInfo'.
fromBuildInfo :: ComponentKind -> String -> BuildInfo -> Component
fromBuildInfo kind name bi =
  Component
    { compKind = kind
    , compName = name
    , compSourceDirs = map getSymbolicPath (hsSourceDirs bi)
    , compExposedModules = []
    , compOtherModules = map prettyShow (otherModules bi)
    , compMain = Nothing
    , compExtensions = map prettyShow (defaultExtensions bi)
    , compGhcOptions = hcOptions GHC bi
    , compDepends = map (unPackageName . depPkgName) (targetBuildDepends bi)
    , compSystemLibs = nub (mapMaybe toNixpkgs (extraLibs bi ++ pkgconfigNames bi))
    , compIncludeDirs = includeDirs bi
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
