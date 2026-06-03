-- | Opt-2 @.cabal@ reader (spec §3): parse a Hackage @.cabal@ file with the
-- @Cabal@ library (as a parser only — never its builder) and derive the same
-- 'Component' model zinc-native @[build]@ blocks produce. This lets zinc build
-- arbitrary upstream packages pulled from git without zinc-ifying them.
module Zinc.Cabal
  ( parseCabalComponents
  ) where

import qualified Data.ByteString.Char8 as BS
import Distribution.Compiler (CompilerFlavor (GHC))
import Distribution.PackageDescription
  ( BuildInfo
  , Executable (buildInfo, modulePath)
  , GenericPackageDescription
  , Library
  , TestSuite (testBuildInfo, testInterface)
  , TestSuiteInterface (TestSuiteExeV10)
  , condExecutables
  , condLibrary
  , condTestSuites
  , defaultExtensions
  , exposedModules
  , hcOptions
  , hsSourceDirs
  , libBuildInfo
  , otherModules
  , targetBuildDepends
  )
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Pretty (prettyShow)
import Distribution.Types.CondTree (condTreeData)
import Distribution.Types.Dependency (depPkgName)
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
import Zinc.Manifest (Component (..), ComponentKind (..))

-- | Derive zinc 'Component's from @.cabal@ source: the library (named @"lib"@),
-- each executable, and each test-suite.
parseCabalComponents :: String -> Either String [Component]
parseCabalComponents src =
  case snd (runParseResult (parseGenericPackageDescription (BS.pack src))) of
    Left err -> Left ("cabal parse error: " ++ show err)
    Right gpd -> Right (libraryComponent gpd ++ executableComponents gpd ++ testComponents gpd)

libraryComponent :: GenericPackageDescription -> [Component]
libraryComponent gpd =
  case condLibrary gpd of
    Nothing -> []
    Just ct -> [fromLibrary (condTreeData ct)]

executableComponents :: GenericPackageDescription -> [Component]
executableComponents gpd =
  [ (fromBuildInfo Executable (unUnqualComponentName n) (buildInfo exe))
      { compMain = Just (modulePath exe) }
  | (n, ct) <- condExecutables gpd
  , let exe = condTreeData ct
  ]

testComponents :: GenericPackageDescription -> [Component]
testComponents gpd =
  [ (fromBuildInfo TestSuite (unUnqualComponentName n) (testBuildInfo ts))
      { compMain = testMain ts }
  | (n, ct) <- condTestSuites gpd
  , let ts = condTreeData ct
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
    , compSystemLibs = []
    }
