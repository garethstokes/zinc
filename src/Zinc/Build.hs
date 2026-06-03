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
  ) where

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeExtension, (-<.>), (</>))
import System.Process (readProcessWithExitCode)

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
  createDirectoryIfMissing True (takeDirectory db)
  exists <- doesDirectoryExist db
  initResult <- if exists then pure (Right ()) else runUnit "ghc-pkg" ["init", db]
  case initResult of
    Left err -> pure (Left err)
    Right () -> do
      let confFile = takeDirectory db </> "register.conf"
      writeFile confFile confText
      runUnit "ghc-pkg" ["--package-db", db, "register", "--force", confFile]

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
