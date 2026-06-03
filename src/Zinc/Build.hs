-- | The GHC build driver (spec §7): zinc drives @ghc --make@ directly rather
-- than executing Cabal's builder. This module constructs the invocation.
module Zinc.Build
  ( GhcInvocation (..)
  , ghcMakeArgs
  ) where

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
