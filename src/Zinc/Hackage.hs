-- | Repo discovery from Hackage (spec §3, §9): when @zinc add@ hits a package
-- with no @[registry]@ entry, seed one from the package's @.cabal@
-- @source-repository@ metadata on Hackage. Hackage is consulted only here, at
-- add time — never at build time.
module Zinc.Hackage
  ( sourceRepoOf
  , hackageCabalUrl
  , hackageSourceRepo
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (listToMaybe)
import Data.List (stripPrefix)
import Distribution.PackageDescription (packageDescription, sourceRepos)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Types.SourceRepo (RepoKind (RepoHead), SourceRepo (repoKind, repoLocation, repoSubdir))
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | Extract a git repo URL from @.cabal@ source: the @source-repository head@
-- location, falling back to any declared repo. A @subdir@ (monorepo packages
-- like prettyprinter) is appended as zinc's @url#subdir@ spec so the package is
-- read from the right directory. 'Nothing' if none / unparseable.
sourceRepoOf :: String -> Maybe String
sourceRepoOf src =
  case snd (runParseResult (parseGenericPackageDescription (BS.pack src))) of
    Left _ -> Nothing
    Right gpd -> listToMaybe (heads ++ others)
      where
        repos = sourceRepos (packageDescription gpd)
        heads = [withSub r (normalize loc) | r <- repos, repoKind r == RepoHead, Just loc <- [repoLocation r]]
        others = [withSub r (normalize loc) | r <- repos, Just loc <- [repoLocation r]]
        withSub r loc = case repoSubdir r of
          Just s | not (null s) && s /= "." -> loc ++ "#" ++ s
          _ -> loc
        -- git:// is deprecated (GitHub no longer serves it); use https.
        normalize u = maybe u ("https://" ++) (stripPrefix "git://" u)

-- | URL of a package's @.cabal@ on Hackage.
hackageCabalUrl :: String -> String
hackageCabalUrl pkg = "https://hackage.haskell.org/package/" ++ pkg ++ "/" ++ pkg ++ ".cabal"

-- | Fetch a package's @.cabal@ from Hackage and extract its source repo.
-- @Right Nothing@ means fetched but no repo declared.
hackageSourceRepo :: String -> IO (Either String (Maybe String))
hackageSourceRepo pkg = do
  (code, out, err) <- readProcessWithExitCode "curl" ["-fsSL", hackageCabalUrl pkg] ""
  pure $ case code of
    ExitSuccess   -> Right (sourceRepoOf out)
    ExitFailure _ -> Left ("fetch " ++ pkg ++ " from Hackage: " ++ err)
