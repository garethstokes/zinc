-- | Repo discovery from Hackage (spec §3, §9): when @zinc add@ hits a package
-- with no @[registry]@ entry, seed one from the package's @.cabal@
-- @source-repository@ metadata on Hackage. Hackage is consulted only here, at
-- add time — never at build time.
module Zinc.Hackage
  ( sourceRepoOf
  , hackageCabalUrl
  , hackageSourceRepo
  , hackageTarballUrl
  , fetchHackageTarball
  , hackageLatestVersion
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Char (toLower)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (listToMaybe)
import Data.List (isInfixOf, stripPrefix)
import Distribution.PackageDescription (homepage, packageDescription, sourceRepos)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Types.SourceRepo (RepoKind (RepoHead), SourceRepo (repoKind, repoLocation, repoSubdir))
import Distribution.Utils.ShortText (fromShortText)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

-- | Extract a git repo URL from @.cabal@ source: the @source-repository head@
-- location, falling back to any declared repo, then to the @homepage@ when it
-- points at a known git host (some packages — e.g. @strict@ — omit
-- @source-repository@ but set @homepage@ to their GitHub repo). A @subdir@
-- (monorepo packages like prettyprinter) is appended as zinc's @url#subdir@
-- spec so the package is read from the right directory. 'Nothing' if none /
-- unparseable.
sourceRepoOf :: String -> Maybe String
sourceRepoOf src =
  case snd (runParseResult (parseGenericPackageDescription (BS.pack src))) of
    Left _ -> Nothing
    Right gpd -> listToMaybe (heads ++ others) <|> homepageRepo
      where
        pd = packageDescription gpd
        repos = sourceRepos pd
        heads = [withSub r (normalize loc) | r <- repos, repoKind r == RepoHead, Just loc <- [repoLocation r]]
        others = [withSub r (normalize loc) | r <- repos, Just loc <- [repoLocation r]]
        withSub r loc = case repoSubdir r of
          Just s | not (null s) && s /= "." -> loc ++ "#" ++ s
          _ -> loc
        -- git:// is deprecated (GitHub no longer serves it); use https.
        normalize u = maybe u ("https://" ++) (stripPrefix "git://" u)
        -- Fallback: a homepage on a known git host is almost always the repo.
        homepageRepo
          | any (`isInfixOf` hp) gitHosts = Just (dropTrailingSlash hp)
          | otherwise                     = Nothing
        hp = fromShortText (homepage pd)
        gitHosts = ["github.com", "gitlab.com", "codeberg.org", "bitbucket.org", "git.sr.ht"]
        dropTrailingSlash s = if not (null s) && last s == '/' then init s else s

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

-- | The latest version of a package on Hackage, read from its preferred
-- @.cabal@ (the @\<pkg\>\/\<pkg\>.cabal@ endpoint serves the newest release).
-- 'Nothing' if the fetch fails or the version is unparseable (zinc-90j.1).
hackageLatestVersion :: String -> IO (Maybe String)
hackageLatestVersion pkg = do
  (code, out, _) <- readProcessWithExitCode "curl" ["-fsSL", hackageCabalUrl pkg] ""
  pure $ case code of
    ExitSuccess   -> versionOf out
    ExitFailure _ -> Nothing
  where
    versionOf src =
      listToMaybe
        [ dropWhile (== ' ') (drop 1 rest)
        | l <- lines src
        , let (key, rest) = break (== ':') l
        , map toLower (dropWhile (== ' ') key) == "version"
        , not (null rest)
        ]

-- | URL of a package's sdist tarball (@\<name\>-\<version\>.tar.gz@) on Hackage —
-- the vendoring source (b1z, design s2). Pinned by sha256 at vendor time;
-- consulted only at the explicit @vendor@/@add@ step, never to /resolve/ a
-- version (the version is supplied, not solved).
hackageTarballUrl :: String -> String -> String
hackageTarballUrl name version =
  "https://hackage.haskell.org/package/" ++ nv ++ "/" ++ nv ++ ".tar.gz"
  where nv = name ++ "-" ++ version

-- | Fetch + unpack @\<name\>-\<version\>@'s Hackage sdist tarball into @dest@,
-- which becomes the package directory (the tarball's top-level
-- @\<name\>-\<version\>/@ wrapper is stripped). Any stale @dest@ is replaced so
-- the unpacked tree is exactly the tarball's content (its hash must match the
-- lock's pin). Returns the unpacked @dest@ on success.
fetchHackageTarball :: String -> String -> FilePath -> IO (Either String FilePath)
fetchHackageTarball name version dest = do
  stale <- doesDirectoryExist dest
  when stale (removeDirectoryRecursive dest)
  createDirectoryIfMissing True dest
  let url = hackageTarballUrl name version
      tgz = dest </> ".sdist.tar.gz"
  (dlCode, _, dlErr) <- readProcessWithExitCode "curl" ["-fsSL", "-o", tgz, url] ""
  case dlCode of
    ExitFailure _ -> pure (Left ("fetch " ++ name ++ "-" ++ version ++ " tarball: " ++ dlErr))
    ExitSuccess -> do
      (xCode, _, xErr) <- readProcessWithExitCode "tar" ["-xzf", tgz, "-C", dest, "--strip-components=1"] ""
      removeFile tgz
      pure $ case xCode of
        ExitSuccess   -> Right dest
        ExitFailure _ -> Left ("unpack " ++ name ++ "-" ++ version ++ " tarball: " ++ xErr)
