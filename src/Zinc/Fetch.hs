-- | The real git-backed manifest fetch for the resolver: clone a dependency
-- at its ref and parse its @[dependencies]@/@[registry]@ into a 'DepManifest'.
-- This is the production implementation of the fetch function that
-- "Zinc.Resolve".'Zinc.Resolve.resolve' takes as a parameter.
module Zinc.Fetch
  ( gitFetchManifest
  , resolveRef
  , packageDirIn
  ) where

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.List (nub)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive)
import System.FilePath (takeExtension, (</>))
import Zinc.Cabal (cabalBuildType, parseCabalComponentsForGhc)
import Zinc.Diagnostic (ZincError (BuildTypeCustom))
import Zinc.Except (failWith, failWithError, liftEither, liftIO, orFail, orFailE, runResult)
import Zinc.Git (cloneAt, listTags, splitRepoSubdir)
import Zinc.Manifest (Component (compDepends, compKind), ComponentKind (Library), Dependency (..), Ref (..), parseDependencies)
import Zinc.Resolve (DepManifest (..))
import Zinc.Version (newestTagFor)

-- | Fetch a dependency's manifest: clone @repo@ at @ref@ into the store and
-- read its dependency list. A zinc-native dep declares @[dependencies]@ +
-- @[registry]@ in its @zinc.toml@; a real upstream (only a @.cabal@) has its
-- deps derived from the cabal file via the Opt-2 reader (their repos then come
-- from the root workspace registry). @ghcVersion@ resolves @impl(ghc)@
-- conditionals. Matches the fetch signature 'Zinc.Resolve.resolve' expects.
gitFetchManifest :: FilePath -> String -> String -> String -> Ref -> IO (Either ZincError DepManifest)
gitFetchManifest storeRoot ghcVersion name repo ref = runResult $ do
  refStr <- orFail (first ((name ++ ": ") ++) <$> resolveRef name repo ref)
  let dest = storeRoot </> "checkout" </> name
  liftIO $ do
    stale <- doesDirectoryExist dest
    when stale (removeDirectoryRecursive dest)
  _ <- orFail (first (("fetch " ++ name ++ ": ") ++) <$> cloneAt repo refStr dest)
  pkgDir <- liftIO (packageDirIn dest repo name)
  hasZinc <- liftIO (doesFileExist (pkgDir </> "zinc.toml"))
  if hasZinc
    then do
      src <- liftIO (readFile (pkgDir </> "zinc.toml"))
      (deps, reg) <- liftEither (first ((name ++ ": ") ++) (parseDependencies src))
      pure (DepManifest deps reg)
    else orFailE (cabalManifest name ghcVersion pkgDir)

-- | Derive a 'DepManifest' for a real upstream from its @.cabal@: the library
-- component's @build-depends@ become dependencies pinned to @Latest@ (their
-- repos are supplied by the root workspace registry). No own registry.
cabalManifest :: String -> String -> FilePath -> IO (Either ZincError DepManifest)
cabalManifest name ghcVersion pkgDir = runResult $ do
  entries <- liftIO (listDirectory pkgDir)
  case filter ((== ".cabal") . takeExtension) entries of
    [] -> failWith (name ++ ": no zinc.toml or .cabal in the checkout")
    (cab : _) -> do
      src <- liftIO (readFile (pkgDir </> cab))
      when (cabalBuildType src == Right "Custom") $
        failWithError (BuildTypeCustom name)
      comps <- liftEither (first ((name ++ ": ") ++) (parseCabalComponentsForGhc ghcVersion src))
      let libDeps = nub (concat [compDepends c | c <- comps, compKind c == Library])
      pure (DepManifest [Dependency d Latest Nothing [] | d <- libDeps] [])

-- | Locate a package's manifest directory inside a fetched checkout. An
-- explicit @url#subdir@ wins; otherwise the repo root if it holds a manifest;
-- otherwise a @\<name\>/@ subdir — metadata-poor monorepos (e.g. @strict@,
-- @strict-base-types@ …) ship no @source-repository@ subdir hint, but each
-- package lives in a directory named after it; else fall back to the root and
-- let the read fail with a clear message. Used identically at resolve time and
-- build time so the two agree on where a subdir package lives.
packageDirIn :: FilePath -> String -> String -> IO FilePath
packageDirIn dest repo name =
  case snd (splitRepoSubdir repo) of
    Just s  -> pure (dest </> s)
    Nothing -> do
      rootOk <- hasManifest dest
      if rootOk
        then pure dest
        else do
          let sub = dest </> name
          subOk <- hasManifest sub
          pure (if subOk then sub else dest)
  where
    hasManifest d = do
      z <- doesFileExist (d </> "zinc.toml")
      if z
        then pure True
        else do
          there <- doesDirectoryExist d
          if there
            then any ((== ".cabal") . takeExtension) <$> listDirectory d
            else pure False

-- | The git checkout target for a ref. 'Latest' is resolved to the repo's
-- newest release tag.
resolveRef :: String -> String -> Ref -> IO (Either String String)
resolveRef _    _    (Tag t)    = pure (Right t)
resolveRef _    _    (Branch b) = pure (Right b)
resolveRef _    _    (Rev r)    = pure (Right r)
resolveRef name repo Latest     = do
  tags <- listTags (fst (splitRepoSubdir repo))
  pure $ case tags of
    Left err -> Left err
    -- Scope Latest by the PACKAGE NAME: a monorepo carries package-prefixed
    -- tags (vector-stream-*, strict-*) that must be preferred over a sibling's
    -- or a stale global tag; a dedicated repo has none, so newestTagFor falls
    -- back to bare version tags (v1.5.1.0). Works whether or not the repo URL
    -- carried a #subdir (strict's was discovered from its homepage, no subdir).
    Right ts -> maybe (Left "no release tags found") Right (newestTagFor (Just name) ts)
