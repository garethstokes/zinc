-- | The real git-backed manifest fetch for the resolver: clone a dependency
-- at its ref and parse its @[dependencies]@/@[registry]@ into a 'DepManifest'.
-- This is the production implementation of the fetch function that
-- "Zinc.Resolve".'Zinc.Resolve.resolve' takes as a parameter.
module Zinc.Fetch
  ( gitFetchManifest
  , resolveRef
  , packageDirIn
  , namedCabal
  , isHpackOnly
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.List (find, nub)
import Data.Maybe (isJust, listToMaybe)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive)
import System.FilePath (takeExtension, (</>))
import Zinc.Cabal (cabalBuildType, parseCabalComponentsForGhc)
import Zinc.Diagnostic (ZincError (BuildTypeCustom))
import Zinc.Except (failWith, failWithError, liftEither, liftIO, orFail, orFailE, runResult)
import Zinc.Git (cloneAt, listTags, splitRepoSubdir)
import Zinc.Hackage (fetchHackageTarball, hackageCabal)
import Zinc.Manifest (Component (compDepends, compKind), ComponentKind (Library), Dependency (..), Ref (..), parseDependencies)
import Zinc.Resolve (DepManifest (..))
import Zinc.Version (newestTagFor)

-- | Fetch a dependency's manifest: bring @repo@ at @ref@ into the store and
-- read its dependency list. A zinc-native dep declares @[dependencies]@ +
-- @[registry]@ in its @zinc.toml@; a real upstream (only a @.cabal@) has its
-- deps derived from the cabal file via the Opt-2 reader (their repos then come
-- from the root workspace registry). A vendored pin ('Vendored') is fetched as
-- a Hackage sdist tarball instead of a git clone (b1z); its deps come from the
-- unpacked @.cabal@ the same way. @ghcVersion@ resolves @impl(ghc)@
-- conditionals. Matches the fetch signature 'Zinc.Resolve.resolve' expects.
gitFetchManifest :: FilePath -> String -> String -> String -> Ref -> IO (Either ZincError DepManifest)
gitFetchManifest storeRoot ghcVersion name repo ref = runResult $ do
  let dest = storeRoot </> "checkout" </> name
  pkgDir <- case ref of
    Vendored ver ->
      orFail (first (("fetch " ++ name ++ ": ") ++) <$> fetchHackageTarball name ver dest)
    _ -> do
      refStr <- orFail (first ((name ++ ": ") ++) <$> resolveRef name repo ref)
      liftIO $ do
        stale <- doesDirectoryExist dest
        when stale (removeDirectoryRecursive dest)
      _ <- orFail (first (("fetch " ++ name ++ ": ") ++) <$> cloneAt repo refStr dest)
      liftIO (packageDirIn dest repo name)
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
  let cabals = filter ((== ".cabal") . takeExtension) entries
  -- Pick THIS package's cabal (<name>.cabal) when present: a monorepo checkout
  -- can hold several, and the first is not necessarily the dep being resolved —
  -- reading a sibling's cabal leaks its foreign deps into the closure.
  case namedCabal name cabals <|> listToMaybe cabals of
    Just cab -> do
      src <- liftIO (readFile (pkgDir </> cab))
      depsFromCabal src
    Nothing -> do
      -- No committed .cabal. If it's an hpack package (package.yaml), read its
      -- deps from the Hackage-published (generated) .cabal so the closure walk
      -- can continue; the source itself is vendored from the sdist at freeze
      -- time (zinc-pzu). Otherwise it's genuinely unbuildable.
      hpack <- liftIO (isHpackOnly pkgDir)
      if not hpack
        then failWith (name ++ ": no zinc.toml or .cabal in the checkout")
        else do
          src <- orFail (first ((name ++ ": ") ++) <$> hackageCabal name)
          depsFromCabal src
  where
    depsFromCabal src = do
      when (cabalBuildType src == Right "Custom") $
        failWithError (BuildTypeCustom name)
      comps <- liftEither (first ((name ++ ": ") ++) (parseCabalComponentsForGhc ghcVersion src))
      let libDeps = nub (concat [compDepends c | c <- comps, compKind c == Library])
      pure (DepManifest [Dependency d Latest Nothing [] | d <- libDeps] [])

-- | An hpack package: a @package.yaml@ but no committed @.cabal@. zinc reads
-- @.cabal@, not @package.yaml@, so such a checkout can't be built directly — its
-- generated cabal comes from the Hackage sdist (vendored at freeze; zinc-pzu).
isHpackOnly :: FilePath -> IO Bool
isHpackOnly pkgDir = do
  yaml <- doesFileExist (pkgDir </> "package.yaml")
  if not yaml
    then pure False
    else do
      there <- doesDirectoryExist pkgDir
      cabals <- if there then filter ((== ".cabal") . takeExtension) <$> listDirectory pkgDir else pure []
      pure (null cabals)

-- | The @\<name\>.cabal@ in a list of cabal filenames (case-insensitively) — the
-- file Cabal names after the package, used to pick the right one out of a
-- multi-package monorepo checkout.
namedCabal :: String -> [FilePath] -> Maybe FilePath
namedCabal name = find ((== lower (name ++ ".cabal")) . lower)
  where
    lower = map toLower

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
      -- Prefer the dir that holds THIS package's manifest (<name>.cabal /
      -- zinc.toml): the root, then a <name>/ subdir. Only when neither names
      -- this package do we fall back to any-manifest dir, then the root.
      named <- firstThatM hasNamedManifest cands
      case named of
        Just d  -> pure d
        Nothing -> do
          anyd <- firstThatM hasAnyManifest cands
          pure (maybe dest id anyd)
  where
    cands = [dest, dest </> name]
    hasNamedManifest d = do
      z <- doesFileExist (d </> "zinc.toml")
      if z
        then pure True
        else do
          there <- doesDirectoryExist d
          if there
            then maybe False (const True) . namedCabal name . filter ((== ".cabal") . takeExtension) <$> listDirectory d
            else pure False
    hasAnyManifest d = do
      z <- doesFileExist (d </> "zinc.toml")
      if z
        then pure True
        else do
          there <- doesDirectoryExist d
          if there
            then any ((== ".cabal") . takeExtension) <$> listDirectory d
            else pure False

-- | The first list element satisfying a monadic predicate.
firstThatM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
firstThatM _ [] = pure Nothing
firstThatM p (x : xs) = do
  ok <- p x
  if ok then pure (Just x) else firstThatM p xs

-- | The git checkout target for a ref. 'Latest' is resolved to the repo's
-- newest release tag.
resolveRef :: String -> String -> Ref -> IO (Either String String)
resolveRef _    _    (Tag t)      = pure (Right t)
resolveRef _    _    (Branch b)   = pure (Right b)
resolveRef _    _    (Rev r)      = pure (Right r)
resolveRef _    _    (Vendored v) = pure (Right v) -- not a git ref; the tarball path uses the version directly
resolveRef name repo Latest     = do
  let (base, msubdir) = splitRepoSubdir repo
  tags <- listTags base
  pure $ case tags of
    Left err -> Left err
    -- Scope Latest by the PACKAGE NAME. For a monorepo SUBDIR dep, package-
    -- prefixed tags (vector-stream-*) must win over a sibling's or a stale
    -- global tag. For a standalone repo (no #subdir, e.g. hashable), the bare
    -- v* tags ARE this package's releases, so they are considered alongside any
    -- scoped tag and the newest overall wins — never masked by a stale scoped
    -- tag like hashable-1.3.2.0 (zinc-myx).
    Right ts -> maybe (Left "no release tags found") Right (newestTagFor (Just name) (isJust msubdir) ts)
