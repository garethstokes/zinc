-- | The real git-backed manifest fetch for the resolver: clone a dependency
-- at its ref and parse its @[dependencies]@/@[registry]@ into a 'DepManifest'.
-- This is the production implementation of the fetch function that
-- "Zinc.Resolve".'Zinc.Resolve.resolve' takes as a parameter.
module Zinc.Fetch
  ( gitFetchManifest
  , resolveRef
  , preferHackageForLatest
  , packageDirIn
  , namedCabal
  , isHpackOnly
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.List (find, nub)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Distribution.System (buildPlatform)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive)
import System.FilePath (takeExtension, (</>))
import Zinc.Cabal (cabalBuildType, parseCabalComponentsForPlatform)
import Zinc.Diagnostic (ZincError (BuildTypeCustom, NoReleaseTags, OtherError))
import Zinc.Except (failWith, failWithError, liftEither, liftIO, orFail, orFailE, runResult)
import Zinc.Git (cloneAt, listTags, splitRepoSubdir)
import Zinc.Hackage (fetchHackageTarball, hackageCabal, hackagePreferredVersion)
import Zinc.Manifest (Component (compDepends, compKind), ComponentKind (Library), Dependency (..), Ref (..), parseDependencies)
import Zinc.Resolve (DepManifest (..))
import Zinc.Version (newestTagFor, parseVersion)

-- | Fetch a dependency's manifest: bring @repo@ at @ref@ into the store and
-- read its dependency list. A zinc-native dep declares @[dependencies]@ +
-- @[registry]@ in its @zinc.toml@; a real upstream (only a @.cabal@) has its
-- deps derived from the cabal file via the Opt-2 reader (their repos then come
-- from the root workspace registry). A vendored pin ('Vendored') is fetched as
-- a Hackage sdist tarball instead of a git clone (b1z); its deps come from the
-- unpacked @.cabal@ the same way. @ghcVersion@ resolves @impl(ghc)@
-- conditionals; @flagsMap@ carries each direct dep's manual cabal flags (keyed
-- by name) so the @build-depends@ closure a dep contributes is finalized with
-- the SAME flags the build will use — a flag that toggles @build-depends@ (e.g.
-- postgresql-libpq's @use-pkg-config@) must select the same provider at resolve
-- time as at build time, or the lock fetches the wrong dependency (zinc-iaj.2).
-- Matches the fetch signature 'Zinc.Resolve.resolve' expects.
gitFetchManifest :: FilePath -> String -> [(String, [(String, Bool)])] -> String -> String -> Ref -> IO (Either ZincError DepManifest)
gitFetchManifest storeRoot ghcVersion flagsMap name repo ref = runResult $ do
  let dest = storeRoot </> "checkout" </> name
  -- zinc-ngd: a Latest ref may resolve to a Hackage-vendored pin (newer than the
  -- newest git tag), so the closure walk reads the COMPATIBLE release's .cabal.
  ref' <- liftIO (preferHackageForLatest name repo ref)
  pkgDir <- case ref' of
    Vendored ver ->
      orFail (first (("fetch " ++ name ++ ": ") ++) <$> fetchHackageTarball name ver dest)
    _ -> do
      refStr <- orFailE (resolveRef name repo ref')
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
    else orFailE (cabalManifest name ghcVersion (fromMaybe [] (lookup name flagsMap)) pkgDir)

-- | Derive a 'DepManifest' for a real upstream from its @.cabal@: the library
-- component's @build-depends@ become dependencies pinned to @Latest@ (their
-- repos are supplied by the root workspace registry). No own registry.
cabalManifest :: String -> String -> [(String, Bool)] -> FilePath -> IO (Either ZincError DepManifest)
cabalManifest name ghcVersion flags pkgDir = runResult $ do
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
      -- Finalize with this dep's manual flags (zinc-iaj.2) on the host platform
      -- (resolve/freeze is native): a flag toggling @build-depends@ must select
      -- the same provider here as the build does, so the closure locks the dep
      -- that actually gets built.
      comps <- liftEither (first ((name ++ ": ") ++) (parseCabalComponentsForPlatform buildPlatform flags ghcVersion src))
      let libDeps = nub (concat [compDepends c | c <- comps, compKind c == Library])
      pure (DepManifest [Dependency d Latest Nothing [] [] | d <- libDeps] [])

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
    -- An explicit @url#subdir@ wins — UNLESS it no longer exists in the
    -- checkout. A package's @source-repository@ subdir hint can lag the repo
    -- layout (e.g. crypton-x509-store-1.9.0 still says @subdir: x509-store@, but
    -- the monorepo renamed it to @crypton-x509-store/@), so a stale hint must
    -- not hard-fail — fall back to <name>/ detection (zinc-y24).
    Just s  -> do
      there <- doesDirectoryExist (dest </> s)
      if there then pure (dest </> s) else byName
    Nothing -> byName
  where
    -- Prefer the dir that holds THIS package's manifest (<name>.cabal /
    -- zinc.toml): the root, then a <name>/ subdir. Only when neither names
    -- this package do we fall back to any-manifest dir, then the root.
    byName = do
      named <- firstThatM hasNamedManifest cands
      case named of
        Just d  -> pure d
        Nothing -> do
          anyd <- firstThatM hasAnyManifest cands
          pure (maybe dest id anyd)
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
resolveRef :: String -> String -> Ref -> IO (Either ZincError String)
resolveRef _    _    (Tag t)      = pure (Right t)
resolveRef _    _    (Branch b)   = pure (Right b)
resolveRef _    _    (Rev r)      = pure (Right r)
resolveRef _    _    (Vendored v) = pure (Right v) -- not a git ref; the tarball path uses the version directly
resolveRef name repo Latest     = do
  let (base, msubdir) = splitRepoSubdir repo
  tags <- listTags base
  pure $ case tags of
    Left err -> Left (OtherError (name ++ ": " ++ err))
    -- Scope Latest by the PACKAGE NAME. For a monorepo SUBDIR dep, package-
    -- prefixed tags (vector-stream-*) must win over a sibling's or a stale
    -- global tag. For a standalone repo (no #subdir, e.g. hashable), the bare
    -- v* tags ARE this package's releases, so they are considered alongside any
    -- scoped tag and the newest overall wins — never masked by a stale scoped
    -- tag like hashable-1.3.2.0 (zinc-myx). No tags at all → a typed,
    -- actionable ZINC_NO_RELEASE_TAGS rather than an opaque ZINC_ERROR (91n.4).
    Right ts -> maybe (Left (NoReleaseTags name repo)) Right (newestTagFor (Just name) (isJust msubdir) ts)

-- | Resolve a @Latest@ ref to a concrete ref (zinc-ngd, refined by zinc-22z and
-- zinc-ix4). For a package PUBLISHED on Hackage, prefer its newest
-- NON-DEPRECATED release ('hackagePreferredVersion') as a 'Vendored' pin — that
-- is the authoritative "latest" for a wildcard dependency.
--
-- zinc deliberately does NOT fall back to a newer-looking git TAG. A tag ahead
-- of Hackage is unpublished/dev work that may not even be this package's release:
-- a monorepo's bare tag is the whole repo's version (e.g. @mauke/data-default@
-- tags @v0.8.0.0@ while the published @data-default-class@ is @0.2.0.0@ — and
-- 0.8.0.0 dropped the @Data.Default.Class@ module consumers import, zinc-ix4),
-- and an absolute-latest can be deprecated/incompatible (network-uri 2.7.0.0,
-- zinc-22z). The published release is the stable, intended "latest".
--
-- A package NOT on Hackage keeps its git ref ('Latest'); non-@Latest@ refs are
-- returned unchanged. Best-effort: any lookup miss falls back to 'Latest'.
preferHackageForLatest :: String -> String -> Ref -> IO Ref
preferHackageForLatest name _repo Latest = do
  mhs <- hackagePreferredVersion name
  pure $ case mhs of
    Just hs | isJust (parseVersion hs) -> Vendored hs -- the published release IS the "latest"
    _                                  -> Latest -- not on Hackage: keep the git ref
preferHackageForLatest _ _ ref = pure ref
