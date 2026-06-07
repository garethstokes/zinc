-- | The real git-backed manifest fetch for the resolver: clone a dependency
-- at its ref and parse its @[dependencies]@/@[registry]@ into a 'DepManifest'.
-- This is the production implementation of the fetch function that
-- "Zinc.Resolve".'Zinc.Resolve.resolve' takes as a parameter.
module Zinc.Fetch
  ( gitFetchManifest
  , resolveRef
  , preferHackageForLatest
  , chooseLatestRef
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
import Zinc.Hackage (fetchHackageTarball, hackageCabal, hackageLatestVersion)
import Zinc.Manifest (Component (compDepends, compKind), ComponentKind (Library), Dependency (..), Ref (..), parseDependencies)
import Zinc.Resolve (DepManifest (..))
import Zinc.Version (newestTagFor, newestVersionFor, parseVersion)

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

-- | The pure prefer-Hackage policy for a @Latest@ ref (zinc-ngd). Given the
-- repo's newest release-tag version and Hackage's latest version (+ its raw
-- string), choose the ref to resolve to. Prefer the Hackage release as a
-- 'Vendored' pin when it is STRICTLY NEWER than the newest git tag — many
-- widely-used packages' newest tag predates the current toolchain (GHC 9.6 /
-- mtl 2.3) and fails to build, while Hackage carries a newer compatible release
-- (the sdist also ships a generated .cabal + pre-run configure). Never a
-- downgrade: if Hackage is absent or not newer than the newest tag, keep
-- 'Latest' (git), so a repo ahead of Hackage is unaffected.
chooseLatestRef :: Maybe [Int] -> Maybe ([Int], String) -> Ref
chooseLatestRef mtag mhackage = case mhackage of
  Nothing -> Latest
  Just (hv, hs)
    | maybe True (hv >) mtag -> Vendored hs -- Hackage newer, or no usable git tag
    | otherwise              -> Latest

-- | Resolve a @Latest@ ref to a concrete ref, preferring a newer Hackage release
-- over a stale newest git tag (zinc-ngd via 'chooseLatestRef'). Consults Hackage
-- for the latest version and the repo for its newest release tag; non-@Latest@
-- refs are returned unchanged. Best-effort: any lookup miss falls back to
-- 'Latest' (git), preserving the prior behavior.
preferHackageForLatest :: String -> String -> Ref -> IO Ref
preferHackageForLatest name repo Latest = do
  mhs <- hackageLatestVersion name
  case (\hs -> (,) hs <$> parseVersion hs) =<< mhs of
    Nothing -> pure Latest -- no Hackage release (or unparseable): keep git
    Just (hs, hv) -> do
      let (base, msubdir) = splitRepoSubdir repo
      tags <- listTags base
      let mtag = either (const Nothing) (newestVersionFor (Just name) (isJust msubdir)) tags
      pure (chooseLatestRef mtag (Just (hv, hs)))
preferHackageForLatest _ _ ref = pure ref
