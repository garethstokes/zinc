-- | Support for Cabal @build-type: Configure@ dependencies (zinc-bxw.2).
--
-- A @Configure@ package (e.g. @network@) ships an autoconf @configure.ac@ and
-- relies on a generated @./configure@ being run before the build, which writes
-- system-probed headers (@network@'s @include/HsNetworkConfig.h@) that its
-- @.hsc@ sources @#include@. Two facts make this awkward under zinc:
--
--   * the package's GIT checkout ships only @configure.ac@ — autoconf generates
--     the actual @configure@ script into the Hackage SDIST, not into git, and
--     autoconf is not in zinc's toolchain. So we obtain @configure@ from the
--     Hackage sdist tarball, which carries it pre-generated.
--   * the content-addressed source checkout is read-only/hash-pinned, so
--     @configure@ cannot write into it. We fetch the sdist into a fresh WRITABLE
--     dir under the package's output dir and run @configure@ there in place; the
--     generated headers land in that dir, which we then fold onto the build's
--     include path (the bxw.1 plumbing already threads include-dirs to hsc2hs,
--     cc, and ghc).
module Zinc.Configure
  ( configureComponent
  , configureIncludeDirs
  ) where

import Data.Bifunctor (first)
import System.Directory (doesFileExist, listDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeExtension, (</>))
import System.Process (CreateProcess (cwd), proc, readCreateProcessWithExitCode)

import Zinc.Cabal (cabalBuildType)
import Zinc.Hackage (fetchHackageTarball)
import Zinc.Manifest (Component (..))

-- | If @comp@'s package (rooted at @pkgDir@) is @build-type: Configure@, run its
-- @./configure@ (sourced from the Hackage sdist for @name-version@) and return
-- the component with the configure-generated include dir prepended to its
-- include-dirs. Otherwise the component is returned unchanged. Errors (sdist
-- fetch, missing @configure@, a failing run) surface as @Left@.
--
-- The generated include dir is ABSOLUTE. Downstream, a component's include-dirs
-- are joined with the package dir (@pkgDir </> d@), and @System.FilePath.</>@
-- keeps an absolute right operand verbatim, so an absolute entry threads through
-- the hsc2hs/cc/ghc include flags unchanged.
configureComponent :: String -> String -> FilePath -> FilePath -> Component -> IO (Either String Component)
configureComponent name version pkgDir outDir comp = do
  bt <- buildTypeOf pkgDir
  if bt /= Just "Configure"
    then pure (Right comp)
    else do
      gen <- configureGenerate name version outDir (compIncludeDirs comp)
      pure (fmap (\incs -> comp {compIncludeDirs = incs ++ compIncludeDirs comp}) gen)

-- | The package's declared @build-type@, read from the @.cabal@ in @pkgDir@.
-- @Nothing@ for a zinc-native dep (no @.cabal@) or an unparseable manifest.
buildTypeOf :: FilePath -> IO (Maybe String)
buildTypeOf pkgDir = do
  entries <- listDirectory pkgDir
  case filter ((== ".cabal") . takeExtension) entries of
    (cabal : _) -> do
      src <- readFile (pkgDir </> cabal)
      pure (either (const Nothing) Just (cabalBuildType src))
    [] -> pure Nothing

-- | Fetch the package's Hackage sdist into a fresh writable dir under @outDir@,
-- run its @./configure@ in place, and return the absolute include dirs holding
-- the generated headers. These cover the package's OWN @include-dirs@ resolved
-- against the CONFIGURED sdist copy (@includeDirs@) — so a header configure
-- writes into e.g. @cbits/config.h@ (unix-time, zinc-hz2) is on the C include
-- path, not just the conventional @include/@ (network's @HsNetworkConfig.h@).
-- The sdist-copy dirs lead so the generated header shadows the git checkout's
-- @config.h.in@-only copy.
configureGenerate :: String -> String -> FilePath -> [FilePath] -> IO (Either String [FilePath])
configureGenerate name version outDir includeDirs = do
  let sdistDir = outDir </> "zinc-configure"
  fetched <- fetchHackageTarball name version sdistDir
  case first ((name ++ ": fetch sdist for configure: ") ++) fetched of
    Left e   -> pure (Left e)
    Right _  -> do
      let configure = sdistDir </> "configure"
      have <- doesFileExist configure
      if not have
        then pure (Left (name ++ ": build-type Configure but the sdist ships no ./configure"))
        else do
          run <- runConfigure name sdistDir
          pure (fmap (const (configureIncludeDirs sdistDir includeDirs)) run)

-- | The include dirs to fold onto the build after @configure@ (zinc-hz2): the
-- package's OWN @include-dirs@ resolved against the configured sdist copy lead —
-- so a header configure writes into one of them (e.g. unix-time's
-- @cbits/config.h@) shadows the git checkout's @config.h.in@-only copy — then
-- the conventional @include/@ and the sdist root (network's @HsNetworkConfig.h@).
-- @sdistDir@ is absolute, so each entry stays absolute and threads verbatim
-- through the downstream @pkgDir </> d@ join.
configureIncludeDirs :: FilePath -> [FilePath] -> [FilePath]
configureIncludeDirs sdistDir includeDirs =
  [sdistDir </> d | d <- includeDirs] ++ [sdistDir </> "include", sdistDir]

-- | Run @sh ./configure@ in @dir@ (the writable sdist copy). Out-of-tree VPATH
-- is unnecessary since the copy is writable, so the generated headers land in
-- @dir@. Invoked via @sh@ so a missing exec bit on the unpacked script is moot.
runConfigure :: String -> FilePath -> IO (Either String ())
runConfigure name dir = do
  (code, out, err) <-
    readCreateProcessWithExitCode (proc "sh" ["configure"]) {cwd = Just dir} ""
  pure $ case code of
    ExitSuccess -> Right ()
    _           -> Left (name ++ ": configure failed:\n" ++ (if null err then out else err))
