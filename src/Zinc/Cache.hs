-- | Content-addressed artifact cache keying (spec §7, §8): a built package is
-- identified by a hash of everything that affects its output, so a dependency
-- compiles once per machine and branch-switching reuses builds.
module Zinc.Cache
  ( BuildKey (..)
  , confCodegenEpoch
  , buildCacheKey
  , buildCacheKeyFor
  , cacheKeyPayload
  , storePkgPath
  , storeConfPath
  , cacheHit
  , writeCachedConf
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Digest.Pure.SHA (sha256, showDigest)
import Data.List (intercalate, sort)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import Zinc.Target (Target (..), targetTriple)

-- | Everything that determines a built package's output.
data BuildKey = BuildKey
  { bkName       :: String   -- ^ package name — distinguishes monorepo siblings that share a commit (zinc-qln)
  , bkRev        :: String   -- ^ resolved source commit
  , bkGhcVersion :: String
  , bkDepUnitIds :: [String] -- ^ dependency unit-ids (order-insensitive)
  , bkOptions    :: [String] -- ^ ghc-options + extensions (order-insensitive)
  , bkFlags      :: [(String, Bool)] -- ^ manual cabal flag assignments (zinc-iaj.2; order-insensitive)
  }
  deriving (Eq, Show)

-- | The conf-generation epoch: a manual version of zinc's @package.conf@
-- derivation (what 'Zinc.Build.renderConf' + the 'PackageConf' it's fed
-- contain). It is folded into every cache key, so bumping it invalidates ALL
-- cached store entries.
--
-- WHY this exists (zinc-bie): the rest of 'BuildKey' describes the SOURCE and
-- how it's invoked (rev, deps, ghc, options, flags) — NOT zinc's logic for
-- turning that source into a @.conf@. So when a zinc change makes the generated
-- conf carry new content for an UNCHANGED source — reexports (zinc-0k7),
-- extra-libraries / system-libs (zinc-389) — the key was unchanged and the
-- stale cached conf was silently re-registered, forcing a manual store purge.
-- The epoch is the missing input.
--
-- BUMP THIS whenever a change alters the generated conf for an unchanged source
-- (a new 'PackageConf' field, a 'renderConf' format change, a change to how a
-- field is derived from the .cabal/.toml). Coarse by design — a bump also
-- re-fetches + recompiles the artifact, not just the cheap conf; that one-time
-- cost on upgrade buys correctness without a per-build source re-parse (the
-- finer split is a future optimization, see zinc-bie option 2).
confCodegenEpoch :: String
confCodegenEpoch = "4" -- bumped: Paths_<pkg> now emits git version metadata (zinc-3x4)

-- | A stable cache key (sha256 hex) over the build inputs. The package name is
-- part of the key: a monorepo's sub-packages share one commit (and often the
-- same deps/options), so without it @effectful@ and @effectful-core@ would
-- collide on one store entry and the second would reuse the first's artifact
-- (zinc-qln). Native target (see 'buildCacheKeyFor').
buildCacheKey :: BuildKey -> String
buildCacheKey = buildCacheKeyFor Native

-- | As 'buildCacheKey', but target-aware (zinc-9po.2): a non-native target adds
-- its triple so wasm artifacts never collide with native in the store —
-- @~\/.zinc\/store@ holds both. @Native@ appends nothing, so its key (and every
-- existing native artifact) is byte-identical: no cache invalidation.
buildCacheKeyFor :: Target -> BuildKey -> String
buildCacheKeyFor target bk = showDigest (sha256 (BL8.pack (cacheKeyPayload target bk)))

-- | The exact pre-hash material a cache key is computed from (the @\\0@-joined
-- inputs). Exposed so the keyed inputs — including the conf-codegen epoch
-- (zinc-bie) — are inspectable/testable without recomputing the digest.
cacheKeyPayload :: Target -> BuildKey -> String
cacheKeyPayload target bk =
  intercalate "\0" $
    [ bkName bk
    , bkRev bk
    , bkGhcVersion bk
    , intercalate "," (sort (bkDepUnitIds bk))
    , intercalate "," (sort (bkOptions bk))
    , -- Manual cabal flags (zinc-iaj.2): a flag can toggle build-depends /
      -- ghc-options (e.g. postgresql-libpq's @use-pkg-config@), so flipping
      -- one must serve a fresh artifact, not the one built with the old
      -- assignment. Rendered deterministically (sorted name=bool) so order
      -- never changes the key.
      intercalate "," (sort ["flag:" ++ n ++ "=" ++ boolStr v | (n, v) <- bkFlags bk])
    , -- The conf-generation epoch (zinc-bie): bumping it invalidates every
      -- cached entry so a conf-codegen change can't silently reuse a stale
      -- package.conf for an unchanged source.
      "codegen:" ++ confCodegenEpoch
    ]
      ++ case target of
        Native -> []
        _      -> ["target:" ++ targetTriple target]
  where
    boolStr b = if b then "true" else "false"

-- | Location of a cached built package within the store.
storePkgPath :: FilePath -> String -> FilePath
storePkgPath root key = root </> "pkg" </> key

-- | The cached package description (.conf) location for a key.
storeConfPath :: FilePath -> String -> FilePath
storeConfPath root key = storePkgPath root key </> "package.conf"

-- | Is there a cached build for this key?
cacheHit :: FilePath -> String -> IO Bool
cacheHit root key = doesDirectoryExist (storePkgPath root key)

-- | Record a build in the cache by writing its .conf into the store entry.
-- (The compiled .hi/.a are placed alongside by the build driver.)
writeCachedConf :: FilePath -> String -> String -> IO ()
writeCachedConf root key confText = do
  createDirectoryIfMissing True (storePkgPath root key)
  writeFile (storeConfPath root key) confText
