-- | Map @.cabal@ C-library names (@extra-libraries@ / @pkgconfig-depends@) to
-- nixpkgs attribute names (spec §3, §6), so system deps discovered from a
-- package's @.cabal@ flow into the generated flake's @system-libs@.
module Zinc.SysLibs
  ( toNixpkgs
  , pkgconfigLinkName
  ) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

-- | Translate a C library name to a nixpkgs attr. 'Nothing' for libc-provided
-- libraries (no nixpkgs attr needed). Unknown names pass through unchanged —
-- many C libs share their nixpkgs attr name (e.g. @ncurses@, @gmp@).
toNixpkgs :: String -> Maybe String
toNixpkgs lib
  | lib `elem` libcProvided = Nothing
  | otherwise = Just (Map.findWithDefault lib lib knownAliases)
  where
    libcProvided = ["c", "m", "dl", "rt", "pthread", "stdc++", "gcc_s"]
    knownAliases =
      Map.fromList
        [ ("z", "zlib")
        , ("crypto", "openssl")
        , ("ssl", "openssl")
        , ("ffi", "libffi")
        , ("sqlite3", "sqlite")
        , -- libpq ships under pkgs.postgresql in nixpkgs (no pkgs.libpq/pq); both
          -- the pkgconfig module (libpq) and the C link name (pq) map to it (zinc-389).
          ("pq", "postgresql")
        , ("libpq", "postgresql")
        ]

-- | The C link name (@-l\<name\>@) for a @pkgconfig-depends@ MODULE name
-- (zinc-mmx). A pkgconfig module name is NOT generally the library link name:
-- @pkg-config --libs zlib@ is @-lz@ (the lib is @libz@), not @-lzlib@. Map the
-- known mismatches; otherwise fall back to stripping a leading @lib@
-- (@libpq@ -> @pq@), which is correct for the common @lib\<name\>@ convention.
-- (The robust general answer is @pkg-config --libs@ at build time — a future
-- refinement; this curated map mirrors 'toNixpkgs' and fixes the cases zinc hits.)
pkgconfigLinkName :: String -> String
pkgconfigLinkName m = Map.findWithDefault (fromMaybe m (stripPrefix "lib" m)) m known
  where
    known = Map.fromList [("zlib", "z")]
