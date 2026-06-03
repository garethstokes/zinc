-- | Map @.cabal@ C-library names (@extra-libraries@ / @pkgconfig-depends@) to
-- nixpkgs attribute names (spec §3, §6), so system deps discovered from a
-- package's @.cabal@ flow into the generated flake's @system-libs@.
module Zinc.SysLibs
  ( toNixpkgs
  ) where

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
        ]
