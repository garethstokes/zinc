-- | Emit the @cabal_macros.h@ CPP header that Cabal normally autogenerates
-- (spec §3). Packages guard code on @MIN_VERSION_<dep>(x,y,z)@, so when zinc
-- drives ghc directly it must provide these macros. Format mirrors Cabal's.
module Zinc.Macros
  ( emitCabalMacros
  ) where

import Data.List (intercalate)

-- | Render @cabal_macros.h@ for a set of @(package name, version)@ pairs.
emitCabalMacros :: [(String, [Int])] -> String
emitCabalMacros = unlines . concatMap macros
  where
    macros (name, version) =
      [ "/* package " ++ name ++ "-" ++ verStr ++ " */"
      , "#ifndef VERSION_" ++ ident
      , "#define VERSION_" ++ ident ++ " \"" ++ verStr ++ "\""
      , "#endif"
      , "#ifndef MIN_VERSION_" ++ ident
      , "#define MIN_VERSION_" ++ ident ++ "(major1,major2,minor) (\\"
      , "  (major1) <  " ++ show v0 ++ " || \\"
      , "  (major1) == " ++ show v0 ++ " && (major2) <  " ++ show v1 ++ " || \\"
      , "  (major1) == " ++ show v0 ++ " && (major2) == " ++ show v1 ++ " && (minor) <= " ++ show v2 ++ ")"
      , "#endif"
      ]
      where
        ident = map dashToUnderscore name
        verStr = intercalate "." (map show version)
        (v0, v1, v2) = case version ++ [0, 0, 0] of
          (a : b : c : _) -> (a, b, c)
          _               -> (0, 0, 0)

    dashToUnderscore '-' = '_'
    dashToUnderscore c   = c
