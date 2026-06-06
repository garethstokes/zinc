-- | Built-in per-package build quirks (zinc-8uh): extra ghc-options a known
-- upstream package needs to compile correctly, applied automatically so the
-- user doesn't have to carry them in their workspace as a @[dependencies.\<pkg\>]
-- ghc-options@ escape hatch.
--
-- Each entry is a known, vetted fix for a specific package. They are MERGED with
-- (not replaced by) any user-supplied @ghc-options@, so a workspace can still add
-- more. Keep the list small and justified; a quirk is a last resort for a
-- package zinc cannot otherwise build cleanly.
module Zinc.Quirks
  ( buildQuirks
  , quirkGhcOptions
  ) where

-- | The quirk table: @(package name, extra ghc-options)@.
buildQuirks :: [(String, [String])]
buildQuirks =
  [ -- colour's modules derive instances via GND under per-module Safe inference,
    -- so zinc's build infers Data.Colour as UNSAFE while nixpkgs' identical
    -- colour-2.3.6 is Safe-importable. Forcing -XSafe matches nixpkgs' build so
    -- ansi-terminal (which imports Data.Colour Safely) compiles (zinc-576.9).
    ("colour", ["-XSafe"])
  ]

-- | The built-in extra ghc-options for a package (empty when it has no quirk).
quirkGhcOptions :: String -> [String]
quirkGhcOptions name = maybe [] id (lookup name buildQuirks)
