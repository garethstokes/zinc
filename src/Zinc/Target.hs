-- | Build targets (zinc-9po): the compile target parameterizes the build driver
-- and the provisioned toolchain. @native@ is the default; @wasm32-wasi@ compiles
-- the workspace to WebAssembly via GHC's wasm cross-compiler (delivered by the
-- @ghc-wasm-meta@ Nix flake). This module is the FOUNDATION (zinc-9po.1): the
-- 'Target' type and the cross-prefix tool resolution; the flake provisioning is
-- in "Zinc.Nix"/"Zinc.Env", the build-driver wiring + @--target@ flag follow in
-- zinc-9po.2/.3. Spec: docs/superpowers/specs/2026-06-05-zinc-wasm-targets-design.md.
module Zinc.Target
  ( Target (..)
  , parseTarget
  , targetTriple
  , toolPrefix
  , ghcFor
  , ghcPkgFor
  , hsc2hsFor
  , isWasm
  ) where

-- | A compile target. @Native@ builds for the host (the default, byte-identical
-- to zinc's pre-9po behaviour); @Wasm32Wasi@ cross-compiles to @wasm32-wasi@.
data Target = Native | Wasm32Wasi
  deriving (Eq, Show)

-- | Parse a @--target@ value. @wasm@ is accepted as a friendly alias.
parseTarget :: String -> Either String Target
parseTarget s = case s of
  "native"      -> Right Native
  "wasm32-wasi" -> Right Wasm32Wasi
  "wasm"        -> Right Wasm32Wasi
  _             -> Left (s ++ ": unknown target (expected: native, wasm32-wasi)")

-- | The target triple, as used in the cache key (zinc-9po.2) and diagnostics.
targetTriple :: Target -> String
targetTriple Native     = "native"
targetTriple Wasm32Wasi = "wasm32-wasi"

-- | The GHC tool name prefix (the cross-compiler convention): @native@ tools are
-- unprefixed (@ghc@), wasm tools carry the @wasm32-wasi-@ prefix.
toolPrefix :: Target -> String
toolPrefix Native     = ""
toolPrefix Wasm32Wasi = "wasm32-wasi-"

-- | The @ghc@ / @ghc-pkg@ / @hsc2hs@ binary names for a target (the build driver
-- invokes these instead of the bare names). @alex@/@happy@ are host code
-- generators and stay unprefixed.
ghcFor, ghcPkgFor, hsc2hsFor :: Target -> String
ghcFor t     = toolPrefix t ++ "ghc"
ghcPkgFor t  = toolPrefix t ++ "ghc-pkg"
hsc2hsFor t  = toolPrefix t ++ "hsc2hs"

-- | Whether a target is a WebAssembly target (true for @wasm32-wasi@).
isWasm :: Target -> Bool
isWasm Wasm32Wasi = True
isWasm Native     = False
