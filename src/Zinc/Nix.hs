-- | Generate the hidden @flake.nix@ that provisions zinc's toolchain (spec §6):
-- a pinned nixpkgs, the exact GHC compiler (just the compiler, not nixpkgs'
-- Haskell package set), the alex/happy preprocessors, and the union of the
-- workspace's system libraries. The user never writes this; zinc manages it.
--
-- For a wasm target (zinc-9po), the flake instead adds @ghc-wasm-meta@ (the GHC
-- wasm cross-compiler, as a Nix flake input) plus @node@ (Template Haskell /
-- GHCi external interpreter) and @wasmtime@ (the runner) — so the user never
-- sets up the wasm toolchain.
module Zinc.Nix
  ( generateFlake
  , generateFlakeFor
  ) where

import Zinc.Target (Target (..))

-- | @generateFlake ghcVersion systemLibs@ renders the NATIVE dev-shell flake
-- (@ghc<version>@ + preprocessors + nixpkgs system libs). Unchanged from before
-- 9po — a thin alias for the native case of 'generateFlakeFor'.
generateFlake :: String -> [String] -> String
generateFlake = generateFlakeFor Native

-- | As 'generateFlake', but for an explicit 'Target'. @Native@ is byte-identical
-- to the historical output; @Wasm32Wasi@ swaps the native GHC for the
-- @ghc-wasm-meta@ toolchain (+ node + wasmtime). zinc-9po.1.
generateFlakeFor :: Target -> String -> [String] -> String
generateFlakeFor Native ghcVersion systemLibs =
  unlines (preamble ++ map (indent <>) packages ++ closing)
  where
    indent = "              "
    ghcAttr = "pkgs.haskell.compiler.ghc" ++ filter (/= '.') ghcVersion
    packages =
      [ ghcAttr
      , "pkgs.haskellPackages.alex"
      , "pkgs.haskellPackages.happy"
      ]
        ++ map ("pkgs." ++) systemLibs

    preamble =
      [ "{"
      , "  description = \"zinc-managed GHC toolchain\";"
      , "  inputs.nixpkgs.url = \"github:NixOS/nixpkgs/nixos-24.05\";"
      , "  outputs = { self, nixpkgs }:"
      , "    let"
      , "      systems = [ \"x86_64-linux\" \"aarch64-linux\" \"x86_64-darwin\" \"aarch64-darwin\" ];"
      , "      forAll = nixpkgs.lib.genAttrs systems;"
      , "    in {"
      , "      devShells = forAll (system:"
      , "        let pkgs = nixpkgs.legacyPackages.${system};"
      , "        in {"
      , "          default = pkgs.mkShell {"
      , "            packages = ["
      ]

    closing =
      [ "            ];"
      , "          };"
      , "        });"
      , "    };"
      , "}"
      ]
-- The wasm flake: GHC's wasm32-wasi cross-compiler comes from the ghc-wasm-meta
-- flake (its `default` bundle exposes wasm32-wasi-ghc/-ghc-pkg/-hsc2hs); node
-- backs Template Haskell + GHCi, wasmtime runs the module, and alex/happy stay
-- native (host code generators). System libs are omitted — the wasm MVP is
-- pure-Haskell closures (spec §5).
generateFlakeFor Wasm32Wasi _ghcVersion _systemLibs =
  unlines
    [ "{"
    , "  description = \"zinc-managed GHC wasm32-wasi toolchain\";"
    , "  inputs.nixpkgs.url = \"github:NixOS/nixpkgs/nixos-24.05\";"
    , "  inputs.ghc-wasm-meta.url = \"github:haskell-wasm/ghc-wasm-meta\";"
    , "  outputs = { self, nixpkgs, ghc-wasm-meta }:"
    , "    let"
    , "      systems = [ \"x86_64-linux\" \"aarch64-linux\" \"x86_64-darwin\" \"aarch64-darwin\" ];"
    , "      forAll = nixpkgs.lib.genAttrs systems;"
    , "    in {"
    , "      devShells = forAll (system:"
    , "        let pkgs = nixpkgs.legacyPackages.${system};"
    , "        in {"
    , "          default = pkgs.mkShell {"
    , "            packages = ["
    , "              ghc-wasm-meta.packages.${system}.default"
    , "              ghc-wasm-meta.packages.${system}.nodejs"
    , "              ghc-wasm-meta.packages.${system}.wasmtime"
    , "              pkgs.haskellPackages.alex"
    , "              pkgs.haskellPackages.happy"
    , "            ];"
    , "          };"
    , "        });"
    , "    };"
    , "}"
    ]
