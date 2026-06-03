-- | Generate the hidden @flake.nix@ that provisions zinc's toolchain (spec §6):
-- a pinned nixpkgs, the exact GHC compiler (just the compiler, not nixpkgs'
-- Haskell package set), the alex/happy preprocessors, and the union of the
-- workspace's system libraries. The user never writes this; zinc manages it.
module Zinc.Nix
  ( generateFlake
  ) where

-- | @generateFlake ghcVersion systemLibs@ renders a flake providing a dev
-- shell with @ghc<version>@ + preprocessors + the given nixpkgs system libs.
generateFlake :: String -> [String] -> String
generateFlake ghcVersion systemLibs =
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
