{
  description = "zinc — a git-native, Nix-assisted build tool for Haskell";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAll = nixpkgs.lib.genAttrs systems;
    in {
      devShells = forAll (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          # GHC wrapped with the libraries zinc itself is built against.
          # Provided via Nix (not cabal's solver) — dogfooding zinc's own thesis.
          ghc = pkgs.haskellPackages.ghcWithPackages (p: [
            p.optparse-applicative
            p.toml-parser
            p.SHA
            p.hspec
          ]);
        in {
          default = pkgs.mkShell {
            packages = [
              ghc
              pkgs.cabal-install   # convenience only; the test loop uses ghc/runghc directly
              pkgs.git
              # Source preprocessors zinc runs for dependencies that ship .x/.y
              # (e.g. toml-parser's alex/happy lexer+parser). hsc2hs ships with GHC.
              pkgs.haskellPackages.alex
              pkgs.haskellPackages.happy
            ];
          };
        });
    };
}
