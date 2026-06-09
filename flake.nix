{
  description = "Fast, reproducible Haskell builds that just work.";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAll = nixpkgs.lib.genAttrs systems;

      # Per-system toolchain + zinc package, shared by devShells and packages.
      perSystem = system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          # GHC wrapped with the (non-boot) libraries zinc itself is built
          # against — provided via Nix, not cabal's solver (dogfooding zinc's
          # own thesis). The rest of zinc's deps are GHC boot libs.
          ghc = pkgs.haskellPackages.ghcWithPackages (p: [
            p.optparse-applicative
            p.toml-parser
            p.SHA
            p.hspec
          ]);
          # zinc, built without cabal: zinc drives `ghc --make` directly, so we
          # do too (zinc.cabal no longer exists). Same invocation as the dev
          # binary. Only src/ + app/ are copied into the build sandbox.
          zinc = pkgs.stdenv.mkDerivation {
            pname = "zinc";
            version = "0.1.0.0";
            src = nixpkgs.lib.fileset.toSource {
              root = ./.;
              fileset = nixpkgs.lib.fileset.unions [ ./src ./app ];
            };
            nativeBuildInputs = [ ghc ];
            buildPhase = ''
              runHook preBuild
              # zinc's own source imports the synthesized Paths_zinc (git version
              # metadata, zinc-3x4/b3z). A cold-start nix build has no existing
              # zinc to synthesize it, and no .git in the sandbox (the fileset is
              # src+app only), so emit a no-git stub here — matching
              # Zinc.Paths.synthesizePaths with noGitMeta — before driving ghc
              # directly (zinc-z9w). version mirrors [package].version in zinc.toml.
              mkdir -p .boot
              cat > .boot/Paths_zinc.hs <<'PATHS_EOF'
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE ImplicitPrelude #-}
module Paths_zinc (version, gitHash, gitCommitCount, gitDirty, fullVersion, getDataFileName, getDataDir, getBinDir, getLibDir, getSysconfDir) where

import Data.Version (Version, makeVersion)

version :: Version
version = makeVersion [0,1,0,0]

gitHash :: String
gitHash = ""

gitCommitCount :: Int
gitCommitCount = 0

gitDirty :: Bool
gitDirty = False

fullVersion :: String
fullVersion = "0.1.0.0"

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = return name

getDataDir, getBinDir, getLibDir, getSysconfDir :: IO FilePath
getDataDir = return "."
getBinDir = return "."
getLibDir = return "."
getSysconfDir = return "."
PATHS_EOF
              ghc -O -threaded -rtsopts -with-rtsopts=-N -isrc -iapp -i.boot \
                -outputdir .build -o zinc app/Main.hs
              runHook postBuild
            '';
            installPhase = ''
              runHook preInstall
              install -Dm755 zinc $out/bin/zinc
              runHook postInstall
            '';
            meta = {
              description = "Fast, reproducible Haskell builds that just work.";
              mainProgram = "zinc";
            };
          };
        in
        { inherit pkgs ghc zinc; };
    in
    {
      packages = forAll (system: { default = (perSystem system).zinc; });

      # `nix run github:garethstokes/zinc`
      apps = forAll (system: {
        default = {
          type = "app";
          program = "${(perSystem system).zinc}/bin/zinc";
          meta = { description = "Fast, reproducible Haskell builds that just work."; };
        };
      });

      # `pkgs.zinc` for downstream nixpkgs overlays.
      overlays.default = final: _prev: {
        zinc = (perSystem final.stdenv.hostPlatform.system).zinc;
      };

      # `nix flake init -t github:garethstokes/zinc` bootstraps a flat project
      # (flake + zinc.toml + app/ + .gitignore) — the Nix-idiomatic entry point,
      # complementing `zinc new` (zinc-6hf.4).
      templates.default = {
        path = ./templates/default;
        description = "A flat single-package zinc project (zinc.toml + app/ + flake + .gitignore).";
        welcomeText = ''
          Created a zinc project.
            - Rename the package: edit `name` and `[build.exe.app]` in zinc.toml.
            - Build and run: `zinc run` (enter `nix develop` first if your zinc
              build does not yet auto-provision the toolchain).
        '';
      };

      devShells = forAll (system:
        let s = perSystem system;
        in {
          default = s.pkgs.mkShell {
            # No cabal-install: zinc is built and tested with ghc/runghc directly
            # and, ultimately, by zinc itself (self-hosting). Nix provides only the
            # compiler, git, and the source preprocessors zinc shells out to.
            packages = [
              s.ghc
              s.pkgs.git
              # Source preprocessors zinc runs for dependencies that ship .x/.y
              # (e.g. toml-parser's alex/happy lexer+parser). hsc2hs ships with GHC.
              s.pkgs.haskellPackages.alex
              s.pkgs.haskellPackages.happy
            ];
          };
        });
    };
}
