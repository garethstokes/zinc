-- | @zinc package \<format\>@ (zinc-7m6): turn a build into a deployable artifact
-- via the generated flake's Nix packaging — zero Nix knowledge. This module is
-- the FOUNDATION (zinc-7m6.1): the format model, the pure packaging-flake
-- generator (its @packages.default@ wraps zinc's built binary into a store
-- derivation — the base every format builds on), and the @runPackage@ flow
-- (build the app, stage a packaging flake, drive Nix, emit the artifact).
--
-- Per-format builders extend the flake's outputs and are tracked separately:
-- docker (7m6.2), static (7m6.3), bundle (7m6.4), nix-copy (7m6.5). @--target@
-- (compile architecture) is orthogonal and composes; @package@ is deploy format.
module Zinc.Package
  ( PackageFormat (..)
  , parsePackageFormat
  , formatName
  , packagingFlake
  ) where

-- | A deploy format. @docker@ → OCI image, @static@ → musl static binary,
-- @bundle@ → portable single-file, @nix@ → store closure (nix copy).
data PackageFormat = Docker | Static | Bundle | NixClosure
  deriving (Eq, Show)

-- | The CLI spelling of a format.
formatName :: PackageFormat -> String
formatName Docker     = "docker"
formatName Static     = "static"
formatName Bundle     = "bundle"
formatName NixClosure = "nix"

-- | Parse the @\<format\>@ argument; 'Left' lists the valid formats.
parsePackageFormat :: String -> Either String PackageFormat
parsePackageFormat s = case s of
  "docker" -> Right Docker
  "static" -> Right Static
  "bundle" -> Right Bundle
  "nix"    -> Right NixClosure
  _        -> Left (s ++ ": unknown format (expected one of: docker, static, bundle, nix)")

-- | The packaging flake for an app whose binary @name@ has been staged next to
-- this flake (zinc-7m6.1). @packages.default@ wraps that binary into a store
-- derivation (its RPATH already points at the nix store, so its runtime closure
-- is captured), and @apps.default@ makes it runnable — the foundation the
-- docker/static/bundle/nix builders extend. Pure: the actual @nix build@ /
-- @nix bundle@ is driven by 'runPackage'.
packagingFlake :: String -> String
packagingFlake name =
  unlines
    [ "{"
    , "  description = \"zinc package: " ++ name ++ "\";"
    , "  inputs.nixpkgs.url = \"github:NixOS/nixpkgs/nixos-24.05\";"
    , "  outputs = { self, nixpkgs }:"
    , "    let"
    , "      systems = [ \"x86_64-linux\" \"aarch64-linux\" \"x86_64-darwin\" \"aarch64-darwin\" ];"
    , "      forAll = nixpkgs.lib.genAttrs systems;"
    , "    in {"
    , "      packages = forAll (system:"
    , "        let pkgs = nixpkgs.legacyPackages.${system};"
    , "            app = pkgs.runCommandLocal \"" ++ name ++ "\" { } ''"
    , "              install -Dm755 ${./" ++ name ++ "} $out/bin/" ++ name
    , "            '';"
    , "        in {"
    , "          default = app;"
    , "          # docker / static / bundle outputs are added by zinc-7m6.2/.3/.4."
    , "        });"
    , "      apps = forAll (system: {"
    , "        default = { type = \"app\"; program = \"${self.packages.${system}.default}/bin/" ++ name ++ "\"; };"
    , "      });"
    , "    };"
    , "}"
    ]
