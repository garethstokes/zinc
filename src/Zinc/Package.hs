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
  , dockerImageRef
  , storePathRefs
  , packagingFlake
  ) where

import Data.Char (isAlphaNum)
import Data.List (isPrefixOf, nub)

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

-- | Resolve a docker image @name:tag@ from the binary name and an optional
-- @--tag@ argument: an explicit @name:tag@ is split; a bare @--tag@ value is the
-- tag (default name); no @--tag@ defaults to @\<binary\>:latest@.
dockerImageRef :: String -> Maybe String -> (String, String)
dockerImageRef bin mtag = case mtag of
  Nothing -> (bin, "latest")
  Just t -> case break (== ':') t of
    (n, ':' : v) | not (null n) && not (null v) -> (n, v)
    _ -> (t, "latest")

-- | Extract the top-level @\/nix\/store\/\<hash>-\<name>@ paths referenced inside
-- a (prebuilt) binary's bytes — its RPATH/@DT_NEEDED@ store dependencies.
--
-- This is the crux of correct packaging (zinc-7m6.2/.4): zinc's binary is a
-- /foreign/ ELF (built by the dynamic GHC, not by Nix), so the libraries it
-- needs at runtime (gmp, libffi, …) were never inputs to the wrapping
-- derivation. Nix's reference scanner only looks for hashes that are in a
-- derivation's input closure, so without help it silently omits those deps —
-- the closure (and any docker image / bundle / nix-copy built from it) ships a
-- binary that can't find @libgmp.so@. 'runPackage' feeds these paths back into
-- the flake as pinned @builtins.storePath@ inputs so the scanner captures them.
storePathRefs :: String -> [String]
storePathRefs = nub . go
  where
    prefix = "/nix/store/"
    go [] = []
    go s@(_ : rest)
      | prefix `isPrefixOf` s = case parsePath (drop (length prefix) s) of
          Just p  -> (prefix ++ p) : go (drop (length prefix + length p) s)
          Nothing -> go rest
      | otherwise = go rest
    -- A store path is a 32-char nix-base32 hash, a dash, then a name; we keep
    -- the top-level path only (stop at the first @/@ — drop @/lib@ etc.).
    parsePath s =
      let (h, r1) = splitAt 32 s
       in if length h == 32 && all isBase32 h
            then case r1 of
              ('-' : r2) ->
                let nm = takeWhile isNameChar r2
                 in if null nm then Nothing else Just (h ++ "-" ++ nm)
              _ -> Nothing
            else Nothing
    isBase32 c = c `elem` ("0123456789abcdfghijklmnpqrsvwxyz" :: String)
    isNameChar c = isAlphaNum c || c `elem` ("+-._?=" :: String)

-- | The packaging flake for an app whose binary @name@ has been staged next to
-- this flake (zinc-7m6.1). @packages.default@ wraps that binary into a store
-- derivation; @depPaths@ are the binary's real runtime store dependencies
-- (from 'storePathRefs'), pinned as @builtins.storePath@ inputs so Nix's
-- reference scanner captures the full runtime closure — essential for docker
-- (7m6.2), bundle (7m6.4) and nix-copy (7m6.5), which ship the closure rather
-- than relying on the build host's store. @apps.default@ makes it runnable.
-- @packages.dockerImage@ is an OCI image of the app + its closure, tagged
-- @\<imageName\>:\<imageTag\>@. Because the pins use @builtins.storePath@, the
-- @nix@ invocations in 'runPackage' run @--impure@. Pure: the actual @nix
-- build@ is driven by 'runPackage'.
packagingFlake :: String -> String -> String -> [String] -> String
packagingFlake name imageName imageTag depPaths =
  unlines
    ( [ "{"
      , "  description = \"zinc package: " ++ name ++ "\";"
      , "  inputs.nixpkgs.url = \"github:NixOS/nixpkgs/nixos-24.05\";"
      , "  outputs = { self, nixpkgs }:"
      , "    let"
      , "      systems = [ \"x86_64-linux\" \"aarch64-linux\" \"x86_64-darwin\" \"aarch64-darwin\" ];"
      , "      forAll = nixpkgs.lib.genAttrs systems;"
      , "    in {"
      , "      packages = forAll (system:"
      , "        let pkgs = nixpkgs.legacyPackages.${system};"
      , "            # The prebuilt binary's real runtime deps, pinned so Nix's"
      , "            # reference scanner captures the full closure (zinc-7m6.2)."
      , "            runtimeDeps = map builtins.storePath ["
      ]
        ++ map (\p -> "              \"" ++ p ++ "\"") depPaths
        ++ [ "            ];"
           , "            app = pkgs.runCommandLocal \"" ++ name ++ "\" { pname = \"" ++ name ++ "\"; version = \"0\"; buildInputs = runtimeDeps; } ''"
           , "              install -Dm755 ${./" ++ name ++ "} $out/bin/" ++ name
           , "            '';"
           , "        in {"
           , "          default = app;"
           , "          # An OCI image of the app + its runtime closure (zinc-7m6.2)."
           , "          dockerImage = pkgs.dockerTools.buildLayeredImage {"
           , "            name = \"" ++ imageName ++ "\";"
           , "            tag = \"" ++ imageTag ++ "\";"
           , "            contents = [ app ];"
           , "            config.Cmd = [ \"/bin/" ++ name ++ "\" ];"
           , "          };"
           , "        });"
           , "      apps = forAll (system: {"
           , "        default = { type = \"app\"; program = \"${self.packages.${system}.default}/bin/" ++ name ++ "\"; };"
           , "      });"
           , "    };"
           , "}"
           ]
    )
