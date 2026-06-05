-- | Deterministic dependency-closure discovery (spec §9, zinc-49o): instead of
-- the iterative @add → "no repo for X" → add repo → retry@ loop, compute a
-- package's non-boot transitive closure from the GHC environment
-- (@ghc-pkg field <pkg> depends@, walked recursively) and auto-derive each
-- member's git repo from Hackage @source-repository@ metadata. Members with no
-- discoverable git repo (e.g. colour, tf-random — darcs-era) are flagged as
-- "needs vendoring". Hackage is consulted only here, at add/discover time.
--
-- Caveat: @ghc-pkg@ only knows /installed/ packages and reflects this env's
-- cabal-flag choices — it is the authoritative closure for /this/ toolchain.
module Zinc.Closure
  ( pkgNameOf
  , parseDependsField
  , transitiveDeps
  , discoverRepos
  , installedVersion
  , ClosureReport (..)
  , runClosure
  , closureReportJson
  , renderClosure
  ) where

import Data.Char (isDigit)
import Data.List (intercalate, stripPrefix)
import Data.Maybe (listToMaybe)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)
import Zinc.Diagnostic (ZincError (OtherError, ToolchainMissing))
import Zinc.Except (failWithError, liftIO, runResult)
import Zinc.Hackage (hackageSourceRepo)
import Zinc.Json (Json (..))
import Zinc.Resolve (isBootLib)
import System.Directory (findExecutable)

-- | The package name from an installed unit-id: everything before the version
-- component (the first dash-separated piece that starts with a digit). E.g.
-- @"aeson-2.2.3.0-abc"@ -> @"aeson"@, @"data-default-class-0.1.2"@ ->
-- @"data-default-class"@, @"rts"@ -> @"rts"@.
pkgNameOf :: String -> String
pkgNameOf = intercalate "-" . takeWhile (not . isVersion) . splitDash
  where
    isVersion (c : _) = isDigit c
    isVersion [] = False
    splitDash s = case break (== '-') s of
      (a, '-' : r) -> a : splitDash r
      (a, _) -> [a]

-- | Parse @ghc-pkg field <pkg> depends@ output into unit-ids (handles the
-- multi-line, indented continuation form).
parseDependsField :: String -> [String]
parseDependsField = filter (/= "depends:") . words

-- | The direct dependency package names of an installed package, via
-- @ghc-pkg field@. Empty if the package is not installed.
ghcPkgDeps :: String -> IO [String]
ghcPkgDeps pkg = do
  (code, out, _) <- readProcessWithExitCode "ghc-pkg" ["field", pkg, "depends"] ""
  pure $ case code of
    ExitSuccess -> map pkgNameOf (parseDependsField out)
    _ -> []

-- | The version of an installed package as ghc-pkg reports it (e.g. @"2.3.6"@),
-- or 'Nothing' if it is not installed in this environment. Used by @zinc vendor@
-- to pin a no-git dependency to the exact version this toolchain ships (b1z) —
-- the same authoritative source as closure discovery.
installedVersion :: String -> IO (Maybe String)
installedVersion pkg = do
  (code, out, _) <- readProcessWithExitCode "ghc-pkg" ["field", pkg, "version"] ""
  pure $ case code of
    ExitSuccess -> listToMaybe [dropWhile (== ' ') v | l <- lines out, Just v <- [stripPrefix "version:" l]]
    _           -> Nothing

-- | A package's non-boot transitive closure (including itself), walked from the
-- GHC environment. Boot libraries (per the predicate) are excluded.
transitiveDeps :: (String -> Bool) -> String -> IO [String]
transitiveDeps isBoot root = go [root] []
  where
    go [] seen = pure (reverse seen)
    go (p : ps) seen
      | isBoot p || p `elem` seen = go ps seen
      | otherwise = do
          deps <- ghcPkgDeps p
          go (ps ++ filter (`notElem` seen) deps) (p : seen)

-- | Discover each package's git repo via the injected discovery function
-- (production: Hackage @source-repository@). Returns the found @(name, repo)@
-- pairs and the names with no discoverable repo (needs vendoring). The
-- discovery function is a parameter so the pure logic is testable.
discoverRepos :: (String -> IO (Maybe String)) -> [String] -> IO ([(String, String)], [String])
discoverRepos discover names = do
  results <- mapM (\n -> (,) n <$> discover n) names
  pure ([(n, r) | (n, Just r) <- results], [n | (n, Nothing) <- results])

-- | The result of discovering a package's closure: each member with its repo
-- (when found), and the subset that needs vendoring.
data ClosureReport = ClosureReport
  { crRoot           :: String
  , crMembers        :: [(String, Maybe String)] -- ^ name -> discovered repo
  , crNeedsVendoring :: [String]
  }
  deriving (Eq, Show)

-- | @zinc closure <pkg>@: compute the non-boot transitive closure from the env
-- and auto-derive each member's repo from Hackage. Fails if the toolchain
-- (@ghc-pkg@) is absent or the package isn't installed in this environment.
runClosure :: String -> IO (Either ZincError ClosureReport)
runClosure root = runResult $ do
  hasGhcPkg <- liftIO (findExecutable "ghc-pkg")
  case hasGhcPkg of
    Nothing -> failWithError (ToolchainMissing "ghc-pkg")
    Just _ -> do
      members <- liftIO (transitiveDeps isBootLib root)
      if null members
        then failWithError (OtherError (root ++ ": not installed in this GHC environment (ghc-pkg can't compute its closure)"))
        else do
          (found, missing) <- liftIO (discoverRepos hackageDiscover members)
          pure (ClosureReport root [(m, lookup m found) | m <- members] missing)
  where
    hackageDiscover n = either (const Nothing) id <$> hackageSourceRepo n

-- | The closure report as JSON.
closureReportJson :: ClosureReport -> Json
closureReportJson r =
  JObject
    [ ("package", JString (crRoot r))
    , ("closure", JArray [JObject (("name", JString n) : maybe [] (\repo -> [("repo", JString repo)]) mr) | (n, mr) <- crMembers r])
    , ("needsVendoring", JArray (map JString (crNeedsVendoring r)))
    ]

-- | A compact human rendering.
renderClosure :: ClosureReport -> String
renderClosure r =
  unlines $
    [crRoot r ++ " — non-boot closure (" ++ show (length (crMembers r)) ++ " packages):"]
      ++ [ "  " ++ n ++ " -> " ++ maybe "(no upstream git — needs vendoring)" id mr | (n, mr) <- crMembers r ]
      ++ ["", show (length (crNeedsVendoring r)) ++ " need vendoring" ++ if null (crNeedsVendoring r) then "." else ": " ++ intercalate ", " (crNeedsVendoring r)]
