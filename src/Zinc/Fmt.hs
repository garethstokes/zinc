-- | @zinc fmt@ (spec §3): rewrite a workspace manifest's dependency tables to
-- the one canonical layout (shared with @renderWorkspace@, so @zinc add@ output
-- is already fmt-clean), preserving everything else — @[workspace]@,
-- @[package]@/@[build.*]@ (zinc's combined self-host manifest), and comments
-- outside the dependency sections. @--check@ writes nothing and reports whether
-- the file is already canonical (CI / non-interactive contract).
module Zinc.Fmt
  ( canonicalizeManifest
  , runFmt
  ) where

import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Zinc.Diagnostic (ZincError (NoZincToml))
import Zinc.Except (failWithError, liftEither, liftIO, runResult)
import Zinc.Manifest (parseWorkspace, renderDependencies, wsDependencies)

-- | Canonicalize a workspace manifest's text: drop the existing dependency
-- sections (@[dependencies]@, @[dependencies.<name>]@, legacy @[registry]@ /
-- @[build-options]@) and append the canonical @[dependencies]@ block, keeping
-- all other lines in place. Fails (Left) if the manifest has no @[workspace]@.
canonicalizeManifest :: String -> Either String String
canonicalizeManifest src = do
  ws <- parseWorkspace src
  let kept = dropTrailingBlank (stripDepSections (lines src))
  pure (unlines (kept ++ [""] ++ renderDependencies (wsDependencies ws)))

-- | Drop every dependency-related section (header line through to the next
-- top-level header), keeping all other lines in order.
stripDepSections :: [String] -> [String]
stripDepSections = reverse . snd . foldl step (False, [])
  where
    step (dropping, acc) l
      | isHeader l = let d = isDepHeader l in (d, if d then acc else l : acc)
      | dropping = (True, acc)
      | otherwise = (False, l : acc)
    isHeader l = case dropWhile isSpace l of ('[' : _) -> True; _ -> False
    isDepHeader l =
      let h = takeWhile (/= ']') (drop 1 (dropWhile (/= '[') l))
       in h == "dependencies" || h == "registry" || h == "build-options" || "dependencies." `isPrefixOf` h

dropTrailingBlank :: [String] -> [String]
dropTrailingBlank = reverse . dropWhile (all isSpace) . reverse

-- | @zinc fmt@ / @zinc fmt --check@ on the workspace at @wsDir@. Returns whether
-- the file was already canonical (so @--check@ can exit non-zero, and the
-- writer can report "already formatted"); in non-check mode it rewrites the file
-- when it isn't canonical.
runFmt :: Bool -> FilePath -> IO (Either ZincError Bool)
runFmt check wsDir = runResult $ do
  let wsFile = wsDir </> "zinc.toml"
  present <- liftIO (doesFileExist wsFile)
  when (not present) (failWithError (NoZincToml wsDir))
  src <- liftIO (readFile wsFile)
  canon <- liftEither (canonicalizeManifest src)
  let clean = src == canon
  when (not check && not clean) (liftIO (writeFile wsFile canon))
  pure clean
