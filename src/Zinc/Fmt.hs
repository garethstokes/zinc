-- | @zinc fmt@ (spec §3): rewrite a workspace manifest's dependency tables to
-- the one canonical layout (shared with @renderWorkspace@, so @zinc add@ output
-- is already fmt-clean), preserving everything else — @[workspace]@,
-- @[package]@/@[build.*]@ (zinc's combined self-host manifest), and comments
-- outside the dependency sections. @--check@ writes nothing and reports whether
-- the file is already canonical (CI / non-interactive contract).
module Zinc.Fmt
  ( canonicalizeManifest
  , setManifestDependencies
  , mergeManifestDependencies
  , runFmt
  ) where

import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (isPrefixOf, sort)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Zinc.Diagnostic (ZincError (NoZincToml))
import Zinc.Except (failWithError, liftEither, liftIO, runResult)
import Zinc.Manifest (Dependency, depName, parseWorkspace, renderDep, renderDependencies, wsDependencies)

-- | Rewrite a manifest's dependency sections to @deps@, preserving every other
-- line — @[workspace]@, the member's @[package]@/@[build.*]@ (a flat
-- single-package project keeps these in the same file), and comments. The
-- text-level editor behind @zinc add@/@vendor@: 'renderWorkspace' alone models
-- only @[workspace]@+@[dependencies]@ and would drop the rest (zinc-lnh). Fails
-- (Left) if the manifest has no @[workspace]@.
setManifestDependencies :: String -> [Dependency] -> Either String String
setManifestDependencies src deps = do
  _ <- parseWorkspace src -- validate it's a workspace manifest
  let kept = dropTrailingBlank (stripDepSections (lines src))
  pure (unlines (kept ++ [""] ++ renderDependencies deps))

-- | The minimal-diff editor behind @zinc add@/@vendor@: rewrite a manifest to
-- declare exactly @deps@ while disturbing the file as little as possible. Unlike
-- the canonical 'setManifestDependencies' (which @zinc fmt@ uses to alphabetise
-- and strip in-table comments), this keeps every existing @[dependencies.name]@
-- block whose dependency is unchanged BYTE-FOR-BYTE — preserving the author's
-- ordering and in-table comments — re-renders only a block whose fields changed,
-- drops a dep no longer desired, and appends new deps (sorted) after the rest.
-- Everything outside the dependency sections is untouched. Falls back to the
-- canonical writer if the @[dependencies]@ table carries one-line shorthand deps
-- (mixing them with appended sub-tables would reorder them under a sub-table),
-- and (via 'setManifestDependencies') if the file isn't a parseable workspace.
mergeManifestDependencies :: String -> [Dependency] -> Either String String
mergeManifestDependencies src deps = do
  orig <- parseWorkspace src
  let ls = lines src
      (before, region, after) = splitDepRegion ls
      (tableHdr, subs) = parseDepRegion region
  if any isKeyValue (drop 1 tableHdr)
    then setManifestDependencies src deps -- shorthand deps present: canonicalise instead
    else
      let existingNames = map fst subs
          desiredNames = map depName deps
          retained = filter (`elem` desiredNames) existingNames
          newNames = sort (filter (`notElem` existingNames) desiredNames)
          findDep xs n = lookup n [(depName d, d) | d <- xs]
          emit n = case (lookup n subs, findDep (wsDependencies orig) n, findDep deps n) of
            (Just blk, Just o, Just d) | o == d -> stripTrailingBlank blk -- unchanged → verbatim
            (_, _, Just d)                       -> dropWhile (all isSpace) (renderDep d)
            _                                    -> []
          blocks = map emit (retained ++ newNames)
          regionOut = ensureHeader (stripTrailingBlank tableHdr) ++ concatMap ("" :) blocks
       in pure (unlines (before ++ regionOut ++ after))

-- | Split lines into (before the dependency region, the region, after it). The
-- region runs from the first dependency header to just before the next
-- non-dependency top-level header (or end of file).
splitDepRegion :: [String] -> ([String], [String], [String])
splitDepRegion ls = case break isDepHeaderLine ls of
  (before, [])   -> (before, [], [])
  (before, rest) ->
    let region = takeWhile (\l -> not (isHeaderLine l) || isDepHeaderLine l) rest
     in (before, region, drop (length region) rest)

-- | Split a dependency region into its @[dependencies]@ header block (the header
-- line plus following comments/blanks, up to the first @[dependencies.name]@)
-- and the named sub-table blocks, each header-through-to-the-next-sub-table.
parseDepRegion :: [String] -> ([String], [(String, [String])])
parseDepRegion region = (hdr, groupSubs rest)
  where
    (hdr, rest) = break isSubDepHeader region
    groupSubs [] = []
    groupSubs (h : ls) = let (body, more) = break isSubDepHeader ls in (subName h, h : body) : groupSubs more

-- | The package name in a @[dependencies.name]@ header.
subName :: String -> String
subName l = takeWhile (/= ']') (drop (length "[dependencies.") (dropWhile (/= '[') l))

isHeaderLine :: String -> Bool
isHeaderLine l = case dropWhile isSpace l of ('[' : _) -> True; _ -> False

isDepHeaderLine :: String -> Bool
isDepHeaderLine l =
  let h = takeWhile (/= ']') (drop 1 (dropWhile (/= '[') l))
   in isHeaderLine l && (h == "dependencies" || "dependencies." `isPrefixOf` h)

isSubDepHeader :: String -> Bool
isSubDepHeader l = isHeaderLine l && "dependencies." `isPrefixOf` takeWhile (/= ']') (drop 1 (dropWhile (/= '[') l))

-- | A non-comment, non-header @key = value@ line (a one-line shorthand dep).
isKeyValue :: String -> Bool
isKeyValue l = case dropWhile isSpace l of
  ('[' : _) -> False
  ('#' : _) -> False
  s         -> '=' `elem` s && not (all isSpace s)

ensureHeader :: [String] -> [String]
ensureHeader hdr = if any isHeaderLine hdr then hdr else "[dependencies]" : hdr

stripTrailingBlank :: [String] -> [String]
stripTrailingBlank = reverse . dropWhile (all isSpace) . reverse

-- | Canonicalize a workspace manifest's text: drop the existing dependency

-- | Canonicalize a workspace manifest's text: drop the existing dependency
-- sections (@[dependencies]@, @[dependencies.<name>]@, legacy @[registry]@ /
-- @[build-options]@) and append the canonical @[dependencies]@ block, keeping
-- all other lines in place. Fails (Left) if the manifest has no @[workspace]@.
canonicalizeManifest :: String -> Either String String
canonicalizeManifest src = do
  ws <- parseWorkspace src
  setManifestDependencies src (wsDependencies ws)

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
