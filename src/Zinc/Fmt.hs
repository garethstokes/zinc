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
  , reflowArrays
  , runFmt
  ) where

import Control.Monad (when)
import Data.Char (isAlphaNum, isSpace)
import Data.List (dropWhileEnd, intercalate, isPrefixOf, sort)
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
  out <- setManifestDependencies src (wsDependencies ws)
  -- Re-attach inline comments on dependency fields (the canonical re-render drops
  -- them), then reflow multi-item arrays one-per-line across the whole manifest
  -- (build sections kept verbatim AND the rendered [dependencies]).
  pure (reflowArrays (reattachDepComments src out))

-- | Re-attach inline comments on @[dependencies.*]@ field lines and shorthand
-- dependency lines that the canonical re-render drops, so @zinc fmt@ keeps
-- helpful annotations like @rev = "..."  # v0.11.0.0@. Matched by
-- @(dependency, field)@ so a comment survives dependency sorting and form
-- canonicalization. Array-valued fields are skipped (item-level comments ride
-- through 'reflowArrays'); run BEFORE 'reflowArrays' so a (rare) array-field
-- comment lands after the closing @]@. Idempotent.
reattachDepComments :: String -> String -> String
reattachDepComments src canon = unlines (go DepOther (lines canon))
  where
    inlineC = scanComments DepOther (lines src)
    go _ [] = []
    go ctx (l : ls)
      | isHeaderLine l = l : go (depCtx l) ls
      | otherwise = apply ctx l : go ctx ls
    apply ctx l = case commentKey ctx l of
      Just k | Just c <- lookup k inlineC, not (hasInlineComment l) -> l ++ "  " ++ c
      _ -> l

    scanComments _ [] = []
    scanComments ctx (l : ls)
      | isHeaderLine l = scanComments (depCtx l) ls
      | otherwise = case (commentKey ctx l, snd (splitComment l)) of
          (Just k, Just cmt) -> (k, dropWhileEnd isSpace cmt) : scanComments ctx ls
          _                  -> scanComments ctx ls

-- | The dependency context a top-level header introduces.
depCtx :: String -> DepCtx
depCtx l
  | isDepHeaderLine l = if null (subName l) then DepShort else DepSub (subName l)
  | otherwise = DepOther

-- | The @(dependency, field)@ key a non-header line contributes to (the field a
-- comment would attach to), if it is a single-line @field = value@ in a
-- dependency context. Array-valued fields and headers/com-only lines yield
-- 'Nothing'.
commentKey :: DepCtx -> String -> Maybe (String, String)
commentKey ctx l = do
  f <- fieldName l
  case ctx of
    DepSub x -> Just (x, f)
    DepShort -> Just (f, f) -- a shorthand line's dep name IS its field key
    DepOther -> Nothing

-- | The field name of a single-line @field = value@ line (the text before @=@),
-- or 'Nothing' for a header, a comment-only line, or an ARRAY-valued field.
fieldName :: String -> Maybe String
fieldName l =
  let (code, _) = splitComment l
      (k, eqRest) = break (== '=') code
   in case eqRest of
        ('=' : v)
          | not (isHeaderLine l)
          , let f = dropWhileEnd isSpace (dropWhile isSpace k)
          , not (null f)
          , take 1 (dropWhile isSpace v) /= "[" -> Just f
        _ -> Nothing

hasInlineComment :: String -> Bool
hasInlineComment l = case splitComment l of (_, Just _) -> True; _ -> False

-- | Split a line into (code before a comment, the @#@-comment incl. leading @#@)
-- — respecting string literals so a @#@ inside a quoted value is not a comment.
splitComment :: String -> (String, Maybe String)
splitComment = goS "" False
  where
    goS acc inStr s = case s of
      [] -> (reverse acc, Nothing)
      (c : cs)
        | inStr, c == '\\' -> case cs of (d : ds) -> goS (d : '\\' : acc) True ds; [] -> (reverse ('\\' : acc), Nothing)
        | inStr, c == '"' -> goS ('"' : acc) False cs
        | inStr -> goS (c : acc) True cs
        | c == '"' -> goS ('"' : acc) True cs
        | c == '#' -> (reverse acc, Just (c : cs))
        | otherwise -> goS (c : acc) False cs

-- | Dependency-region context for comment re-attachment.
data DepCtx = DepSub String | DepShort | DepOther

-- | Reflow every @key = [ ... ]@ array to ONE ITEM PER LINE — the canonical
-- @zinc fmt@ array layout. A multi-item array becomes:
--
-- > key = [
-- >   "a",
-- >   "b",
-- > ]
--
-- A single-item or empty array stays inline (@key = ["a"]@ / @key = []@). Inline
-- comments on an item and standalone comment lines inside the array are
-- preserved. Conservative + non-destructive: an array it can't cleanly parse
-- (nested arrays/inline tables, an unterminated bracket) is left exactly as-is,
-- and the transform is idempotent — fmt rewrites the file in place, so it must
-- never mangle a hand-curated manifest.
reflowArrays :: String -> String
reflowArrays = unlines . go . lines
  where
    go [] = []
    go (l : ls) = case arrayOpen l of
      Nothing -> l : go ls
      Just (indent, keyPart, afterBr) ->
        case collect afterBr ls of
          Nothing -> l : go ls -- no matching close found: leave untouched
          Just (body, trailer, rest) -> case parseParts body of
            Nothing    -> l : go ls -- nested/odd content: leave untouched
            Just parts -> emit indent keyPart parts trailer ++ go rest

    -- A line that opens an array: @<indent><barekey> = [<rest>@. Inline tables
    -- (@{@) and scalar values are not matched.
    arrayOpen l =
      let (indent, rest) = span isSpace l
          (k, eqRest) = break (== '=') rest
          key = dropWhileEnd isSpace k
       in case eqRest of
            ('=' : afterEq) | validKey key -> case dropWhile (== ' ') afterEq of
              ('[' : afterBr) -> Just (indent, key ++ " = [", afterBr)
              _               -> Nothing
            _ -> Nothing
    validKey s = not (null s) && all (\c -> isAlphaNum c || c == '-' || c == '_') s

    -- Gather the array body (from just after @[@ to the matching top-level @]@),
    -- the trailer after @]@, and the remaining lines. 'Nothing' if no clean
    -- close (or a nested @[@/@{@) is found.
    collect afterBr ls =
      case breakClose (intercalate "\n" (afterBr : ls)) of
        Nothing -> Nothing
        Just (body, after) ->
          let (trailer, restPart) = break (== '\n') after
              rest = case restPart of ('\n' : r) -> splitNL r; _ -> []
           in Just (body, trailer, rest)

    -- Scan to the top-level @]@: track string literals (with escapes) and skip
    -- @#@-comments; bail (Nothing) on a nested @[@/@{@ at the top level.
    breakClose = goC "" False
      where
        goC _ _ [] = Nothing
        goC acc True (c : cs) = case c of
          '\\' -> case cs of (d : ds) -> goC (d : '\\' : acc) True ds; [] -> Nothing
          '"'  -> goC ('"' : acc) False cs
          _    -> goC (c : acc) True cs
        goC acc False (c : cs) = case c of
          '"' -> goC ('"' : acc) True cs
          '#' -> let (cmt, r) = break (== '\n') cs in goC (reverse cmt ++ ('#' : acc)) False r
          ']' -> Just (reverse acc, cs)
          '[' -> Nothing
          '{' -> Nothing
          _   -> goC (c : acc) False cs

    -- Parse a body into value/comment parts; 'Nothing' on a nested bracket/table.
    parseParts = part . skip
      where
        skip = dropWhile (\c -> isSpace c || c == ',')
        part [] = Just []
        part s@(c : _)
          | c == '#'  = let (cmt, r) = break (== '\n') s in (ACmt (dropWhileEnd isSpace cmt) :) <$> part (skip r)
          | c == '"'  = do (v, r) <- readStr s; let (mc, r') = inlineComment r in (AVal v mc :) <$> part (skip r')
          | c == '['  = Nothing
          | c == '{'  = Nothing
          | otherwise = do (v, r) <- readBare s; let (mc, r') = inlineComment r in (AVal v mc :) <$> part (skip r')
        readStr ('"' : cs) = goS ['"'] cs
          where
            goS _ [] = Nothing
            goS acc ('\\' : d : ds) = goS (d : '\\' : acc) ds
            goS acc ('"' : ds) = Just (reverse ('"' : acc), ds)
            goS acc (x : xs) = goS (x : acc) xs
        readStr _ = Nothing
        readBare s = case break (\c -> c == ',' || c == '#' || isSpace c) s of
          ("", _)    -> Nothing
          (tok, r)   -> Just (tok, r)
        -- An inline comment is on the SAME line as the value (after optional
        -- spaces + a comma). No comment → return the input unchanged.
        inlineComment s =
          let s1 = dropWhile (`elem` " \t") s
              s2 = case s1 of (',' : r) -> dropWhile (`elem` " \t") r; _ -> s1
           in case s2 of
                ('#' : _) -> let (cmt, r) = break (== '\n') s2 in (Just (dropWhileEnd isSpace cmt), r)
                _         -> (Nothing, s)

    emit indent keyPart parts trailer
      | length vals <= 1 && null cmts && all noInline parts =
          [indent ++ keyPart ++ intercalate ", " vals ++ "]" ++ rtrailer]
      | otherwise =
          [indent ++ keyPart] ++ map (itemLine indent) parts ++ [indent ++ "]" ++ rtrailer]
      where
        vals = [v | AVal v _ <- parts]
        cmts = [c | ACmt c <- parts]
        noInline (AVal _ (Just _)) = False
        noInline _ = True
        rtrailer = case dropWhile isSpace trailer of "" -> ""; t -> "  " ++ t
        itemLine ind (AVal v mc) = ind ++ "  " ++ v ++ "," ++ maybe "" ("  " ++) mc
        itemLine ind (ACmt c)    = ind ++ "  " ++ c

    splitNL s = case break (== '\n') s of (a, '\n' : r) -> a : splitNL r; (a, _) -> [a]

-- | A parsed array element for 'reflowArrays': a value (with an optional inline
-- comment) or a standalone comment line.
data ArrPart = AVal String (Maybe String) | ACmt String

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
