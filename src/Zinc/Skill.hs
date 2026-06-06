-- | Skills as a package kind (zinc-dp6, thin experiment): the data model +
-- parsers for installing Claude Code \"skills\" (a directory with a @SKILL.md@)
-- git-native, pinned, and content-verified — reusing zinc's resolve/fetch/store/
-- lock, with no Haskell build step (Nix is never invoked). This module is the
-- FOUNDATION (zinc-dp6.1): the @[skills]@ manifest table, the @[[skill]]@ lock
-- block, and the @SKILL.md@ frontmatter reader. The @zinc skill@ subcommands
-- (add/list/remove/sync) build on these (dp6.2–.4). Spec:
-- docs/superpowers/specs/2026-06-04-zinc-skills-experiment-design.md.
module Zinc.Skill
  ( SkillDep (..)
  , LockedSkill (..)
  , parseSkills
  , parseSkillRef
  , parseSkillLock
  , renderSkillLock
  , readSkillFrontmatter
  ) where

import Data.Char (isHexDigit, isSpace)
import Data.List (dropWhileEnd, intercalate)
import qualified Data.Map as Map
import qualified Toml
import Toml.Value (Value (..))
import Zinc.Manifest (Ref (..))
import Zinc.TOML (stringField)

-- | A skill dependency declared in the workspace @[skills]@ table:
-- @name = { repo = "…", ref = "v1" }@. The @name@ is the manifest key (the
-- frozen lock's name comes from the skill's own @SKILL.md@; dp6.2).
data SkillDep = SkillDep
  { skName :: String
  , skRepo :: String
  , skRef  :: Ref
  }
  deriving (Eq, Show)

-- | A frozen skill in @zinc.lock@: a @[[skill]]@ block pinning the resolved
-- commit + content hash. Kept separate from 'Zinc.Lock.LockedPackage' so the
-- experiment can't perturb the package lock model.
data LockedSkill = LockedSkill
  { lskName   :: String
  , lskRepo   :: String
  , lskRev    :: String
  , lskSha256 :: String
  }
  deriving (Eq, Show)

-- | Parse the @[skills]@ table from a workspace manifest. Each entry is an
-- inline table with a required @repo@ and an optional @ref@ (default: latest).
-- An absent @[skills]@ table means no skills. Mirrors 'Zinc.Manifest.parseDeps'
-- but for the skill kind.
parseSkills :: String -> Either String [SkillDep]
parseSkills src = do
  top <- Toml.parse src
  case Map.lookup "skills" top of
    Nothing          -> Right []
    Just (Table t)   -> mapM skillOf (Map.toList t)
    Just _           -> Left "expected a [skills] table"
  where
    skillOf (name, Table t) = do
      repo <- stringField "repo" t
      pure (SkillDep name repo (skillRefOf t))
    skillOf (name, String s) = Right (SkillDep name s Latest) -- shorthand: name = "repo"
    skillOf (name, _)        = Left (name ++ ": expected { repo, ref } or a repo string")

-- | The ref a @[skills]@ entry pins to. A @ref@ string is a tag (the dominant
-- skill-versioning form), unless it is @*@ (latest) or a full hex commit (rev);
-- explicit @tag@/@branch@/@rev@ keys also work. Absent → latest.
skillRefOf :: Map.Map String Value -> Ref
skillRefOf t
  | Just (String s) <- Map.lookup "ref" t        = parseSkillRef (Just s)
  | Just (String "*") <- Map.lookup "tag" t       = Latest
  | Just (String s) <- Map.lookup "tag" t          = Tag s
  | Just (String s) <- Map.lookup "branch" t       = Branch s
  | Just (String s) <- Map.lookup "rev" t          = Rev s
  | otherwise                                      = Latest

-- | Interpret a @--ref@ / @ref =@ string as a git ref: @*@ (or absent) is
-- latest, a full hex string is a commit, anything else a tag (the dominant
-- skill-versioning form). Branches use an explicit @branch =@ key in the table.
parseSkillRef :: Maybe String -> Ref
parseSkillRef Nothing      = Latest
parseSkillRef (Just "*")   = Latest
parseSkillRef (Just s)
  | length s >= 7 && all isHexDigit s = Rev s
  | otherwise                         = Tag s

-- | Parse the @[[skill]]@ array from a lockfile. An absent array means none.
-- Mirrors 'Zinc.Lock.parseLock'.
parseSkillLock :: String -> Either String [LockedSkill]
parseSkillLock src = do
  top <- Toml.parse src
  case Map.lookup "skill" top of
    Nothing         -> Right []
    Just (Array xs) -> mapM toLocked xs
    Just _          -> Left "expected an array of [[skill]] tables"
  where
    toLocked (Table t) =
      LockedSkill
        <$> stringField "name" t
        <*> stringField "repo" t
        <*> stringField "rev" t
        <*> stringField "sha256" t
    toLocked _ = Left "expected a table in the [[skill]] array"

-- | Render frozen skills back to @[[skill]]@ TOML blocks. Round-trips with
-- 'parseSkillLock'. (zinc.lock can hold both @[[locked]]@ packages and
-- @[[skill]]@ skills; each parser reads only its own array.)
renderSkillLock :: [LockedSkill] -> String
renderSkillLock = intercalate "\n" . map renderOne
  where
    renderOne s =
      unlines
        [ "[[skill]]"
        , "name = " ++ str (lskName s)
        , "repo = " ++ str (lskRepo s)
        , "rev = " ++ str (lskRev s)
        , "sha256 = " ++ str (lskSha256 s)
        ]
    str v = "\"" ++ v ++ "\""

-- | Read a skill's identity from its @SKILL.md@ YAML frontmatter (the Claude
-- Code format), returning @(name, description)@. Both are required (spec §5);
-- the @name@ becomes the install-directory name. Lenient on CRLF and quoting.
readSkillFrontmatter :: String -> Either String (String, String)
readSkillFrontmatter src = do
  fm <- frontmatter
  (,) <$> field "name" fm <*> field "description" fm
  where
    -- The block between the leading `---` and the next `---`.
    frontmatter = case map stripCR (lines src) of
      ("---" : rest) -> Right (takeWhile (/= "---") rest)
      _              -> Left "SKILL.md: missing YAML frontmatter (expected a leading '---')"
    field k fls = case [unquote (trim v) | l <- fls, Just v <- [valueOf k l]] of
      (v : _) | not (null v) -> Right v
      _ -> Left ("SKILL.md: missing required frontmatter field '" ++ k ++ "'")
    valueOf k l = case break (== ':') l of
      (key, ':' : v) | trim key == k -> Just v
      _                              -> Nothing
    stripCR = dropWhileEnd (== '\r')
    trim = dropWhile isSpace . dropWhileEnd isSpace
    unquote v = case v of
      ('"' : rest)  | not (null rest) && last rest == '"'  -> init rest
      ('\'' : rest) | not (null rest) && last rest == '\'' -> init rest
      _                                                    -> v
