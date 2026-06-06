-- | @zinc skill@ subcommands (zinc-dp6.2–.4): the IO actions that install,
-- inspect, and re-materialize Claude Code skills git-native, pinned +
-- content-verified, reusing zinc's resolve/fetch/store/lock — with NO Haskell
-- build step (the Nix preflight is never invoked). Pure data + parsers live in
-- "Zinc.Skill"; this module is the effectful command layer.
module Zinc.SkillCmd
  ( runSkillAdd
  , runSkillList
  , runSkillRemove
  , runSkillSync
  , skillStorePath
  , skillRepoName
  , writeSkillLockEntry
  , renderSkillList
  ) where

import Control.Monad (forM, when)
import Data.Bifunctor (first)
import Data.List (isSuffixOf)
import System.Directory (createDirectoryIfMissing, createDirectoryLink, doesDirectoryExist, doesFileExist, doesPathExist, removeDirectoryRecursive, removePathForcibly, renameDirectory)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (readFile')
import Zinc.Diagnostic (ZincError)
import Zinc.Except (Result, failWith, liftEither, liftIO, orFail, runResult)
import Zinc.Fetch (resolveRef)
import Zinc.Git (cloneAt, splitRepoSubdir)
import Zinc.Lock (parseLock, renderLock)
import Zinc.Skill (LockedSkill (..), parseSkillLock, parseSkillRef, readSkillFrontmatter, renderSkillLock)
import Zinc.Store (contentHash, resolveStoreRoot, srcDirName)

-- | @zinc skill add \<repo\> [--ref \<ref\>]@ (spec §3): resolve the ref, clone
-- the repo into the content store, content-hash it, read its @SKILL.md@
-- frontmatter, record a @[[skill]]@ entry in @zinc.lock@, and symlink the store
-- tree into @.claude/skills/\<name\>@. The skill's own @SKILL.md@ name is the
-- install-directory name. Never touches Nix/ghc.
runSkillAdd :: String -> Maybe String -> FilePath -> IO (Either ZincError String)
runSkillAdd repo mref wsDir = runResult $ do
  storeRoot <- liftIO resolveStoreRoot
  let repoUrl  = fst (splitRepoSubdir repo)
      provName = skillRepoName repoUrl -- provisional name for resolve/errors (real name is in SKILL.md)
  refStr <- orFail (first ((provName ++ ": ") ++) <$> resolveRef provName repo (parseSkillRef mref))
  -- Clone to a staging dir, then move under the COMMIT-keyed store path so `add`
  -- and `sync` (which only knows the locked commit) agree on the location.
  let staging = storeRoot </> "skill" </> (".staging-" ++ srcDirName repoUrl refStr)
  liftIO (doesDirectoryExist staging >>= \s -> when s (removeDirectoryRecursive staging))
  rev <- orFail (first (("fetch " ++ provName ++ ": ") ++) <$> cloneAt repoUrl refStr staging)
  let dest = skillStorePath storeRoot repoUrl rev
  liftIO $ do
    have <- doesDirectoryExist dest
    if have
      then removeDirectoryRecursive staging
      else createDirectoryIfMissing True (takeDirectory dest) >> renameDirectory staging dest
  sha <- liftIO (contentHash dest)
  (name, desc) <- readSkillIdentity provName dest
  liftIO $ do
    writeSkillLockEntry (wsDir </> "zinc.lock") (LockedSkill name repoUrl rev sha)
    linkSkill wsDir name dest
  pure ("Installed skill " ++ name ++ " (" ++ desc ++ ") \8594 .claude/skills/" ++ name)

-- | @zinc skill list@ (spec §3): the installed skills recorded in @zinc.lock@.
runSkillList :: FilePath -> IO (Either ZincError [LockedSkill])
runSkillList wsDir = runResult (readLockedSkills wsDir)

-- | @zinc skill remove \<name\>@ (spec §3): drop the skill's symlink and its
-- @[[skill]]@ lock entry (the store clone is left for the GC / other links).
runSkillRemove :: String -> FilePath -> IO (Either ZincError String)
runSkillRemove name wsDir = runResult $ do
  skills <- readLockedSkills wsDir
  when (name `notElem` map lskName skills) (failWith (name ++ ": not an installed skill"))
  liftIO $ do
    let link = wsDir </> ".claude" </> "skills" </> name
    present <- doesPathExist link
    when present (removePathForcibly link)
    rewriteSkills (wsDir </> "zinc.lock") (filter ((/= name) . lskName))
  pure ("Removed skill " ++ name)

-- | @zinc skill sync@ (spec §3): re-materialize every locked skill — ensure its
-- pinned commit is in the store (clone if missing), verify the content hash, and
-- (re)create its symlink. The payoff: commit @zinc.lock@, and a fresh checkout
-- gets the exact pinned, hash-verified skill set. Returns the synced names.
runSkillSync :: FilePath -> IO (Either ZincError [String])
runSkillSync wsDir = runResult $ do
  storeRoot <- liftIO resolveStoreRoot
  skills <- readLockedSkills wsDir
  forM skills $ \sk -> do
    let dest = skillStorePath storeRoot (lskRepo sk) (lskRev sk)
    have <- liftIO (doesDirectoryExist dest)
    when (not have) $ do
      let staging = dest ++ ".staging"
      liftIO (doesDirectoryExist staging >>= \s -> when s (removeDirectoryRecursive staging))
      _ <- orFail (first (("fetch " ++ lskName sk ++ ": ") ++) <$> cloneAt (lskRepo sk) (lskRev sk) staging)
      liftIO (createDirectoryIfMissing True (takeDirectory dest) >> renameDirectory staging dest)
    got <- liftIO (contentHash dest)
    when (got /= lskSha256 sk) $
      failWith (lskName sk ++ ": content hash mismatch (lock " ++ lskSha256 sk ++ ", got " ++ got ++ ")")
    liftIO (linkSkill wsDir (lskName sk) dest)
    pure (lskName sk)

-- | The content-store location for a skill's checkout — its own @skill/@ area,
-- NOT the package @src/@ store, so the package GC (which only knows @[[locked]]@
-- packages) never sweeps a live skill out from under its symlink. Keyed by the
-- resolved commit so add + sync agree.
skillStorePath :: FilePath -> String -> String -> FilePath
skillStorePath storeRoot repo rev = storeRoot </> "skill" </> srcDirName repo rev

-- | The provisional skill name from a repo URL: its basename minus a @.git@
-- suffix (used only for resolution + error messages; the installed name is the
-- @SKILL.md@ frontmatter name).
skillRepoName :: String -> String
skillRepoName url = dropGit (takeFileName (dropTrailingSlash url))
  where
    dropTrailingSlash s = if not (null s) && last s == '/' then init s else s
    dropGit s = if ".git" `isSuffixOf` s then take (length s - 4) s else s

-- | A one-skill-per-line listing: @name  rev  repo@, aligned (spec §3).
renderSkillList :: [LockedSkill] -> String
renderSkillList [] = "No skills installed (zinc skill add <repo>).\n"
renderSkillList sks = unlines (header : map row sks)
  where
    nameW = maximum (4 : map (length . lskName) sks)
    revW = 12
    pad w s = s ++ replicate (max 0 (w - length s)) ' '
    header = pad nameW "name" ++ "  " ++ pad revW "rev" ++ "  repo"
    row s = pad nameW (lskName s) ++ "  " ++ pad revW (take 12 (lskRev s)) ++ "  " ++ lskRepo s

-- Read the [[skill]] entries from the workspace lock (empty if no lock).
readLockedSkills :: FilePath -> Result [LockedSkill]
readLockedSkills wsDir = do
  let lockFile = wsDir </> "zinc.lock"
  exists <- liftIO (doesFileExist lockFile)
  src <- liftIO (if exists then readFile' lockFile else pure "")
  liftEither (parseSkillLock src)

-- Read + validate a cloned skill's SKILL.md identity (name, description).
readSkillIdentity :: String -> FilePath -> Result (String, String)
readSkillIdentity provName dest = do
  let skillMd = dest </> "SKILL.md"
  hasMd <- liftIO (doesFileExist skillMd)
  when (not hasMd) (failWith (provName ++ ": no SKILL.md at the repo root (not a Claude Code skill)"))
  md <- liftIO (readFile skillMd)
  liftEither (first ((provName ++ ": ") ++) (readSkillFrontmatter md))

-- (Re)create the symlink @.claude/skills/<name> -> <store tree>@.
linkSkill :: FilePath -> String -> FilePath -> IO ()
linkSkill wsDir name dest = do
  let link = wsDir </> ".claude" </> "skills" </> name
  createDirectoryIfMissing True (takeDirectory link)
  present <- doesPathExist link
  when present (removePathForcibly link)
  createDirectoryLink dest link

-- | Record a @[[skill]]@ entry in @zinc.lock@, replacing any existing entry of
-- the same name and PRESERVING the package @[[locked]]@ array.
writeSkillLockEntry :: FilePath -> LockedSkill -> IO ()
writeSkillLockEntry lockFile sk =
  rewriteSkills lockFile (\sks -> sk : filter ((/= lskName sk) . lskName) sks)

-- Rewrite a lockfile's @[[skill]]@ array via @f@, preserving the @[[locked]]@
-- packages (the two kinds share one file; each parser reads only its own array).
-- A strict read so the handle is closed before the rewrite.
rewriteSkills :: FilePath -> ([LockedSkill] -> [LockedSkill]) -> IO ()
rewriteSkills lockFile f = do
  exists <- doesFileExist lockFile
  src <- if exists then readFile' lockFile else pure ""
  let pkgs    = either (const []) id (parseLock src)
      skills  = f (either (const []) id (parseSkillLock src))
      out     = renderLock pkgs ++ (if null pkgs then "" else "\n") ++ renderSkillLock skills
  writeFile lockFile out
