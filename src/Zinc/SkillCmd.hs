-- | @zinc skill@ subcommands (zinc-dp6.2+): the IO actions that install Claude
-- Code skills git-native, pinned + content-verified, reusing zinc's resolve/
-- fetch/store/lock — with NO Haskell build step (the Nix preflight is never
-- invoked). Pure data + parsers live in "Zinc.Skill"; this module is the
-- effectful command layer.
module Zinc.SkillCmd
  ( runSkillAdd
  , skillStorePath
  , skillRepoName
  , writeSkillLockEntry
  ) where

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.List (isSuffixOf)
import System.Directory (createDirectoryIfMissing, createDirectoryLink, doesDirectoryExist, doesFileExist, doesPathExist, removeDirectoryRecursive, removePathForcibly)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (readFile')
import Zinc.Diagnostic (ZincError)
import Zinc.Except (failWith, liftEither, liftIO, orFail, runResult)
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
      ref      = parseSkillRef mref
  -- Resolve the ref (tag/branch/latest) to a concrete ref, clone at it.
  refStr <- orFail (first ((provName ++ ": ") ++) <$> resolveRef provName repo ref)
  let dest = skillStorePath storeRoot repoUrl refStr
  liftIO $ do
    stale <- doesDirectoryExist dest
    when stale (removeDirectoryRecursive dest)
  rev <- orFail (first (("fetch " ++ provName ++ ": ") ++) <$> cloneAt repoUrl refStr dest)
  sha <- liftIO (contentHash dest)
  -- A skill MUST carry a SKILL.md at its root (spec §5); its frontmatter names it.
  let skillMd = dest </> "SKILL.md"
  hasMd <- liftIO (doesFileExist skillMd)
  when (not hasMd) (failWith (provName ++ ": no SKILL.md at the repo root (not a Claude Code skill)"))
  md <- liftIO (readFile skillMd)
  (name, desc) <- liftEither (first ((provName ++ ": ") ++) (readSkillFrontmatter md))
  liftIO $ do
    -- Pin in the lock (the source of truth `zinc skill sync` re-materializes from).
    writeSkillLockEntry (wsDir </> "zinc.lock") (LockedSkill name repoUrl rev sha)
    -- Materialize: symlink the content-store tree into the agent's skills dir.
    let link = wsDir </> ".claude" </> "skills" </> name
    createDirectoryIfMissing True (takeDirectory link)
    present <- doesPathExist link
    when present (removePathForcibly link)
    createDirectoryLink dest link
  pure ("Installed skill " ++ name ++ " (" ++ desc ++ ") \8594 .claude/skills/" ++ name)

-- | The content-store location for a skill's checkout — its own @skill/@ area,
-- NOT the package @src/@ store, so the package GC (which only knows @[[locked]]@
-- packages) never sweeps a live skill out from under its symlink.
skillStorePath :: FilePath -> String -> String -> FilePath
skillStorePath storeRoot repo ref = storeRoot </> "skill" </> srcDirName repo ref

-- | The provisional skill name from a repo URL: its basename minus a @.git@
-- suffix (used only for resolution + error messages; the installed name is the
-- @SKILL.md@ frontmatter name).
skillRepoName :: String -> String
skillRepoName url = dropGit (takeFileName (dropTrailingSlash url))
  where
    dropTrailingSlash s = if not (null s) && last s == '/' then init s else s
    dropGit s = if ".git" `isSuffixOf` s then take (length s - 4) s else s

-- | Record a @[[skill]]@ entry in @zinc.lock@, replacing any existing entry of
-- the same name and PRESERVING the package @[[locked]]@ array (the two kinds
-- share one lockfile, each parser reading only its own). A strict read so the
-- handle is closed before the rewrite.
writeSkillLockEntry :: FilePath -> LockedSkill -> IO ()
writeSkillLockEntry lockFile sk = do
  exists <- doesFileExist lockFile
  src <- if exists then readFile' lockFile else pure ""
  let pkgs    = either (const []) id (parseLock src)
      skills  = either (const []) id (parseSkillLock src)
      skills' = sk : filter ((/= lskName sk) . lskName) skills
      pkgText = renderLock pkgs
      out     = pkgText ++ (if null pkgs then "" else "\n") ++ renderSkillLock skills'
  writeFile lockFile out
