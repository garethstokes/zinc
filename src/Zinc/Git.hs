-- | Thin wrapper over the @git@ CLI for fetching dependency sources.
-- zinc fetches Haskell source deps with plain git (spec §2); Nix is not
-- involved in fetching.
module Zinc.Git
  ( cloneAt
  , listTags
  , splitRepoSubdir
  , gitEnv
  ) where

import Data.Char (isSpace)
import Data.List (stripPrefix)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode)

-- | Split a repo spec into its clone URL and an optional in-repo subdirectory,
-- encoded as a @url#subdir@ suffix. This lets a dependency point at a package
-- living inside a monorepo (e.g. @…\/prettyprinter#prettyprinter@); the whole
-- repo is still cloned, but the manifest/sources are read from the subdir.
splitRepoSubdir :: String -> (String, Maybe FilePath)
splitRepoSubdir spec = case break (== '#') spec of
  (url, '#' : sub) | not (null sub) -> (url, Just sub)
  _ -> (spec, Nothing)

-- | Clone @repo@ into @dest@, check out @ref@ (a tag, branch, or commit), and
-- return the exact commit SHA it resolved to. @dest@ must not already exist.
-- A @#subdir@ suffix on @repo@ is stripped for cloning (the whole repo is
-- fetched; the subdir is resolved by the caller against @dest@).
cloneAt :: String -> String -> FilePath -> IO (Either String String)
cloneAt repo ref dest =
  step (git ["clone", "--quiet", fst (splitRepoSubdir repo), dest]) $ \_ ->
    step (git ["-C", dest, "checkout", "--quiet", ref]) $ \_ ->
      fmap (fmap trim) (git ["-C", dest, "rev-parse", "--verify", "HEAD"])
  where
    git = run "git"

-- | List a repo's tag names (without the @refs/tags/@ prefix), via
-- @git ls-remote@ — works on local paths and remote URLs alike.
listTags :: String -> IO (Either String [String])
listTags repo = fmap (fmap parseTags) (run "git" ["ls-remote", "--tags", "--refs", repo])
  where
    parseTags out =
      [t | line <- lines out, (_ : ref : _) <- [words line], Just t <- [stripPrefix "refs/tags/" ref]]

-- | Chain an IO action that may fail, short-circuiting on 'Left'.
step :: IO (Either String a) -> (a -> IO (Either String b)) -> IO (Either String b)
step act k = act >>= either (pure . Left) k

run :: String -> [String] -> IO (Either String String)
run cmd args = do
  env' <- gitEnv <$> getEnvironment
  (code, out, err) <- readCreateProcessWithExitCode (proc cmd args) {env = Just env'} ""
  pure $ case code of
    ExitSuccess   -> Right out
    ExitFailure _ -> Left (trim (if null err then out else err))

-- | Augment the ambient environment with non-interactive guards so @git@ can
-- never block on a @\/dev\/tty@ credential or host-key prompt (fatal for
-- headless agents and CI): missing auth fails fast instead. zinc still
-- delegates authentication to the ambient git config (SSH agent, credential
-- helper, @insteadOf@) — this only makes the failure mode non-blocking. The
-- two guards override any inherited values; everything else (PATH, HOME, …) is
-- preserved.
gitEnv :: [(String, String)] -> [(String, String)]
gitEnv parent = guards ++ filter ((`notElem` map fst guards) . fst) parent
  where
    guards =
      [ ("GIT_TERMINAL_PROMPT", "0")
      , ("GIT_SSH_COMMAND", "ssh -o BatchMode=yes")
      ]

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace
