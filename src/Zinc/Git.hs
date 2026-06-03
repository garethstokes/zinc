-- | Thin wrapper over the @git@ CLI for fetching dependency sources.
-- zinc fetches Haskell source deps with plain git (spec §2); Nix is not
-- involved in fetching.
module Zinc.Git
  ( cloneAt
  , listTags
  ) where

import Data.Char (isSpace)
import Data.List (stripPrefix)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | Clone @repo@ into @dest@, check out @ref@ (a tag, branch, or commit), and
-- return the exact commit SHA it resolved to. @dest@ must not already exist.
cloneAt :: String -> String -> FilePath -> IO (Either String String)
cloneAt repo ref dest =
  step (git ["clone", "--quiet", repo, dest]) $ \_ ->
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
  (code, out, err) <- readProcessWithExitCode cmd args ""
  pure $ case code of
    ExitSuccess   -> Right out
    ExitFailure _ -> Left (trim (if null err then out else err))

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace
