-- | Local dependency overrides (zinc-g1b): a per-developer, git-ignored redirect
-- of a git dependency to a LIVE local checkout, so uncommitted working-tree edits
-- flow straight into the build — like cargo @[patch]@, nix @--override-input@, go
-- @replace@. Overrides live in @zinc.local.toml@ (git-ignored) and affect ONLY
-- the build: they are never frozen into @zinc.lock@, so the committed
-- manifest+lock stay byte-for-byte reproducible for everyone else, and a clean
-- clone (no @zinc.local.toml@) gets the locked, content-verified build.
module Zinc.Override
  ( loadOverrides
  , parseOverrides
  , overrideLocalFile
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Toml
import Toml.Value (Value (String))
import Zinc.TOML (subTable)

-- | The git-ignored override file zinc reads, relative to the workspace root.
overrideLocalFile :: FilePath
overrideLocalFile = "zinc.local.toml"

-- | Parse the @[overrides]@ table of a @zinc.local.toml@ into a @name -> path@
-- list (a dependency name mapped to a local checkout, optionally @path#subdir@
-- for a monorepo). Non-string entries and a missing table yield none; an
-- unparseable file is ignored (empty), so a malformed local file never breaks a
-- build for the rest of the team.
parseOverrides :: String -> [(String, FilePath)]
parseOverrides src = case Toml.parse src of
  Left _    -> []
  Right top -> [(k, p) | (k, String p) <- Map.toList (overridesTable top)]
  where
    overridesTable :: Map String Value -> Map String Value
    overridesTable = subTable "overrides"

-- | Load the workspace's local dependency overrides from
-- @\<wsDir\>/zinc.local.toml@ (git-ignored). Empty when the file is absent. Used
-- only by the build path; @add@/@update@ ignore it so the lock keeps the real
-- git pin.
loadOverrides :: FilePath -> IO [(String, FilePath)]
loadOverrides wsDir = do
  let f = wsDir </> overrideLocalFile
  there <- doesFileExist f
  if there then parseOverrides <$> readFile f else pure []
