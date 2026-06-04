-- | The real git-backed manifest fetch for the resolver: clone a dependency
-- at its ref and parse its @[dependencies]@/@[registry]@ into a 'DepManifest'.
-- This is the production implementation of the fetch function that
-- "Zinc.Resolve".'Zinc.Resolve.resolve' takes as a parameter.
module Zinc.Fetch
  ( gitFetchManifest
  , resolveRef
  ) where

import Control.Monad (when)
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import Zinc.Git (cloneAt, listTags, splitRepoSubdir)
import Zinc.Manifest (Ref (..), parseDependencies)
import Zinc.Resolve (DepManifest (..))
import Zinc.Version (newestTag)

-- | Fetch a dependency's manifest: clone @repo@ at @ref@ into the store and
-- read its @zinc.toml@. Matches the fetch signature 'Zinc.Resolve.resolve'
-- expects (@name -> repo -> ref -> m (Either String DepManifest)@).
gitFetchManifest :: FilePath -> String -> String -> Ref -> IO (Either String DepManifest)
gitFetchManifest storeRoot name repo ref = do
  resolved <- resolveRef repo ref
  case resolved of
    Left err -> pure (Left (name ++ ": " ++ err))
    Right refStr -> do
      let dest = storeRoot </> "checkout" </> name
      stale <- doesDirectoryExist dest
      when stale (removeDirectoryRecursive dest)
      cloned <- cloneAt repo refStr dest
      case cloned of
        Left err -> pure (Left ("fetch " ++ name ++ ": " ++ err))
        Right _rev -> do
          let pkgDir = maybe dest (dest </>) (snd (splitRepoSubdir repo))
              manifest = pkgDir </> "zinc.toml"
          present <- doesFileExist manifest
          if not present
            then pure (Left (name ++ ": no zinc.toml in " ++ repo))
            else do
              src <- readFile manifest
              pure $ case parseDependencies src of
                Left err          -> Left (name ++ ": " ++ err)
                Right (deps, reg) -> Right (DepManifest deps reg)

-- | The git checkout target for a ref. 'Latest' is resolved to the repo's
-- newest release tag.
resolveRef :: String -> Ref -> IO (Either String String)
resolveRef _    (Tag t)    = pure (Right t)
resolveRef _    (Branch b) = pure (Right b)
resolveRef _    (Rev r)    = pure (Right r)
resolveRef repo Latest     = do
  tags <- listTags (fst (splitRepoSubdir repo))
  pure $ case tags of
    Left err -> Left err
    Right ts -> maybe (Left "no release tags found") Right (newestTag ts)
