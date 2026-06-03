-- | The real git-backed manifest fetch for the resolver: clone a dependency
-- at its ref and parse its @[dependencies]@/@[registry]@ into a 'DepManifest'.
-- This is the production implementation of the fetch function that
-- "Zinc.Resolve".'Zinc.Resolve.resolve' takes as a parameter.
module Zinc.Fetch
  ( gitFetchManifest
  ) where

import Control.Monad (when)
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import Zinc.Git (cloneAt)
import Zinc.Manifest (Ref (..), parseDependencies)
import Zinc.Resolve (DepManifest (..))

-- | Fetch a dependency's manifest: clone @repo@ at @ref@ into the store and
-- read its @zinc.toml@. Matches the fetch signature 'Zinc.Resolve.resolve'
-- expects (@name -> repo -> ref -> m (Either String DepManifest)@).
gitFetchManifest :: FilePath -> String -> String -> Ref -> IO (Either String DepManifest)
gitFetchManifest storeRoot name repo ref =
  case refString ref of
    Left err -> pure (Left (name ++ ": " ++ err))
    Right refStr -> do
      let dest = storeRoot </> "checkout" </> name
      stale <- doesDirectoryExist dest
      when stale (removeDirectoryRecursive dest)
      cloned <- cloneAt repo refStr dest
      case cloned of
        Left err -> pure (Left ("fetch " ++ name ++ ": " ++ err))
        Right _rev -> do
          let manifest = dest </> "zinc.toml"
          present <- doesFileExist manifest
          if not present
            then pure (Left (name ++ ": no zinc.toml in " ++ repo))
            else do
              src <- readFile manifest
              pure $ case parseDependencies src of
                Left err          -> Left (name ++ ": " ++ err)
                Right (deps, reg) -> Right (DepManifest deps reg)

-- | The git checkout target for a ref. 'Latest' needs newest-tag discovery,
-- which is not yet implemented (tracked separately).
refString :: Ref -> Either String String
refString (Tag t)    = Right t
refString (Branch b) = Right b
refString (Rev r)    = Right r
refString Latest     = Left "Latest (*) ref resolution not yet implemented"
