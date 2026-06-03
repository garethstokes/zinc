module Zinc.Scaffold
  ( FileSpec (..)
  , scaffoldNew
  , materialize
  ) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

-- | A file the scaffolder intends to create: a path plus its contents.
-- Keeping this pure (no IO) makes `zinc new` fully testable; the actual
-- writing is a thin shell around 'scaffoldNew'.
data FileSpec = FileSpec
  { specPath :: FilePath
  , specBody :: String
  }
  deriving (Eq, Show)

-- | Pure plan for @zinc new \<name\>@: the files a fresh single-member
-- workspace needs. Workspaces are first-class from day one (spec §4), so even
-- a one-package project gets a workspace root.
scaffoldNew :: String -> [FileSpec]
scaffoldNew name =
  [ FileSpec "zinc.toml" workspaceManifest
  , FileSpec (memberDir ++ "/zinc.toml") memberManifest
  , FileSpec (memberDir ++ "/app/Main.hs") mainModule
  ]
  where
    memberDir = "packages/" ++ name

    workspaceManifest =
      unlines
        [ "[workspace]"
        , "members = [\"" ++ memberDir ++ "\"]"
        , "ghc = \"9.6.5\""
        , ""
        , "[dependencies]"
        , ""
        , "[registry]"
        ]

    memberManifest =
      unlines
        [ "[package]"
        , "name = \"" ++ name ++ "\""
        , "version = \"0.1.0\""
        , ""
        , "[build.exe." ++ name ++ "]"
        , "source-dirs = [\"app\"]"
        , "main = \"Main.hs\""
        , "depends = []"
        ]

    mainModule =
      unlines
        [ "module Main (main) where"
        , ""
        , "main :: IO ()"
        , "main = putStrLn \"Hello from " ++ name ++ "!\""
        ]

-- | Write a scaffold plan to disk under @root@, creating parent directories.
-- The thin IO shell around the pure 'scaffoldNew'.
materialize :: FilePath -> [FileSpec] -> IO ()
materialize root = mapM_ writeSpec
  where
    writeSpec (FileSpec p body) = do
      let full = root </> p
      createDirectoryIfMissing True (takeDirectory full)
      writeFile full body
