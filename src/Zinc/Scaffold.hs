module Zinc.Scaffold
  ( FileSpec (..)
  , scaffoldNew
  , scaffoldWorkspace
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

-- | Pure plan for @zinc new \<name\>@: a FLAT single-package project at the repo
-- root (zinc-6hf.1) — one @zinc.toml@ (@[workspace]@ member @"."@ + @[package]@ +
-- @[build.exe.\<name\>]@ + @[dependencies]@) with @app\/Main.hs@ at the root, NO
-- @packages\/\<name\>\/@ nesting. A lone package is the implicit one-member
-- workspace (spec §4); 'scaffoldWorkspace' produces the multi-member layout.
scaffoldNew :: String -> [FileSpec]
scaffoldNew name =
  [ FileSpec "zinc.toml" manifest
  , FileSpec "app/Main.hs" (mainModule name)
  , FileSpec ".gitignore" gitignore
  ]
  where
    manifest =
      unlines
        [ "[workspace]"
        , "members = [\".\"]"
        , "ghc = \"9.6.5\""
        , ""
        , "[package]"
        , "name = \"" ++ name ++ "\""
        , "version = \"0.1.0\""
        , ""
        , "[build.exe." ++ name ++ "]"
        , "source-dirs = [\"app\"]"
        , "main = \"Main.hs\""
        , "depends = []"
        , ""
        , "[dependencies]"
        ]

-- | Pure plan for @zinc new --workspace \<name\>@: the multi-member layout — a
-- workspace-root @zinc.toml@ plus a nested @packages\/\<name\>\/@ member.
scaffoldWorkspace :: String -> [FileSpec]
scaffoldWorkspace name =
  [ FileSpec "zinc.toml" workspaceManifest
  , FileSpec (memberDir ++ "/zinc.toml") memberManifest
  , FileSpec (memberDir ++ "/app/Main.hs") (mainModule name)
  , FileSpec ".gitignore" gitignore
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

-- | The @.gitignore@ shared by both scaffolds (zinc-6hf.3): zinc's build dir and
-- Nix's @result@ symlinks plus stray GHC artifacts.
gitignore :: String
gitignore =
  unlines
    [ ".zinc/"
    , "result"
    , "result-*"
    , "*.hi"
    , "*.o"
    ]

-- | The placeholder @app\/Main.hs@ shared by both scaffolds.
mainModule :: String -> String
mainModule name =
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
