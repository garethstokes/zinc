-- | The dependency resolver: walk the self-describing git dependency graph
-- (spec §2). Each package's manifest declares its own deps /and/ their repos,
-- so the graph is discovered by walking, not hand-listed. Exactly one git ref
-- is chosen per package name; there is no version solver.
--
-- Manifest fetching is injected (a @name -> repo -> ref -> manifest@ function)
-- so the walk is testable without git; the real git-backed fetch is wired
-- separately.
module Zinc.Resolve
  ( DepManifest (..)
  , ResolvedDep (..)
  , resolve
  , topoSort
  , isBootLib
  ) where

import Control.Monad (foldM)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Zinc.Manifest (Dependency (..), Ref)

-- | What a fetched package declares about its own dependencies: their pins
-- (@[dependencies]@) and where they live (@[registry]@).
data DepManifest = DepManifest
  { dmDeps     :: [Dependency]
  , dmRegistry :: [(String, String)]
  }
  deriving (Eq, Show)

-- | A resolved node in the closure.
data ResolvedDep = ResolvedDep
  { rdName    :: String
  , rdRepo    :: String
  , rdRef     :: Ref
  , rdDepends :: [String] -- ^ non-boot direct dep names (for later topo sort)
  }
  deriving (Eq, Show)

-- | An item of work: resolve this name, pinned to this ref, from this repo.
data Req = Req String Ref String

-- | Resolve a workspace's direct dependencies into the full transitive
-- closure. Boot libraries (per @isBoot@) ship with GHC and are excluded.
-- Conflicts collapse to one ref per name: the first resolution wins, and
-- because root deps are walked first, a root pin overrides a transitive one.
resolve
  :: Monad m
  => (String -> Bool)                                            -- ^ is this a GHC boot lib?
  -> (String -> String -> Ref -> m (Either String DepManifest))  -- ^ fetch: name repo ref
  -> [Dependency]                                                -- ^ root @[dependencies]@
  -> [(String, String)]                                          -- ^ root @[registry]@
  -> m (Either String [ResolvedDep])
resolve isBoot fetch rootDeps rootReg =
  case traverse (toReq rootReg "<workspace>") rootDeps of
    Left err   -> pure (Left err)
    Right reqs -> go Map.empty reqs
  where
    toReq reg parent d = case lookup (depName d) reg of
      Just repo -> Right (Req (depName d) (depRef d) repo)
      Nothing ->
        Left ("no repo in [registry] for '" ++ depName d ++ "' (required by " ++ parent ++ ")")

    go seen [] = pure (Right (Map.elems seen))
    go seen (Req name ref repo : rest)
      | isBoot name            = go seen rest
      | name `Map.member` seen = go seen rest -- one ref per name; first/root wins
      | otherwise = do
          fetched <- fetch name repo ref
          case fetched of
            Left err -> pure (Left err)
            Right dm ->
              let transitive = filter (not . isBoot . depName) (dmDeps dm)
                  node = ResolvedDep name repo ref (map depName transitive)
               in case traverse (toReq (dmRegistry dm) name) transitive of
                    Left err      -> pure (Left err)
                    Right newReqs -> go (Map.insert name node seen) (rest ++ newReqs)

-- | Topologically sort a resolved closure so each package appears after all
-- the in-closure dependencies it builds against (build order). Dependency
-- names not in the closure (e.g. boot libs) are ignored. Fails on a cycle —
-- GHC cannot build cyclic package dependencies.
topoSort :: [ResolvedDep] -> Either String [ResolvedDep]
topoSort nodes = do
  (_, ordered) <- foldM (visit Set.empty) (Set.empty, []) (map rdName nodes)
  pure (map (byName Map.!) (reverse ordered))
  where
    byName = Map.fromList [(rdName n, n) | n <- nodes]
    depsOf name = maybe [] (filter (`Map.member` byName) . rdDepends) (Map.lookup name byName)

    -- DFS post-order with a path set for cycle detection.
    visit
      :: Set String                 -- names on the current DFS path
      -> (Set String, [String])     -- (finished, reverse build order)
      -> String
      -> Either String (Set String, [String])
    visit path acc@(done, _) name
      | name `Set.member` done = Right acc
      | name `Set.member` path = Left ("dependency cycle involving '" ++ name ++ "'")
      | otherwise = do
          (done', order') <- foldM (visit (Set.insert name path)) acc (depsOf name)
          pure (Set.insert name done', name : order')

-- | The GHC boot libraries that ship with the compiler and are never fetched
-- (spec §2). The production 'resolve' uses this as its @isBoot@ predicate.
-- (Deriving this from @ghc-pkg list@ in the Nix env is a future refinement.)
isBootLib :: String -> Bool
isBootLib = (`elem` bootLibs)
  where
    bootLibs =
      [ "base", "ghc-prim", "ghc-bignum", "integer-gmp", "template-haskell"
      , "array", "binary", "bytestring", "containers", "deepseq", "directory"
      , "exceptions", "filepath", "ghc-boot", "ghc-boot-th", "ghc-heap", "ghci"
      , "mtl", "parsec", "pretty", "process", "stm", "text", "time"
      , "transformers", "unix", "Cabal", "Cabal-syntax", "rts"
      ]
