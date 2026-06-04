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
  , topoLevels
  , isBootLib
  ) where

import Control.Monad (foldM)
import Control.Monad.Trans.Except (ExceptT (ExceptT), except, runExceptT)
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
resolve isBoot fetch rootDeps rootReg = runExceptT $ do
  reqs <- except (traverse (toReq rootReg "<workspace>") rootDeps)
  go Map.empty reqs
  where
    -- A dep's repo comes from the declaring package's own @[registry]@ first,
    -- then falls back to the root workspace registry. Real upstreams (only a
    -- .cabal, no zinc.toml) carry no registry, so the root workspace must list
    -- the repos for the whole non-boot closure.
    toReq reg parent d = case lookup (depName d) (reg ++ rootReg) of
      Just repo -> Right (Req (depName d) (depRef d) repo)
      Nothing ->
        Left ("no repo in [registry] for '" ++ depName d ++ "' (required by " ++ parent ++ ")")

    go seen [] = pure (Map.elems seen)
    go seen (Req name ref repo : rest)
      | isBoot name            = go seen rest
      | name `Map.member` seen = go seen rest -- one ref per name; first/root wins
      | otherwise = do
          dm <- ExceptT (fetch name repo ref)
          let transitive = filter (not . isBoot . depName) (dmDeps dm)
              node = ResolvedDep name repo ref (map depName transitive)
          newReqs <- except (traverse (toReq (dmRegistry dm) name) transitive)
          go (Map.insert name node seen) (rest ++ newReqs)

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

-- | Group a resolved closure into dependency /levels/ for parallel building:
-- level 0 has the nodes with no in-closure dependencies, and each later level
-- holds nodes whose every in-closure dependency sits in an earlier level.
-- Nodes within a level are mutually independent, so they can be compiled
-- concurrently; flattening the levels yields a valid 'topoSort' order. Fails on
-- a cycle (no node ever becomes ready). Input order is preserved within levels.
topoLevels :: [ResolvedDep] -> Either String [[ResolvedDep]]
topoLevels nodes = go Set.empty (map rdName nodes) []
  where
    byName = Map.fromList [(rdName n, n) | n <- nodes]
    depsOf name = maybe [] (filter (`Map.member` byName) . rdDepends) (Map.lookup name byName)

    go _ [] acc = Right (reverse acc)
    go done remaining acc =
      let ready = [name | name <- remaining, all (`Set.member` done) (depsOf name)]
       in if null ready
            then Left ("dependency cycle among: " ++ unwords remaining)
            else
              go
                (foldr Set.insert done ready)
                (filter (`notElem` ready) remaining)
                (map (byName Map.!) ready : acc)

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
