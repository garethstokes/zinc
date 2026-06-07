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
import Data.Maybe (fromMaybe)
import Zinc.Diagnostic (ZincError (NoRepoInRegistry, OtherError), manyErrors)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Zinc.Manifest (Dependency (..), Ref, isVendored)

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
  => (String -> Bool)                                             -- ^ is this a GHC boot lib?
  -> (String -> String -> Ref -> m (Either ZincError DepManifest)) -- ^ fetch: name repo ref
  -> (String -> m (Maybe String))                                 -- ^ discover a repo (Hackage) when not in any registry
  -> [(String, Ref)]                                              -- ^ SOFT pins: hold these names at a ref WITHOUT forcing inclusion (90j.3)
  -> [Dependency]                                                 -- ^ root @[dependencies]@
  -> [(String, String)]                                           -- ^ root @[registry]@
  -> m (Either ZincError [ResolvedDep])
resolve isBoot fetch discoverRepo pins rootDeps rootReg = do
  -- Walk the WHOLE closure, accumulating every blocker (no repo, fetch failure)
  -- rather than failing on the first, so `zinc add` reports them all in one pass
  -- instead of forcing a fix/re-run cycle per blocker (zinc-91n.1). An
  -- unresolvable dep is recorded and its subtree skipped; resolvable branches
  -- continue.
  rootEithers <- resolveReqs rootReg "<workspace>" rootDeps
  let rootBlockers = [(n, e) | (n, Left e) <- rootEithers]
      rootReqs     = [r | (_, Right r) <- rootEithers]
  (seen, blockers) <- go Map.empty (Set.fromList (map fst rootBlockers)) (map snd rootBlockers) rootReqs
  pure $ if null blockers then Right (Map.elems seen) else Left (manyErrors (reverse blockers))
  where
    -- Resolve a batch of (non-boot) deps to fetch requests, paired with their
    -- name so blockers can be deduped by name (one report line per dep).
    resolveReqs reg parent = mapM (\d -> (,) (depName d) <$> toReq reg parent d)

    -- A dep's repo comes from the declaring package's own @[registry]@ first,
    -- then the root workspace registry, then — for real upstreams that carry no
    -- registry at all — Hackage @source-repository@ auto-discovery (zinc-49o):
    -- so the whole non-boot closure need not be hand-listed. Only when discovery
    -- also draws a blank is it a hard 'NoRepoInRegistry'.
    -- A vendored pin has no git repo to discover: its source is the Hackage
    -- tarball, fetched by name+version, so skip registry/Hackage lookup (b1z).
    -- A SOFT pin overrides a name's ref when it is walked, but never forces it
    -- into the closure — so @update \<pkg\>@ holds every other dep at its locked
    -- ref while letting deps the new \<pkg\> version drops fall out (90j.3).
    toReq reg parent d =
      let name = depName d
          ref = fromMaybe (depRef d) (lookup name pins)
       in if isVendored ref
            then pure (Right (Req name ref ""))
            else case lookup name (reg ++ rootReg) of
              Just repo -> pure (Right (Req name ref repo))
              Nothing -> do
                mRepo <- discoverRepo name
                pure $ case mRepo of
                  Just repo -> Right (Req name ref repo)
                  Nothing   -> Left (NoRepoInRegistry name parent)

    -- @go seen blocked blockers worklist@: @seen@ the resolved nodes, @blocked@
    -- the names already recorded as blockers (so a name reached via several
    -- parents blocks once), @blockers@ the accumulated errors (newest first).
    go seen _ blockers [] = pure (seen, blockers)
    go seen blocked blockers (Req name ref repo : rest)
      | isBoot name            = go seen blocked blockers rest
      | name `Map.member` seen || name `Set.member` blocked = go seen blocked blockers rest -- one ref per name; first/root wins
      | otherwise = do
          r <- fetch name repo ref
          case r of
            -- Fetch failed (e.g. no release tags, clone error): record + skip its
            -- subtree, but keep walking the rest of the closure (zinc-91n.1).
            Left err -> go seen (Set.insert name blocked) (err : blockers) rest
            Right dm -> do
              -- Exclude boot libs AND the package's own name: a package's .cabal
              -- can list itself (internal sub-libraries, e.g. attoparsec), which
              -- is a spurious self-edge, not a real closure dependency / cycle.
              let transitive = filter (\d -> depName d /= name && not (isBoot (depName d))) (dmDeps dm)
                  node = ResolvedDep name repo ref (map depName transitive)
              nameEithers <- resolveReqs (dmRegistry dm) name transitive
              let newBlockers = [(n, e) | (n, Left e) <- nameEithers, not (n `Set.member` blocked), not (n `Map.member` seen)]
                  newReqs     = [rq | (_, Right rq) <- nameEithers]
                  blocked'    = foldr (Set.insert . fst) blocked newBlockers
              go (Map.insert name node seen) blocked' (map snd newBlockers ++ blockers) (rest ++ newReqs)

-- | Topologically sort a resolved closure so each package appears after all
-- the in-closure dependencies it builds against (build order). Dependency
-- names not in the closure (e.g. boot libs) are ignored. Fails on a cycle —
-- GHC cannot build cyclic package dependencies.
topoSort :: [ResolvedDep] -> Either ZincError [ResolvedDep]
topoSort nodes = do
  (_, ordered) <- foldM (visit Set.empty) (Set.empty, []) (map rdName nodes)
  pure (map (byName Map.!) (reverse ordered))
  where
    byName = Map.fromList [(rdName n, n) | n <- nodes]
    -- Drop self-edges (a package's .cabal can name itself via sub-libraries);
    -- a node never waits on itself, so this is never a real build cycle.
    depsOf name = maybe [] (filter (\n -> n /= name && n `Map.member` byName) . rdDepends) (Map.lookup name byName)

    -- DFS post-order with a path set for cycle detection.
    visit
      :: Set String                 -- names on the current DFS path
      -> (Set String, [String])     -- (finished, reverse build order)
      -> String
      -> Either ZincError (Set String, [String])
    visit path acc@(done, _) name
      | name `Set.member` done = Right acc
      | name `Set.member` path = Left (OtherError ("dependency cycle involving '" ++ name ++ "'"))
      | otherwise = do
          (done', order') <- foldM (visit (Set.insert name path)) acc (depsOf name)
          pure (Set.insert name done', name : order')

-- | Group a resolved closure into dependency /levels/ for parallel building:
-- level 0 has the nodes with no in-closure dependencies, and each later level
-- holds nodes whose every in-closure dependency sits in an earlier level.
-- Nodes within a level are mutually independent, so they can be compiled
-- concurrently; flattening the levels yields a valid 'topoSort' order. Fails on
-- a cycle (no node ever becomes ready). Input order is preserved within levels.
topoLevels :: [ResolvedDep] -> Either ZincError [[ResolvedDep]]
topoLevels nodes = go Set.empty (map rdName nodes) []
  where
    byName = Map.fromList [(rdName n, n) | n <- nodes]
    -- Drop self-edges (a package's .cabal can name itself via sub-libraries);
    -- a node never waits on itself, so this is never a real build cycle.
    depsOf name = maybe [] (filter (\n -> n /= name && n `Map.member` byName) . rdDepends) (Map.lookup name byName)

    go _ [] acc = Right (reverse acc)
    go done remaining acc =
      let ready = [name | name <- remaining, all (`Set.member` done) (depsOf name)]
       in if null ready
            then Left (OtherError ("dependency cycle among: " ++ unwords remaining))
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
