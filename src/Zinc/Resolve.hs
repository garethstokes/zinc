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
  ) where

import qualified Data.Map as Map
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
