-- | Human-facing rendering for diagnostics & error UX (epic zinc-unv).
-- Currently: the resolution table shown on @resolve@ / @build@ / @add@ so the
-- user can see exactly which code (repo + ref) each dependency resolves to.
module Zinc.Report
  ( renderResolution
  ) where

import Zinc.Manifest (Ref (..))
import Zinc.Resolve (ResolvedDep (..))

-- | Render the resolved closure as an aligned @package / ref / repo@ table.
renderResolution :: [ResolvedDep] -> String
renderResolution [] = "(no dependencies)\n"
renderResolution rds = unlines (header : map row rds)
  where
    nameW = maximum (length "package" : map (length . rdName) rds)
    refW = maximum (length "ref" : map (length . renderRef . rdRef) rds)
    pad w s = s ++ replicate (w - length s) ' '
    header = pad nameW "package" ++ "  " ++ pad refW "ref" ++ "  repo"
    row d = pad nameW (rdName d) ++ "  " ++ pad refW (renderRef (rdRef d)) ++ "  " ++ rdRepo d

-- | Compact display form of a ref.
renderRef :: Ref -> String
renderRef (Tag t)    = t
renderRef (Branch b) = b
renderRef (Rev r)    = take 8 r
renderRef Latest     = "*"
