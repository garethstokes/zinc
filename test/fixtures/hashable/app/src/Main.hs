module Main (main) where

import Data.Hashable (hash)

main :: IO ()
main = print (hash (42 :: Int))
