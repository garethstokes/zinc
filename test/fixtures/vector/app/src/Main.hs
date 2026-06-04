module Main (main) where

import qualified Data.Vector as V

main :: IO ()
main = print (V.sum (V.fromList [1 .. 10 :: Int]))
