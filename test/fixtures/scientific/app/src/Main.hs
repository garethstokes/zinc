module Main (main) where

import Data.Scientific (Scientific, scientific)

main :: IO ()
main = print (scientific 314 (-2) :: Scientific)
