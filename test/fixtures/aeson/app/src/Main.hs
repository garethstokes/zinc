{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson (encode, object, (.=))

main :: IO ()
main = print (encode (object ["zinc" .= (1 :: Int)]))
