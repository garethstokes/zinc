{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, decimal)

main :: IO ()
main = print (parseOnly (decimal :: Parser Int) "42")
