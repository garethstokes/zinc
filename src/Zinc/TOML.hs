-- | Small shared helpers for pulling typed fields out of a parsed TOML
-- table (@toml-parser@'s @Map String Value@). Used by "Zinc.Manifest" and
-- "Zinc.Lock".
module Zinc.TOML
  ( tableField
  , stringField
  , stringArrayField
  , optStringArray
  , subTable
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Toml.Value (Value (..))

-- | Require a nested table at @key@.
tableField :: String -> Map String Value -> Either String (Map String Value)
tableField k t = case Map.lookup k t of
  Just (Table v) -> Right v
  Just _         -> Left ("expected a table for [" ++ k ++ "]")
  Nothing        -> Left ("missing required table [" ++ k ++ "]")

-- | A nested table by key, or empty if absent / not a table.
subTable :: String -> Map String Value -> Map String Value
subTable k t = case Map.lookup k t of
  Just (Table v) -> v
  _              -> Map.empty

-- | Require a string field at @key@.
stringField :: String -> Map String Value -> Either String String
stringField k t = case Map.lookup k t of
  Just (String s) -> Right s
  _               -> Left ("missing required string field: " ++ k)

-- | Require an array-of-strings field at @key@.
stringArrayField :: String -> Map String Value -> Either String [String]
stringArrayField k t = case Map.lookup k t of
  Just (Array xs) -> mapM (asString k) xs
  _               -> Left ("missing required array field: " ++ k)

-- | An array-of-strings field at @key@, defaulting to @[]@ when absent.
optStringArray :: String -> Map String Value -> Either String [String]
optStringArray k t = case Map.lookup k t of
  Nothing         -> Right []
  Just (Array xs) -> mapM (asString k) xs
  Just _          -> Left ("expected an array for field: " ++ k)

asString :: String -> Value -> Either String String
asString _ (String s) = Right s
asString k _          = Left ("non-string element in array: " ++ k)
