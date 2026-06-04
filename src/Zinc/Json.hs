-- | A tiny JSON encoder. zinc avoids heavy dependencies (no aeson); the
-- agent-facing @--json@ surface only needs to /emit/ JSON, so a small,
-- dependency-free 'Json' value + renderer is enough.
module Zinc.Json
  ( Json (..)
  , renderJson
  , object
  ) where

import Data.Char (ord)
import Data.List (intercalate)
import Numeric (showHex)

-- | A JSON value.
data Json
  = JString String
  | JBool Bool
  | JNull
  | JInt Int
  | JArray [Json]
  | JObject [(String, Json)]
  deriving (Eq, Show)

-- | Build an object from optional fields, dropping the @Nothing@s — so optional
-- diagnostic fields (@detail@, @nextAction@, …) are simply absent when unset.
object :: [(String, Maybe Json)] -> Json
object kvs = JObject [(k, v) | (k, Just v) <- kvs]

-- | Render a 'Json' value to a compact (no-whitespace) string.
renderJson :: Json -> String
renderJson j = case j of
  JNull -> "null"
  JBool True -> "true"
  JBool False -> "false"
  JInt n -> show n
  JString s -> jstr s
  JArray xs -> "[" ++ intercalate "," (map renderJson xs) ++ "]"
  JObject kvs -> "{" ++ intercalate "," [jstr k ++ ":" ++ renderJson v | (k, v) <- kvs] ++ "}"

-- | A JSON string literal with the required escaping.
jstr :: String -> String
jstr s = "\"" ++ concatMap esc s ++ "\""
  where
    esc c = case c of
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      _ | ord c < 0x20 -> let h = showHex (ord c) "" in "\\u" ++ replicate (4 - length h) '0' ++ h
        | otherwise -> [c]
