-- | A tiny JSON encoder. zinc avoids heavy dependencies (no aeson); the
-- agent-facing @--json@ surface only needs to /emit/ JSON, so a small,
-- dependency-free 'Json' value + renderer is enough.
module Zinc.Json
  ( Json (..)
  , renderJson
  , object
  , parseJson
  ) where

import Data.Char (chr, isDigit, isSpace, ord)
import Data.List (intercalate)
import Numeric (readHex, showHex)

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

-- | Parse a JSON document (the inverse of 'renderJson', for reading back
-- zinc's own machine output — e.g. @.zinc/metrics.jsonl@ records). Numbers are
-- parsed as integers ('JInt'), since zinc only ever emits integers. Returns the
-- parse error on malformed input.
parseJson :: String -> Either String Json
parseJson s = case pValue (skip s) of
  Right (j, rest)
    | all isSpace rest -> Right j
    | otherwise        -> Left ("trailing input: " ++ take 24 rest)
  Left e -> Left e

-- | Skip leading JSON whitespace.
skip :: String -> String
skip = dropWhile isSpace

pValue :: String -> Either String (Json, String)
pValue s = case s of
  'n' : 'u' : 'l' : 'l' : r           -> Right (JNull, r)
  't' : 'r' : 'u' : 'e' : r           -> Right (JBool True, r)
  'f' : 'a' : 'l' : 's' : 'e' : r     -> Right (JBool False, r)
  '"' : r                             -> do (str, r') <- pStr r; Right (JString str, r')
  '[' : r                             -> pArray (skip r)
  '{' : r                             -> pObject (skip r)
  c : _ | c == '-' || isDigit c       -> pNumber s
  _                                   -> Left ("unexpected token: " ++ take 24 s)

-- | Parse the body of a string (after the opening quote), up to the closing
-- quote, reversing 'jstr' escaping.
pStr :: String -> Either String (String, String)
pStr = go id
  where
    go acc s = case s of
      '"' : r        -> Right (acc "", r)
      '\\' : e : r   -> case e of
        '"'  -> go (acc . ('"' :)) r
        '\\' -> go (acc . ('\\' :)) r
        '/'  -> go (acc . ('/' :)) r
        'n'  -> go (acc . ('\n' :)) r
        'r'  -> go (acc . ('\r' :)) r
        't'  -> go (acc . ('\t' :)) r
        'b'  -> go (acc . ('\b' :)) r
        'f'  -> go (acc . ('\f' :)) r
        'u'  -> case splitAt 4 r of
          (hex, r') | length hex == 4, [(n, "")] <- readHex hex -> go (acc . (chr n :)) r'
          _ -> Left "bad \\u escape"
        _    -> Left ("bad escape: \\" ++ [e])
      c : r          -> go (acc . (c :)) r
      []             -> Left "unterminated string"

pNumber :: String -> Either String (Json, String)
pNumber s =
  let (digits, rest) = span (\c -> isDigit c || c == '-') s
   in case reads digits :: [(Int, String)] of
        [(n, "")] -> Right (JInt n, rest)
        _         -> Left ("bad number: " ++ take 24 s)

pArray :: String -> Either String (Json, String)
pArray (']' : r) = Right (JArray [], r)
pArray s = go id s
  where
    go acc t = do
      (v, t1) <- pValue (skip t)
      case skip t1 of
        ',' : t2 -> go (acc . (v :)) (skip t2)
        ']' : t2 -> Right (JArray (acc [v]), t2)
        _        -> Left ("expected , or ] in array near: " ++ take 24 t1)

pObject :: String -> Either String (Json, String)
pObject ('}' : r) = Right (JObject [], r)
pObject s = go id s
  where
    go acc t = case skip t of
      '"' : t1 -> do
        (k, t2) <- pStr t1
        case skip t2 of
          ':' : t3 -> do
            (v, t4) <- pValue (skip t3)
            case skip t4 of
              ',' : t5 -> go (acc . ((k, v) :)) (skip t5)
              '}' : t5 -> Right (JObject (acc [(k, v)]), t5)
              _        -> Left ("expected , or } in object near: " ++ take 24 t4)
          _ -> Left ("expected : in object near: " ++ take 24 t2)
      _ -> Left ("expected string key in object near: " ++ take 24 t)

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
