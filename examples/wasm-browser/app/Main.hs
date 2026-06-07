{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

-- A minimal browser reactor: 'hs_start' (exported to JS) writes to the DOM via a
-- synchronous JS-FFI import. Use `unsafe` for synchronous calls; a plain
-- `foreign import javascript` is async (returns a Promise the RTS awaits).
module Main (main) where

foreign import javascript unsafe
  "document.getElementById('app').textContent = 'Rendered by Haskell -> wasm. The answer is ' + $1 + '.'"
  js_render :: Int -> IO ()

foreign export ccall hs_start :: IO ()

hs_start :: IO ()
hs_start = js_render 42

-- Unused under -no-hs-main (which zinc passes for a reactor), but GHC still
-- wants a Main module with a `main`.
main :: IO ()
main = pure ()
