{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

-- A real miso application compiled to a browser WebAssembly reactor by zinc
-- (the headline proof-point for zinc-90t). The browser calls the exported
-- `hs_start`, which mounts a miso component (a counter) into the DOM.
module Main (main) where

import           Miso
import qualified Miso.Html as H
import           Miso.Lens

-- | Component model: a single counter.
newtype Model = Model { _counter :: Int } deriving (Show, Eq)

counter :: Lens Model Int
counter = lens _counter (\m v -> m { _counter = v })

data Action = AddOne | SubtractOne | SayHello deriving (Show, Eq)

-- | Entry point for the miso application.
main :: IO ()
main = startApp defaultEvents app

-- | The wasm reactor entry point exported to JS (zinc wasm-exports = ["hs_start"]).
foreign export javascript "hs_start" main :: IO ()

app :: App Model Action
app = component (Model 0) updateModel viewModel

updateModel :: Action -> Effect parent props Model Action
updateModel = \case
  AddOne      -> counter += 1
  SubtractOne -> counter -= 1
  SayHello    -> io_ (consoleLog "Hello from miso -> wasm, built by zinc!")

viewModel :: props -> Model -> View Model Action
viewModel _ m =
  vfrag
    [ H.h1_ [] [ text "miso \8594 WebAssembly, built by zinc" ]
    , H.button_ [ H.onClick SubtractOne ] [ text "-" ]
    , H.span_ [] [ text (" " <> ms (m ^. counter) <> " ") ]
    , H.button_ [ H.onClick AddOne ] [ text "+" ]
    , H.p_ [] [ H.button_ [ H.onClick SayHello ] [ text "console.log hello" ] ]
    ]
