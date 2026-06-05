-- | A tiny raw-ANSI styling layer (hw6.2): SGR escape sequences with NO
-- external dependency (no ansi-terminal / colour). Every helper takes the
-- color-enabled flag; when it is 'False' the text is returned unchanged, so
-- NO_COLOR / non-TTY / piped output is plain by construction — the call sites
-- never branch on color themselves.
module Zinc.Ansi
  ( style
  , bold
  , dim
  , red
  , redBold
  , green
  , greenBold
  , yellow
  , cyan
  , clearLine
  ) where

import Data.List (intercalate)

-- | Wrap text in an SGR sequence built from the given codes, then reset — but
-- only when color is enabled. Combined codes (e.g. @[1,32]@ for bold green) go
-- in one sequence so a single reset restores cleanly (nesting separate spans
-- would let an inner reset clear an outer color).
style :: Bool -> [Int] -> String -> String
style False _ s = s
style True codes s = "\ESC[" ++ intercalate ";" (map show codes) ++ "m" ++ s ++ "\ESC[0m"

bold, dim, red, redBold, green, greenBold, yellow, cyan :: Bool -> String -> String
bold on = style on [1]
dim on = style on [2]
red on = style on [31]
redBold on = style on [1, 31]
green on = style on [32]
greenBold on = style on [1, 32]
yellow on = style on [33]
cyan on = style on [36]

-- | Erase the current terminal line and return the cursor to column 0, so a
-- live progress line can be overwritten in place (or cleared before committing
-- scrollback). Independent of color: it is cursor control, used only on a TTY.
clearLine :: String
clearLine = "\ESC[2K\r"
