module Main where

import Data.Foldable (for_)
import qualified River as R
import Data.Either (rights)
import System.Directory

initialKeymap :: [[String]]
initialKeymap =
  [ (R.riverNormalMap R.Super "Return" $ R.rSpawn "foot")
  , (R.riverNormalMap R.Super "W" $ R.rSpawn "brave")
  , (R.riverNormalMap R.Super "Q" $ R.rClose)
  , (R.riverNormalMap R.SS "E" $ R.rExit)
  , (R.riverNormalMap R.Super "J" $ R.rFocusView R.Next)
  , (R.riverNormalMap R.Super "K" $ R.rFocusView R.Previous)
  , (R.riverNormalMap R.SS "J" $ R.rSwap R.Next)
  , (R.riverNormalMap R.SS "K" $ R.rSwap R.Previous)
  , (R.riverNormalMap R.SA "H" $ R.rMove R.Left "100")
  , (R.riverNormalMap R.SA "L" $ R.rMove R.Right "100")
  , (R.riverNormalMap R.SA "J" $ R.rMove R.Down "100")
  , (R.riverNormalMap R.SA "K" $ R.rMove R.Up "100")
  , (R.riverNormalMap R.Super "Print" $ R.rSpawn "'grim -g \"$(slurp)\" -| wl-copy -t image/png && notify-send \"Screenshot Clipped\"'")
  , (R.riverNormalMap R.Super "X" $ R.rSpawn "fuzzel")
  ]

initialPointerKeymap :: [[String]]
initialPointerKeymap = rights
  [ (R.riverNormalMapPointer R.Super "BTN_LEFT" $ R.rMoveView)
  , (R.riverNormalMapPointer R.Super "BTN_RIGHT" $ R.rResizeView)
  , (R.riverNormalMapPointer R.Super "BTN_MIDDLE" $ R.rToggleFloat)
  ]

main :: IO ()
main = do
  R.applyKeybinds initialKeymap
  R.applyKeybinds initialPointerKeymap
  R.riverCreateTags 9
  R.riverAllTags "0"
  for_ [R.riverHideCursor "timeout" "5000",R.riverHideCursor "when-typing" "enabled"] R.callRiver

  for_ [ R.riverSetCursorWarp
       , R.riverFocusFollowsCursor
       , R.riverBackgroundColour "0xffffff"
       , R.riverBorderColourFocused "0xc4c4c4"
       , R.riverBorderColourUnfocused "0xe0e0e0"
       , R.riverSetRepeat "50" "250"
       , R.riverDefaultLayout "wideriver"
       , R.riverRuleAdd "-app-id 'bar' csd"] R.callRiver

  R.callExternal "wideriver" ["--layout", "left", "--stack", "dwindle","--count", "1", "--ratio", "0.5", "--outer-gaps", "4"] 
  R.callExternal "i3bar-river" []

