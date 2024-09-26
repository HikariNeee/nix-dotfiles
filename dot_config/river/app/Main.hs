module Main where

import Data.Foldable (for_)
import qualified River as R
import Data.Either (rights)
import System.Directory

initialKeymap :: [[String]]
initialKeymap =
  [ (R.riverNormalMap R.Super "Return" $ R.rSpawn "foot")
  , (R.riverNormalMap R.Super "W" $ R.rSpawn "firefox")
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
  , (R.riverNormalMap R.Super "Print" $ R.rSpawn "'grim -g \"$(slurp)\" -| wl-copy && notify-send \"Screenshot Clipped\"'")
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
       , R.riverBackgroundColour "0x002b36"
       , R.riverBorderColourFocused "0x7287fd"
       , R.riverBorderColourUnfocused "0xeff1f5"
       , R.riverSetRepeat "50" "250"
       , R.riverDefaultLayout "bsp-layout"
       , R.riverRuleAdd "-app-id 'bar' csd"] R.callRiver

  wallpaperscript <- getXdgDirectory XdgConfig "river/wallpaper.fish"
  R.callExternal wallpaperscript []
  R.callExternal "river-bsp-layout" ["--inner-gap","3", "--outer-gap", "7", "--split-perc", "0.5"] 
  R.callExternal "i3bar-river" []

