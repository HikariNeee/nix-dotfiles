{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}

module River where

import Control.Monad (join,void)
import Data.Bits (shiftL)
import Data.Bool (bool)
import Data.Foldable (for_,traverse_)
import Data.List.Split
import System.Process.Typed

data View = Next | Previous
data Position = Left | Right | Up | Down
data Axis = Vertical | Horizontal
data Mode = Normal | Locked
data Modifiers = Super | Alt | Control | SA | SAC | SC | SS | None | AS | SAS

data RiverOpt
  = MkOpt String
  | MkPar String String
  | MkPos String Position
  | MkAxis String Axis
  | MkView String View
  | MkTri String String String
  | MkMode String Mode

instance Show RiverOpt where
  show (MkOpt a) = a
  show (MkPar a b) = a ++ " " ++ b
  show (MkPos a b) = a ++ " " ++ show b
  show (MkAxis a b) = a ++ " " ++ show b
  show (MkView a b) = a ++ " " ++ show b
  show (MkTri a b c) = a ++ " " ++ b ++ " " ++ c
  show (MkMode a b) = a ++ " " ++ show b

-- Nullary opts
rMap :: RiverOpt
rMap = MkOpt "map"

rToggleFloat :: RiverOpt
rToggleFloat = MkOpt "toggle-float"

rToggleFullscreen :: RiverOpt
rToggleFullscreen = MkOpt "toggle-fullscreen"

rZoom :: RiverOpt
rZoom = MkOpt "zoom"

rMapPointer :: RiverOpt
rMapPointer = MkOpt "map-pointer"

rResizeView :: RiverOpt
rResizeView = MkOpt "resize-view"

rMoveView :: RiverOpt
rMoveView = MkOpt "move-view"

rOnFocusChange :: RiverOpt
rOnFocusChange = MkOpt "on-focus-change"

rClose :: RiverOpt
rClose = MkOpt "close"

rExit :: RiverOpt
rExit = MkOpt "exit"

-- Unary opts
rSnap :: Position -> RiverOpt
rSnap = MkPos "snap" 

rSetFocusedTags :: String -> RiverOpt
rSetFocusedTags = MkPar "set-focused-tags" 

rSetViewTags :: String -> RiverOpt
rSetViewTags = MkPar "set-view-tags" 

rToggleFocusedTags :: String -> RiverOpt
rToggleFocusedTags = MkPar "toggle-focused-tags" 

rToggleViewTags :: String -> RiverOpt
rToggleViewTags = MkPar "toggle-view-tags" 

rSpawn :: String -> RiverOpt
rSpawn = MkPar "spawn" 

rSendLayoutCommand :: String -> RiverOpt
rSendLayoutCommand = MkPar "send-layout-cmd" 

rSendToOutput :: View -> RiverOpt
rSendToOutput = MkView "send-to-output" 

rFocusView :: View -> RiverOpt
rFocusView = MkView "focus-view" 

rSwapView :: View -> RiverOpt
rSwapView = MkView "swap-view" 

rFocusOutput :: View -> RiverOpt
rFocusOutput = MkView "focus-output" 

rBackgroundColour :: String -> RiverOpt
rBackgroundColour = MkPar "background-color" 

rBorderColourUnfocused :: String -> RiverOpt
rBorderColourUnfocused = MkPar "border-color-unfocused" 

rBorderColourFocused :: String -> RiverOpt
rBorderColourFocused = MkPar "border-color-focused" 

rRuleAdd :: String -> RiverOpt
rRuleAdd = MkPar "rule-add" 

rDefaultLayout :: String -> RiverOpt
rDefaultLayout = MkPar "default-layout" 

rDeclareMode :: String -> RiverOpt
rDeclareMode a = MkPar "declare-mode" a

rFocusFollowsCursor :: Mode -> RiverOpt
rFocusFollowsCursor a = MkMode "focus-follows-cursor" a

rSetCursorWarp :: String -> RiverOpt
rSetCursorWarp = MkPar "set-cursor-warp"

rSwap :: View -> RiverOpt
rSwap = MkView "swap" 

-- binary opts
rResize :: Axis -> String -> RiverOpt
rResize a = MkTri "resize" (show a)

rMove :: Position -> String -> RiverOpt
rMove a = MkTri "move" (show a) 

rSetRepeat :: String -> String -> RiverOpt
rSetRepeat = MkTri "set-repeat"

rHideCursor :: String -> String -> RiverOpt
rHideCursor = MkTri "hide-cursor"

data RiverCmdKb = RiverCmdKb
  { function :: RiverOpt
  , mode :: Mode
  , modifier :: Modifiers
  , key :: String
  , command :: RiverOpt
  }
  deriving (Show)

instance Show View where
  show Next = "next"
  show Previous = "previous"

instance Show Position where
  show River.Up = "up"
  show River.Down = "down"
  show River.Right = "right"
  show River.Left = "left"

instance Show Axis where
  show Vertical = "vertical"
  show Horizontal = "horizontal"

instance Show Modifiers where
  show Super = "Super"
  show Alt = "Alt"
  show Control = "Control"
  show SA = "Super+Alt"
  show SAC = "Super+Alt+Control"
  show SC = "Super+Control"
  show SS = "Super+Shift"
  show None = "None"
  show AS = "Alt+Shift"
  show SAS = "Super+Alt+Shift"

instance Show Mode where
  show Normal = "normal"
  show Locked = "locked"

callRiver :: [String] -> IO ()
callRiver a = void (startProcess $ proc "riverctl" a)

splitAtQuote :: String -> [String]
splitAtQuote = (split . dropBlanks . dropDelims . whenElt) (== '\'')

riverToList :: RiverCmdKb -> [String]
riverToList a =
  if (join . splitAtQuote $ show $ command a) == (show $ command a)
    then
      concatMap words [show $ function a, show $ mode a, show $ modifier a, key a, show $ command a]
    else map trimSpaces $ [show $ function a, show $ mode a, show $ modifier a, key a] ++ z
 where
  z = splitAtQuote $ show $ command a
  trimSpaces = unwords . words

riverMap :: Mode -> Modifiers -> String -> RiverOpt -> [String]
riverMap a b c d = riverToList $ RiverCmdKb{function = rMap, mode = a, modifier = b, key = c, command = d}

riverNormalMap :: Modifiers -> String -> RiverOpt -> [String]
riverNormalMap = riverMap Normal

riverMapPointer :: Mode -> Modifiers -> String -> RiverOpt -> Either String [String]
riverMapPointer a b c d = bool x y (c `elem` ["BTN_LEFT", "BTN_RIGHT", "BTN_MIDDLE"])
 where
  y = Prelude.Right $ riverToList $ RiverCmdKb{function = rMapPointer, mode = a, modifier = b, key = c, command = d}
  x = Prelude.Left "You are supposed to provide either BTN_LEFT,BTN_RIGHT or BTN_MIDDLE!"

riverNormalMapPointer :: Modifiers -> String -> RiverOpt -> Either String [String]
riverNormalMapPointer = riverMapPointer Normal

riverBackgroundColour :: String -> [String]
riverBackgroundColour a = words (show $ rBackgroundColour a)

riverBorderColourFocused :: String -> [String]
riverBorderColourFocused a = words (show $ rBorderColourFocused a)

riverBorderColourUnfocused :: String -> [String]
riverBorderColourUnfocused a = words (show $ rBorderColourUnfocused a)

riverRuleAdd :: String -> [String]
riverRuleAdd a = words (show $ rRuleAdd a)

riverFocusFollowsCursor :: [String]
riverFocusFollowsCursor = words (show $ rFocusFollowsCursor Normal)

riverSetCursorWarp :: [String]
riverSetCursorWarp = words (show $ rSetCursorWarp (show rOnFocusChange))

riverHideCursor :: String -> String -> [String]
riverHideCursor a b = words (show $ rHideCursor a b)

riverDefaultLayout :: String -> [String]
riverDefaultLayout a = words (show $ rDefaultLayout a)

riverSetRepeat :: String -> String -> [String]
riverSetRepeat a b = words (show $ rSetRepeat a b)

applyKeybinds :: [[String]] -> IO ()
applyKeybinds = traverse_ callRiver

callExternal :: String -> [String] -> IO ()
callExternal a b = void (startProcess $ proc a b)

computeTags :: Int -> Int
computeTags y = 1 `shiftL` (y - 1)

riverCreateTags :: Int -> IO ()
riverCreateTags y = for_ [1 .. y] h
 where
  h x =
    let
      tags = show $ computeTags x
     in
      applyKeybinds
        [ (riverMap Normal Super (show x) (rSetFocusedTags tags))
        , (riverMap Normal SS (show x) (rSetViewTags tags))
        , (riverMap Normal SC (show x) (rToggleFocusedTags tags))
        , (riverMap Normal SAC (show x) (rToggleViewTags tags))
        ]

riverAllTags :: String -> IO ()
riverAllTags x =
  let
    tag :: String
    tag = show $ ((1 :: Int) `shiftL` 32) - (1 :: Int)
   in
    applyKeybinds
      [ (riverMap Normal Super x (rSetFocusedTags $ tag))
      , (riverMap Normal SS x (rSetViewTags $ tag))
      ]
