import Test.Tasty
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.HUnit
import qualified River as R

main :: IO ()
main = defaultMainWithRerun tests

tests :: TestTree
tests = testCaseSteps "example" $ \step -> do
        step "Testing callRiver..."
        R.callRiver ["-version"]

        step "Testing riverToList..."
        (R.riverToList $ 
          R.RiverCmdKb {R.function = R.rMap, R.mode = R.Normal, R.modifier = R.Super, R.key = "Comma", R.command = R.rSpawn "beep"}) @?= 
           ["map","normal","Super","Comma","spawn","beep"]
        
        step "Testing riverMap..."
        R.riverMap R.Normal R.Super "Comma" (R.rSpawn "beep") @?= ["map","normal","Super","Comma","spawn","beep"]


        step "Testing riverMap with multi-parameter calls..."
        R.riverMap R.Normal R.Super "Comma" (R.rSpawn "'what is hell supposed to be like? my code will tell you'") @?= ["map","normal","Super","Comma","spawn","what is hell supposed to be like? my code will tell you"]

        step "Testing riverMapPointer..."
        R.riverMapPointer R.Normal R.Super "BTN_LEFT" (R.rSpawn "beep") @?= Right ["map-pointer","normal","Super","BTN_LEFT","spawn","beep"]
   
        step "Testing riverBackgroundColour..."
        R.riverBackgroundColour "0xffffffff" @?= ["background-color","0xffffffff"]
        
        step "Testing riverBorderColourFocused..."
        R.riverBorderColourFocused "0xffffffff" @?= ["border-color-focused","0xffffffff"]
        
        step "Testing riverBorderColourUnfocused..."
        R.riverBorderColourUnfocused "0xffffffff" @?= ["border-color-unfocused","0xffffffff"]   

        step "Testing riverRuleAdd..."
        R.riverRuleAdd "-app-id firefox" @?= ["rule-add","-app-id","firefox"]

        step "Testing riverDefaultLayout..."
        R.riverDefaultLayout "river-bsp-layout" @?= ["default-layout","river-bsp-layout"]

        step "Testing applyKeybinds..."
        R.applyKeybinds [["map","normal","Super","Comma","spawn","beep"],["map","normal","Super","Comma","spawn","helo"]]

        step "Testing riverCreateTags..."
        R.riverCreateTags 8

        step "Testing riverAllTags..."
        R.riverAllTags "0"
      
