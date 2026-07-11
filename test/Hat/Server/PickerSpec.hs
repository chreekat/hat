module Hat.Server.PickerSpec (spec) where

import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Test.Hspec

import Hat.Model (PickerItem (..), PickerState (..))
import Hat.Server.Keys (Key, parseKeyName)
import Hat.Server.Picker

-- Build the key a name denotes (all names here are valid).
key :: T.Text -> Key
key n = maybe (error ("bad key: " <> T.unpack n)) id (parseKeyName n)

demo :: PickerState
demo = PickerState
    { title = "windows"
    , items =
        [ PickerItem "0:editor" "select-window -t 0"
        , PickerItem "1:shell"  "select-window -t 1"
        , PickerItem "2:logs"   "select-window -t 2"
        ]
    , cursor = 0
    , query = ""
    , searching = False
    }

spec :: Spec
spec = do
    describe "editPicker (menu mode)" $ do
        it "moves the cursor down with j, clamped to the last item" $ do
            let step p = case editPicker p (key "j") of
                    PickerStay p' -> p'
                    _ -> p
                p3 = iterate step demo !! 5
            p3.cursor `shouldBe` 2
        it "runs the selected item's command on Enter" $
            editPicker demo { cursor = 1 } (key "Enter")
                `shouldBe` PickerRun "select-window -t 1"
        it "cancels on q and Escape" $ do
            editPicker demo (key "q") `shouldBe` PickerCancel
            editPicker demo (key "Escape") `shouldBe` PickerCancel
        it "enters search mode on /" $
            case editPicker demo (key "/") of
                PickerStay p -> p.searching `shouldBe` True
                _ -> expectationFailure "expected to stay open in search mode"

    describe "search mode" $ do
        let searching = demo { searching = True }
        it "types into the query and filters items" $ do
            let afterL = case editPicker searching (key "l") of
                    PickerStay p -> p
                    _ -> error "expected stay"
            afterL.query `shouldBe` "l"
            map (.label) (visibleItems afterL) `shouldBe` ["1:shell", "2:logs"]
        it "runs the sole match on Enter" $ do
            let p = searching { query = "logs" }
            editPicker p (key "Enter") `shouldBe` PickerRun "select-window -t 2"

    describe "pickerLines" $
        it "marks the cursor row and shows the title" $ do
            let ls = pickerLines 10 demo { cursor = 1 }
            listToMaybe ls `shouldBe` Just "windows"
            filter (T.isPrefixOf "\x25b8") ls `shouldBe` ["\x25b8 1:shell"]
