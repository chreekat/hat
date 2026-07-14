module Hat.Server.PickerSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Hat.Geometry (Rect (..), Size (..))
import Hat.Model
    ( PaneId (..), WindowId (..), PickerNode (..), PickerState (..)
    , PreviewTarget (..) )
import Hat.Server.Keys (Key, parseKeyName)
import Hat.Server.Picker

-- Build the key a name denotes (all names here are valid).
key :: T.Text -> Key
key n = maybe (error ("bad key: " <> T.unpack n)) id (parseKeyName n)

-- A collapsible parent node, initially expanded.
node :: T.Text -> T.Text -> [PickerNode] -> PickerNode
node lbl cmd kids = PickerNode lbl cmd Nothing kids True

demo :: PickerState
demo = PickerState
    { title = "tree"
    , roots =
        [ node "work" "switch-client -t work"
            [ leaf "0:editor" "select-window -t work:0"
            , leaf "1:shell"  "select-window -t work:1" ]
        , node "play" "switch-client -t play"
            [ leaf "0:game" "select-window -t play:0" ]
        ]
    , cursor = 0
    , query = ""
    , searching = False
    , zoomed = False
    }

stay :: PickerState -> Key -> PickerState
stay p k = case editPicker p k of
    PickerStay p' -> p'
    _             -> error "expected the picker to stay open"

labels :: PickerState -> [T.Text]
labels = map ((.label) . (.node)) . visibleRows

spec :: Spec
spec = do
    describe "visibleRows" $ do
        it "flattens the fully expanded tree depth-first" $
            labels demo `shouldBe` ["work", "0:editor", "1:shell", "play", "0:game"]

        it "records the depth of each row" $
            map (.depth) (visibleRows demo) `shouldBe` [0, 1, 1, 0, 1]

    describe "editPicker (menu mode)" $ do
        it "moves the cursor down with j, clamped to the last row" $ do
            let p = iterate (`stay` key "j") demo !! 9
            p.cursor `shouldBe` 4

        it "collapses the node under the cursor with h, hiding its children" $
            labels (stay demo (key "h")) `shouldBe` ["work", "play", "0:game"]

        it "re-expands a collapsed node with l" $ do
            let collapsed = stay demo (key "h")
            labels (stay collapsed (key "l")) `shouldBe` labels demo

        it "toggles the node under the cursor with O" $
            labels (stay demo (key "O")) `shouldBe` ["work", "play", "0:game"]

        it "runs a leaf's command on Enter" $
            editPicker demo { cursor = 2 } (key "Enter")
                `shouldBe` PickerRun "select-window -t work:1"

        it "runs a parent's own command on Enter" $
            editPicker demo (key "Enter")
                `shouldBe` PickerRun "switch-client -t work"

        it "cancels on q and Escape" $ do
            editPicker demo (key "q") `shouldBe` PickerCancel
            editPicker demo (key "Escape") `shouldBe` PickerCancel

        it "enters search mode on /" $
            (stay demo (key "/")).searching `shouldBe` True

    describe "search mode" $ do
        let searching = demo { searching = True }
        it "filters to matches and keeps their ancestors" $ do
            let p = foldl stay searching (map key ["s", "h", "e", "l", "l"])
            p.query `shouldBe` "shell"
            labels p `shouldBe` ["work", "1:shell"]

        -- Enter while typing a search commits the filter (leaves search mode
        -- with the cursor on the first match) rather than immediately
        -- activating it; activation takes a deliberate second Enter.
        it "commits the search on Enter: leaves search mode, keeps the cursor" $ do
            let p = foldl stay searching (map key ["s", "h", "e", "l", "l"])
                committed = stay p (key "Enter")
            committed.searching `shouldBe` False
            committed.cursor `shouldBe` p.cursor

        it "runs the match only on a second Enter, now in menu mode" $ do
            let p = foldl stay searching (map key ["s", "h", "e", "l", "l"])
                committed = stay p (key "Enter")
            editPicker committed (key "Enter")
                `shouldBe` PickerRun "select-window -t work:1"

    describe "selectedPreview" $ do
        let previewTree = demo
                { roots =
                    [ (node "work" "switch-client -t work"
                        [ (leaf "0:editor" "c")
                            { preview = Just (PreviewPane (PaneId 7)) } ])
                        { preview = Just (PreviewWindow (WindowId 3)) } ] }
        it "previews the target of the row under the cursor" $ do
            -- the parent row previews its whole window (splits and all);
            -- the child row previews just its pane.
            selectedPreview previewTree { cursor = 0 }
                `shouldBe` Just (PreviewWindow (WindowId 3))
            selectedPreview previewTree { cursor = 1 }
                `shouldBe` Just (PreviewPane (PaneId 7))

    describe "pickerSplit" $ do
        it "gives the list a third of a wide overlay" $
            pickerSplit 90 `shouldBe` Just 30
        it "declines to split a narrow overlay" $
            pickerSplit 30 `shouldBe` Nothing

    describe "pickerRegion" $ do
        let csize = Size { rows = 24, cols = 80 }
            paneRect = Rect { startRow = 0, endRow = 23, startCol = 41, endCol = 80 }
        it "uses the active pane's rect when not zoomed" $
            pickerRegion False csize 0 (Just paneRect) `shouldBe` paneRect
        it "fills the content area (minus the status row) when zoomed" $
            pickerRegion True csize 0 (Just paneRect)
                `shouldBe` Rect { startRow = 0, endRow = 23, startCol = 0, endCol = 80 }
        it "falls back to the content area with no active pane" $
            pickerRegion False csize 1 Nothing
                `shouldBe` Rect { startRow = 1, endRow = 24, startCol = 0, endCol = 80 }

    describe "pickerLines" $
        it "shows the title, marks the cursor row, and draws arrows" $ do
            let ls = pickerLines 10 demo { cursor = 1 }
            map snd (take 1 ls) `shouldBe` ["tree"]
            [ t | (selected, t) <- ls, selected ] `shouldBe` ["    0:editor"]
            [ t | (_, t) <- ls, "\x25be" `T.isInfixOf` t ] `shouldBe` ["\x25be work", "\x25be play"]
