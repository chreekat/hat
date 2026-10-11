module Hat.Server.PopupSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Hspec

import Hat.Server.Box (BoxLines (..), Grid)
import Hat.Server.Popup
import Hat.Server.Theme (terminalTheme)
import Hat.Term.Cell qualified as Cell

-- display-popup -w20 -h6 -x6 -y3 on a 40x14 client: the box at (6, 0).
popup :: PopupState
popup = newPopupState 6 0 20 6 BoxSingle ""

rowText :: Grid -> Int -> Text
rowText g y = T.pack [ Cell.baseChar c | c <- maybe [] V.toList (g V.!? y) ]

mouse :: Int -> (Int, Int) -> Int -> (Int, Int) -> PopupInput
mouse = PopupMouse

key :: Text -> PopupInput
key k = PopupKey k False

spec :: Spec
spec = do
    describe "geometry" $ do
        it "holds the command's screen inside its border, or fills the box without one" $ do
            innerSize popup `shouldBe` Just (18, 4)
            innerSize popup { lines = BoxNone } `shouldBe` Just (20, 6)
            innerSize popup { sx = 2 } `shouldBe` Nothing
        it "shrinks with a small client and grows back to what it asked for" $ do
            let small = refitPopup 15 4 popup
            (small.px, small.py, small.sx, small.sy) `shouldBe` (0, 0, 15, 4)
            let back = refitPopup 40 14 small
            (back.px, back.py, back.sx, back.sy) `shouldBe` (6, 0, 20, 6)

    describe "keys" $ do
        it "closes on Escape or C-c unless its command's exit will" $ do
            snd (popupInput 40 14 JobRunning (key "Escape") popup) `shouldBe` PopupCloses
            snd (popupInput 40 14 JobRunning (key "C-c") popup { onExit = CloseOnExit })
                `shouldBe` PopupKeyToJob
            snd (popupInput 40 14 JobDone (key "C-c") popup { onExit = CloseOnExit })
                `shouldBe` PopupCloses
        it "sends other keys to the command while it runs" $
            snd (popupInput 40 14 JobRunning (key "x") popup) `shouldBe` PopupKeyToJob
        it "closes on any key but a paste after the command exits, with -k" $ do
            snd (popupInput 40 14 JobDone (key "x") popup { keys = AnyKeyCloses })
                `shouldBe` PopupCloses
            snd (popupInput 40 14 JobDone (PopupKey "x" True) popup { keys = AnyKeyCloses })
                `shouldBe` PopupTook
            snd (popupInput 40 14 JobDone (key "x") popup) `shouldBe` PopupTook

    describe "mouse" $ do
        it "sends a click inside the border to the command, at its screen's cell" $
            snd (popupInput 40 14 JobRunning (mouse 0 (8, 2) 3 (8, 2)) popup)
                `shouldBe` PopupMouseToJob (1, 1)
        it "takes a click outside the box or on its border" $ do
            snd (popupInput 40 14 JobRunning (mouse 0 (2, 2) 3 (2, 2)) popup) `shouldBe` PopupTook
            snd (popupInput 40 14 JobRunning (mouse 0 (6, 2) 3 (6, 2)) popup) `shouldBe` PopupTook
        it "moves by a drag of its edge with button 1, as far as the client allows" $ do
            let (grabbed, _) = popupInput 40 14 JobRunning (mouse 32 (7, 0) 0 (6, 0)) popup
                (moved, _) = popupInput 40 14 JobRunning (mouse 32 (30, 5) 32 (7, 0)) grabbed
            grabbed.dragging `shouldBe` DragMove
            (moved.px, moved.py, moved.ppx) `shouldBe` (20, 5, 20)
            (fst (popupInput 40 14 JobRunning (mouse 0 (30, 5) 32 (30, 5)) moved)).dragging
                `shouldBe` DragOff
        it "resizes by a drag with button 3, never below a cell inside" $ do
            let (grabbed, _) = popupInput 40 14 JobRunning (mouse 34 (25, 5) 2 (25, 5)) popup
                (sized, _) = popupInput 40 14 JobRunning (mouse 34 (30, 9) 34 (25, 5)) grabbed
                (tooSmall, _) = popupInput 40 14 JobRunning (mouse 34 (7, 1) 34 (30, 9)) sized
            grabbed.dragging `shouldBe` DragSize
            (sized.sx, sized.sy, sized.psx) `shouldBe` (24, 9, 24)
            (tooSmall.sx, tooSmall.sy) `shouldBe` (24, 9)
        it "moves from anywhere inside with Meta held" $
            (fst (popupInput 40 14 JobRunning (mouse 40 (10, 2) 8 (10, 2)) popup)).dragging
                `shouldBe` DragMove

    describe "popupGrid" $ do
        let content = V.fromList [ V.fromList [ Cell.glyphCell c Cell.defaultStyle | c <- "POPUP" ] ]
        it "draws the box with the command's screen inside it" $ do
            let g = popupGrid terminalTheme Cell.defaultStyle popup { title = "T" } content
            map (rowText g) [0, 1, 5] `shouldBe`
                [ "\x250c\x2500T" <> T.replicate 16 "\x2500" <> "\x2510"
                , "\x2502POPUP" <> T.replicate 13 " " <> "\x2502"
                , "\x2514" <> T.replicate 18 "\x2500" <> "\x2518" ]
        it "fills the whole box with the screen when it has no border" $
            rowText (popupGrid terminalTheme Cell.defaultStyle popup { lines = BoxNone } content) 0
                `shouldBe` "POPUP" <> T.replicate 15 " "
        it "puts the command's cursor inside the border" $
            popupCursor popup (5, 0) `shouldBe` (12, 1)
