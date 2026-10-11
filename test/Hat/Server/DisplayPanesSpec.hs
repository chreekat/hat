module Hat.Server.DisplayPanesSpec (spec) where

import Data.List (sort)
import Test.Hspec

import Hat.Geometry (Pos (..))
import Hat.Model (PanesKeyHold (..))
import Hat.Server.DisplayPanes
import Hat.Term.Cell qualified as Cell

blue :: Cell.Color
blue = Cell.Indexed 4

-- The block cells of a stamp (big digits paint background blocks).
blocksOf :: [(Pos, Cell.Cell)] -> [Pos]
blocksOf stamps = sort [ p | (p, c) <- stamps, c.style.bg == blue ]

-- The text cells of a stamp (number/size drawn as foreground glyphs).
textOf :: [(Pos, Cell.Cell)] -> [(Pos, Char)]
textOf stamps = sort [ (p, Cell.baseChar c) | (p, c) <- stamps, c.style.fg == blue ]

spec :: Spec
spec = do
    describe "panesStamp" $ do
        let label = Just [ (i, Cell.glyphCell ch Cell.defaultStyle { Cell.fg = blue })
                         | (i, ch) <- zip [7 ..] "11x7" ]
        it "draws a big 5x5 digit centered in a roomy pane" $ do
            -- "1" is the rightmost column of its glyph; in an 11x7 pane
            -- the block starts at col (11-5)/2 = 3, row (7-5)/2 = 1.
            let stamps = panesStamp blue 1 Nothing 11 7
            blocksOf stamps `shouldBe` [ Pos (1 + j) 7 | j <- [0 .. 4] ]
        it "draws display-panes-format on the pane's top row" $
            textOf (panesStamp blue 1 label 11 7) `shouldBe`
                [ (Pos 0 (7 + i), ch) | (i, ch) <- zip [0 ..] "11x7" ]
        it "lays two digits side by side with a gap column" $ do
            let stamps = panesStamp blue 10 Nothing 20 5
                one  = [ Pos j 8 | j <- [0 .. 4] ]
                zero = [ Pos 0 (10 + i) | i <- [0 .. 4] ]
                    <> [ Pos j (10 + i) | j <- [1 .. 3], i <- [0, 4] ]
                    <> [ Pos 4 (10 + i) | i <- [0 .. 4] ]
            blocksOf stamps `shouldBe` sort (one <> zero)
        it "falls back to the number as text when the pane is too short" $
            -- 12x3: no room for 5 block rows; "3" centered on row 1.
            textOf (panesStamp blue 3 Nothing 12 3) `shouldBe` [(Pos 1 5, '3')]
        it "needs two more rows for the blocks when there is a format" $
            blocksOf (panesStamp blue 1 label 11 6) `shouldBe` []
        it "never paints outside the pane box" $ do
            let inside (Pos r c, _) = r >= 0 && r < 2 && c >= 0 && c < 2
            panesStamp blue 123 label 2 2 `shouldSatisfy` all inside

    describe "panesKeyAction" $ do
        it "a digit selects that pane number" $
            panesKeyAction PanesClose "3" `shouldBe` PanesSelect 3
        it "q and Escape dismiss, swallowing the key" $ do
            panesKeyAction PanesClose "q" `shouldBe` PanesDismiss
            panesKeyAction PanesClose "Escape" `shouldBe` PanesDismiss
        it "any other key dismisses and is processed normally" $
            panesKeyAction PanesClose "x" `shouldBe` PanesDismissForward
        it "-N keeps the overlay up on other keys; digits still select" $ do
            panesKeyAction PanesHold "x" `shouldBe` PanesForward
            panesKeyAction PanesHold "3" `shouldBe` PanesSelect 3

    describe "panesDeadline" $ do
        it "adds the duration in ms to the shown instant" $
            panesDeadline 1000 5 `shouldBe` Just 1000000005
        it "0 means until a key, no deadline" $
            panesDeadline 0 5 `shouldBe` Nothing
