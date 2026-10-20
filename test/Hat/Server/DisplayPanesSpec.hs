module Hat.Server.DisplayPanesSpec (spec) where

import Data.List (sort)
import Data.Vector qualified as V
import Test.Hspec

import Hat.Geometry (Pos (..), Rect (..), Size (..))
import Hat.Model.Ids (PaneId (..))
import Hat.Model.Options (Options (..), defaultOptions)
import Hat.Server.DisplayPanes
import Hat.Server.FormatDraw (colourValue)
import Hat.Server.Layout (Cell (..), Layout (..), Orientation (..))
import Hat.Server.Scene (PaneStatus (..))
import Hat.Term.Cell qualified as Cell
import Hat.TestSupport (clientTheme)

blue :: Cell.Color
blue = Cell.Indexed 4

fgc :: Cell.Style
fgc = Cell.defaultStyle { Cell.fg = blue }

-- The block cells of a stamp (big digits paint background blocks).
blocksOf :: [(Pos, Cell.Cell)] -> [Pos]
blocksOf stamps = sort [ p | (p, c) <- stamps, c.style.bg == blue ]

-- The text cells of a stamp (number/size drawn as foreground glyphs).
textOf :: [(Pos, Cell.Cell)] -> [(Pos, Char)]
textOf stamps = sort [ (p, Cell.baseChar c) | (p, c) <- stamps, c.style.fg == blue ]

-- | Two panes side by side in an 80x24 window, split at column 40.
sideBySide :: Layout
sideBySide = Layout 80 24 0 0 $ NodeCell LeftRight
    [ Layout 40 24 0 0 (LeafCell (PaneId 0))
    , Layout 39 24 41 0 (LeafCell (PaneId 1)) ]

pane :: Int -> PanesPane
pane n = PanesPane
    { pane = PaneId n, number = n, cells = V.empty, cursor = Nothing
    , preZoom = Nothing, colour = fgc, label = Nothing }

spec :: Spec
spec = do
    describe "the default colours" $
        it "are hat's ANSI blue, and red for the active pane, under a client's theme" $ do
            let th = clientTheme defaultOptions
            colourValue th defaultOptions.displayPanesColour `shouldBe` Just (Just blue)
            colourValue th defaultOptions.displayPanesActiveColour
                `shouldBe` Just (Just (Cell.Indexed 1))

    describe "panesNumber" $ do
        let label = Just (\_ -> [ (i, Cell.glyphCell ch fgc) | (i, ch) <- zip [7 ..] "11x7" ])
        it "draws a big 5x5 digit centered in a roomy pane" $
            -- "1" is the rightmost column of its glyph; in an 11x7 pane
            -- the block starts at col (11-5)/2 = 3, row (7-5)/2 = 1.
            blocksOf (panesNumber fgc 1 Nothing 11 7) `shouldBe` [ Pos (1 + j) 7 | j <- [0 .. 4] ]
        it "draws display-panes-format on the pane's top row" $
            textOf (panesNumber fgc 1 label 11 7) `shouldBe`
                [ (Pos 0 (7 + i), ch) | (i, ch) <- zip [0 ..] "11x7" ]
        it "falls back to the number as text when the pane is too short" $
            -- 12x3: no room for 5 block rows; "3" centered on row 1.
            textOf (panesNumber fgc 3 Nothing 12 3) `shouldBe` [(Pos 1 5, '3')]
        it "needs two more rows for the blocks when there is a format" $
            blocksOf (panesNumber fgc 1 label 11 6) `shouldBe` []
        it "names panes 10 to 34 by letter too" $ do
            textOf (panesNumber fgc 12 Nothing 4 3)
                `shouldBe` [(Pos 1 0, '1'), (Pos 1 1, '2'), (Pos 1 2, ' '), (Pos 1 3, 'c')]
            textOf (panesNumber fgc 12 Nothing 11 8) `shouldBe` [(Pos 6 10, 'c')]

    describe "panesKeyIndex" $
        it "reads digits, and unmodified letters from 10" $
            map panesKeyIndex ["3", "a", "z", "A", "M-a", "Enter"]
                `shouldBe` [Just 3, Just 10, Just 35, Nothing, Nothing, Nothing]

    describe "panesPicture" $ do
        let picture sz status = panesPicture PanesPicture
                { root = sideBySide, size = sz, status, panes = [pane 0, pane 1]
                , floatOrder = [], border = Cell.defaultStyle }
        it "lays the panes out as the window does when it fits" $
            snd (picture (Size 24 80) PaneStatusOff) `shouldBe`
                [(PaneId 0, Rect 0 24 0 40), (PaneId 1, Rect 0 24 41 80)]
        it "scales a bigger window down" $
            snd (picture (Size 12 40) PaneStatusOff) `shouldBe`
                [(PaneId 0, Rect 0 12 0 20), (PaneId 1, Rect 0 12 20 40)]
        it "keeps the pane status row out of the panes" $
            snd (picture (Size 24 80) PaneStatusTop) `shouldBe`
                [(PaneId 0, Rect 1 24 0 40), (PaneId 1, Rect 1 24 41 80)]
        it "draws the dividers, joined where they meet" $ do
            let rows = fst $ panesPicture PanesPicture
                    { root = Layout 80 24 0 0 $ NodeCell LeftRight
                        [ Layout 40 24 0 0 (LeafCell (PaneId 0))
                        , Layout 39 24 41 0 $ NodeCell TopBottom
                            [ Layout 39 12 41 0 (LeafCell (PaneId 1))
                            , Layout 39 11 41 13 (LeafCell (PaneId 2)) ] ]
                    , size = Size 24 80, status = PaneStatusOff
                    , panes = map pane [0, 1, 2], floatOrder = [], border = Cell.defaultStyle }
                at r c = Cell.baseChar (rows V.! r V.! c)
            map (at 12) [40, 41] `shouldBe` "├─"
            at 5 40 `shouldBe` '│'
