module Hat.Server.SceneSpec (spec) where

import Test.Hspec

import Data.Map.Strict qualified as Map
import Data.Vector qualified as V

import Hat.Geometry (Pos (..), Rect (..), Size (..))
import Hat.Model.Ids (PaneId (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (utc, utcToZonedTime)
import Hat.Model.Options (BorderLines (..), defaultOptions, optionText)
import Hat.Server.Format (renderFormat)
import Hat.Server.Layout (Orientation (..))
import Hat.Server.Scene
import Hat.Server.Theme (terminalTheme)
import Hat.Term.Cell qualified as Cell
import Hat.TestSupport (clientTheme)

-- | A scene's chrome as text rows, pane cells as @.@.
picture :: ChromeLook -> Scene -> [String]
picture look scene =
    [ [ maybe '.' Cell.baseChar (lookup (Pos y x) chrome) | x <- [0 .. w - 1] ]
    | y <- [0 .. V.length scene - 1] ]
  where
    chrome = sceneChrome look scene
    w = maybe 0 V.length (scene V.!? 0)

plainLook :: ChromeLook
plainLook = ChromeLook
    { borderLines = BorderSingle, windowStyle = Cell.defaultStyle
    , paneStyles = Map.empty, scrollbarLooks = Map.empty, active = Nothing, marked = Nothing
    , numbers = Map.fromList [ (PaneId i, fromIntegral i) | i <- [0 .. 9] ]
    , paneLines = Map.empty, lineSets = Map.empty
    , fillInside = Cell.glyphCell ' ' Cell.defaultStyle
    , fillOutside = Cell.glyphCell '~' Cell.defaultStyle
    , arrows = Map.empty, menuCells = V.empty }

rect :: Int -> Int -> Int -> Int -> Rect
rect r0 r1 c0 c1 = Rect { startRow = r0, endRow = r1, startCol = c0, endCol = c1 }

-- | A 7x5 window split into four 3x2 panes.
cross :: SceneInput
cross = SceneInput
    { window = Size { rows = 5, cols = 7 }, area = Size { rows = 5, cols = 7 }
    , panes =
        [ (PaneId 0, rect 0 2 0 3), (PaneId 1, rect 0 2 4 7)
        , (PaneId 2, rect 3 5 0 3), (PaneId 3, rect 3 5 4 7) ]
    , paneStatus = PaneStatusOff, twoPane = Nothing
                    , scrollbars = Map.empty, scrollbarSide = ScrollbarRight, overlays = Map.empty, floats = [], menu = Nothing }

spec :: Spec
spec = do
    describe "buildScene" $ do
        it "joins crossing borders into one junction" $
            picture plainLook (buildScene cross) `shouldBe`
                [ "...│..."
                , "...│..."
                , "───┼───"
                , "...│..."
                , "...│..." ]
        it "draws the window's far edges and fills beyond them" $ do
            let inp = SceneInput
                    { window = Size { rows = 2, cols = 3 }
                    , area = Size { rows = 4, cols = 6 }
                    , panes = [ (PaneId 0, rect 0 2 0 3) ]
                    , paneStatus = PaneStatusOff, twoPane = Nothing
                    , scrollbars = Map.empty, scrollbarSide = ScrollbarRight, overlays = Map.empty, floats = [], menu = Nothing }
            picture plainLook (buildScene inp) `shouldBe`
                [ "...│~~"
                , "...│~~"
                , "───┘~~"
                , "~~~~~~" ]
        it "gives a top status line the border from two columns in" $ do
            let inp = cross { panes = [ (PaneId 0, rect 1 5 0 3), (PaneId 1, rect 1 5 4 7) ]
                            , paneStatus = PaneStatusTop }
                scene = buildScene inp
            [ (off, ct) | SceneStatus (PaneId 1) off ct <- V.toList (scene V.! 0) ]
                `shouldBe` [ (0, CellLR) ]
            picture plainLook scene !! 0 `shouldBe` "───┬───"
    describe "the window's menu" $
        it "covers every pane, border and floating pane under its box, clipped to the window" $ do
            let float = FloatingPane
                    { pane = PaneId 9, rect = rect 2 3 2 5, lineSet = BorderSingle, status = PaneStatusOff }
                menuLook = plainLook { menuCells = V.fromList
                    [ V.fromList [ Cell.glyphCell c Cell.defaultStyle | c <- row ] | row <- ["abcd", "efgh"] ] }
            picture menuLook (buildScene cross { floats = [float], menu = Just (rect 1 3 5 9) })
                `shouldBe`
                [ "...│..."
                , ".┌───ab"
                , "─│...ef"
                , ".└───┘."
                , "...│..." ]
    describe "floating panes" $ do
        let whole = cross { panes = [ (PaneId 0, rect 0 5 0 7) ] }
            float r = FloatingPane
                { pane = PaneId 9, rect = r, lineSet = BorderSingle, status = PaneStatusOff }
        it "draw their own box over the tiled panes" $
            picture plainLook (buildScene whole { floats = [ float (rect 2 3 2 5) ] })
                `shouldBe`
                [ "......."
                , ".┌───┐."
                , ".│...│."
                , ".└───┘."
                , "......." ]
        it "cover the tiled borders, never joining them" $
            picture plainLook (buildScene cross { floats = [ float (rect 2 3 2 5) ] })
                `shouldBe`
                [ "...│..."
                , ".┌───┐."
                , "─│...│─"
                , ".└───┘."
                , "...│..." ]
        it "stop at the window's edges, dropping the border cut off there" $
            take 3 (picture plainLook (buildScene whole { floats = [ float (rect 0 2 5 9) ] }))
                `shouldBe` [ "....│..", "....│..", "....└──" ]
        it "the frontmost covers those behind" $
            picture plainLook (buildScene whole
                    { floats = [ float (rect 1 2 1 4), (float (rect 2 3 3 6)) { pane = PaneId 8 } ] })
                `shouldBe`
                [ "┌───┐.."
                , "│.┌───┐"
                , "└─│...│"
                , "..└───┘"
                , "......." ]
        it "draw no border under pane-border-lines none" $
            picture plainLook (buildScene whole
                    { floats = [ (float (rect 2 3 2 5)) { lineSet = BorderNone } ] })
                `shouldBe` replicate 5 "......."
        it "draw their border in their own line set" $
            picture plainLook { lineSets = Map.singleton (PaneId 9) BorderDouble }
                    (buildScene whole { floats = [ float (rect 2 3 2 5) ] }) !! 1
                `shouldBe` ".╔═══╗."
        it "carry their own pane status line" $ do
            let scene = buildScene whole
                    { floats = [ (float (rect 2 3 1 6)) { status = PaneStatusTop } ] }
            [ (off, ct) | SceneStatus (PaneId 9) off ct <- V.toList (scene V.! 1) ]
                `shouldBe` [ (0, CellLR), (1, CellLR), (2, CellLR) ]
        it "take their scrollbar inside their border" $ do
            let scene = buildScene whole
                    { floats = [ float (rect 2 3 2 4) ]
                    , scrollbars = Map.singleton (PaneId 9) 1 }
            (scene V.! 2) V.! 4 `shouldBe` SceneScrollbar (PaneId 9) 0 0
            picture plainLook scene !! 1 `shouldBe` ".┌───┐."
    describe "two-pane colours" $
        it "splits the dividing border between the panes beside it" $ do
            let inp = SceneInput
                    { window = Size { rows = 4, cols = 3 }, area = Size { rows = 4, cols = 3 }
                    , panes = [ (PaneId 0, rect 0 4 0 1), (PaneId 1, rect 0 4 2 3) ]
                    , paneStatus = PaneStatusOff, twoPane = Just LeftRight
                    , scrollbars = Map.empty, scrollbarSide = ScrollbarRight, overlays = Map.empty, floats = [], menu = Nothing }
                owners = [ case (buildScene inp V.! y) V.! 1 of
                               SceneBorder b -> b.styleOwner
                               _ -> Nothing
                         | y <- [0 .. 3] ]
            owners `shouldBe` map (Just . PaneId) [0, 0, 0, 1]
    describe "scrollbars" $ do
        let look = plainLook { scrollbarLooks = Map.singleton (PaneId 0) ScrollbarLook
                { style = ScrollbarStyle
                    { colours = Cell.defaultStyle { Cell.fg = Cell.Indexed 7, Cell.bg = Cell.Indexed 0 }
                    , width = 1, pad = 1 }
                , slider = (2, 1), padStyle = Cell.defaultStyle, onLeft = False } }
            inp = SceneInput
                { window = Size { rows = 3, cols = 5 }, area = Size { rows = 3, cols = 5 }
                , panes = [ (PaneId 0, rect 0 3 0 3) ], paneStatus = PaneStatusOff
                , twoPane = Nothing, scrollbars = Map.singleton (PaneId 0) 2
                , scrollbarSide = ScrollbarRight, overlays = Map.empty, floats = [], menu = Nothing }
            chrome = sceneChrome look (buildScene inp)
            bgAt x y = (.style.bg) <$> lookup (Pos y x) chrome
        it "takes the columns between the pane and its border" $
            picture look (buildScene inp) `shouldBe` [ "...  ", "...  ", "...  " ]
        it "pads beside the pane, draws the bar, swaps colours on the slider" $
            map (\y -> (bgAt 3 y, bgAt 4 y)) [0, 2] `shouldBe`
                [ (Just Cell.DefaultColor, Just (Cell.Indexed 0))
                , (Just Cell.DefaultColor, Just (Cell.Indexed 7)) ]
    describe "overlay scrollbars (modal, auto-hide)" $ do
        let inp side = SceneInput
                { window = Size { rows = 2, cols = 5 }, area = Size { rows = 2, cols = 5 }
                , panes = [ (PaneId 0, rect 0 2 0 3), (PaneId 1, rect 0 2 4 5) ]
                , paneStatus = PaneStatusOff, twoPane = Nothing, scrollbars = Map.empty
                , scrollbarSide = side, overlays = Map.singleton (PaneId 0) 2
                , floats = [], menu = Nothing }
            look = plainLook { scrollbarLooks = Map.singleton (PaneId 0) ScrollbarLook
                { style = ScrollbarStyle
                    { colours = Cell.defaultStyle, width = 1, pad = 1 }
                , slider = (0, 2), padStyle = Cell.defaultStyle, onLeft = False } }
        it "covers the pane's last columns, its border where it was" $
            picture look (buildScene (inp ScrollbarRight)) `shouldBe` [ ".  │.", ".  │." ]
        it "on the left, covers its first columns" $ do
            let scene = buildScene (inp ScrollbarLeft)
            (scene V.! 0) V.! 0 `shouldBe` SceneScrollbar (PaneId 0) 0 0
            (scene V.! 0) V.! 2 `shouldBe` ScenePane (PaneId 0)
        it "is its width and padding, or its width alone, within the pane" $
            map (uncurry overlayWidth) [((1, 1), 5), ((2, 3), 4), ((6, 0), 4)]
                `shouldBe` [2, 2, 4]
    describe "sliderOf" $ do
        it "with no history fills the bar" $
            sliderOf 10 10 0 Nothing `shouldBe` (0, 10)
        it "sits at the bottom, sized by the screen's share of the history" $
            sliderOf 10 10 30 Nothing `shouldBe` (8, 2)
        it "in copy mode follows the viewport's offset" $
            sliderOf 10 10 30 (Just (0, 30)) `shouldBe` (0, 2)
    describe "scrollbarStyle" $ do
        it "reads width and pad" $
            (\s -> (s.width, s.pad)) (scrollbarStyle terminalTheme "width=2,pad=1") `shouldBe` (2, 1)
        it "keeps the default for what the option leaves" $
            (scrollbarStyle terminalTheme "fg=red").colours.bg `shouldBe` Cell.Indexed 0
        it "is at least a cell wide" $
            (scrollbarStyle terminalTheme "width=0").width `shouldBe` 1
    describe "fillCell" $ do
        let border = Cell.defaultStyle { Cell.fg = Cell.Indexed 7, Cell.bold = True }
        it "draws the expanded fill-character over the border style" $ do
            let c = fillCell terminalTheme border "#[fg=colour3]#[acs]~"
            Cell.baseChar c `shouldBe` '\x00b7'
            c.style `shouldBe` border { Cell.fg = Cell.Indexed 3 }
        it "keeps the border colours a fill leaves default" $ do
            let c = fillCell terminalTheme border "#[bg=colour4]x"
            (Cell.baseChar c, c.style) `shouldBe` ('x', border { Cell.bg = Cell.Indexed 4 })
        it "an empty fill is a blank" $
            Cell.baseChar (fillCell terminalTheme border "") `shouldBe` ' '
        it "hat's default fill leaves the cells blank, inside the window or out" $ do
            let fillAt inside = fillCell (clientTheme defaultOptions) Cell.defaultStyle
                    (renderFormat (Map.fromList [("is_inside", inside)]) (const "")
                        (utcToZonedTime utc (posixSecondsToUTCTime 0))
                        (optionText "fill-character" defaultOptions))
            [ (Cell.baseChar c, c.style) | c <- map fillAt ["1", "0"] ]
                `shouldBe` replicate 2 (' ', Cell.defaultStyle)
        it "a fill too wide for its cell is a blank" $
            Cell.baseChar (fillCell terminalTheme border "中") `shouldBe` ' '
    describe "borderGlyph" $ do
        it "number draws the owner's index" $
            borderGlyph BorderNumber (Just 13) CellUD `shouldBe` '3'
        it "spaces and none draw blanks" $
            map (\l -> borderGlyph l Nothing CellLRUD) [BorderSpaces, BorderNone]
                `shouldBe` "  "
        it "heavy, double and simple have every junction" $
            map (\l -> borderGlyph l Nothing CellURD) [BorderHeavy, BorderDouble, BorderSimple]
                `shouldBe` "\x2523\x2560+"
    describe "statusAdjust" $ do
        it "moves a top-edge pane down a row for a top status line" $
            statusAdjust PaneStatusTop 10 (rect 0 10 0 5) `shouldBe` rect 1 10 0 5
        it "takes the last row of a bottom-edge pane for a bottom one" $
            statusAdjust PaneStatusBottom 10 (rect 4 10 0 5) `shouldBe` rect 4 9 0 5
        it "leaves inner panes alone" $
            statusAdjust PaneStatusTop 10 (rect 4 10 0 5) `shouldBe` rect 4 10 0 5
