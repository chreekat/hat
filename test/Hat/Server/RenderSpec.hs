module Hat.Server.RenderSpec (spec) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Geometry
import Hat.Model.Ids (PaneId (..))
import Hat.Server.Render
import Hat.Term.Cell
import Hat.Transport.Wire (DrawOp (..))

-- | The styles a generated cell is drawn in, told apart by index when
-- cells are shown.
cellStyles :: [Style]
cellStyles =
    [ defaultStyle
    , defaultStyle { bold = True }
    , defaultStyle { fg = Indexed 1 }
    , defaultStyle { bg = Indexed 4, underline = True }
    ]

-- ASCII-only frames: the reference interpreter below doesn't model
-- double-width characters (the emulator marks those; diffing keeps
-- wide+continuation pairs atomic, unit-tested separately).
genCell :: Gen Cell
genCell = glyphCell <$> chooseEnum (' ', '~') <*> elements cellStyles

-- | Toward a blank: unstyled first, then a space.
shrinkCell :: Cell -> [Cell]
shrinkCell c =
    [ glyphCell ch defaultStyle | c.style /= defaultStyle ]
    <> [ glyphCell ' ' c.style | ch /= ' ' ]
  where
    ch = baseChar c

styleIx :: Cell -> String
styleIx c = maybe "?" show (List.elemIndex c.style cellStyles)

type Cells = V.Vector (V.Vector Cell)

genCells :: Int -> Int -> Gen Cells
genCells rows cols = V.replicateM rows (V.replicateM cols genCell)

-- | One line per row: the glyphs, then each cell's style index.
showCells :: Cells -> String
showCells = concatMap row . V.toList
  where
    row cs = "\n" <> map baseChar (V.toList cs) <> " |" <> concatMap styleIx (V.toList cs)

-- | Equal cells, or both shown.
sameCells :: Cells -> Cells -> Property
sameCells a b = counterexample (showCells a <> "\n/=" <> showCells b) (a == b)

sameRow :: V.Vector Cell -> V.Vector Cell -> Property
sameRow a b = sameCells (V.singleton a) (V.singleton b)

-- | One cell shrunk, the shape kept.
shrinkCells :: Cells -> [Cells]
shrinkCells rows =
    [ rows V.// [(r, row V.// [(c, cell')])]
    | (r, row) <- V.toList (V.indexed rows)
    , (c, cell) <- V.toList (V.indexed row)
    , cell' <- shrinkCell cell ]

-- | One row dropped, with its index.
droppedRows :: Cells -> [(Int, Cells)]
droppedRows rows = [ (r, dropAt r rows) | r <- [0 .. V.length rows - 1] ]

-- | One column dropped.
droppedCols :: Cells -> [Cells]
droppedCols rows =
    [ V.map (dropAt c) rows | c <- [0 .. maybe 0 V.length (rows V.!? 0) - 1] ]

dropAt :: Int -> V.Vector a -> V.Vector a
dropAt i = V.ifilter (\j _ -> j /= i)

-- | Edges toward the origin.
shrinkRect :: Rect -> [Rect]
shrinkRect r =
    [ Rect sr er sc ec | (sr, er, sc, ec) <- shrink (r.startRow, r.endRow, r.startCol, r.endCol) ]

smallSize :: Size
smallSize = Size { rows = 6, cols = 12 }

-- | A client frame of 'smallSize'.
newtype SmallFrame = SmallFrame Frame

instance Show SmallFrame where
    show (SmallFrame f) = showCells f

instance Arbitrary SmallFrame where
    arbitrary = SmallFrame <$> genSmallFrame
    shrink (SmallFrame f) = SmallFrame <$> shrinkCells f

genSmallFrame :: Gen Frame
genSmallFrame = genCells (fromIntegral smallSize.rows) (fromIntegral smallSize.cols)

-- | A rect that may hang past (or start before) a small frame.
genOverlayRect :: Gen Rect
genOverlayRect = Rect
    <$> chooseInt (-2, 8) <*> chooseInt (-2, 8)
    <*> chooseInt (-2, 15) <*> chooseInt (-2, 15)

-- | A grid of any size relative to its rect.
genOverlayCells :: Gen Cells
genOverlayCells = do
    gr <- chooseInt (0, 8)
    gc <- chooseInt (0, 15)
    genCells gr gc

-- | A grid laid over a frame through a rect.
data Overlay = Overlay Frame Rect Cells

instance Show Overlay where
    show (Overlay f r g) = "frame" <> showCells f <> "\n" <> show r <> "\ngrid" <> showCells g

instance Arbitrary Overlay where
    arbitrary = Overlay <$> genSmallFrame <*> genOverlayRect <*> genOverlayCells
    shrink (Overlay f r g) =
        [ Overlay f' r g | f' <- shrinkCells f ]
        <> [ Overlay f r' g | r' <- shrinkRect r ]
        <> [ Overlay f r g' | g' <- map snd (droppedRows g) <> droppedCols g <> shrinkCells g ]

-- | A pane's layer: its rect, its cells and, except in copy mode, a
-- generation stamp per row.
data Layer = Layer Rect Cells (Maybe (V.Vector Int))

instance Show Layer where
    show (Layer r cells gens) = show r <> " gens " <> show gens <> showCells cells

genLayer :: Gen Layer
genLayer = do
    rect <- genOverlayRect
    cells <- genOverlayCells
    stamped <- arbitrary
    gens <- V.fromList <$> vectorOf (V.length cells) (chooseInt (0, 5))
    pure (Layer rect cells (if stamped then Just gens else Nothing))

shrinkLayer :: Layer -> [Layer]
shrinkLayer (Layer r cells gens) =
    [ Layer r' cells gens | r' <- shrinkRect r ]
    <> [ Layer r cells' (dropAt i <$> gens) | (i, cells') <- droppedRows cells ]
    <> [ Layer r cells' gens | cells' <- droppedCols cells <> shrinkCells cells ]
    <> [ Layer r cells Nothing | Just _ <- [gens] ]
    <> [ Layer r cells (Just (gs V.// [(i, g')]))
       | Just gs <- [gens], (i, g) <- V.toList (V.indexed gs), g' <- shrink g ]

-- | The layers as panes 1.., in order.
paneLayers :: [Layer] -> [PaneLayer]
paneLayers layers =
    [ PaneLayer { pane = PaneId i, rect, cells, gens }
    | (i, Layer rect cells gens) <- zip [1 ..] layers ]

-- | Chrome cells plus pane layers.
data Compose = Compose [(Pos, Cell)] [Layer]

instance Show Compose where
    show (Compose chrome layers) =
        "chrome " <> show [ (p.row, p.col, baseChar c, styleIx c) | (p, c) <- chrome ]
        <> concat [ "\nlayer " <> show l | l <- layers ]

instance Arbitrary Compose where
    arbitrary = do
        nLayers <- chooseInt (0, 3)
        layers <- vectorOf nLayers genLayer
        nChrome <- chooseInt (0, 10)
        chrome <- vectorOf nChrome $ (,)
            <$> (Pos <$> chooseInt (0, 5) <*> chooseInt (0, 11))
            <*> genCell
        pure (Compose chrome layers)
    shrink (Compose chrome layers) =
        [ Compose chrome' layers | chrome' <- shrinkList shrinkChrome chrome ]
        <> [ Compose chrome layers' | layers' <- shrinkList shrinkLayer layers ]
      where
        shrinkChrome (p, c) =
            [ (Pos r co, c) | (r, co) <- shrink (p.row, p.col) ]
            <> [ (p, c') | c' <- shrinkCell c ]

-- Reference interpreter: what a (single-width) terminal would show
-- after executing the ops.
applyOps :: Frame -> [DrawOp] -> Frame
applyOps = foldl apply
  where
    apply frame = \case
        ClearAll -> V.map (V.map (const blankCell)) frame
        CursorAt _ _ -> frame
        Put pos st txt ->
            case frame V.!? pos.row of
                Nothing -> frame
                Just row ->
                    let updates =
                            [ (c, glyphCell ch st)
                            | (i, ch) <- zip [0 ..] (T.unpack txt)
                            , let c = pos.col + i
                            , c < V.length row
                            ]
                    in frame V.// [(pos.row, row V.// updates)]

spec :: Spec
spec = do
    describe "pane default colours (window-style, window-active-style)" $ do
        let ws = defaultStyle { fg = Indexed 2, bg = Indexed 4 }
            was = defaultStyle { bg = Indexed 1 }
            none = (DefaultColor, DefaultColor)
        it "the active pane takes window-active-style where it sets a colour" $
            paneDefaults True none ws was `shouldBe` (Indexed 2, Indexed 1)
        it "other panes take window-style" $
            paneDefaults False none ws was `shouldBe` (Indexed 2, Indexed 4)
        it "the program's own colours show where the styles leave them" $
            paneDefaults False (RGB 1 1 1, RGB 2 2 2) defaultStyle { fg = Indexed 3 } was
                `shouldBe` (Indexed 3, RGB 2 2 2)
        it "the active pane's own background beats window-style" $
            paneDefaults True (DefaultColor, RGB 2 2 2) ws defaultStyle
                `shouldBe` (Indexed 2, RGB 2 2 2)
        it "a pane palette recolours its palette entries" $ do
            let cells = V.singleton (V.fromList
                    [ glyphCell 'a' defaultStyle { fg = Indexed 1, bg = Indexed 2 }
                    , glyphCell 'b' defaultStyle { fg = Indexed 3 } ])
                palette = Map.fromList [(1, RGB 1 2 3), (2, Indexed 200)]
            V.map (V.map (.style)) (paintPalette palette cells)
                `shouldBe` V.singleton (V.fromList
                    [ defaultStyle { fg = RGB 1 2 3, bg = Indexed 200 }
                    , defaultStyle { fg = Indexed 3 } ])
        it "paints only the default colours of a pane's cells" $ do
            let cells = V.singleton (V.fromList
                    [ glyphCell 'a' defaultStyle
                    , glyphCell 'b' defaultStyle { fg = Indexed 7 } ])
            V.map (V.map (.style)) (paintDefaults (Indexed 2, Indexed 4) cells)
                `shouldBe` V.singleton (V.fromList
                    [ defaultStyle { fg = Indexed 2, bg = Indexed 4 }
                    , defaultStyle { fg = Indexed 7, bg = Indexed 4 } ])
    prop "diff ops transform old frame into new frame" $
        \(SmallFrame old) (SmallFrame new) -> applyOps old (diffFrame old new) `sameCells` new

    prop "identical frames need no ops" $
        \(SmallFrame frame) -> diffFrame frame frame === []

    it "keeps wide-char pairs atomic" $ do
        let wide = Cell { content = Glyph '日' [] Wide, style = defaultStyle }
            cont = Cell { content = Continuation, style = defaultStyle }
            a = glyphCell 'a' defaultStyle
            row cs = V.fromList [V.fromList cs]
            old = row [wide, cont, a]
            new = row [a, a, a]
        -- Overwriting a wide char redraws both of its columns in one
        -- run; the untouched third column is not re-sent.
        let ops = diffFrame old new
        ops `shouldBe` [Put Pos { row = 0, col = 0 } defaultStyle "aa"]

    it "sends the cell before one led by a zero-width character" $ do
        let iso = Cell { content = Glyph '\x2069' ['|'] Narrow, style = defaultStyle { bold = True } }
            row cs = V.fromList [V.fromList cs]
            old = row [blankCell, blankCell, blankCell]
            new = row [blankCell, blankCell, iso]
        diffFrame old new `shouldBe`
            [ Put Pos { row = 0, col = 1 } defaultStyle " "
            , Put Pos { row = 0, col = 2 } defaultStyle { bold = True } "\x2069|" ]

    prop "a known-equal row mask never changes the diff" $
        \(SmallFrame old) (SmallFrame new) (coins :: [Bool]) ->
            -- Sound mask: true only on genuinely equal rows (any subset).
            let known r = old V.!? r == new V.!? r && lookup r (zip [0 ..] coins) == Just True
            in diffFrameKnown known old new === diffFrame old new

    prop "composeRows equals overlays-then-borders composition" $
        \(Compose cells ls) ->
            let layers = paneLayers ls
                legacy = applyBorders
                    (foldl (\acc l -> overlayGrid acc l.rect l.cells) (blankFrame smallSize) layers)
                    cells
                chrome = chromeFromCells cells
            in fst (composeRows smallSize chrome layers V.empty V.empty)
                `sameCells` legacy

    prop "reuses a previous row exactly when provenance matches" $
        \(Compose cells ls) ->
            let layers = paneLayers ls
                chrome = chromeFromCells cells
                (f1, o1) = composeRows smallSize chrome layers V.empty V.empty
                poison = V.map (V.map (const (glyphCell '!' defaultStyle))) f1
                (f2, o2) = composeRows smallSize chrome layers poison o1
            in conjoin
                [ sameRow (f2 V.! r)
                    ((if sameOrigin (o1 V.! r) (o2 V.! r) then poison else f1) V.! r)
                | r <- [0 .. V.length f1 - 1] ]

    prop "overlays a grid clipped to both rect and frame" $
        \(Overlay frame rect grid) ->
            let expect r c
                    | r >= rect.startRow, r < rect.endRow
                    , c >= rect.startCol, c < rect.endCol =
                        maybe blankCell id
                            (grid V.!? (r - rect.startRow)
                                >>= (V.!? (c - rect.startCol)))
                    | otherwise = frame V.! r V.! c
            in V.imap (\r -> V.imap (\c _ -> expect r c)) frame
                `sameCells` overlayGrid frame rect grid

    it "pads and clips a pane screen into a client frame" $ do
        let paneCells = V.fromList
                [ V.fromList [c 'h', c 'i'] ]
            c ch = glyphCell ch defaultStyle
            frame = composeFrame Size { rows = 2, cols = 3 } paneCells
        V.length frame `shouldBe` 2
        V.map V.length frame `shouldBe` V.fromList [3, 3]
        baseChar (frame V.! 0 V.! 0) `shouldBe` 'h'
        baseChar (frame V.! 0 V.! 2) `shouldBe` ' '
        baseChar (frame V.! 1 V.! 0) `shouldBe` ' '
