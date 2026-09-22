module Hat.Server.RenderSpec (spec) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Geometry
import Hat.Server.Render
import Hat.Term.Cell
import Hat.Transport.Wire (DrawOp (..))

-- ASCII-only frames: the reference interpreter below doesn't model
-- double-width characters (the emulator marks those; diffing keeps
-- wide+continuation pairs atomic, unit-tested separately).
genCell :: Gen Cell
genCell = do
    ch <- chooseEnum (' ', '~')
    st <- elements
        [ defaultStyle
        , defaultStyle { bold = True }
        , defaultStyle { fg = Indexed 1 }
        , defaultStyle { bg = Indexed 4, underline = True }
        ]
    pure (glyphCell ch st)

genFrame :: Size -> Gen Frame
genFrame sz =
    V.replicateM (fromIntegral sz.rows)
        (V.replicateM (fromIntegral sz.cols) genCell)

smallSize :: Size
smallSize = Size { rows = 6, cols = 12 }

-- Rects may hang past (or start before) the frame; grids may be any
-- size relative to the rect.
genOverlay :: Gen (Frame, Rect, V.Vector (V.Vector Cell))
genOverlay = do
    frame <- genFrame smallSize
    rect <- Rect
        <$> chooseInt (-2, 8) <*> chooseInt (-2, 8)
        <*> chooseInt (-2, 15) <*> chooseInt (-2, 15)
    gr <- chooseInt (0, 8)
    gc <- chooseInt (0, 15)
    grid <- V.replicateM gr (V.replicateM gc genCell)
    pure (frame, rect, grid)

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
    prop "diff ops transform old frame into new frame" $
        forAll ((,) <$> genFrame smallSize <*> genFrame smallSize) $
            \(old, new) -> applyOps old (diffFrame old new) === new

    prop "identical frames need no ops" $
        forAll (genFrame smallSize) $ \frame ->
            diffFrame frame frame === []

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

    prop "overlays a grid clipped to both rect and frame" $
        forAll genOverlay $ \(frame, rect, grid) ->
            let expect r c
                    | r >= rect.startRow, r < rect.endRow
                    , c >= rect.startCol, c < rect.endCol =
                        maybe blankCell id
                            (grid V.!? (r - rect.startRow)
                                >>= (V.!? (c - rect.startCol)))
                    | otherwise = frame V.! r V.! c
            in V.imap (\r -> V.imap (\c _ -> expect r c)) frame
                === overlayGrid frame rect grid

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
