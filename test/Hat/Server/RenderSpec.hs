module Hat.Server.RenderSpec (spec) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Geometry
import Hat.Server.Render
import Hat.Term.Cell
import Hat.Wire (DrawOp (..))

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
    pure Cell { text = T.singleton ch, width = 1, style = st }

genFrame :: Size -> Gen Frame
genFrame sz =
    V.replicateM (fromIntegral sz.rows)
        (V.replicateM (fromIntegral sz.cols) genCell)

smallSize :: Size
smallSize = Size { rows = 6, cols = 12 }

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
                            [ (c, Cell { text = T.singleton ch, width = 1, style = st })
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
        let wide = Cell { text = "日", width = 2, style = defaultStyle }
            cont = Cell { text = "", width = 0, style = defaultStyle }
            a = Cell { text = "a", width = 1, style = defaultStyle }
            row cs = V.fromList [V.fromList cs]
            old = row [wide, cont, a]
            new = row [a, a, a]
        -- Overwriting a wide char redraws both of its columns in one
        -- run; the untouched third column is not re-sent.
        let ops = diffFrame old new
        ops `shouldBe` [Put Pos { row = 0, col = 0 } defaultStyle "aa"]

    it "pads and clips a pane screen into a client frame" $ do
        let paneCells = V.fromList
                [ V.fromList [c 'h', c 'i'] ]
            c ch = Cell { text = T.singleton ch, width = 1, style = defaultStyle }
            frame = composeFrame Size { rows = 2, cols = 3 } paneCells
        V.length frame `shouldBe` 2
        V.map V.length frame `shouldBe` V.fromList [3, 3]
        (frame V.! 0 V.! 0).text `shouldBe` "h"
        (frame V.! 0 V.! 2).text `shouldBe` " "
        (frame V.! 1 V.! 0).text `shouldBe` " "
