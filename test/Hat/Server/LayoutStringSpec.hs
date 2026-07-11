module Hat.Server.LayoutStringSpec (spec) where

import Data.Ratio ((%))
import qualified Data.Text as T
import Test.Hspec

import Hat.Geometry (Rect (..))
import Hat.Model.Ids (PaneId (..))
import Hat.Server.Layout (Layout (..), Orientation (..), layoutPanes)
import Hat.Server.LayoutString

win :: Rect
win = Rect { startRow = 0, endRow = 24, startCol = 0, endCol = 80 }

spec :: Spec
spec = do
    describe "emitLayout" $ do
        it "emits a leaf as WxH,x,y,id with a checksum prefix" $
            emitLayout win (Leaf (PaneId 3))
                `shouldBe` prefixed "80x24,0,0,3"
        it "uses {} for a left-right split and [] for top-bottom" $ do
            let lr = Split LeftRight (1 % 2) (Leaf (PaneId 0)) (Leaf (PaneId 1))
            emitLayout win lr `shouldSatisfy` T.isInfixOf "{"
            let tb = Split TopBottom (1 % 2) (Leaf (PaneId 0)) (Leaf (PaneId 1))
            emitLayout win tb `shouldSatisfy` T.isInfixOf "["

    describe "layoutFromString" $ do
        it "round-trips a split layout back to the same pane structure" $ do
            let lay = Split LeftRight (1 % 2)
                    (Leaf (PaneId 0))
                    (Split TopBottom (1 % 2) (Leaf (PaneId 1)) (Leaf (PaneId 2)))
                str = emitLayout win lay
            case layoutFromString str [PaneId 10, PaneId 11, PaneId 12] of
                Just lay' -> layoutPanes lay'
                    `shouldBe` [PaneId 10, PaneId 11, PaneId 12]
                Nothing -> expectationFailure "failed to parse emitted layout"
        it "preserves the split shape (orientations) on round-trip" $ do
            let lay = Split TopBottom (1 % 3) (Leaf (PaneId 0)) (Leaf (PaneId 1))
                str = emitLayout win lay
            case layoutFromString str [PaneId 0, PaneId 1] of
                Just (Split TopBottom _ (Leaf _) (Leaf _)) -> pure ()
                other -> expectationFailure ("wrong shape: " <> show other)
        it "rejects a layout string with too few panes" $
            layoutFromString (emitLayout win (Leaf (PaneId 0))) []
                `shouldBe` Nothing
  where
    prefixed body = hex4 body <> "," <> T.pack body
    -- Recompute the checksum the same way the codec does, for the leaf test.
    hex4 body =
        let s = layoutChecksum (T.pack body)
            h = showHex' s
        in T.pack (replicate (4 - length h) '0') <> T.pack h
    showHex' n = go n ""
      where
        go 0 acc | null acc = "0"
                 | otherwise = acc
        go k acc = go (k `div` 16) (hexDigit (k `mod` 16) : acc)
        hexDigit d | d < 10 = toEnum (fromEnum '0' + d)
                   | otherwise = toEnum (fromEnum 'a' + d - 10)
