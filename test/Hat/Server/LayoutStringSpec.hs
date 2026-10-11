module Hat.Server.LayoutStringSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)
import Test.Hspec

import Hat.Geometry (Size (..))
import Hat.Model.Ids (PaneId (..))
import Hat.Server.Layout
    ( FloatGeom (..), Layout (..), Orientation (..), Placement (..), SplitSpan (..)
    , SplitSpec (..), fitLayout, floatGeom, floatPane, layoutPanes, singlePane
    , splitPane, tiledPanes )
import Hat.Server.LayoutString

area :: Size
area = Size { rows = 24, cols = 80 }

-- | A body with its checksum prefix, as tmux prints it.
prefixed :: Text -> Text
prefixed body = T.justifyRight 4 '0' (T.pack (showHex (layoutChecksum body) "")) <> "," <> body

halves :: Orientation -> Maybe Layout
halves o = splitPane (SplitSpec o Nothing After SpanPane) (PaneId 0) (PaneId 1)
    (fitLayout area (singlePane (PaneId 0)))

-- | One pane with two floating panes beside it, %2 overhanging the top left.
floated :: Maybe Layout
floated = floatPane (Just (PaneId 0)) (PaneId 1) (FloatGeom 18 4 9 4)
    (fitLayout area (singlePane (PaneId 0)))
    >>= floatPane (Just (PaneId 1)) (PaneId 2) (FloatGeom 10 3 (-2) (-1))

spec :: Spec
spec = do
    describe "emitLayout" $ do
        it "emits a pane as WxH,x,y,id with a checksum prefix" $
            emitLayout (fitLayout area (singlePane (PaneId 3)))
                `shouldBe` prefixed "80x24,0,0,3"
        it "uses {} for a left-right node and [] for a top-bottom one" $ do
            fmap emitLayout (halves LeftRight)
                `shouldBe` Just (prefixed "80x24,0,0{40x24,0,0,0,39x24,41,0,1}")
            fmap emitLayout (halves TopBottom)
                `shouldBe` Just (prefixed "80x24,0,0[80x12,0,0,0,80x11,0,13,1]")

        it "lists floating cells in the tree and again, front first, in <>" $
            fmap (emitLayoutZ [PaneId 2, PaneId 1]) floated
                `shouldBe` Just (prefixed
                    "80x24,0,0[80x24,0,0,0,18x4,9,4,1,10x3,-2,-1,2]\
                    \<10x3,-2,-1,2,18x4,9,4,1>")

    describe "restoreLayout" $ do
        it "reads back floating panes and their stacking onto new panes" $ case floated of
            Nothing -> expectationFailure "no float"
            Just lay -> do
                let str = emitLayoutZ [PaneId 2, PaneId 1] lay
                    pids = [PaneId 10, PaneId 11, PaneId 12]
                    back = restoreLayout str pids
                fmap (tiledPanes . fst) back `shouldBe` Right [PaneId 10]
                fmap snd back `shouldBe` Right [PaneId 12, PaneId 11]
                fmap (floatGeom (PaneId 12) . fst) back
                    `shouldBe` Right (Just (FloatGeom 10 3 (-2) (-1)))
        it "reads a string without floating panes as select-layout does" $
            fmap fst (restoreLayout (prefixed "80x24,0,0{40x24,0,0,0,39x24,41,0,1}")
                [PaneId 0, PaneId 1])
                `shouldBe` layoutFromString (prefixed "80x24,0,0{40x24,0,0,0,39x24,41,0,1}")
                    [PaneId 0, PaneId 1]

    describe "layoutFromString" $ do
        it "rejects a floating pane list, as tmux's parser does" $
            case fmap emitLayout floated of
                Nothing -> expectationFailure "no float"
                Just str -> layoutFromString str (map PaneId [0, 1, 2])
                    `shouldBe` Left "invalid layout"
        it "maps the cells onto the panes in order" $
            fmap layoutPanes (layoutFromString
                (prefixed "80x24,0,0{40x24,0,0,0,39x24,41,0[39x12,41,0,1,39x11,41,13,2]}")
                [PaneId 10, PaneId 11, PaneId 12])
                `shouldBe` Right [PaneId 10, PaneId 11, PaneId 12]
        it "round-trips its own output" $ case halves TopBottom of
            Nothing -> expectationFailure "no room to split"
            Just lay -> fmap emitLayout (layoutFromString (emitLayout lay) [PaneId 0, PaneId 1])
                `shouldBe` Right (emitLayout lay)
        it "rejects a checksum that does not match the body" $
            layoutFromString "0000,80x24,0,0,1" [PaneId 0]
                `shouldBe` Left "invalid layout"
        it "rejects more panes than cells" $
            layoutFromString (prefixed "80x24,0,0,1") [PaneId 0, PaneId 1]
                `shouldBe` Left "have 2 panes but need 1"
        it "closes surplus cells bottom-right first" $
            fmap emitLayout (layoutFromString
                (prefixed "80x24,0,0{40x24,0,0,0,39x24,41,0,1}") [PaneId 7])
                `shouldBe` Right (prefixed "80x24,0,0,7")
        it "rejects children that do not fill their node" $
            layoutFromString (prefixed "80x24,0,0{40x24,0,0,0,39x20,41,0,1}")
                [PaneId 0, PaneId 1]
                `shouldBe` Left "size mismatch after applying layout"
