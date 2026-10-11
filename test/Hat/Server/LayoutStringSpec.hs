module Hat.Server.LayoutStringSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)
import Test.Hspec

import Hat.Geometry (Size (..))
import Hat.Model.Ids (PaneId (..))
import Hat.Server.Layout
    ( Layout (..), Orientation (..), Placement (..), SplitSpan (..), SplitSpec (..)
    , fitLayout, layoutPanes, singlePane, splitPane )
import Hat.Server.LayoutString

area :: Size
area = Size { rows = 24, cols = 80 }

-- | A body with its checksum prefix, as tmux prints it.
prefixed :: Text -> Text
prefixed body = T.justifyRight 4 '0' (T.pack (showHex (layoutChecksum body) "")) <> "," <> body

halves :: Orientation -> Maybe Layout
halves o = splitPane (SplitSpec o Nothing After SpanPane) (PaneId 0) (PaneId 1)
    (fitLayout area (singlePane (PaneId 0)))

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

    describe "layoutFromString" $ do
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
