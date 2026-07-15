module Hat.Client.DrawSpec (spec) where

import qualified Data.ByteString as B
import Test.Hspec

import Hat.Client.Draw
import Hat.Geometry
import Hat.Term.Cell
import Hat.Transport.Wire (DrawOp (..))

spec :: Spec
spec = do
    -- Bug d6fd9d65: a full redraw (ClearAll then repaint) let the
    -- terminal present the cleared screen before the repaint arrived,
    -- flickering the whole window on pane open/close.
    it "brackets a batch in DEC 2026 synchronized output" $ do
        let bytes = opsToAnsi
                [ ClearAll
                , Put Pos { row = 0, col = 0 } defaultStyle "hi"
                , CursorAt Pos { row = 0, col = 2 } True
                ]
        bytes `shouldSatisfy` B.isPrefixOf "\ESC[?2026h"
        bytes `shouldSatisfy` B.isSuffixOf "\ESC[?2026l"
