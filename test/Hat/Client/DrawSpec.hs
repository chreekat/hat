module Hat.Client.DrawSpec (spec) where

import qualified Data.ByteString as B
import Test.Hspec

import Hat.Client.Draw
import Hat.Geometry
import Hat.Term.Cell
import Hat.Transport.Wire (DrawOp (..))

spec :: Spec
spec = do
    -- Bug 3: a suggestion / ghost-text run coloured with a low palette
    -- index (e.g. Claude Code's grey `\ESC[38;5;8m`) must round-trip as
    -- the same palette slot. Emitting the aixterm `90..97` form instead
    -- retargets a different, separately-configurable colour, so the
    -- suggestion renders at the wrong intensity/hue.
    it "emits low palette indices as 38;5;n, not aixterm 9x" $ do
        sgr defaultStyle { fg = Indexed 8 } `shouldBe` "\ESC[0;38;5;8m"
        sgr defaultStyle { bg = Indexed 8 } `shouldBe` "\ESC[0;48;5;8m"
        sgr defaultStyle { fg = Indexed 15 } `shouldBe` "\ESC[0;38;5;15m"

    -- The first eight indices are the plain ANSI SGR 30-37/40-47 and
    -- stay in that form.
    it "keeps the first eight indices as ANSI 30-37/40-47" $ do
        sgr defaultStyle { fg = Indexed 1 } `shouldBe` "\ESC[0;31m"
        sgr defaultStyle { bg = Indexed 4 } `shouldBe` "\ESC[0;44m"
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
