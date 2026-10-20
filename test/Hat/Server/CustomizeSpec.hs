-- | customize-mode's preview text: tmux's @screen_write_text@ wrapping and
-- the label-and-value lines over it.
module Hat.Server.CustomizeSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import Hat.Server.Customize
import Hat.Server.Theme (terminalTheme)
import Hat.Term.Cell qualified as Cell

rows :: Pen -> [Text]
rows = map (T.stripEnd . T.pack . map Cell.baseChar) . penGrid

drawn :: Either Pen Pen -> Pen
drawn = either id id

spec :: Spec
spec = describe "the preview pen" $ do
    it "wraps text at the last space that fits" $
        rows (drawn (penText False Cell.defaultStyle "one two three" (blankPen 9 3)))
            `shouldBe` ["one two", "three", ""]
    it "breaks a word longer than the line" $
        rows (drawn (penText False Cell.defaultStyle "abcdefghij" (blankPen 4 4)))
            `shouldBe` ["abcd", "efgh", "ij", ""]
    it "stops at the bottom row, keeping what fit" $ do
        let r = penText False Cell.defaultStyle "aa bb cc dd" (blankPen 2 2)
        either (const True) (const False) r `shouldBe` True
        rows (drawn r) `shouldBe` ["aa", "bb"]
    it "writes a value after its label on the same line" $
        rows (drawn (penValue terminalTheme "Option value: " "on" (blankPen 20 3)))
            `shouldBe` ["Option value: on", "", ""]
    it "skips a line, stopping at the bottom row" $ do
        let p = drawn (penText False Cell.defaultStyle "x" (blankPen 5 3))
        either (const True) (const False) (penSkipLine p) `shouldBe` True
