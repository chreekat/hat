module Hat.Server.StyleSpec (spec) where

import Test.Hspec

import Hat.Term.Cell qualified as Cell
import Hat.Server.Style

spec :: Spec
spec = do
    describe "parseColor" $ do
        it "parses colourN into an indexed colour" $
            parseColor "colour240" `shouldBe` Cell.Indexed 240
        it "parses default and named colours" $ do
            parseColor "default" `shouldBe` Cell.DefaultColor
            parseColor "brightwhite" `shouldBe` Cell.Indexed 15
            parseColor "black" `shouldBe` Cell.Indexed 0
        it "parses #rrggbb hex" $
            parseColor "#ff8800" `shouldBe` Cell.RGB 255 136 0

    describe "parseStyle" $ do
        it "parses fg + attribute lists" $
            parseStyle "fg=brightwhite,bold"
                `shouldBe` Cell.defaultStyle
                    { Cell.fg = Cell.Indexed 15, Cell.bold = True }
        it "parses fg and bg together" $
            parseStyle "fg=default,bg=colour59"
                `shouldBe` Cell.defaultStyle { Cell.bg = Cell.Indexed 59 }
        it "ignores unknown tokens" $
            parseStyle "nonsense" `shouldBe` Cell.defaultStyle
