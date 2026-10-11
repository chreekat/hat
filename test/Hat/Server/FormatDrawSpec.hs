module Hat.Server.FormatDrawSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Hspec

import Hat.Server.FormatDraw
import Hat.Server.Theme (terminalTheme)
import Hat.Term.Cell qualified as Cell

base :: Cell.Style
base = Cell.defaultStyle { Cell.fg = Cell.Indexed 0, Cell.bg = Cell.Indexed 2 }

-- | Draw onto a row of @w@ blanks in the base style.
draw :: Int -> Text -> V.Vector Cell.Cell
draw w fmt = formatDraw terminalTheme base fmt (V.replicate w (Cell.glyphCell ' ' base))

rowText :: V.Vector Cell.Cell -> Text
rowText = T.pack . map Cell.baseChar . V.toList

styleAt :: Int -> V.Vector Cell.Cell -> Cell.Style
styleAt i row = (row V.! i).style

red, blue :: Cell.Color
red = Cell.Indexed 1
blue = Cell.Indexed 4

spec :: Spec
spec = do
    describe "formatDraw" $ do
        it "draws plain text from the left, leaving the rest of the row" $ do
            let row = draw 6 "abc"
            rowText row `shouldBe` "abc   "
            styleAt 0 row `shouldBe` base
        it "places align=right and align=centre sections" $ do
            rowText (draw 5 "#[align=right]xy") `shouldBe` "   xy"
            rowText (draw 6 "#[align=centre]ab") `shouldBe` "  ab  "
            rowText (draw 7 "L#[align=centre]c#[align=right]R")
                `shouldBe` "L  c  R"
        it "accumulates styles and resets to the default" $ do
            let row = draw 3 "#[fg=red]a#[bold]b#[default]c"
            (styleAt 0 row).fg `shouldBe` red
            styleAt 1 row `shouldBe` base { Cell.fg = red, Cell.bold = True }
            styleAt 2 row `shouldBe` base
        it "push-default makes the current style the default until pop" $ do
            let row = draw 4
                    "#[fg=red]#[push-default]a#[fg=blue]b#[default]c\
                    \#[pop-default]#[default]d"
            (styleAt 0 row).fg `shouldBe` red
            (styleAt 1 row).fg `shouldBe` blue
            (styleAt 2 row).fg `shouldBe` red
            styleAt 3 row `shouldBe` base
        it "fill clears the whole area to the fill colour first" $ do
            let row = draw 3 "#[fill=blue]x"
            rowText row `shouldBe` "x  "
            styleAt 2 row `shouldBe` Cell.defaultStyle { Cell.bg = blue }
        it "skips an invalid style and keeps drawing" $
            rowText (draw 3 "a#[bogus=1]b") `shouldBe` "ab "
        it "unescapes leading hashes" $ do
            rowText (draw 4 "a##b") `shouldBe` "a#b "
            rowText (draw 4 "##[x") `shouldBe` "#[x "
            rowText (draw 4 "###[fg=red]x") `shouldBe` "#x  "
        it "keeps left and right, trimming the right's start first" $
            rowText (draw 6 "#[align=left]LLLL#[align=right]RRRR")
                `shouldBe` "LLLLRR"
        it "trims a list around its focus, with markers" $ do
            let fmt = "#[list=on align=left]#[list=left-marker]<#[list=right-marker]>\
                      \#[list=on]aaaa#[list=focus]BB#[list=on]cccc"
            rowText (draw 6 fmt) `shouldBe` "<aBBc>"
            rowText (draw 12 fmt) `shouldBe` "aaaaBBcccc  "
        it "draws after-list text beside a left list" $
            rowText (draw 8 "#[list=on align=left]abc#[nolist]Z") `shouldBe` "abcZ    "
        it "draws acs text as its line-drawing glyphs, until noacs" $
            rowText (draw 4 "#[acs]~q#[noacs]~q") `shouldBe` "\x00b7\x2500~q"
        it "draws wide characters across two cells" $ do
            let row = draw 3 "中a"
            V.length row `shouldBe` 3
            Cell.cellWidth (row V.! 0) `shouldBe` 2
            Cell.baseChar (row V.! 2) `shouldBe` 'a'

    describe "styleParse" $ do
        it "parses tmux's style keywords" $ do
            let parsed = styleParse base defaultDrawStyle
                    "fg=red,bg=default,align=right,fill=blue,list=focus"
            fmap (\sy -> (sy.cell.fg, sy.cell.bg, sy.align, sy.fill, sy.list))
                parsed
                `shouldBe` Just (red, base.bg, AlignRight, Just blue, ListFocus)
        it "resolves theme colours to their terminal colours" $
            ((.cell.bg) <$> styleParse base defaultDrawStyle "bg=themeyellow")
                `shouldBe` Just (Cell.Indexed 3)
        it "rejects an unknown token" $
            (() <$ styleParse base defaultDrawStyle "fg=red,frobnicate")
                `shouldBe` Nothing

    describe "style-aware width and trimming" $ do
        it "formatWidth skips styles and counts escaped hashes once" $ do
            formatWidth "#[fg=red]abc" `shouldBe` 3
            formatWidth "a##b" `shouldBe` 3
        it "formatTrimLeft keeps styles and cuts text" $
            formatTrimLeft 2 "#[fg=red]abc#[default]d"
                `shouldBe` "#[fg=red]ab"
        it "formatTrimRight keeps the end" $
            formatTrimRight 2 "#[fg=red]abc" `shouldBe` "#[fg=red]bc"
