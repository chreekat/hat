module Hat.Server.MessageSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (utc, utcToZonedTime)
import Data.Vector qualified as V
import Test.Hspec

import Hat.Model (Markup (..))
import Hat.Model.Options
import Hat.Server.Format (FormatCtx (..), formatCtx)
import Hat.Server.Message
import Hat.Term.Cell qualified as Cell
import Hat.TestSupport (clientTheme)

set :: Text -> Text -> Options -> Options
set n v = applyEntry n (OVScalar v)

ctxFor :: Options -> FormatCtx
ctxFor opts = formatCtx (optionFormats opts) (const "")
    (utcToZonedTime utc (posixSecondsToUTCTime 0))

-- | A row of plain text in the default style.
under :: Text -> V.Vector Cell.Cell
under t = V.fromList [ Cell.glyphCell c Cell.defaultStyle | c <- T.unpack t ]

message :: Options -> (Markup, Text) -> Text -> V.Vector Cell.Cell
message opts (mk, m) row = drawMessage (ctxFor opts) opts mk m (under row)

prompt :: Options -> PromptView -> Text -> (V.Vector Cell.Cell, Int)
prompt opts pv row = drawPrompt (ctxFor opts) opts pv (under row)

rowText :: V.Vector Cell.Cell -> Text
rowText = T.pack . map Cell.baseChar . V.toList

yellow, black, red :: Cell.Color
yellow = Cell.Indexed 3
black = Cell.Indexed 0
red = Cell.Indexed 1

spec :: Spec
spec = do
    describe "drawMessage" $ do
        it "fills the line in hat's black on ANSI yellow under a client's theme" $ do
            let ctx = (ctxFor defaultOptions) { theme = clientTheme defaultOptions }
                row = drawMessage ctx defaultOptions MarkupLiteral "hi" (under "abcdefgh")
            rowText row `shouldBe` "hi      "
            (row V.! 0).style `shouldBe`
                Cell.defaultStyle { Cell.fg = black, Cell.bg = yellow }
            (row V.! 7).style.bg `shouldBe` yellow
        it "draws a styled message's #[...] directives" $ do
            let row = message defaultOptions ((,) MarkupStyled "#[fg=red]hi") "    "
            rowText row `shouldBe` "hi  "
            (row V.! 0).style.fg `shouldBe` red
        it "shows a literal message's #[...] as text" $
            rowText (message defaultOptions ((,) MarkupLiteral "#[x]#") "        ")
                `shouldBe` "#[x]#   "
        it "expands message-format around #{message}" $
            rowText (message (set "message-format" "<#{message}>" defaultOptions)
                ((,) MarkupLiteral "hi") "      ")
                `shouldBe` "<hi>  "
        it "takes message-style's width and align, leaving the rest" $ do
            let opts = set "message-style" "bg=blue,width=4,align=right"
                    defaultOptions
            rowText (message opts ((,) MarkupLiteral "hi") "abcdefghij")
                `shouldBe` "abcdefghhi"

    describe "drawPrompt" $ do
        let pv = PromptView { label = ":", input = "abc", cursor = 1
                            , commandMode = False }
        it "draws the label then the input, placing the cursor" $ do
            let (row, cx) = prompt defaultOptions pv "          "
            rowText row `shouldBe` ":abc      "
            cx `shouldBe` 2
            (row V.! 9).style.bg `shouldBe` yellow
        it "scrolls a long input to keep the cursor visible" $ do
            let (row, cx) = prompt defaultOptions
                    pv { input = "abcdefgh", cursor = 8 } "     "
            rowText row `shouldBe` ":fgh "
            cx `shouldBe` 4
        it "uses message-command-style in vi command mode" $ do
            let (row, _) = prompt defaultOptions pv { commandMode = True } "     "
            (row V.! 1).style `shouldBe`
                Cell.defaultStyle { Cell.fg = yellow, Cell.bg = black }
        it "expands the label as a format" $ do
            let opts = set "@who" "me" defaultOptions
                (row, _) = prompt opts pv { label = "#{@who}> " } "         "
            T.take 7 (rowText row) `shouldBe` "me> abc"
