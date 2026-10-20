module Hat.Client.DrawSpec (spec) where

import Data.ByteString qualified as B
import Data.Text (Text)
import Test.Hspec

import Hat.Client.Draw
import Hat.Geometry
import Hat.Term.Caps (createTerm)
import Hat.Term.Cell
import Hat.Transport.Wire (DrawOp (..))

-- | The dialect of a terminal with these terminfo capabilities besides the
-- ones every terminal needs.
dialect :: [Text] -> Dialect
dialect extra = either (error . show) dialectOf $
    createTerm "vt100" (["clear=\ESC[H\ESC[2J", "cup=x", "am=1"] <> extra) [] [] Nothing

xterm256 :: Dialect
xterm256 = dialect
    [ "colors=256", "bold=\ESC[1m", "smul=\ESC[4m", "rev=\ESC[7m", "sitm=\ESC[3m"
    , "smxx=\ESC[9m", "dim=\ESC[2m" ]

spec :: Spec
spec = do
    -- Bug 3: a suggestion / ghost-text run coloured with a low palette
    -- index (e.g. Claude Code's grey `\ESC[38;5;8m`) must round-trip as
    -- the same palette slot. Emitting the aixterm `90..97` form instead
    -- retargets a different, separately-configurable colour, so the
    -- suggestion renders at the wrong intensity/hue.
    it "emits low palette indices as 38;5;n, not aixterm 9x" $ do
        sgr xterm256 defaultStyle { fg = Indexed 8 } `shouldBe` "\ESC[0;38;5;8m"
        sgr xterm256 defaultStyle { bg = Indexed 8 } `shouldBe` "\ESC[0;48;5;8m"
        sgr legacyDialect defaultStyle { fg = Indexed 15 } `shouldBe` "\ESC[0;38;5;15m"

    -- The first eight indices are the plain ANSI SGR 30-37/40-47 and
    -- stay in that form.
    it "keeps the first eight indices as ANSI 30-37/40-47" $ do
        sgr xterm256 defaultStyle { fg = Indexed 1 } `shouldBe` "\ESC[0;31m"
        sgr xterm256 defaultStyle { bg = Indexed 4 } `shouldBe` "\ESC[0;44m"

    -- Bug 33/48: a faint (dim) run — Claude Code's ghost suggestions — must
    -- reach the client's tty as SGR 2 so it renders dim, not normal text.
    it "emits the faint attribute as SGR 2" $
        sgr xterm256 defaultStyle { faint = True } `shouldBe` "\ESC[0;2m"

    -- Bug d6fd9d65: a full redraw (ClearAll then repaint) let the
    -- terminal present the cleared screen before the repaint arrived,
    -- flickering the whole window on pane open/close.
    it "brackets a batch in DEC 2026 synchronized output on a Sync terminal" $ do
        let bytes = opsToAnsi (dialect ["Sync=\ESC[?2026%?%p1%{1}%-%tl%eh%;"]) (Size 24 80)
                [ ClearAll
                , Put Pos { row = 0, col = 0 } defaultStyle "hi"
                , CursorAt Pos { row = 0, col = 2 } True
                ]
        bytes `shouldSatisfy` B.isPrefixOf "\ESC[?2026h"
        bytes `shouldSatisfy` B.isSuffixOf "\ESC[?2026l"

    it "does not synchronize output without Sync" $
        opsToAnsi xterm256 (Size 24 80) [ClearAll]
            `shouldSatisfy` (not . B.isInfixOf "2026")

    it "leaves out an attribute the terminal lacks" $
        sgr (dialect []) defaultStyle { bold = True, strike = True } `shouldBe` "\ESC[0m"

    it "writes an attribute's own capability when it is not a plain SGR" $ do
        sgr (dialect ["smso=\ESC[3m"]) defaultStyle { italic = True } `shouldBe` "\ESC[0;3m"
        sgr (dialect ["bold=\ESC[1;2m"]) defaultStyle { bold = True } `shouldBe` "\ESC[0m\ESC[1;2m"

    it "writes 24-bit colours as they are on a terminal that takes them" $
        sgr legacyDialect defaultStyle { fg = RGB 1 22 255, bg = RGB 0 100 9 }
            `shouldBe` "\ESC[0;38;2;1;22;255;48;2;0;100;9m"

    describe "colours a terminal cannot show" $ do
        it "takes 24-bit colours to the nearest of the 256" $
            sgr xterm256 defaultStyle { fg = RGB 255 0 0, bg = RGB 0x80 0x80 0x80 }
                `shouldBe` "\ESC[0;38;5;196;48;5;244m"
        it "takes the 256 to the 16 as bright aixterm colours" $
            sgr (dialect ["colors=16"]) defaultStyle { fg = Indexed 196, bg = Indexed 9 }
                `shouldBe` "\ESC[0;91;101m"
        it "makes a bright foreground bold on an 8-colour terminal" $
            sgr (dialect ["colors=8", "bold=\ESC[1m"]) defaultStyle { fg = Indexed 9, bg = Indexed 12 }
                `shouldBe` "\ESC[0;1;31;44m"
        it "avoids black on black" $
            sgr (dialect ["colors=8"]) defaultStyle { fg = Indexed 238, bg = Indexed 0 }
                `shouldBe` "\ESC[0;37;40m"

    it "never writes the bottom-right cell of a terminal without am" $
        opsToAnsi (either (error . show) dialectOf $
                createTerm "vt100" ["clear=\ESC[H\ESC[2J", "cup=x"] [] [] Nothing)
            (Size 2 10) [Put Pos { row = 1, col = 6 } defaultStyle "WXYZ"]
            `shouldSatisfy` \bs -> B.isInfixOf "WXY" bs && not (B.isInfixOf "WXYZ" bs)

    it "sets a title only with tsl and fsl" $ do
        let untitled = either (error . show) dialectOf $
                createTerm "vt100" ["clear=\ESC[H\ESC[2J", "cup=x"] ["*:tsl@"] [] Nothing
        titleBytes untitled "t" `shouldBe` ""
        titleBytes (dialect []) "t" `shouldBe` "\ESC]0;t\a"
