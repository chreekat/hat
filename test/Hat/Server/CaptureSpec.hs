{-# LANGUAGE OverloadedStrings #-}

module Hat.Server.CaptureSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Hspec

import Hat.Log (newLogger)
import Hat.Model (newServerState)
import Hat.Server
    (CaptureOpts (..), CaptureRow (..), Reply (..), captureBounds, captureText
    , cmdCapturePane)
import Hat.Server.Command.CaptureRaw (flagLetters, rawDump)
import Hat.Term.RawGrid qualified as Raw
import Hat.Term.RawGrid (RawGrid (..), RawLine (..), RawWidth (..), RawColor (..), Underline (..), blankRawCell, noAttrs)
import Hat.Term.Cell (Cell (..), Content (..), Color (..), Style (..), Width (..), defaultStyle, glyphCell)

-- | A width-1 cell with default style.
c :: Char -> Cell
c ch = glyphCell ch defaultStyle

-- | A width-1 cell with the given style.
cs :: Style -> Char -> Cell
cs st ch = glyphCell ch st

-- | A row from an ASCII string, padded with blanks to the given width.
row :: Int -> String -> V.Vector Cell
row sx s = V.fromList (map c s <> replicate (sx - length s) (c ' '))

-- | An unwrapped screen row (number 0) from an ASCII string.
r0 :: Int -> String -> CaptureRow
r0 sx s = CaptureRow { number = 0, wrapped = False, flags = "-", cells = row sx s }

-- | A cell row with explicit number and wrap flag.
crow :: Int -> Bool -> [Cell] -> CaptureRow
crow n w cells = CaptureRow { number = n, wrapped = w, flags = "-", cells = V.fromList cells }

wide :: V.Vector Cell
wide = V.fromList
    [ Cell { content = Glyph '\12354' [] Wide, style = defaultStyle }  -- あ
    , Cell { content = Continuation, style = defaultStyle }      -- continuation
    , c 'B' ]

opts0 :: CaptureOpts
opts0 = CaptureOpts
    { captJoin = False, captNoTrim = False, captUsed = False
    , captSeq = False, captOctal = False, captNumber = False
    , captFlags = False }

red, green, boldSt, boldRed, boldItalic :: Style
red = defaultStyle { fg = Indexed 1 }
green = defaultStyle { fg = Indexed 2 }
boldSt = defaultStyle { bold = True }
boldRed = defaultStyle { bold = True, fg = Indexed 1 }
boldItalic = defaultStyle { bold = True, italic = True }

spec :: Spec
spec = do
    describe "captureBounds (tmux -S/-E resolution)" $ do
        it "defaults to the visible screen" $
            captureBounds 0 3 Nothing Nothing `shouldBe` (0, 2)

        it "-S 0 -E - is the visible screen even with history" $ do
            captureBounds 0 3 (Just "0") (Just "-") `shouldBe` (0, 2)
            captureBounds 2 3 (Just "0") (Just "-") `shouldBe` (2, 4)

        it "-S - -E - is the whole history plus screen" $
            captureBounds 2 3 (Just "-") (Just "-") `shouldBe` (0, 4)

        it "a negative -S reaches into history" $
            captureBounds 5 3 (Just "-1") Nothing `shouldBe` (4, 7)

        it "clamps a negative -S that underflows history to the top" $
            captureBounds 5 3 (Just "-100") (Just "-") `shouldBe` (0, 7)

        it "swaps bounds when the end precedes the start" $
            captureBounds 0 3 (Just "2") (Just "0") `shouldBe` (0, 2)

    describe "captureText (tmux grid dump)" $ do
        it "trims trailing blanks and ends each line with a newline" $
            captureText opts0 [r0 5 "X B"] `shouldBe` "X B\n"

        it "collapses a wide char's continuation cell" $
            captureText opts0 [crow 0 False (V.toList wide)] `shouldBe` "\12354B\n"

        it "keeps trailing blanks with -N" $
            captureText opts0 { captNoTrim = True } [r0 5 "X B"]
                `shouldBe` "X B  \n"

        it "preserves interior blank lines and leading spaces" $
            captureText opts0 [r0 5 "one", r0 5 "", r0 5 " Xo"]
                `shouldBe` "one\n\n Xo\n"

        it "prefixes screen-relative line numbers with -L" $
            captureText opts0 { captNumber = True }
                [ CaptureRow (-1) False "-" (row 3 "hh"), r0 3 "hi" ]
                `shouldBe` "-1 hh\n0 hi\n"

        it "prefixes each line's flag letters with -F" $
            captureText opts0 { captFlags = True }
                [ (crow 0 True (map c "ab")) { flags = "WX" }, r0 3 "c" ]
                `shouldBe` "WX ab\n- c\n"

        it "-T stops at the last used cell (visible with -N)" $
            captureText opts0 { captNoTrim = True, captUsed = True }
                [r0 5 "X B"] `shouldBe` "X B\n"

    describe "captureText -J (join wrapped lines, bug d2)" $ do
        it "joins a wrapped row onto its successor" $
            captureText opts0 { captJoin = True }
                [ crow 0 True (map c "abc"), crow 1 False (map c "de" <> [c ' ']) ]
                `shouldBe` "abcde\n"

        it "takes used cells only and skips trimming" $
            captureText opts0 { captJoin = True }
                [ crow 0 False [c 'x', cs red ' ', c ' ', c ' '] ]
                `shouldBe` "x \n"

        it "leaves no newline after a final wrapped row" $
            captureText opts0 { captJoin = True }
                [ crow 0 True (map c "ab") ] `shouldBe` "ab"

        it "numbers every physical row even when joining" $
            captureText opts0 { captJoin = True, captNumber = True }
                [ crow 0 True (map c "ab"), crow 1 False (map c "cd") ]
                `shouldBe` "0 ab1 cd\n"

    describe "captureText -e (SGR reconstruction, bug d2)" $ do
        it "emits SGR only when the style changes" $
            captureText opts0 { captSeq = True }
                [ crow 0 False [c 'a', cs boldSt 'b', c 'c'] ]
                `shouldBe` "a\ESC[1mb\ESC[0mc\n"

        it "resets once when attributes drop to default" $
            captureText opts0 { captSeq = True }
                [ crow 0 False [cs boldRed 'a', c 'b'] ]
                `shouldBe` "\ESC[1m\ESC[31ma\ESC[0mb\n"

        it "switches colors without a reset" $
            captureText opts0 { captSeq = True }
                [ crow 0 False [cs red 'a', cs green 'b'] ]
                `shouldBe` "\ESC[31ma\ESC[32mb\n"

        it "returns to the default foreground with 39" $
            captureText opts0 { captSeq = True }
                [ crow 0 False [cs red 'a', c 'b'] ]
                `shouldBe` "\ESC[31ma\ESC[39mb\n"

        it "sets attributes in one CSI, colors in their own" $
            captureText opts0 { captSeq = True }
                [ crow 0 False [cs boldItalic { fg = Indexed 1 } 'a'] ]
                `shouldBe` "\ESC[1;3m\ESC[31ma\n"

        it "re-sets surviving attributes after a reset" $
            captureText opts0 { captSeq = True }
                [ crow 0 False [cs boldItalic 'a', cs boldSt 'b'] ]
                `shouldBe` "\ESC[1;3ma\ESC[0;1mb\n"

        it "carries the pen across lines" $
            captureText opts0 { captSeq = True }
                [ crow 0 False [cs red 'a'], crow 1 False [cs red 'b'] ]
                `shouldBe` "\ESC[31ma\nb\n"

        it "keeps codes emitted before trimmed blanks" $
            captureText opts0 { captSeq = True }
                [ crow 0 False [cs red 'a', c ' ', c ' '] ]
                `shouldBe` "\ESC[31ma\ESC[39m\n"

        it "emits nothing for a never-written row after a coloured one" $
            captureText opts0 { captSeq = True }
                [ crow 0 False (replicate 4 (cs red 'a'))
                , crow 1 False (replicate 4 (c ' ')) ]
                `shouldBe` "\ESC[31maaaa\n\n"

        it "encodes bright, 256, and RGB colors like tmux" $ do
            captureText opts0 { captSeq = True }
                [ crow 0 False [cs defaultStyle { fg = Indexed 9 } 'a'] ]
                `shouldBe` "\ESC[91ma\n"
            captureText opts0 { captSeq = True }
                [ crow 0 False [cs defaultStyle { fg = Indexed 250 } 'a'] ]
                `shouldBe` "\ESC[38;5;250ma\n"
            captureText opts0 { captSeq = True }
                [ crow 0 False [cs defaultStyle { fg = RGB 1 2 3 } 'a'] ]
                `shouldBe` "\ESC[38;2;1;2;3ma\n"
            captureText opts0 { captSeq = True }
                [ crow 0 False [cs defaultStyle { bg = Indexed 3 } 'a'] ]
                `shouldBe` "\ESC[43ma\n"
            captureText opts0 { captSeq = True }
                [ crow 0 False [cs defaultStyle { bg = Indexed 12 } 'a'] ]
                `shouldBe` "\ESC[104ma\n"

    describe "captureText -C (octal escapes, bug d2)" $ do
        it "escapes backslash and control cells as octal" $
            captureText opts0 { captOctal = True }
                [ crow 0 False
                    [ c 'a'
                    , glyphCell '\\' defaultStyle
                    , glyphCell '\a' defaultStyle ] ]
                `shouldBe` "a\\\\\\007\n"

        it "spells the SGR introducer as literal octal too" $
            captureText opts0 { captOctal = True, captSeq = True }
                [ crow 0 False [c 'a', cs boldSt 'b'] ]
                `shouldBe` "a\\033[1mb\n"

    describe "cmdCapturePane flag vetting (bug d2)" $
        it "rejects flags it cannot implement loudly" $ do
            lg <- newLogger "/dev/null"
            st <- newServerState Map.empty lg "/tmp/hat-capturespec.sock" Nothing
            let rejects f = do
                    rs <- cmdCapturePane st Nothing ["-p", f]
                    rs `shouldBe`
                        [RErr ("capture-pane: " <> f <> " is not implemented")]
            rejects "-P"

    describe "rawDump (tmux capture-pane -R)" $ do
        let rc ch = blankRawCell { Raw.text = [ch] }
            line w cs' = RawLine { wrapped = w, prompt = False, cells = cs' }
            grid ls = RawGrid
                { cols = 2, rows = length ls, hsize = 0, hlimit = 0, lines = ls }
            cellTail = " fg=default[8] bg=default[8] us=default[8] link=NONE linkid=NONE"

        it "prints the header, line records and cell records byte for byte" $
            rawDump (grid [line True [rc 'A', blankRawCell]]) `shouldBe` T.concat
                [ "G 2x1 (0/0)\n"
                , "\tL 0 (0) flags=WRAPPED[1] 1/1\n"
                , "\t\tC 0,0 data=(1,1,A) flags=NONE[0] attr=NONE[0]", cellTail, "\n"
                , "\t\tC 0,1 data=(1,1, ) flags=NONE[0] attr=NONE[0]", cellTail, "\n" ]

        it "numbers history lines '-' and screen lines from 0" $
            T.lines (rawDump (grid [line False [], line False []])
                        { hsize = 1, hlimit = 5 })
                `shouldBe`
                [ "G 2x2 (1/5)", "\tL 0 (-) flags=NONE[0] 0/0"
                , "\tL 1 (0) flags=NONE[0] 0/0" ]

        it "spells a wide glyph, its padding, and the line's EXTENDED flag" $
            T.lines (rawDump (grid [line False
                    [ (rc '\12354') { Raw.width = RawWide }
                    , blankRawCell { Raw.width = SpacerTail } ]]))
                `shouldContain`
                [ "\tL 0 (0) flags=EXTENDED[2] 2/2"
                , "\t\tC 0,0 data=(2,3,\12354) flags=NONE[0] attr=NONE[0]" <> cellTail
                , "\t\tC 0,1 data=(1,1,!) flags=PADDING[4] attr=NONE[0]" <> cellTail ]

        it "encodes colors and attributes as tmux's grid does" $ do
            let cell = (rc 'X')
                    { Raw.fg = RawPalette 196, Raw.bg = RawPalette 4, Raw.us = RawPalette 45
                    , Raw.attrs = noAttrs
                        { Raw.bold = True, Raw.underline = Curly, Raw.overline = True } }
            T.lines (rawDump (grid [line False [cell]])) `shouldContain`
                [ "\t\tC 0,0 data=(1,1,X) flags=FG256[1]"
                  <> " attr=BRIGHT,UNDERSCORE_3,OVERLINE[2401]"
                  <> " fg=colour196[10000c4] bg=blue[4] us=colour45[100002d]"
                  <> " link=NONE linkid=NONE" ]

        it "marks a blank left by a colored erase CLEARED" $
            T.lines (rawDump (grid [line False
                    [blankRawCell { Raw.bgOnly = True, Raw.bg = RawRgb 1 2 3 }]]))
                `shouldContain`
                [ "\t\tC 0,0 data=(1,1, ) flags=CLEARED[40] attr=NONE[0]"
                  <> " fg=default[8] bg=#010203[2010203] us=default[8]"
                  <> " link=NONE linkid=NONE" ]

        it "reports a hyperlink on the cell and the line" $ do
            let out = rawDump (grid [line False [(rc 'L') { Raw.link = Just "https://x" }]])
            T.lines out `shouldContain` ["\tL 0 (0) flags=EXTENDED,HYPERLINK[102] 1/1"]
            T.isInfixOf "link=https://x linkid=NONE" out `shouldBe` True

    describe "flagLetters (capture-pane -F)" $ do
        let line w p cs' = RawLine { wrapped = w, prompt = p, cells = cs' }
        it "is - for a plain line" $
            flagLetters (line False False [blankRawCell]) `shouldBe` "-"
        it "orders the letters as tmux does" $
            flagLetters (line True True
                [blankRawCell { Raw.text = "\12354", Raw.width = RawWide, Raw.link = Just "u" }])
                `shouldBe` "HPWX"
