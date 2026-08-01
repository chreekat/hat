{-# LANGUAGE OverloadedStrings #-}

module Hat.Server.CaptureSpec (spec) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Hspec

import Hat.Server (CaptureOpts (..), captureBounds, captureText)
import Hat.Term.Cell (Cell (..), defaultStyle)

-- | A width-1 cell with default style.
c :: Char -> Cell
c ch = Cell { text = T.singleton ch, width = 1, style = defaultStyle }

-- | A row from an ASCII string, padded with blanks to the given width.
row :: Int -> String -> V.Vector Cell
row sx s = V.fromList (map c s <> replicate (sx - length s) blank)
  where blank = c ' '

wide :: V.Vector Cell
wide = V.fromList
    [ Cell { text = "\12354", width = 2, style = defaultStyle }  -- あ
    , Cell { text = "", width = 0, style = defaultStyle }        -- continuation
    , c 'B' ]

trimOpts :: CaptureOpts
trimOpts = CaptureOpts { captTrim = True, captSeq = False, captNumber = False }

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

    describe "captureText (grid dump)" $ do
        it "trims trailing blanks by default" $
            captureText trimOpts [(0, row 5 "X B")] `shouldBe` "X B"

        it "collapses a wide char's continuation cell" $
            captureText trimOpts [(0, wide)] `shouldBe` "\12354B"

        it "keeps trailing blanks with trim off (-N)" $
            captureText trimOpts { captTrim = False } [(0, row 5 "X B")]
                `shouldBe` "X B  "

        it "preserves interior blank lines and leading spaces" $
            captureText trimOpts
                [(0, row 5 "one"), (1, row 5 ""), (2, row 5 " Xo")]
                `shouldBe` "one\n\n Xo"

        it "prefixes screen-relative line numbers with -L" $
            captureText trimOpts { captNumber = True }
                [(-1, row 3 "hh"), (0, row 3 "hi")]
                `shouldBe` "-1 hh\n0 hi"
