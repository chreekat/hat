module Hat.Server.StatusSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (utc, utcToZonedTime)
import Data.Vector qualified as V
import Test.Hspec

import Hat.Model.Options
import Hat.Server.Format (FormatCtx (..), formatCtx)
import Hat.Server.Status
import Hat.Term.Cell qualified as Cell
import Hat.TestSupport (clientTheme)

-- | Options with a fixed status-right, so rows carry no clock.
opts0 :: Options
opts0 = set "status-right" "R" defaultOptions

set :: Text -> Text -> Options -> Options
set n v = applyEntry n (OVScalar v)

-- | A session "s" with windows 0:sh (current) and 1:vim (last).
ctxFor :: Options -> FormatCtx
ctxFor opts = (formatCtx vars (const "") epoch)
    { windows = [win 0 "sh" "*" True False, win 1 "vim" "-" False True] }
  where
    epoch = utcToZonedTime utc (posixSecondsToUTCTime 0)
    vars = Map.union (Map.fromList
        [ ("session_name", "s"), ("session_id", "$0"), ("window_index", "0")
        , ("window_name", "sh") ]) (optionFormats opts)
    win :: Int -> Text -> Text -> Bool -> Bool -> Map.Map Text Text
    win ix name flags active lastF = Map.fromList
        [ ("window_index", T.pack (show ix)), ("window_name", name)
        , ("window_flags", flags), ("window_active", flag active)
        , ("window_last_flag", flag lastF), ("window_bell_flag", "0")
        , ("window_activity_flag", "0"), ("window_silence_flag", "0") ]
    flag b = if b then "1" else "0"

rows :: Options -> Int -> [V.Vector Cell.Cell]
rows opts = statusRows (ctxFor opts) opts

-- | The single row of a one-line status.
row1 :: Options -> Int -> V.Vector Cell.Cell
row1 opts w = case rows opts w of
    [r] -> r
    rs -> error ("expected one status row, got " <> show (length rs))

rowText :: V.Vector Cell.Cell -> Text
rowText = T.pack . map Cell.baseChar . V.toList

green, black, red :: Cell.Color
green = Cell.Indexed 2
black = Cell.Indexed 0
red = Cell.Indexed 1

spec :: Spec
spec = do
    describe "the default status-format" $ do
        let row = row1 opts0 24
        it "lays out status-left, the window list and status-right" $
            rowText row `shouldBe` "[s] 0:sh* 1:vim-       R"
        it "paints the line in status-style" $
            (row V.! 20).style `shouldBe`
                Cell.defaultStyle { Cell.fg = black, Cell.bg = green }
        it "underscores the current window (window-status-current-style)" $ do
            (row V.! 4).style.underline `shouldBe` True
            (row V.! 9).style.underline `shouldBe` False
            (row V.! 10).style.underline `shouldBe` False
        it "draws hat's black on ANSI green under a client's theme" $ do
            let drawn = statusRows (ctxFor opts0) { theme = clientTheme opts0 } opts0 24
                plain = Cell.defaultStyle { Cell.fg = black, Cell.bg = green }
            map (.style) (concatMap V.toList drawn) `shouldSatisfy`
                all (`elem` [plain, plain { Cell.underline = True }])
        it "spells a flagless window without tmux's padding space" $ do
            let win ix name = Map.fromList
                    [ ("window_index", ix), ("window_name", name)
                    , ("window_flags", ""), ("window_active", "0") ]
                ctx = (ctxFor opts0) { windows = [win "0" "sh", win "1" "vim"] }
            map (T.take 14 . rowText) (statusRows ctx opts0 24)
                `shouldBe` ["[s] 0:sh 1:vim"]

    describe "status options read by the default format" $ do
        it "status-justify right moves the list against status-right" $ do
            let row = row1 (set "status-justify" "right" opts0) 24
            rowText row `shouldBe` "[s]        0:sh* 1:vim-R"
        it "window-status-separator joins the entries" $ do
            let row = row1 (set "window-status-separator" "|" opts0) 24
            T.take 15 (rowText row) `shouldBe` "[s] 0:sh*|1:vim"
        it "window-status-last-style styles the last window" $ do
            let row = row1 (set "window-status-last-style" "fg=red" opts0) 24
            (row V.! 10).style.fg `shouldBe` red
            (row V.! 4).style.fg `shouldBe` black
        it "status-left-style styles status-left" $ do
            let row = row1 (set "status-left-style" "bold" opts0) 24
            (row V.! 0).style.bold `shouldBe` True
            (row V.! 4).style.bold `shouldBe` False

    describe "status-bg and status-fg" $ do
        it "override status-style's colours" $ do
            let opts = set "status-bg" "red" (set "status-fg" "colour7" opts0)
                row = row1 opts 24
            (row V.! 20).style.bg `shouldBe` red
            (row V.! 20).style.fg `shouldBe` Cell.Indexed 7
        it "leave status-style alone when default" $ do
            let row = row1 (set "status-style" "bg=blue" opts0) 24
            (row V.! 20).style.bg `shouldBe` Cell.Indexed 4

    describe "status lines" $ do
        it "draw one status-format entry per line, blank where unset" $ do
            let opts = applyEntry "status-format"
                    (OVArray (Map.fromList [(KeyIndex 0, "A"), (KeyIndex 2, "C")]))
                    (set "status" "3" opts0)
            map rowText (rows opts 3) `shouldBe` ["A  ", "   ", "C  "]
        it "show the window's panes on the second line by default" $ do
            let opts = set "status" "2" opts0
                pane = Map.fromList
                    [ ("pane_id", "%0"), ("pane_index", "0"), ("pane_title", "t")
                    , ("pane_active", "1"), ("pane_flags", "*") ]
                ctx = (ctxFor opts) { panes = [pane] }
            case statusRows ctx opts 12 of
                [_, row] -> do
                    rowText row `shouldBe` " P: 0:[t]*  "
                    (row V.! 4).style.underline `shouldBe` True
                rs -> expectationFailure ("rows: " <> show (length rs))
        it "draw nothing when status is off" $
            rows (set "status" "off" opts0) 3 `shouldBe` []
