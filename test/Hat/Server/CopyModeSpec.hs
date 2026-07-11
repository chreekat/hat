module Hat.Server.CopyModeSpec (spec) where

import Data.Functor.Identity (Identity, runIdentity)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Hspec

import Hat.Geometry (Pos (..))
import Hat.Model (CopyModeState (..), SelKind (SelChar))
import Hat.Model.Options (ModeKeys (..), Options (..), defaultOptions)
import Hat.Server.CopyMode
import Hat.Term.Cell (Cell (..), Style (..), defaultStyle)

-- | The pane grid the upstream copy-mode tests build (40x10). The
-- source file's leading TAB on row 1 reaches hat's grid as spaces
-- (libvterm expands tabs), so it is modelled as spaces here.
testRows :: [Text]
testRows =
    [ "A line of words"
    , "        Indented line"
    , "Another line..."
    , "... @nd then $ym_bols[]{}"
    , " ?? 500xyz"
    , "", "", "", "", ""
    ]

grid :: Grid Identity
grid = listGrid 40 testRows

viSeparators :: Text
viSeparators = defaultOptions.wordSeparators

-- | A tiny copy-mode interpreter: applies @-X@ command names to a
-- (state, buffers) pair, purely, mirroring the server dispatch.
data Sim = Sim { sState :: CopyModeState, sBufs :: [Text] }

start :: Sim
start = Sim
    { sState = CopyModeState
        { cursorRow = 0
        , cursorCol = 0
        , selection = Nothing
        , keyTable = "copy-mode"
        , viewportOffY = 0
        }
    , sBufs = []
    }

step :: ModeKeys -> Text -> Text -> Sim -> Sim
step keys seps cmd sim = case cmd of
    "history-top" -> sim { sState = st { cursorRow = 0, cursorCol = 0 } }
    "start-of-line" -> sim { sState = st { cursorCol = 0 } }
    "begin-selection" -> sim
        { sState = st
            { selection = Just ((st.cursorRow, st.cursorCol), SelChar) } }
    "copy-selection" ->
        let body = runIdentity (extractSelection grid keys st)
        in sim
            { sState = st { selection = Nothing }
            , sBufs = maybe id (:) body sim.sBufs
            }
    "next-word" -> motion (mNextWord seps)
    "next-word-end" -> motion (mNextWordEnd seps)
    "previous-word" -> motion (mPreviousWord seps)
    "next-space" -> motion (mNextWord "")
    "next-space-end" -> motion (mNextWordEnd "")
    "previous-space" -> motion (mPreviousWord "")
    "cursor-left" -> motion mCursorLeft
    "cursor-right" -> motion mCursorRight
    "cursor-up" -> sim { sState = runIdentity (cursorVertical (-1) grid st) }
    "cursor-down" -> sim { sState = runIdentity (cursorVertical 1 grid st) }
    "end-of-line" -> sim { sState = runIdentity (endOfLine grid st) }
    other -> error ("unknown copy-mode command: " <> show other)
  where
    st = sim.sState
    motion m = sim { sState = runIdentity (runMotion grid keys seps m st) }

-- | Run a command script and return the buffers yanked, oldest first.
-- Trailing newlines are dropped to mirror how the upstream tests read
-- buffers back (@[ "$(show-buffer)" = ... ]@ strips them).
run :: ModeKeys -> Text -> [Text] -> [Text]
run keys seps cmds =
    map (T.dropWhileEnd (== '\n'))
        (reverse (foldl (\s c -> step keys seps c s) start cmds).sBufs)

vi :: [Text] -> [Text]
vi = run KeysVi viSeparators

emacs :: [Text] -> [Text]
emacs = run KeysEmacs ""

spec :: Spec
spec = do
    describe "vi copy-mode motions (upstream copy-mode-test-vi)" $ do
        it "reproduces every yanked buffer in order" $
            vi
                [ "history-top", "start-of-line"
                -- previous-word/space clamp at start of text -> "A"
                , "begin-selection", "previous-word", "previous-space"
                , "previous-word", "copy-selection"
                -- next-word-end skips single-letter words -> "line"
                , "next-word-end", "begin-selection", "previous-word"
                , "copy-selection"
                -- next-word-end stops at end of line -> "words"
                , "next-word", "next-word", "begin-selection"
                , "next-word-end", "next-word-end", "copy-selection"
                , "next-word"
                -- next-word wraps un-indented breaks -> "line\nA"
                , "next-word", "begin-selection", "next-word"
                , "copy-selection"
                -- next-word-end does not treat periods as letters -> "line"
                , "next-word", "begin-selection", "next-word-end"
                , "copy-selection"
                -- next-space-end treats periods as letters -> "line..."
                , "previous-word", "begin-selection", "next-space-end"
                , "copy-selection"
                -- previous/next-space treat periods as letters -> "line...\n."
                , "previous-space", "begin-selection", "next-space"
                , "copy-selection"
                -- next-word/next-word-end skip symbols -> "... @nd then"
                , "begin-selection", "next-word", "next-word"
                , "next-word-end", "next-word-end", "copy-selection"
                -- next-space wraps indented symbols -> "$ym_bols[]{}\n ?"
                , "next-space", "begin-selection", "next-space"
                , "copy-selection"
                -- next-word-end treats digits as letters -> "? 500xyz"
                , "next-word-end", "begin-selection", "next-word-end"
                , "copy-selection"
                -- previous-word treats digits as letters -> "500xyz"
                , "begin-selection", "previous-word", "copy-selection"
                -- motions stop at end of text -> "500xyz"
                , "begin-selection", "next-word", "next-word-end"
                , "next-word", "next-space", "next-space-end"
                , "copy-selection"
                ]
            `shouldBe`
                [ "A", "line", "words", "line\nA", "line", "line..."
                , "line...\n.", "... @nd then", "$ym_bols[]{}\n ?"
                , "? 500xyz", "500xyz", "500xyz"
                ]

    describe "basic cursor motions" $ do
        -- Row 0 is "A line of words" (l at col 2). A single-cell vi
        -- selection yanks the character under the cursor, so each script
        -- reveals where the cursor came to rest.
        it "cursor-right advances one cell per press" $
            vi [ "history-top", "cursor-right", "cursor-right"
               , "begin-selection", "copy-selection" ]
                `shouldBe` ["l"]
        it "cursor-left retreats one cell" $
            vi [ "history-top", "cursor-right", "cursor-right"
               , "cursor-right", "cursor-left", "begin-selection"
               , "copy-selection" ]
                `shouldBe` ["l"]
        it "end-of-line lands on the last non-blank cell" $
            vi [ "history-top", "end-of-line"
               , "begin-selection", "copy-selection" ]
                `shouldBe` ["s"]
        it "cursor-down moves to the next row, clamping the column" $
            vi [ "history-top", "end-of-line", "cursor-down"
               , "begin-selection", "copy-selection" ]
                `shouldBe` ["e"]  -- row 1 "        Indented line", col 14
        it "cursor-up returns toward the previous row" $
            vi [ "history-top", "cursor-down", "cursor-up"
               , "begin-selection", "copy-selection" ]
                `shouldBe` ["A"]

    describe "emacs copy-mode motions (upstream copy-mode-test-emacs)" $ do
        it "clamps previous-word/space at the start of text" $
            emacs
                [ "history-top", "start-of-line", "begin-selection"
                , "previous-word", "previous-space", "previous-word"
                , "copy-selection"
                ]
            `shouldBe` [""]

        it "next-word-end does not skip single-letter words" $
            emacs
                [ "history-top", "start-of-line"
                , "next-word-end", "begin-selection", "previous-word"
                , "copy-selection"
                ]
            `shouldBe` ["A"]

    describe "selection overlay (reverse video)" $ do
        let cellsOf txt = V.fromList
                [ Cell { text = T.singleton c, width = 1, style = defaultStyle }
                | c <- take 5 (T.unpack txt <> repeat ' ') ]
            gridOf = V.fromList . map cellsOf
            -- All board cells start from defaultStyle, so a highlighted
            -- cell is exactly defaultStyle with reverse flipped on.
            revAt g r c = (g V.! r V.! c).style == defaultStyle { reverse = True }
            sel row col anchor keys = CopyModeState
                { cursorRow = row, cursorCol = col
                , selection = Just (anchor, SelChar)
                , keyTable = keys, viewportOffY = 0 }
            board = gridOf ["abcde", "fghij", "klmno"]

        it "reverse-videos a one-line span through the cursor cell (vi)" $ do
            let g = overlaySelection KeysVi 0 (sel 0 3 (0, 1) "copy-mode-vi") board
            map (revAt g 0) [0 .. 4] `shouldBe` [False, True, True, True, False]
            map (revAt g 1) [0 .. 4] `shouldBe` replicate 5 False

        it "ends the span before the cursor cell for emacs" $ do
            let g = overlaySelection KeysEmacs 0 (sel 0 3 (0, 1) "copy-mode") board
            map (revAt g 0) [0 .. 4] `shouldBe` [False, True, True, False, False]

        it "spans rows, filling the middle line to full width" $ do
            let g = overlaySelection KeysVi 0 (sel 2 1 (0, 2) "copy-mode-vi") board
            map (revAt g 0) [0 .. 4] `shouldBe` [False, False, True, True, True]
            map (revAt g 1) [0 .. 4] `shouldBe` replicate 5 True
            map (revAt g 2) [0 .. 4] `shouldBe` [True, True, False, False, False]

        it "leaves the grid untouched with no selection" $ do
            let noSel = (sel 0 0 (0, 0) "copy-mode-vi") { selection = Nothing }
                g = overlaySelection KeysVi 0 noSel board
            any (revAt g 0) [0 .. 4] `shouldBe` False

    describe "copy cursor placement" $ do
        let st row = CopyModeState
                { cursorRow = row, cursorCol = 2, selection = Nothing
                , keyTable = "copy-mode-vi", viewportOffY = 0 }
        it "maps an absolute cursor row into the viewport" $
            copyCursorPos 5 10 (st 7) `shouldBe` Just Pos { row = 2, col = 2 }
        it "is Nothing above the viewport" $
            copyCursorPos 5 10 (st 3) `shouldBe` Nothing
        it "is Nothing below the viewport" $
            copyCursorPos 5 3 (st 12) `shouldBe` Nothing

    describe "viewport scrolling (scrollToCursor)" $ do
        let st row voY = CopyModeState
                { cursorRow = row, cursorCol = 0, selection = Nothing
                , keyTable = "copy-mode-vi", viewportOffY = voY }
        it "keeps a visible cursor's viewport unchanged" $
            (scrollToCursor 5 10 (st 7 0)).viewportOffY `shouldBe` 0
        it "scrolls up to reveal a cursor in scrollback" $
            (scrollToCursor 5 3 (st 0 0)).viewportOffY `shouldBe` 5
        it "scrolls back down when the cursor drops below the viewport" $
            (scrollToCursor 5 3 (st 7 5)).viewportOffY `shouldBe` 0
