module Hat.Server.CopyModeSpec (spec) where

import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Data.Either (isLeft, isRight)
import Data.Functor.Identity (Identity, runIdentity)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import System.IO (hIsClosed)
import Test.Hspec

import Hat.Geometry (Pos (..), Size (..))
import Hat.Model
    ( CharSearch (..), CharStop (..), CopyModeState (..), FrozenGrid (..)
    , copyModeAt, ModeKind (..), CopyModeData (..), LineNumbers (..), MarkShown (..)
    , SearchDirection (..), SearchKind (..), SelKind (SelChar, SelLine, SelRect) )
import Hat.Model.OptionTable (OptionValue (..))
import Hat.Model.Options
    (ModeKeys (..), Options (..), applyDelta, defaultOptions, optionText, singletonDelta)
import Hat.Server (defaultKeymap)
import Hat.Server.Keys (Binding (..))
import Hat.Server.CopyMode
import Hat.Server.FormatDraw (styleCell)
import Hat.TestSupport (clientTheme)
import Hat.Term.Cell
    ( Cell (..), Color (..), Content (..), Style (..), Width (..), defaultStyle
    , glyphCell )
import Hat.Term.Emulator qualified as Emu

import Data.Map.Strict qualified as Map

-- | The pane grid the upstream copy-mode tests build (40x10). The
-- source file's leading TAB on row 1 reaches hat's grid as spaces
-- (the emulator expands tabs), so it is modelled as spaces here.
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

-- A grid with scrollback for the paging motions: 20 scrollback lines
-- above a 10-row screen (bottom row = 29), every row a single 'x'.
scrollGrid :: Grid Identity
scrollGrid = (listGrid 40 (replicate 30 "x")) { gHsize = 20, gSy = 10 }

viSeparators :: Text
viSeparators = defaultOptions.wordSeparators

-- | A tiny copy-mode interpreter: applies @-X@ command names to a
-- (state, buffers) pair, purely, mirroring the server dispatch.
data Sim = Sim { sState :: CopyModeState, sBufs :: [Text] }

start :: Sim
start = Sim
    { sState = copyModeAt "copy-mode" 0 0
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
    "cursor-right" -> motion (mCursorRight (selColumnBound st))
    "rectangle-toggle" -> sim { sState = rectangleToggle st }
    "cursor-up" -> sim { sState = runIdentity (cursorVertical (-1) grid st) }
    "cursor-down" -> sim { sState = runIdentity (cursorVertical 1 grid st) }
    "end-of-line" -> sim { sState = runIdentity (endOfLine keys grid st) }
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
    describe "the word and line under a position (mouse_word, mouse_line)" $ do
        let g = listGrid 20 ["alpha beta gamma", "  x-y "]
            word r c = runIdentity (gridWord "-" g r c)
        it "finds the word around a column" $ do
            word 0 2 `shouldBe` "alpha"
            word 0 7 `shouldBe` "beta"
            word 0 0 `shouldBe` "alpha"
        it "on a separator, takes the word after it" $
            word 0 5 `shouldBe` "beta"
        it "splits on the word separators" $ do
            word 1 2 `shouldBe` "x"
            word 1 4 `shouldBe` "y"
        it "reads a line up to its trimmed length" $
            runIdentity (gridLine g 1) `shouldBe` "  x-y"
    describe "default copy-mode key bindings" $ do
        let bound table key =
                (.commands) <$> (Map.lookup key =<< Map.lookup table defaultKeymap)
            beginSel = Just [["send-keys", "-X", "begin-selection"]]
        it "vi mode: Space begins a selection (tmux parity)" $
            bound "copy-mode-vi" "Space" `shouldBe` beginSel
        it "vi mode: v begins a selection" $
            bound "copy-mode-vi" "v" `shouldBe` beginSel
        it "emacs mode: Space begins a selection" $
            bound "copy-mode" "Space" `shouldBe` beginSel
        let rectToggle = Just [["send-keys", "-X", "rectangle-toggle"]]
        it "vi mode: C-v toggles rectangle selection (tmux parity)" $
            bound "copy-mode-vi" "C-v" `shouldBe` rectToggle
        it "emacs mode: R toggles rectangle selection (tmux parity)" $
            bound "copy-mode" "R" `shouldBe` rectToggle

    describe "default prefix key bindings (tmux parity)" $ do
        let bound key = (.commands) <$> (Map.lookup key =<< Map.lookup "prefix" defaultKeymap)
        it "! breaks the pane into a new window" $
            bound "!" `shouldBe` Just [["break-pane"]]
        it "q shows the pane numbers" $
            bound "q" `shouldBe` Just [["display-panes"]]
        it "{ swaps the pane with its predecessor" $
            bound "{" `shouldBe` Just [["swap-pane", "-U"]]
        it "} swaps the pane with its successor" $
            bound "}" `shouldBe` Just [["swap-pane", "-D"]]

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
        it "end-of-line lands on the last non-blank cell" $
            vi [ "history-top", "end-of-line"
               , "begin-selection", "copy-selection" ]
                `shouldBe` ["s"]
        it "cursor-down moves to the next row, clamping the column" $
            vi [ "history-top", "end-of-line", "cursor-down"
               , "begin-selection", "copy-selection" ]
                `shouldBe` ["e"]  -- row 1 "        Indented line", col 14

    describe "a wide character's padding column" $ do
        -- Row 0 "abc中" (中 at cols 3-4, col 4 its padding), row 1 "xyz",
        -- mirroring the tail of upstream copy-mode-test-vi.
        let narrow c = glyphCell c defaultStyle
            row0 = V.fromList ([narrow 'a', narrow 'b', narrow 'c'
                       , Cell { content = Glyph '中' [] Wide, style = defaultStyle }
                       , Cell { content = Continuation, style = defaultStyle }]
                       <> replicate 15 (narrow ' '))
            row1 = V.fromList (map narrow "xyz" <> replicate 17 (narrow ' '))
            wide = frozenGrid FrozenGrid
                { fgHsize = 0, fgSy = 2, fgSx = 20
                , fgRows = V.fromList [row0, row1], fgWrapped = V.empty }
            at col row = start.sState { cursorRow = row, cursorCol = col }
            go m s = runIdentity (runMotion wide KeysVi viSeparators m s)
            pos s = (s.cursorCol, s.cursorRow)
        it "vi cursor-right skips it onto the next line" $
            pos (go (mCursorRight ContentColumns) (at 3 0)) `shouldBe` (0, 1)
        it "vi cursor-left from the next line lands on the wide character" $
            pos (go mCursorLeft (at 0 1)) `shouldBe` (3, 0)
        it "cursor-left steps off the wide character normally" $
            pos (go mCursorLeft (at 3 0)) `shouldBe` (2, 0)

    describe "rectangle selection frees the column band" $ do
        -- Row 0 "A line of words" has content length 15; the grid is 40
        -- wide. A rectangular selection lets the cursor roam the blank
        -- space past the content, so a block can cover trailing blanks.
        let at row col = start.sState { cursorRow = row, cursorCol = col }
            rect s = s { selection = Just ((s.cursorRow, s.cursorCol), SelRect) }
            right st = runIdentity
                (runMotion grid KeysVi viSeparators
                    (mCursorRight (selColumnBound st)) st)
        it "cursor-right advances past the last character" $
            (right (rect (at 0 15))).cursorCol `shouldBe` 16
        it "cursor-right wraps to the next line at the grid width" $ do
            let s = right (rect (at 0 40))
            (s.cursorRow, s.cursorCol) `shouldBe` (1, 0)
        it "cursor-right wraps past the last character without a rectangle" $ do
            let s = right (at 0 14)
            (s.cursorRow, s.cursorCol) `shouldBe` (1, 0)
        it "cursor-down keeps the column" $ do
            let s = runIdentity (cursorVertical 1 grid (rect (at 0 30)))
            (s.cursorRow, s.cursorCol) `shouldBe` (1, 30)
        it "page-up keeps the column" $ do
            let s = runIdentity (scrollUp scrollGrid.gSy scrollGrid
                        (rect ((at 29 30) { viewportOffY = 0 })))
            s.cursorCol `shouldBe` 30
        it "vi end-of-line lands on the last grid column" $
            (runIdentity (endOfLine KeysVi grid (rect (at 0 0)))).cursorCol
                `shouldBe` 39
        it "emacs end-of-line lands one past the last grid column" $
            (runIdentity (endOfLine KeysEmacs grid (rect (at 0 0)))).cursorCol
                `shouldBe` 40
        it "cursor-left steps one cell through blank space (bug: snapped to line end)" $ do
            let s = runIdentity
                    (runMotion grid KeysVi viSeparators mCursorLeft
                        (rect (at 0 30)))
            s.cursorCol `shouldBe` 29
        it "a full-width block yanks every line whole" $
            vi [ "history-top", "rectangle-toggle", "end-of-line"
               , "cursor-down", "cursor-down", "copy-selection" ]
                `shouldBe`
                    ["A line of words\n        Indented line\nAnother line..."]

    describe "paging and jump motions" $ do
        -- The shared grid has 10 rows (sy = 10, no scrollback), so the
        -- bottom row is 9 and the viewport spans rows 0..9.
        let at row col = start.sState { cursorRow = row, cursorCol = col }
            runG f s = runIdentity (f grid s)
        it "history-bottom (G) jumps to the last grid row" $ do
            let s = runG historyBottom (at 0 3)
            (s.cursorRow, s.cursorCol) `shouldBe` (9, 0)  -- row 9 is blank
        it "top/middle/bottom-line land on viewport rows" $ do
            (runG topLine (at 5 0)).cursorRow `shouldBe` 0
            (runG middleLine (at 0 0)).cursorRow `shouldBe` 4
            (runG bottomLine (at 0 0)).cursorRow `shouldBe` 9
        it "page-up scrolls the viewport and cursor up a screenful" $ do
            -- 20 scrollback + 10 screen; start at the bottom (offset 0).
            let bottom = (at 29 0) { viewportOffY = 0 }
                s = runIdentity (scrollUp scrollGrid.gSy scrollGrid bottom)
            (s.viewportOffY, s.cursorRow) `shouldBe` (10, 19)
        it "page-down reverses page-up" $ do
            let up = runIdentity (scrollUp scrollGrid.gSy scrollGrid
                        ((at 29 0) { viewportOffY = 0 }))
                s = runIdentity (scrollDown scrollGrid.gSy scrollGrid up)
            (s.viewportOffY, s.cursorRow) `shouldBe` (0, 29)
        it "halfpage-up scrolls half a screenful" $ do
            let s = runIdentity (scrollUp (scrollGrid.gSy `div` 2) scrollGrid
                        ((at 29 0) { viewportOffY = 0 }))
            (s.viewportOffY, s.cursorRow) `shouldBe` (5, 24)
        it "page-up clamps at the top of the scrollback" $ do
            let s = runIdentity (scrollUp 999 scrollGrid
                        ((at 29 0) { viewportOffY = 0 }))
            s.viewportOffY `shouldBe` 20   -- gHsize
        it "back-to-indentation (^) lands on the first non-blank column" $
            -- Row 1 is \"        Indented line\" (8 leading spaces).
            (runG backToIndentation (at 1 20)).cursorCol `shouldBe` 8

    describe "line selection (V) and other-end (o)" $ do
        it "other-end swaps the cursor and the selection anchor" $ do
            let s = (start.sState)
                    { cursorRow = 3, cursorCol = 5
                    , selection = Just ((1, 2), SelChar) }
                s' = otherEnd s
            (s'.cursorRow, s'.cursorCol) `shouldBe` (1, 2)
            s'.selection `shouldBe` Just ((3, 5), SelChar)
        it "other-end is a no-op without a selection" $
            otherEnd (start.sState) `shouldBe` start.sState
        it "rectangle-toggle begins a rectangular selection at the cursor" $ do
            let s = (start.sState) { cursorRow = 3, cursorCol = 5 }
            (rectangleToggle s).selection `shouldBe` Just ((3, 5), SelRect)
        it "rectangle-toggle keeps the anchor, only changing the shape" $ do
            let s = (start.sState)
                    { cursorRow = 3, cursorCol = 5
                    , selection = Just ((1, 2), SelChar) }
                s' = rectangleToggle s
            s'.selection `shouldBe` Just ((1, 2), SelRect)
            (s'.cursorRow, s'.cursorCol) `shouldBe` (3, 5)
        it "rectangle-toggle flips a rectangle back to a char selection" $ do
            let s = (start.sState)
                    { selection = Just ((1, 2), SelRect) }
            (rectangleToggle s).selection `shouldBe` Just ((1, 2), SelChar)
        it "select-line yanks whole lines from anchor to cursor" $ do
            let s = (start.sState)
                    { cursorRow = 2, cursorCol = 3
                    , selection = Just ((0, 5), SelLine) }
            runIdentity (extractSelection grid KeysVi s)
                `shouldBe` Just
                    "A line of words\n        Indented line\nAnother line..."
        it "rectangle yanks each row clamped to the same column band" $ do
            -- Anchor (0,2), cursor (2,5), vi-inclusive: columns 2..5 of
            -- rows 0..2. Row 0 "A line of words" -> "line", row 1's
            -- indentation -> four spaces, row 2 "Another line..." -> "othe".
            let s = (start.sState)
                    { cursorRow = 2, cursorCol = 5
                    , selection = Just ((0, 2), SelRect) }
            runIdentity (extractSelection grid KeysVi s)
                `shouldBe` Just "line\n    \nothe"

    describe "a stopped selection (stop-selection, scrolling while selecting)" $ do
        let selecting = (start.sState)
                { cursorRow = 2, cursorCol = 0, selection = Just ((0, 0), SelChar) }
            stopped = runIdentity (stopSelection grid selecting)
            yank = runIdentity . extractSelection grid KeysEmacs
        it "stop-selection leaves the far end where the cursor was" $ do
            let moved = runIdentity (cursorVertical 2 grid stopped)
            yank moved `shouldBe` yank selecting
        it "other-end picks the stopped end back up with the cursor" $ do
            let s = otherEnd stopped
            (s.cursorRow, s.cursorCol) `shouldBe` (2, 0)
            yank (runIdentity (cursorVertical 1 grid s))
                `shouldBe` Just "A line of words\n        Indented line\nAnother line...\n"
        it "a second other-end swaps the cursor to the anchor" $ do
            let s = otherEnd (otherEnd stopped)
            (s.cursorRow, s.cursorCol) `shouldBe` (0, 0)
            s.selection `shouldBe` Just ((2, 0), SelChar)
        it "stopping a line selection keeps its whole lines" $ do
            let lines' = selecting { selection = Just ((0, 3), SelLine), cursorCol = 4 }
                s = runIdentity (stopSelection grid lines')
            yank (runIdentity (cursorVertical 2 grid s)) `shouldBe` yank lines'
        it "clear-selection forgets the stopped end" $
            (clearSelection stopped).pinnedEnd `shouldBe` Nothing
        it "scroll-up with a selection only moves the view" $ do
            let sel = (copyModeAt "copy-mode" 25 0)
                    { selection = Just ((22, 0), SelChar) }
                s = runIdentity (scrollUpCmd KeysEmacs scrollGrid sel)
            s.viewportOffY `shouldBe` 1
            selectionEnd s `shouldBe` (25, 0)

    describe "scroll-up and scroll-down move the view a line" $ do
        -- scrollGrid: 20 history lines over a 10-row screen.
        let at row voY = (copyModeAt "copy-mode" row 0) { viewportOffY = voY }
            screenRow s = s.cursorRow - (20 - s.viewportOffY)
            down keys = runIdentity . scrollLine keys 1 scrollGrid
            up keys = runIdentity . scrollLine keys (-1) scrollGrid
        it "keeps the cursor's screen row under emacs keys" $ do
            let s = down KeysEmacs (at 3 20)
            (s.viewportOffY, s.cursorRow, screenRow s) `shouldBe` (19, 4, 3)
        it "keeps the cursor on its line under vi keys" $ do
            let s = down KeysVi (at 3 20)
            (s.viewportOffY, s.cursorRow) `shouldBe` (19, 3)
            (up KeysVi (at 3 19)).cursorRow `shouldBe` 3
        it "carries the cursor along at the screen's edge under vi keys" $ do
            (down KeysVi (at 0 20)).cursorRow `shouldBe` 1
            (up KeysVi (at 10 19)).cursorRow `shouldBe` 9

    describe "scrolling the cursor's line (scroll-top/middle/bottom, recentre)" $ do
        -- scrollGrid: 20 history lines over a 10-row screen.
        let at row voY = (copyModeAt "copy-mode" row 0) { viewportOffY = voY }
            screenRow s = s.cursorRow - (20 - s.viewportOffY)
        it "scroll-top puts the cursor's line on the first row" $ do
            let s = scrollCursorTo 0 scrollGrid (at 12 10)
            (s.cursorRow, screenRow s) `shouldBe` (12, 0)
        it "scroll-bottom puts it on the last row" $ do
            let s = scrollCursorTo 9 scrollGrid (at 15 10)
            (s.cursorRow, screenRow s) `shouldBe` (15, 9)
        it "does nothing when the history cannot scroll that far" $
            (scrollCursorTo 0 scrollGrid (at 29 0)).viewportOffY `shouldBe` 0
        it "recentre cycles middle, top, bottom on the same line" $ do
            let r1 = recentreTopBottom scrollGrid (at 15 10)
                r2 = recentreTopBottom scrollGrid r1
                r3 = recentreTopBottom scrollGrid r2
            map screenRow [r1, r2, r3] `shouldBe` [4, 0, 9]
        it "recentre starts over at the middle on another line" $ do
            let r1 = recentreTopBottom scrollGrid (at 15 10)
                moved = r1 { cursorRow = 16 }
            screenRow (recentreTopBottom scrollGrid moved) `shouldBe` 4

    describe "a resize in copy mode" $ do
        -- Rows as text, a trailing '+' marking a row that wraps.
        let gridOf w sy hs rows = FrozenGrid
                { fgHsize = hs, fgSy = sy, fgSx = w
                , fgRows = V.fromList [ rowOf w (filter (/= '+') r) | r <- rows ]
                , fgWrapped = V.fromList [ "+" `T.isSuffixOf` T.pack r | r <- rows ] }
            rowOf w r = V.fromList (cellsOf r <> replicate (w - widthOf r) (glyphCell ' ' defaultStyle))
            cellsOf = concatMap $ \c -> if c == '日'
                then [ Cell { content = Glyph c [] Wide, style = defaultStyle }
                     , Cell { content = Continuation, style = defaultStyle } ]
                else [glyphCell c defaultStyle]
            widthOf = sum . map (\c -> if c == '日' then 2 else 1)
            textOf fg = [ T.stripEnd (T.pack [ baseChar c | c <- V.toList r, c.content /= Continuation ])
                          <> (if w then "+" else "")
                        | (r, w) <- zip (V.toList fg.fgRows)
                            (V.toList fg.fgWrapped <> repeat False) ]
            baseChar c = case c.content of
                Glyph ch _ _ -> ch
                Continuation -> ' '
            resized sz fg st = resizeCopyMode sz CopyModeData { kind = ModeCopy, frozen = fg, copyState = st }
            cursorAt r c = (copyModeAt "copy-mode" r c)
        it "rewraps the lines at the new width" $ do
            let pm = resized (Size { rows = 3, cols = 3 })
                    (gridOf 4 3 0 ["abcd+", "ef", "xy"]) (cursorAt 1 1)
            textOf pm.frozen `shouldBe` ["abc+", "def", "xy"]
            (pm.frozen.fgHsize, pm.frozen.fgSy, pm.frozen.fgSx) `shouldBe` (0, 3, 3)
        it "joins wrapped rows again as it widens, padding the screen" $ do
            let pm = resized (Size { rows = 3, cols = 6 })
                    (gridOf 3 3 0 ["abc+", "de", "x"]) (cursorAt 1 1)
            textOf pm.frozen `shouldBe` ["abcde", "x", ""]
        it "moves a wide character that does not fit to the next row" $
            textOf (resized (Size { rows = 3, cols = 2 }) (gridOf 4 3 0 ["a日b", "", ""]) (cursorAt 0 0)).frozen
                `shouldBe` ["a+", "日+", "b", "", ""]
        it "keeps the cursor on its character, scrolled to it in the history" $ do
            let onF = resized (Size { rows = 2, cols = 3 })
                    (gridOf 4 2 1 ["abcd+", "ef", "xy"]) (cursorAt 1 1)
                onB = resized (Size { rows = 2, cols = 3 })
                    (gridOf 4 2 1 ["abcd+", "ef", "xy"]) (cursorAt 0 1)
            (onF.copyState.cursorRow, onF.copyState.cursorCol, onF.copyState.viewportOffY)
                `shouldBe` (1, 2, 0)
            onB.frozen.fgHsize `shouldBe` 1
            (onB.copyState.cursorRow, onB.copyState.cursorCol, onB.copyState.viewportOffY)
                `shouldBe` (0, 1, 1)
        it "a new height alone moves rows between the screen and the history" $ do
            let pm = resized (Size { rows = 2, cols = 4 })
                    (gridOf 4 3 0 ["ab", "cd", "ef"]) (cursorAt 2 1)
            textOf pm.frozen `shouldBe` ["ab", "cd", "ef"]
            pm.frozen.fgHsize `shouldBe` 1
            (pm.copyState.cursorRow, pm.copyState.cursorCol, pm.copyState.viewportOffY)
                `shouldBe` (2, 1, 0)

    describe "copy-mode formats" $ do
        -- Two history lines above a two-row screen.
        let rows = ["one two", "three", "four five", "six"]
            frozen = FrozenGrid
                { fgHsize = 2, fgSy = 2, fgSx = 10
                , fgRows = V.fromList
                    [ V.fromList [ glyphCell c defaultStyle | c <- take 10 (T.unpack r <> repeat ' ') ]
                    | r <- rows ], fgWrapped = V.empty }
            varsUnder n st = Map.fromList (copyModeFormats " " n CopyModeData { kind = ModeCopy, frozen = frozen, copyState = st })
            vars = varsUnder NumbersOff
            at = copyModeAt "copy-mode" 2 5
            look k st = Map.lookup k (vars st)
        it "places the cursor on the screen and reads its line and word" $ do
            let st = (at { viewportOffY = 1 })
            map (`look` st) ["copy_cursor_x", "copy_cursor_y", "copy_cursor_line", "copy_cursor_word", "scroll_position"]
                `shouldBe` map Just ["5", "1", "four five", "five", "1"]
        it "counts the position from the bottom of the history" $
            map (`look` at { viewportOffY = 2 }) ["copy_position", "copy_position_limit"]
                `shouldBe` [Just "2", Just "2"]
        it "has no selection to report until one is begun" $ do
            map (`look` at) ["selection_present", "selection_active", "selection_start_x"]
                `shouldBe` [Just "0", Just "0", Nothing]
        it "reports a selection's ends, and whether the cursor still drives it" $ do
            let st = at { selection = Just ((1, 2), SelChar) }
                stopped = runIdentity (stopSelection (frozenGrid frozen) st)
            map (`look` st) ["selection_start_x", "selection_start_y", "selection_end_x", "selection_end_y", "selection_present", "selection_active"]
                `shouldBe` map Just ["2", "1", "5", "2", "1", "1"]
            look "selection_active" stopped `shouldBe` Just "0"
        it "keeps tmux's start and end through other-end" $ do
            let swapped = otherEnd at { selection = Just ((1, 2), SelChar) }
            map (`look` swapped) ["selection_start_y", "selection_end_y"]
                `shouldBe` [Just "1", Just "2"]
        it "counts the position from the top under absolute line numbers" $ do
            let v = varsUnder NumbersAbsolute (at { viewportOffY = 2 })
            map (`Map.lookup` v) ["copy_position", "copy_position_limit", "copy_line_numbers"]
                `shouldBe` map Just ["1", "4", "1"]
        it "reports the highlighted search and its matches" $ do
            let searched = at { lastQuery = Just ("o", Forward, SearchText), searchMarks = Just Forward }
            map (`look` searched) ["search_present", "search_count"]
                `shouldBe` [Just "1", Just "3"]
            look "search_present" at `shouldBe` Just "0"
        it "a selection begun and not yet moved is not present" $
            look "selection_present" (at { selection = Just ((2, 5), SelChar) }) `shouldBe` Just "0"
        it "gives no variable copyModeNames leaves out" $ do
            let states =
                    [ at
                    , at { selection = Just ((1, 2), SelChar) }
                    , at { lastQuery = Just ("o", Forward, SearchText), searchMarks = Just Forward }
                    ]
                given = [ k | n <- [NumbersOff, NumbersAbsolute], st <- states, k <- Map.keys (varsUnder n st) ]
            filter (`notElem` copyModeNames) given `shouldBe` []

    describe "copy-pipe's command" $ do
        let withCopyCommand c = applyDelta (singletonDelta "copy-command" (OVScalar c)) defaultOptions
        it "is the one given" $
            pipeCommand (withCopyCommand "xsel") "cat" `shouldBe` Just "cat"
        it "falls back to copy-command" $
            pipeCommand (withCopyCommand "xsel -i") "" `shouldBe` Just "xsel -i"
        it "runs nothing when both are empty" $
            pipeCommand defaultOptions "" `shouldBe` Nothing

    describe "drawing copy mode (copyView)" $ do
        let rows = ["alpha beta", "gamma alpha", "delta"]
            frozen = FrozenGrid
                { fgHsize = 0, fgSy = 3, fgSx = 12
                , fgRows = V.fromList
                    [ V.fromList [ glyphCell c defaultStyle | c <- take 12 (T.unpack r <> repeat ' ') ]
                    | r <- rows ], fgWrapped = V.empty }
            fgOf n = defaultStyle { fg = Indexed n }
            look = CopyLook
                { keys = KeysVi, numbers = NumbersOff
                , selectionStyle = fgOf 1, matchStyle = fgOf 2
                , currentMatchStyle = fgOf 3
                , markStyle = defaultStyle { fg = Indexed 4, bg = Indexed 5 }
                , currentLineStyle = Nothing
                , lineNumberStyle = fgOf 7, currentLineNumberStyle = fgOf 8
                , drawPosition = \area -> area V.// [(0, glyphCell 'P' (fgOf 9))]
                , searchPattern = Nothing }
            view l st = copyView l CopyModeData { kind = ModeCopy, frozen = frozen, copyState = st }
            at r c = (copyModeAt "copy-mode-vi" r c) { hidePosition = True }
            text l st r = T.pack [ baseChar' cell | cell <- V.toList (fst (view l st) V.! r) ]
            baseChar' cell = case cell.content of
                Glyph c _ _ -> c
                _ -> '?'
            styleAt l st r c = (fst (view l st) V.! r V.! c).style
        it "draws the selection in copy-mode-selection-style" $ do
            let st = (at 0 2) { selection = Just ((0, 0), SelChar) }
            map (styleAt look st 0) [0, 2, 3] `shouldBe` [fgOf 1, fgOf 1, defaultStyle]
        it "draws the position format over the first row unless hidden" $ do
            text look (at 1 0) { hidePosition = False } 0 `shouldBe` "Plpha beta  "
            text look (at 1 0) 0 `shouldBe` "alpha beta  "
        it "draws the position in hat's black on ANSI yellow by default" $
            styleCell (clientTheme defaultOptions)
                (optionText "copy-mode-position-style" defaultOptions)
                `shouldBe` defaultStyle { fg = Indexed 0, bg = Indexed 3 }
        it "numbers the lines beside the text, which moves over" $ do
            let numbered = look { numbers = NumbersAbsolute }
            map (text numbered (at 1 0)) [0, 1] `shouldBe` ["  1 alpha be", "  2 gamma al"]
            snd (view numbered (at 1 3)) `shouldBe` Just Pos { row = 1, col = 7 }
            map (\r -> styleAt numbered (at 1 0) r 0) [0, 1] `shouldBe` [fgOf 7, fgOf 8]
        it "counts relative and hybrid numbers from the cursor's line" $ do
            map (text look { numbers = NumbersRelative } (at 1 0)) [0, 1, 2]
                `shouldBe` ["  1 alpha be", "  0 gamma al", "  1 delta   "]
            text look { numbers = NumbersHybrid } (at 1 0) 1 `shouldBe` "  2 gamma al"
        it "shows no numbers once copy mode hides them" $
            text look { numbers = NumbersAbsolute } (at 1 0) { lineNumbers = LinesHidden } 0
                `shouldBe` "alpha beta  "
        it "marks a cursor past the numbered text's width with $" $
            text look { numbers = NumbersAbsolute } (at 1 9) 1 `shouldBe` "  2 gamma a$"
        it "draws the cursor's line in copy-mode-current-line-style" $ do
            let lined = look { currentLineStyle = Just defaultStyle { bg = Indexed 6 } }
            styleAt lined (at 1 0) 1 4 `shouldBe` defaultStyle { bg = Indexed 6 }
            styleAt lined (at 1 0) 0 4 `shouldBe` defaultStyle
        it "draws a shown mark's line in copy-mode-mark-style, inverted at the mark" $ do
            let marked = (at 0 0) { mark = (2, 1), markShown = MarkShown }
            styleAt look marked 2 0 `shouldBe` defaultStyle { fg = Indexed 4, bg = Indexed 5 }
            styleAt look marked 2 1 `shouldBe` defaultStyle { fg = Indexed 5, bg = Indexed 4 }
            styleAt look (at 0 0) { mark = (2, 1) } 2 0 `shouldBe` defaultStyle
        it "highlights the search's matches, the one under the cursor as current" $ do
            let searched = look { searchPattern = Just (either (error . T.unpack) id (compilePattern SearchText "alpha")) }
                st = (at 1 6) { searchMarks = Just Forward }
            styleAt searched st 0 0 `shouldBe` fgOf 2
            styleAt searched st 1 8 `shouldBe` fgOf 3
            styleAt searched st { searchMarks = Nothing } 0 0 `shouldBe` defaultStyle
        it "under emacs keys a forward search's current match is the one before the cursor" $ do
            let searched = look { keys = KeysEmacs, searchPattern = Just (either (error . T.unpack) id (compilePattern SearchText "alpha")) }
            styleAt searched (at 0 5) { searchMarks = Just Forward } 0 0 `shouldBe` fgOf 3

    describe "copy mode's mark, line numbers and search highlighting" $ do
        it "set-mark shows the mark at the cursor" $
            let s = setMark (copyModeAt "copy-mode" 4 2)
            in (s.mark, s.markShown) `shouldBe` ((4, 2), MarkShown)
        it "jump-to-mark trades the cursor and the mark" $ do
            let s = jumpToMark scrollGrid ((copyModeAt "copy-mode" 25 1) { mark = (3, 0) })
            ((s.cursorRow, s.cursorCol), s.mark, s.viewportOffY) `shouldBe` ((3, 0), (25, 1), 17)
        it "line-numbers-on forces numbers the option leaves off" $ do
            (setLineNumbers ForceNumbers NumbersOff True (copyModeAt "copy-mode" 0 0)).lineNumbers
                `shouldBe` LinesForced
            (setLineNumbers ForceNumbers NumbersRelative True (copyModeAt "copy-mode" 0 0)).lineNumbers
                `shouldBe` LinesFromOption
            effectiveNumbers NumbersOff LinesForced `shouldBe` NumbersDefault
        it "motions keep the search highlighted under vi keys, not emacs" $ do
            clearsMarks KeysVi "cursor-down" `shouldBe` False
            clearsMarks KeysEmacs "cursor-down" `shouldBe` True
            clearsMarks KeysVi "begin-selection" `shouldBe` True
            clearsMarks KeysEmacs "search-again" `shouldBe` False
        it "an emacs forward search leaves the cursor past the match" $ do
            let p = either (error . T.unpack) id (compilePattern SearchText "line")
                s = runIdentity (searchStep grid KeysEmacs WrapAround Forward p (start.sState))
            ((s.cursorRow, s.cursorCol), s.searchMarks) `shouldBe` ((0, 6), Just Forward)
        it "a vi search leaves the cursor on the match" $ do
            let p = either (error . T.unpack) id (compilePattern SearchText "line")
                s = runIdentity (searchStep grid KeysVi WrapAround Forward p (start.sState))
            (s.cursorRow, s.cursorCol) `shouldBe` (0, 2)
        it "the next emacs search can start under the cursor" $ do
            let p = either (error . T.unpack) id (compilePattern SearchText "ab")
                g = listGrid 10 ["abab"]
                s = runIdentity (searchStep g KeysEmacs WrapAround Forward p
                        (start.sState) { cursorCol = 2 })
            s.cursorCol `shouldBe` 4

    describe "char search (f/F/t/T)" $ do
        -- Row 0 is "A line of words"; 'o' is at columns 7 and 11.
        let row0 col = (start.sState) { cursorRow = 0, cursorCol = col }
            search dir stp c st =
                runIdentity (charSearch grid (CharSearch dir stp) c st)
        it "f moves onto the next occurrence" $
            (search Forward OnTarget 'o' (row0 0)).cursorCol `shouldBe` 7
        it "t stops one cell before the next occurrence" $
            (search Forward ShortOfTarget 'o' (row0 0)).cursorCol `shouldBe` 6
        it "F moves onto the previous occurrence" $
            (search Backward OnTarget 'o' (row0 10)).cursorCol `shouldBe` 7
        it "T stops one cell after the previous occurrence" $
            (search Backward ShortOfTarget 'o' (row0 10)).cursorCol `shouldBe` 8
        it "repeating f (via ;) finds the following occurrence" $
            (search Forward OnTarget 'o' (row0 7)).cursorCol `shouldBe` 11
        it "does not move when the target is absent" $
            (search Forward OnTarget 'z' (row0 0)).cursorCol `shouldBe` 0

    describe "string search (/ ? n N)" $ do
        -- "line" occurs at (0,2), (1,17) and (2,8) in the shared grid.
        let from r c = (start.sState) { cursorRow = r, cursorCol = c }
            pat kind q = either (error . T.unpack) id (compilePattern kind q)
            findW wrap dir p st =
                runIdentity (findMatch grid wrap dir p (st.cursorRow, st.cursorCol))
            find = findW WrapAround
            regex = pat SearchRegex
        it "/ finds the next match" $
            find Forward (regex "line") (from 0 0) `shouldBe` Just (0, 2)
        it "n advances to the following match" $
            find Forward (regex "line") (from 0 2) `shouldBe` Just (1, 17)
        it "forward search wraps to the first match" $
            find Forward (regex "line") (from 2 8) `shouldBe` Just (0, 2)
        it "? / N find the previous match" $
            find Backward (regex "line") (from 2 8) `shouldBe` Just (1, 17)
        it "backward search wraps to the last match" $
            find Backward (regex "line") (from 0 2) `shouldBe` Just (2, 8)
        it "with wrap-search off, a forward search stops at the last match" $
            findW StopAtEnd Forward (regex "line") (from 2 8) `shouldBe` Nothing
        it "with wrap-search off, a backward search stops at the first match" $
            findW StopAtEnd Backward (regex "line") (from 0 2) `shouldBe` Nothing
        it "no match returns Nothing" $
            find Forward (regex "zzz") (from 0 0) `shouldBe` Nothing
        -- tmux >= 3.1: / and ? take a regex; -text variants are literal.
        it "character classes and quantifiers match" $
            find Forward (regex "[0-9]+xyz") (from 0 0) `shouldBe` Just (4, 4)
        it "^ anchors to the start of a line" $
            find Forward (regex "^Another") (from 0 0) `shouldBe` Just (2, 0)
        it "$ anchors to the end of a line" $
            find Forward (regex "words$") (from 0 0) `shouldBe` Just (0, 10)
        it "alternation matches in reading order" $
            find Forward (regex "Indented|Another") (from 0 0)
                `shouldBe` Just (1, 8)
        it "the default search treats . as a wildcard" $
            find Forward (regex "line...") (from 0 0) `shouldBe` Just (0, 2)
        it "a -text search treats . literally" $
            find Forward (pat SearchText "line...") (from 0 0)
                `shouldBe` Just (2, 8)
        it "an all-lowercase regex matches uppercase text (bug 17)" $
            find Forward (regex "another") (from 0 0) `shouldBe` Just (2, 0)
        it "an all-lowercase -text query matches uppercase text (bug 17)" $
            find Forward (pat SearchText "another") (from 0 0)
                `shouldBe` Just (2, 0)
        it "an uppercase character keeps a regex case-sensitive (bug 17)" $
            find Forward (regex "ANOTHER") (from 0 0) `shouldBe` Nothing
        it "an uppercase character keeps a -text query case-sensitive (bug 17)" $
            find Forward (pat SearchText "ANOTHER") (from 0 0)
                `shouldBe` Nothing
        it "character classes work under smart-case (bug 17)" $
            find Forward (regex "[a-z]nother") (from 0 0) `shouldBe` Just (2, 0)
        it "a pattern matching empty still terminates and advances" $
            find Forward (regex "z*") (from 0 0) `shouldBe` Just (0, 1)
        it "an invalid regex is a compile error, not a silent no-match" $
            isLeft (compilePattern SearchRegex "[") `shouldBe` True
        it "a -text pattern never fails to compile" $
            isRight (compilePattern SearchText "[") `shouldBe` True
        it "the literal -text command variants are wired" $
            map (`Map.member` handlers)
                ["search-forward-text", "search-backward-text"]
                `shouldBe` [True, True]

    describe "paragraph motions" $ do
        -- Shared grid: rows 0..4 hold text, rows 5..9 are blank.
        let at row col = start.sState { cursorRow = row, cursorCol = col }
        it "next-paragraph (}) jumps to the following blank line" $
            (runIdentity (paragraphDown grid (at 0 3))).cursorRow `shouldBe` 5
        it "previous-paragraph ({) jumps to the preceding blank line" $
            (runIdentity (paragraphUp grid (at 5 0))).cursorRow `shouldBe` 0

    describe "numeric count prefix" $ do
        let base = start.sState
        it "accumulates digits into the count" $ do
            (pushDigit 3 base).numPrefix `shouldBe` Just 3
            (pushDigit 0 (pushDigit 1 base)).numPrefix `shouldBe` Just 10
            (pushDigit 5 (pushDigit 2 base)).numPrefix `shouldBe` Just 25
        it "treats a bare 0 as start-of-line, not a count" $ do
            let s = pushDigit 0 (base { cursorCol = 7 })
            (s.numPrefix, s.cursorCol) `shouldBe` (Nothing, 0)

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
                [ glyphCell c defaultStyle
                | c <- take 5 (T.unpack txt <> repeat ' ') ]
            gridOf = V.fromList . map cellsOf
            -- The default mode style is reverse-only, so on a defaultStyle
            -- board a highlighted cell is exactly defaultStyle with reverse
            -- flipped on.
            dfltMode = defaultOptions.modeStyle
            revAt g r c = (g V.! r V.! c).style == defaultStyle { reverse = True }
            sel row col anchor keys =
                (copyModeAt keys row col) { selection = Just (anchor, SelChar) }
            board = gridOf ["abcde", "fghij", "klmno"]

        it "reverse-videos a one-line span through the cursor cell (vi)" $ do
            let g = overlaySelection dfltMode KeysVi 0 (sel 0 3 (0, 1) "copy-mode-vi") board
            map (revAt g 0) [0 .. 4] `shouldBe` [False, True, True, True, False]
            map (revAt g 1) [0 .. 4] `shouldBe` replicate 5 False

        it "ends the span before the cursor cell for emacs" $ do
            let g = overlaySelection dfltMode KeysEmacs 0 (sel 0 3 (0, 1) "copy-mode") board
            map (revAt g 0) [0 .. 4] `shouldBe` [False, True, True, False, False]

        it "spans rows, filling the middle line to full width" $ do
            let g = overlaySelection dfltMode KeysVi 0 (sel 2 1 (0, 2) "copy-mode-vi") board
            map (revAt g 0) [0 .. 4] `shouldBe` [False, False, True, True, True]
            map (revAt g 1) [0 .. 4] `shouldBe` replicate 5 True
            map (revAt g 2) [0 .. 4] `shouldBe` [True, True, False, False, False]

        it "leaves the grid untouched with no selection" $ do
            let noSel = (sel 0 0 (0, 0) "copy-mode-vi") { selection = Nothing }
                g = overlaySelection dfltMode KeysVi 0 noSel board
            any (revAt g 0) [0 .. 4] `shouldBe` False

        it "line selection reverse-videos whole rows between the ends" $ do
            -- Anchor row 0, cursor row 1: rows 0 and 1 fully highlighted,
            -- row 2 untouched, regardless of columns.
            let lineSel = (sel 1 2 (0, 3) "copy-mode-vi")
                    { selection = Just ((0, 3), SelLine) }
                g = overlaySelection dfltMode KeysVi 0 lineSel board
            map (revAt g 0) [0 .. 4] `shouldBe` replicate 5 True
            map (revAt g 1) [0 .. 4] `shouldBe` replicate 5 True
            map (revAt g 2) [0 .. 4] `shouldBe` replicate 5 False

        it "rectangle clamps every row to the same column band" $ do
            -- Anchor (0,1), cursor (2,3), vi-inclusive: columns 1..3 of
            -- every row, a straight block rather than a ragged span.
            let rectSel = (sel 2 3 (0, 1) "copy-mode-vi")
                    { selection = Just ((0, 1), SelRect) }
                g = overlaySelection dfltMode KeysVi 0 rectSel board
            map (revAt g 0) [0 .. 4] `shouldBe` [False, True, True, True, False]
            map (revAt g 1) [0 .. 4] `shouldBe` [False, True, True, True, False]
            map (revAt g 2) [0 .. 4] `shouldBe` [False, True, True, True, False]

        it "paints a themed mode-style's colours over selected cells" $ do
            -- A concrete bg/fg mode style overrides the cell's own colours
            -- (not reverse-video); unselected cells keep defaultStyle.
            let themed = defaultStyle
                    { fg = Indexed 0, bg = Indexed 108 }
                g = overlaySelection themed KeysVi 0
                        (sel 0 3 (0, 1) "copy-mode-vi") board
                styleAt r c = (g V.! r V.! c).style
            map (styleAt 0) [0 .. 4] `shouldBe`
                [ defaultStyle, themed, themed, themed, defaultStyle ]

    describe "copy cursor placement" $ do
        let st row = copyModeAt "copy-mode-vi" row 2
        it "maps an absolute cursor row into the viewport" $
            copyCursorPos 5 10 (st 7) `shouldBe` Just Pos { row = 2, col = 2 }
        it "is Nothing above the viewport" $
            copyCursorPos 5 10 (st 3) `shouldBe` Nothing
        it "is Nothing below the viewport" $
            copyCursorPos 5 3 (st 12) `shouldBe` Nothing

    describe "viewport scrolling (scrollToCursor)" $ do
        let st row voY = (copyModeAt "copy-mode-vi" row 0) { viewportOffY = voY }
        it "keeps a visible cursor's viewport unchanged" $
            (scrollToCursor 5 10 (st 7 0)).viewportOffY `shouldBe` 0
        it "scrolls up to reveal a cursor in scrollback" $
            (scrollToCursor 5 3 (st 0 0)).viewportOffY `shouldBe` 5
        it "scrolls back down when the cursor drops below the viewport" $
            (scrollToCursor 5 3 (st 7 5)).viewportOffY `shouldBe` 0

    describe "frozen snapshot (freezeGrid)" $ do
        -- Copy mode browses a frozen snapshot of the pane: output the
        -- program produces after entering copy mode must NOT leak into the
        -- captured grid.
        let rowText g r = runIdentity $
                T.pack <$> forM [0 .. g.gSx - 1] (g.gChar r)
        it "keeps the captured screen unchanged when the program emits more" $ do
            e <- Emu.newEmulator Size { rows = 4, cols = 10 } 100
            _ <- Emu.feed e "before"
            frozen <- freezeGrid e
            _ <- Emu.feed e "\r\nafter"
            let g = frozenGrid frozen :: Grid Identity
            T.stripEnd (rowText g 0) `shouldBe` "before"
            all (T.null . T.strip . rowText g) [1 .. g.gSy - 1]
                `shouldBe` True
        it "reads live only via a fresh capture" $ do
            e <- Emu.newEmulator Size { rows = 4, cols = 10 } 100
            _ <- Emu.feed e "before"
            _ <- freezeGrid e
            _ <- Emu.feed e "\r\nafter"
            g <- frozenGrid <$> freezeGrid e
            T.stripEnd (rowText (g :: Grid Identity) 1) `shouldBe` "after"

    describe "copy-pipe fd hygiene (withDevNullSink)" $ do
        -- The /dev/null sink a copy-pipe hands its child must be released on
        -- every path. A createProcess that throws would otherwise orphan the
        -- fd — one leaked per copy-pipe on a long-lived server.
        it "closes the handle even when the body throws" $ do
            captured <- newIORef Nothing
            let boom = withDevNullSink $ \h -> do
                    writeIORef captured (Just h)
                    ioError (userError "boom")
            (_ :: Either SomeException ()) <- try boom
            Just h <- readIORef captured
            hIsClosed h `shouldReturn` True
        it "closes the handle on the normal path" $ do
            h <- withDevNullSink pure
            hIsClosed h `shouldReturn` True
