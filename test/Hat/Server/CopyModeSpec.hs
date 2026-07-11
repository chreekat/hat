module Hat.Server.CopyModeSpec (spec) where

import Data.Functor.Identity (Identity, runIdentity)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Hat.Model (CopyModeState (..), SelKind (SelChar))
import Hat.Model.Options (ModeKeys (..), Options (..), defaultOptions)
import Hat.Server.CopyMode

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
