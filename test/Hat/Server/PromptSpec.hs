module Hat.Server.PromptSpec (spec) where

import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Test.Hspec

import Hat.Model (PromptState (..))
import Hat.Server.Keys (tokenizeKeys)
import Hat.Server.Prompt

-- | Feed raw bytes (as a user would type them) through the editor, one
-- tokenized key at a time, stopping early on Submit/Cancel.
typeKeys :: [Text] -> PromptState -> Text -> PromptEdit
typeKeys history st0 raw =
    foldl step (Editing st0) (tokenizeKeys (TE.encodeUtf8 raw))
  where
    step (Editing st) k = editPrompt history st k
    step done _ = done

-- | The buffer after typing, assuming the prompt stays open.
bufferAfter :: Text -> Text
bufferAfter raw = (stateAfter raw).input

-- | The full prompt state after typing, assuming the prompt stays open.
stateAfter :: Text -> PromptState
stateAfter raw = case typeKeys [] emptyPrompt raw of
    Editing st -> st
    other -> error ("prompt closed unexpectedly: " <> show other)

spec :: Spec
spec = do
    describe "typing" $ do
        it "inserts printable characters" $
            bufferAfter "split-window -h" `shouldBe` "split-window -h"

        it "inserts spaces" $
            bufferAfter "a b" `shouldBe` "a b"

    describe "backspace" $ do
        it "deletes the character before the cursor" $
            bufferAfter "abc\DEL" `shouldBe` "ab"

        it "is a no-op on an empty buffer" $
            bufferAfter "\DEL" `shouldBe` ""

        it "deletes mid-line, before the cursor" $
            -- abc, Left, BSpace -> deletes 'b'
            bufferAfter "abc\ESC[D\DEL" `shouldBe` "ac"

    describe "cursor movement" $ do
        it "Left then insert lands before the moved-over char" $
            bufferAfter "ac\ESC[Db" `shouldBe` "abc"

        it "Left is a no-op at the start of the line" $
            stateAfter "\ESC[D" `shouldBe` emptyPrompt

        it "Right is a no-op at the end of the line" $ do
            let st = stateAfter "ab\ESC[C"
            st.input `shouldBe` "ab"
            st.cursor `shouldBe` 2

        it "Home / C-a jump to the start" $ do
            bufferAfter "abc\SOHX" `shouldBe` "Xabc"      -- C-a
            bufferAfter "abc\ESC[HX" `shouldBe` "Xabc"    -- Home

        it "End / C-e jump to the end" $ do
            bufferAfter "abc\SOH\ENQX" `shouldBe` "abcX"  -- C-a then C-e
            bufferAfter "abc\SOH\ESC[FX" `shouldBe` "abcX"

    describe "delete" $ do
        it "Delete / C-d removes the char at the cursor" $ do
            bufferAfter "abc\SOH\EOT" `shouldBe` "bc"     -- C-a then C-d
            bufferAfter "abc\SOH\ESC[3~" `shouldBe` "bc"  -- C-a then Delete

        it "C-d is a no-op at the end of the line" $
            bufferAfter "abc\EOT" `shouldBe` "abc"

    describe "readline editing" $ do
        it "C-b moves left, C-f moves right" $ do
            bufferAfter "ac\STXb" `shouldBe` "abc"          -- C-b then insert
            bufferAfter "ac\SOH\ACKb" `shouldBe` "abc"      -- C-a, C-f, insert

        it "M-f moves to the end of the next word" $
            bufferAfter "foo bar\SOH\ESCfX" `shouldBe` "fooX bar"

        it "M-b moves to the start of the previous word" $
            bufferAfter "foo bar\ESCbX" `shouldBe` "foo Xbar"

        it "C-k kills to the end of the line" $
            bufferAfter "foo bar\SOH\ESCf\v" `shouldBe` "foo"

        it "C-u kills to the start of the line" $
            bufferAfter "foo bar\ESCb\NAK" `shouldBe` "bar"

        it "C-w kills the previous whitespace-delimited word" $
            bufferAfter "foo bar\ETB" `shouldBe` "foo "

        it "M-d kills the next word" $
            bufferAfter "foo bar\SOH\ESCd" `shouldBe` " bar"

        it "C-y yanks the last kill back at the cursor" $ do
            bufferAfter "foo bar\ETB\EM" `shouldBe` "foo bar"       -- kill then yank
            bufferAfter "foo bar\ETB\SOH\EM" `shouldBe` "barfoo "   -- yank elsewhere

    describe "history browsing" $ do
        -- history is most-recent-first.
        let hist = ["second", "first"]
            recall raw = case typeKeys hist emptyPrompt raw of
                Editing st -> st.input
                other -> error (show other)

        it "Up recalls the most recent command" $ do
            let st = case typeKeys hist emptyPrompt "\ESC[A" of
                    Editing s -> s
                    other -> error (show other)
            st.input `shouldBe` "second"
            st.cursor `shouldBe` 6  -- cursor lands at end

        it "repeated Up walks back to older commands" $
            recall "\ESC[A\ESC[A" `shouldBe` "first"

        it "Up stops at the oldest command" $
            recall "\ESC[A\ESC[A\ESC[A" `shouldBe` "first"

        it "Down walks back toward the newest" $
            recall "\ESC[A\ESC[A\ESC[B" `shouldBe` "second"

        it "Down past the newest restores the in-progress line" $
            recall "wip\ESC[A\ESC[B" `shouldBe` "wip"

        it "Down is a no-op when not browsing history" $
            recall "wip\ESC[B" `shouldBe` "wip"

        it "Up is a no-op with empty history" $
            bufferAfter "wip\ESC[A" `shouldBe` "wip"

    describe "pushHistory" $ do
        it "prepends the newest line" $
            pushHistory "c" ["b", "a"] `shouldBe` ["c", "b", "a"]

        it "drops a line identical to the most recent" $
            pushHistory "b" ["b", "a"] `shouldBe` ["b", "a"]

        it "ignores empty lines" $
            pushHistory "   " ["a"] `shouldBe` ["a"]

    describe "submit" $
        it "Enter submits the current line" $
            typeKeys [] emptyPrompt "new-window\r"
                `shouldBe` Submit "new-window"

    describe "cancel" $ do
        it "Escape cancels" $
            typeKeys [] emptyPrompt "abc\ESC" `shouldBe` Cancel

        it "C-c cancels" $
            typeKeys [] emptyPrompt "abc\ETX" `shouldBe` Cancel

    describe "promptFor" $ do
        let st = promptFor "(rename-window) " "editor" "rename-window '%%'"

        it "pre-fills the buffer with the initial text" $ do
            st.input `shouldBe` "editor"
            st.cursor `shouldBe` 6  -- cursor lands at the end

        it "carries the template and prompt label" $ do
            st.template `shouldBe` "rename-window '%%'"
            st.promptLabel `shouldBe` "(rename-window) "

        it "starts unbrowsed, like a fresh prompt" $ do
            st.histIx `shouldBe` Nothing
            st.pending `shouldBe` ""

    describe "applyTemplate" $ do
        it "substitutes %% with the typed line" $
            applyTemplate "rename-window '%%'" "my win"
                `shouldBe` "rename-window 'my win'"

        it "substitutes %1 as an alias for %%" $
            applyTemplate "rename-window '%1'" "logs"
                `shouldBe` "rename-window 'logs'"

        it "runs the typed line verbatim when the template is empty" $
            applyTemplate "" "new-window" `shouldBe` "new-window"
