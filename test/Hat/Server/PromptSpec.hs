module Hat.Server.PromptSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Test.Hspec

import Hat.Model (PromptKeys (..), PromptKind (..), PromptMode (..), PromptState (..))
import Hat.Model.Options (ModeKeys (..))
import Hat.Server.Keys (Key (..), tokenizeKeys)
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

        it "deletes mid-line, before the cursor" $
            -- abc, Left, BSpace -> deletes 'b'
            bufferAfter "abc\ESC[D\DEL" `shouldBe` "ac"

    describe "cursor movement" $ do
        it "Left then insert lands before the moved-over char" $
            bufferAfter "ac\ESC[Db" `shouldBe` "abc"

        it "Home / C-a jump to the start" $ do
            bufferAfter "abc\SOHX" `shouldBe` "Xabc"      -- C-a
            bufferAfter "abc\ESC[HX" `shouldBe` "Xabc"    -- Home

        it "End / C-e jump to the end" $ do
            bufferAfter "abc\SOH\ENQX" `shouldBe` "abcX"  -- C-a then C-e
            bufferAfter "abc\SOH\ESC[FX" `shouldBe` "abcX"

    describe "delete" $
        it "Delete / C-d removes the char at the cursor" $ do
            bufferAfter "abc\SOH\EOT" `shouldBe` "bc"     -- C-a then C-d
            bufferAfter "abc\SOH\ESC[3~" `shouldBe` "bc"  -- C-a then Delete

    describe "no-op keys" $ do
        -- each edge-case key must leave the full prompt state untouched
        let typedState hist raw = case typeKeys hist emptyPrompt raw of
                Editing st -> st
                other -> error ("prompt closed unexpectedly: " <> show other)
            rows :: [(String, [Text], Text, Text)]
            rows =
                [ ("backspace on an empty buffer", [], "", "\DEL")
                , ("Left at the start of the line", [], "", "\ESC[D")
                , ("Right at the end of the line", [], "ab", "\ESC[C")
                , ("C-d at the end of the line", [], "abc", "\EOT")
                , ("Down when not browsing history", ["prior"], "wip", "\ESC[B")
                , ("Up with empty history", [], "wip", "\ESC[A")
                ]
        forM_ rows $ \(name, hist, prefix, key) ->
            it (name <> " is a no-op") $
                typedState hist (prefix <> key) `shouldBe` typedState hist prefix

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

    describe "pushHistory" $ do
        it "prepends the newest line" $
            pushHistory 100 "c" ["b", "a"] `shouldBe` ["c", "b", "a"]

        it "drops a line identical to the most recent" $
            pushHistory 100 "b" ["b", "a"] `shouldBe` ["b", "a"]

        it "ignores empty lines" $
            pushHistory 100 "   " ["a"] `shouldBe` ["a"]

        it "keeps at most prompt-history-limit lines, dropping the oldest" $ do
            pushHistory 2 "c" ["b", "a"] `shouldBe` ["c", "b"]
            pushHistory 0 "c" [] `shouldBe` []

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

    describe "templateReplace" $ do
        it "escapes a single quote for %%, which sits inside single quotes" $
            templateReplace "set @r '%%'" "can't ; x" 1
                `shouldBe` "set @r 'can'\\''t ; x'"

        it "replaces only the first %%" $
            templateReplace "a '%%' ; b '%%'" "v" 1 `shouldBe` "a 'v' ; b '%%'"

        it "backslash-escapes the double-quote specials for %%%" $
            templateReplace "set @r \"%%%\"" "a\"$;~\\z" 1
                `shouldBe` "set @r \"a\\\"\\$\\;\\~\\\\z\""

        it "replaces every %N of its own index raw" $
            templateReplace "x %1 ; y %1 %2" "v ; w" 1 `shouldBe` "x v ; w ; y v ; w %2"

        it "backslash-escapes the double-quote specials for %N%" $
            templateReplace "\"%2%\"" "b\"$" 2 `shouldBe` "\"b\\\"\\$\""

        it "leaves a lone % alone" $
            templateReplace "50% %x" "v" 1 `shouldBe` "50% %x"

    describe "applyTemplate" $ do
        it "runs the typed line verbatim when the template is empty" $
            applyTemplate "" ["new-window"] `shouldBe` "new-window"

        it "gives each answer its own index, and the next %% to each" $
            applyTemplate "a %1 %2 '%%' '%%'" ["x", "y"] `shouldBe` "a x y 'x' 'y'"

    describe "answerPrompt" $ do
        let two = (promptFor "one " "" "set @a %1 ; set @b %2")
                { queued = [("two ", "init")] }

        it "asks the queued prompt next, with its own label and input" $
            case answerPrompt two "x" of
                NextPrompt nxt -> do
                    nxt.promptLabel `shouldBe` "two "
                    nxt.input `shouldBe` "init"
                    nxt.queued `shouldBe` []
                other -> expectationFailure ("expected NextPrompt: " <> show other)

        it "splices every answer once the last prompt is answered" $
            case answerPrompt two "x" of
                NextPrompt nxt ->
                    answerPrompt nxt "y" `shouldBe` AnswersDone "set @a x ; set @b y"
                other -> expectationFailure ("expected NextPrompt: " <> show other)

    describe "a key prompt" $ do
        it "takes the next key, by name" $
            editPrompt [] (promptFor "" "" "") { promptKind = KeyEntry }
                Key { name = "C-M-a", raw = "\ESC\x01" }
                `shouldBe` Submit "C-M-a"
        it "asks its queued prompts for keys too" $
            case answerPrompt (promptFor "" "" "") { promptKind = KeyEntry, queued = [("two ", "")] } "a" of
                NextPrompt nxt -> nxt.promptKind `shouldBe` KeyEntry
                other -> expectationFailure ("expected NextPrompt: " <> show other)

    describe "status-keys vi" $ do
        let viPrompt = emptyPrompt
                { promptKeys = emptyPrompt.promptKeys
                    { statusKeys = KeysVi, wordSeparators = "-" } }
            -- Each chunk is its own keystroke batch, so a lone ESC is Escape.
            typeVi chunks = foldl step (Editing viPrompt) chunks
              where
                step (Editing st) raw = foldl step1 (Editing st)
                    (tokenizeKeys (TE.encodeUtf8 raw))
                step done _ = done
                step1 (Editing st) k = editPrompt [] st k
                step1 done _ = done
            viState chunks = case typeVi chunks of
                Editing st -> st
                other -> error ("prompt closed unexpectedly: " <> show other)
            buf = (.input) . viState
            at = (.cursor) . viState

        it "types into insert mode" $
            buf ["abc"] `shouldBe` "abc"
        it "Escape enters command mode, backing the cursor up one" $ do
            (viState ["abc", "\ESC"]).promptMode `shouldBe` PromptCommand
            at ["abc", "\ESC"] `shouldBe` 2
        it "command-mode letters move and edit, not insert" $ do
            at ["abc", "\ESC", "h"] `shouldBe` 1
            at ["abc", "\ESC", "0"] `shouldBe` 0
            at ["abc", "\ESC", "0", "$"] `shouldBe` 3
            buf ["abc", "\ESC", "x"] `shouldBe` "ab"
            buf ["abc", "\ESC", "0", "D"] `shouldBe` ""
            buf ["abc", "\ESC", "d"] `shouldBe` ""
        it "i, a, A and I return to insert mode at their places" $ do
            buf ["abc", "\ESC", "i", "Z"] `shouldBe` "abZc"
            buf ["abc", "\ESC", "0", "a", "Z"] `shouldBe` "aZbc"
            buf ["abc", "\ESC", "0", "A", "Z"] `shouldBe` "abcZ"
            buf ["abc", "\ESC", "I", "Z"] `shouldBe` "Zabc"
            buf ["abc", "\ESC", "S", "Z"] `shouldBe` "Z"
        it "w, b and e move by words, stopping at separators" $ do
            at ["foo-bar baz", "\ESC", "0", "w"] `shouldBe` 3
            at ["foo-bar baz", "\ESC", "0", "W"] `shouldBe` 8
            at ["foo-bar baz", "\ESC", "b"] `shouldBe` 8
            at ["foo-bar baz", "\ESC", "0", "e"] `shouldBe` 2
        it "Enter submits and q cancels from command mode" $ do
            typeVi ["abc", "\ESC", "\r"] `shouldBe` Submit "abc"
            typeVi ["abc", "\ESC", "q"] `shouldBe` Cancel
        it "emacs-only keys do nothing in insert mode" $
            buf ["abc", "\STX", "Z"] `shouldBe` "abcZ"
