module Hat.Server.PromptSpec (spec) where

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
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
bufferAfter raw = case typeKeys [] emptyPrompt raw of
    Editing st -> st.input
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

    describe "submit" $
        it "Enter submits the current line" $
            typeKeys [] emptyPrompt "new-window\r"
                `shouldBe` Submit "new-window"

    describe "cancel" $ do
        it "Escape cancels" $
            typeKeys [] emptyPrompt "abc\ESC" `shouldBe` Cancel

        it "C-c cancels" $
            typeKeys [] emptyPrompt "abc\ETX" `shouldBe` Cancel
