module Hat.Server.ClipboardSpec (spec) where

import Data.ByteString qualified as B
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Model.Options (OptionValue (..), applyEntry, defaultOptions)
import Hat.Server.Clipboard
import Hat.Term.HostProtocol (OscTerm (..), cleanName)

spec :: Spec
spec = do
    describe "base64" $ do
        prop "decoding undoes encoding" $ \bytes ->
            base64Decode (base64Encode (B.pack bytes)) === Just (B.pack bytes)
        it "encodes with padding" $
            base64Encode "Hello" `shouldBe` "SGVsbG8="
        it "refuses what is not base64" $
            base64Decode "S*Vs" `shouldBe` Nothing

    describe "an app's OSC 52 (set-clipboard, get-clipboard)" $ do
        let with sets = foldr (\(n, v) -> applyEntry n (OVScalar v)) defaultOptions sets
            on = with [("set-clipboard", "on")]
        it "set-clipboard external (the default) ignores it" $
            clipboardAction defaultOptions "c;SGVsbG8=" `shouldBe` ClipboardIgnored
        it "set-clipboard on takes the decoded text" $
            clipboardAction on "c;SGVsbG8=" `shouldBe` ClipboardSet "Hello"
        it "a query is answered from the buffers under get-clipboard buffer" $
            clipboardAction on "c;?" `shouldBe` ClipboardAnswer 'c'
        it "get-clipboard off leaves a query unanswered" $
            clipboardAction (with [("set-clipboard", "on"), ("get-clipboard", "off")]) "c;?"
                `shouldBe` ClipboardIgnored
        it "the reply echoes the query's terminator" $ do
            clipboardReply 'c' TermBel "Hello" `shouldBe` "\ESC]52;c;SGVsbG8=\a"
            clipboardReply '\NUL' TermSt "Hello" `shouldBe` "\ESC]52;;SGVsbG8=\ESC\\"
        it "a copy goes out as tmux's Ms" $
            selectionBytes "Hello" `shouldBe` "\ESC]52;;SGVsbG8=\a"

    describe "cleanName (names and titles an app sends)" $ do
        it "refuses invalid UTF-8" $
            cleanName "bad\xc2name" `shouldBe` Nothing
        it "defuses #( but keeps other #" $
            cleanName "title#(bad)#[fg=red]" `shouldBe` Just "title_(bad)#[fg=red]"
        it "makes control characters and backslash visible" $
            cleanName "a\tb\\c\ESC" `shouldBe` Just "a\\tb\\\\c\\033"
