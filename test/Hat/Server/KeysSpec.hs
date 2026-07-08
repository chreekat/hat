module Hat.Server.KeysSpec (spec) where

import Test.Hspec

import Hat.Server.Keys


spec :: Spec
spec = do
    describe "tokenizeKeys" $ do
        it "reads printable chars" $
            map (.name) (tokenizeKeys "ab!") `shouldBe` ["a", "b", "!"]

        it "reads control chars" $
            map (.name) (tokenizeKeys "\x02\x01") `shouldBe` ["C-b", "C-a"]

        it "reads C-Space" $
            map (.name) (tokenizeKeys "\x00") `shouldBe` ["C-Space"]

        it "reads Enter, Tab, Space, BSpace" $
            map (.name) (tokenizeKeys "\r\t \x7f")
                `shouldBe` ["Enter", "Tab", "Space", "BSpace"]

        it "reads arrow keys (CSI and SS3)" $
            map (.name) (tokenizeKeys "\ESC[A\ESC[B\ESCOC\ESCOD")
                `shouldBe` ["Up", "Down", "Right", "Left"]

        it "reads meta keys" $
            map (.name) (tokenizeKeys "\ESCx\ESCn")
                `shouldBe` ["M-x", "M-n"]

        it "reads meta arrows" $
            map (.name) (tokenizeKeys "\ESC\ESC[A") `shouldBe` ["M-Up"]

        it "treats a lone trailing escape as Escape" $
            map (.name) (tokenizeKeys "a\ESC") `shouldBe` ["a", "Escape"]

        it "keeps raw bytes for passthrough" $
            map (.raw) (tokenizeKeys "a\ESC[A\x02")
                `shouldBe` ["a", "\ESC[A", "\x02"]

        it "reads utf-8 sequences as single keys" $
            map (.name) (tokenizeKeys "\xc3\xa9") `shouldBe` ["é"]

    describe "parseKeyName" $ do
        it "parses plain characters" $ do
            (.name) <$> parseKeyName "x" `shouldBe` Just "x"
            (.name) <$> parseKeyName "%" `shouldBe` Just "%"
        it "parses control keys" $
            (.raw) <$> parseKeyName "C-b" `shouldBe` Just "\x02"
        it "parses C-Space" $
            (.raw) <$> parseKeyName "C-Space" `shouldBe` Just "\x00"
        it "parses meta keys" $
            (.raw) <$> parseKeyName "M-n" `shouldBe` Just "\ESCn"
        it "parses named keys" $ do
            (.raw) <$> parseKeyName "Up" `shouldBe` Just "\ESC[A"
            (.raw) <$> parseKeyName "Enter" `shouldBe` Just "\r"
        it "roundtrips through tokenization" $ do
            let names = ["C-b", "C-Space", "M-x", "Up", "Down", "Space",
                         "Enter", "Tab", "BSpace", "x", "%", "\"", "M-Up"]
            [k.name | Just k0 <- map parseKeyName names
                    , k <- tokenizeKeys k0.raw]
                `shouldBe` names
        it "rejects nonsense" $
            parseKeyName "NotAKey" `shouldBe` Nothing
