module Hat.Server.KeysSpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict as Map

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

        it "reads ctrl arrows (CSI modifier 5)" $
            map (.name) (tokenizeKeys "\ESC[1;5A\ESC[1;5B\ESC[1;5C\ESC[1;5D")
                `shouldBe` ["C-Up", "C-Down", "C-Right", "C-Left"]

        it "keeps raw bytes for ctrl arrows" $
            map (.raw) (tokenizeKeys "\ESC[1;5B")
                `shouldBe` ["\ESC[1;5B"]

        it "treats a lone trailing escape as Escape" $
            map (.name) (tokenizeKeys "a\ESC") `shouldBe` ["a", "Escape"]

        it "keeps raw bytes for passthrough" $
            map (.raw) (tokenizeKeys "a\ESC[A\x02")
                `shouldBe` ["a", "\ESC[A", "\x02"]

        it "reads utf-8 sequences as single keys" $
            map (.name) (tokenizeKeys "\xc3\xa9") `shouldBe` ["é"]

    describe "routeKeys" $ do
        let km = Map.fromList
                [ ("prefix", Map.fromList
                    [ ("d", [["detach-client"]])
                    , ("C-Space", [["send-prefix"]])
                    ])
                , ("root", Map.fromList
                    [ ("M-Up", [["resize-pane", "-U"]]) ])
                ]
            run st bs = routeKeys "C-Space" km Nothing st (tokenizeKeys bs)
        it "passes plain input through, coalesced" $
            run NoPrefix "hello" `shouldBe` (NoPrefix, [Passthrough "hello"])
        it "arms on prefix and runs the bound command" $
            run NoPrefix "\x00\&d" `shouldBe`
                (NoPrefix, [RunCommands [["detach-client"]]])
        it "holds the armed state across chunks" $ do
            run NoPrefix "\x00" `shouldBe` (PrefixArmed, [])
            run PrefixArmed "d" `shouldBe`
                (NoPrefix, [RunCommands [["detach-client"]]])
        it "runs root-table bindings without the prefix" $
            run NoPrefix "\ESC\ESC[A" `shouldBe`
                (NoPrefix, [RunCommands [["resize-pane", "-U"]]])
        it "swallows unbound prefixed keys" $
            run NoPrefix "\x00q after" `shouldBe`
                (NoPrefix, [Passthrough " after"])

    describe "routeKeys in copy mode" $ do
        let km = Map.fromList
                [ ("prefix", Map.fromList
                    [ ("]", [["paste-buffer"]]) ])
                , ("root", Map.fromList
                    [ ("M-Up", [["resize-pane", "-U"]]) ])
                , ("copy-mode-vi", Map.fromList
                    [ ("h", [["send-keys", "-X", "cursor-left"]])
                    , ("q", [["send-keys", "-X", "cancel"]])
                    ])
                ]
            run st bs =
                routeKeys "C-Space" km (Just "copy-mode-vi") st (tokenizeKeys bs)
        it "runs bindings from the pane's copy-mode table" $
            run NoPrefix "h" `shouldBe`
                (NoPrefix, [RunCommands [["send-keys", "-X", "cursor-left"]]])
        it "swallows keys not bound in the copy-mode table" $
            run NoPrefix "xyz" `shouldBe` (NoPrefix, [])
        it "arms the prefix in copy mode so prefix-table commands still run" $
            -- bug 83: prefix ] pastes even while in copy mode, rather than
            -- being swallowed by the mode.
            run NoPrefix "\x00]" `shouldBe`
                (NoPrefix, [RunCommands [["paste-buffer"]]])
        it "holds the armed state across chunks while in copy mode" $ do
            run NoPrefix "\x00" `shouldBe` (PrefixArmed, [])
            run PrefixArmed "]" `shouldBe`
                (NoPrefix, [RunCommands [["paste-buffer"]]])
        it "runs a copy-mode binding on the prefix key rather than arming" $ do
            let kmp = Map.insert "copy-mode-vi"
                    (Map.fromList [("C-Space", [["send-keys", "-X", "top-line"]])])
                    km
            routeKeys "C-Space" kmp (Just "copy-mode-vi") NoPrefix
                (tokenizeKeys "\x00")
                `shouldBe` (NoPrefix, [RunCommands [["send-keys", "-X", "top-line"]]])
        it "does not fall through to root bindings while in mode" $
            run NoPrefix "\ESC\ESC[A" `shouldBe` (NoPrefix, [])

    describe "parseKeyName" $ do
        it "parses plain characters" $ do
            (.name) <$> parseKeyName "x" `shouldBe` Just "x"
            (.name) <$> parseKeyName "%" `shouldBe` Just "%"
        it "parses control keys" $
            (.raw) <$> parseKeyName "C-b" `shouldBe` Just "\x02"
        it "parses C-Space" $
            (.raw) <$> parseKeyName "C-Space" `shouldBe` Just "\x00"
        it "parses ctrl arrows" $ do
            (.name) <$> parseKeyName "C-Down" `shouldBe` Just "C-Down"
            (.raw) <$> parseKeyName "C-Down" `shouldBe` Just "\ESC[1;5B"
            (.raw) <$> parseKeyName "C-Up" `shouldBe` Just "\ESC[1;5A"
            (.raw) <$> parseKeyName "C-Right" `shouldBe` Just "\ESC[1;5C"
            (.raw) <$> parseKeyName "C-Left" `shouldBe` Just "\ESC[1;5D"
        it "parses ctrl arrows case-insensitively" $
            (.name) <$> parseKeyName "C-down" `shouldBe` Just "C-Down"
        it "parses meta keys" $
            (.raw) <$> parseKeyName "M-n" `shouldBe` Just "\ESCn"
        it "parses named keys" $ do
            (.raw) <$> parseKeyName "Up" `shouldBe` Just "\ESC[A"
            (.raw) <$> parseKeyName "Enter" `shouldBe` Just "\r"
        it "roundtrips through tokenization" $ do
            let names = ["C-b", "C-Space", "M-x", "Up", "Down", "Space",
                         "Enter", "Tab", "BSpace", "x", "%", "\"", "M-Up",
                         "C-Up", "C-Down", "C-Left", "C-Right"]
            [k.name | Just k0 <- map parseKeyName names
                    , k <- tokenizeKeys k0.raw]
                `shouldBe` names
        it "rejects nonsense" $
            parseKeyName "NotAKey" `shouldBe` Nothing
