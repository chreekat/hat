module Hat.Server.KeysSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Hat.Model.OptionTable (ArrayKey (..))
import Hat.Server.Keys

-- A key an outer terminal can spell as an extended-key sequence: a base
-- codepoint hat can name, under an xterm modifier parameter (1 + bitmask).
data ExtKey = ExtKey Int Int
    deriving (Show)

instance Arbitrary ExtKey where
    arbitrary = ExtKey <$> elements extBases <*> ((1 +) <$> choose (1, 7))
    shrink (ExtKey c m) =
        [ ExtKey c' m | c' <- takeWhile (/= c) extBases ]
        ++ [ ExtKey c m' | m' <- [2 .. m - 1] ]

extBases :: [Int]
-- Uppercase letters are absent: an extended key carries the UNSHIFTED
-- codepoint, and Shift rides the modifier mask.
extBases = [13, 9, 27, 32, 127] ++ [0x21 .. 0x40] ++ [0x5b .. 0x7e]

-- The two wire spellings of one extended key: xterm's modifyOtherKeys form
-- and the CSI-u form.
extForms :: ExtKey -> [B.ByteString]
extForms (ExtKey c m) =
    [ B8.pack ("\ESC[27;" <> show m <> ";" <> show c <> "~")
    , B8.pack ("\ESC[" <> show c <> ";" <> show m <> "u") ]


spec :: Spec
spec = do
    describe "tokenizeKeys" $ do
        it "reads printable chars" $
            map (.name) (tokenizeKeys "ab!") `shouldBe` ["a", "b", "!"]

        it "reads control chars" $
            map (.name) (tokenizeKeys "\x02\x01") `shouldBe` ["C-b", "C-a"]

        it "names 0x1c-0x1f as ctrl punctuation, not chars past z (1b)" $ do
            map (.name) (tokenizeKeys "\x1c\x1d\x1e\x1f")
                `shouldBe` ["C-\\", "C-]", "C-^", "C-_"]
            map (.raw) (tokenizeKeys "\x1c\x1d\x1e\x1f")
                `shouldBe` ["\x1c", "\x1d", "\x1e", "\x1f"]

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

        it "reads alt arrows (CSI modifier 3) as M-arrows" $
            -- bug 149: a terminal-sent CSI alt-arrow must match M-arrow bindings.
            map (.name) (tokenizeKeys "\ESC[1;3A\ESC[1;3B\ESC[1;3C\ESC[1;3D")
                `shouldBe` ["M-Up", "M-Down", "M-Right", "M-Left"]

        it "reads every xterm modifier param, prefixes in tmux's C-M-S- order" $
            map (.name) (tokenizeKeys
                "\ESC[1;2A\ESC[1;4B\ESC[1;6C\ESC[1;7D\ESC[1;8A\ESC[1;3H\ESC[1;3F")
                `shouldBe` ["S-Up", "M-S-Down", "C-S-Right", "C-M-Left",
                            "C-M-S-Up", "M-Home", "M-End"]

        it "keeps raw bytes for modified arrows" $
            map (.raw) (tokenizeKeys "\ESC[1;3A") `shouldBe` ["\ESC[1;3A"]

        it "reads F1-F4 in both encodings (SS3 and legacy CSI)" $ do
            -- bug fad
            map (.name) (tokenizeKeys "\ESCOP\ESCOQ\ESCOR\ESCOS")
                `shouldBe` ["F1", "F2", "F3", "F4"]
            map (.name) (tokenizeKeys "\ESC[11~\ESC[12~\ESC[13~\ESC[14~")
                `shouldBe` ["F1", "F2", "F3", "F4"]

        it "reads F5-F12 (CSI tilde codes)" $
            -- bug fad
            map (.name) (tokenizeKeys
                "\ESC[15~\ESC[17~\ESC[18~\ESC[19~\ESC[20~\ESC[21~\ESC[23~\ESC[24~")
                `shouldBe` ["F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12"]

        it "reads Insert and BTab (Shift-Tab)" $
            -- bug fad
            map (.name) (tokenizeKeys "\ESC[2~\ESC[Z")
                `shouldBe` ["Insert", "BTab"]

        it "reads modified function keys and specials" $
            -- bug fad
            map (.name) (tokenizeKeys
                "\ESC[1;2P\ESC[15;5~\ESC[2;3~\ESC[3;2~\ESC[5;5~\ESC[6;4~")
                `shouldBe` ["S-F1", "C-F5", "M-Insert", "S-Delete",
                            "C-PgUp", "M-S-PgDn"]

        it "swallows a genuinely unknown CSI whole, raw bytes kept" $ do
            map (.name) (tokenizeKeys "\ESC[97~x") `shouldBe` ["Unknown", "x"]
            map (.raw) (tokenizeKeys "\ESC[97~x") `shouldBe` ["\ESC[97~", "x"]

        it "treats a lone trailing escape as Escape" $
            map (.name) (tokenizeKeys "a\ESC") `shouldBe` ["a", "Escape"]

        it "keeps raw bytes for passthrough" $
            map (.raw) (tokenizeKeys "a\ESC[A\x02")
                `shouldBe` ["a", "\ESC[A", "\x02"]

        it "reads utf-8 sequences as single keys" $
            map (.name) (tokenizeKeys "\xc3\xa9") `shouldBe` ["é"]

        it "reads the keypad in application mode" $
            map (.name) (tokenizeKeys (B.concat
                [ "\ESCO" <> B8.singleton c | c <- "Mjkmnopqrstuvwxy" ]))
                `shouldBe` [ "KPEnter", "KP*", "KP+", "KP-", "KP.", "KP/", "KP0"
                           , "KP1", "KP2", "KP3", "KP4", "KP5", "KP6", "KP7", "KP8"
                           , "KP9" ]

        it "reads rxvt's modified arrows" $
            map (.name) (tokenizeKeys "\ESCOa\ESCOb\ESCOc\ESCOd\ESC[a\ESC[b\ESC[c\ESC[d")
                `shouldBe` [ "C-Up", "C-Down", "C-Right", "C-Left"
                           , "S-Up", "S-Down", "S-Right", "S-Left" ]

        it "reads rxvt's modified function keys" $ do
            map (.name) (tokenizeKeys
                "\ESC[25~\ESC[26~\ESC[28~\ESC[29~\ESC[31~\ESC[32~\ESC[33~\ESC[34~\ESC[23$\ESC[24$")
                `shouldBe` [ "S-F3", "S-F4", "S-F5", "S-F6", "S-F7", "S-F8", "S-F9"
                           , "S-F10", "S-F11", "S-F12" ]
            map (.name) (tokenizeKeys "\ESC[11^\ESC[17^\ESC[24^\ESC[11@\ESC[21@\ESC[24@")
                `shouldBe` ["C-F1", "C-F6", "C-F12", "C-S-F1", "C-S-F10", "C-S-F12"]

        it "reads C-S-Tab" $
            map (.name) (tokenizeKeys "\ESC[1;5Z") `shouldBe` ["C-S-Tab"]

        it "names an escaped key in tmux's C-M-S- order" $
            map (.name) (tokenizeKeys "\ESC\x01\ESC\x7f\ESC\ESCOM\ESC\ESC[a")
                `shouldBe` ["C-M-a", "M-BSpace", "M-KPEnter", "M-S-Up"]

        it "reads an escaped Escape as M-Escape" $
            map (.name) (tokenizeKeys "\ESC\ESC") `shouldBe` ["M-Escape"]

        it "reads a sequence cut short as its first byte escaped, then the rest" $ do
            map (.name) (tokenizeKeys "\ESC[") `shouldBe` ["M-["]
            map (.name) (tokenizeKeys "\ESCO") `shouldBe` ["M-O"]
            map (.name) (tokenizeKeys "\ESC[1;") `shouldBe` ["M-[", "1", ";"]

    -- bug 64: an outer terminal spells a combo with no legacy byte as an
    -- extended-key sequence. Both spellings must land on the same canonical
    -- name as the raw-byte twin, and neither may carry its own bytes into a
    -- pane.
    describe "tokenizeKeys (extended keys)" $ do
        it "names xterm modifyOtherKeys sequences" $
            map (.name) (tokenizeKeys
                "\ESC[27;5;115~\ESC[27;6;115~\ESC[27;5;13~\ESC[27;2;13~\ESC[27;3;120~")
                `shouldBe` ["C-s", "C-S-s", "C-Enter", "S-Enter", "M-x"]

        it "names CSI-u sequences" $
            map (.name) (tokenizeKeys
                "\ESC[115;5u\ESC[13;5u\ESC[9;5u\ESC[32;5u\ESC[127;5u\ESC[27u")
                `shouldBe` ["C-s", "C-Enter", "C-Tab", "C-Space", "C-BSpace",
                            "Escape"]

        it "encodes a kitty sequence from its base codepoint, subparams ignored" $
            map (.name) (tokenizeKeys "\ESC[115:83;5:1u") `shouldBe` ["C-s"]

        it "gives an extended key the same name as its raw-byte twin" $ do
            let twins =
                    [ ("\ESC[27;5;115~", "\x13"), ("\ESC[27;3;120~", "\ESCx")
                    , ("\ESC[32;5u", "\NUL") ]
            [ (map (.name) (tokenizeKeys a), map (.name) (tokenizeKeys b))
                | (a, b) <- twins ]
                `shouldSatisfy` all (uncurry (==))

        it "carries the legacy bytes a protocol-less pane expects" $
            map (.raw) (tokenizeKeys
                "\ESC[27;5;115~\ESC[27;5;13~\ESC[27;2;13~\ESC[27;3;120~\ESC[32;5u")
                `shouldBe` ["\x13", "\r", "\r", "\ESCx", "\NUL"]

        prop "never forwards extended-key bytes, in either spelling" $ \k ->
            conjoin
                [ counterexample (show (bs, ks)) $
                    length ks === 1 .&&. all (not . isExtended . (.raw)) ks
                | bs <- extForms k, let ks = tokenizeKeys bs ]

        prop "names both spellings of a key identically" $ \k ->
            case map (map (.name) . tokenizeKeys) (extForms k) of
                [mok, csiu] -> mok === csiu
                _           -> property False

        it "leaves a sequence it cannot name unknown" $ do
            -- kitty's functional-key codepoints have no tmux name.
            map (.name) (tokenizeKeys "\ESC[57399;5u") `shouldBe` ["Unknown"]
            map (.name) (tokenizeKeys "\ESC[97~") `shouldBe` ["Unknown"]

    describe "extendedKeyCode" $ do
        it "reads the codepoint and modifier mask off a modified name" $ do
            extendedKeyCode "C-s" `shouldBe` Just (115, 5)
            extendedKeyCode "C-S-s" `shouldBe` Just (115, 6)
            extendedKeyCode "C-Enter" `shouldBe` Just (13, 5)
            extendedKeyCode "M-x" `shouldBe` Just (120, 3)
            extendedKeyCode "C-Space" `shouldBe` Just (32, 5)
            extendedKeyCode "C-BSpace" `shouldBe` Just (127, 5)
        it "declines names with no modifier or no codepoint base" $ do
            extendedKeyCode "s" `shouldBe` Nothing
            extendedKeyCode "Enter" `shouldBe` Nothing
            extendedKeyCode "C-Up" `shouldBe` Nothing
            extendedKeyCode "S-F1" `shouldBe` Nothing
        prop "round-trips the name a tokenized extended key gets" $ \k@(ExtKey c m) ->
            case extForms k of
                (mok : _) | [key] <- tokenizeKeys mok ->
                    extendedKeyCode key.name === Just (c, m)
                _ -> property False

    -- escape-time coalescing core: with escape-time 0 (EscImmediate) a lone
    -- trailing ESC is Escape at once; with escape-time > 0 (EscBuffered) it is
    -- held, then either coalesced with the next bytes or flushed on timeout.
    describe "feedKeys / flushEscape (escape-time)" $ do
        it "EscImmediate: a lone trailing ESC is Escape now, nothing held" $
            feedKeys EscImmediate [] NoEscPending "a\ESC"
                `shouldBe` EscTokens
                    { escKeys = tokenizeKeys "a\ESC", escPending = NoEscPending }

        it "EscBuffered: a lone trailing ESC is held, not emitted" $
            feedKeys EscBuffered [] NoEscPending "a\ESC"
                `shouldBe` EscTokens
                    { escKeys = [Key { name = "a", raw = "a" }]
                    , escPending = EscPending "\ESC" }

        it "EscBuffered: a sequence cut short is held whole" $ do
            let held = feedKeys EscBuffered [] NoEscPending "\ESC[1;"
            held.escPending `shouldBe` EscPending "\ESC[1;"
            map (.name) (feedKeys EscBuffered [] held.escPending "5A").escKeys
                `shouldBe` ["C-Up"]
            map (.name) (flushEscape held.escPending) `shouldBe` ["M-[", "1", ";"]
            map (.name) (flushEscape (feedKeys EscBuffered [] NoEscPending "\ESC\ESC").escPending)
                `shouldBe` ["M-Escape"]

        it "EscBuffered: a held ESC coalesces with a following [A into Up" $ do
            let held = feedKeys EscBuffered [] NoEscPending "\ESC"
            held.escPending `shouldBe` EscPending "\ESC"
            map (.name) (feedKeys EscBuffered [] held.escPending "[A").escKeys
                `shouldBe` ["Up"]

        it "EscBuffered: a held ESC coalesces with a following x into M-x" $ do
            let held = feedKeys EscBuffered [] NoEscPending "\ESC"
            map (.name) (feedKeys EscBuffered [] held.escPending "x").escKeys
                `shouldBe` ["M-x"]

        it "EscBuffered: bytes with no trailing ESC hold nothing" $
            feedKeys EscBuffered [] NoEscPending "ab"
                `shouldBe` EscTokens
                    { escKeys = tokenizeKeys "ab", escPending = NoEscPending }

        it "flushEscape emits the held ESC as Escape on timeout" $
            map (.name) (flushEscape (EscPending "\ESC")) `shouldBe` ["Escape"]

        it "flushEscape emits nothing when no ESC is held" $
            flushEscape NoEscPending `shouldBe` []

    describe "user-keys" $ do
        let users = userKeys (Map.fromList
                [ (KeyIndex 0, "\ESC[5;30012~"), (KeyIndex 3, "x"), (KeyIndex 4, "") ])
            names bs = map (.name) (feedKeys EscImmediate users NoEscPending bs).escKeys
        it "a user sequence is the User key of its array index" $
            names "a\ESC[5;30012~b" `shouldBe` ["a", "User0", "b"]
        it "a user sequence wins over the key its bytes would spell" $
            names "xy" `shouldBe` ["User3", "y"]
        it "User keys are bindable names, up to User1000" $ do
            (.name) <$> parseKeyName "User0" `shouldBe` Just "User0"
            (.name) <$> parseKeyName "User1000" `shouldBe` Just "User1000"
            parseKeyName "User1001" `shouldBe` Nothing

    describe "backspaceBytes" $ do
        it "the default C-? sends DEL" $
            backspaceBytes "C-?" `shouldBe` Just "\DEL"
        it "a control key sends its control byte" $
            backspaceBytes "C-h" `shouldBe` Just "\b"
        it "a plain key sends itself" $
            backspaceBytes "x" `shouldBe` Just "x"
        it "a key with other modifiers sends nothing" $
            backspaceBytes "M-x" `shouldBe` Nothing

    describe "routeKeys" $ do
        let km = Map.fromList
                [ ("prefix", Map.fromList
                    [ ("d", bindOnce [["detach-client"]])
                    , ("C-Space", bindOnce [["send-prefix"]])
                    , ("Up", bindRepeat [["select-pane", "-U"]])
                    , ("Down", bindRepeat [["select-pane", "-D"]])
                    ])
                , ("root", Map.fromList
                    [ ("M-Up", bindOnce [["resize-pane", "-U"]]) ])
                , ("custom", Map.fromList
                    [ ("x", bindOnce [["kill-pane"]]) ])
                ]
            run st bs = tableOf (routeKeys ctx0 km st (tokenizeKeys bs))
        it "passes plain input through, coalesced" $
            run initialKeyState "hello" `shouldBe` (Nothing, [Passthrough "hello"])
        it "arms on prefix and runs the bound command" $
            run initialKeyState "\x00\&d" `shouldBe`
                (Nothing, [RunCommands [["detach-client"]]])
        it "holds the armed state across chunks" $ do
            run initialKeyState "\x00" `shouldBe` (Just "prefix", [])
            run armed "d" `shouldBe` (Nothing, [RunCommands [["detach-client"]]])
        it "runs root-table bindings without the prefix" $
            run initialKeyState "\ESC\ESC[A" `shouldBe`
                (Nothing, [RunCommands [["resize-pane", "-U"]]])
        it "fires a root-table M-Up binding on the CSI alt-arrow" $
            -- bug 149
            run initialKeyState "\ESC[1;3A" `shouldBe`
                (Nothing, [RunCommands [["resize-pane", "-U"]]])
        it "passes an unbound modified arrow through raw" $
            run initialKeyState "\ESC[1;2A" `shouldBe`
                (Nothing, [Passthrough "\ESC[1;2A"])
        it "swallows unbound prefixed keys" $
            run initialKeyState "\x00q after" `shouldBe`
                (Nothing, [Passthrough " after"])
        it "the prefix key twice runs the prefix table's binding for it" $
            run initialKeyState "\x00\x00" `shouldBe`
                (Nothing, [RunCommands [["send-prefix"]]])

        describe "prefix2" $ do
            let ctx2 = ctx0 { prefixKeys = ["C-Space", "C-a"] }
            it "either prefix key arms the prefix table" $ do
                tableOf (routeKeys ctx2 km initialKeyState (tokenizeKeys "\x01\&d"))
                    `shouldBe` (Nothing, [RunCommands [["detach-client"]]])
                tableOf (routeKeys ctx2 km initialKeyState (tokenizeKeys "\x00\&d"))
                    `shouldBe` (Nothing, [RunCommands [["detach-client"]]])
            it "with prefix None, the old prefix key is just a key" $
                tableOf (routeKeys ctx0 { prefixKeys = [] } km initialKeyState
                    (tokenizeKeys "\x00\&d"))
                    `shouldBe` (Nothing, [Passthrough "\x00\&d"])

        describe "key-table" $ do
            let custom = ctx0 { defaultTable = "custom" }
            it "a key bound in the default table runs" $
                tableOf (routeKeys custom km initialKeyState (tokenizeKeys "x"))
                    `shouldBe` (Nothing, [RunCommands [["kill-pane"]]])
            it "a key unbound there reaches the pane" $
                tableOf (routeKeys custom km initialKeyState (tokenizeKeys "y"))
                    `shouldBe` (Nothing, [Passthrough "y"])
            it "root-table bindings no longer apply" $
                tableOf (routeKeys custom km initialKeyState (tokenizeKeys "\ESC[1;3A"))
                    `shouldBe` (Nothing, [Passthrough "\ESC[1;3A"])

        describe "repeat-time" $ do
            let at t = ctx0 { now = t }
                step t st bs = routeKeys (at t) km st (tokenizeKeys bs)
                (afterUp, upActs) = step 0 initialKeyState "\x00\ESC[A"
            it "a repeating binding keeps the prefix table open" $ do
                upActs `shouldBe` [RunCommands [["select-pane", "-U"]]]
                afterUp.table `shouldBe` Just "prefix"
            it "within repeat-time another repeating key runs without the prefix" $
                snd (step 400 afterUp "\ESC[B")
                    `shouldBe` [RunCommands [["select-pane", "-D"]]]
            it "each repeat restarts the window" $ do
                let (upAgain, _) = step 400 afterUp "\ESC[A"
                snd (step 800 upAgain "\ESC[B")
                    `shouldBe` [RunCommands [["select-pane", "-D"]]]
            it "after repeat-time the key goes to the pane" $
                tableOf (step 600 afterUp "\ESC[B")
                    `shouldBe` (Nothing, [Passthrough "\ESC[B"])
            it "a non-repeating key ends the repeat and runs from the root table" $
                tableOf (step 100 afterUp "d")
                    `shouldBe` (Nothing, [Passthrough "d"])
            it "expireRepeat drops the client back to its default table" $ do
                (expireRepeat 499 afterUp).table `shouldBe` Just "prefix"
                (expireRepeat 500 afterUp).table `shouldBe` Nothing
            it "0 turns repeating off" $
                (fst (routeKeys ctx0 { repeatTime = 0 } km initialKeyState
                    (tokenizeKeys "\x00\ESC[A"))).table `shouldBe` Nothing
            it "initial-repeat-time opens the window; repeat-time extends it" $ do
                let ctxI t = ctx0 { initialRepeatTime = 100, now = t }
                    (st1, _) = routeKeys (ctxI 0) km initialKeyState (tokenizeKeys "\x00\ESC[A")
                (expireRepeat 100 st1).table `shouldBe` Nothing
                let (st2, _) = routeKeys (ctxI 50) km st1 (tokenizeKeys "\ESC[A")
                (expireRepeat 549 st2).table `shouldBe` Just "prefix"

        describe "prefix-timeout" $ do
            let timed t = ctx0 { prefixTimeout = 1000, now = t }
                (armedAt0, _) = routeKeys (timed 0) km initialKeyState (tokenizeKeys "\x00")
            it "a key within the timeout runs from the prefix table" $
                snd (routeKeys (timed 1000) km armedAt0 (tokenizeKeys "d"))
                    `shouldBe` [RunCommands [["detach-client"]]]
            it "a key after it goes to the pane" $
                tableOf (routeKeys (timed 1001) km armedAt0 (tokenizeKeys "d"))
                    `shouldBe` (Nothing, [Passthrough "d"])

        describe "bracketed paste" $ do
            it "pasted keys bypass the tables, brackets included" $
                tableOf (routeKeys ctx0 km initialKeyState
                    (tokenizeKeys "\ESC[200~\x00\&d\ESC[201~d"))
                    `shouldBe`
                        (Nothing, [Paste, Paste, Paste, Paste, Passthrough "d"])
            it "a paste spanning chunks stays a paste" $ do
                let (st1, _) = routeKeys ctx0 km initialKeyState (tokenizeKeys "\ESC[200~ab")
                snd (routeKeys ctx0 km st1 (tokenizeKeys "\x00\ESC[201~"))
                    `shouldBe` [Paste, Paste]

    describe "routeKeys in copy mode" $ do
        let km = Map.fromList
                [ ("prefix", Map.fromList
                    [ ("]", bindOnce [["paste-buffer"]]) ])
                , ("root", Map.fromList
                    [ ("M-Up", bindOnce [["resize-pane", "-U"]]) ])
                , ("copy-mode-vi", Map.fromList
                    [ ("h", bindOnce [["send-keys", "-X", "cursor-left"]])
                    , ("q", bindOnce [["send-keys", "-X", "cancel"]])
                    , ("C-Space", bindOnce [["send-keys", "-X", "top-line"]])
                    ])
                ]
            inMode = ctx0 { modeTable = Just "copy-mode-vi" }
            run st bs = tableOf (routeKeys inMode km st (tokenizeKeys bs))
        it "runs bindings from the pane's copy-mode table" $
            run initialKeyState "h" `shouldBe`
                (Nothing, [RunCommands [["send-keys", "-X", "cursor-left"]]])
        it "swallows keys not bound in the copy-mode table" $
            run initialKeyState "xyz" `shouldBe` (Nothing, [])
        it "arms the prefix in copy mode so prefix-table commands still run" $
            -- bug 83: prefix ] pastes even while in copy mode, rather than
            -- being swallowed by the mode.
            run initialKeyState "\x00]" `shouldBe`
                (Nothing, [RunCommands [["paste-buffer"]]])
        it "holds the armed state across chunks while in copy mode" $ do
            run initialKeyState "\x00" `shouldBe` (Just "prefix", [])
            run armed "]" `shouldBe` (Nothing, [RunCommands [["paste-buffer"]]])
        it "the prefix key arms even when the mode table binds it" $
            run initialKeyState "\x00" `shouldBe` (Just "prefix", [])
        it "falls back to root-table bindings" $
            run initialKeyState "\ESC\ESC[A" `shouldBe`
                (Nothing, [RunCommands [["resize-pane", "-U"]]])

    describe "parseKeyName" $ do
        it "parses mouse keys, canonicalising modifiers and case" $ do
            (.name) <$> parseKeyName "MouseDown1Pane" `shouldBe` Just "MouseDown1Pane"
            (.name) <$> parseKeyName "s-c-wheelupstatus" `shouldBe` Just "C-S-WheelUpStatus"
            (.name) <$> parseKeyName "MouseDown1Control9" `shouldBe` Just "MouseDown1Control9"
            parseKeyName "MouseDown0Pane" `shouldBe` Nothing
        it "parses a key by its codepoint in hex, as sscanf reads it" $ do
            map (fmap (.name) . parseKeyName) ["0x23=", "0x01", "0x1B", "0x20", "0x7F", "0x41"]
                `shouldBe` map Just ["#", "C-a", "Escape", "Space", "BSpace", "A"]
            map (fmap (.raw) . parseKeyName) ["0x01", "0xe9"]
                `shouldBe` map Just ["\x01", "\xc3\xa9"]
            parseKeyName "0xzz" `shouldBe` Nothing

        it "parses plain characters" $ do
            (.name) <$> parseKeyName "x" `shouldBe` Just "x"
            (.name) <$> parseKeyName "%" `shouldBe` Just "%"
        it "parses control keys" $
            (.raw) <$> parseKeyName "C-b" `shouldBe` Just "\x02"
        it "parses ctrl punctuation in both spellings (1b)" $ do
            (.raw) <$> parseKeyName "C-]" `shouldBe` Just "\x1d"
            (.raw) <$> parseKeyName "^]" `shouldBe` Just "\x1d"
            (.name) <$> parseKeyName "^]" `shouldBe` Just "C-]"
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
        it "parses modifier combos on arrows in any prefix order" $ do
            (.name) <$> parseKeyName "S-M-Up" `shouldBe` Just "M-S-Up"
            (.raw) <$> parseKeyName "C-M-S-Left" `shouldBe` Just "\ESC[1;8D"
            (.raw) <$> parseKeyName "S-Up" `shouldBe` Just "\ESC[1;2A"
        it "parses meta keys" $
            (.raw) <$> parseKeyName "M-n" `shouldBe` Just "\ESCn"
        it "parses named keys" $ do
            (.raw) <$> parseKeyName "Up" `shouldBe` Just "\ESC[A"
            (.raw) <$> parseKeyName "Enter" `shouldBe` Just "\r"
        it "encodes Home/End as tmux-256color terminfo (khome/kend), not xterm" $ do
            -- khome=\E[1~, kend=\E[4~; the xterm SS3 forms \EOH/\EOF are read
            -- as a bare H/F by a pager keyed off terminfo (less opens help).
            (.raw) <$> parseKeyName "Home" `shouldBe` Just "\ESC[1~"
            (.raw) <$> parseKeyName "End"  `shouldBe` Just "\ESC[4~"
        it "parses F1-F12 with tmux-256color terminfo bytes (kf1..kf12)" $ do
            -- bug fad
            (.raw) <$> parseKeyName "F1" `shouldBe` Just "\ESCOP"
            (.raw) <$> parseKeyName "F4" `shouldBe` Just "\ESCOS"
            (.raw) <$> parseKeyName "F5" `shouldBe` Just "\ESC[15~"
            (.name) <$> parseKeyName "f12" `shouldBe` Just "F12"
            (.raw) <$> parseKeyName "f12" `shouldBe` Just "\ESC[24~"
        it "parses Insert and BTab" $ do
            -- bug fad
            (.raw) <$> parseKeyName "Insert" `shouldBe` Just "\ESC[2~"
            (.raw) <$> parseKeyName "BTab" `shouldBe` Just "\ESC[Z"
        it "resolves tmux's alternate spellings to the canonical name" $ do
            -- bug fad
            (.name) <$> parseKeyName "IC" `shouldBe` Just "Insert"
            (.name) <$> parseKeyName "DC" `shouldBe` Just "Delete"
            (.name) <$> parseKeyName "PageUp" `shouldBe` Just "PgUp"
            (.name) <$> parseKeyName "NPage" `shouldBe` Just "PgDn"
            (.name) <$> parseKeyName "S-IC" `shouldBe` Just "S-Insert"
        it "parses modified function keys and specials" $ do
            -- bug fad
            (.raw) <$> parseKeyName "S-F1" `shouldBe` Just "\ESC[1;2P"
            (.raw) <$> parseKeyName "C-F5" `shouldBe` Just "\ESC[15;5~"
            (.raw) <$> parseKeyName "M-Insert" `shouldBe` Just "\ESC[2;3~"
        it "roundtrips through tokenization" $ do
            let names = ["C-b", "C-Space", "M-x", "Up", "Down", "Space",
                         "Enter", "Tab", "BSpace", "x", "%", "\"", "M-Up",
                         "C-Up", "C-Down", "C-Left", "C-Right", "S-Up",
                         "M-S-Down", "C-S-Right", "C-M-Left", "C-M-S-Up",
                         "M-Home", "M-End", "F1", "F4", "F5", "F12",
                         "Insert", "BTab", "S-F1", "C-F5", "M-Insert",
                         "S-Delete"]
            [k.name | Just k0 <- map parseKeyName names
                    , k <- tokenizeKeys k0.raw]
                `shouldBe` names
        it "parses the modified character keys the wire can carry" $ do
            -- bug 64: a key hat can now receive must also be bindable.
            (.name) <$> parseKeyName "C-Enter" `shouldBe` Just "C-Enter"
            (.raw) <$> parseKeyName "C-Enter" `shouldBe` Just "\r"
            (.name) <$> parseKeyName "c-enter" `shouldBe` Just "C-Enter"
            (.name) <$> parseKeyName "S-M-s" `shouldBe` Just "M-S-s"
            (.raw) <$> parseKeyName "C-S-s" `shouldBe` Just "\x13"
        it "parses the keypad keys, sending their numeric-mode bytes" $ do
            (.raw) <$> parseKeyName "KP*" `shouldBe` Just "*"
            (.raw) <$> parseKeyName "KPEnter" `shouldBe` Just "\n"
            (.raw) <$> parseKeyName "M-KP5" `shouldBe` Just "\ESC5"
            Map.lookup "KP5" keypadAppBytes `shouldBe` Just "\ESCOu"
        it "rejects nonsense" $
            parseKeyName "NotAKey" `shouldBe` Nothing

-- Whether bytes are an extended-key sequence: the modifyOtherKeys form or a
-- CSI-u one.
isExtended :: B.ByteString -> Bool
isExtended bs =
    "\ESC[27;" `B.isPrefixOf` bs
    || ("\ESC[" `B.isPrefixOf` bs && "u" `B.isSuffixOf` bs)

-- Routing at time 0 with only C-Space as the prefix and tmux's default
-- repeat-time.
ctx0 :: KeyContext
ctx0 = KeyContext
    { prefixKeys = ["C-Space"], defaultTable = "root", modeTable = Nothing
    , prefixTimeout = 0, repeatTime = 500, initialRepeatTime = 0, mouseOn = True
    , now = 0 }

bindOnce, bindRepeat :: [[Text]] -> Binding
bindOnce cmds = Binding { commands = cmds, repeats = Once, note = Nothing }
bindRepeat cmds = Binding { commands = cmds, repeats = Repeats, note = Nothing }

-- A client that just pressed the prefix.
armed :: KeyState
armed = initialKeyState { table = Just "prefix" }

tableOf :: (KeyState, a) -> (Maybe Text, a)
tableOf (st, a) = (st.table, a)
