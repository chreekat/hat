-- | Typed option-value validation, in tmux's words (upstream
-- options-values.sh / options-array.sh).
module Hat.Server.OptionParseSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Test.Hspec

import Hat.Model.OptionTable
import Hat.Server.OptionParse

def :: Text -> OptionDef
def = fromJust . lookupOptionDef

-- A scalar set with no own value at the scope, against a current value.
setScalar :: Text -> Text -> Maybe Text -> Either Text Text
setScalar n current = parseScalar (def n) Assign Nothing current

spec :: Spec
spec = do
    describe "numbers" $ do
        it "accepts a value in range" $
            setScalar "display-time" "750" (Just "4000") `shouldBe` Right "4000"
        it "rejects below the minimum" $
            setScalar "display-time" "750" (Just "-5")
                `shouldBe` Left "value is too small: -5"
        it "rejects above the maximum" $
            setScalar "pane-base-index" "0" (Just "65536")
                `shouldBe` Left "value is too large: 65536"
        it "rejects a non-number" $
            setScalar "display-time" "750" (Just "abc")
                `shouldBe` Left "value is invalid: abc"
        it "rejects a missing value" $
            setScalar "display-time" "750" Nothing `shouldBe` Left "empty value"

    describe "choices" $ do
        it "accepts a listed choice" $
            setScalar "mode-keys" "emacs" (Just "vi") `shouldBe` Right "vi"
        it "rejects an unlisted value" $
            setScalar "mode-keys" "emacs" (Just "bogus")
                `shouldBe` Left "unknown value: bogus"
        it "toggles between the first two choices with no value" $
            setScalar "status" "on" Nothing `shouldBe` Right "off"

    describe "patterns" $ do
        it "accepts what default-size's glob matches, scannable or not" $ do
            setScalar "default-size" "80x24" (Just "100x30") `shouldBe` Right "100x30"
            setScalar "default-size" "80x24" (Just "1junkx2") `shouldBe` Right "1junkx2"
        it "rejects what the glob does not match" $ do
            setScalar "default-size" "80x24" (Just "100") `shouldBe` Left "value is invalid: 100"
            setScalar "default-size" "80x24" (Just "x24") `shouldBe` Left "value is invalid: x24"
            setScalar "default-size" "80x24" (Just "80x") `shouldBe` Left "value is invalid: 80x"
        it "checks an appended value whole" $
            parseScalar (def "default-size") Append (Just "80") "80x24" (Just "x2")
                `shouldBe` Right "80x2"

    describe "flags" $ do
        it "toggles with no value" $ do
            setScalar "focus-events" "off" Nothing `shouldBe` Right "on"
            setScalar "focus-events" "on" Nothing `shouldBe` Right "off"
        it "accepts tmux's spellings case-insensitively" $ do
            setScalar "focus-events" "off" (Just "yes") `shouldBe` Right "on"
            setScalar "focus-events" "on" (Just "NO") `shouldBe` Right "off"
            setScalar "focus-events" "off" (Just "1") `shouldBe` Right "on"
        it "rejects garbage" $
            setScalar "focus-events" "off" (Just "maybe")
                `shouldBe` Left "bad value: maybe"

    describe "colours" $ do
        it "canonicalises a colour option's value" $ do
            setScalar "status-bg" "default" (Just "red") `shouldBe` Right "red"
            setScalar "status-bg" "default" (Just "colour123")
                `shouldBe` Right "colour123"
            setScalar "status-bg" "default" (Just "#00FF00")
                `shouldBe` Right "#00ff00"
            setScalar "status-bg" "default" (Just "gray50")
                `shouldBe` Right "#7f7f7f"
        it "rejects junk with tmux's colour-type error" $
            setScalar "status-bg" "default" (Just "xxxyyy")
                `shouldBe` Left "bad colour: xxxyyy"
        it "rejects junk with tmux's colour-string error" $
            setScalar "cursor-colour" "" (Just "xxxyyy")
                `shouldBe` Left "invalid colour: xxxyyy"

    describe "styles" $ do
        it "keeps the spelling as set" $
            setScalar "status-style" "default" (Just "fg=red,bg=black")
                `shouldBe` Right "fg=red,bg=black"
        it "rejects an unknown keyword" $
            setScalar "status-style" "default" (Just "bg=xxxyyy")
                `shouldBe` Left "invalid style: bg=xxxyyy"
        it "leaves a format-bearing style to expand later" $
            setScalar "status-style" "default" (Just "fg=#{@c}")
                `shouldBe` Right "fg=#{@c}"
        it "appends with the style separator" $
            parseScalar (def "status-style") Append (Just "fg=red") "fg=red"
                (Just "bold") `shouldBe` Right "fg=red,bold"

    describe "strings" $
        it "appends with no separator" $
            parseScalar (def "status-left") Append (Just "foo") "foo"
                (Just "bar") `shouldBe` Right "foobar"

    describe "keys" $ do
        it "stores the canonical key name" $
            setScalar "prefix" "C-b" (Just "C-space") `shouldBe` Right "C-Space"
        it "rejects an unknown key" $
            setScalar "prefix" "C-b" (Just "boguskey")
                `shouldBe` Left "bad key: boguskey"

    describe "implementation gate" $ do
        it "refuses an unimplemented choice of a live option" $
            supportedValue (def "status") "3"
                `shouldBe` Left "status: value not yet supported by hat: 3"
        it "passes an implemented choice" $
            supportedValue (def "status") "off" `shouldBe` Right ()

    describe "commands" $ do
        it "accepts a command that parses" $
            setScalar "default-client-command" "new-session" (Just "new-window")
                `shouldBe` Right "new-window"
        it "rejects a command that does not parse" $
            setScalar "default-client-command" "new-session" (Just "if -x {")
                `shouldBe` Left "syntax error"
        it "parses each hook command" $
            setArrayKey (def "alert-bell") Assign Map.empty (KeyIndex 0) "if -x {"
                `shouldBe` Left "syntax error"

    describe "arrays" $ do
        let ue = def "update-environment"
        it "splits a whole-array assignment on the separator" $
            assignArray ue Map.empty "AAA BBB,CCC" `shouldBe` Right
                (Map.fromList
                    [(KeyIndex 0, "AAA"), (KeyIndex 1, "BBB"), (KeyIndex 2, "CCC")])
        it "fills the first free index" $
            assignArray ue (Map.fromList [(KeyIndex 0, "A"), (KeyIndex 2, "C")])
                "FFF" `shouldBe` Right (Map.fromList
                    [(KeyIndex 0, "A"), (KeyIndex 1, "FFF"), (KeyIndex 2, "C")])
        it "validates a colour array's entries" $
            setArrayKey (def "pane-colours") Assign Map.empty (KeyIndex 1) "xxxyyy"
                `shouldBe` Left "bad colour: xxxyyy"
        it "appends onto one key" $
            setArrayKey (def "command-alias") Append
                (Map.fromList [(KeyIndex 7, "zoom=resize-pane")]) (KeyIndex 7) " -Z"
                `shouldBe` Right (Map.fromList [(KeyIndex 7, "zoom=resize-pane -Z")])
