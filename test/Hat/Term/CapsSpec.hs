module Hat.Term.CapsSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Hat.Term.Caps

-- | What a client reads for a plain VT100-like terminal.
vt100 :: [Text]
vt100 = ["am=1", "clear=\ESC[H\ESC[2J$<50>", "cup=\ESC[%i%p1%d;%p2%dH", "colors=8"]

defaultOverrides, defaultFeatures :: [Text]
defaultOverrides = ["linux*:AX@"]
defaultFeatures =
    ["xterm*:clipboard:ccolour:cstyle:focus:title", "screen*:title", "rxvt*:ignorefkeys"]

term :: Text -> [Text] -> [Text] -> Maybe Text -> TtyTerm
term nm raw overrides colorterm =
    either (error . show) id (createTerm nm raw overrides defaultFeatures colorterm)

spec :: Spec
spec = do
    describe "overrideEntries" $ do
        it "splits at colons" $
            overrideEntries "linux*:AX@" `shouldBe` ["linux*", "AX@"]
        it "reads a doubled colon as a literal one" $
            overrideEntries "x::y:Ms=a::b" `shouldBe` ["x:y", "Ms=a:b"]

    describe "unvis" $ do
        it "decodes \\E and control escapes" $
            unvis "\\E[8h\\a\\^G" `shouldBe` Just "\ESC[8h\a\a"
        it "decodes octal" $
            unvis "\\033[m" `shouldBe` Just "\ESC[m"
        it "refuses an unknown escape" $
            unvis "\\q" `shouldBe` Nothing

    describe "createTerm" $ do
        it "refuses a terminal that cannot clear" $
            createTerm "dumb" ["cup=x"] [] [] Nothing
                `shouldBe` Left "terminal does not support clear"
        it "strips terminfo padding" $
            capString "clear" (term "vt100" vt100 [] Nothing) `shouldBe` "\ESC[H\ESC[2J"
        it "gives a VT100-like terminal bpaste, focus and title" $
            featureNames (term "vt100" vt100 [] Nothing) `shouldBe` "bpaste,focus,title"
        it "adds terminal-features entries matching TERM" $
            featureNames (term "xterm-256color" vt100 [] Nothing)
                `shouldBe` "bpaste,ccolour,clipboard,cstyle,focus,title"
        it "applies a feature's capabilities" $ do
            let t = term "xterm" vt100 [] Nothing
            capString "tsl" t `shouldBe` "\ESC]0;"
            capString "Ms" t `shouldBe` "\ESC]52;%p1%s;%p2%s\a"
        it "takes RGB from COLORTERM" $ do
            let t = term "vt100" vt100 [] (Just "truecolor")
            rgbColours t `shouldBe` True
            clientColourCount t `shouldBe` 16777216
        it "takes 256 colours from a COLORTERM naming them" $
            clientColourCount (term "vt100" vt100 [] (Just "x-256")) `shouldBe` 256
        it "takes RGB from the Tc flag" $
            rgbColours (term "vt100" ("Tc=1" : vt100) [] Nothing) `shouldBe` True
        it "counts the colors capability without 256 or RGB" $
            clientColourCount (term "vt100" vt100 [] Nothing) `shouldBe` 8
        it "lets an override win over a feature" $ do
            let t = term "linux" vt100 defaultOverrides (Just "truecolor")
            hasCap "AX" t `shouldBe` False
            hasCap "setrgbf" t `shouldBe` True
        it "removes a capability with name@" $
            noAutoMargin (term "screen" vt100 ["*:am@"] Nothing) `shouldBe` True
        it "sets an unvis'd string from an override" $
            capString "Bidi" (term "screen" vt100 ["*:Bidi=\\E[8h"] Nothing)
                `shouldBe` "\ESC[8h"
        it "skips overrides whose pattern does not match" $
            hasCap "Bidi" (term "screen" vt100 ["xterm*:Bidi=x"] Nothing) `shouldBe` False

    describe "featurePresent" $ do
        it "answers for an applied feature" $
            featurePresent "title" (term "vt100" vt100 [] Nothing) `shouldBe` True
        it "needs the 256-colour flag, not only the capabilities" $
            featurePresent "256" (term "vt100" ("AX=1" : "setab=x" : "setaf=y" : vt100) [] Nothing)
                `shouldBe` False
        it "finds a feature whose capabilities are all present" $
            featurePresent "strikethrough" (term "vt100" ("smxx=\ESC[9m" : vt100) [] Nothing)
                `shouldBe` True

    describe "capList and termFromWire" $
        it "carry a resolved term across" $ do
            let t = term "xterm-256color" ("Tc=1" : "U8=1" : vt100) ["*:am@"] Nothing
            termFromWire t.name (capList t) (appliedFeatures t) `shouldBe` t
