module Hat.Server.CommandTypesSpec (spec) where

import Test.Hspec hiding (Arg)

import Hat.Command.Arg (Arg (..))
import Hat.Server.Command.Types (parseArgs, parseArgsOver)

spec :: Spec
spec = describe "parseArgs" $ do
    it "splits a bundle into flags and a value flag's rest" $
        parseArgs "s" ["-ds", "foo", "bar"] `shouldBe` ([("-s", "foo")], ["-d"], ["bar"])
    it "splits a word that is not UTF-8 as bytes, keeping its value raw" $
        parseArgsOver "s" [] [RawArg "-dsa\xffb", RawArg "\xff", Utf8Arg "-sx"]
            `shouldBe` ([("-s", RawArg "a\xffb")], ["-d"], [RawArg "\xff", Utf8Arg "-sx"])
    it "keeps a value glued to its flag as written, = and all" $
        parseArgs "tx" ["-t=", "-xM", "name"] `shouldBe` ([("-x", "M"), ("-t", "=")], [], ["name"])
