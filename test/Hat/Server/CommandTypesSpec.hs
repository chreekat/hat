module Hat.Server.CommandTypesSpec (spec) where

import Test.Hspec

import Hat.Server.Command.Types (parseArgs)

spec :: Spec
spec = describe "parseArgs" $ do
    it "splits a bundle into flags and a value flag's rest" $
        parseArgs "s" ["-ds", "foo", "bar"] `shouldBe` ([("-s", "foo")], ["-d"], ["bar"])
    it "keeps a value glued to its flag as written, = and all" $
        parseArgs "tx" ["-t=", "-xM", "name"] `shouldBe` ([("-x", "M"), ("-t", "=")], [], ["name"])
