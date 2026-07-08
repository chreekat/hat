module Hat.Server.FormatSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

import Hat.Server.Format

env :: Map.Map Text Text
env = Map.fromList
    [ ("session_name", "work")
    , ("window_index", "3")
    , ("window_name", "vim")
    , ("window_flags", "*")
    , ("window_active_clients", "2")
    , ("empty", "")
    , ("zero", "0")
    ]

eval :: Text -> Text
eval = evaluate env (\cmd -> "OUT:" <> cmd)

spec :: Spec
spec = do
    it "passes plain text through" $
        eval "hello world" `shouldBe` "hello world"

    it "expands shorthand variables" $
        eval "#S #I:#W#F" `shouldBe` "work 3:vim*"

    it "expands braced variables" $
        eval "#{session_name}" `shouldBe` "work"

    it "renders unknown variables empty" $
        eval "x#{nope}y" `shouldBe` "xy"

    it "escapes ##" $
        eval "100##" `shouldBe` "100#"

    it "evaluates conditionals on non-empty" $ do
        eval "#{?session_name,yes,no}" `shouldBe` "yes"
        eval "#{?empty,yes,no}" `shouldBe` "no"
        eval "#{?zero,yes,no}" `shouldBe` "no"

    it "expands inside conditional branches" $
        eval "#{?window_flags,[#S],-}" `shouldBe` "[work]"

    it "supports nested conditions" $
        eval "#{?#{e|>:#{window_active_clients},1},shared,solo}"
            `shouldBe` "shared"

    it "compares with e| operators" $ do
        eval "#{e|>:2,1}" `shouldBe` "1"
        eval "#{e|>:1,2}" `shouldBe` "0"
        eval "#{e|==:5,5}" `shouldBe` "1"
        eval "#{e|+:2,3}" `shouldBe` "5"

    it "truncates with =N:" $
        eval "#{=2:session_name}" `shouldBe` "wo"

    it "delegates #() to the shell resolver" $
        eval "a #(uptime -p) b" `shouldBe` "a OUT:uptime -p b"

    it "keeps strftime sequences untouched for the caller" $
        eval "%H:%M" `shouldBe` "%H:%M"
