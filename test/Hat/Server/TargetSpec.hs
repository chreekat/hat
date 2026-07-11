module Hat.Server.TargetSpec (spec) where

import Test.Hspec

import Hat.Server.Target (PaneTarget (..), parsePaneTarget)

spec :: Spec
spec = describe "parsePaneTarget" $ do
    it "treats a missing -t as the current pane" $
        parsePaneTarget Nothing `shouldBe` PaneCurrent
    it "parses ! as the last (alternate) pane" $
        parsePaneTarget (Just "!") `shouldBe` PaneLast
    it "parses ~ and {marked} as the marked pane" $ do
        parsePaneTarget (Just "~") `shouldBe` PaneMarked
        parsePaneTarget (Just "{marked}") `shouldBe` PaneMarked
    it "parses %N as a pane id" $
        parsePaneTarget (Just "%7") `shouldBe` PaneById 7
    it "falls back to the current pane for unrecognized targets" $
        parsePaneTarget (Just "nonsense") `shouldBe` PaneCurrent
