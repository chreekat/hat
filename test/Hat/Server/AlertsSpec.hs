module Hat.Server.AlertsSpec (spec) where

import Test.Hspec

import Hat.Model.OptionTable (OptionValue (..))
import Hat.Model.Options (applyDelta, defaultOptions, singletonDelta)
import Hat.Server.Alerts

spec :: Spec
spec = do
    describe "what a client is told of an alert (visual-*)" $ do
        it "off rings the bell alone" $
            alertNotice BellAlert VisualOff (InWindow 2) `shouldBe` (Rings, Nothing)
        it "on shows a message instead" $
            alertNotice ActivityAlert VisualOn (InWindow 2)
                `shouldBe` (Quiet, Just "Activity in window 2")
        it "both rings and shows the message" $
            alertNotice SilenceAlert VisualBoth InCurrentWindow
                `shouldBe` (Rings, Just "Silence in current window")

    describe "whether an alert applies (*-action)" $ do
        let with name v = applyDelta (singletonDelta name (OVScalar v)) defaultOptions
        it "other, the activity and silence default, spares the current window" $ do
            alertApplies ActivityAlert defaultOptions InCurrentWindow `shouldBe` False
            alertApplies SilenceAlert defaultOptions (InWindow 1) `shouldBe` True
        it "current admits only the current window" $ do
            alertApplies SilenceAlert (with "silence-action" "current") InCurrentWindow
                `shouldBe` True
            alertApplies SilenceAlert (with "silence-action" "current") (InWindow 1)
                `shouldBe` False
        it "any, the bell default, admits both; none neither" $ do
            alertApplies BellAlert defaultOptions InCurrentWindow `shouldBe` True
            alertApplies ActivityAlert (with "activity-action" "none") (InWindow 1)
                `shouldBe` False
