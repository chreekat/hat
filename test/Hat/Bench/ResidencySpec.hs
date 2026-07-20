module Hat.Bench.ResidencySpec (spec) where

import Test.Hspec

import Hat.Bench.Residency

spec :: Spec
spec = do
    let base = Baseline (LiveBytes 1000)
        tol  = mkTolerance 0.05  -- ±5% → band [950, 1050]

    describe "classify" $ do
        it "accepts a measurement equal to the baseline" $
            classify tol base (LiveBytes 1000) `shouldBe` WithinBand

        it "accepts a measurement inside the band" $
            classify tol base (LiveBytes 1040) `shouldBe` WithinBand

        it "treats the upper edge as within band" $
            classify tol base (LiveBytes 1050) `shouldBe` WithinBand

        it "treats the lower edge as within band" $
            classify tol base (LiveBytes 950) `shouldBe` WithinBand

        it "flags a regression just above the band" $
            classify tol base (LiveBytes 1051) `shouldBe` Regressed (LiveBytes 1051)

        it "flags an improvement just below the band" $
            classify tol base (LiveBytes 949) `shouldBe` Improved (LiveBytes 949)

        it "carries the measured figure so a caller can ratchet" $
            classify tol base (LiveBytes 600) `shouldBe` Improved (LiveBytes 600)

        it "clamps a nonsensical tolerance so a real regression still fires" $
            -- Unclamped, ±500% would stretch the band to [-4000, 6000] and
            -- swallow a 5× blowup; mkTolerance saturates below 1 instead.
            classify (mkTolerance 5.0) base (LiveBytes 5000)
                `shouldBe` Regressed (LiveBytes 5000)
