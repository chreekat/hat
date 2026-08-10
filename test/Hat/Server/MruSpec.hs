module Hat.Server.MruSpec (spec) where

import Test.Hspec

import Hat.Server.Mru

spec :: Spec
spec = do
    describe "recordVisit" $ do
        it "pushes the departed item onto the head" $
            recordVisit 'a' 'b' "" `shouldBe` "a"

        it "toggles: last-* against the head returns the pair" $
            -- on C with history [B,A]; last-* goes to B and leaves [C,A].
            recordVisit 'C' 'B' "BA" `shouldBe` "CA"

        it "moves a revisited item to the head rather than duplicating" $
            recordVisit 'C' 'A' "BA" `shouldBe` "CB"

    describe "scrub" $
        it "drops entries that no longer survive" $
            scrub (`elem` ['A', 'C']) "BAC" `shouldBe` "AC"

    describe "popOnClose" $ do
        it "pops the most-recent survivor and exposes the next as new last" $
            -- on C with history [B,A]; closing C jumps to B, and A is the
            -- new last to jump to.
            popOnClose (`elem` ['B', 'A']) (Just 'z') "BA" `shouldBe` Just ('B', "A")

        it "skips dead history entries" $
            popOnClose (== 'A') (Just 'z') "BA" `shouldBe` Just ('A', "")

        it "falls back with an empty history when no history survives" $
            popOnClose (const False) (Just 'z') "BA" `shouldBe` Just ('z', "")

        it "has no choice when nothing survives at all" $
            popOnClose (const False) Nothing "BA" `shouldBe` (Nothing :: Maybe (Char, String))
