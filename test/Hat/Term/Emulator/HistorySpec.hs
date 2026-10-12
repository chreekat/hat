module Hat.Term.Emulator.HistorySpec (spec) where

import Data.Functor.Identity (runIdentity)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Term.Emulator.History

-- | A recording history of @s@ rows under @lim@, libghostty holding exactly
-- those.
holding :: Int -> Int -> History
holding lim s = (newHistory lim) { recording = Recording, size = s, seen = s }

-- | tmux's grid, one scrolled row at a time: collect, then scroll.
stepwise :: Int -> Int -> Int -> Int
stepwise lim n s0 = iterate step s0 !! n
  where
    step s
        | s == 0 || s < lim = s + 1
        | otherwise = s - min s (max 1 (lim `div` 10)) + 1

spec :: Spec
spec = do
    describe "afterOutput" $ do
        it "grows by every row scrolled in below the limit" $
            (afterOutput 15 (holding 20 5)).size `shouldBe` 15
        it "trims a tenth of the limit before a row scrolls into a full history" $
            (afterOutput 21 (holding 20 20)).size `shouldBe` 19
        it "holds a full history under ten rows at its limit" $
            (afterOutput 50 (holding 5 5)).size `shouldBe` 5
        it "shrinks a history swollen past its limit by the trim, one row at a time" $
            (afterOutput 1001 (holding 100 1000)).size `shouldBe` 991
        it "keeps nothing when the pane never recorded history" $
            (afterOutput 30 (newHistory 0)).size `shouldBe` 0
        it "keeps at most what libghostty still holds when it shrinks" $
            (afterOutput 7 (holding 20 12)).size `shouldBe` 7
        it "takes an unseen resize's growth whole" $ do
            let h = (holding 20 20) { primary = ResizedUnseen }
            afterOutput 60 h `shouldBe` h { size = 60, seen = 60, primary = RowsSeen }
        prop "agrees with tmux's row-at-a-time collection" $
            \(NonNegative lim) (NonNegative s) (NonNegative n) ->
                let h = holding (lim `mod` 300) (s `mod` 1000)
                in (afterOutput (h.seen + n `mod` 2000) h).size
                    === stepwise h.limit (n `mod` 2000) h.size

    describe "afterLimit" $ do
        it "trims a fuller history down to the new limit" $
            (afterLimit 10 (holding 20 18)).size `shouldBe` 10
        it "trims one row from a history exactly at the new limit" $
            (afterLimit 18 (holding 20 18)).size `shouldBe` 17
        it "leaves a history under the new limit alone" $
            afterLimit 30 (holding 20 18) `shouldBe` (holding 20 18) { limit = 30 }

    describe "reflow helpers" $ do
        it "counts the logical lines beginning in a run of rows" $
            linesBegun [True, False, False, True, True] `shouldBe` 3
        it "finds the rows holding the first logical lines" $ do
            let flags = [True, False, False, True, True, False, False]
            runIdentity (rowsHolding 2 7 (pure . (flags !!))) `shouldBe` 3
            runIdentity (rowsHolding 3 7 (pure . (flags !!))) `shouldBe` 6
            runIdentity (rowsHolding 9 7 (pure . (flags !!))) `shouldBe` 7
