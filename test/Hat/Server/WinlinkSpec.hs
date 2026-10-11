module Hat.Server.WinlinkSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Hspec

import Hat.Server.Winlink (reselect, shuffleUp)

spec :: Spec
spec = do
    describe "reselect" $ do
        let alive = Set.fromList [0, 2, 5]
        it "keeps a surviving current window, dropping vanished history" $
            reselect alive 2 [7, 5, 0] `shouldBe` Just (2, [5, 0])
        it "moves a vanished current window to the last window" $
            reselect alive 3 [5, 0] `shouldBe` Just (5, [0])
        it "without a last window, falls to the previous index" $
            reselect alive 3 [] `shouldBe` Just (2, [])
        it "the previous index wraps to the highest" $
            reselect alive (-1) [9] `shouldBe` Just (5, [])
        it "nothing survives an empty map" $
            reselect Set.empty 0 [1] `shouldBe` Nothing

    describe "shuffleUp" $ do
        let ws = Map.fromList [(0, 'a'), (1, 'b'), (2, 'c'), (5, 'd')]
        it "moves the occupied run at the index up, highest first" $
            shuffleUp 1 ws `shouldBe` [(2, 3), (1, 2)]
        it "moves nothing when the index is free" $
            shuffleUp 3 ws `shouldBe` []
