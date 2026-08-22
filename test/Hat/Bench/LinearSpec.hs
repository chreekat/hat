module Hat.Bench.LinearSpec (spec) where

import Data.List (nub)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Bench.Linear

spec :: Spec
spec = do
    describe "fitLinear" $ do
        it "draws the line through two points exactly" $
            fitLinear [(1, 3), (3, 7)] `shouldSatisfy`
                fits (\l -> close 2 l.slope && close 1 l.intercept)

        it "splits symmetric residuals evenly" $
            -- Points sit 1 above and 1 below y = x; the fit is y = x.
            fitLinear [(0, -1), (0, 1), (2, 1), (2, 3)] `shouldSatisfy`
                fits (\l -> close 1 l.slope && close 0 l.intercept)

        it "rejects fewer than two points" $
            fitLinear [(1, 1)] `shouldSatisfy` either (const True) (const False)

        it "rejects points that share one x (a vertical line)" $
            fitLinear [(2, 1), (2, 5), (2, 9)] `shouldSatisfy`
                either (const True) (const False)

        prop "recovers the slope and intercept of exact points" $
            \(a :: Int) (b :: Int) (xs :: [Int]) ->
                let distinct = nub xs
                in length distinct >= 2 ==>
                    let pts = [ (fromIntegral x, fromIntegral (a * x + b))
                              | x <- distinct ]
                    in fitLinear pts `shouldSatisfy` fits (\l ->
                        close (fromIntegral a) l.slope
                            && close (fromIntegral b) l.intercept)

fits :: (Line -> Bool) -> Either e Line -> Bool
fits p = either (const False) p

-- | Equal up to fitLinear's arithmetic noise.
close :: Double -> Double -> Bool
close expected actual = abs (expected - actual) < 1e-6 * (1 + abs expected)
