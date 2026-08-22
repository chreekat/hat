module Hat.InternSpec (spec) where

import Data.IORef (IORef, newIORef, readIORef)
import Data.Map (Map)
import Data.Map qualified as Map
import System.Mem.StableName (makeStableName)
import System.Mem.Weak (Weak)
import Test.Hspec

import Hat.Intern (shareVals)

-- | Two references are the same heap object.
sameObject :: a -> a -> IO Bool
sameObject a b = (==) <$> makeStableName a <*> makeStableName b

-- | A freshly allocated @[Int]@ equal to @[1,2,3]@. 'clone' is 'NOINLINE' and
-- runs in IO, so the optimiser can't fold two calls into one shared literal:
-- each yields a distinct spine that compares equal.
freshList :: IO [Int]
freshList = clone [1, 2, 3]

{-# NOINLINE clone #-}
clone :: [a] -> IO [a]
clone [] = pure []
clone (x : xs) = (x :) <$> clone xs

newTable :: IO (IORef (Map [Int] (Weak [Int])))
newTable = newIORef Map.empty

tableSize :: IORef (Map k v) -> IO Int
tableSize t = Map.size <$> readIORef t

spec :: Spec
spec = do
    describe "shareVals" $ do
        it "preserves the value" $ do
            t <- newTable
            x <- freshList
            y <- shareVals t x
            y `shouldBe` [1, 2, 3]

        it "dedups equal values into one table entry" $ do
            t <- newTable
            _ <- shareVals t =<< freshList
            _ <- shareVals t =<< freshList
            tableSize t `shouldReturn` 1

        it "collapses distinct-but-equal values to one heap object" $ do
            a <- freshList
            b <- freshList
            distinct <- sameObject a b
            distinct `shouldBe` False   -- guards the test: inputs really differ
            t <- newTable
            a' <- shareVals t a
            b' <- shareVals t b
            sameObject a' b' `shouldReturn` True

        it "hands back the first representative on a hit" $ do
            t <- newTable
            a <- freshList
            a' <- shareVals t a
            b' <- shareVals t =<< freshList
            sameObject a' a `shouldReturn` True
            sameObject b' a `shouldReturn` True

        it "keeps distinct values distinct" $ do
            t <- newTable
            p <- shareVals t [1, 2, 3]
            q <- shareVals t [9, 9, 9]
            (p, q) `shouldBe` ([1, 2, 3], [9, 9, 9])
            tableSize t `shouldReturn` 2
            sameObject p q `shouldReturn` False
