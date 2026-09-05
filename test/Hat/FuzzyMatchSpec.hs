module Hat.FuzzyMatchSpec (spec) where

import Data.Maybe (fromJust, isJust)
import Data.Text qualified as T
import Test.Hspec

import Hat.FuzzyMatch (score)

-- Score a query against a candidate (String args for readable tests).
sc :: String -> String -> Maybe Int
sc q c = score (T.pack q) (T.pack c)

-- The score of a match that is expected to exist.
s :: String -> String -> Int
s q c = fromJust (sc q c)

spec :: Spec
spec = do
    describe "matching" $ do
        it "matches a contiguous run" $
            sc "abc" "abcdef" `shouldSatisfy` isJust

        it "matches a non-contiguous subsequence" $
            sc "ace" "abcde" `shouldSatisfy` isJust

        it "matches case-insensitively" $ do
            sc "AB" "ab" `shouldSatisfy` isJust
            sc "ab" "AB" `shouldSatisfy` isJust

        it "matches a query spanning words (with a separator between)" $
            sc "projhat" "projects hat" `shouldSatisfy` isJust

        it "rejects a query that is not a subsequence" $
            sc "abc" "ab" `shouldBe` Nothing

        it "rejects characters out of order" $
            sc "acb" "abc" `shouldBe` Nothing

        it "scores an empty query as zero" $
            sc "" "anything" `shouldBe` Just 0

    describe "ranking" $ do
        it "prefers a match at a word boundary over one mid-word" $
            s "b" "a b" `shouldSatisfy` (> s "b" "ab")

        it "prefers a contiguous match over a gapped one" $
            s "ab" "ab" `shouldSatisfy` (> s "ab" "aXb")

        it "prefers a CamelCase hump over a buried match" $
            s "mn" "BatMan" `shouldSatisfy` (> s "mn" "batman")

        it "prefers the earlier, tighter of two boundary matches" $
            -- both start at a boundary and run contiguously; the shorter,
            -- earlier candidate is not worse.
            s "cat" "cat" `shouldSatisfy` (>= s "cat" "cat food")
