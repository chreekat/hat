module Hat.Bench.PerfStatSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Hat.Bench.PerfStat

-- | A capture of @perf stat -x, -e instructions,task-clock,cycles@, comment
-- header included, with one uncountable and one unsupported event.
sample :: T.Text
sample = T.unlines
    [ "# started on Sat Aug 22 21:10:21 2026"
    , ""
    , "1027715,,instructions:u,8297205,100.00,,"
    , "7.03,msec,task-clock:u,7031000,100.00,,"
    , "<not counted>,,cycles:u,0,100.00,,"
    , "<not supported>,,branch-misses,0,100.00,,"
    ]

spec :: Spec
spec = do
    describe "parsePerfStat" $ do
        it "reads an integral counter" $
            (parsePerfStat sample >>= counterWord "instructions:u")
                `shouldBe` Right 1027715

        it "finds a counter by its bare name when perf appended a modifier" $
            (parsePerfStat sample >>= counterWord "instructions")
                `shouldBe` Right 1027715

        it "reads a fractional counter" $
            (parsePerfStat sample >>= counterDouble "task-clock")
                `shouldBe` Right 7.03

        it "keeps every event row, countable or not" $
            fmap counterCount (parsePerfStat sample) `shouldBe` Right 4

        it "reports a <not counted> event as such, not as a number" $
            (parsePerfStat sample >>= counterWord "cycles")
                `shouldSatisfy` either (T.isInfixOf "not counted") (const False)

        it "reports a <not supported> event as such" $
            (parsePerfStat sample >>= counterWord "branch-misses")
                `shouldSatisfy` either (T.isInfixOf "not supported") (const False)

        it "reports a missing event rather than defaulting it" $
            (parsePerfStat sample >>= counterWord "page-faults")
                `shouldSatisfy` either (T.isInfixOf "page-faults") (const False)

        it "rejects a line that is not a perf counter row" $
            parsePerfStat "perf: permission denied\n"
                `shouldSatisfy` either (const True) (const False)

        it "rejects output with no counter rows at all" $
            parsePerfStat "# started on Sat Aug 22 21:10:21 2026\n"
                `shouldSatisfy` either (const True) (const False)
