module Hat.Bench.RtsStatsSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Hat.Bench.RtsStats

-- | A trimmed capture of @+RTS -t --machine-readable@ from the hat binary,
-- leading argv line included.
sample :: T.Text
sample = T.unlines
    [ "'/path/to/hat' '-S' '/tmp/sock' 'ls-sessions' +RTS '-N2' '--machine-readable' "
    , " [(\"bytes allocated\", \"164960\")"
    , " ,(\"num_GCs\", \"1\")"
    , " ,(\"major_gcs\", \"1\")"
    , " ,(\"mut_cpu_seconds\", \"0.001689\")"
    , " ,(\"GC_cpu_seconds\", \"0.250000\")"
    , " ,(\"allocated_bytes\", \"164960\")"
    , " ,(\"max_live_bytes\", \"57192\")"
    , " ]"
    ]

spec :: Spec
spec = do
    describe "parseRtsStats" $ do
        it "reads a key whose name contains a space" $
            (parseRtsStats sample >>= statWord "bytes allocated")
                `shouldBe` Right 164960

        it "reads a fractional-seconds field" $
            (parseRtsStats sample >>= statDouble "GC_cpu_seconds")
                `shouldBe` Right 0.25

        it "keeps every pair in the block" $
            fmap statCount (parseRtsStats sample) `shouldBe` Right 7

        it "tolerates a field this build has never heard of" $
            -- GHC adds counters between releases; an unknown key must not
            -- sink the whole parse.
            fmap statCount
                (parseRtsStats (T.replace "num_GCs" "gcs_from_the_future" sample))
                `shouldBe` Right 7

        it "reports a missing key rather than defaulting it" $
            (parseRtsStats sample >>= statWord "no_such_counter")
                `shouldSatisfy` either (T.isInfixOf "no_such_counter") (const False)

        it "reports a key whose value is not a number" $
            (parseRtsStats (T.replace "\"57192\"" "\"lots\"" sample)
                >>= statWord "max_live_bytes")
                `shouldSatisfy` either (T.isInfixOf "max_live_bytes") (const False)

        it "rejects output with no stats block at all" $
            parseRtsStats "hat: no server running\n"
                `shouldSatisfy` either (const True) (const False)
