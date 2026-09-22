module Hat.Bench.ReportSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import Hat.Bench.Linear
import Hat.Bench.Report

key :: Text -> Text -> Text -> SeriesKey
key m w r = SeriesKey { mux = m, workload = w, role = r }

hasLine :: [Text] -> [Text] -> Expectation
hasLine rendered wanted =
    rendered `shouldSatisfy` any (\l -> all (`T.isInfixOf` l) wanted)

spec :: Spec
spec = do
    describe "parseSeriesFile" $ do
        it "reads mux, workload, role, and N from a perf CSV name" $
            parseSeriesFile "hat-type-server-200.csv"
                `shouldBe` Just (key "hat" "type" "server", 200)

        it "accepts a digit in the workload name" $
            parseSeriesFile "tmux-type4-client-1600.csv"
                `shouldBe` Just (key "tmux" "type4" "client", 1600)

        it "rejects a non-CSV file" $
            parseSeriesFile "hat-type-server-200.txt" `shouldBe` Nothing

        it "rejects a name with too few fields" $
            parseSeriesFile "hat-server-200.csv" `shouldBe` Nothing

        it "rejects a non-numeric size" $
            parseSeriesFile "hat-type-server-x.csv" `shouldBe` Nothing

    describe "humanize" $ do
        it "shows millions" $ humanize 28146502 `shouldBe` "28.1M"
        it "shows thousands" $ humanize 41000 `shouldBe` "41.0k"
        it "shows three significant digits" $
            humanize 936000000 `shouldBe` "936M"
        it "shows billions" $ humanize 1880000000 `shouldBe` "1.88G"
        it "shows small values plainly" $ humanize 12 `shouldBe` "12.0"
        it "keeps the sign of a negative value" $
            humanize (-6352815) `shouldBe` "-6.35M"

    describe "summaryTable" $ do
        let series =
                [ (key "hat" "type" "server", Line 28.0e6 9.0e8)
                , (key "tmux" "type" "server", Line 40.0e3 9.0e7)
                ]
        it "renders one humanized row per series" $ do
            summaryTable series `hasLine` ["hat type server", "28.0M"]
            summaryTable series `hasLine` ["tmux type server", "40.0k"]

        it "renders the hat:tmux ratio where both muxes measured a series" $
            summaryTable series `hasLine` ["type server", "700x"]

        it "renders no ratio when one mux is missing" $
            summaryTable [(key "hat" "type" "server", Line 28.0e6 0)]
                `shouldSatisfy` not . any ("x tmux" `T.isInfixOf`)

        it "renders no ratio against a flat tmux series" $
            summaryTable
                [ (key "hat" "type" "client", Line 43.0e3 0)
                , (key "tmux" "type" "client", Line 0.01 0)
                ]
                `shouldSatisfy` not . any ("x tmux" `T.isInfixOf`)

    describe "compareTable" $ do
        let beforeRun = [(key "hat" "type" "server", Line 30.0e6 0)]
            afterRun = [(key "hat" "type" "server", Line 20.0e6 0)]
        it "renders old, new, and the signed percent change" $
            compareTable beforeRun afterRun
                `hasLine` ["hat type server", "30.0M", "20.0M", "-33.3%"]

        it "names a series measured on only one side" $
            compareTable [] afterRun `hasLine` ["hat type server", "only"]

    describe "parseBaseline" $ do
        let good = T.unlines
                [ "# instructions per unit, fitted slope"
                , "tolerance 0.20"
                , "hat type server 28000000"
                ]
        it "reads tolerance and series entries" $
            parseBaseline good `shouldBe` Right Baseline
                { tolerance = 0.2
                , entries = [(key "hat" "type" "server", 28000000)]
                }

        it "rejects a file with no tolerance" $
            parseBaseline "hat type server 28000000\n"
                `shouldSatisfy` either (const True) (const False)

        it "rejects an unreadable line" $
            parseBaseline "tolerance 0.20\nwhat is this\n"
                `shouldSatisfy` either (const True) (const False)

    describe "renderBaseline" $ do
        it "roundtrips through parseBaseline" $ do
            let b = Baseline
                    { tolerance = 0.2
                    , entries =
                        [ (key "hat" "type" "client", 43000)
                        , (key "hat" "type" "server", 28000000)
                        ]
                    }
            parseBaseline (renderBaseline b) `shouldBe` Right b

    describe "checkBaseline" $ do
        let base = Baseline
                { tolerance = 0.2
                , entries = [(key "hat" "type" "server", 100)]
                }
            run measured = checkBaseline base
                [(key "hat" "type" "server", Line measured 0)]
        it "passes a measurement inside the band" $
            checkPassed (run 110) `shouldBe` True

        it "trips on a regression beyond tolerance" $ do
            checkPassed (run 130) `shouldBe` False
            renderChecks (run 130) `hasLine` ["hat type server", "REGRESSED"]

        it "trips on an improvement beyond tolerance, asking for an update" $ do
            checkPassed (run 70) `shouldBe` False
            renderChecks (run 70) `hasLine` ["hat type server", "baseline"]

        it "trips when a baseline series was not measured" $ do
            checkPassed (checkBaseline base []) `shouldBe` False
            renderChecks (checkBaseline base [])
                `hasLine` ["hat type server", "no measurement"]
