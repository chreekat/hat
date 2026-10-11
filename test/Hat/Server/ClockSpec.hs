module Hat.Server.ClockSpec (spec) where

import Data.Text qualified as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import Data.Vector qualified as V
import Test.Hspec

import Hat.Geometry (Size (..))
import Hat.Server.Clock
import Hat.Term.Cell (Cell (..), Color (..), Content (..), Style (..), defaultStyle)

at :: Int -> Int -> Int -> LocalTime
at h m s = LocalTime (fromGregorian 2026 10 11) (TimeOfDay h m (fromIntegral s))

spec :: Spec
spec = do
    describe "the time clock-mode-style spells" $ do
        it "12 and 12-with-seconds: a padded hour and AM or PM" $ do
            clockText Clock12 (at 13 5 0) `shouldBe` " 1:05 PM"
            clockText Clock12Seconds (at 9 3 7) `shouldBe` " 9:03:07 AM"
        it "24 and 24-with-seconds" $ do
            clockText Clock24 (at 9 3 7) `shouldBe` "09:03"
            clockText Clock24Seconds (at 21 3 7) `shouldBe` "21:03:07"
        it "reads the option's choices" $
            map clockStyleOf ["12", "24", "12-with-seconds", "24-with-seconds"]
                `shouldBe` [Clock12, Clock24, Clock12Seconds, Clock24Seconds]

    describe "the clock-mode screen" $ do
        let colour = Indexed 4
            char c = case c.content of
                Glyph ch _ _ -> ch
                Continuation -> '?'
            row cells r = T.pack (map char (V.toList (cells V.! r)))
        it "centres the time in big glyphs in clock-mode-colour" $ do
            -- "12:34" is 5 glyphs, 6 columns apiece, from column 20 - 15.
            let cells = clockCells colour Clock24 (Size { rows = 10, cols = 40 }) (at 12 34 0)
            row cells 2 `shouldBe` "         # #####       ##### #   #      "
            ((cells V.! 2) V.! 9).style `shouldBe` defaultStyle { fg = colour, bg = colour }
            row cells 0 `shouldBe` T.replicate 40 " "
        it "falls back to plain text in a pane too small for the glyphs" $ do
            let cells = clockCells colour Clock24 (Size { rows = 3, cols = 10 }) (at 12 34 0)
            row cells 1 `shouldBe` "   12:34  "
            ((cells V.! 1) V.! 3).style `shouldBe` defaultStyle { fg = colour }
