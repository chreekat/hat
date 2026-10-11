module Hat.Server.ThemeSpec (spec) where

import Test.Hspec

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (utc, utcToZonedTime)

import Hat.Model.Options (OptionValue (..), Options, applyEntry, defaultOptions)
import Hat.Server.ColorScheme (ColorScheme (..))
import Hat.Server.Format (renderFormat)
import Hat.Server.Theme
import Hat.Term.Cell qualified as Cell

-- | Expand a palette option as a hat client does.
expand :: Text -> Text
expand = renderFormat (Map.fromList [("client_colours", clientColours)]) (const "")
    (utcToZonedTime utc (posixSecondsToUTCTime 0))

with :: [(Text, Text)] -> Options
with = foldr (\(n, v) -> applyEntry n (OVScalar v)) defaultOptions

yellowOf, redOf :: Options -> Maybe ColorScheme -> Maybe Cell.Color
yellowOf o s = themeColour (resolveTheme expand o s) "themeyellow"
redOf o s = themeColour (resolveTheme expand o s) "ThemeRed"

spec :: Spec
spec = do
    it "detect with no reported scheme draws the dark palette" $
        yellowOf defaultOptions Nothing `shouldBe` Just (Cell.RGB 184 134 11)  -- darkgoldenrod
    it "detect follows a light scheme" $
        redOf defaultOptions (Just SchemeLight) `shouldBe` Just (Cell.RGB 139 58 58)  -- indianred4
    it "light and dark force their palette" $ do
        redOf (with [("theme", "light")]) (Just SchemeDark) `shouldBe` Just (Cell.RGB 139 58 58)
        redOf (with [("theme", "dark")]) (Just SchemeLight) `shouldBe` Just (Cell.RGB 205 92 92)
    it "terminal draws the ANSI colours" $
        yellowOf (with [("theme", "terminal")]) Nothing `shouldBe` Just (Cell.Indexed 3)
    it "a palette entry takes the user's colour" $
        yellowOf (with [("dark-theme-yellow", "colour100")]) Nothing `shouldBe` Just (Cell.Indexed 100)
    it "a palette entry naming a theme colour leaves the slot default" $
        yellowOf (with [("dark-theme-yellow", "themered")]) Nothing `shouldBe` Just Cell.DefaultColor
    it "unknown slot names are not theme colours" $
        themeColour terminalTheme "themeorange" `shouldBe` Nothing
