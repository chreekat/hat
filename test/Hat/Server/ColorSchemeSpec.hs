module Hat.Server.ColorSchemeSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Hat.Model.Options (Options (..), defaultOptions)
import Hat.Server (setOption)
import Hat.Server.ColorScheme
import qualified Hat.Term.Cell as Cell

spec :: Spec
spec = do
    it "parses gsettings get output" $ do
        parseSchemeLine "'prefer-dark'" `shouldBe` Just SchemeDark
        parseSchemeLine "'prefer-light'" `shouldBe` Just SchemeLight

    it "parses gsettings monitor lines" $ do
        parseSchemeLine "color-scheme: 'prefer-dark'"
            `shouldBe` Just SchemeDark
        parseSchemeLine "color-scheme: 'prefer-light'"
            `shouldBe` Just SchemeLight

    it "treats the default scheme as light" $ do
        parseSchemeLine "'default'" `shouldBe` Just SchemeLight
        parseSchemeLine "color-scheme: 'default'"
            `shouldBe` Just SchemeLight

    it "ignores unrelated or malformed lines" $ do
        parseSchemeLine "" `shouldBe` Nothing
        parseSchemeLine "accent-color: 'purple'" `shouldBe` Nothing
        parseSchemeLine "color-scheme: 'sepia'" `shouldBe` Nothing

    it "renders as the color_scheme format value" $ do
        schemeName SchemeDark `shouldBe` "dark"
        schemeName SchemeLight `shouldBe` "light"

    describe "applyPalette" $ do
        it "adapts the default chrome to the scheme" $ do
            let dark = applyPalette SchemeDark defaultOptions
                light = applyPalette SchemeLight defaultOptions
            dark.statusStyle.bg `shouldBe` Cell.Indexed 22
            dark.windowStatusCurrentStyle.bold `shouldBe` True
            light.statusStyle.bg `shouldBe` Cell.Indexed 151
            light.statusStyle.fg `shouldBe` Cell.Indexed 22
            -- borders stay green and must not vanish against either
            -- background
            dark.paneBorderStyle.fg `shouldBe` Cell.Indexed 65
            light.paneBorderStyle.fg `shouldBe` Cell.Indexed 65
            dark.paneActiveBorderStyle.fg `shouldBe` Cell.Indexed 10
            dark.paneActiveBorderStyle.bold `shouldBe` True
            light.paneActiveBorderStyle.fg `shouldBe` Cell.Indexed 22
            light.paneActiveBorderStyle.bold `shouldBe` True

        it "never touches an option the user has set" $ do
            opts <- either (fail . T.unpack) pure
                (setOption False defaultOptions "status-style" "bg=colour196")
            let dark = applyPalette SchemeDark opts
            dark.statusStyle `shouldBe` opts.statusStyle
            dark.statusStyle.bg `shouldBe` Cell.Indexed 196
            -- options the user did not set still adapt
            dark.paneBorderStyle.fg `shouldBe` Cell.Indexed 65
