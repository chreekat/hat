module Hat.Server.ColorSchemeSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Hat.Model.Options (Options (..), defaultOptions)
import Hat.Server (SetMode (..), setOption)
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
            -- borders must not vanish against either background, yet
            -- inactive borders must stay clearly lighter than the active
            -- one. On a dark background 65 is a receding gray; on a light
            -- background that same gray reads too heavy, so light mode
            -- inactive borders use a lighter gray (250).
            dark.paneBorderStyle.fg `shouldBe` Cell.Indexed 65
            light.paneBorderStyle.fg `shouldBe` Cell.Indexed 250
            dark.paneActiveBorderStyle.fg `shouldBe` Cell.Indexed 10
            dark.paneActiveBorderStyle.bold `shouldBe` True
            light.paneActiveBorderStyle.fg `shouldBe` Cell.Indexed 22
            light.paneActiveBorderStyle.bold `shouldBe` True

        it "keeps light-mode inactive borders lighter than the active one" $ do
            let light = applyPalette SchemeLight defaultOptions
                idx c = case c of
                    Cell.Indexed n -> Just n
                    _ -> Nothing
            -- higher indices in the 256-color grayscale/cube read lighter;
            -- the inactive border must not be bold either
            idx light.paneBorderStyle.fg > idx light.paneActiveBorderStyle.fg
                `shouldBe` True
            light.paneBorderStyle.bold `shouldBe` False

        it "never touches an option the user has set" $ do
            opts <- either (fail . T.unpack) pure
                (setOption Assign defaultOptions "status-style" "bg=colour196")
            let dark = applyPalette SchemeDark opts
            dark.statusStyle `shouldBe` opts.statusStyle
            dark.statusStyle.bg `shouldBe` Cell.Indexed 196
            -- options the user did not set still adapt
            dark.paneBorderStyle.fg `shouldBe` Cell.Indexed 65
