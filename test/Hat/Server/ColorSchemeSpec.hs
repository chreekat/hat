module Hat.Server.ColorSchemeSpec (spec) where

import Test.Hspec

import Hat.Server.ColorScheme

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
