module Hat.Server.TitleSpec (spec) where

import Test.Hspec

import Hat.Server.Title

parts :: TitleParts
parts = TitleParts
    { session = "work"
    , window  = "editor"
    , path    = "/home/b/Projects/hat"
    , home    = "/home/b"
    , program = "vim"
    }

spec :: Spec
spec = do
    it "reads least specific to most specific when everything fits" $
        composeTitle 100 parts
            `shouldBe` "hat: work: editor: ~/Projects/hat: vim"

    it "abbreviates the home prefix to ~" $
        composeTitle 100 parts { path = "/home/b" }
            `shouldBe` "hat: work: editor: ~: vim"

    it "leaves paths outside home absolute" $
        composeTitle 100 parts { path = "/etc/nixos" }
            `shouldBe` "hat: work: editor: /etc/nixos: vim"

    it "skips a session or window that is just a number" $ do
        composeTitle 100 parts { session = "0" }
            `shouldBe` "hat: editor: ~/Projects/hat: vim"
        composeTitle 100 parts { window = "12" }
            `shouldBe` "hat: work: ~/Projects/hat: vim"
        composeTitle 100 parts { session = "3", window = "12" }
            `shouldBe` "hat: ~/Projects/hat: vim"

    it "collapses from the left when the budget is tight" $ do
        -- the full form is exactly 38 chars; under that, drop "hat: "
        -- first, then the session, then the window
        composeTitle 38 parts
            `shouldBe` "hat: work: editor: ~/Projects/hat: vim"
        composeTitle 37 parts
            `shouldBe` "work: editor: ~/Projects/hat: vim"
        composeTitle 30 parts
            `shouldBe` "editor: ~/Projects/hat: vim"
        composeTitle 24 parts
            `shouldBe` "~/Projects/hat: vim"

    it "shortens the path from the left before dropping it" $ do
        composeTitle 17 parts `shouldBe` "…/hat: vim"
        composeTitle 11 parts { path = "/home/b/a/b/c" }
            `shouldBe` "…/b/c: vim"
        composeTitle 9 parts { path = "/home/b/a/b/c" }
            `shouldBe` "…/c: vim"

    it "falls back to the program alone" $ do
        composeTitle 5 parts `shouldBe` "vim"
        -- never mutilates the program, even over budget
        composeTitle 2 parts { program = "cabal" } `shouldBe` "cabal"

    it "drops empty components" $
        composeTitle 100 parts { window = "", path = "" }
            `shouldBe` "hat: work: vim"
