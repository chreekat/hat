-- | The desktop title @set-titles-string@'s default expands to.
module Hat.Server.TitleSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import Hat.Model.OptionTable
import Hat.Server.Format (evaluate)

data Parts = Parts
    { session :: Text
    , window  :: Text
    , auto    :: Bool  -- ^ the window is automatic-rename
    , path    :: Text
    , home    :: Text
    , oscTitle :: Text  -- ^ a title the program set itself
    , command :: Text
    }

parts :: Parts
parts = Parts
    { session = "work"
    , window  = "editor"
    , auto    = False
    , path    = "/home/b/Projects/hat"
    , home    = "/home/b"
    , oscTitle = ""
    , command = "vim"
    }

defaultTitle :: Text
defaultTitle = case (.defaultValue) <$> lookupOptionDef "set-titles-string" of
    Just (OVScalar t) -> t
    _ -> error "set-titles-string has no scalar default"

-- | The default, at a width other than its own.
titleAt :: Int -> Parts -> Text
titleAt width p = evaluate vars (const "") (T.replace "#{J/80/" budgeted defaultTitle)
  where
    budgeted = "#{J/" <> T.pack (show width) <> "/"
    vars = Map.fromList
        [ ("session_name", p.session)
        , ("window_name", p.window)
        , ("automatic_rename", if p.auto then "1" else "0")
        , ("pane_current_path", p.path)
        , ("HOME", p.home)
        , ("pane_title", p.oscTitle)
        , ("pane_current_command", p.command)
        ]

spec :: Spec
spec = do
    it "is 80 characters wide" $
        defaultTitle `shouldSatisfy` T.isPrefixOf "#{J/80/"

    it "reads least specific to most specific when everything fits" $
        titleAt 100 parts
            `shouldBe` "hat: work: editor: ~/Projects/hat: vim"

    it "abbreviates the home prefix to ~" $
        titleAt 100 parts { path = "/home/b" }
            `shouldBe` "hat: work: editor: ~: vim"

    it "leaves paths outside home absolute" $
        titleAt 100 parts { path = "/etc/nixos" }
            `shouldBe` "hat: work: editor: /etc/nixos: vim"

    it "skips a session or window that is just a number" $ do
        titleAt 100 parts { session = "0" }
            `shouldBe` "hat: editor: ~/Projects/hat: vim"
        titleAt 100 parts { window = "12" }
            `shouldBe` "hat: work: ~/Projects/hat: vim"
        titleAt 100 parts { session = "3", window = "12" }
            `shouldBe` "hat: ~/Projects/hat: vim"

    it "skips an automatic-rename window's name" $
        titleAt 100 parts { auto = True }
            `shouldBe` "hat: work: ~/Projects/hat: vim"

    it "prefers the program's own title to its command" $ do
        titleAt 100 parts { oscTitle = "README.md" }
            `shouldBe` "hat: work: editor: ~/Projects/hat: README.md"
        titleAt 100 parts { oscTitle = "0" }
            `shouldBe` "hat: work: editor: ~/Projects/hat: 0"

    it "collapses from the left when the budget is tight" $ do
        -- the full form is exactly 38 chars; under that, drop "hat: "
        -- first, then the session, then the window
        titleAt 38 parts
            `shouldBe` "hat: work: editor: ~/Projects/hat: vim"
        titleAt 37 parts
            `shouldBe` "work: editor: ~/Projects/hat: vim"
        titleAt 30 parts
            `shouldBe` "editor: ~/Projects/hat: vim"
        titleAt 24 parts
            `shouldBe` "~/Projects/hat: vim"

    it "shortens the path from the left before dropping it" $ do
        titleAt 17 parts `shouldBe` "…/hat: vim"
        titleAt 11 parts { path = "/home/b/a/b/c" }
            `shouldBe` "…/b/c: vim"
        titleAt 9 parts { path = "/home/b/a/b/c" }
            `shouldBe` "…/c: vim"

    it "falls back to the program alone" $ do
        titleAt 5 parts `shouldBe` "vim"
        -- never mutilates the program, even over budget
        titleAt 2 parts { command = "cabal" } `shouldBe` "cabal"

    it "drops empty components" $
        titleAt 100 parts { window = "", path = "" }
            `shouldBe` "hat: work: vim"
