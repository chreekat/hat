module Hat.Server.ColorSchemeSpec (spec) where

import Control.Exception (AsyncException (ThreadKilled), toException)
import Control.Concurrent.Async (AsyncCancelled (AsyncCancelled))
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec

import Hat.Model.Options
    ( Options (..), OptionName (..), OptionValue (..)
    , resolveOptions, singletonDelta )
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

    describe "watcherFault" $ do
        it "restarts on a transient synchronous failure" $ do
            -- the monitor subprocess dying / EOF on its pipe surfaces as a
            -- sync IOException: theme-following must be durable, so respawn.
            watcherFault (toException (userError "gsettings monitor exited"))
                `shouldBe` RestartWatcher

        it "propagates async cancellation instead of restarting" $ do
            -- withDaemons cancels the daemon at shutdown; swallowing or
            -- restarting on that would spin teardown forever.
            watcherFault (toException AsyncCancelled)
                `shouldBe` PropagateFault
            watcherFault (toException ThreadKilled)
                `shouldBe` PropagateFault

        it "propagates ExitCode instead of restarting" $ do
            -- an exit thrown into the loop must not be buried by a restart.
            watcherFault (toException ExitSuccess)
                `shouldBe` PropagateFault

    describe "applyPalette" $ do
        it "adapts the default chrome to the scheme" $ do
            let dark = resolveOptions [applyPalette SchemeDark]
                light = resolveOptions [applyPalette SchemeLight]
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

        it "gives copy-mode selection a themed amber highlight per scheme" $ do
            let dark = resolveOptions [applyPalette SchemeDark]
                light = resolveOptions [applyPalette SchemeLight]
            -- black text on amber, a warm accent against the green chrome;
            -- deeper gold on dark, lighter amber on light.
            dark.modeStyle.fg `shouldBe` Cell.Indexed 0
            dark.modeStyle.bg `shouldBe` Cell.Indexed 179
            light.modeStyle.fg `shouldBe` Cell.Indexed 0
            light.modeStyle.bg `shouldBe` Cell.Indexed 222

        it "keeps light-mode inactive borders lighter than the active one" $ do
            let light = resolveOptions [applyPalette SchemeLight]
                idx c = case c of
                    Cell.Indexed n -> Just n
                    _ -> Nothing
            -- higher indices in the 256-color grayscale/cube read lighter;
            -- the inactive border must not be bold either
            idx light.paneBorderStyle.fg > idx light.paneActiveBorderStyle.fg
                `shouldBe` True
            light.paneBorderStyle.bold `shouldBe` False

        it "never overrides an option the user has set" $ do
            -- the scheme is the lowest layer, so a user's set-option (here a
            -- session overlay) always wins on resolution.
            let userStyle = Cell.defaultStyle { Cell.bg = Cell.Indexed 196 }
                userDelta = singletonDelta OptStatusStyle (OVStyle userStyle)
                dark = resolveOptions [userDelta, applyPalette SchemeDark]
            dark.statusStyle `shouldBe` userStyle
            dark.statusStyle.bg `shouldBe` Cell.Indexed 196
            -- options the user did not set still adapt
            dark.paneBorderStyle.fg `shouldBe` Cell.Indexed 65
