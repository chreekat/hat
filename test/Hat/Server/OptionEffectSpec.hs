-- | The executable companion to @docs/options-audit.md@: each option gets a
-- prefix-style effect test that sets a non-default value and asserts the
-- option→behavior mapping it controls — the pure function or single dispatch
-- step that turns the value into a behavioral mode — so a silent no-op (or
-- wrong semantics) fails a test instead of needing another manual audit.
-- The behavior behind each mode lives in its owning spec.
module Hat.Server.OptionEffectSpec (spec) where

import Test.Hspec

import Control.Concurrent.STM (readTVarIO)
import Data.List qualified as List
import Data.Map.Strict qualified as Map

import Hat.Geometry (Pos (..), Rect (..), Size (..))
import Hat.Log (newLogger)
import Hat.Model (Markup (..), PaneStart (..), ServerState (..), Toast (..), newServerState)
import Hat.Model.Options
    ( BorderLines (..), OptionValue (..), Options (..)
    , StatusPosition (..), WindowSizeMode (..), applyEntry, defaultOptions
    , optionText )
import Hat.Server
    ( Reply (..), deliversKey, escTiming, namedSizesOf, resizeModeOf
    , runCommands, toastDeadline, toastExpired )
import Hat.Server.Environ
    ( EnvEntry (..), EnvVisibility (..), environFind, environFromPairs
    , environUpdate )
import Hat.Server.Keys (EscTiming (..), Key (..))
import Hat.Server.Pane (paneProgram, shellStart, suitableShell)
import Hat.Model.Ids (PaneId (..))
import Hat.Server.Layout (Layout (..), LayoutName (..), namedLayout, paneCell)
import Hat.Server.Scene
import Hat.Server.View (messageLine, statusLayout)
import Hat.Term.Cell qualified as Cell

spec :: Spec
spec = do
    describe "option effects (see docs/options-audit.md)" $ do
        -- focus-events: a FocusIn/FocusOut report reaches the pane only when
        -- focus-events is on (the full gating matrix is SessionSpec's).
        describe "focus-events gates FocusIn/FocusOut delivery" $
            it "turning the option on flips a focus report from dropped to delivered" $ do
                let focusIn = Key { name = "FocusIn", raw = "\ESC[I" }
                deliversKey defaultOptions True focusIn `shouldBe` False
                deliversKey (defaultOptions { focusEvents = True }) True focusIn
                    `shouldBe` True

        -- display-time: a toast lives that many milliseconds; 0 keeps it up
        -- until a key is pressed (bug ff).
        describe "display-time bounds the toast's lifetime" $ do
            let ms n = n * 1000000  -- monotonic clock is in ns
                shownAt = ms 41000
                toastFor opts =
                    Toast { text = "hi", markup = MarkupLiteral, deadline = toastDeadline opts shownAt }
            it "750: alive at t+700ms, expired at t+800ms" $ do
                let toast = toastFor (defaultOptions { displayTime = 750 })
                toastExpired (shownAt + ms 700) toast `shouldBe` False
                toastExpired (shownAt + ms 800) toast `shouldBe` True
            it "0: no deadline, never expires by time" $ do
                let toast = toastFor (defaultOptions { displayTime = 0 })
                toast.deadline `shouldBe` Nothing
                toastExpired (shownAt + ms 3600000) toast `shouldBe` False

        -- main-pane-*/other-pane-*: cells or a percentage of the window.
        describe "main-pane-width/-height and other-pane-width/-height size the named layouts" $ do
            let pids = map PaneId [0, 1, 2]
                with = foldr (\(n, v) -> applyEntry n (OVScalar v)) defaultOptions
                cellOf p sets area name =
                    namedLayout (namedSizesOf (with sets)) area name pids >>= paneCell (PaneId p)
                wide = Size { rows = 50, cols = 200 }
                tall = Size { rows = 40, cols = 200 }
            it "main-vertical: main-pane-width cells wide" $
                fmap (.sx) (cellOf 0 [("main-pane-width", "100")] wide MainVertical)
                    `shouldBe` Just 100
            it "main-horizontal: main-pane-height cells tall" $
                fmap (.sy) (cellOf 0 [("main-pane-height", "10")] tall MainHorizontal)
                    `shouldBe` Just 10
            it "main-vertical: a main-pane-width percentage of the window" $
                fmap (.sx) (cellOf 0 [("main-pane-width", "25%")] wide MainVertical)
                    `shouldBe` Just 49
            it "main-vertical: other-pane-width sizes the other panes" $
                fmap (.sx) (cellOf 1 [("other-pane-width", "30")] wide MainVertical)
                    `shouldBe` Just 30

        describe "tiled-layout-max-columns caps the tiled grid's columns" $
            it "1: every pane on its own row" $ do
                let pids = map PaneId [0, 1, 2, 3]
                    opts = applyEntry "tiled-layout-max-columns" (OVScalar "1") defaultOptions
                    lay = namedLayout (namedSizesOf opts) Size { rows = 50, cols = 200 } Tiled pids
                map (\p -> (.xoff) <$> (lay >>= paneCell (PaneId p))) [0 .. 3]
                    `shouldBe` replicate 4 (Just 0)

        -- pane-border-lines picks the glyph set the borders draw.
        describe "pane-border-lines maps the border glyph set" $
            it "single, heavy, double, simple, number, spaces" $
                map (\l -> borderGlyph l (Just 4) CellLR)
                    [ BorderSingle, BorderHeavy, BorderDouble, BorderSimple
                    , BorderNumber, BorderSpaces ]
                    `shouldBe` "\x2500\x2501\x2550-4 "

        -- pane-border-style / -active-border-style: a border takes the
        -- style of the active pane beside it, else of any pane beside it.
        describe "pane-border styles mark the active pane" $ do
            let borderSty = Cell.defaultStyle { Cell.fg = Cell.Indexed 5 }
                activeSty = Cell.defaultStyle { Cell.fg = Cell.Indexed 9 }
                rect r0 r1 c0 c1 = Rect { startRow = r0, endRow = r1, startCol = c0, endCol = c1 }
                input = SceneInput
                    { window = Size { rows = 3, cols = 5 }, area = Size { rows = 3, cols = 5 }
                    , panes = [ (PaneId 0, rect 0 3 0 2), (PaneId 1, rect 0 1 3 5)
                              , (PaneId 2, rect 2 3 3 5) ]
                    , paneStatus = PaneStatusOff, twoPane = Nothing }
                look ind = ChromeLook
                    { borderLines = BorderSingle, windowStyle = Cell.defaultStyle
                    , paneStyles = Map.fromList
                        [ (PaneId 0, borderSty), (PaneId 1, activeSty), (PaneId 2, borderSty) ]
                    , active = Just (PaneId 1), marked = Nothing, numbers = Map.empty
                    , paneLines = Map.empty
                    , fillInside = Cell.blankCell, fillOutside = Cell.blankCell
                    , arrows = ind }
                styleAt p = (.style) <$> lookup p (sceneChrome (look Map.empty) (buildScene input))
            it "a border beside the active pane takes its style" $
                styleAt (Pos { row = 1, col = 4 }) `shouldBe` Just activeSty
            it "a border away from it takes its neighbour's" $
                styleAt (Pos { row = 2, col = 2 }) `shouldBe` Just borderSty
            -- pane-border-indicators: arrows replace the border glyph.
            it "indicator arrows replace the border glyph" $
                (Cell.baseChar . snd <$> List.find ((== Pos 1 2) . fst)
                    (sceneChrome (look (Map.singleton (Pos 1 2) '\x25b6')) (buildScene input)))
                    `shouldBe` Just '\x25b6'

        -- message-line: the status line the prompt and messages take,
        -- clamped to the last line there is.
        describe "message-line picks the message's status line" $ do
            let with st ml = applyEntry "message-line" (OVScalar ml)
                    (applyEntry "status" (OVScalar st) defaultOptions)
            it "takes the given line" $
                messageLine (with "3" "1") `shouldBe` 1
            it "clamps to the last status line" $
                messageLine (with "2" "4") `shouldBe` 1
            it "is the first line by default" $
                messageLine defaultOptions `shouldBe` 0

        -- status-position: bottom puts the bar on the last row with no
        -- content offset; top puts it on row 0 and pushes content down one.
        describe "status-position places the status bar" $ do
            it "bottom (default): last row, no content offset" $
                statusLayout StatusBottom 1 24 `shouldBe` (0, Just 23)
            it "top: row 0, content offset by one" $
                statusLayout StatusTop 1 24 `shouldBe` (1, Just 0)
            it "off (status 0): no bar, content fills the height" $
                statusLayout StatusBottom 0 24 `shouldBe` (0, Nothing)

        describe "window-size picks the resize mode" $ do
            it "latest by default, as in tmux" $
                resizeModeOf defaultOptions `shouldBe` SizeLatest
            it "follows the option" $
                resizeModeOf (applyEntry "window-size" (OVScalar "manual") defaultOptions)
                    `shouldBe` SizeManual

        -- update-environment: on attach, each listed pattern pulls matching
        -- vars from the client's env into the session; a pattern matching
        -- nothing clears that name, masking the stale value.
        describe "update-environment refreshes only the listed vars" $ do
            let clientEnv = [("DISPLAY", ":1"), ("TEST_GLOB", "gv"), ("FOO", "bar")]
                sessEnv = environFromPairs [("DISPLAY", ":0"), ("KEEP", "yes")]
            it "a listed var the client has replaces the session's" $
                environFind "DISPLAY" (environUpdate ["DISPLAY"] clientEnv sessEnv)
                    `shouldBe` Just (EnvEntry (Just ":1") EnvVisible)
            it "an unlisted var is left untouched" $
                environFind "KEEP" (environUpdate ["DISPLAY"] clientEnv sessEnv)
                    `shouldBe` Just (EnvEntry (Just "yes") EnvVisible)
            it "a glob pattern imports every matching client var" $
                environFind "TEST_GLOB" (environUpdate ["TEST_*"] clientEnv sessEnv)
                    `shouldBe` Just (EnvEntry (Just "gv") EnvVisible)
            it "a listed var the client lacks is cleared, not kept" $
                environFind "ABSENT" (environUpdate ["ABSENT"] clientEnv sessEnv)
                    `shouldBe` Just (EnvEntry Nothing EnvVisible)

        -- escape-time: 0 handles a lone trailing ESC at once; non-zero holds
        -- it for coalescing (the coalescing itself is KeysSpec's).
        describe "escape-time picks the lone-ESC timing mode" $ do
            it "0 (default): immediate" $
                escTiming defaultOptions `shouldBe` EscImmediate
            it "non-zero: buffered for coalescing" $
                escTiming (defaultOptions { escapeTime = 500 })
                    `shouldBe` EscBuffered

        -- command-alias: a matching entry re-routes dispatch to its
        -- expansion, with the invocation's own arguments appended.
        describe "command-alias re-routes dispatch" $ do
            let withState body = do
                    lg <- newLogger "/dev/null"
                    st <- newServerState Map.empty lg
                        "/tmp/hat-optioneffectspec.sock" Nothing
                    body st
            it "a set alias runs its expansion with the args appended" $
                withState $ \st -> do
                    _ <- runCommands st Nothing
                        [ [ "set", "-s", "command-alias[100]"
                          , "et=set -s escape-time" ]
                        , ["et", "123"] ]
                    opts <- readTVarIO st.options
                    opts.escapeTime `shouldBe` 123
            it "an alias beats the builtin table, as in tmux" $
                withState $ \st -> do
                    _ <- runCommands st Nothing
                        [ [ "set", "-s", "command-alias[100]"
                          , "start-server=set -s escape-time 9" ]
                        , ["start-server"] ]
                    opts <- readTVarIO st.options
                    opts.escapeTime `shouldBe` 9
            it "an alias expansion never re-expands (no recursion)" $
                withState $ \st -> do
                    _ <- runCommands st Nothing
                        [["set", "-s", "command-alias[100]", "loop=loop"]]
                    rs <- runCommands st Nothing [["loop"]]
                    rs `shouldBe` [RErr "unknown command: loop"]

        -- default-shell / default-command: what a new pane execs, as tmux's
        -- spawn does it.
        describe "default-shell and default-command choose a pane's program" $ do
            it "a fresh pane runs default-shell" $
                paneProgram "/usr/bin/zsh" FreshShell
                    `shouldBe` ("/usr/bin/zsh", [])
            it "a one-string command runs through default-shell -c" $
                paneProgram "/usr/bin/zsh" (ShellCommand "top -d 1")
                    `shouldBe` ("/usr/bin/zsh", ["-c", "top -d 1"])
            it "an argv runs directly" $
                paneProgram "/usr/bin/zsh" (ExecArgv ["vim", "x"])
                    `shouldBe` ("vim", ["x"])
            it "default-command stands in for a missing command" $ do
                shellStart "" Nothing `shouldBe` FreshShell
                shellStart "htop" Nothing `shouldBe` ShellCommand "htop"
                shellStart "htop" (Just "top") `shouldBe` ShellCommand "top"
            it "only an absolute, executable path is a suitable shell" $ do
                suitableShell "/bin/sh" `shouldReturn` True
                suitableShell "sh" `shouldReturn` False
                suitableShell "/not/a/shell" `shouldReturn` False
            it "set refuses an unsuitable default-shell, keeping the old" $ do
                lg <- newLogger "/dev/null"
                st <- newServerState Map.empty lg
                    "/tmp/hat-optioneffectspec.sock" Nothing
                runCommands st Nothing [["set", "-g", "default-shell", "/not/a/shell"]]
                    `shouldReturn` [RErr "not a suitable shell: /not/a/shell"]
                runCommands st Nothing [["set", "-g", "default-shell", "/bin/sh"]]
                    `shouldReturn` []
                opts <- readTVarIO st.options
                optionText "default-shell" opts `shouldBe` "/bin/sh"
