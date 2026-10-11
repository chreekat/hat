-- | The executable companion to @docs/options-audit.md@: each option gets a
-- prefix-style effect test that sets a non-default value and asserts the
-- option→behavior mapping it controls — the pure function or single dispatch
-- step that turns the value into a behavioral mode — so a silent no-op (or
-- wrong semantics) fails a test instead of needing another manual audit.
-- The behavior behind each mode lives in its owning spec.
module Hat.Server.OptionEffectSpec (spec) where

import Test.Hspec

import Control.Concurrent.STM (readTVarIO)
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))

import Hat.Geometry (Pos (..), Rect (..), Size (..))
import Hat.Log (newLogger)
import Hat.Model (Markup (..), ServerState (..), Toast (..), newServerState)
import Hat.Model.Options
    ( BorderIndicators (..), BorderLines (..), OptionValue (..), Options (..)
    , StatusPosition (..), WindowSizeMode (..), applyEntry, defaultOptions )
import Hat.Server
    ( Reply (..), deliversKey, escTiming, mainPaneRatio, resizeModeOf
    , runCommands, toastDeadline, toastExpired )
import Hat.Server.Environ
    ( EnvEntry (..), EnvVisibility (..), environFind, environFromPairs
    , environUpdate )
import Hat.Server.Keys (EscTiming (..), Key (..))
import Hat.Server.Layout (LayoutName (..))
import Hat.Server.View (borderCells, mapGlyph, messageLine, statusLayout)
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

        -- main-pane-width/-height: an absolute cell count, realised as that
        -- many cells out of the window along the layout's axis.
        describe "main-pane-width/-height give the main pane an absolute cell count" $ do
            let area = Size { rows = 50, cols = 200 }
            it "main-vertical: main-pane-width cells is that fraction of the cols" $
                mainPaneRatio MainVertical
                    (defaultOptions { mainPaneWidth = 100 }) area `shouldBe` 1 % 2
            it "a wider main-pane-width takes proportionally more (absolute cells)" $
                mainPaneRatio MainVertical
                    (defaultOptions { mainPaneWidth = 160 }) area `shouldBe` 4 % 5
            it "main-horizontal: main-pane-height cells is that fraction of the rows" $
                mainPaneRatio MainHorizontal
                    (defaultOptions { mainPaneHeight = 10 })
                    (Size { rows = 40, cols = 200 }) `shouldBe` 1 % 4

        -- pane-border-lines picks the box-drawing glyph set the borders draw.
        describe "pane-border-lines maps the border glyph set" $ do
            let horiz = '\x2500'  -- ─
            it "single leaves the glyph, heavy/double/simple remap it" $ do
                mapGlyph BorderSingle horiz `shouldBe` '\x2500'  -- ─
                mapGlyph BorderHeavy  horiz `shouldBe` '\x2501'  -- ━
                mapGlyph BorderDouble horiz `shouldBe` '\x2550'  -- ═
                mapGlyph BorderSimple horiz `shouldBe` '-'

        -- pane-border-style / -active-border-style: the active pane's
        -- perimeter takes the active style, every other border the plain one.
        describe "pane-border styles mark the active pane" $ do
            let borderSty = Cell.defaultStyle { Cell.fg = Cell.Indexed 5 }
                activeSty = Cell.defaultStyle { Cell.fg = Cell.Indexed 9 }
                opts = defaultOptions
                    { paneBorderStyle = borderSty
                    , paneActiveBorderStyle = activeSty
                    , paneBorderIndicators = IndicatorsColour }
                rect = Rect { startRow = 0, endRow = 2, startCol = 1, endCol = 3 }
                cells = borderCells opts (Just rect) 0
                    [ (Pos { row = 0, col = 0 }, '\x2502')    -- on the perimeter
                    , (Pos { row = 5, col = 5 }, '\x2502') ]  -- off it
                styleAt p = (.style) <$> lookup p cells
            it "an active-perimeter cell takes pane-active-border-style" $
                styleAt (Pos { row = 0, col = 0 }) `shouldBe` Just activeSty
            it "a non-perimeter cell takes pane-border-style" $
                styleAt (Pos { row = 5, col = 5 }) `shouldBe` Just borderSty

        -- pane-border-indicators: arrows draws an inward arrow on the active
        -- edge; off leaves the plain line glyph.
        describe "pane-border-indicators toggles the active-edge arrow" $ do
            let rect = Rect { startRow = 0, endRow = 2, startCol = 1, endCol = 3 }
                arrowPos = Pos { row = 1, col = 0 }  -- midpoint of the left edge
                glyphWith ind = Cell.baseChar <$> lookup arrowPos
                    (borderCells (defaultOptions { paneBorderIndicators = ind })
                        (Just rect) 0 [ (arrowPos, '\x2502') ])
            it "arrows: the active edge shows an arrow" $
                glyphWith IndicatorsArrows `shouldBe` Just '\x25b6'  -- ▶
            it "off: the active edge keeps the plain line" $
                glyphWith IndicatorsOff `shouldBe` Just '\x2502'  -- │

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
