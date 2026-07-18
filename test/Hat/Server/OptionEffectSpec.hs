-- | The executable companion to @docs/options-audit.md@: each option gets a
-- prefix-style effect test that sets a non-default value and asserts the
-- observable behavior the value controls, so a silent no-op (or wrong
-- semantics) fails a test instead of needing another manual audit.
--
-- Options whose consumer still needs a pure seam extracted (see the audit's
-- @needs seam@ column) are not yet here; the known defects are pinned as
-- pending specs below until they are fixed.
module Hat.Server.OptionEffectSpec (spec) where

import Test.Hspec

import Data.Ratio ((%))
import qualified Data.Text as T
import qualified Data.Vector as V

import Hat.Geometry (Pos (..), Rect (..), Size (..))
import Hat.Model.Options
    ( BorderIndicators (..), BorderLines (..), Options (..)
    , StatusPosition (..), defaultOptions )
import Hat.Server
    (applyUpdateEnvironment, deliversKey, mainPaneRatio, resizeModeOf)
import Hat.Server.Keys (Key (..))
import Hat.Server.Layout (LayoutName (..), ResizeMode (..))
import Hat.Server.View
    ( assembleStatusRow, borderCells, mapGlyph, statusLayout
    , windowEntryFormat, windowEntryStyle )
import qualified Hat.Term.Cell as Cell

spec :: Spec
spec = do
    describe "option effects (see docs/options-audit.md)" $ do
        -- focus-events: a FocusIn/FocusOut report reaches the pane only when
        -- focus-events is on; with it off the report is swallowed.
        describe "focus-events gates FocusIn/FocusOut delivery" $ do
            let focusIn = Key { name = "FocusIn", raw = "\ESC[I" }
            it "off (default): the focus report is dropped" $
                deliversKey defaultOptions True focusIn `shouldBe` False
            it "on: the focus report is delivered" $
                deliversKey (defaultOptions { focusEvents = True }) True focusIn
                    `shouldBe` True
            it "on but pane never asked (?1004 off): still dropped" $
                deliversKey (defaultOptions { focusEvents = True }) False focusIn
                    `shouldBe` False

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
                glyphWith ind = (.text) <$> lookup arrowPos
                    (borderCells (defaultOptions { paneBorderIndicators = ind })
                        (Just rect) 0 [ (arrowPos, '\x2502') ])
            it "arrows: the active edge shows an arrow" $
                glyphWith IndicatorsArrows `shouldBe` Just "\x25b6"  -- ▶
            it "off: the active edge keeps the plain line" $
                glyphWith IndicatorsOff `shouldBe` Just "\x2502"  -- │

        -- window-status-style / -current-style / -bell-style: which style a
        -- window's entry in the status bar takes.
        describe "window-status styles pick the per-window entry style" $ do
            let cur    = Cell.defaultStyle { Cell.fg = Cell.Indexed 1 }
                bellS  = Cell.defaultStyle { Cell.fg = Cell.Indexed 2 }
                normal = Cell.defaultStyle { Cell.fg = Cell.Indexed 3 }
                opts = defaultOptions
                    { windowStatusCurrentStyle = cur
                    , windowStatusBellStyle = bellS
                    , windowStatusStyle = normal }
            it "the current window takes -current-style (even with a bell)" $ do
                windowEntryStyle opts True False `shouldBe` cur
                windowEntryStyle opts True True `shouldBe` cur
            it "a non-current window with a bell takes -bell-style" $
                windowEntryStyle opts False True `shouldBe` bellS
            it "any other window takes window-status-style" $
                windowEntryStyle opts False False `shouldBe` normal

        -- window-status-format / -current-format: which format string a
        -- window's entry renders through (the rendering itself is FormatSpec).
        describe "window-status-format picks the per-window entry format" $ do
            let opts = defaultOptions
                    { windowStatusCurrentFormat = "CUR #W"
                    , windowStatusFormat = "OTH #W" }
            it "the current window renders window-status-current-format" $
                windowEntryFormat opts True `shouldBe` "CUR #W"
            it "any other window renders window-status-format" $
                windowEntryFormat opts False `shouldBe` "OTH #W"

        -- status-left-length / status-right-length / status-style: the bar caps
        -- each side's text and pads with the status style.
        describe "status-*-length cap the bar text and status-style pads it" $ do
            let barSty = Cell.defaultStyle { Cell.bg = Cell.Indexed 4 }
                opts = defaultOptions
                    { statusStyle = barSty
                    , statusLeftLength = 3
                    , statusRightLength = 2 }
                row = assembleStatusRow opts 10 "HELLO" "WORLD" []
                rowText = T.concat (map (.text) (V.toList row))
            it "status-left-length caps the left text" $
                T.take 3 rowText `shouldBe` "HEL"
            it "status-right-length caps the right text" $
                T.takeEnd 2 rowText `shouldBe` "WO"
            it "status-style paints the padding" $
                (.style) (row V.! 5) `shouldBe` barSty

        -- status-position: bottom puts the bar on the last row with no
        -- content offset; top puts it on row 0 and pushes content down one.
        describe "status-position places the status bar" $ do
            it "bottom (default): last row, no content offset" $
                statusLayout StatusBottom 24 `shouldBe` (0, 23)
            it "top: row 0, content offset by one" $
                statusLayout StatusTop 24 `shouldBe` (1, 0)

        -- aggressive-resize: on follows the active client, off fits the
        -- smallest.
        describe "aggressive-resize picks the resize mode" $ do
            it "off (default): fit the smallest client" $
                resizeModeOf defaultOptions `shouldBe` SmallestClient
            it "on: follow the active client" $
                resizeModeOf (defaultOptions { aggressiveResize = True })
                    `shouldBe` ActiveClient

        -- update-environment: on attach, the listed vars are pulled from the
        -- client's env into the session; others are left alone.
        describe "update-environment refreshes only the listed vars" $ do
            let clientEnv = [("DISPLAY", ":1"), ("FOO", "bar")]
                sessEnv = [("DISPLAY", ":0"), ("KEEP", "yes")]
            it "a listed var the client has replaces the session's" $
                lookup "DISPLAY" (applyUpdateEnvironment ["DISPLAY"] clientEnv sessEnv)
                    `shouldBe` Just ":1"
            it "an unlisted var is left untouched" $
                lookup "KEEP" (applyUpdateEnvironment ["DISPLAY"] clientEnv sessEnv)
                    `shouldBe` Just "yes"
            it "a listed var the client lacks leaves the session as-is" $
                lookup "DISPLAY" (applyUpdateEnvironment ["MISSING"] clientEnv sessEnv)
                    `shouldBe` Just ":0"

        -- Defects catalogued in docs/options-audit.md, pinned here as the
        -- executable backlog. Each flips to a real assertion when fixed.
        describe "defects (pending until fixed)" $
            it "escape-time delays a lone trailing ESC by escape-time ms" $
                pendingWith
                    "no consumer: ESC disambiguation is hardcoded to \
                    \escape-time 0 (see audit defect 1)"
