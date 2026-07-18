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

import Hat.Geometry (Size (..))
import Hat.Model.Options (Options (..), defaultOptions)
import Hat.Server (deliversKey, mainPaneRatio)
import Hat.Server.Keys (Key (..))
import Hat.Server.Layout (LayoutName (..))

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

        -- Defects catalogued in docs/options-audit.md, pinned here as the
        -- executable backlog. Each flips to a real assertion when fixed.
        describe "defects (pending until fixed)" $
            it "escape-time delays a lone trailing ESC by escape-time ms" $
                pendingWith
                    "no consumer: ESC disambiguation is hardcoded to \
                    \escape-time 0 (see audit defect 1)"
