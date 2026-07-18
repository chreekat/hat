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

import Hat.Model.Options (Options (..), defaultOptions)
import Hat.Server (deliversKey)
import Hat.Server.Keys (Key (..))

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

        -- Defects catalogued in docs/options-audit.md, pinned here as the
        -- executable backlog. Each flips to a real assertion when fixed.
        describe "defects (pending until fixed)" $ do
            it "escape-time delays a lone trailing ESC by escape-time ms" $
                pendingWith
                    "no consumer: ESC disambiguation is hardcoded to \
                    \escape-time 0 (see audit defect 1)"
            it "main-pane-width sizes the main pane to an absolute cell count" $
                pendingWith
                    "wrong semantics: treated as a proportion of window width, \
                    \not absolute cells (see audit defect 2)"
            it "main-pane-height sizes the main pane to an absolute cell count" $
                pendingWith
                    "wrong semantics: treated as a proportion of window height, \
                    \not absolute cells (see audit defect 3)"
