module Hat.Server.ScrollbarsSpec (spec) where

import Test.Hspec

import Hat.Geometry (Rect (..))
import Hat.Server.Scene (ScrollbarSide (..))
import Hat.Server.Scrollbars

spec :: Spec
spec = do
    describe "overlaysScrollbar" $ do
        let normal = PaneScreen { alternate = False, inMode = False }
            copying = normal { inMode = True }
            alt = normal { alternate = True }
        it "auto-hide: over any pane off its alternate screen" $
            map (overlaysScrollbar ScrollbarsAutoHide) [normal, copying, alt]
                `shouldBe` [True, True, False]
        it "modal: only over a pane in a mode" $
            map (overlaysScrollbar ScrollbarsModal) [normal, copying]
                `shouldBe` [False, True]
        it "on and off: never" $
            map (\m -> overlaysScrollbar m copying) [ScrollbarsOn, ScrollbarsOff]
                `shouldBe` [False, False]
    describe "inScrollbarArea" $ do
        let r = Rect { startRow = 2, endRow = 5, startCol = 10, endCol = 20 }
        it "is the width and padding at the pane's edge" $ do
            map (\x -> inScrollbarArea ScrollbarRight (1, 1) r (x, 3)) [17, 18, 19]
                `shouldBe` [False, True, True]
            map (\x -> inScrollbarArea ScrollbarLeft (1, 1) r (x, 3)) [10, 11, 12]
                `shouldBe` [True, True, False]
        it "only beside the pane's rows" $
            inScrollbarArea ScrollbarRight (1, 0) r (19, 5) `shouldBe` False
        it "the whole pane when it does not fit" $
            inScrollbarArea ScrollbarRight (8, 4) r (10, 2) `shouldBe` True
    describe "coversColumn" $
        it "the bar's columns hide the cursor, its padding does not" $ do
            map (coversColumn ScrollbarRight 2 10) [7, 8, 9] `shouldBe` [False, True, True]
            map (coversColumn ScrollbarLeft 2 10) [0, 1, 2] `shouldBe` [True, True, False]
    describe "scrollShowsScrollbar" $ do
        it "a scroll that moves the view" $
            scrollShowsScrollbar "scroll-up" 0 1 `shouldBe` True
        it "a motion that leaves it" $
            scrollShowsScrollbar "cursor-up" 4 4 `shouldBe` False
        it "a page scroll, moved or not" $
            scrollShowsScrollbar "page-up" 100 100 `shouldBe` True
