module Hat.Server.ViewportSpec (spec) where

import Test.Hspec

import Hat.Geometry (Pos (..), Size (..))
import Hat.Model (Viewport (..), WindowId (..), unpannedViewport)
import Hat.Server.Viewport (Pan (..), panViewport, viewportFor)

w1, w2 :: WindowId
w1 = WindowId 1
w2 = WindowId 2

big, client :: Size
big = Size { rows = 20, cols = 60 }
client = Size { rows = 14, cols = 40 }

at :: Int -> Int -> Pos
at r c = Pos { row = r, col = c }

spec :: Spec
spec = do
    describe "viewportFor" $ do
        it "a window that fits is drawn whole from its top-left, unpinning" $
            viewportFor w1 Size { rows = 9, cols = 28 } client (Just (at 5 5))
                unpannedViewport { pinned = Just w1, origin = at 3 3 }
                `shouldBe` Viewport (at 0 0) Size { rows = 9, cols = 28 } Nothing
        it "follows the cursor across a larger window" $ do
            (.origin) (viewportFor w1 big client (Just (at 2 3)) unpannedViewport)
                `shouldBe` at 0 0
            (.origin) (viewportFor w1 big client (Just (at 15 30)) unpannedViewport)
                `shouldBe` at 6 0
            (.origin) (viewportFor w1 big client (Just (at 19 59)) unpannedViewport)
                `shouldBe` at 6 20
            let huge = Size { rows = 100, cols = 200 }
            (.origin) (viewportFor w1 huge client (Just (at 50 100)) unpannedViewport)
                `shouldBe` at 37 80
        it "without a visible cursor shows the top-left" $
            viewportFor w1 big client Nothing unpannedViewport
                `shouldBe` Viewport (at 0 0) client Nothing
        it "keeps a pan on its own window, clamped, and ignores the cursor" $ do
            let panned = unpannedViewport { origin = at 30 50, pinned = Just w1 }
            viewportFor w1 big client (Just (at 0 0)) panned
                `shouldBe` Viewport (at 6 20) client (Just w1)
        it "drops a pan made on another window" $
            viewportFor w2 big client Nothing
                unpannedViewport { origin = at 6 20, pinned = Just w1 }
                `shouldBe` Viewport (at 0 0) client Nothing

    describe "panViewport" $ do
        let shown = Viewport (at 4 10) client Nothing
        it "starts from the drawn origin and pins the window" $
            panViewport (PanUp 1) w1 big shown
                `shouldBe` Viewport (at 3 10) client (Just w1)
        it "stops at the window's edges" $ do
            (.origin) (panViewport (PanUp 100) w1 big shown) `shouldBe` at 0 10
            (.origin) (panViewport (PanLeft 100) w1 big shown) `shouldBe` at 4 0
            (.origin) (panViewport (PanDown 100) w1 big shown) `shouldBe` at 6 10
            (.origin) (panViewport (PanRight 100) w1 big shown) `shouldBe` at 4 20
        it "continues an existing pan on the same window" $ do
            let pinnedV = Viewport (at 2 2) client (Just w1)
            (.origin) (panViewport (PanRight 3) w1 big pinnedV) `shouldBe` at 2 5
        it "-c returns the view to following the cursor" $
            panViewport Unpan w1 big (Viewport (at 2 2) client (Just w1))
                `shouldBe` Viewport (at 2 2) client Nothing
