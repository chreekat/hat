module Hat.Server.FloatingSpec (spec) where

import Data.Maybe (fromMaybe)
import Test.Hspec

import Hat.Geometry
import Hat.Model.Ids (PaneId (..))
import Hat.Server.Floating
import Hat.Server.Layout
import Hat.Server.Mouse (ClickOutside (..))

area :: Size
area = Size { rows = 24, cols = 80 }

noArgs :: GeomArgs
noArgs = GeomArgs Nothing Nothing Nothing Nothing

-- One tiled pane and floating panes 1, 2, 3, in that order.
threeFloats :: Layout
threeFloats = fromMaybe (error "no float") $
    foldr (\n acc -> acc >>= floatPane FloatLast (PaneId n) (FloatGeom 5 5 1 1))
        (Just (fitLayout area (singlePane (PaneId 0)))) [3, 2, 1]

spec :: Spec
spec = do
    describe "floatingGeom" $ do
        it "counts the border in the size and position the flags name" $
            fmap fst (floatingGeom area Bordered Nothing (0, 0)
                (GeomArgs (Just "20") (Just "6") (Just "8") (Just "3")))
                `shouldBe` Right (FloatGeom 18 4 9 4)
        it "takes percentages of the window, and nothing for no border" $
            fmap fst (floatingGeom area Borderless Nothing (0, 0)
                (GeomArgs (Just "50%") (Just "50%") (Just "10%") (Just "10%")))
                `shouldBe` Right (FloatGeom 40 12 8 2)
        it "cascades an unplaced pane from the last one" $ do
            floatingGeom area Bordered Nothing (0, 0) noArgs
                `shouldBe` Right (FloatGeom 40 6 4 2, (4, 2))
            fmap snd (floatingGeom area Bordered Nothing (4, 2) noArgs)
                `shouldBe` Right (8, 4)
        it "rejects a box with no room inside its border" $
            floatingGeom area Bordered Nothing (0, 0) noArgs { sizeX = Just "2" }
                `shouldBe` Left "invalid width"

    describe "stacking" $ do
        let fs = noFloats
        it "puts panes it does not name in front, in layout order" $
            stacking fs threeFloats `shouldBe` map PaneId [1, 2, 3]
        it "moves a pane a step, to an end, or to a depth" $ do
            stacking (restack Backward (PaneId 1) fs threeFloats) threeFloats
                `shouldBe` map PaneId [2, 1, 3]
            stacking (restack ToBack (PaneId 1) fs threeFloats) threeFloats
                `shouldBe` map PaneId [2, 3, 1]
            stacking (restack ForwardLoop (PaneId 1) fs threeFloats) threeFloats
                `shouldBe` map PaneId [2, 3, 1]
            stacking (restackTo 1 (PaneId 3) fs threeFloats) threeFloats
                `shouldBe` map PaneId [1, 3, 2]
        it "stacks a new pane behind the modal pane" $ do
            let modal = fs { modal = Just (Modal (PaneId 2) Nothing False KeepModal) }
            stacking (stackNew (PaneId 3) modal threeFloats) threeFloats
                `shouldBe` map PaneId [1, 2, 3]
        it "puts tiled panes behind every floating one" $
            paneZ fs threeFloats (PaneId 0) `shouldBe` 4

    describe "loseModal" $ do
        let open = noFloats { modal = Just (Modal (PaneId 5) (Just (PaneId 0)) True KeepModal) }
        it "hands the window back when the modal pane goes" $
            loseModal (PaneId 5) open
                `shouldBe` (noFloats, Just (ModalReturn (Just (PaneId 0)) True))
        it "has nothing to return to once that pane goes" $
            loseModal (PaneId 0) open
                `shouldBe` (noFloats { modal = Just (Modal (PaneId 5) Nothing False KeepModal) }, Nothing)

    describe "dragNewGeom (new-pane -M)" $ do
        it "spans the drag, its border inside it" $
            dragNewGeom Bordered (2, 0) (9, 4) `shouldBe` FloatGeom 6 3 3 1
        it "dragged up and left, still from the start" $
            dragNewGeom Bordered (9, 4) (2, 0) `shouldBe` FloatGeom 6 3 3 1
        it "a borderless box is the drag itself, never under a cell" $ do
            dragNewGeom Borderless (2, 0) (9, 4) `shouldBe` FloatGeom 8 5 2 0
            dragNewGeom Bordered (2, 0) (3, 0) `shouldBe` FloatGeom 1 1 3 1
    describe "dragFloatGeom (resize-pane -M on a floating pane)" $ do
        let g = FloatGeom { width = 10, height = 4, left = 10, top = 5 }
        it "the right border resizes across" $
            dragFloatGeom g (20, 6) (25, 6) `shouldBe` Just g { width = 15 }
        it "the bottom border resizes down" $
            dragFloatGeom g (12, 9) (12, 11) `shouldBe` Just g { height = 6 }
        it "the left border resizes, moving the left edge" $
            dragFloatGeom g (9, 6) (7, 6) `shouldBe` Just g { width = 12, left = 8 }
        it "a corner resizes both ways" $
            dragFloatGeom g (20, 9) (22, 10) `shouldBe` Just g { width = 12, height = 5 }
        it "the top border moves the box" $
            dragFloatGeom g (14, 4) (16, 2) `shouldBe` Just g { left = 12, top = 3 }
        it "never below a cell" $
            dragFloatGeom g (20, 6) (5, 6) `shouldBe` Nothing
        it "inside the box, nothing" $
            dragFloatGeom g (12, 6) (14, 7) `shouldBe` Nothing

    describe "splitFloatingGeom" $
        it "puts the new box beside the old, sharing the space at the window edge" $
            splitFloatingGeom area Bordered (FloatSplit True False False) (FloatGeom 30 6 40 3)
                `shouldBe` Right (FloatGeom 17 6 40 3, FloatGeom 17 6 59 3)

