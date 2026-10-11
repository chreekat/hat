module Hat.Server.FloatingSpec (spec) where

import Data.Maybe (fromMaybe)
import Test.Hspec

import Hat.Geometry
import Hat.Model.Ids (PaneId (..))
import Hat.Server.Floating
import Hat.Server.Layout

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
            let modal = fs { modal = Just (Modal (PaneId 2) Nothing False) }
            stacking (stackNew (PaneId 3) modal threeFloats) threeFloats
                `shouldBe` map PaneId [1, 2, 3]
        it "puts tiled panes behind every floating one" $
            paneZ fs threeFloats (PaneId 0) `shouldBe` 4

    describe "loseModal" $ do
        let open = noFloats { modal = Just (Modal (PaneId 5) (Just (PaneId 0)) True) }
        it "hands the window back when the modal pane goes" $
            loseModal (PaneId 5) open
                `shouldBe` (noFloats, Just (ModalReturn (Just (PaneId 0)) True))
        it "has nothing to return to once that pane goes" $
            loseModal (PaneId 0) open
                `shouldBe` (noFloats { modal = Just (Modal (PaneId 5) Nothing False) }, Nothing)

    describe "splitFloatingGeom" $
        it "puts the new box beside the old, sharing the space at the window edge" $
            splitFloatingGeom area Bordered (FloatSplit True False False) (FloatGeom 30 6 40 3)
                `shouldBe` Right (FloatGeom 17 6 40 3, FloatGeom 17 6 59 3)

