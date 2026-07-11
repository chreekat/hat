module Hat.Server.LayoutSpec (spec) where

import qualified Data.List as List
import qualified Data.Set as Set
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Geometry
import Hat.Model.Ids
import Hat.Server.Layout

-- Random split trees over distinct pane ids, bounded depth.
genLayout :: Int -> Gen Layout
genLayout depth = go depth 0 >>= pure . fst
  where
    go :: Int -> Int -> Gen (Layout, Int)
    go d next
        | d <= 0 = pure (Leaf (PaneId next), next + 1)
        | otherwise = frequency
            [ (1, pure (Leaf (PaneId next), next + 1))
            , (2, do
                o <- elements [LeftRight, TopBottom]
                r <- choose (0.2, 0.8 :: Double)
                (a, n1) <- go (d - 1) next
                (b, n2) <- go (d - 1) n1
                pure (Split o (toRational r) a b, n2))
            ]

windowRect :: Rect
windowRect = Rect { startRow = 0, endRow = 40, startCol = 0, endCol = 120 }

cellsOf :: Rect -> [(Int, Int)]
cellsOf r = [(row, col) | row <- [r.startRow .. r.endRow - 1]
                        , col <- [r.startCol .. r.endCol - 1]]

spec :: Spec
spec = do
    prop "panes and borders tile the window exactly" $
        forAll (genLayout 3) $ \lay ->
            let (rects, borders) = arrange windowRect lay
                paneCells = concatMap (cellsOf . snd) rects
                borderCells = [(p.row, p.col) | (p, _) <- borders]
                allCells = paneCells <> borderCells
            in List.sort allCells === List.sort (List.nub allCells)
                .&&. Set.fromList allCells === Set.fromList (cellsOf windowRect)

    prop "splitting adds exactly the new pane" $
        forAll (genLayout 3) $ \lay ->
            let pids = layoutPanes lay
                newPid = PaneId 999
            in case pids of
                [] -> property Discard
                (target : _) ->
                    let lay' = splitLeaf target LeftRight False newPid lay
                    in Set.fromList (layoutPanes lay')
                        === Set.insert newPid (Set.fromList pids)

    prop "removing a split pane restores the original pane set" $
        forAll (genLayout 3) $ \lay ->
            let pids = layoutPanes lay
                target = last pids
                newPid = PaneId 999
                lay' = splitLeaf target TopBottom True newPid lay
            in fmap (Set.fromList . layoutPanes) (removeLeaf newPid lay')
                === Just (Set.fromList pids)

    prop "removing the only pane empties the layout" $ \n ->
        removeLeaf (PaneId n) (Leaf (PaneId n)) === Nothing

    describe "neighbor" $ do
        -- +-------+-------+
        -- |   0   |   1   |
        -- +-------+-------+
        -- |   2   |   3   |
        -- +-------+-------+
        let quad = Split TopBottom 0.5
                (Split LeftRight 0.5 (Leaf (PaneId 0)) (Leaf (PaneId 1)))
                (Split LeftRight 0.5 (Leaf (PaneId 2)) (Leaf (PaneId 3)))
            (rects, _) = arrange windowRect quad
        it "finds the right neighbor" $
            neighbor rects (PaneId 0) DirRight `shouldBe` Just (PaneId 1)
        it "finds the down neighbor" $
            neighbor rects (PaneId 1) DirDown `shouldBe` Just (PaneId 3)
        it "finds the left neighbor" $
            neighbor rects (PaneId 3) DirLeft `shouldBe` Just (PaneId 2)
        it "finds the up neighbor" $
            neighbor rects (PaneId 2) DirUp `shouldBe` Just (PaneId 0)
        it "returns Nothing at the edge" $
            neighbor rects (PaneId 0) DirLeft `shouldBe` Nothing

    describe "resizeSplit" $ do
        let two = Split LeftRight 0.5 (Leaf (PaneId 0)) (Leaf (PaneId 1))
        it "grows the active pane toward the divider" $ do
            let lay' = resizeSplit (PaneId 0) DirRight 10 windowRect two
                (rects, _) = arrange windowRect lay'
            case List.lookup (PaneId 0) rects of
                Nothing -> expectationFailure "pane 0 has no rect"
                Just r0 -> (r0.endCol - r0.startCol) `shouldSatisfy` (> 59)
        it "keeps every pane at least one cell wide" $ do
            let lay' = iterate (resizeSplit (PaneId 0) DirRight 30 windowRect) two !! 10
                (rects, _) = arrange windowRect lay'
            [r.endCol - r.startCol | (_, r) <- rects]
                `shouldSatisfy` all (>= 1)
