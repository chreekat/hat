module Hat.Server.LayoutSpec (spec) where

import qualified Data.List as List
import Data.Ratio ((%))
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
                cellSet = Set.fromList allCells
            in length allCells === Set.size cellSet  -- no cell covered twice
                .&&. cellSet === Set.fromList (cellsOf windowRect)

    describe "border junctions" $ do
        let smallRect = Rect { startRow = 0, endRow = 5, startCol = 0, endCol = 5 }
            a = PaneId 0; b = PaneId 1; c = PaneId 2; d = PaneId 3
            glyphAt r co lay =
                List.lookup (Pos { row = r, col = co }) (snd (arrange smallRect lay))

        it "joins a divider ending against a perpendicular one with a tee" $
            -- A vertical divider in the top half meets the full-width
            -- horizontal divider from above: ┴ (up + left + right).
            glyphAt 2 2
                (Split TopBottom (1 % 2)
                    (Split LeftRight (1 % 2) (Leaf a) (Leaf b))
                    (Leaf c))
                `shouldBe` Just '\x2534'

        it "joins two dividers crossing with a full cross" $
            -- Both halves split at the same row, straddling the central
            -- vertical divider: ┼ (all four arms).
            glyphAt 2 2
                (Split LeftRight (1 % 2)
                    (Split TopBottom (1 % 2) (Leaf a) (Leaf b))
                    (Split TopBottom (1 % 2) (Leaf c) (Leaf d)))
                `shouldBe` Just '\x253c'

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

    describe "splitFull" $ do
        it "makes the new pane a full-height sibling of the whole tree" $ do
            -- Two stacked panes; a full -h split adds a full-height column
            -- on the right, so its rect spans the entire window height.
            let stacked = Split TopBottom 0.5 (Leaf (PaneId 0)) (Leaf (PaneId 1))
                lay' = splitFull LeftRight False (PaneId 9) stacked
                (rects, _) = arrange windowRect lay'
            case List.lookup (PaneId 9) rects of
                Nothing -> expectationFailure "new pane has no rect"
                Just r -> (r.startRow, r.endRow) `shouldBe` (0, 40)
        it "adds exactly the new pane to the set" $
            forAll (genLayout 3) $ \lay ->
                Set.fromList (layoutPanes (splitFull TopBottom True (PaneId 999) lay))
                    === Set.insert (PaneId 999) (Set.fromList (layoutPanes lay))

    describe "swapLeaves" $ do
        it "exchanges two panes' positions" $ do
            let two = Split LeftRight 0.5 (Leaf (PaneId 0)) (Leaf (PaneId 1))
            swapLeaves (PaneId 0) (PaneId 1) two
                `shouldBe` Split LeftRight 0.5 (Leaf (PaneId 1)) (Leaf (PaneId 0))
        it "preserves the pane set and is its own inverse" $
            forAll (genLayout 3) $ \lay -> case layoutPanes lay of
                (a : b : _) ->
                    swapLeaves a b (swapLeaves a b lay) === lay
                _ -> property Discard

    describe "namedLayout" $ do
        let pids4 = map PaneId [0, 1, 2, 3]
        it "main-vertical gives the main pane the requested width share" $ do
            -- window 120 wide; main share 90 -> main pane ~90 cols on the left
            -- (one column goes to the border between it and the stack).
            let lay = namedLayout MainVertical (90 % 120) pids4
                (rects, _) = arrange windowRect lay
            case List.lookup (PaneId 0) rects of
                Just r -> do
                    r.startCol `shouldBe` 0
                    r.endCol `shouldSatisfy` (\c -> c >= 88 && c <= 90)
                Nothing -> expectationFailure "main pane missing"
        it "preserves the full pane set for every layout" $
            [ Set.fromList (layoutPanes (namedLayout n (1 % 2) pids4))
            | n <- [EvenHorizontal, EvenVertical, MainVertical, MainHorizontal, Tiled] ]
                `shouldSatisfy` all (== Set.fromList pids4)
        it "even-horizontal splits four panes into equal columns" $ do
            let (rects, _) = arrange windowRect (namedLayout EvenHorizontal (1 % 2) pids4)
                widths = [ r.endCol - r.startCol | (_, r) <- rects ]
            maximum widths - minimum widths `shouldSatisfy` (<= 1)

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
