{-# OPTIONS_GHC -Wno-orphans #-}  -- Arbitrary PaneCycle lives with its test: the library must not depend on QuickCheck

module Hat.Server.LayoutSpec (spec) where

import qualified Data.List as List
import Data.Ratio ((%))
import qualified Data.Set as Set
import qualified Data.Text as T
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

-- Shrink toward 'PaneNext' so a failing 'PanePrev' case also reports as
-- 'PaneNext' if that simpler case reproduces it.
instance Arbitrary PaneCycle where
    arbitrary = elements [PaneNext, PanePrev]
    shrink PaneNext = []
    shrink PanePrev = [PaneNext]

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
                    let lay' = splitLeaf target LeftRight After newPid lay
                    in Set.fromList (layoutPanes lay')
                        === Set.insert newPid (Set.fromList pids)

    prop "removing a split pane restores the original pane set" $
        forAll (genLayout 3) $ \lay ->
            let pids = layoutPanes lay
                target = last pids
                newPid = PaneId 999
                lay' = splitLeaf target TopBottom Before newPid lay
            in fmap (Set.fromList . layoutPanes) (removeLeaf newPid lay')
                === Just (Set.fromList pids)

    prop "removing the only pane empties the layout" $ \n ->
        removeLeaf (PaneId n) (Leaf (PaneId n)) === Nothing

    describe "splitFull" $ do
        it "makes the new pane a full-height sibling of the whole tree" $ do
            -- Two stacked panes; a full -h split adds a full-height column
            -- on the right, so its rect spans the entire window height.
            let stacked = Split TopBottom 0.5 (Leaf (PaneId 0)) (Leaf (PaneId 1))
                lay' = splitFull LeftRight After (PaneId 9) stacked
                (rects, _) = arrange windowRect lay'
            case List.lookup (PaneId 9) rects of
                Nothing -> expectationFailure "new pane has no rect"
                Just r -> (r.startRow, r.endRow) `shouldBe` (0, 40)
        it "adds exactly the new pane to the set" $
            forAll (genLayout 3) $ \lay ->
                Set.fromList (layoutPanes (splitFull TopBottom Before (PaneId 999) lay))
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

    describe "nextLayoutName" $ do
        it "starts the cycle at even-horizontal when none is set" $
            nextLayoutName Nothing `shouldBe` EvenHorizontal
        it "advances through tmux's layout order" $
            map (nextLayoutName . Just)
                [EvenHorizontal, EvenVertical, MainHorizontal, MainVertical]
                `shouldBe` [EvenVertical, MainHorizontal, MainVertical, Tiled]
        it "wraps from the last layout back to the first" $
            nextLayoutName (Just Tiled) `shouldBe` EvenHorizontal

    describe "previousLayoutName" $ do
        it "starts at the last layout when none is set" $
            previousLayoutName Nothing `shouldBe` Tiled
        it "steps backward through the cycle" $
            previousLayoutName (Just EvenVertical) `shouldBe` EvenHorizontal
        it "wraps from the first layout back to the last" $
            previousLayoutName (Just EvenHorizontal) `shouldBe` Tiled

    describe "cyclePane" $ do
        let three = map PaneId [0, 1, 2]
        it "moves to the next pane in order" $
            cyclePane PaneNext three (PaneId 0) `shouldBe` Just (PaneId 1)
        it "wraps forward past the last pane" $
            cyclePane PaneNext three (PaneId 2) `shouldBe` Just (PaneId 0)
        it "wraps backward before the first pane" $
            cyclePane PanePrev three (PaneId 0) `shouldBe` Just (PaneId 2)
        it "stays put with a single pane" $
            cyclePane PaneNext [PaneId 7] (PaneId 7) `shouldBe` Just (PaneId 7)
        it "has nowhere to go when the pane is absent" $
            cyclePane PaneNext three (PaneId 9) `shouldBe` Nothing

    describe "parsePaneIndex" $ do
        it "parses the bare next form" $
            parsePaneIndex ":.+" `shouldBe` Just (IndexRelative PaneNext 1)
        it "parses the bare previous form" $
            parsePaneIndex ":.-" `shouldBe` Just (IndexRelative PanePrev 1)
        it "parses a forward count" $
            parsePaneIndex ":.+2" `shouldBe` Just (IndexRelative PaneNext 2)
        it "parses a backward count" $
            parsePaneIndex ":.-2" `shouldBe` Just (IndexRelative PanePrev 2)
        it "parses the dotless next form" $
            parsePaneIndex "+" `shouldBe` Just (IndexRelative PaneNext 1)
        it "parses the dotless previous form" $
            parsePaneIndex "-" `shouldBe` Just (IndexRelative PanePrev 1)
        it "parses a dotless forward count" $
            parsePaneIndex "+3" `shouldBe` Just (IndexRelative PaneNext 3)
        it "parses the dot-prefixed absolute form" $
            parsePaneIndex ":.3" `shouldBe` Just (IndexAbsolute 3)
        it "parses a dotless absolute form" $
            parsePaneIndex "3" `shouldBe` Just (IndexAbsolute 3)
        it "parses a window-and-pane dotted absolute form" $
            parsePaneIndex "mywin.2" `shouldBe` Just (IndexAbsolute 2)
        it "parses a window-and-pane dotted relative form" $
            parsePaneIndex "mywin.+2" `shouldBe` Just (IndexRelative PaneNext 2)
        it "rejects a trailing non-number" $
            parsePaneIndex ":.+x" `shouldBe` Nothing
        it "rejects an empty target" $
            parsePaneIndex "" `shouldBe` Nothing
        prop "round-trips a rendered relative target with an explicit count" $
            \(Positive n) dir ->
                let t = case dir of PaneNext -> "+"; PanePrev -> "-"
                in parsePaneIndex (":." <> t <> T.pack (show (n :: Int)))
                    === Just (IndexRelative dir n)

    describe "resolvePaneIndex" $ do
        let three = map PaneId [0, 1, 2]
        it "cycles forward by one for the next form" $
            resolvePaneIndex (IndexRelative PaneNext 1) three (PaneId 0)
                `shouldBe` Just (PaneId 1)
        it "cycles forward by a count, wrapping" $
            resolvePaneIndex (IndexRelative PaneNext 2) three (PaneId 2)
                `shouldBe` Just (PaneId 1)
        it "cycles backward by a count, wrapping" $
            resolvePaneIndex (IndexRelative PanePrev 2) three (PaneId 0)
                `shouldBe` Just (PaneId 1)
        it "selects an absolute positional index" $
            resolvePaneIndex (IndexAbsolute 2) three (PaneId 0)
                `shouldBe` Just (PaneId 2)
        it "has nowhere to go for an out-of-range absolute index" $
            resolvePaneIndex (IndexAbsolute 5) three (PaneId 0)
                `shouldBe` Nothing

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
        it "wraps around at the left edge to the rightmost pane in the row" $
            neighbor rects (PaneId 0) DirLeft `shouldBe` Just (PaneId 1)
        it "wraps around at the right edge to the leftmost pane in the row" $
            neighbor rects (PaneId 1) DirRight `shouldBe` Just (PaneId 0)
        it "wraps around at the top edge to the bottommost pane in the column" $
            neighbor rects (PaneId 0) DirUp `shouldBe` Just (PaneId 2)
        it "wraps around at the bottom edge to the topmost pane in the column" $
            neighbor rects (PaneId 2) DirDown `shouldBe` Just (PaneId 0)
        it "stays put with a single pane" $
            neighbor [(PaneId 5, windowRect)] (PaneId 5) DirLeft `shouldBe` Nothing

    describe "directionalTarget" $ do
        -- Resolving against the full layout (not a zoom-collapsed one) is
        -- what lets prefix+hjkl move away from a zoomed pane. See bug 5.
        let two = Split LeftRight 0.5 (Leaf (PaneId 0)) (Leaf (PaneId 1))
            sz = Size { rows = 24, cols = 80 }
        it "moves to the neighbor computed from the full split layout" $
            directionalTarget sz two (PaneId 0) DirRight `shouldBe` Just (PaneId 1)

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

    describe "effectiveWindowSize (aggressive-resize)" $ do
        let big = Size { rows = 50, cols = 200 }
            small = Size { rows = 24, cols = 80 }
            fallback = Size { rows = 30, cols = 100 }
        it "shrinks to the smallest client without aggressive resize" $
            effectiveWindowSize SmallestClient fallback [(1, big), (2, small)]
                `shouldBe` small
        it "intersects each dimension independently" $
            effectiveWindowSize SmallestClient fallback
                [ (1, Size { rows = 24, cols = 200 })
                , (2, Size { rows = 50, cols = 80 }) ]
                `shouldBe` Size { rows = 24, cols = 80 }
        it "follows the most-recently-active client when aggressive" $
            -- stamp 2 (big) is newer than stamp 1 (small): take big and let
            -- the smaller client clip. Hence "aggressive".
            effectiveWindowSize ActiveClient fallback [(1, small), (2, big)]
                `shouldBe` big
        it "follows the active client even when it is the smaller one" $
            effectiveWindowSize ActiveClient fallback [(2, big), (5, small)]
                `shouldBe` small
        it "keeps the fallback size when no client is attached" $ do
            effectiveWindowSize SmallestClient fallback [] `shouldBe` fallback
            effectiveWindowSize ActiveClient fallback [] `shouldBe` fallback
