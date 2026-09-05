{-# OPTIONS_GHC -Wno-orphans #-}  -- Arbitrary PaneCycle lives with its test: the library must not depend on QuickCheck

module Hat.Server.LayoutSpec (spec) where

import Control.Monad (forM_)
import Data.List qualified as List
import Data.Ratio ((%))
import Data.Set qualified as Set
import Data.Text qualified as T
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

-- Small enough to keep the tiling walk cheap, big enough that every
-- depth-3 split of 'genLayout' ratios keeps at least one cell per side.
tileRect :: Rect
tileRect = Rect { startRow = 0, endRow = 14, startCol = 0, endCol = 30 }

cellsOf :: Rect -> [(Int, Int)]
cellsOf r = [(row, col) | row <- [r.startRow .. r.endRow - 1]
                        , col <- [r.startCol .. r.endCol - 1]]

spec :: Spec
spec = do
    prop "panes and borders tile the window exactly" $
        forAll (genLayout 3) $ \lay ->
            let (rects, borders) = arrange tileRect lay
                paneCells = concatMap (cellsOf . snd) rects
                borderCells = [(p.row, p.col) | (p, _) <- borders]
                allCells = paneCells <> borderCells
                cellSet = Set.fromList allCells
            in length allCells === Set.size cellSet  -- no cell covered twice
                .&&. cellSet === Set.fromList (cellsOf tileRect)

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

    describe "nextLayoutName/previousLayoutName" $ do
        -- tmux's layout cycle pinned from every point in both directions:
        -- Nothing (no layout set yet) enters at either end; the ends wrap.
        forM_
            [ (Nothing, EvenHorizontal, Tiled)
            , (Just EvenHorizontal, EvenVertical, Tiled)
            , (Just EvenVertical, MainHorizontal, EvenHorizontal)
            , (Just MainHorizontal, MainVertical, EvenVertical)
            , (Just MainVertical, Tiled, MainHorizontal)
            , (Just Tiled, EvenHorizontal, MainVertical)
            ] $ \(from, next, previous) ->
            it (show from <> ": next " <> show next
                    <> ", previous " <> show previous) $ do
                nextLayoutName from `shouldBe` next
                previousLayoutName from `shouldBe` previous

    describe "cyclePane" $ do
        let three = map PaneId [0, 1, 2]
        forM_
            [ ("moves to the next pane in order",
                PaneNext, three, PaneId 0, Just (PaneId 1))
            , ("wraps forward past the last pane",
                PaneNext, three, PaneId 2, Just (PaneId 0))
            , ("wraps backward before the first pane",
                PanePrev, three, PaneId 0, Just (PaneId 2))
            , ("stays put with a single pane",
                PaneNext, [PaneId 7], PaneId 7, Just (PaneId 7))
            , ("has nowhere to go when the pane is absent",
                PaneNext, three, PaneId 9, Nothing)
            ] $ \(name, dir, ps, from, expected) ->
            it name $ cyclePane dir ps from `shouldBe` expected

    describe "parsePaneIndex" $ do
        -- The relative/absolute pane part of a target, with or without the
        -- ":."/window prefix; garbage and emptiness parse to Nothing.
        forM_
            [ ("the bare next form", ":.+", Just (IndexRelative PaneNext 1))
            , ("the bare previous form", ":.-", Just (IndexRelative PanePrev 1))
            , ("a forward count", ":.+2", Just (IndexRelative PaneNext 2))
            , ("a backward count", ":.-2", Just (IndexRelative PanePrev 2))
            , ("the dotless next form", "+", Just (IndexRelative PaneNext 1))
            , ("the dotless previous form", "-", Just (IndexRelative PanePrev 1))
            , ("a dotless forward count", "+3", Just (IndexRelative PaneNext 3))
            , ("the dot-prefixed absolute form", ":.3", Just (IndexAbsolute 3))
            , ("a dotless absolute form", "3", Just (IndexAbsolute 3))
            , ("a window-and-pane dotted absolute form", "mywin.2",
                Just (IndexAbsolute 2))
            , ("a window-and-pane dotted relative form", "mywin.+2",
                Just (IndexRelative PaneNext 2))
            , ("a trailing non-number", ":.+x", Nothing)
            , ("an empty target", "", Nothing)
            ] $ \(name, t, expected) ->
            it (maybe "rejects " (const "parses ") expected <> name) $
                parsePaneIndex t `shouldBe` expected
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
        -- Adjacent panes in each direction; at an edge, wrap to the far
        -- side of the same row or column.
        forM_
            [ ("finds the right neighbor", PaneId 0, DirRight, PaneId 1)
            , ("finds the down neighbor", PaneId 1, DirDown, PaneId 3)
            , ("finds the left neighbor", PaneId 3, DirLeft, PaneId 2)
            , ("finds the up neighbor", PaneId 2, DirUp, PaneId 0)
            , ("wraps at the left edge to the rightmost pane in the row",
                PaneId 0, DirLeft, PaneId 1)
            , ("wraps at the right edge to the leftmost pane in the row",
                PaneId 1, DirRight, PaneId 0)
            , ("wraps at the top edge to the bottommost pane in the column",
                PaneId 0, DirUp, PaneId 2)
            , ("wraps at the bottom edge to the topmost pane in the column",
                PaneId 2, DirDown, PaneId 0)
            ] $ \(name, from, dir, expected) ->
            it name $ neighbor rects from dir `shouldBe` Just expected
        it "stays put with a single pane" $
            neighbor [(PaneId 5, windowRect)] (PaneId 5) DirLeft `shouldBe` Nothing

        -- +---+---+---+
        -- |   |   | 2 |
        -- | 0 | 1 +---+
        -- |   |   | 3 |
        -- +---+---+---+
        -- Bug 3b: a wrap crosses the whole window, so every column stays
        -- reachable by repeating the same direction.
        let columns = Split LeftRight (1 % 3)
                (Leaf (PaneId 0))
                (Split LeftRight 0.5
                    (Leaf (PaneId 1))
                    (Split TopBottom 0.5 (Leaf (PaneId 2)) (Leaf (PaneId 3))))
            (colRects, _) = arrange windowRect columns
        it "wraps past a middle column to the far column" $
            neighbor colRects (PaneId 0) DirLeft `shouldBe` Just (PaneId 2)
        it "wraps past a middle column back to the near column" $
            neighbor colRects (PaneId 2) DirRight `shouldBe` Just (PaneId 0)

    describe "directionalTarget" $ do
        -- Resolving against the full layout (not a zoom-collapsed one) is
        -- what lets prefix+hjkl move away from a zoomed pane. See bug 5.
        let two = Split LeftRight 0.5 (Leaf (PaneId 0)) (Leaf (PaneId 1))
            sz = Size { rows = 24, cols = 80 }
        it "moves to the neighbor computed from the full split layout" $
            directionalTarget sz two (PaneId 0) DirRight `shouldBe` Just (PaneId 1)

    describe "resizeSplit" $ do
        let two = Split LeftRight 0.5 (Leaf (PaneId 0)) (Leaf (PaneId 1))
            column = Split TopBottom 0.5 (Leaf (PaneId 0)) (Leaf (PaneId 1))
            paneRect pid lay = List.lookup pid (fst (arrange windowRect lay))
            widthOf pid lay = fmap (\r -> r.endCol - r.startCol) (paneRect pid lay)
            heightOf pid lay = fmap (\r -> r.endRow - r.startRow) (paneRect pid lay)
        it "moves the divider right from either side of it" $ do
            widthOf (PaneId 0) (resizeSplit (PaneId 0) DirRight 10 windowRect two)
                `shouldBe` Just 70
            widthOf (PaneId 0) (resizeSplit (PaneId 1) DirRight 10 windowRect two)
                `shouldBe` Just 70
        it "moves the divider left from either side of it" $ do
            widthOf (PaneId 0) (resizeSplit (PaneId 0) DirLeft 10 windowRect two)
                `shouldBe` Just 50
            widthOf (PaneId 0) (resizeSplit (PaneId 1) DirLeft 10 windowRect two)
                `shouldBe` Just 50
        it "moves the divider down from either side of it" $ do
            heightOf (PaneId 0) (resizeSplit (PaneId 0) DirDown 10 windowRect column)
                `shouldBe` Just 30
            heightOf (PaneId 0) (resizeSplit (PaneId 1) DirDown 10 windowRect column)
                `shouldBe` Just 30
        it "moves the divider up from either side of it" $ do
            heightOf (PaneId 0) (resizeSplit (PaneId 0) DirUp 10 windowRect column)
                `shouldBe` Just 10
            heightOf (PaneId 0) (resizeSplit (PaneId 1) DirUp 10 windowRect column)
                `shouldBe` Just 10
        it "moves the pane's own right border, not a divider inside its subtree" $ do
            -- L | M | R, with L|M nested inside the outer split.
            let nested = Split LeftRight (1 % 2)
                    (Split LeftRight (1 % 2) (Leaf (PaneId 0)) (Leaf (PaneId 1)))
                    (Leaf (PaneId 2))
                lay' = resizeSplit (PaneId 1) DirRight 10 windowRect nested
            case lay' of
                Split LeftRight outer (Split LeftRight _ _ _) (Leaf _) ->
                    outer `shouldBe` (1 % 2 + 10 % 119)
                _ -> expectationFailure "resize reshaped the tree"
            fmap (.startCol) (paneRect (PaneId 2) lay') `shouldBe` Just 71
            map (`widthOf` lay') [PaneId 0, PaneId 1, PaneId 2]
                `shouldBe` [Just 30, Just 39, Just 49]
        -- Bug a1: a resize moves one border. Ratios are relative, so a naive
        -- outer-split change rescales the dividers nested inside it.
        it "leaves the dividers inside the neighbouring subtree where they were" $ do
            -- A | B | C, with A|B nested inside the outer split.
            let nested = Split LeftRight (1 % 2)
                    (Split LeftRight (1 % 2) (Leaf (PaneId 0)) (Leaf (PaneId 1)))
                    (Leaf (PaneId 2))
                grown = resizeSplit (PaneId 2) DirLeft 10 windowRect nested
                shrunk = resizeSplit (PaneId 2) DirRight 10 windowRect nested
            map (`widthOf` nested) [PaneId 0, PaneId 1, PaneId 2]
                `shouldBe` [Just 30, Just 29, Just 59]
            -- C grows leftward: only its neighbour B pays for it.
            map (`widthOf` grown) [PaneId 0, PaneId 1, PaneId 2]
                `shouldBe` [Just 30, Just 19, Just 69]
            -- C shrinks: only B takes the space back.
            map (`widthOf` shrunk) [PaneId 0, PaneId 1, PaneId 2]
                `shouldBe` [Just 30, Just 39, Just 49]
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

    describe "sessionWindowArea (status-line carve)" $ do
        let fallback = Size { rows = 3, cols = 10 }
            client = Size { rows = 24, cols = 80 }
        it "gives a detached session its full size (no status line drawn)" $
            sessionWindowArea 1 SmallestClient fallback [] `shouldBe` fallback
        it "carves one status row from an attached client's viewport" $
            sessionWindowArea 1 SmallestClient fallback [(1, client)]
                `shouldBe` Size { rows = 23, cols = 80 }
        it "carves nothing when status is off, even attached" $
            sessionWindowArea 0 SmallestClient fallback [(1, client)]
                `shouldBe` client
        it "carves the full status height for a multi-line bar" $
            sessionWindowArea 3 SmallestClient fallback [(1, client)]
                `shouldBe` Size { rows = 21, cols = 80 }
        it "never carves below a single row" $
            sessionWindowArea 1 ActiveClient fallback [(1, Size { rows = 1, cols = 5 })]
                `shouldBe` Size { rows = 1, cols = 5 }
