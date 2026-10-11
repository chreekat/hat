{-# OPTIONS_GHC -Wno-orphans #-}  -- Arbitrary PaneCycle lives with its test: the library must not depend on QuickCheck

module Hat.Server.LayoutSpec (spec) where

import Control.Monad (foldM, forM_)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Geometry
import Hat.Model.Ids
import Hat.Model.Options (WindowSizeMode (..))
import Hat.Server.Layout
import Hat.TestSupport (nodeOf)

-- Random layouts grown the way commands grow them: split after split of
-- random panes, any orientation, size, side and span, at a random window
-- size (a split with no room is skipped).
genLayout :: Gen Layout
genLayout = do
    w <- chooseInt (1, 60)
    h <- chooseInt (1, 30)
    n <- chooseInt (0, 8)
    let start = fitLayout (Size (fromIntegral h) (fromIntegral w)) (singlePane (PaneId 0))
    foldM grow start [1 .. n]
  where
    grow lay k = do
        target <- elements (layoutPanes lay)
        o <- elements [LeftRight, TopBottom]
        sz <- oneof [pure Nothing, Just <$> chooseInt (0, 40)]
        pl <- elements [Before, After]
        sp <- elements [SpanPane, SpanWindow]
        pure (fromMaybe lay (splitPane (SplitSpec o sz pl sp) target (PaneId k) lay))

-- Shrink toward 'PaneNext' so a failing 'PanePrev' case also reports as
-- 'PaneNext' if that simpler case reproduces it.
instance Arbitrary PaneCycle where
    arbitrary = elements [PaneNext, PanePrev]
    shrink PaneNext = []
    shrink PanePrev = [PaneNext]

windowRect :: Rect
windowRect = Rect { startRow = 0, endRow = 40, startCol = 0, endCol = 120 }

windowSize :: Size
windowSize = Size { rows = 40, cols = 120 }

-- Any small rect, degenerate sizes included: a tree starved of space
-- must still tile, never spilling a cell outside the window.
genTileRect :: Gen Rect
genTileRect = do
    r0 <- chooseInt (0, 3)
    c0 <- chooseInt (0, 3)
    h <- chooseInt (0, 14)
    w <- chooseInt (0, 30)
    pure Rect { startRow = r0, endRow = r0 + h, startCol = c0, endCol = c0 + w }

cellsOf :: Rect -> [(Int, Int)]
cellsOf r = [(row, col) | row <- [r.startRow .. r.endRow - 1]
                        , col <- [r.startCol .. r.endCol - 1]]

leaf :: Int -> Layout
leaf = singlePane . PaneId

-- A pane's cell size as (width, height).
cellOf :: Int -> Layout -> Maybe (Int, Int)
cellOf n lay = (\c -> (c.sx, c.sy)) <$> paneCell (PaneId n) lay

split1
    :: Orientation -> Maybe Int -> Placement -> SplitSpan -> Int -> Int -> Layout
    -> Maybe Layout
split1 o sz pl sp target new = splitPane (SplitSpec o sz pl sp) (PaneId target) (PaneId new)

-- Every node's children exactly fill it, their offsets laid out in order.
consistent :: Layout -> Bool
consistent l = case l.cell of
    LeafCell _ -> True
    NodeCell o cs -> all consistent cs && case o of
        LeftRight -> all (\c -> c.sy == l.sy && c.yoff == l.yoff) cs
            && sum [ c.sx + 1 | c <- cs ] - 1 == l.sx
            && and (zipWith (\a b -> b.xoff == a.xoff + a.sx + 1) cs (drop 1 cs))
        TopBottom -> all (\c -> c.sx == l.sx && c.xoff == l.xoff) cs
            && sum [ c.sy + 1 | c <- cs ] - 1 == l.sy
            && and (zipWith (\a b -> b.yoff == a.yoff + a.sy + 1) cs (drop 1 cs))

spec :: Spec
spec = do
    prop "panes and borders tile the window exactly, at any size" $
        forAll ((,) <$> genLayout <*> genTileRect) $ \(lay, rect) ->
            let (rects, borders) = arrange rect lay
                paneCells = concatMap (cellsOf . snd) rects
                borderCells = [(p.row, p.col) | (p, _) <- borders]
                allCells = paneCells <> borderCells
                cellSet = Set.fromList allCells
            in length allCells === Set.size cellSet  -- no cell covered twice
                .&&. cellSet === Set.fromList (cellsOf rect)

    prop "splits, closes and resizes keep every node exactly filled" $
        forAll genLayout $ \lay ->
            let ps = layoutPanes lay
                closed = mapMaybe (`removePane` lay) ps
                resized = [ resizePane p o d lay
                          | p <- ps, o <- [LeftRight, TopBottom], d <- [-3, 5] ]
            in conjoin (map (property . consistent) (lay : closed <> resized))

    prop "fitting to the layout's own size changes nothing" $
        forAll genLayout $ \lay -> fitLayout (layoutArea lay) lay === lay

    -- A split of an already-empty rect once fabricated a one-row child
    -- outside the window.
    it "keeps a space-starved nested split inside the window" $ do
        let rect = Rect { startRow = 0, endRow = 2, startCol = 0, endCol = 4 }
            lay = nodeOf TopBottom [leaf 1, nodeOf TopBottom [leaf 2, leaf 3]]
            inside r = r.startRow >= 0 && r.endRow <= 2
                && r.startCol >= 0 && r.endCol <= 4
        all (inside . snd) (fst (arrange rect lay)) `shouldBe` True

    describe "border junctions" $ do
        let smallRect = Rect { startRow = 0, endRow = 5, startCol = 0, endCol = 5 }
            glyphAt r co lay =
                List.lookup (Pos { row = r, col = co }) (snd (arrange smallRect lay))

        it "joins a divider ending against a perpendicular one with a tee" $
            -- A vertical divider in the top half meets the full-width
            -- horizontal divider from above: ┴ (up + left + right).
            glyphAt 2 2 (nodeOf TopBottom [nodeOf LeftRight [leaf 0, leaf 1], leaf 2])
                `shouldBe` Just '\x2534'

        it "joins two dividers crossing with a full cross" $
            -- Both halves split at the same row, straddling the central
            -- vertical divider: ┼ (all four arms).
            glyphAt 2 2
                (nodeOf LeftRight
                    [nodeOf TopBottom [leaf 0, leaf 1], nodeOf TopBottom [leaf 2, leaf 3]])
                `shouldBe` Just '\x253c'

    describe "splitPane" $ do
        let one = fitLayout Size { rows = 24, cols = 80 } (leaf 0)
        it "halves a pane, the extra cell going to the first" $
            (split1 LeftRight Nothing After SpanPane 0 1 one >>= cellOf 0)
                `shouldBe` Just (40, 24)
        it "gives the new pane the requested size, the rest less the border to the old" $ do
            let lay = split1 LeftRight (Just 20) After SpanPane 0 1 one
            (lay >>= cellOf 1) `shouldBe` Just (20, 24)
            (lay >>= cellOf 0) `shouldBe` Just (59, 24)
        it "puts the new pane first under Before" $
            fmap layoutPanes (split1 TopBottom Nothing Before SpanPane 0 1 one)
                `shouldBe` Just [PaneId 1, PaneId 0]
        it "under SpanWindow spans the whole window, the rest shrinking proportionally" $ do
            -- tmux pane-ops: 80x24 split -h -l 20, then -v -l 25% of the
            -- left pane, then a full-height 10-column pane before them all.
            let lay = split1 LeftRight (Just 20) After SpanPane 0 1 one
                    >>= split1 TopBottom (Just 6) After SpanPane 0 2
                    >>= split1 LeftRight (Just 10) Before SpanWindow 0 3
            (lay >>= cellOf 3) `shouldBe` Just (10, 24)
            (lay >>= cellOf 0) `shouldBe` Just (50, 17)
            (lay >>= cellOf 2) `shouldBe` Just (50, 6)
            fmap layoutPanes lay `shouldBe` Just (map PaneId [3, 0, 2, 1])
        it "refuses a pane too small to hold two" $
            split1 LeftRight Nothing After SpanPane 0 1
                (fitLayout Size { rows = 24, cols = 2 } (leaf 0))
                `shouldBe` Nothing
        prop "adds exactly the new pane" $
            forAll genLayout $ \lay -> case layoutPanes lay of
                [] -> property Discard
                target : _ -> case splitPane (SplitSpec LeftRight Nothing After SpanPane)
                        target (PaneId 999) lay of
                    Nothing -> property Discard
                    Just lay' -> Set.fromList (layoutPanes lay')
                        === Set.insert (PaneId 999) (Set.fromList (layoutPanes lay))

    describe "removePane" $ do
        let three = fitLayout windowSize (nodeOf LeftRight [leaf 0, leaf 1, leaf 2])
            widths ns l = mapMaybe (\n -> fst <$> cellOf n l) ns
        it "gives a pane's space to its next sibling" $
            fmap (widths [1, 2]) (removePane (PaneId 0) three)
                `shouldBe` Just (case widths [0, 1, 2] three of
                    [a, b, c] -> [a + 1 + b, c]
                    other -> other)
        it "gives a last pane's space to its previous sibling" $
            fmap (widths [0, 1]) (removePane (PaneId 2) three)
                `shouldBe` Just (case widths [0, 1, 2] three of
                    [a, b, c] -> [a, b + 1 + c]
                    other -> other)
        prop "removing a split-off pane restores the original pane set" $
            forAll genLayout $ \lay -> case splitPane
                    (SplitSpec TopBottom Nothing Before SpanPane)
                    (last (layoutPanes lay)) (PaneId 999) lay of
                Nothing -> property Discard
                Just lay' -> fmap (Set.fromList . layoutPanes) (removePane (PaneId 999) lay')
                    === Just (Set.fromList (layoutPanes lay))
        prop "removing the only pane empties the layout" $ \n ->
            removePane (PaneId n) (singlePane (PaneId n)) === Nothing

    describe "swapLeaves" $ do
        it "exchanges two panes' positions" $
            layoutPanes (swapLeaves (PaneId 0) (PaneId 1) (nodeOf LeftRight [leaf 0, leaf 1]))
                `shouldBe` [PaneId 1, PaneId 0]
        it "is its own inverse" $
            forAll genLayout $ \lay -> case layoutPanes lay of
                (a : b : _) -> swapLeaves a b (swapLeaves a b lay) === lay
                _ -> property Discard

    describe "paneOrder" $
        it "numbers panes in layout order, not creation order" $ do
            let lay = nodeOf LeftRight [leaf 7, leaf 3]
                ps = Map.fromList [(PaneId 3, ()), (PaneId 7, ())]
            paneOrder lay ps `shouldBe` [PaneId 7, PaneId 3]

    describe "rotateLeaves" $ do
        let three = nodeOf LeftRight [leaf 0, nodeOf TopBottom [leaf 1, leaf 2]]
        it "-U moves each pane to the previous position" $
            layoutPanes (rotateLeaves RotateUp three)
                `shouldBe` map PaneId [1, 2, 0]
        it "-D moves each pane to the next position" $
            layoutPanes (rotateLeaves RotateDown three)
                `shouldBe` map PaneId [2, 0, 1]
        it "keeps the shape, and -D undoes -U" $
            forAll genLayout $ \lay ->
                rotateLeaves RotateDown (rotateLeaves RotateUp lay) === lay

    describe "resizePane" $ do
        let two = fitLayout windowSize (nodeOf LeftRight [leaf 0, leaf 1])
            column = fitLayout windowSize (nodeOf TopBottom [leaf 0, leaf 1])
            width n l = fst <$> cellOf n l
            height n l = snd <$> cellOf n l
        it "moves the shared border right from either side of it" $ do
            width 0 (resizePane (PaneId 0) LeftRight 10 two) `shouldBe` Just 70
            width 0 (resizePane (PaneId 1) LeftRight 10 two) `shouldBe` Just 70
        it "moves the shared border left from either side of it" $ do
            width 0 (resizePane (PaneId 0) LeftRight (-10) two) `shouldBe` Just 50
            width 0 (resizePane (PaneId 1) LeftRight (-10) two) `shouldBe` Just 50
        it "moves the shared border down and up" $ do
            height 0 (resizePane (PaneId 1) TopBottom 10 column) `shouldBe` Just 30
            height 0 (resizePane (PaneId 0) TopBottom (-10) column) `shouldBe` Just 10
        it "takes the space from the nearest sibling that has some" $ do
            -- A | B | C with B at its minimum: growing A shrinks C.
            let abc = fitLayout windowSize (nodeOf LeftRight [leaf 0, leaf 1, leaf 2])
                squeezed = resizePaneTo (PaneId 1) LeftRight 1 abc
                grown = resizePane (PaneId 0) LeftRight 5 squeezed
            width 1 grown `shouldBe` Just 1
            fmap (+ 5) (width 0 squeezed) `shouldBe` width 0 grown
            fmap (subtract 5) (width 2 squeezed) `shouldBe` width 2 grown
        it "keeps every pane at least one cell wide" $ do
            let lay = iterate (resizePane (PaneId 0) LeftRight 30) two !! 10
            map (`width` lay) [0, 1] `shouldSatisfy` all (maybe False (>= 1))
        it "resizes to an absolute size, a last pane through its left border" $ do
            width 1 (resizePaneTo (PaneId 1) LeftRight 20 two) `shouldBe` Just 20
            width 0 (resizePaneTo (PaneId 0) LeftRight 20 two) `shouldBe` Just 20

    describe "fitLayout" $
        it "never shrinks a pane below one cell, overhanging the window instead" $
            layoutArea (fitLayout Size { rows = 1, cols = 1 }
                (fitLayout windowSize (nodeOf LeftRight [leaf 0, leaf 1, leaf 2])))
                `shouldBe` Size { rows = 1, cols = 5 }

    describe "namedLayout" $ do
        let pids4 = map PaneId [0, 1, 2, 3]
            cells name = do
                lay <- namedLayout defaultNamedSizes windowSize name pids4
                traverse (`cellOf` lay) [0, 1, 2, 3]
        it "even-horizontal shares the width, the remainder going first" $
            cells EvenHorizontal `shouldBe` Just [(30, 40), (29, 40), (29, 40), (29, 40)]
        it "main-vertical gives the main pane main-pane-width columns" $
            cells MainVertical `shouldBe` Just [(80, 40), (39, 13), (39, 13), (39, 12)]
        it "main-horizontal-mirrored puts the others above the main pane" $
            fmap layoutPanes (namedLayout defaultNamedSizes windowSize MainHorizontalMirrored pids4)
                `shouldBe` Just (map PaneId [1, 2, 3, 0])
        it "tiled makes a grid, the last row and column taking the slack" $
            cells Tiled `shouldBe` Just [(59, 19), (60, 19), (59, 20), (60, 20)]
        it "leaves a lone pane alone" $
            namedLayout defaultNamedSizes windowSize Tiled [PaneId 0] `shouldBe` Nothing
        it "is found by a unique prefix" $ do
            lookupLayoutName "even-h" `shouldBe` Just EvenHorizontal
            lookupLayoutName "main-v" `shouldBe` Nothing
            lookupLayoutName "main-vertical" `shouldBe` Just MainVertical

    describe "nextLayoutName/previousLayoutName" $ do
        -- tmux's layout cycle pinned from every point in both directions:
        -- Nothing (no layout set yet) enters at either end; the ends wrap.
        forM_
            [ (Nothing, EvenHorizontal, Tiled)
            , (Just EvenHorizontal, EvenVertical, Tiled)
            , (Just EvenVertical, MainHorizontal, EvenHorizontal)
            , (Just MainHorizontal, MainHorizontalMirrored, EvenVertical)
            , (Just MainHorizontalMirrored, MainVertical, MainHorizontal)
            , (Just MainVertical, MainVerticalMirrored, MainHorizontalMirrored)
            , (Just MainVerticalMirrored, Tiled, MainVertical)
            , (Just Tiled, EvenHorizontal, MainVerticalMirrored)
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
        let quad = nodeOf TopBottom
                [nodeOf LeftRight [leaf 0, leaf 1], nodeOf LeftRight [leaf 2, leaf 3]]
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
        let columns = nodeOf LeftRight
                [leaf 0, nodeOf LeftRight [leaf 1, nodeOf TopBottom [leaf 2, leaf 3]]]
            (colRects, _) = arrange windowRect columns
        it "wraps past a middle column to the far column" $
            neighbor colRects (PaneId 0) DirLeft `shouldBe` Just (PaneId 2)
        it "wraps past a middle column back to the near column" $
            neighbor colRects (PaneId 2) DirRight `shouldBe` Just (PaneId 0)

    describe "directionalTarget" $ do
        -- Resolving against the full layout (not a zoom-collapsed one) is
        -- what lets prefix+hjkl move away from a zoomed pane. See bug 5.
        let two = nodeOf LeftRight [leaf 0, leaf 1]
            sz = Size { rows = 24, cols = 80 }
        it "moves to the neighbor computed from the full split layout" $
            directionalTarget sz two (PaneId 0) DirRight `shouldBe` Just (PaneId 1)

    describe "effectiveWindowSize (window-size)" $ do
        let big = Size { rows = 50, cols = 200 }
            small = Size { rows = 24, cols = 80 }
            fallback = Size { rows = 30, cols = 100 }
        it "smallest shrinks to the smallest client" $
            effectiveWindowSize SizeSmallest fallback [(1, big), (2, small)]
                `shouldBe` small
        it "smallest intersects each dimension independently" $
            effectiveWindowSize SizeSmallest fallback
                [ (1, Size { rows = 24, cols = 200 })
                , (2, Size { rows = 50, cols = 80 }) ]
                `shouldBe` Size { rows = 24, cols = 80 }
        it "largest covers each dimension's biggest client" $
            effectiveWindowSize SizeLargest fallback
                [ (1, Size { rows = 24, cols = 200 })
                , (2, Size { rows = 50, cols = 80 }) ]
                `shouldBe` Size { rows = 50, cols = 200 }
        it "latest follows the most-recently-active client" $
            -- stamp 2 (big) is newer than stamp 1 (small): take big and let
            -- the smaller client clip.
            effectiveWindowSize SizeLatest fallback [(1, small), (2, big)]
                `shouldBe` big
        it "latest follows the active client even when it is the smaller one" $
            effectiveWindowSize SizeLatest fallback [(2, big), (5, small)]
                `shouldBe` small
        it "manual ignores every client" $
            effectiveWindowSize SizeManual fallback [(1, big), (2, small)]
                `shouldBe` fallback
        it "keeps the fallback size when no client is attached" $
            forM_ [SizeSmallest, SizeLargest, SizeLatest, SizeManual] $ \m ->
                effectiveWindowSize m fallback [] `shouldBe` fallback

    describe "sessionWindowArea (status-line carve)" $ do
        let fallback = Size { rows = 3, cols = 10 }
            client = Size { rows = 24, cols = 80 }
        it "gives a detached session its full size (no status line drawn)" $
            sessionWindowArea 1 SizeSmallest fallback [] `shouldBe` fallback
        it "carves one status row from an attached client's viewport" $
            sessionWindowArea 1 SizeSmallest fallback [(1, client)]
                `shouldBe` Size { rows = 23, cols = 80 }
        it "carves nothing when status is off, even attached" $
            sessionWindowArea 0 SizeSmallest fallback [(1, client)]
                `shouldBe` client
        it "carves the full status height for a multi-line bar" $
            sessionWindowArea 3 SizeSmallest fallback [(1, client)]
                `shouldBe` Size { rows = 21, cols = 80 }
        it "never carves a manual size, attached or not" $
            sessionWindowArea 1 SizeManual fallback [(1, client)]
                `shouldBe` fallback
        it "never carves below a single row" $
            sessionWindowArea 1 SizeLatest fallback [(1, Size { rows = 1, cols = 5 })]
                `shouldBe` Size { rows = 1, cols = 5 }
