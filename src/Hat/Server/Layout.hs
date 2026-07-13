-- | Pane geometry: the split tree and its projection onto a window's
-- rectangle. One border row/column separates siblings, tmux-style.
module Hat.Server.Layout
    ( Layout (..)
    , Orientation (..)
    , Direction (..)
    , LayoutName (..)
    , arrange
    , sizeRect
    , layoutPanes
    , splitLeaf
    , splitFull
    , swapLeaves
    , removeLeaf
    , neighbor
    , resizeSplit
    , namedLayout
    , childRects
    ) where

import qualified Data.List as List
import Data.Ord (clamp)
import Data.Ratio ((%))

import Hat.Geometry
import Hat.Model.Ids (PaneId)

data Orientation = LeftRight | TopBottom
    deriving (Eq, Show)

data Direction = DirLeft | DirRight | DirUp | DirDown
    deriving (Eq, Show)

-- | The ratio is the first (left/top) child's share of the available
-- space, border excluded.
data Layout
    = Leaf PaneId
    | Split Orientation Rational Layout Layout
    deriving (Eq, Show)

-- | Project the tree onto a rectangle: every pane's rect plus the
-- border cells between them.
arrange :: Rect -> Layout -> ([(PaneId, Rect)], [(Pos, Char)])
arrange rect = \case
    Leaf pid -> ([(pid, rect)], [])
    Split LeftRight ratio a b ->
        let w = rect.endCol - rect.startCol
            avail = w - 1
            aw = clamp (1, max 1 (avail - 1)) (round (ratio * fromIntegral avail))
            borderCol = rect.startCol + aw
            ra = rect { endCol = borderCol }
            rb = rect { startCol = min rect.endCol (borderCol + 1) }
            border =
                [ (Pos { row = r, col = borderCol }, '│')
                | borderCol < rect.endCol
                , r <- [rect.startRow .. rect.endRow - 1]
                ]
        in arrange ra a <> arrange rb b <> (mempty, border)
    Split TopBottom ratio a b ->
        let h = rect.endRow - rect.startRow
            avail = h - 1
            ah = clamp (1, max 1 (avail - 1)) (round (ratio * fromIntegral avail))
            borderRow = rect.startRow + ah
            ra = rect { endRow = borderRow }
            rb = rect { startRow = min rect.endRow (borderRow + 1) }
            border =
                [ (Pos { row = borderRow, col = c }, '─')
                | borderRow < rect.endRow
                , c <- [rect.startCol .. rect.endCol - 1]
                ]
        in arrange ra a <> arrange rb b <> (mempty, border)

sizeRect :: Size -> Rect
sizeRect sz = Rect
    { startRow = 0
    , endRow = fromIntegral sz.rows
    , startCol = 0
    , endCol = fromIntegral sz.cols
    }

layoutPanes :: Layout -> [PaneId]
layoutPanes = \case
    Leaf pid -> [pid]
    Split _ _ a b -> layoutPanes a <> layoutPanes b

-- | Split the target leaf in two; the new pane takes half. @before@
-- puts the new pane on the left/top.
splitLeaf :: PaneId -> Orientation -> Bool -> PaneId -> Layout -> Layout
splitLeaf target orient before newPid = go
  where
    go = \case
        Leaf p
            | p == target ->
                if before
                    then Split orient (1 % 2) (Leaf newPid) (Leaf p)
                    else Split orient (1 % 2) (Leaf p) (Leaf newPid)
            | otherwise -> Leaf p
        Split o r a b -> Split o r (go a) (go b)

-- | A full-window split: the new pane becomes a sibling of the /entire/
-- existing layout, spanning the full window height (@LeftRight@) or width
-- (@TopBottom@). @before@ puts it on the left/top. Backs @split-window -f@.
splitFull :: Orientation -> Bool -> PaneId -> Layout -> Layout
splitFull orient before newPid old
    | before    = Split orient (1 % 2) (Leaf newPid) old
    | otherwise = Split orient (1 % 2) old (Leaf newPid)

-- | Exchange two panes' positions in the tree, moving their content
-- between screen locations. Backs @swap-pane@.
swapLeaves :: PaneId -> PaneId -> Layout -> Layout
swapLeaves a b = go
  where
    go = \case
        Leaf p
            | p == a -> Leaf b
            | p == b -> Leaf a
            | otherwise -> Leaf p
        Split o r x y -> Split o r (go x) (go y)

-- | Nothing when the last pane goes; the sibling absorbs the space.
removeLeaf :: PaneId -> Layout -> Maybe Layout
removeLeaf target = \case
    Leaf p
        | p == target -> Nothing
        | otherwise -> Just (Leaf p)
    Split o r a b -> case (removeLeaf target a, removeLeaf target b) of
        (Nothing, _) -> Just b
        (_, Nothing) -> Just a
        (Just a', Just b') -> Just (Split o r a' b')

-- | The tmux named layouts.
data LayoutName
    = EvenHorizontal | EvenVertical | MainVertical | MainHorizontal | Tiled
    deriving (Eq, Show)

-- | Arrange @pids@ into a named layout. @mainRatio@ is the main pane's
-- share of the window (from @main-pane-width@/@-height@), used only by the
-- @main-*@ layouts. Assumes a non-empty pane list.
namedLayout :: LayoutName -> Rational -> [PaneId] -> Layout
namedLayout name mainRatio pids = case pids of
    []      -> error "namedLayout: no panes"
    [p]     -> Leaf p
    (m : rest) -> case name of
        EvenHorizontal -> evenChain LeftRight (map Leaf pids)
        EvenVertical   -> evenChain TopBottom (map Leaf pids)
        MainVertical   ->
            Split LeftRight mainRatio (Leaf m) (evenChain TopBottom (map Leaf rest))
        MainHorizontal ->
            Split TopBottom mainRatio (Leaf m) (evenChain LeftRight (map Leaf rest))
        Tiled          -> tiled pids

-- | A balanced chain of equal-ratio splits along one axis.
evenChain :: Orientation -> [Layout] -> Layout
evenChain _ []       = error "evenChain: empty"
evenChain _ [l]      = l
evenChain o (l : ls) = Split o (1 % toInteger (length (l : ls))) l (evenChain o ls)

-- | A grid: ceil(sqrt n) columns of rows, each row an even horizontal row.
tiled :: [PaneId] -> Layout
tiled pids =
    evenChain TopBottom [ evenChain LeftRight (map Leaf row) | row <- rows ]
  where
    n = length pids
    cols = ceiling (sqrt (fromIntegral n :: Double))
    rows = chunksOf (max 1 cols) pids
    chunksOf _ [] = []
    chunksOf k xs = let (a, b) = splitAt k xs in a : chunksOf k b

-- | Geometric pane navigation: the adjacent pane in a direction with
-- the largest shared edge.
neighbor :: [(PaneId, Rect)] -> PaneId -> Direction -> Maybe PaneId
neighbor rects from dir = do
    fromRect <- List.lookup from rects
    let candidates =
            [ (overlap fromRect r, pid)
            | (pid, r) <- rects
            , pid /= from
            , adjacent fromRect r
            , overlap fromRect r > 0
            ]
    case candidates of
        [] -> Nothing
        _ -> Just (snd (maximum candidates))
  where
    adjacent a b = case dir of
        DirLeft -> b.endCol + 1 == a.startCol || b.endCol == a.startCol
        DirRight -> a.endCol + 1 == b.startCol || a.endCol == b.startCol
        DirUp -> b.endRow + 1 == a.startRow || b.endRow == a.startRow
        DirDown -> a.endRow + 1 == b.startRow || a.endRow == b.startRow
    overlap a b = case dir of
        DirLeft -> rowOverlap a b
        DirRight -> rowOverlap a b
        DirUp -> colOverlap a b
        DirDown -> colOverlap a b
    rowOverlap a b = min a.endRow b.endRow - max a.startRow b.startRow
    colOverlap a b = min a.endCol b.endCol - max a.startCol b.startCol

-- | Move the divider nearest the target pane along the given axis by
-- @delta@ cells (grows the pane in that direction).
resizeSplit :: PaneId -> Direction -> Int -> Rect -> Layout -> Layout
resizeSplit target dir delta rect layout =
    snd (go rect layout)
  where
    wantOrient = case dir of
        DirLeft -> LeftRight
        DirRight -> LeftRight
        DirUp -> TopBottom
        DirDown -> TopBottom
    -- Returns (found target in subtree, adjusted subtree). Adjusts the
    -- deepest matching-orientation split that contains the target.
    go :: Rect -> Layout -> (Bool, Layout)
    go _ (Leaf p) = (p == target, Leaf p)
    go r (Split o ratio a b) =
        let (ra, rb) = childRects r o ratio
            (inA, a') = go ra a
            (inB, b') = go rb b
            found = inA || inB
            adjusted
                | o == wantOrient && (adjustedInA inA || adjustedInB inB) =
                    Split o (newRatio r o ratio (inA, inB)) a' b'
                | otherwise = Split o ratio a' b'
            -- only adjust here if the child subtree didn't already
            adjustedInA ina = ina && not (childAdjusted a a')
            adjustedInB inb = inb && not (childAdjusted b b')
        in (found, adjusted)
    childAdjusted old new = old /= new
    newRatio r o ratio (inA, _) =
        let avail = fromIntegral $ case o of
                LeftRight -> (r.endCol - r.startCol) - 1
                TopBottom -> (r.endRow - r.startRow) - 1
            step = fromIntegral delta / max 1 avail
            growFirst = case dir of
                DirRight -> inA
                DirDown -> inA
                DirLeft -> not inA
                DirUp -> not inA
            r' = if growFirst then ratio + step else ratio - step
            minR = 1 / max 2 avail
        in clamp (minR, 1 - minR) r'

childRects :: Rect -> Orientation -> Rational -> (Rect, Rect)
childRects rect o ratio = case o of
    LeftRight ->
        let avail = (rect.endCol - rect.startCol) - 1
            aw = clamp (1, max 1 (avail - 1)) (round (ratio * fromIntegral avail))
            borderCol = rect.startCol + aw
        in ( rect { endCol = borderCol }
           , rect { startCol = min rect.endCol (borderCol + 1) }
           )
    TopBottom ->
        let avail = (rect.endRow - rect.startRow) - 1
            ah = clamp (1, max 1 (avail - 1)) (round (ratio * fromIntegral avail))
            borderRow = rect.startRow + ah
        in ( rect { endRow = borderRow }
           , rect { startRow = min rect.endRow (borderRow + 1) }
           )
