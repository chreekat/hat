-- | Pane geometry: tmux's layout tree (@layout.c@, @layout-set.c@). Every
-- cell carries its own absolute size and offset, a node holds any number of
-- children laid along one axis, and one border row\/column separates
-- siblings. Sizes are cells, never ratios, so every operation reproduces
-- tmux's arithmetic exactly.
module Hat.Server.Layout
    ( Layout (..)
    , Cell (..)
    , Orientation (..)
    , Placement (..)
    , SplitSpan (..)
    , SplitSpec (..)
    , Direction (..)
    , LayoutName (..)
    , layoutNames
    , layoutNameText
    , lookupLayoutName
    , NamedSizes (..)
    , defaultNamedSizes
    , PaneCycle (..)
    , PaneIndex (..)
    , parsePaneIndex
    , resolvePaneIndex
    , effectiveWindowSize
    , sessionWindowArea
    , nextLayoutName
    , previousLayoutName
    , cyclePane
    , singlePane
    , layoutArea
    , fitLayout
    , fixOffsets
    , arrange
    , sizeRect
    , layoutPanes
    , paneOrder
    , paneCell
    , splitPane
    , removePane
    , destroyBottomRight
    , swapLeaves
    , Rotation (..)
    , rotateLeaves
    , resizePane
    , resizePaneTo
    , searchByBorder
    , parentOrientation
    , resizeCellAt
    , spreadOut
    , namedLayout
    , neighbor
    , directionalTarget
    ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR

import Hat.Geometry
import Hat.Model.Ids (PaneId)
import Hat.Model.Options (WindowSizeMode (..))
import Hat.Server.ArgNumber (stringPercentage)

data Orientation = LeftRight | TopBottom
    deriving (Eq, Show)

-- | The size a window takes for the sizes of the clients attached to its
-- session, under @window-size@ (tmux's @clients_calculate_size@).
-- 'SizeSmallest' intersects every client so the window fits them all;
-- 'SizeLargest' covers the biggest, letting smaller clients clip;
-- 'SizeLatest' follows the most-recently-active client — the one with the
-- largest activity stamp — and with a single client is the same as
-- smallest. With no clients attached, or under 'SizeManual', the window keeps
-- its @fallback@ (its current size; only @resize-window@ moves a manual one).
effectiveWindowSize :: WindowSizeMode -> Size -> [(Int, Size)] -> Size
effectiveWindowSize SizeManual fallback _ = fallback
effectiveWindowSize _ fallback [] = fallback
effectiveWindowSize mode _ clients = case mode of
    SizeLargest -> combine maximum clients
    SizeLatest -> snd (List.maximumBy (comparing fst) clients)
    _ -> combine minimum clients
  where
    combine pick cs = Size
        { rows = pick (map (\(_, s) -> s.rows) cs)
        , cols = pick (map (\(_, s) -> s.cols) cs)
        }

-- | The pane area of a session's window: its 'effectiveWindowSize' over its
-- clients' claims, each with the status rows that client draws carved out
-- of its size. A detached session keeps its full size — tmux draws no
-- status line without a client, so @new-session -d -x W -y H@ gives an
-- H-row window, not H-1 — and @status off@ (0 lines) carves nothing even
-- when attached. A manual size is the window's own and is never carved.
sessionWindowArea :: WindowSizeMode -> Size -> [(Int, Size, Int)] -> Size
sessionWindowArea mode fallback claims =
    effectiveWindowSize mode fallback
        [ (stamp, sz { rows = fromIntegral (max 1 (fromIntegral sz.rows - status)) })
        | (stamp, sz, status) <- claims ]

data Direction = DirLeft | DirRight | DirUp | DirDown
    deriving (Eq, Show)

-- | @-b@: which side of the target a new pane lands on.
data Placement
    = Before  -- ^ the new pane takes the left\/top slot
    | After   -- ^ the new pane takes the right\/bottom slot
    deriving (Eq, Show)

-- | A cell of the layout tree: its size and its offset in the window.
data Layout = Layout
    { sx   :: !Int
    , sy   :: !Int
    , xoff :: !Int
    , yoff :: !Int
    , cell :: !Cell
    }
    deriving (Eq, Show)

-- | What a cell holds: one pane, or two or more children laid along an axis.
data Cell
    = LeafCell PaneId
    | NodeCell Orientation [Layout]
    deriving (Eq, Show)

-- | tmux's @PANE_MINIMUM@: the smallest a pane may be along either axis.
paneMinimum :: Int
paneMinimum = 1

-- | A window holding one pane, at a nominal size 'fitLayout' replaces.
singlePane :: PaneId -> Layout
singlePane pid = Layout 1 1 0 0 (LeafCell pid)

-- | The size of the area a layout covers.
layoutArea :: Layout -> Size
layoutArea l = Size { rows = fromIntegral l.sy, cols = fromIntegral l.sx }

sizeRect :: Size -> Rect
sizeRect sz = Rect
    { startRow = 0
    , endRow = fromIntegral sz.rows
    , startCol = 0
    , endCol = fromIntegral sz.cols
    }

extent :: Orientation -> Layout -> Int
extent LeftRight l = l.sx
extent TopBottom l = l.sy

setExtent :: Orientation -> Int -> Layout -> Layout
setExtent LeftRight n l = l { sx = n }
setExtent TopBottom n l = l { sy = n }

isLeaf :: Layout -> Bool
isLeaf l = case l.cell of
    LeafCell _ -> True
    NodeCell _ _ -> False

-- Paths ---------------------------------------------------------------------

-- | Child indices from the root down to a cell.
type Path = [Int]

pathTo :: PaneId -> Layout -> Maybe Path
pathTo pid l = case l.cell of
    LeafCell p -> if p == pid then Just [] else Nothing
    NodeCell _ cs -> listToMaybe
        [ i : p | (i, c) <- zip [0 ..] cs, Just p <- [pathTo pid c] ]

at :: Path -> Layout -> Layout
at [] l = l
at (i : p) l = case l.cell of
    NodeCell _ cs | (c : _) <- drop i cs -> at p c
    _ -> l

modifyAt :: Path -> (Layout -> Layout) -> Layout -> Layout
modifyAt [] f l = f l
modifyAt (i : p) f l = case l.cell of
    NodeCell t cs -> l { cell = NodeCell t
        [ if j == i then modifyAt p f c else c | (j, c) <- zip [0 ..] cs ] }
    LeafCell _ -> l

modifyChild :: Int -> (Layout -> Layout) -> [Layout] -> [Layout]
modifyChild i f cs = [ if j == i then f c else c | (j, c) <- zip [0 ..] cs ]

-- Sizing --------------------------------------------------------------------

-- | How much a cell can give up along an axis (@layout_resize_check@).
spare :: Orientation -> Layout -> Int
spare o l = case l.cell of
    LeafCell _ -> max 0 (extent o l - paneMinimum)
    NodeCell t cs
        | t == o -> sum (map (spare o) cs)
        | otherwise -> minimum (map (spare o) cs)

-- | Grow or shrink a cell along an axis, spreading the change over its
-- children one cell at a time (@layout_resize_adjust@). The change must
-- already be bounded by what the cell can give.
adjust :: Orientation -> Int -> Layout -> Layout
adjust o change l0 = case l.cell of
    LeafCell _ -> l
    NodeCell t cs
        | t /= o -> l { cell = NodeCell t (map (adjust o change) cs) }
        | otherwise -> l { cell = NodeCell t (spread change cs) }
  where
    l = setExtent o (extent o l0 + change) l0
    spread 0 cs = cs
    spread ch cs = case List.foldl' step (ch, False, []) cs of
        (ch', True, acc) | ch' /= 0 -> spread ch' (reverse acc)
        (_, _, acc) -> reverse acc
    step (ch, changed, acc) c
        | ch == 0 = (ch, changed, c : acc)
        | ch > 0 = (ch - 1, True, adjust o 1 c : acc)
        | spare o c > 0 = (ch + 1, True, adjust o (-1) c : acc)
        | otherwise = (ch, changed, c : acc)

-- | Lay every cell's offset out from its siblings' sizes, the root at the
-- origin (@layout_fix_offsets@).
fixOffsets :: Layout -> Layout
fixOffsets = place 0 0
  where
    place x y l = case l.cell of
        LeafCell _ -> l'
        NodeCell LeftRight cs -> l' { cell = NodeCell LeftRight (alongX x cs) }
        NodeCell TopBottom cs -> l' { cell = NodeCell TopBottom (alongY y cs) }
      where
        l' = l { xoff = x, yoff = y }
        alongX _ [] = []
        alongX cx (c : rest) = place cx y c : alongX (cx + c.sx + 1) rest
        alongY _ [] = []
        alongY cy (c : rest) = place x cy c : alongY (cy + c.sy + 1) rest

-- | Resize the whole layout to a window size (@layout_resize@): never below
-- what its panes can give up, so a window too small for its panes leaves the
-- layout overhanging it.
fitLayout :: Size -> Layout -> Layout
fitLayout sz =
    fixOffsets
        . fitAxis TopBottom (fromIntegral sz.rows)
        . fitAxis LeftRight (fromIntegral sz.cols)
  where
    fitAxis o target l
        | change /= 0 = adjust o change l
        | otherwise = l
      where
        cur = extent o l
        limit = spare o l
        change
            | limit == 0 = if target <= cur then 0 else target - cur
            | target - cur < negate limit = negate limit
            | otherwise = target - cur

-- | A child's share of a parent resized from @prev@ to @size@ cells, with
-- @countLeft@ children (this one included) left to fit in @sizeLeft@
-- (@layout_new_pane_size@).
newPaneSize :: Int -> Layout -> Orientation -> Int -> Int -> Int -> Int
newPaneSize prev c o size countLeft sizeLeft
    | countLeft == 1 = sizeLeft
    | otherwise =
        let ext = extent o c
            mn = max ((paneMinimum + 1) * (countLeft - 1)) (ext - spare o c)
            ns = (ext * size) `div` prev
            mx = sizeLeft - mn
            -- A negative bound is tmux's unsigned wrap: no limit at all.
            ns' = if mx >= 0 && ns > mx then mx else ns
        in max paneMinimum ns'

-- | Whether a cell and all its children fit a new size along an axis
-- (@layout_set_size_check@).
sizeFits :: Orientation -> Int -> Layout -> Bool
sizeFits o size l = case l.cell of
    LeafCell _ -> size >= paneMinimum
    NodeCell t cs
        | t == o ->
            let count = length cs
                prev = extent o l
                go _ _ [] = True
                go idx avail (c : rest) =
                    let ns = newPaneSize prev c o size (count - idx) avail
                        need = if null rest then ns else ns + 1
                    in need <= avail && sizeFits o ns c
                        && go (idx + 1) (avail - need) rest
            in size >= count * 2 - 1 && go (0 :: Int) size cs
        | otherwise -> and [ sizeFits o size c | c <- cs, not (isLeaf c) ]

-- | Refit a node's children proportionally into its (changed) size
-- (@layout_resize_child_cells@).
resizeChildCells :: Layout -> Layout
resizeChildCells l = case l.cell of
    LeafCell _ -> l
    NodeCell t cs ->
        let count = length cs
            prev = sum (map (extent t) cs) + count - 1
            go _ _ [] = []
            go idx avail (c : rest) =
                let left = count - idx
                    (c1, avail1) = case t of
                        TopBottom -> (c { sx = l.sx, xoff = l.xoff }, avail)
                        LeftRight ->
                            let w = newPaneSize prev c LeftRight l.sx left avail
                            in (c { sx = w }, avail - (w + 1))
                    (c2, avail2) = case t of
                        LeftRight -> (c1 { sy = l.sy, yoff = l.yoff }, avail1)
                        TopBottom ->
                            let h = newPaneSize prev c1 TopBottom l.sy left avail1
                            in (c1 { sy = h }, avail1 - (h + 1))
                in resizeChildCells c2 : go (idx + 1) avail2 rest
        in l { cell = NodeCell t (go (0 :: Int) (extent t l) cs) }

-- Queries -------------------------------------------------------------------

-- | The panes in layout order.
layoutPanes :: Layout -> [PaneId]
layoutPanes l = case l.cell of
    LeafCell p -> [p]
    NodeCell _ cs -> concatMap layoutPanes cs

-- | A window's panes in tmux's pane order (what @pane_index@ counts): the
-- layout's leaf order, then any pane the layout does not hold.
paneOrder :: Layout -> Map.Map PaneId a -> [PaneId]
paneOrder lay ps = inLayout <> filter (`notElem` inLayout) (Map.keys ps)
  where
    inLayout = filter (`Map.member` ps) (layoutPanes lay)

-- | A pane's cell.
paneCell :: PaneId -> Layout -> Maybe Layout
paneCell pid l = (`at` l) <$> pathTo pid l

-- | Project the layout, fitted to @rect@'s size, onto it: every pane's rect
-- plus the border cells between them, all clipped to the rect.
arrange :: Rect -> Layout -> ([(PaneId, Rect)], [(Pos, Char)])
arrange rect lay =
    let w = max 0 (rect.endCol - rect.startCol)
        h = max 0 (rect.endRow - rect.startRow)
        fitted = fitLayout Size { rows = fromIntegral h, cols = fromIntegral w } lay
        (rects, borders) = project fitted
        shift r = Rect
            { startRow = clampR (r.startRow + rect.startRow)
            , endRow = clampR (r.endRow + rect.startRow)
            , startCol = clampC (r.startCol + rect.startCol)
            , endCol = clampC (r.endCol + rect.startCol) }
        clampR = max rect.startRow . min rect.endRow
        clampC = max rect.startCol . min rect.endCol
        inside p = p.row >= rect.startRow && p.row < rect.endRow
            && p.col >= rect.startCol && p.col < rect.endCol
        moved = [ (p', ch)
                | (p, ch) <- borders
                , let p' = Pos { row = p.row + rect.startRow, col = p.col + rect.startCol }
                , inside p' ]
    in ([ (pid, shift r) | (pid, r) <- rects ], resolveJunctions moved)

-- | Cells as rects and the dividers between siblings as plain @│@\/@─@
-- runs spanning their parent.
project :: Layout -> ([(PaneId, Rect)], [(Pos, Char)])
project l = case l.cell of
    LeafCell p -> ([(p, Rect l.yoff (l.yoff + l.sy) l.xoff (l.xoff + l.sx))], [])
    NodeCell t cs -> mconcat (map project cs) <> ([], concatMap (divider t) (init cs))
  where
    divider LeftRight c =
        [ (Pos { row = r, col = c.xoff + c.sx }, '│') | r <- [l.yoff .. l.yoff + l.sy - 1] ]
    divider TopBottom c =
        [ (Pos { row = c.yoff + c.sy, col = x }, '─') | x <- [l.xoff .. l.xoff + l.sx - 1] ]

-- | Fuse abutting divider runs into box-drawing junctions. Each cell keeps
-- the two arms of its own line (@│@ has up+down, @─@ has left+right) and
-- gains a perpendicular arm wherever the neighbouring cell carries the
-- other line — so a @─@ run ending against a @│@ becomes @┤@\/@├@\/@┼@ and
-- a @│@ tip meeting a @─@ becomes @┬@\/@┴@. Divider ends at the window edge
-- keep their full line (no neighbour, no spurious stub).
resolveJunctions :: [(Pos, Char)] -> [(Pos, Char)]
resolveJunctions cells = [ (p, fuse p ch) | (p, ch) <- cells ]
  where
    hasV q = Map.lookup q m == Just '│'
    hasH q = Map.lookup q m == Just '─'
    m = Map.fromList cells
    fuse p ch
        | ch == '│' = junction True True (hasH (left p)) (hasH (right p))
        | ch == '─' = junction (hasV (up p)) (hasV (down p)) True True
        | otherwise = ch
    up    p = p { row = p.row - 1 }
    down  p = p { row = p.row + 1 }
    left  p = p { col = p.col - 1 }
    right p = p { col = p.col + 1 }

-- | The single-line box-drawing glyph with the requested up\/down\/left\/
-- right arms. Dividers always carry both arms of their own axis, so only
-- tees and the cross arise in practice; corners round-trip harmlessly.
junction :: Bool -> Bool -> Bool -> Bool -> Char
junction u d l r = case (u, d, l, r) of
    (True,  True,  False, False) -> '│'
    (False, False, True,  True ) -> '─'
    (True,  True,  True,  True ) -> '┼'
    (True,  True,  True,  False) -> '┤'
    (True,  True,  False, True ) -> '├'
    (False, True,  True,  True ) -> '┬'
    (True,  False, True,  True ) -> '┴'
    (False, True,  False, True ) -> '┌'
    (False, True,  True,  False) -> '┐'
    (True,  False, False, True ) -> '└'
    (True,  False, True,  False) -> '┘'
    _                            -> if u || d then '│' else '─'

-- Splitting and closing -----------------------------------------------------

-- | @-f@: whether a split divides the target pane or the whole window.
data SplitSpan = SpanPane | SpanWindow
    deriving (Eq, Show)

-- | How to split: the axis, the new pane's size (cells; 'Nothing' halves),
-- which side it lands on, and what it divides.
data SplitSpec = SplitSpec
    { orient    :: Orientation
    , size      :: Maybe Int
    , placement :: Placement
    , span      :: SplitSpan
    }
    deriving (Eq, Show)

-- | Add @new@ beside @target@ (or beside the whole window under
-- 'SpanWindow'), taking the requested size from it (@layout_split_pane@).
-- 'Nothing' when there is no room for both panes.
splitPane :: SplitSpec -> PaneId -> PaneId -> Layout -> Maybe Layout
splitPane spec target new root = do
    lcPath <- case spec.span of
        SpanWindow -> Just []
        SpanPane -> pathTo target root
    let o = spec.orient
        lc = at lcPath root
        ss = extent o lc
        before = spec.placement == Before
        full = spec.span == SpanWindow
        s2raw = case spec.size of
            Nothing -> ((ss + 1) `div` 2) - 1
            Just n | before -> ss - n - 1
                   | otherwise -> n
        s2  -- a negative @s2raw@ is tmux's unsigned wrap: as large as allowed
            | s2raw < 0 = ss - 2
            | s2raw < paneMinimum = paneMinimum
            | s2raw > ss - 2 = ss - 2
            | otherwise = s2raw
        s1 = ss - 1 - s2
        newSize = if before then s2 else s1
        fresh = Layout 0 0 0 0 (LeafCell new)
        (first, second) = if before then (fresh, lc) else (lc, fresh)
        (first', second') = case o of
            LeftRight ->
                ( first { sx = s1, sy = lc.sy, xoff = lc.xoff, yoff = lc.yoff }
                , second { sx = s2, sy = lc.sy, xoff = lc.xoff + s1 + 1, yoff = lc.yoff } )
            TopBottom ->
                ( first { sx = lc.sx, sy = s1, xoff = lc.xoff, yoff = lc.yoff }
                , second { sx = lc.sx, sy = s2, xoff = lc.xoff, yoff = lc.yoff + s1 + 1 } )
        pair = map resizeChildCells [first', second']
        sameAxisParent = case lcPath of
            [] -> False
            _ -> case (at (init lcPath) root).cell of
                NodeCell t _ -> t == o
                LeafCell _ -> False
    guard (ss >= paneMinimum * 2 + 1)
    guard (not full || sizeFits o newSize lc)
    pure $ fixOffsets $ case lc.cell of
        _ | not full && sameAxisParent ->
            let i = last lcPath
            in modifyAt (init lcPath) (\p -> case p.cell of
                    NodeCell t cs -> p { cell = NodeCell t (take i cs <> pair <> drop (i + 1) cs) }
                    LeafCell _ -> p) root
        NodeCell t cs | full, t == o ->
            let resized = setExtent o ss (resizeChildCells (setExtent o newSize lc))
                kids = case resized.cell of
                    NodeCell _ ks -> ks
                    LeafCell _ -> cs
                added = setExtent o (ss - 1 - newSize) $ case o of
                    LeftRight -> fresh { sy = lc.sy }
                    TopBottom -> fresh { sx = lc.sx }
            in resized { cell = NodeCell t (if before then added : kids else kids <> [added]) }
        _ -> modifyAt lcPath (const lc { cell = NodeCell o pair }) root

-- | Close a pane's cell, giving its space to a neighbour (@layout_close_pane@).
-- 'Nothing' when it was the last pane.
removePane :: PaneId -> Layout -> Maybe Layout
removePane pid root = do
    p <- pathTo pid root
    guard (not (null p))
    pure (fixOffsets (destroyAt p root))

-- | Close the bottom-right cell, as a layout string with more cells than the
-- window has panes does (@layout_parse@). 'Nothing' for a lone cell.
destroyBottomRight :: Layout -> Maybe Layout
destroyBottomRight root = do
    let p = bottomRight root
    guard (not (null p))
    pure (fixOffsets (destroyAt p root))
  where
    bottomRight l = case l.cell of
        LeafCell _ -> []
        NodeCell _ cs -> (length cs - 1) : bottomRight (last cs)

-- | Remove the cell at a (non-root) path: the next sibling — the previous
-- one for a last child — absorbs its space and border, and a parent left
-- with one child is replaced by it (@layout_destroy_cell@).
destroyAt :: Path -> Layout -> Layout
destroyAt p = modifyAt (init p) collapse
  where
    i = last p
    collapse parent = case parent.cell of
        NodeCell t cs ->
            let gone = cs !! i
                j = if i == length cs - 1 then i - 1 else i + 1
                grown = modifyChild j (adjust t (extent t gone + 1)) cs
                kept = [ c | (k, c) <- zip [0 ..] grown, k /= i ]
            in case kept of
                [only] -> only
                _ -> parent { cell = NodeCell t kept }
        LeafCell _ -> parent

-- Rearranging ---------------------------------------------------------------

mapLeaves :: (PaneId -> PaneId) -> Layout -> Layout
mapLeaves f l = case l.cell of
    LeafCell p -> l { cell = LeafCell (f p) }
    NodeCell t cs -> l { cell = NodeCell t (map (mapLeaves f) cs) }

-- | Exchange two panes' positions in the tree; a pane absent from the tree
-- simply takes the other's place. Backs @swap-pane@.
swapLeaves :: PaneId -> PaneId -> Layout -> Layout
swapLeaves a b = mapLeaves $ \p ->
    if p == a then b else if p == b then a else p

-- | Which way @rotate-window@ turns the panes: @-U@ moves each pane to
-- the previous position (the first wraps to the last), @-D@ to the next.
data Rotation = RotateUp | RotateDown
    deriving (Eq, Show)

-- | Rotate the panes through the layout's positions, keeping its shape.
-- Backs @rotate-window@.
rotateLeaves :: Rotation -> Layout -> Layout
rotateLeaves dir lay = mapLeaves (\p -> fromMaybe p (lookup p moves)) lay
  where
    ps = layoutPanes lay
    turned = case (dir, ps) of
        (_, []) -> []
        (RotateUp, p : rest) -> rest <> [p]
        (RotateDown, _) -> last ps : init ps
    moves = zip ps turned

-- Resizing ------------------------------------------------------------------

-- | The cell a resize along @o@ moves for the pane at @p@: the pane's
-- nearest ancestor-or-self whose parent runs along @o@.
resizeAnchor :: Orientation -> Path -> Layout -> Maybe Path
resizeAnchor o p root = go p
  where
    go [] = Nothing
    go q = case (at (init q) root).cell of
        NodeCell t _ | t == o -> Just q
        _ -> go (init q)

-- | Move a pane's right\/bottom border by @change@ cells — its left\/top
-- one for a last child — taking the space from the nearest sibling that has
-- some, on either side (@layout_resize_pane@ with @opposite@).
resizePane :: PaneId -> Orientation -> Int -> Layout -> Layout
resizePane pid o change root = fromMaybe root $ do
    p <- pathTo pid root
    q <- resizeAnchor o p root
    let parentPath = init q
        n = case (at parentPath root).cell of
            NodeCell _ cs -> length cs
            LeafCell _ -> 1
        i0 = last q
        i = if i0 == n - 1 then i0 - 1 else i0
    pure (fixOffsets (modifyAt parentPath (resizeAmong o i change) root))

-- | @layout_resize_layout@ on the @i@th child of a node.
resizeAmong :: Orientation -> Int -> Int -> Layout -> Layout
resizeAmong o i change parent = case parent.cell of
    NodeCell t cs -> parent { cell = NodeCell t (loop change cs) }
    LeafCell _ -> parent
  where
    loop needed cs
        | needed == 0 = cs
        | change > 0 = case grow needed cs of
            Nothing -> cs
            Just (s, cs') -> loop (needed - s) cs'
        | otherwise = case shrink needed cs of
            Nothing -> cs
            Just (s, cs') -> loop (needed + s) cs'
    giving cs = List.find (\j -> spare o (cs !! j) > 0)
    grow needed cs = do
        j <- giving cs [i + 1 .. length cs - 1] <|> giving cs [i - 1, i - 2 .. 0]
        let s = min (spare o (cs !! j)) needed
        pure (s, modifyChild i (adjust o s) (modifyChild j (adjust o (negate s)) cs))
    shrink needed cs = do
        j <- giving cs [i, i - 1 .. 0]
        guard (i + 1 < length cs)
        let s = min (spare o (cs !! j)) (negate needed)
        pure (s, modifyChild (i + 1) (adjust o s) (modifyChild j (adjust o (negate s)) cs))

-- | tmux's @layout_search_by_border@: the cell whose right or bottom border
-- runs through a point.
searchByBorder :: Int -> Int -> Layout -> Maybe Path
searchByBorder x y l = case l.cell of
    LeafCell _ -> Nothing
    NodeCell t cs -> go t Nothing (zip [0 ..] cs)
  where
    inside c = x >= c.xoff && x < c.xoff + c.sx && y >= c.yoff && y < c.yoff + c.sy
    go _ _ [] = Nothing
    go t mlast ((i, c) : rest)
        | inside c = (i :) <$> searchByBorder x y c
        | Just (j, lst) <- mlast, between t lst c = Just [j]
        | otherwise = go t (Just (i, c)) rest
    between LeftRight lst c = x < c.xoff && x >= lst.xoff + lst.sx
    between TopBottom lst c = y < c.yoff && y >= lst.yoff + lst.sy

-- | The axis a cell's parent lays it along, for a cell below the root.
parentOrientation :: Path -> Layout -> Maybe Orientation
parentOrientation [] _ = Nothing
parentOrientation p root = case (at (init p) root).cell of
    NodeCell t _ -> Just t
    LeafCell _ -> Nothing

-- | tmux's @layout_resize_layout@ on the cell at a path: move its right or
-- bottom border by @change@ cells along its parent's axis.
resizeCellAt :: Path -> Int -> Layout -> Layout
resizeCellAt p change root = case parentOrientation p root of
    Nothing -> root
    Just o -> fixOffsets (modifyAt (init p) (resizeAmong o (last p) change) root)

-- | Resize a pane to @newSize@ cells along an axis (@layout_resize_pane_to@).
resizePaneTo :: PaneId -> Orientation -> Int -> Layout -> Layout
resizePaneTo pid o newSize root = fromMaybe root $ do
    p <- pathTo pid root
    q <- resizeAnchor o p root
    let lc = at q root
        isLast = case (at (init q) root).cell of
            NodeCell _ cs -> last q == length cs - 1
            LeafCell _ -> False
        cur = extent o lc
        change = if isLast then cur - newSize else newSize - cur
    pure (resizePane pid o change root)

-- | Even out the pane's siblings — or, if they already are, the nearest
-- ancestor's that are not (@layout_spread_out@, @select-layout -E@).
spreadOut :: PaneId -> Layout -> Layout
spreadOut pid root = fromMaybe root $ do
    p <- pathTo pid root
    guard (not (null p))
    go (init p)
  where
    go q = case spreadCell (at q root) of
        Just c' -> Just (fixOffsets (modifyAt q (const c') root))
        Nothing | null q -> Nothing
                | otherwise -> go (init q)

-- | Share a node's space evenly among its children, any remainder going to
-- the first ones (@layout_spread_cell@). 'Nothing' when nothing changes.
spreadCell :: Layout -> Maybe Layout
spreadCell parent = case parent.cell of
    NodeCell t cs -> do
        let number = length cs
            total = extent t parent
            each = (total - (number - 1)) `div` number
            remainder = total - number * (each + 1) + 1
            go _ [] = []
            go r (c : rest) =
                let this = each + (if r > 0 then 1 else 0)
                in adjust t (this - extent t c) c : go (r - 1) rest
            cs' = go remainder cs
        guard (number > 1 && total >= number - 1 && each > 0)
        guard (cs' /= cs)
        pure parent { cell = NodeCell t cs' }
    LeafCell _ -> Nothing

-- Named layouts -------------------------------------------------------------

-- | The tmux named layouts, in @layout_sets@ order.
data LayoutName
    = EvenHorizontal
    | EvenVertical
    | MainHorizontal
    | MainHorizontalMirrored
    | MainVertical
    | MainVerticalMirrored
    | Tiled
    deriving (Eq, Show, Enum, Bounded)

layoutNames :: [LayoutName]
layoutNames = [minBound .. maxBound]

layoutNameText :: LayoutName -> Text
layoutNameText = \case
    EvenHorizontal -> "even-horizontal"
    EvenVertical -> "even-vertical"
    MainHorizontal -> "main-horizontal"
    MainHorizontalMirrored -> "main-horizontal-mirrored"
    MainVertical -> "main-vertical"
    MainVerticalMirrored -> "main-vertical-mirrored"
    Tiled -> "tiled"

-- | A layout by its full name, else by a unique prefix (@layout_set_lookup@).
lookupLayoutName :: Text -> Maybe LayoutName
lookupLayoutName t =
    case [ n | n <- layoutNames, layoutNameText n == t ] of
        n : _ -> Just n
        [] -> case [ n | n <- layoutNames, t `T.isPrefixOf` layoutNameText n ] of
            [n] -> Just n
            _ -> Nothing

-- | The next layout in tmux's cycle for @next-layout@; @Nothing@ (no
-- named layout applied yet) starts at the head, and the tail wraps back
-- to the head.
nextLayoutName :: Maybe LayoutName -> LayoutName
nextLayoutName = \case
    Nothing -> minBound
    Just n | n == maxBound -> minBound
           | otherwise -> succ n

-- | The previous layout for @previous-layout@; @Nothing@ starts at the
-- tail, and the head wraps back to the tail.
previousLayoutName :: Maybe LayoutName -> LayoutName
previousLayoutName = \case
    Nothing -> maxBound
    Just n | n == minBound -> maxBound
           | otherwise -> pred n

-- | The option values the named layouts size from: @main-pane-width@\/
-- @-height@ and @other-pane-width@\/@-height@ (cells or @N%@), and
-- @tiled-layout-max-columns@.
data NamedSizes = NamedSizes
    { mainWidth   :: Text
    , mainHeight  :: Text
    , otherWidth  :: Text
    , otherHeight :: Text
    , maxColumns  :: Int
    }
    deriving (Eq, Show)

-- | tmux's option defaults.
defaultNamedSizes :: NamedSizes
defaultNamedSizes = NamedSizes "80" "24" "0" "0" 0

-- | Arrange @pids@ (in window order) into a named layout for a window of
-- @area@. 'Nothing' for fewer than two panes, which tmux leaves alone.
namedLayout :: NamedSizes -> Size -> LayoutName -> [PaneId] -> Maybe Layout
namedLayout sizes area name pids = do
    guard (n > 1)
    pure $ fixOffsets $ case name of
        EvenHorizontal -> evenLayout LeftRight
        EvenVertical -> evenLayout TopBottom
        MainHorizontal -> mainLayout TopBottom False
        MainHorizontalMirrored -> mainLayout TopBottom True
        MainVertical -> mainLayout LeftRight False
        MainVerticalMirrored -> mainLayout LeftRight True
        Tiled -> tiledLayout
  where
    n = length pids
    wsx = fromIntegral area.cols
    wsy = fromIntegral area.rows
    leaf w h p = Layout w h 0 0 (LeafCell p)
    node t w h cs = Layout w h 0 0 (NodeCell t cs)
    spreadOrKeep c = fromMaybe c (spreadCell c)
    evenLayout t =
        let needed = n * (paneMinimum + 1) - 1
            (w, h) = case t of
                LeftRight -> (max needed wsx, wsy)
                TopBottom -> (wsx, max needed wsy)
        in spreadOrKeep (node t w h [ leaf wsx wsy p | p <- pids ])
    -- The main pane along @t@ (its first child), the rest stacked across.
    mainLayout t mirrored =
        let others = n - 1
            (whole, mainOpt, otherOpt, fallback) = case t of
                TopBottom -> (wsy, sizes.mainHeight, sizes.otherHeight, 24)
                LeftRight -> (wsx, sizes.mainWidth, sizes.otherWidth, 80)
            avail = whole - 1
            mainReq = either (const fallback) id (stringPercentage 0 avail avail mainOpt)
            (mainS, otherS)
                | mainReq + paneMinimum >= avail =
                    ( if avail <= paneMinimum + paneMinimum then paneMinimum
                      else avail - paneMinimum
                    , paneMinimum )
                | otherwise = case stringPercentage 0 avail avail otherOpt of
                    Right o | o /= 0 ->
                        if o > avail || avail - o < mainReq
                            then (mainReq, avail - mainReq)
                            else (avail - o, o)
                    _ -> (mainReq, avail - mainReq)
            across = case t of
                TopBottom -> wsx
                LeftRight -> wsy
            acrossS = max (others * (paneMinimum + 1) - 1) across
            sized a b = case t of
                TopBottom -> (a, b)
                LeftRight -> (b, a)
            mk s p = let (w, h) = sized acrossS s in leaf w h p
            (rootW, rootH) = sized acrossS (mainS + otherS + 1)
        in case pids of
            m : rest ->
                let mainCell = mk mainS m
                    otherCell = case rest of
                        [o] -> mk otherS o
                        _ ->
                            let (w, h) = sized acrossS otherS
                                cross = if t == TopBottom then LeftRight else TopBottom
                                small p = let (lw, lh) = sized paneMinimum otherS
                                          in leaf lw lh p
                            in spreadOrKeep (node cross w h (map small rest))
                in node t rootW rootH
                    (if mirrored then [otherCell, mainCell] else [mainCell, otherCell])
            [] -> node t rootW rootH []
    tiledLayout =
        let maxc = sizes.maxColumns
            grid r c
                | r * c >= n = (r, c)
                | otherwise =
                    let r' = r + 1
                        c' = if r' * c < n && (maxc == 0 || c < maxc) then c + 1 else c
                    in grid r' c'
            (rowsN, colsN) = grid 1 1
            width = max paneMinimum ((wsx - (colsN - 1)) `div` colsN)
            height = max paneMinimum ((wsy - (rowsN - 1)) `div` rowsN)
            rootW = max ((width + 1) * colsN - 1) wsx
            rootH = max ((height + 1) * rowsN - 1) wsy
            chunks [] = []
            chunks xs = let (a, b) = splitAt colsN xs in a : chunks b
            rowOf [p] = leaf wsx height p
            rowOf ps =
                let used = length ps * (width + 1) - 1
                    kids = [ leaf width height p | p <- ps ]
                    kids' | wsx > used = init kids <> [adjust LeftRight (wsx - used) (last kids)]
                          | otherwise = kids
                in node LeftRight wsx height kids'
            rowCells = map rowOf (chunks pids)
            usedH = rowsN * height + rowsN - 1
            rowCells'
                | wsy > usedH = init rowCells <> [adjust TopBottom (wsy - usedH) (last rowCells)]
                | otherwise = rowCells
        in case rowCells' of
            [only] -> only
            _ -> node TopBottom rootW rootH rowCells'

-- Pane navigation -----------------------------------------------------------

-- | @select-pane@'s cyclic direction: the pane after or before the
-- active one in window order. See 'cyclePane'.
data PaneCycle = PaneNext | PanePrev
    deriving (Eq, Show)

-- | The pane @select-pane -t :.+@ / @:.-@ lands on: the one after
-- ('PaneNext') or before ('PanePrev') the active pane in @pids@ order,
-- wrapping at the ends. 'Nothing' when the active pane is not in the
-- list; a single-pane window stays put.
cyclePane :: PaneCycle -> [PaneId] -> PaneId -> Maybe PaneId
cyclePane dir pids active = resolvePaneIndex (IndexRelative dir 1) pids active

-- | A @select-pane -t@ pane-index: the @.[+-][N]@ tail of a tmux target.
-- See 'parsePaneIndex' (grammar) and 'resolvePaneIndex' (meaning against a
-- window's pane order).
data PaneIndex
    = IndexAbsolute Int           -- ^ @.N@ — the pane at positional index N
    | IndexRelative PaneCycle Int -- ^ @.+N@ / @.-N@ — cycle N panes forward\/back
    deriving (Eq, Show)

-- | Parse the pane-index tail of a @select-pane -t@ target. tmux's target
-- grammar ends in an optional @.[+-][N]@; this recognises that tail after
-- an optional window prefix. @.+@\/@.-@ (or a bare @+@\/@-@) cycle by one;
-- @.+N@\/@.-N@ cycle by N; @.N@ (or a bare number) selects absolute index
-- N. A missing count means N=1. Anything else is 'Nothing'.
parsePaneIndex :: Text -> Maybe PaneIndex
parsePaneIndex t = parseIndex paneIndexPart
  where
    -- The pane index is the part after the last dot; without a dot the
    -- whole target is the index (a bare @+@\/@-@\/number).
    paneIndexPart = snd (T.breakOnEnd "." t)
    parseIndex part = case T.uncons part of
        Just ('+', rest) -> IndexRelative PaneNext <$> count rest
        Just ('-', rest) -> IndexRelative PanePrev <$> count rest
        _                -> IndexAbsolute <$> wholeNum part
    -- A relative count defaults to 1 when omitted.
    count rest
        | T.null rest = Just 1
        | otherwise   = wholeNum rest
    wholeNum s = case TR.decimal s of
        Right (k, rest) | T.null rest -> Just k
        _                             -> Nothing

-- | The pane @select-pane -t@ lands on for a 'PaneIndex', against a
-- window's @pids@ order and its @active@ pane. 'IndexRelative' cycles by
-- the count (wrapping, like 'cyclePane' stepped N times); 'IndexAbsolute'
-- picks the pane at that positional index, or 'Nothing' when out of range
-- or the active pane is absent.
resolvePaneIndex :: PaneIndex -> [PaneId] -> PaneId -> Maybe PaneId
resolvePaneIndex idx pids active = case idx of
    IndexAbsolute k -> case drop k pids of
        (p : _) -> Just p
        []      -> Nothing
    IndexRelative dir k -> do
        i <- List.elemIndex active pids
        let step = case dir of PaneNext -> k; PanePrev -> negate k
        pure (pids !! ((i + step) `mod` length pids))

-- | The pane a directional @select-pane@ moves to, resolved against the full
-- split layout rather than a zoom-collapsed arrangement, so navigation still
-- finds a neighbor (and can then cancel zoom) while a pane is zoomed. See
-- 'Hat.Server.cmdSelectPane'.
directionalTarget :: Size -> Layout -> PaneId -> Direction -> Maybe PaneId
directionalTarget sz lay active dir =
    neighbor (fst (arrange (sizeRect sz) lay)) active dir

-- | Geometric pane navigation: the adjacent pane in a direction with the
-- largest shared edge. At the window edge the search wraps around to the pane
-- at the opposite edge of the whole window, perpendicular overlap breaking a
-- tie among the panes sharing that edge — so @select-pane -R@ from the
-- rightmost pane reaches the leftmost, however many columns lie between.
neighbor :: [(PaneId, Rect)] -> PaneId -> Direction -> Maybe PaneId
neighbor rects from dir = do
    fromRect <- List.lookup from rects
    let adjacents =
            [ ((overlap fromRect r, 0), pid)
            | (pid, r) <- rects
            , pid /= from
            , adjacent fromRect r
            , overlap fromRect r > 0
            ]
        wraps =
            [ ((farness fromRect r, overlap fromRect r), pid)
            | (pid, r) <- rects
            , pid /= from
            , wrapped fromRect r
            , overlap fromRect r > 0
            ]
    case adjacents of
        [] -> best wraps
        cs -> best cs
  where
    best = \case
        [] -> Nothing
        cs -> Just (snd (maximum cs))
    adjacent a b = case dir of
        DirLeft -> b.endCol + 1 == a.startCol || b.endCol == a.startCol
        DirRight -> a.endCol + 1 == b.startCol || a.endCol == b.startCol
        DirUp -> b.endRow + 1 == a.startRow || b.endRow == a.startRow
        DirDown -> a.endRow + 1 == b.startRow || a.endRow == b.startRow
    -- The far pane the search rolls onto: for a leftward move it lies to the
    -- right of the source (and vice versa), so wrapping crosses the window.
    wrapped a b = case dir of
        DirLeft -> b.startCol > a.startCol
        DirRight -> b.startCol < a.startCol
        DirUp -> b.startRow > a.startRow
        DirDown -> b.startRow < a.startRow
    -- How far the wrapped pane's own far border sits past the source's, in the
    -- wrap direction; the larger, the closer to the opposite window edge.
    farness a b = case dir of
        DirLeft -> b.endCol - a.endCol
        DirRight -> a.startCol - b.startCol
        DirUp -> b.endRow - a.endRow
        DirDown -> a.startRow - b.startRow
    overlap a b = case dir of
        DirLeft -> rowOverlap a b
        DirRight -> rowOverlap a b
        DirUp -> colOverlap a b
        DirDown -> colOverlap a b
    rowOverlap a b = min a.endRow b.endRow - max a.startRow b.startRow
    colOverlap a b = min a.endCol b.endCol - max a.startCol b.startCol
