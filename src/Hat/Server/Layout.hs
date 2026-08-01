-- | Pane geometry: the split tree and its projection onto a window's
-- rectangle. One border row/column separates siblings, tmux-style.
module Hat.Server.Layout
    ( Layout (..)
    , Orientation (..)
    , Placement (..)
    , Direction (..)
    , LayoutName (..)
    , PaneCycle (..)
    , PaneIndex (..)
    , parsePaneIndex
    , resolvePaneIndex
    , ResizeMode (..)
    , effectiveWindowSize
    , sessionWindowArea
    , nextLayoutName
    , previousLayoutName
    , cyclePane
    , arrange
    , sizeRect
    , layoutPanes
    , splitLeaf
    , splitFull
    , swapLeaves
    , removeLeaf
    , neighbor
    , directionalTarget
    , resizeSplit
    , namedLayout
    , childRects
    ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Ord (clamp, comparing)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

import Hat.Geometry
import Hat.Model.Ids (PaneId)

data Orientation = LeftRight | TopBottom
    deriving (Eq, Show)

-- | How a window chooses its size among the clients attached to its session.
-- See 'effectiveWindowSize'.
data ResizeMode
    = SmallestClient  -- ^ shrink to fit every attached client
    | ActiveClient    -- ^ track the most-recently-active client (aggressive-resize)
    deriving (Eq, Show)

-- | The size a window takes for the sizes of the clients attached to its
-- session. 'SmallestClient' intersects every client so the window fits them
-- all (the tmux default). 'ActiveClient' (@aggressive-resize on@) follows the
-- most-recently-active client — the one with the largest activity stamp —
-- letting smaller clients clip. With no clients attached the window keeps its
-- @fallback@ (its last size).
effectiveWindowSize :: ResizeMode -> Size -> [(Int, Size)] -> Size
effectiveWindowSize _ fallback [] = fallback
effectiveWindowSize SmallestClient _ clients = Size
    { rows = minimum (map (\(_, s) -> s.rows) clients)
    , cols = minimum (map (\(_, s) -> s.cols) clients)
    }
effectiveWindowSize ActiveClient _ clients =
    snd (List.maximumBy (comparing fst) clients)

-- | The pane area of a session's window: its 'effectiveWindowSize' with one
-- status row carved out when a client is attached to draw it. A detached
-- session keeps its full size — tmux draws no status line without a client, so
-- @new-session -d -x W -y H@ gives an H-row window, not H-1.
sessionWindowArea :: ResizeMode -> Size -> [(Int, Size)] -> Size
sessionWindowArea _ fallback [] = fallback
sessionWindowArea mode fallback clients =
    let s = effectiveWindowSize mode fallback clients
    in s { rows = max 1 (s.rows - 1) }

data Direction = DirLeft | DirRight | DirUp | DirDown
    deriving (Eq, Show)

-- | @-b@: which side of the target a new pane lands on.
data Placement
    = Before  -- ^ the new pane takes the left\/top slot
    | After   -- ^ the new pane takes the right\/bottom slot
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
arrange rect lay =
    let (rects, borders) = arrangeRaw rect lay
    in (rects, resolveJunctions borders)

-- | The raw projection: dividers as plain @│@\/@─@ runs. Sibling dividers
-- only abut here; 'resolveJunctions' fuses them into box-drawing junctions
-- once the whole tree's borders are known.
arrangeRaw :: Rect -> Layout -> ([(PaneId, Rect)], [(Pos, Char)])
arrangeRaw rect = \case
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
        in arrangeRaw ra a <> arrangeRaw rb b <> (mempty, border)
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
        in arrangeRaw ra a <> arrangeRaw rb b <> (mempty, border)

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

-- | Split the target leaf in two; the new pane takes half. 'Before'
-- puts the new pane on the left/top.
splitLeaf :: PaneId -> Orientation -> Placement -> PaneId -> Layout -> Layout
splitLeaf target orient placement newPid = go
  where
    go = \case
        Leaf p
            | p == target -> case placement of
                Before -> Split orient (1 % 2) (Leaf newPid) (Leaf p)
                After  -> Split orient (1 % 2) (Leaf p) (Leaf newPid)
            | otherwise -> Leaf p
        Split o r a b -> Split o r (go a) (go b)

-- | A full-window split: the new pane becomes a sibling of the /entire/
-- existing layout, spanning the full window height (@LeftRight@) or width
-- (@TopBottom@). 'Before' puts it on the left/top. Backs @split-window -f@.
splitFull :: Orientation -> Placement -> PaneId -> Layout -> Layout
splitFull orient placement newPid old = case placement of
    Before -> Split orient (1 % 2) (Leaf newPid) old
    After  -> Split orient (1 % 2) old (Leaf newPid)

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

-- | @select-pane@'s cyclic direction: the pane after or before the
-- active one in window order. See 'cyclePane'.
data PaneCycle = PaneNext | PanePrev
    deriving (Eq, Show)

-- tmux's fixed layout cycle order, walked by @next-layout@.
layoutOrder :: [LayoutName]
layoutOrder = [EvenHorizontal, EvenVertical, MainHorizontal, MainVertical, Tiled]

-- | The next layout in tmux's cycle for @next-layout@; @Nothing@ (no
-- named layout applied yet) starts at the head, and the tail wraps back
-- to the head.
nextLayoutName :: Maybe LayoutName -> LayoutName
nextLayoutName = stepLayout layoutOrder

-- | The previous layout for @previous-layout@; @Nothing@ starts at the
-- tail, and the head wraps back to the tail.
previousLayoutName :: Maybe LayoutName -> LayoutName
previousLayoutName = stepLayout (reverse layoutOrder)

stepLayout :: [LayoutName] -> Maybe LayoutName -> LayoutName
stepLayout order mcur = case mcur >>= (`List.elemIndex` order) of
    Just i  -> order !! ((i + 1) `mod` length order)
    Nothing -> case order of
        (first : _) -> first
        []          -> error "stepLayout: empty layout order"

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
        Right (n, rest) | T.null rest -> Just n
        _                             -> Nothing

-- | The pane @select-pane -t@ lands on for a 'PaneIndex', against a
-- window's @pids@ order and its @active@ pane. 'IndexRelative' cycles by
-- the count (wrapping, like 'cyclePane' stepped N times); 'IndexAbsolute'
-- picks the pane at that positional index, or 'Nothing' when out of range
-- or the active pane is absent.
resolvePaneIndex :: PaneIndex -> [PaneId] -> PaneId -> Maybe PaneId
resolvePaneIndex idx pids active = case idx of
    IndexAbsolute n -> case drop n pids of
        (p : _) -> Just p
        []      -> Nothing
    IndexRelative dir n -> do
        i <- List.elemIndex active pids
        let step = case dir of PaneNext -> n; PanePrev -> negate n
        pure (pids !! ((i + step) `mod` length pids))

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

-- | Geometric pane navigation: the adjacent pane in a direction with the
-- largest shared edge. At the window edge the search wraps around to the
-- opposite side, landing on the pane there with the largest perpendicular
-- overlap — so @select-pane -R@ from the rightmost pane reaches the leftmost.
-- | The pane a directional @select-pane@ moves to, resolved against the full
-- split layout rather than a zoom-collapsed arrangement, so navigation still
-- finds a neighbor (and can then cancel zoom) while a pane is zoomed. See
-- 'Hat.Server.cmdSelectPane'.
directionalTarget :: Size -> Layout -> PaneId -> Direction -> Maybe PaneId
directionalTarget sz lay active dir =
    neighbor (fst (arrange (sizeRect sz) lay)) active dir

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
        -- On a tie in overlap the farthest wrapped pane wins, so a wrap
        -- lands on the pane at the very opposite edge.
        wraps =
            [ ((overlap fromRect r, farness fromRect r), pid)
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
    -- How far past the source the wrapped pane sits, in the wrap direction;
    -- the larger, the closer to the opposite edge.
    farness a b = case dir of
        DirLeft -> b.startCol - a.startCol
        DirRight -> a.startCol - b.startCol
        DirUp -> b.startRow - a.startRow
        DirDown -> a.startRow - b.startRow
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
