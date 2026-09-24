-- | Frame composition and diffing: pane grids in, 'DrawOp's out.
--
-- The server keeps the last frame sent to each client and sends only
-- the cells that changed, grouped into styled text runs.
module Hat.Server.Render
    ( Frame
    , blankFrame
    , composeFrame
    , RowOrigin (..)
    , RowSpan (..)
    , PaneSlice (..)
    , sameOrigin
    , PaneLayer (..)
    , Chrome (..)
    , chromeFromCells
    , composeRows
    , overlayGrid
    , applyBorders
    , tintInnerRing
    , diffFrame
    , diffFrameKnown
    , fullRedraw
    ) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU

import Hat.Geometry
import Hat.Model.Ids (PaneId)
import Hat.PtrEq (samePtr)
import Hat.Term.Cell
import Hat.Transport.Wire (DrawOp (..))

type Frame = V.Vector (V.Vector Cell)

blankFrame :: Size -> Frame
blankFrame sz = V.replicate (fromIntegral sz.rows)
    (V.replicate (fromIntegral sz.cols) blankCell)

-- | Place a grid at the origin of a client-sized frame, clipping or
-- padding with blanks as needed. (Multi-pane composition arrives with
-- layouts; this is the single-pane case.)
composeFrame :: Size -> V.Vector (V.Vector Cell) -> Frame
composeFrame sz grid = V.generate (fromIntegral sz.rows) $ \r ->
    let src = maybe V.empty id (grid V.!? r)
        w = fromIntegral sz.cols
    in V.generate w $ \c -> maybe blankCell id (src V.!? c)

-- | Draw a pane grid into a frame at the given rectangle, clipping to
-- both the rect and the frame. Rows outside the rect keep their
-- identity, and a source row covering the full frame width passes
-- through as-is, so unchanged rows stay pointer-equal for reuse checks
-- downstream.
overlayGrid :: Frame -> Rect -> V.Vector (V.Vector Cell) -> Frame
overlayGrid frame rect grid = frame V.// updates
  where
    updates =
        [ (r, overlayRowAt rect grid r (frame V.! r))
        | r <- [max 0 rect.startRow .. min (V.length frame) rect.endRow - 1]
        ]

-- See 'overlayGrid' for the clipping and pass-through rules.
overlayRowAt :: Rect -> V.Vector (V.Vector Cell) -> Int -> V.Vector Cell -> V.Vector Cell
overlayRowAt rect grid r frameRow
    | Just srcRow <- msrc
    , rect.startCol == 0, rect.endCol >= V.length frameRow
    , V.length srcRow == V.length frameRow = srcRow
    | otherwise = V.imap overlayCell frameRow
  where
    msrc = grid V.!? (r - rect.startRow)
    src = fromMaybe V.empty msrc
    overlayCell c cell
        | c < rect.startCol || c >= rect.endCol = cell
        | otherwise = fromMaybe blankCell (src V.!? (c - rect.startCol))

-- | What produced one row of a client's composed frame: the ordered
-- contributions 'composeRows' laid onto it, or anything else. Two equal
-- span lists promise equal cells; 'VolatileRow' promises nothing, so
-- 'RowOrigin' has no 'Eq' — matching is 'sameOrigin'.
data RowOrigin = VolatileRow | ComposedRow [RowSpan]
    deriving Show

-- | One contribution to a composed frame row, in draw order: the chrome
-- stamped beneath the panes — carried as its row's 'Chrome' generation,
-- 0 when the row has no chrome cells at all — or one pane's slice.
data RowSpan
    = ChromeSpan Int
    | PaneSpan PaneSlice
    deriving (Eq, Show)

-- | Which pane row (at which generation stamp,
-- 'Hat.Term.Emulator.snapshotWithGens') supplied cols @[from, to)@.
data PaneSlice = PaneSlice
    { pane :: PaneId
    , prow :: Int
    , gen  :: Int
    , from :: Int
    , to   :: Int
    }
    deriving (Eq, Show)

-- | Whether two rows' provenance proves their cells equal. The pointer
-- check is only ever a shortcut for the structural one, and only on
-- 'ComposedRow' — two 'VolatileRow's share one static object but must
-- still never match.
sameOrigin :: RowOrigin -> RowOrigin -> Bool
sameOrigin a@(ComposedRow as) b@(ComposedRow bs) = samePtr a b || as == bs
sameOrigin _ _ = False

-- | One pane's contribution to 'composeRows': where it sits, its cells,
-- and their generation stamps when the cells are a plain snapshot.
data PaneLayer = PaneLayer
    { pane  :: PaneId
    , rect  :: Rect
    , cells :: V.Vector (V.Vector Cell)
    , gens  :: Maybe (V.Vector Int)
    }
    deriving Show

-- | A frame's chrome (border) cells, grouped by row and stamped: two
-- 'Chrome's with the same generation hold the same cells everywhere, so
-- a row's chrome contribution is pinned by the generation alone. See
-- 'Hat.Server.View.renderOnce' for the cache that maintains the stamp.
data Chrome = Chrome
    { gen :: Int
    , byRow :: Map.Map Int [(Int, Cell)]
    }
    deriving Show

-- | Chrome from bare positioned cells, as a one-off (generation 1).
chromeFromCells :: [(Pos, Cell)] -> Chrome
chromeFromCells cells = Chrome
    { gen = 1
    , byRow = Map.fromListWith (<>)
        [ (p.row, [(p.col, cell)]) | (p, cell) <- cells ]
    }

-- | Compose a client frame row by row — chrome cells onto blanks, then
-- each pane's slice in draw order — reusing the previous frame's row
-- wherever the row's provenance matches ('sameOrigin'): a matched row is
-- the old frame's very vector, never recomposed, and the caller's diff
-- may skip it unread. Rows whose provenance promises nothing (a pane
-- without stamps, a stale stamp during a resize) come out 'VolatileRow'
-- and are always recomposed.
composeRows
    :: Size -> Chrome -> [PaneLayer]
    -> Frame -> V.Vector RowOrigin
    -> (Frame, V.Vector RowOrigin)
composeRows sz chrome layers old oldOrigins = (frame, origins)
  where
    rowsN = fromIntegral sz.rows
    colsN = fromIntegral sz.cols
    chromeAt r = Map.findWithDefault [] r chrome.byRow
    -- Chrome-free rows stamp 0: a border change elsewhere leaves them be.
    chromeStamp r = if null (chromeAt r) then 0 else chrome.gen
    layersAt r = [ l | l <- layers, r >= l.rect.startRow, r < l.rect.endRow ]
    -- Rows and origins are forced as they are built: a lazily reused row
    -- would otherwise chain thunks onto the previous frame's rows for as
    -- long as it stays unread.
    origins = V.fromListN rowsN
        [ o | r <- [0 .. rowsN - 1], let !o = mkOrigin r ]
    -- A row whose spans would come out identical keeps the old origin
    -- OBJECT — matched field-by-field before anything is allocated — so
    -- this frame's reuse check and the next frame's take the pointer
    -- shortcut in 'sameOrigin'.
    mkOrigin r = case oldOrigins V.!? r of
        Just prev@(ComposedRow (ChromeSpan g : ps))
            | g == chromeStamp r, panesMatch ps (layersAt r) -> prev
        _ -> maybe VolatileRow (ComposedRow . (ChromeSpan (chromeStamp r) :))
            (traverse (paneSpan r) (layersAt r))
      where
        panesMatch (PaneSpan sl : ps) (l : ls)
            | Just gens <- l.gens
            , Just g <- gens V.!? (r - l.rect.startRow) =
                sl.pane == l.pane && sl.prow == r - l.rect.startRow
                    && sl.gen == g && sl.from == max 0 l.rect.startCol
                    && sl.to == min colsN l.rect.endCol
                    && panesMatch ps ls
        panesMatch ps ls = null ps && null ls
    paneSpan r l = do
        gens <- l.gens
        g <- gens V.!? (r - l.rect.startRow)
        pure $ PaneSpan PaneSlice
            { pane = l.pane, prow = r - l.rect.startRow, gen = g
            , from = max 0 l.rect.startCol, to = min colsN l.rect.endCol }
    frame = V.fromListN rowsN
        [ row | r <- [0 .. rowsN - 1], let !row = rowAt r ]
    rowAt r = case old V.!? r of
        Just prev
            | V.length prev == colsN
            , sameOrigin (origins V.! r)
                (fromMaybe VolatileRow (oldOrigins V.!? r)) -> prev
        _ -> composeRow r
    blankRow = V.replicate colsN blankCell
    composeRow r = List.foldl' paneOver chromed (layersAt r)
      where
        -- A chrome-free row starts as the shared blank row itself, so a
        -- full-width pane pass-through composes without any copy.
        chromed = case chromeAt r of
            [] -> blankRow
            cs -> blankRow V.// [ (c, cell) | (c, cell) <- cs, c < colsN ]
        paneOver row l = overlayRowAt l.rect l.cells r row

-- | Stamp pre-styled border cells onto a frame. The caller chooses each
-- cell's glyph (per @pane-border-lines@) and style (per the pane-border
-- styles and @pane-border-indicators@).
applyBorders :: Frame -> [(Pos, Cell)] -> Frame
applyBorders frame borders = frame V.// updates
  where
    byRow = Map.toList $ Map.fromListWith (<>)
        [ (p.row, [(p.col, cell)]) | (p, cell) <- borders ]
    updates =
        [ (r, row V.// [ (c, cell) | (c, cell) <- cols, c < V.length row ])
        | (r, cols) <- byRow
        , r < V.length frame
        , let row = frame V.! r
        ]

-- | Restyle the ring of cells just inside the rect, keeping their glyphs.
tintInnerRing :: Style -> Frame -> Rect -> Frame
tintInnerRing sty frame rect = V.imap tintRow frame
  where
    tintRow r row
        | r < rect.startRow || r >= rect.endRow = row
        | otherwise = V.imap (tintCol r) row
    tintCol r c cell
        | c < rect.startCol || c >= rect.endCol = cell
        | r == rect.startRow || r == rect.endRow - 1
            || c == rect.startCol || c == rect.endCol - 1 =
            cell { style = sty }
        | otherwise = cell

-- | Ops that turn @old@ into @new@ on the client's terminal.
diffFrame :: Frame -> Frame -> [DrawOp]
diffFrame = diffFrameKnown (\_ -> False)

-- | 'diffFrame' skipping — without comparing — rows the caller knows are
-- equal in both frames. The predicate must be true only for rows whose
-- cells genuinely match, or the screen keeps stale content. A row that is
-- the same vector in both frames (compose reuse) is skipped outright.
diffFrameKnown :: (Int -> Bool) -> Frame -> Frame -> [DrawOp]
diffFrameKnown known old new = go 0
  where
    go r
        | r >= V.length new = []
        | samePtr oldRow newRow || known r || oldRow == newRow =
            go (r + 1)
        | otherwise = rowOps r oldRow newRow <> go (r + 1)
      where
        newRow = new V.! r
        oldRow = fromMaybe V.empty (old V.!? r)

rowOps :: Int -> V.Vector Cell -> V.Vector Cell -> [DrawOp]
rowOps r oldRow newRow = runsToOps r newRow eff
  where
    n = V.length newRow
    raw = VU.generate n $ \c ->
        let nc = newRow V.! c
        in case oldRow V.!? c of
            Nothing -> True
            Just oldCell -> not (samePtr oldCell nc) && oldCell /= nc
    -- A wide char and its continuation cell redraw together: touching
    -- either marks both.
    eff = VU.generate n $ \c ->
        raw VU.! c
            || (colWidth c == 2 && c + 1 < n && raw VU.! (c + 1))
            || (colWidth c == 0 && c > 0 && raw VU.! (c - 1))
    colWidth c = maybe 1 cellWidth (newRow V.!? c)

-- Group consecutive changed cells with equal style into single Puts.
runsToOps :: Int -> V.Vector Cell -> VU.Vector Bool -> [DrawOp]
runsToOps r row changed = go 0
  where
    n = V.length row
    go c
        | c >= n = []
        | not (changed VU.! c) = go (c + 1)
        | otherwise =
            let st = (row V.! c).style
                runEnd = findRunEnd c st
                txt = T.pack $
                    concatMap (\i -> cluster (row V.! i)) [c .. runEnd - 1]
            in Put Pos { row = r, col = c } st txt : go runEnd
    findRunEnd c st
        | c >= n = c
        | changed VU.! c && (row V.! c).style == st = findRunEnd (c + 1) st
        | otherwise = c

-- | Redraw everything (first frame after attach, or after resize).
-- The screen is cleared first, so unchanged-from-blank cells are skipped.
fullRedraw :: Frame -> [DrawOp]
fullRedraw new = ClearAll : diffFrame (V.map (V.map (const blankCell)) new) new
