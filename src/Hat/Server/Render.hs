-- | Frame composition and diffing: pane grids in, 'DrawOp's out.
--
-- The server keeps the last frame sent to each client and sends only
-- the cells that changed, grouped into styled text runs.
module Hat.Server.Render
    ( Frame
    , blankFrame
    , composeFrame
    , overlayGrid
    , applyBorders
    , diffFrame
    , fullRedraw
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V

import Hat.Geometry
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
-- both the rect and the frame.
overlayGrid :: Frame -> Rect -> V.Vector (V.Vector Cell) -> Frame
overlayGrid frame rect grid = V.imap overlayRow frame
  where
    overlayRow r frameRow
        | r < rect.startRow || r >= rect.endRow = frameRow
        | otherwise =
            let src = maybe V.empty (\x -> x) (grid V.!? (r - rect.startRow))
            in V.imap (overlayCell src) frameRow
    overlayCell src c cell
        | c < rect.startCol || c >= rect.endCol = cell
        | otherwise = maybe blankCell (\x -> x) (src V.!? (c - rect.startCol))

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

-- | Ops that turn @old@ into @new@ on the client's terminal.
diffFrame :: Frame -> Frame -> [DrawOp]
diffFrame old new = concat
    [ rowOps r oldRow newRow
    | (r, newRow) <- zip [0 ..] (V.toList new)
    , let oldRow = maybe V.empty id (old V.!? r)
    , oldRow /= newRow
    ]

rowOps :: Int -> V.Vector Cell -> V.Vector Cell -> [DrawOp]
rowOps r oldRow newRow = runsToOps r newRow (widenChanged newRow changed)
  where
    n = V.length newRow
    changed = V.generate n $ \c ->
        case oldRow V.!? c of
            Nothing -> True
            Just oldCell -> oldCell /= newRow V.! c

-- A wide char and its continuation cell redraw together: touching
-- either marks both.
widenChanged :: V.Vector Cell -> V.Vector Bool -> V.Vector Bool
widenChanged row changed = V.generate (V.length changed) $ \c ->
    let self = changed V.! c
        asLead = cellWidth c == 2 && orFalse (changed V.!? (c + 1))
        asCont = cellWidth c == 0 && c > 0 && changed V.! (c - 1)
    in self || asLead || asCont
  where
    cellWidth c = maybe 1 (.width) (row V.!? c)
    orFalse = maybe False id

-- Group consecutive changed cells with equal style into single Puts.
runsToOps :: Int -> V.Vector Cell -> V.Vector Bool -> [DrawOp]
runsToOps r row changed = go 0
  where
    n = V.length row
    go c
        | c >= n = []
        | not (changed V.! c) = go (c + 1)
        | otherwise =
            let st = (row V.! c).style
                runEnd = findRunEnd c st
                txt = T.concat [(row V.! i).text | i <- [c .. runEnd - 1]]
            in Put Pos { row = r, col = c } st txt : go runEnd
    findRunEnd c st
        | c >= n = c
        | changed V.! c && (row V.! c).style == st = findRunEnd (c + 1) st
        | otherwise = c

-- | Redraw everything (first frame after attach, or after resize).
-- The screen is cleared first, so unchanged-from-blank cells are skipped.
fullRedraw :: Frame -> [DrawOp]
fullRedraw new = ClearAll : diffFrame (V.map (V.map (const blankCell)) new) new
