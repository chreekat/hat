-- | What every cell of a client's window area shows, before any pane
-- content is drawn: tmux's redraw scene (@screen-redraw.c@'s
-- @redraw_build_cells@). Each cell is pane content, a border shared by the
-- panes around it, a pane status line, or fill — inside the window where no
-- pane reaches, or outside it where the client is bigger than the window.
-- 'sceneChrome' then turns every non-pane cell into a styled cell, as
-- tmux's @redraw_draw_border_span@ and @redraw_draw_status_span@ do.
module Hat.Server.Scene
    ( -- * Building
      SceneInput (..)
    , FloatingPane (..)
    , PaneStatus (..)
    , Scene
    , SceneCell (..)
    , ChromeKind (..)
    , sceneKinds
    , BorderCell (..)
    , CellType (..)
    , buildScene
    , markingOrder
    , statusAdjust
      -- * Drawing
    , ChromeLook (..)
    , ScrollbarSide (..)
    , ScrollbarStyle (..)
    , ScrollbarLook (..)
    , scrollbarStyle
    , overlayWidth
    , sliderOf
    , sceneChrome
    , statusBase
    , borderGlyph
    , fillCell
    ) where

import Data.Bits ((.&.), (.|.))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as V

import Hat.Geometry
import Hat.Model.Ids (PaneId)
import Hat.Model.Options (BorderLines (..))
import Hat.Server.FormatDraw
    (DrawStyle (..), Width (..), acsGlyph, formatDraw, styleParse, themedDrawStyle)
import Hat.Server.Layout (Orientation (..))
import Hat.Server.Theme (Theme)
import Hat.Term.Cell qualified as Cell

-- | @pane-scrollbars-position@.
data ScrollbarSide = ScrollbarRight | ScrollbarLeft
    deriving (Eq, Show)

-- | @pane-border-status@.
data PaneStatus = PaneStatusOff | PaneStatusTop | PaneStatusBottom
    deriving (Eq, Show)

-- | tmux's border cell types (@CELL_*@): which neighbours a border cell
-- joins, named by the arms its glyph has.
data CellType
    = CellUD | CellLR | CellRD | CellLD | CellRU | CellLU
    | CellLRD | CellLRU | CellURD | CellULD | CellLRUD | CellNone
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | A border cell: the panes on each side of it (a pane above it owns its
-- @top@ … ), the pane whose style colours it in a two-pane split, and the
-- arms joining it to neighbouring border cells.
data BorderCell = BorderCell
    { top :: Maybe PaneId
    , bottom :: Maybe PaneId
    , left :: Maybe PaneId
    , right :: Maybe PaneId
    , styleOwner :: Maybe PaneId
    , mask :: Int
    }
    deriving (Eq, Show)

data SceneCell
    = ScenePane PaneId
    | SceneOutside  -- ^ beyond the window's edge
    | SceneEmpty    -- ^ inside the window, but no pane reaches it
    | SceneBorder BorderCell
    | SceneStatus PaneId Int CellType
        -- ^ a pane's status line: the column within it, and the border
        -- type the line was drawn over
    | SceneScrollbar PaneId Int Int
        -- ^ a pane's scrollbar: the pane row, and the column within the
        -- scrollbar (its padding included)
    | SceneMenu Int Int
        -- ^ the window's menu: the column and row within its box
    deriving (Eq, Show)

-- | Rows of cells.
type Scene = V.Vector (V.Vector SceneCell)

-- | A kind of chrome a scene can call for.
data ChromeKind
    = BorderChrome
    | StatusChrome
    | FillChrome  -- ^ cells no pane covers: 'SceneOutside' and 'SceneEmpty'
    deriving (Eq, Ord, Show)

-- | Every kind of chrome anywhere in a scene.
sceneKinds :: Scene -> Set ChromeKind
sceneKinds = V.foldl' (V.foldl' (\ks c -> maybe ks (`Set.insert` ks) (kindOf c))) Set.empty
  where
    kindOf = \case
        SceneBorder _ -> Just BorderChrome
        SceneStatus {} -> Just StatusChrome
        SceneOutside -> Just FillChrome
        SceneEmpty -> Just FillChrome
        ScenePane _ -> Nothing
        SceneScrollbar {} -> Nothing
        SceneMenu {} -> Nothing

data SceneInput = SceneInput
    { window :: Size
    , area :: Size
        -- ^ the scene's own size: at least the window's, grown to cover
        -- the client
    , panes :: [(PaneId, Rect)]
        -- ^ every visible pane's content, in window coordinates (after
        -- 'statusAdjust'), in marking order
    , paneStatus :: PaneStatus
    , twoPane :: Maybe Orientation
        -- ^ for the colour indicator: the layout's split when it holds
        -- exactly two panes (tmux's @redraw_check_two_pane_colours@)
    , scrollbars :: Map PaneId Int
        -- ^ the panes showing a scrollbar, with its width and padding
    , scrollbarSide :: ScrollbarSide
    , overlays :: Map PaneId Int
        -- ^ the panes showing a scrollbar over their own content, with its
        --   width there ('overlayWidth')
    , floats :: [FloatingPane]
        -- ^ the floating panes, back to front
    , menu :: Maybe Rect
        -- ^ the window's menu box, in window coordinates
    }
    deriving (Eq, Show)

-- | A floating pane as its scene draws it.
data FloatingPane = FloatingPane
    { pane :: PaneId
    , rect :: Rect
        -- ^ its content, in window coordinates, unclipped
    , lineSet :: BorderLines
    , status :: PaneStatus
    }
    deriving (Eq, Show)

borderL, borderR, borderU, borderD :: Int
borderL = 1
borderR = 2
borderU = 4
borderD = 8

-- | tmux's @redraw_get_cell_type@.
cellTypeOf :: Int -> CellType
cellTypeOf m = case m of
    15 -> CellLRUD
    7 -> CellLRU
    11 -> CellLRD
    3 -> CellLR
    1 -> CellLR
    2 -> CellLR
    13 -> CellULD
    5 -> CellLU
    9 -> CellLD
    14 -> CellURD
    6 -> CellRU
    10 -> CellRD
    12 -> CellUD
    4 -> CellUD
    8 -> CellUD
    _ -> CellNone

-- | A pane's content rect once its window's pane status line takes a row
-- from it (tmux's @layout_fix_panes@): the panes along the window's top
-- edge move down a row for a top status line, those along its bottom edge
-- give up their last row for a bottom one; each keeps at least a row.
statusAdjust :: PaneStatus -> Int -> Rect -> Rect
statusAdjust st windowRows r = case st of
    PaneStatusTop | r.startRow == 0 ->
        let start = r.startRow + 1 in r { startRow = start, endRow = start + shrunk }
    PaneStatusBottom | r.endRow == windowRows -> r { endRow = r.startRow + shrunk }
    _ -> r
  where
    h = r.endRow - r.startRow
    shrunk = if h > 1 then h - 1 else h

-- | The order tmux marks a window's tiled panes in: newest first (the
-- reverse of its z-order, where each new pane joins the end).
markingOrder :: [(PaneId, a)] -> [(PaneId, a)]
markingOrder = List.sortOn (Down . fst)

-- Note [Chrome over panes]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
-- A scene gives each cell to exactly one thing: a pane's content, or
-- chrome. A floating pane takes the cells under its box from whatever was
-- there, tiled panes and borders alike, so pane rects overlap but the
-- scene's chrome never sits where it shows a pane. A frame therefore draws
-- each pane's whole rect back to front, then stamps the chrome over them:
-- a later pane hides the content of one behind it, and the chrome (a
-- floating pane's border among it) hides any content the scene gave to it.

-- | tmux's @redraw_build_cells@: the tiled panes, then the floating ones
-- back to front, each over everything before it, then the window's menu
-- above them all.
buildScene :: SceneInput -> Scene
buildScene inp =
    split (markMenu (List.foldl' markFloat (List.foldl' markPane blank inp.panes) inp.floats))
  where
    wsx = fromIntegral inp.window.cols
    wsy = fromIntegral inp.window.rows
    ssx = max wsx (fromIntegral inp.area.cols)
    ssy = max wsy (fromIntegral inp.area.rows)
    blank = Map.empty :: Map (Int, Int) SceneCell
    reset x y
        | x < wsx && y < wsy = SceneEmpty
        | otherwise = SceneOutside
    get m x y = case Map.lookup (x, y) m of
        Just c -> c
        -- Tiled panes' insides go unmarked: no cell but a floating pane's
        -- covers them.
        Nothing -> case [ pid | (pid, r) <- inp.panes, inRect r x y ] of
            (pid : _) -> ScenePane pid
            [] -> reset x y
    inRect r x y = x >= r.startCol && x < r.endCol && y >= r.startRow && y < r.endRow
    -- redraw_window_to_scene: the window's far edge is still drawable.
    inScene x y = x >= 0 && y >= 0 && x <= wsx && y <= wsy && x < ssx && y < ssy

    markPane m (pid, r) = markOverlay pid r (markScrollbar pid r (markStatus pid r (markBorders pid r m)))

    sbWidth pid = Map.findWithDefault 0 pid inp.scrollbars
    sbLeft = inp.scrollbarSide == ScrollbarLeft

    -- redraw_mark_pane_scrollbar, for a scrollbar beside the pane.
    markScrollbar pid r m = case sbWidth pid of
        0 -> m
        w ->
            let x0 = if sbLeft then r.startCol - w else r.endCol
            in List.foldl' (\acc (x, y) -> Map.insert (x, y)
                    (SceneScrollbar pid (y - r.startRow) (x - x0)) acc)
                m [ (x, y) | y <- [r.startRow .. r.endRow - 1], x <- [x0 .. x0 + w - 1]
                           , inScene x y ]

    -- redraw_mark_pane_scrollbar, for a scrollbar over the pane's own edge.
    markOverlay pid r m = case Map.findWithDefault 0 pid inp.overlays of
        0 -> m
        w ->
            let x0 = if sbLeft then r.startCol else r.endCol - w
            in List.foldl' (\acc (x, y) -> Map.insert (x, y)
                    (SceneScrollbar pid (y - r.startRow) (x - x0)) acc)
                m [ (x, y) | y <- [r.startRow .. r.endRow - 1], x <- [x0 .. x0 + w - 1]
                           , inScene x y ]

    edges r = (r.startCol - 1, r.endCol, r.startRow - 1, r.endRow)

    -- A scrollbar sits between the pane and its border.
    borderEdges pid r =
        let (lft, rgt, tp, btm) = edges r
            w = sbWidth pid
        in if sbLeft then (lft - w, rgt, tp, btm) else (lft, rgt + w, tp, btm)

    markBorders pid r m0 =
        let (lft, rgt, tp, btm) = borderEdges pid r
            markTop = tp >= 0 && inp.paneStatus /= PaneStatusBottom
            markBottom = btm <= wsy && inp.paneStatus /= PaneStatusTop
            markLeft = lft >= 0
            markRight = rgt <= wsx
            horiz y ownTop ownBottom m = List.foldl'
                (\acc x -> markCell pid r x y ownTop ownBottom
                    ((if x > lft then borderL else 0) .|. (if x < rgt then borderR else 0)) acc)
                m [lft .. rgt]
            vert x m = List.foldl'
                (\acc y -> markCell pid r x y False False
                    ((if y > tp then borderU else 0) .|. (if y < btm then borderD else 0)) acc)
                m [tp .. btm]
            m1 = if markTop then horiz tp False True m0 else m0
            m2 = if markBottom then horiz btm True False m1 else m1
            m3 = if markLeft then vert lft m2 else m2
        in if markRight then vert rgt m3 else m3

    -- redraw_mark_border_cell for a tiled pane: only fill and border cells
    -- take a border; a border merges this pane's sides into its own.
    markCell pid r x y ownTop ownBottom msk m
        | not (inScene x y) = m
        | otherwise = case get m x y of
            SceneBorder b -> Map.insert (x, y) (SceneBorder (own b)) m
            SceneEmpty -> Map.insert (x, y) (SceneBorder (own fresh)) m
            SceneOutside -> Map.insert (x, y) (SceneBorder (own fresh)) m
            _ -> m
      where
        fresh = freshBorder
        own b0 =
            let b1 = if ownTop then b0 { top = Just pid } else b0
                b2 = if ownBottom then b1 { bottom = Just pid } else b1
                b3 | msk .&. (borderU .|. borderD) == 0 = b2
                   | x < r.startCol = b2 { right = Just pid }
                   | x >= r.endCol = b2 { left = Just pid }
                   | otherwise = b2
            in b3 { mask = b3.mask .|. msk }

    markStatus pid r m = statusRow inp.paneStatus pid r (borderEdges pid r) m

    -- redraw_mark_border_status: the pane's status line takes the border
    -- cells of its status row from two columns in to its right border.
    statusRow st pid r (_, rgt, tp, btm) m = case st of
        PaneStatusOff -> m
        _ ->
            let y = if st == PaneStatusTop then tp else btm
                sx = r.startCol + 2
            in List.foldl' (\acc (x, off) ->
                    if not (inScene x y) then acc else case get acc x y of
                        SceneBorder b -> Map.insert (x, y)
                            (SceneStatus pid off (cellTypeOf b.mask)) acc
                        _ -> acc)
                m (zip [sx .. rgt - 1] [0 ..])

    -- redraw_mark_pane for a floating pane: its inside and its border
    -- (clipped to the window, the edges the window cuts off not drawn)
    -- replace whatever they cover.
    markFloat m0 f =
        markOverlay f.pane f.rect (markScrollbar f.pane f.rect (markFloatBorders f (markInside f m0)))

    markInside f m = List.foldl' (\acc (x, y) -> Map.insert (x, y) (ScenePane f.pane) acc) m
        [ (x, y) | y <- [max 0 f.rect.startRow .. min wsy f.rect.endRow - 1]
                 , x <- [max 0 f.rect.startCol .. min wsx f.rect.endCol - 1]
                 , inScene x y ]

    markFloatBorders f m0
        | f.lineSet == BorderNone = m0
        | otherwise =
            let (lft0, rgt0, tp0, btm0) = borderEdges f.pane f.rect
                (lft, rgt, tp, btm) =
                    (max 0 lft0, min (wsx - 1) rgt0, max 0 tp0, min (wsy - 1) btm0)
                horiz y ownTop ownBottom m = List.foldl'
                    (\acc x -> markFloatCell f x y ownTop ownBottom
                        ((if x > lft then borderL else 0) .|. (if x < rgt then borderR else 0)) acc)
                    m [lft .. rgt]
                vert x m = List.foldl'
                    (\acc y -> markFloatCell f x y False False
                        ((if y > tp then borderU else 0) .|. (if y < btm then borderD else 0)) acc)
                    m [tp .. btm]
                m1 = if tp0 >= 0 then horiz tp False True m0 else m0
                m2 = if btm0 < wsy then horiz btm True False m1 else m1
                m3 = if lft0 >= 0 then vert lft m2 else m2
                m4 = if rgt0 < wsx then vert rgt m3 else m3
                clamped = (lft, rgt, tp, btm)
            in statusRow f.status f.pane f.rect clamped m4

    -- redraw_mark_menu: the box takes every cell under it.
    markMenu m = case inp.menu of
        Nothing -> m
        Just r -> List.foldl'
            (\acc (x, y) -> Map.insert (x, y) (SceneMenu (x - r.startCol) (y - r.startRow)) acc) m
            [ (x, y) | y <- [r.startRow .. r.endRow - 1], x <- [r.startCol .. r.endCol - 1]
                     , inScene x y ]

    -- redraw_mark_border_cell for a floating pane: it takes any cell, only
    -- merging with a border it already owns.
    markFloatCell f x y ownTop ownBottom msk m
        | not (inScene x y) = m
        | otherwise = Map.insert (x, y) (SceneBorder (own base)) m
      where
        pid = f.pane
        base = case get m x y of
            SceneBorder b | hasPane b pid -> b
            _ -> freshBorder
        own b0 =
            let b1 = if ownTop then b0 { top = Just pid } else b0
                b2 = if ownBottom then b1 { bottom = Just pid } else b1
                b3 | msk .&. (borderU .|. borderD) == 0 = b2
                   | x < f.rect.startCol = b2 { right = Just pid }
                   | x >= f.rect.endCol = b2 { left = Just pid }
                   | otherwise = b2
            in b3 { mask = b3.mask .|. msk }

    split m = V.generate ssy $ \y -> V.generate ssx $ \x ->
        twoPaneOwner x y (get m x y)

    -- redraw_mark_two_pane_colours: each half of the one dividing border
    -- takes the colour of the pane beside it.
    twoPaneOwner x y = \case
        SceneBorder b -> SceneBorder $ case inp.twoPane of
            Just LeftRight | isJust b.left, isJust b.right ->
                b { styleOwner = if y <= wsy `div` 2 then b.left else b.right }
            Just TopBottom | isJust b.top, isJust b.bottom ->
                b { styleOwner = if x <= wsx `div` 2 then b.top else b.bottom }
            _ -> b
        c -> c

freshBorder :: BorderCell
freshBorder = BorderCell Nothing Nothing Nothing Nothing Nothing 0

-- | How a scene's chrome looks.
data ChromeLook = ChromeLook
    { borderLines :: BorderLines
        -- ^ the window's @pane-border-lines@
    , lineSets :: Map PaneId BorderLines
        -- ^ a floating pane's own @pane-border-lines@
    , windowStyle :: Cell.Style
        -- ^ the window's @pane-border-style@, for cells no pane owns
    , paneStyles :: Map PaneId Cell.Style
        -- ^ each pane's own border style (@pane-active-border-style@ for
        -- the active pane)
    , scrollbarLooks :: Map PaneId ScrollbarLook
    , active :: Maybe PaneId
    , marked :: Maybe PaneId
    , numbers :: Map PaneId Int  -- ^ each pane's index, for @number@ lines
    , paneLines :: Map PaneId (V.Vector Cell.Cell)
    , fillInside :: Cell.Cell
    , fillOutside :: Cell.Cell
    , arrows :: Map Pos Char
        -- ^ indicator glyphs replacing the border at those cells
    , menuCells :: V.Vector (V.Vector Cell.Cell)
        -- ^ the window's menu as it draws itself
    }
    deriving (Eq, Show)

-- | Every non-pane cell of a scene, drawn. See Note [Chrome over panes].
sceneChrome :: ChromeLook -> Scene -> [(Pos, Cell.Cell)]
sceneChrome look scene =
    [ (pos, c)
    | (y, row) <- zip [0 ..] (V.toList scene)
    , (x, sc) <- zip [0 ..] (V.toList row)
    , let pos = Pos { row = y, col = x }
    , Just c <- [cellAt pos sc] ]
  where
    cellAt pos = \case
        ScenePane _ -> Nothing
        SceneScrollbar pid y off -> scrollbarCell y off <$> Map.lookup pid look.scrollbarLooks
        SceneMenu x y -> look.menuCells V.!? y >>= (V.!? x)
        SceneOutside -> Just look.fillOutside
        SceneEmpty -> Just look.fillInside
        SceneStatus pid off ct -> Just $
            fromMaybe (Cell.glyphCell (borderGlyph (linesOf look (Just pid)) Nothing ct) look.windowStyle)
                (Map.lookup pid look.paneLines >>= (V.!? off))
        SceneBorder b ->
            let owner = borderOwner look.active b
                sty = maybe look.windowStyle
                    (\p -> Map.findWithDefault look.windowStyle p look.paneStyles) owner
                marked = maybe False (hasPane b) look.marked
                sty' = if marked then sty { Cell.reverse = not sty.reverse } else sty
                glyph = fromMaybe
                    (borderGlyph (linesOf look owner) (owner >>= (`Map.lookup` look.numbers))
                        (cellTypeOf b.mask))
                    (Map.lookup pos look.arrows)
            in Just (Cell.glyphCell glyph sty')

-- | The line set a pane's border draws in (tmux's
-- @window_pane_get_pane_lines@): a floating pane's own, else the window's.
linesOf :: ChromeLook -> Maybe PaneId -> BorderLines
linesOf look = maybe look.borderLines (\p -> Map.findWithDefault look.borderLines p look.lineSets)

-- | The pane whose border style colours a border cell (tmux's
-- @redraw_get_pane_for_border_style@): its two-pane owner, else the active
-- pane beside it, else any pane beside it.
borderOwner :: Maybe PaneId -> BorderCell -> Maybe PaneId
borderOwner mactive b = case b.styleOwner of
    Just p -> Just p
    Nothing
        | Just a <- mactive, hasPane b a -> Just a
        | otherwise -> case [ p | Just p <- [b.top, b.bottom, b.left, b.right] ] of
            (p : _) -> Just p
            [] -> Nothing

hasPane :: BorderCell -> PaneId -> Bool
hasPane b p = Just p `elem` [b.top, b.bottom, b.left, b.right]

-- | A pane's status line before its format is drawn over it (tmux's
-- @window_make_pane_status@): the border it replaces, in the pane's border
-- style, as wide as the status cells the scene gave it. 'Nothing' when it
-- has none.
statusBase :: BorderLines -> Cell.Style -> Int -> Scene -> PaneId -> Maybe (V.Vector Cell.Cell)
statusBase lns sty number scene pid = case cells of
    [] -> Nothing
    _ ->
        let width = maximum (map fst cells) + 1
            typeAt i = fromMaybe CellLR (lookup i cells)
        in Just $ V.generate width $ \i ->
            Cell.glyphCell (borderGlyph lns (Just number) (typeAt i)) sty
  where
    cells = [ (off, ct) | row <- V.toList scene, SceneStatus p off ct <- V.toList row
            , p == pid ]

-- | The glyph a border cell of a type draws under @pane-border-lines@
-- (tmux's @window_get_border_cell@); @number@ draws the owning pane's
-- index, or @*@ for a border no pane owns.
borderGlyph :: BorderLines -> Maybe Int -> CellType -> Char
borderGlyph lns number ct = case lns of
    BorderNumber
        | ct == CellNone -> acsGlyph '~'
        | otherwise -> maybe '*' (\i -> toEnum (fromEnum '0' + i `mod` 10)) number
    BorderSpaces -> ' '
    BorderNone -> ' '
    BorderSimple -> " |-+++++++++." !! (fromEnum ct + 1)
    BorderDouble -> "\x2551\x2550\x2554\x2557\x255a\x255d\x2566\x2569\x2560\x2563\x256c\x00b7"
        !! fromEnum ct
    BorderHeavy -> "\x2503\x2501\x250f\x2513\x2517\x251b\x2533\x253b\x2523\x252b\x254b\x00b7"
        !! fromEnum ct
    BorderSingle -> acsGlyph ("xqlkmjwvtun~" !! fromEnum ct)

-- | A window's fill cell (tmux's @window_set_fill_cell@ and
-- @window_copy_fill_cell@): the expanded @fill-character@ drawn into one
-- cell, laid over the border style — its glyph and attributes added, its
-- colours taken where it sets them.
fillCell :: Theme -> Cell.Style -> Text -> Cell.Cell
fillCell th border expanded = Cell.Cell drawn.content Cell.Style
    { Cell.fg = pick f.fg border.fg, Cell.bg = pick f.bg border.bg
    , Cell.bold = f.bold || border.bold
    , Cell.underline = f.underline || border.underline
    , Cell.italic = f.italic || border.italic
    , Cell.reverse = f.reverse || border.reverse
    , Cell.strike = f.strike || border.strike
    , Cell.blink = f.blink || border.blink
    , Cell.faint = f.faint || border.faint }
  where
    drawn = formatDraw th Cell.defaultStyle expanded (V.singleton Cell.blankCell) V.! 0
    f = drawn.style
    pick c dflt = if c == Cell.DefaultColor then dflt else c

-- | A pane's @pane-scrollbars-style@: its colours, and the scrollbar's
-- width and padding in cells.
data ScrollbarStyle = ScrollbarStyle
    { colours :: Cell.Style
    , width :: Int
    , pad :: Int
    }
    deriving (Eq, Show)

-- | tmux's @style_set_scrollbar_style_from_option@: the option over the
-- registry default, the default alone when the option does not parse; at
-- least a cell wide.
scrollbarStyle :: Theme -> Text -> ScrollbarStyle
scrollbarStyle th raw = ScrollbarStyle
    { colours = sy.cell
    , width = case sy.width of
        Just (WidthCells n) | n >= 1 -> n
        Just (WidthPercent n) | n >= 1 -> n
        _ -> 1
    , pad = fromMaybe 0 sy.pad }
  where
    base = themedDrawStyle th
    dflt = fromMaybe base (styleParse Cell.defaultStyle base defaultScrollbarStyle)
    sy = fromMaybe dflt (styleParse Cell.defaultStyle dflt raw)

defaultScrollbarStyle :: Text
defaultScrollbarStyle = "bg=themedarkgrey,fg=themelightgrey,width=1,pad=0"

-- | The columns a scrollbar of a style's @(width, pad)@ takes over an
-- @sx@-column pane (tmux's @redraw_mark_pane@): both while they fit, else
-- its width alone, within the pane.
overlayWidth :: (Int, Int) -> Int -> Int
overlayWidth (w, pad) sx
    | w + pad <= sx = w + pad
    | otherwise = min w sx

-- | How one pane's scrollbar draws.
data ScrollbarLook = ScrollbarLook
    { style :: ScrollbarStyle
    , slider :: (Int, Int)  -- ^ the slider's first row and height
    , padStyle :: Cell.Style  -- ^ the pane's own default colours
    , onLeft :: Bool
    }
    deriving (Eq, Show)

-- | The slider's first row and height in a scrollbar @height@ rows tall
-- (tmux's @redraw_draw_scrollbar_span@): over the pane's history and
-- screen, at the bottom; in copy mode, at the viewport's offset into the
-- history.
sliderOf :: Int -> Int -> Int -> Maybe (Int, Int) -> (Int, Int)
sliderOf height screenRows hsize copy = (clampY y0, max 1 h0)
  where
    sbh = fromIntegral height :: Double
    (y0, h0) = case copy of
        Nothing ->
            let total = fromIntegral (screenRows + hsize)
                h = truncate (sbh * (sbh / total))
            in (height - max 0 h, h)
        Just (offset, size) ->
            let total = fromIntegral size + sbh
            in ( truncate ((sbh + 1) * (fromIntegral offset / total))
               , truncate (sbh * (sbh / total)) )
    clampY y = if y >= height then height - 1 else y

scrollbarCell :: Int -> Int -> ScrollbarLook -> Cell.Cell
scrollbarCell y off look
    | isPad = Cell.glyphCell ' ' look.padStyle
    | y >= sy && y < sy + sh =
        Cell.glyphCell ' ' sty { Cell.fg = sty.bg, Cell.bg = sty.fg }
    | otherwise = Cell.glyphCell ' ' sty
  where
    sty = look.style.colours
    (sy, sh) = look.slider
    w = look.style.width
    isPad
        | look.onLeft = off >= w && off < w + look.style.pad
        | otherwise = off < look.style.pad
