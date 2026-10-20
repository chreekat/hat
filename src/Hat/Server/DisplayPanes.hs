-- | @display-panes@ as tmux's panes-mode (@window-panes.c@): a picture of a
-- window drawn in the pane the mode holds — each pane's screen where the
-- layout puts it, scaled when the window and the pane differ in size, its
-- number stamped over it and @display-panes-format@ along its top — from
-- which a key or a click chooses a pane to run a command on.
module Hat.Server.DisplayPanes
    ( cmdDisplayPanes
    , panesModeKey
    , panesModeMouse
    , drawPanesMode
      -- * Drawing
    , PanesPicture (..)
    , PanesPane (..)
    , panesPicture
    , panesNumber
    , panesKeyIndex
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM, forM_, void, when)
import Data.Bits ((.&.), (.|.))
import Data.Char (isAsciiLower, isDigit, ord)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

import Hat.Geometry (Pos (..), Rect (..), Size (..))
import Hat.Model
import Hat.Model.Options (BorderLines (..), Options (..), optionText)
import Hat.Server.ArgNumber (strtonum)
import Hat.Server.Clock (digitGlyph)
import Hat.Server.Command.Types
    (CommandImpl, Dispatch (..), Reply (..), parseArgs, templateReplace, unknownFlag)
import Hat.Server.Floating (stacking)
import Hat.Server.FormatDraw (colourValue, formatDraw)
import Hat.Server.FormatEnv (FormatScope (..), FormatTarget (..), clientTheme, expandAt)
import Hat.Server.Keys (Key (..))
import Hat.Server.Layout (Cell (..), Layout (..), Orientation (..), fitLayout, paneCell)
import Hat.Server.Locate (findTarget, locatePane)
import Hat.Server.ModeStack (SetMode (..), unzoomPaneWindow)
import Hat.Server.PaneMode (resetPaneMode, setPaneMode, zoomForMode)
import Hat.Server.Resize (applySessionSize, windowPaneStatus)
import Hat.Server.Scene (PaneStatus (..), borderGlyph, cellTypeOf)
import Hat.Server.Target qualified as Target
import Hat.Server.Toast (showToast)
import Hat.Server.TreeMode (modeClient, previewCells, styleIn)
import Hat.Term.Cell qualified as Cell
import Hat.Term.Emulator qualified as Emu

-- | @display-panes [-bkNZ] [-d duration] [-s source-window] [-t
-- target-pane] [template]@: put the target pane into panes-mode, showing
-- the panes of its window (or of @-s@). @-b@, tmux's \"don't block the
-- queue\" from before the mode, is already so: the mode never blocks.
cmdDisplayPanes :: CommandImpl
cmdDisplayPanes st mclient args
    | Just e <- unknownFlag "display-panes" "bkNZ" flags = pure [RErr e]
    | otherwise = findTarget st mclient Target.FindPane (lookup "-t" opts) >>= \case
        Left e -> pure [RErr e]
        Right (sess, _, win, pane) -> do
            msource <- case lookup "-s" opts of
                Nothing -> pure (Right (sess.id, win.id))
                Just s -> fmap (\(ss, _, sw, _) -> (ss.id, sw.id))
                    <$> findTarget st mclient Target.FindWindow (Just s)
            wopts <- resolveForWindowIO st sess win
            let mdelay = maybe (Right wopts.displayPanesTime) (strtonum 0 maxBound)
                    (lookup "-d" opts)
            case (msource, mdelay) of
                (Left e, _) -> pure [RErr e]
                (_, Left cause) -> pure [RErr ("delay " <> cause)]
                (Right (ssid, swid), Right delay) -> do
                    entered <- setPaneMode st pane "panes-mode"
                        (PanesMode <$> start sess win pane ssid swid) exit
                    -- The mode now hides its pane's chrome.
                    applySessionSize st sess.id
                    when (entered == ModeEntered && delay > 0) $ do
                        Just (PanesMode ps) <- atomically (topMode pane)
                        void . forkIO $ do
                            threadDelay (delay * 1000)
                            expirePanes st pane ps.shownAt
                    pure []
  where
    (opts, flags, pos) = parseArgs "dst" args
    has f = f `elem` flags
    exit = if has "-k" then KillPane else KeepPane
    start sess win pane ssid swid = do
        wasZoomed <- isJust <$> readTVarIO win.zoomed
        (preview, zoom) <- if has "-Z" || wasZoomed
            then pure (Nothing, ModeLeftZoom)
            else do
                scr <- Emu.snapshot pane.emulator
                z <- zoomForMode st sess win pane
                let cursor = if scr.cursorVisible then Just scr.cursor else Nothing
                pure (Just PanesPreview { cells = scr.cells, cursor }, z)
        now <- getMonotonicTimeNSec
        pure PanesState
            { sourceSession = ssid
            , sourceWindow = swid
            , session = sess.id
            , template = case pos of
                t : _ -> t
                [] -> "select-pane -t \"%%%\""
            , hold = if has "-N" then PanesHold else PanesClose
            , zoom
            , preZoom = preview
            , shownAt = now
            }

-- | Leave the pane's panes-mode when its duration is up, if it is still the
-- showing that set the timer.
expirePanes :: ServerState -> Pane -> Word64 -> IO ()
expirePanes st pane shown = do
    same <- atomically $ topMode pane >>= \case
        Just (PanesMode ps) -> pure (ps.shownAt == shown)
        _ -> pure False
    when same (resetPaneMode st pane)

-- | A key while the pane shows panes-mode: @q@ and @Escape@ leave, a pane's
-- number chooses it, and any other key leaves too unless @-N@, which
-- ignores every key but those that leave.
panesModeKey :: Dispatch -> ServerState -> Maybe Client -> Pane -> PanesState -> Key -> IO ()
panesModeKey d st mclient pane ps key
    | key.name `elem` ["q", "Escape"] = resetPaneMode st pane
    | ps.hold == PanesHold = pure ()
    | otherwise = do
        mchosen <- case panesKeyIndex key.name of
            Nothing -> pure Nothing
            Just ix -> atomically $ panesSource st ps >>= \case
                Nothing -> pure Nothing
                Just (_, _, win) -> paneAtIndex st win ix
        case mchosen of
            Nothing -> resetPaneMode st pane
            Just chosen -> choose d st mclient pane ps chosen

-- | A mouse key while the pane shows panes-mode: a press of the first
-- button chooses the pane drawn at @at@, in the mode's own coordinates.
panesModeMouse
    :: Dispatch -> ServerState -> Maybe Client -> Pane -> PanesState -> Text -> (Int, Int)
    -> IO ()
panesModeMouse d st mclient pane ps keyName (x, y)
    | ps.hold == PanesHold || keyName /= "MouseDown1Pane" = pure ()
    | otherwise = do
        (_, areas) <- panesScreen st pane ps
        let hit (_, r) = x >= r.startCol && x < r.endCol && y >= r.startRow && y < r.endRow
        forM_ (List.find hit (reverse areas)) $ \(pid, _) ->
            choose d st mclient pane ps pid

-- | Run the mode's template on the chosen pane and leave the mode, the
-- window unzoomed first.
choose :: Dispatch -> ServerState -> Maybe Client -> Pane -> PanesState -> PaneId -> IO ()
choose d st mclient pane ps chosen = do
    unzoomPaneWindow st pane
    resetPaneMode st pane
    mc <- modeClient st mclient pane
    out <- d.runCommandText st mc
        (templateReplace ps.template ("%" <> T.pack (show (rawPane chosen))) 1)
    forM_ mc $ \c -> forM_ [ e | RErr e <- out ] (showToast st c)

-- | The pane index a key names (tmux's @window_panes_key_pane@): a digit,
-- or an unmodified letter for 10 onwards.
panesKeyIndex :: Text -> Maybe Int
panesKeyIndex name = case T.unpack name of
    [c] | isDigit c -> Just (ord c - ord '0')
        | isAsciiLower c -> Just (10 + ord c - ord 'a')
    _ -> Nothing

-- | The window's pane at an index, counted from @pane-base-index@.
paneAtIndex :: ServerState -> Window -> Int -> STM (Maybe PaneId)
paneAtIndex st win ix = do
    base <- (.paneBaseIndex) <$> readTVar st.options
    ps <- windowPanes win
    pure (lookup ix (zip [base ..] (map (.id) ps)))

-- | The window the mode draws, with the session and window index it is
-- found at: the source session's, else the target session's, else any
-- holding it; 'Nothing' once the window is gone.
panesSource :: ServerState -> PanesState -> STM (Maybe (Session, Int, Window))
panesSource st ps = do
    sessions <- readTVar st.sessions
    let inSession sid = case Map.lookup sid sessions of
            Nothing -> pure Nothing
            Just sess -> do
                ws <- readTVar sess.windows
                pure $ case [ (ix, w) | (ix, w) <- Map.toList ws, w.id == ps.sourceWindow ] of
                    (ix, w) : _ -> Just (sess, ix, w)
                    [] -> Nothing
    a <- inSession ps.sourceSession
    b <- inSession ps.session
    others <- mapM (inSession . (.id)) (Map.elems sessions)
    pure (listToMaybe (catMaybes (a : b : others)))

-- | The mode's screen, for the renderer.
drawPanesMode :: ServerState -> Pane -> PanesState -> IO (V.Vector (V.Vector Cell.Cell))
drawPanesMode st pane ps = fst <$> panesScreen st pane ps

-- | The mode's screen and where each pane is drawn on it.
panesScreen
    :: ServerState -> Pane -> PanesState
    -> IO (V.Vector (V.Vector Cell.Cell), [(PaneId, Rect)])
panesScreen st pane ps = do
    size <- readTVarIO pane.size
    msrc <- atomically (panesSource st ps)
    mhome <- atomically (locatePane st pane.id)
    sessions <- readTVarIO st.sessions
    case (msrc, mhome) of
        (Just (ssess, swix, swin), Just (tsid, twin))
            | Just tsess <- Map.lookup tsid sessions -> do
                th <- clientTheme st
                topts <- resolveForWindowIO st tsess twin
                twix <- atomically (readTVar tsess.currentIx)
                (root, status, panes, active, floatOrder) <- atomically $ do
                    eff <- readTVar swin.size
                    lay <- readTVar swin.layout
                    let root = fitLayout eff lay
                    status <- windowPaneStatus st swin
                    panes <- windowPanes swin
                    active <- readTVar swin.activeId
                    fs <- readTVar swin.floats
                    pure (root, status, panes, active, reverse (stacking fs root))
                base <- (.paneBaseIndex) <$> readTVarIO st.options
                let fmt = optionText "display-panes-format" topts
                    colourOf pid = Cell.defaultStyle
                        { Cell.fg = fromMaybe Cell.DefaultColor $ maybe Nothing id $
                            colourValue th $ optionText
                                (if pid == active then "display-panes-active-colour"
                                 else "display-panes-colour") topts }
                tpix <- paneIndexIn base twin pane
                border <- styleIn th
                    (expandAt st (FormatTarget (InPane tsess twix twin tpix pane) Map.empty))
                    (optionText "display-panes-border-style" topts)
                drawn <- forM (zip [base ..] panes) $ \(n, p) -> do
                    scr <- Emu.snapshot p.emulator
                    let fgc = colourOf p.id
                    label <- if T.null fmt then pure Nothing else do
                        expanded <- expandAt st
                            (FormatTarget (InPane ssess swix swin n p) Map.empty) fmt
                        pure (Just (drawnCells (formatDraw th fgc expanded)))
                    let own = if p.id == pane.id then ps.preZoom else Nothing
                    pure PanesPane
                        { pane = p.id
                        , number = n
                        , cells = scr.cells
                        , cursor = if scr.cursorVisible then Just scr.cursor else Nothing
                        , preZoom = own
                        , colour = fgc
                        , label
                        }
                pure $ panesPicture PanesPicture
                    { root, size, status, panes = drawn, floatOrder, border }
        _ -> pure (blankGrid size, [])
  where
    paneIndexIn base win p = do
        order <- atomically (map (.id) <$> windowPanes win)
        pure (maybe base (base +) (List.elemIndex p.id order))

blankGrid :: Size -> V.Vector (V.Vector Cell.Cell)
blankGrid size = V.replicate (fromIntegral size.rows)
    (V.replicate (fromIntegral size.cols) Cell.blankCell)

-- | The cells a drawing writes into an @n@-cell row, by column: what it
-- leaves untouched is no cell at all.
drawnCells :: (V.Vector Cell.Cell -> V.Vector Cell.Cell) -> Int -> [(Int, Cell.Cell)]
drawnCells draw n =
    [ (c, cell) | (c, cell) <- zip [0 ..] (V.toList (draw untouched)), cell /= hole ]
  where
    hole = Cell.glyphCell '\0' Cell.defaultStyle
    untouched = V.replicate n hole

-- | What panes-mode draws: a window's layout and panes onto a screen.
data PanesPicture = PanesPicture
    { root :: Layout
        -- ^ the window's layout, fitted to its area
    , size :: Size  -- ^ the screen drawn on
    , status :: PaneStatus  -- ^ the window's @pane-border-status@
    , panes :: [PanesPane]  -- ^ in pane-index order
    , floatOrder :: [PaneId]  -- ^ the floating panes, back to front
    , border :: Cell.Style  -- ^ @display-panes-border-style@
    }

-- | One pane in the picture.
data PanesPane = PanesPane
    { pane :: PaneId
    , number :: Int  -- ^ its pane index
    , cells :: V.Vector (V.Vector Cell.Cell)  -- ^ its screen
    , cursor :: Maybe Pos  -- ^ 'Nothing' when hidden
    , preZoom :: Maybe PanesPreview
        -- ^ for the mode's own pane, its screen before the mode zoomed it
    , colour :: Cell.Style
        -- ^ @display-panes-colour@, or @-active-colour@ for the active pane
    , label :: Maybe (Int -> [(Int, Cell.Cell)])
        -- ^ its @display-panes-format@ drawn into a width, by column;
        --   'Nothing' when the option is empty
    }

-- | A drawing in progress, cell by cell.
type Canvas = Map (Int, Int) Cell.Cell

-- | tmux's @window_panes_draw_screen@: the tiled panes, the borders
-- between them, then the floating panes back to front, each cleared and
-- framed. Also where each pane landed, in drawing order.
panesPicture :: PanesPicture -> (V.Vector (V.Vector Cell.Cell), [(PaneId, Rect)])
panesPicture pic = (render final, reverse areasRev)
  where
    dsx = fromIntegral pic.size.cols :: Int
    dsy = fromIntegral pic.size.rows :: Int
    osx = pic.root.sx
    osy = pic.root.sy
    floating p = maybe False isFloatCell (paneCell p.pane pic.root)
    tiled = filter (not . floating) pic.panes
    byId = Map.fromList [ (p.pane, p) | p <- pic.panes ]
    floats = mapMaybe (`Map.lookup` byId) pic.floatOrder

    (afterTiled, areasTiled) = List.foldl' drawPane (Map.empty, []) tiled
    withBorders = paintBorders pic.border (tiledBorders pic dsx dsy) afterTiled
    (final, areasRev) = List.foldl' drawFloat (withBorders, areasTiled) floats

    drawFloat (cv, as) p = case paneCell p.pane pic.root of
        Nothing -> (cv, as)
        Just lc ->
            let frame = floatingFrame osx osy dsx dsy lc
                cleared = maybe cv (clearArea cv) frame
                (drawnOn, as') = drawPane (cleared, as) p
                framed = maybe drawnOn (\f -> paintBorders pic.border (frameMap dsx dsy f) drawnOn) frame
            in (framed, as')

    drawPane (cv, as) p = case paneCell p.pane pic.root of
        Nothing -> (cv, as)
        Just lc -> case geometry lc >>= clipFloating lc of
            Nothing -> (cv, as)
            Just (x, y, sx, sy) ->
                let src = case p.preZoom of
                        Just pv | sx <= gridWidth pv.cells, sy <= V.length pv.cells -> (pv.cells, pv.cursor)
                        _ -> (p.cells, p.cursor)
                    content
                        | osx <= dsx && osy <= dsy =
                            [ [ cellIn (fst src) cx cy | cx <- [0 .. sx - 1] ] | cy <- [0 .. sy - 1] ]
                        | otherwise = uncurry previewCells src sx sy
                    withContent = List.foldl' (\c (r, row) ->
                        List.foldl' (\c' (col, cell) -> put (x + col) (y + r) cell c') c (zip [0 ..] row))
                        cv (zip [0 ..] content)
                    stamped = List.foldl' (\c (q, cell) -> put (x + q.col) (y + q.row) cell c)
                        withContent (panesNumber p.colour p.number p.label sx sy)
                in (stamped, (p.pane, Rect y (y + sy) x (x + sx)) : as)

    -- window_panes_get_geometry
    geometry lc
        | osx == 0 || osy == 0 || dsx == 0 || dsy == 0 = Nothing
        | lc.xoff < 0 || lc.yoff < 0 = Nothing
        | x >= dsx || y >= dsy = Nothing
        | sx == 0 || sy == 0 = Nothing
        | horizontalBorder pic.status pic.root lc && sy > 1 =
            Just (x, if pic.status == PaneStatusTop then y + 1 else y, sx, sy - 1)
        | otherwise = Just (x, y, sx, sy)
      where
        unscaled = osx <= dsx && osy <= dsy
        sc v d o = if unscaled then v else v * d `div` o
        x = sc lc.xoff dsx osx
        y = sc lc.yoff dsy osy
        x2 = min dsx (max (x + 1) (sc (lc.xoff + lc.sx) dsx osx))
        y2 = min dsy (max (y + 1) (sc (lc.yoff + lc.sy) dsy osy))
        sx = x2 - x
        sy = y2 - y

    -- window_panes_clip_floating_pane
    clipFloating lc (x, y, sx, sy) = case floatingFrame osx osy dsx dsy lc of
        Nothing -> Just (x, y, sx, sy)
        Just (fx, fy, fx2, fy2) ->
            let bx = if fx >= 0 && x <= fx then fx + 1 else x
                by = if fy >= 0 && y <= fy then fy + 1 else y
                bx2 = if fx2 < dsx && x + sx - 1 >= fx2 then fx2 - 1 else x + sx - 1
                by2 = if fy2 < dsy && y + sy - 1 >= fy2 then fy2 - 1 else y + sy - 1
            in if bx2 < bx || by2 < by then Nothing
               else Just (bx, by, bx2 - bx + 1, by2 - by + 1)

    -- window_panes_clear_floating_area
    clearArea cv (fx, fy, fx2, fy2) =
        List.foldl' (\c (cx, cy) -> put cx cy Cell.blankCell c) cv
            [ (cx, cy) | cy <- [max 0 fy .. min (dsy - 1) fy2], cx <- [max 0 fx .. min (dsx - 1) fx2] ]

    put cx cy cell cv
        | cx >= 0 && cx < dsx && cy >= 0 && cy < dsy = Map.insert (cx, cy) cell cv
        | otherwise = cv

    render cv = V.generate dsy $ \cy -> V.generate dsx $ \cx ->
        Map.findWithDefault Cell.blankCell (cx, cy) cv

    cellIn g cx cy = fromMaybe Cell.blankCell (g V.!? cy >>= (V.!? cx))
    gridWidth g = maybe 0 V.length (g V.!? 0)

isFloatCell :: Layout -> Bool
isFloatCell l = case l.cell of
    FloatCell _ -> True
    _ -> False

-- | Whether a tiled cell gives a row to the window's pane status line
-- (tmux's @layout_add_horizontal_border@).
horizontalBorder :: PaneStatus -> Layout -> Layout -> Bool
horizontalBorder status root lc
    | isFloatCell lc = False
    | otherwise = case status of
        PaneStatusTop -> lc.yoff == root.yoff
        PaneStatusBottom -> lc.yoff + lc.sy == root.yoff + root.sy
        PaneStatusOff -> False

-- | Scale a window coordinate onto the screen when the window is the
-- bigger (tmux's @window_panes_map_x@ and @_map_y@).
mapTo :: Int -> Int -> Int -> Int
mapTo o d v = if o <= d then v else v * d `div` o

-- | A floating pane's frame on the screen: its left, top, right and
-- bottom border lines, @-1@ for a side at the window's edge (tmux's
-- @window_panes_get_floating_borders@); 'Nothing' for a tiled pane.
floatingFrame :: Int -> Int -> Int -> Int -> Layout -> Maybe (Int, Int, Int, Int)
floatingFrame osx osy dsx dsy lc
    | not (isFloatCell lc) = Nothing
    | otherwise = Just
        ( if lc.xoff == 0 then -1 else mapTo osx dsx (lc.xoff - 1)
        , if lc.yoff == 0 then -1 else mapTo osy dsy (lc.yoff - 1)
        , mapTo osx dsx (lc.xoff + lc.sx)
        , mapTo osy dsy (lc.yoff + lc.sy) )

-- | Border arms, by screen cell (tmux's @WINDOW_PANES_BORDER_*@ bits).
type BorderMap = Map (Int, Int) Int

armL, armR, armU, armD :: Int
armL = 1
armR = 2
armU = 4
armD = 8

markArm :: Int -> Int -> Int -> Int -> Int -> BorderMap -> BorderMap
markArm dsx dsy x y mask m
    | x >= 0 && x < dsx && y >= 0 && y < dsy = Map.insertWith (.|.) (x, y) mask m
    | otherwise = m

hasBit :: Int -> Int -> Bool
hasBit v arm = v .&. arm /= 0

-- | tmux's @window_panes_mark_vline@.
markVLine :: Int -> Int -> Int -> Int -> Int -> BorderMap -> BorderMap
markVLine dsx dsy x y0 y20 m
    | x < 0 || x >= dsx || y20 <= y0 = m
    | otherwise = List.foldl' step m [y .. y2 - 1]
  where
    y = max 0 y0
    y2 = min dsy y20
    step acc yy =
        let mask = (if yy > y then armU else 0) + (if yy + 1 < y2 then armD else 0)
        in markArm dsx dsy x yy (if mask == 0 then armU + armD else mask) acc

-- | tmux's @window_panes_mark_hline@.
markHLine :: Int -> Int -> Int -> Int -> Int -> BorderMap -> BorderMap
markHLine dsx dsy x0 x20 y m
    | y < 0 || y >= dsy || x20 <= x0 = m
    | otherwise = List.foldl' step m [x .. x2 - 1]
  where
    x = max 0 x0
    x2 = min dsx x20
    step acc xx =
        let mask = (if xx > x then armL else 0) + (if xx + 1 < x2 then armR else 0)
        in markArm dsx dsy xx y (if mask == 0 then armL + armR else mask) acc

-- | The borders between a window's tiled panes, its pane status lines
-- among them, joined where they meet (tmux's
-- @window_panes_mark_borders_cell@, @_mark_pane_status_borders@ and
-- @_mark_border_joins_cell@).
tiledBorders :: PanesPicture -> Int -> Int -> BorderMap
tiledBorders pic dsx dsy = joins pic.root (statusLines (dividers pic.root Map.empty))
  where
    osx = pic.root.sx
    osy = pic.root.sy
    mx = mapTo osx dsx
    my = mapTo osy dsy
    -- Each tiled child and the tiled sibling after it.
    pairs lc = case lc.cell of
        NodeCell o cs ->
            let tiledCs = filter (not . isFloatCell) cs
            in (o, cs, zip tiledCs (drop 1 tiledCs))
        _ -> (LeftRight, [], [])
    dividers lc m0 =
        let (o, cs, ps) = pairs lc
            m1 = List.foldl' (flip dividers) m0 cs
        in List.foldl' (\m (child, _) -> case o of
                LeftRight -> markVLine dsx dsy (mx (child.xoff + child.sx))
                    (my lc.yoff) (my (lc.yoff + lc.sy)) m
                TopBottom -> markHLine dsx dsy (mx lc.xoff) (mx (lc.xoff + lc.sx))
                    (my (child.yoff + child.sy)) m)
            m1 ps
    statusLines m = List.foldl' statusLine m
        [ lc | p <- pic.panes, Just lc <- [paneCell p.pane pic.root]
             , horizontalBorder pic.status pic.root lc ]
    statusLine m lc =
        let y = if pic.status == PaneStatusTop then my lc.yoff else my (lc.yoff + lc.sy) - 1
        in markHLine dsx dsy (mx lc.xoff) (mx (lc.xoff + lc.sx)) y m
    at m x y = Map.findWithDefault 0 (x, y) m
    horizontal v = hasBit v armL || hasBit v armR
    vertical v = hasBit v armU || hasBit v armD
    joins lc m0 =
        let (o, cs, ps) = pairs lc
            m1 = List.foldl' (flip joins) m0 cs
        in List.foldl' (\m (child, _) -> case o of
                LeftRight ->
                    let x = mx (child.xoff + child.sx)
                        y = my lc.yoff
                        y2 = my (lc.yoff + lc.sy)
                        above n = if y > 0 && horizontal (at n x (y - 1))
                            then markArm dsx dsy x y armU (markArm dsx dsy x (y - 1) armD n) else n
                        below n = if y2 < dsy && horizontal (at n x y2)
                            then markArm dsx dsy x (y2 - 1) armD (markArm dsx dsy x y2 armU n) else n
                    in if x < 0 || x >= dsx then m else below (above m)
                TopBottom ->
                    let x = mx lc.xoff
                        x2 = mx (lc.xoff + lc.sx)
                        y = my (child.yoff + child.sy)
                        leftOf n = if x > 0 && vertical (at n (x - 1) y)
                            then markArm dsx dsy x y armL (markArm dsx dsy (x - 1) y armR n) else n
                        rightOf n = if x2 < dsx && vertical (at n x2 y)
                            then markArm dsx dsy (x2 - 1) y armR (markArm dsx dsy x2 y armL n) else n
                    in if y < 0 || y >= dsy then m else rightOf (leftOf m))
            m1 ps

-- | A floating pane's frame as border arms (tmux's
-- @window_panes_draw_floating_border@).
frameMap :: Int -> Int -> (Int, Int, Int, Int) -> BorderMap
frameMap dsx dsy (x, y, x2, y2) =
    markVLine dsx dsy x2 y (y2 + 1) $ markVLine dsx dsy x y (y2 + 1) $
        markHLine dsx dsy x (x2 + 1) y2 $ markHLine dsx dsy x (x2 + 1) y Map.empty

-- | Draw border arms as single lines in a style.
paintBorders :: Cell.Style -> BorderMap -> Canvas -> Canvas
paintBorders style m cv = Map.foldlWithKey' paint cv m
  where
    paint acc pos mask
        | mask == 0 = acc
        | otherwise = Map.insert pos
            (Cell.glyphCell (borderGlyph BorderSingle Nothing (cellTypeOf mask)) style) acc

-- | The cells one pane's number paints, relative to its area's top-left
-- (tmux's @window_panes_draw_number@): 5x5 block digits centered, with the
-- letter that also chooses it (10 to 34) under their right end; or, when
-- the area cannot fit them, the number (and letter) as one centered line;
-- and the drawn @display-panes-format@ along the top row.
panesNumber
    :: Cell.Style -> Int -> Maybe (Int -> [(Int, Cell.Cell)]) -> Int -> Int
    -> [(Pos, Cell.Cell)]
panesNumber fgc n mlabel sx sy
    | sx < len = []
    | sx < bigWidth || sy < (if isJust mlabel then 7 else 5) =
        let width = if llen /= 0 && sx >= len + llen + 1 then len + llen + 1 else len
            cx = (sx - width) `div` 2
            txt = buf <> (if width > len then ' ' : letter else "")
        in textAt (sy `div` 2) cx txt <> (if sy > 1 then labelRow else [])
    | otherwise =
        blocks
            <> (if sy <= 6 then [] else labelRow
                    <> (if llen /= 0 then textAt (py + 5) (px0 + 6 * len - llen - 1) letter else []))
  where
    buf = show n
    len = length buf
    letter = [ toEnum (fromEnum 'a' + n - 10) | n > 9, n < 35 ]
    llen = length letter
    bigWidth = 6 * len - 1
    labelRow = [ (Pos 0 c, cell) | Just draw <- [mlabel], (c, cell) <- draw sx ]
    px0 = (sx - bigWidth) `div` 2
    py = (sy - 5) `div` 2
    blocks =
        [ (Pos (py + r) (px0 + 6 * d + c), block)
        | (d, ch) <- zip [0 ..] buf
        , (r, rowBits) <- zip [0 ..] (digitGlyph ch)
        , (c, True) <- zip [0 ..] rowBits ]
    textAt r c0 s = [ (Pos r (c0 + i), Cell.glyphCell ch fgc) | (i, ch) <- zip [0 ..] s ]
    block = Cell.blankCell { Cell.style = Cell.defaultStyle { Cell.bg = fgc.fg } }
