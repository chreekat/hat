-- | Keeping pane and session geometry in agreement with the layout: the
-- server-wide reconcile loop that is the sole writer of @pane.size@, the
-- per-session effective-size recompute clients drive, and the barrier a
-- command waits on before reporting that a resize has reached the child.
module Hat.Server.Resize
    ( resizeModeOf
    , applySessionSize
    , recalculateSizes
    , clientWindowArea
    , clientClaim
    , windowArea
    , defaultSizeOf
    , clampWindowSize
    , reconcileLoop
    , awaitReconcileTick
    , awaitReconciled
    , reconcilePaneSizes
    , paneSizeTargets
    , AreaClaim (..)
    , pickArea
    , rectSize
    , windowArrange
    , unzoomedArrange
    , zoomHidesChrome
    , windowPaneStatus
    , windowFloats
    , paneScrollbars
    , scrollbarReserve
    , windowLayoutAt
    ) where

import Control.Concurrent.STM
import Control.Exception (IOException, handle)
import Control.Monad (forM, forM_, when)
import Data.Word (Word16)
import Data.Map.Strict qualified as Map
import Data.List qualified as List
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR

import Hat.Geometry
import Hat.Log
import Hat.Model
import Hat.Model.Options
import Hat.Server.ControlTypes (ControlState (..))
import Hat.Server.Layout
import Hat.Server.Hooks (PayloadItem (..), notifyPane)
import Hat.Server.Floating (stacking)
import Hat.Server.Menu (refitMenu)
import Hat.Server.CopyMode (resizeCopyMode)
import Hat.Server.Scene
    ( FloatingPane (..), PaneStatus (..), ScrollbarSide (..), ScrollbarStyle (..), scrollbarStyle
    , statusAdjust )
import Hat.Server.Theme (terminalTheme)
import Hat.Term.Emulator qualified as Emu
import Hat.Term.Pty qualified

-- | How a window is sized: @window-size@.
resizeModeOf :: Options -> WindowSizeMode
resizeModeOf opts = case optionText "window-size" opts of
    "largest" -> SizeLargest
    "smallest" -> SizeSmallest
    "manual" -> SizeManual
    _ -> SizeLatest

-- | Re-derive every session's window area ('recalculateSizes') after
-- something about @sid@ changed, and mark its clients for a full redraw.
-- Pane pty and emulator sizes are not touched here: 'reconcileLoop' pulls
-- them into agreement with the layout off the same dirty tick this bumps, so
-- a session resized here — or a layout changed anywhere else — reconciles
-- uniformly.
applySessionSize :: ServerState -> SessionId -> IO ()
applySessionSize st sid = atomically $ do
    recalculateSizes st
    cs <- sessionClients st sid
    forM_ cs $ \c -> writeTVar c.needsFull True
    bumpDirty st

-- | tmux's @recalculate_sizes@: every session's area becomes its current
-- window's 'windowSize', kept when no client sizes that window. A client of
-- one session can size a window another holds, so no session is
-- recomputed alone. Clients of a session whose area moved get a full
-- redraw. See Note [Sizing per session].
recalculateSizes :: ServerState -> STM ()
recalculateSizes st = do
    sessions <- Map.elems <$> readTVar st.sessions
    forM_ sessions $ \sess -> do
        old <- readTVar sess.lastSize
        new <- currentWindow sess >>= \case
            Just win -> fromMaybe old <$> windowSize st win
            Nothing -> pure old
        when (new /= old) $ do
            writeTVar sess.lastSize new
            cs <- sessionClients st sess.id
            forM_ cs $ \c -> writeTVar c.needsFull True

-- | The window size @mode@ derives from the session's attached clients, or
-- @default-size@ when none is attached (tmux's @default_window_size@). Used
-- by @resize-window -A@\/@-a@, which pick a size from the clients without
-- leaving the window following them.
clientWindowArea :: ServerState -> Session -> WindowSizeMode -> STM Size
clientWindowArea st sess mode = do
    cs <- sessionClients st sess.id
    mwin <- currentWindow sess
    opts <- maybe (resolveForSession st sess) (resolveForWindow st sess) mwin
    claims <- catMaybes <$> mapM (clientClaim opts.statusLines Nothing) cs
    pure (clampWindowSize
        (sessionWindowArea mode (defaultSizeOf opts) claims))

-- Note [Sizing per session]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- tmux sizes each window from the clients that can see it ('windowSize'),
-- and a client shows its session's current window at that window's size.
-- Hat keeps one stored area per session (@lastSize@), which renderers read
-- as the current window's size, so it holds the current window's
-- 'windowSize' ('recalculateSizes') and must be re-derived whenever a
-- client, a current window, or a sizing option changes. A window no client
-- sizes has no size of its own to keep: it keeps the session's. That is
-- also where a @manual@ size lives, so it belongs to the session, not the
-- window, until windows carry their own size.

-- | The size tmux's @recalculate_size@ gives a window, from the clients of
-- the sessions holding it; 'Nothing' when no client sizes it, which keeps
-- the size it has.
windowSize :: ServerState -> Window -> STM (Maybe Size)
windowSize st win = do
    wopts <- resolveWindowTables st win
    sessions <- Map.elems <$> readTVar st.sessions
    holders <- fmap concat . forM sessions $ \sess -> do
        holds <- any (\w -> w.id == win.id) <$> readTVar sess.windows
        if not holds then pure [] else do
            viewing <- currentWindow sess >>= \case
                Just w | w.id == win.id -> pure Viewing
                _ -> pure Elsewhere
            sopts <- resolveForSession st sess
            cs <- sessionClients st sess.id
            fmap catMaybes . forM cs $ \c -> do
                mclaim <- clientClaim sopts.statusLines (Just win.id) c
                own <- controlWindowSize win.id c
                pure (fmap (\claim -> Sizer { claim, own, viewing }) mclaim)
    let by = if wopts.aggressiveResize then ViewingSessions else HoldingSessions
    pure (sizeWindow (resizeModeOf wopts) by holders)

-- | A client of a session holding a window, as it bears on the window's
-- size.
data Sizer = Sizer
    { claim   :: (Int, Size, Int)  -- ^ see 'clientClaim'
    , own     :: Maybe Size
        -- ^ a control client's own size for this window, which caps it
    , viewing :: Viewing
    }
    deriving (Eq, Show)

-- | Whether a window is its session's current window.
data Viewing = Viewing | Elsewhere
    deriving (Eq, Show)

-- | Which clients size a window (@aggressive-resize@): those of every
-- session holding it, or only of those whose current window it is.
data SizedBy = HoldingSessions | ViewingSessions
    deriving (Eq, Show)

-- | tmux's @clients_calculate_size@ for one window, given every client of a
-- session holding it. Under @latest@ with more than one such client, the
-- window follows its most recently active viewer: tmux's per-window
-- @latest@ client, read from activity stamps.
sizeWindow :: WindowSizeMode -> SizedBy -> [Sizer] -> Maybe Size
sizeWindow SizeManual _ _ = Nothing
sizeWindow mode by holders =
    clamp (foldl capBy (fmap (.cols) claimed, fmap (.rows) claimed) caps)
  where
    sizers = case by of
        HoldingSessions -> holders
        ViewingSessions -> filter ((== Viewing) . (.viewing)) holders
    counted = case mode of
        SizeLatest | length holders > 1 ->
            take 1 (List.sortOn (Down . stamp)
                [ s | s <- sizers, s.viewing == Viewing ])
        _ -> sizers
    claimed = case map (.claim) counted of
        [] -> Nothing
        claims@((_, sz, _) : _) -> Just (sessionWindowArea mode sz claims)
    caps = catMaybes (map (.own) sizers)
    capBy (mx, my) o = (cap mx o.cols, cap my o.rows)
    cap m 0 = m
    cap m v = Just (maybe v (min v) m)
    clamp (mx, my) = Size <$> my <*> mx
    stamp s = let (t, _, _) = s.claim in t

-- | A control client's own size for a window (@refresh-client -C @id:WxH@).
controlWindowSize :: WindowId -> Client -> STM (Maybe Size)
controlWindowSize wid c = case c.control of
    Nothing -> pure Nothing
    Just ctl -> (>>= Map.lookup wid) <$> readTVar ctl.windowSizes

-- | What a client offers toward a session's window size: its activity
-- stamp, its size, and the status rows it draws (@statusLines@). A control
-- client draws no status line and offers nothing until @refresh-client -C@
-- sizes it (or one of its windows); its size is 80x24 until then, and its
-- size for the given window, if it set one, replaces it.
clientClaim :: Int -> Maybe WindowId -> Client -> STM (Maybe (Int, Size, Int))
clientClaim statusLines mwid c = do
    stamp <- readTVar c.lastActive
    tty <- readTVar c.ttyState
    case c.control of
        -- A locked client sizes nothing (tmux's @CLIENT_NOSIZEFLAGS@).
        Nothing | tty == TtyStopped -> pure Nothing
        Nothing -> do
            sz <- readTVar c.size
            pure (Just (stamp, sz, statusLines))
        Just cs -> do
            msz <- readTVar cs.size
            mwins <- readTVar cs.windowSizes
            let own = mwid >>= \wid -> mwins >>= Map.lookup wid
            pure $ case (msz, mwins) of
                (Nothing, Nothing) -> Nothing
                _ -> Just (stamp, fromMaybe (fromMaybe defaultControl msz) own, 0)
  where
    defaultControl = Size { rows = 24, cols = 80 }

-- | @default-size@ read as tmux's @sscanf("%ux%u")@ does: leading digits, an
-- @x@, digits, anything after ignored; anything else is 80x24.
defaultSizeOf :: Options -> Size
defaultSizeOf opts = fromMaybe Size { rows = 24, cols = 80 } $ do
    (w, afterW) <- either (const Nothing) Just (TR.decimal (optionText "default-size" opts))
    rest <- T.stripPrefix "x" afterW
    (h, _) <- either (const Nothing) Just (TR.decimal rest)
    pure Size { rows = dim h, cols = dim w }
  where
    dim :: Integer -> Word16
    dim = fromInteger . max 1 . min 10000

-- | tmux's @WINDOW_MINIMUM@..@WINDOW_MAXIMUM@ bounds on either dimension.
clampWindowSize :: Size -> Size
clampWindowSize s = Size { rows = clampDim s.rows, cols = clampDim s.cols }
  where
    clampDim = max 1 . min 10000

-- | A single server-wide task that pulls every pane's pty and emulator size
-- into agreement with the current layout whenever the screen is marked
-- dirty. Because the same 'bumpDirty' that schedules a repaint also wakes
-- this loop, no state change can update the picture without resizing the
-- panes behind it — a zoom cancelled by 'cmdSelectPane', a split, or a
-- client resize all reconcile here. Sole writer of 'pane.size' and sole
-- caller of the resize primitives, so no per-client 'renderLoop' races it.
reconcileLoop :: ServerState -> IO ()
reconcileLoop st = loop (-1)
  where
    loop lastGen = do
        gen <- atomically (awaitReconcileTick st lastGen)
        reconcilePaneSizes st
        atomically (writeTVar st.reconciled gen)
        loop gen

-- | The next dirty generation 'reconcileLoop' sizes panes for: a newer one,
-- but only once no user-command batch is open (@commandDepth@ back to zero), so
-- a multi-command binding settles into a single layout before any pane is
-- resized to a width in the middle of it. Blocks until such a generation
-- exists.
awaitReconcileTick :: ServerState -> Int -> STM Int
awaitReconcileTick st lastGen = do
    g <- readTVar st.dirty
    depth <- readTVar st.commandDepth
    check (g /= lastGen && depth == 0)
    pure g

-- | Block until 'reconcileLoop' has resized panes through the current dirty
-- generation, so the caller observes a screen whose children have already
-- been told their size. 'controlLoop' waits here before reporting a command
-- done, giving @select-pane@ and friends a happens-before with the pty
-- resize (@TIOCSWINSZ@ \/ @SIGWINCH@): when the command returns, no child is
-- still holding a stale size. A no-op command reconciles to nothing and
-- returns at once. Depends on a live 'reconcileLoop' to advance
-- 'Hat.Model.reconciled'.
awaitReconciled :: ServerState -> IO ()
awaitReconciled st = do
    target <- readTVarIO st.dirty
    atomically $ do
        done <- readTVar st.reconciled
        check (done >= target)

-- | Resize the pty and emulator of every pane whose stored size lags the
-- layout, waking renderers once the child has been told (see 'reconcileLoop').
-- A pane whose pty has already closed (its child exiting mid-tick) fails only
-- its own resize — logged, not raised — so one dead pane neither aborts the
-- rest nor kills the loop. 'pane.size' is committed only after the child is
-- actually resized, so a failure leaves the pane to retry rather than claiming
-- a size its child never heard about.
reconcilePaneSizes :: ServerState -> IO ()
reconcilePaneSizes st = do
    targets <- atomically (paneSizeTargets st)
    forM_ targets $ \(pane, sz) -> do
        old <- readTVarIO pane.size
        when (old /= sz) $
            handle (\(e :: IOException) ->
                        logEvent st.logger PaneResizeFailed
                            { pane = rawPane pane.id, err = T.pack (show e) }) $ do
                -- Flushed, not just queued: the emulator's resize can abort() the
                -- whole process (a native assertion), and this line is what
                -- names the culprit pane and dimensions afterwards.
                logEvent st.logger PaneResizing
                    { pane = rawPane pane.id
                    , toRows = fromIntegral sz.rows
                    , toCols = fromIntegral sz.cols }
                flushLogger st.logger
                -- Resize the emulator model BEFORE the pty: setting the pty
                -- winsize SIGWINCHes the pane's program, which redraws once for
                -- the new size. If the emulator were still at the old size when
                -- that redraw arrived, it would mis-wrap the one-shot repaint
                -- into persistent garbage (a zoomed pager). Model first, then
                -- signal the child.
                Emu.resize pane.emulator sz
                Hat.Term.Pty.resize pane.pty sz
                atomically $ do
                    writeTVar pane.size sz
                    modifyTVar' pane.modes $ map $ \e -> case e.mode of
                        CopyMode d -> e { mode = CopyMode (resizeCopyMode sz d) }
                        _ -> e
                    bumpDirty st
                notifyPane st "pane-resized" pane
                    [ ("old_width", PInt (fromIntegral old.cols))
                    , ("old_height", PInt (fromIntegral old.rows))
                    , ("width", PInt (fromIntegral sz.cols))
                    , ("height", PInt (fromIntegral sz.rows)) ]

-- | The size the current layout assigns to every live pane across all
-- sessions, each window laid out once at its 'windowAreas' size. See
-- 'reconcilePaneSizes'.
paneSizeTargets :: ServerState -> STM [(Pane, Size)]
paneSizeTargets st = do
    areas <- windowAreas st
    fmap concat $ forM areas $ \(win, eff) -> do
        _ <- windowLayoutAt eff win
        refitWindowMenu eff win
        (rects, _) <- windowArrange st eff win
        ps <- readTVar win.panes
        pure [ (p, rectSize rect)
             | (pidL, rect) <- rects
             , Just p <- [Map.lookup pidL ps]
             ]

-- | A window's menu kept inside it as it is resized (tmux's
-- @menu_resize@ from @window_resize@).
refitWindowMenu :: Size -> Window -> STM ()
refitWindowMenu eff win = do
    mmenu <- readTVar win.menu
    forM_ mmenu $ \wm -> do
        let wm' = refitMenu (fromIntegral eff.cols) (fromIntegral eff.rows) wm
        when (wm' /= wm) $ writeTVar win.menu (Just wm')

-- | Every window once, with the window area its panes are laid out at.
windowAreas :: ServerState -> STM [(Window, Size)]
windowAreas st = do
    sessions <- Map.elems <$> readTVar st.sessions
    wins <- Map.elems . Map.fromList . concatMap (map (\w -> (w.id, w)))
        <$> mapM (fmap Map.elems . readTVar . (.windows)) sessions
    forM wins $ \win -> (,) win <$> windowArea st win

-- | The area a window's panes are laid out at: its 'windowSize', read from
-- the session holding it alone that shows it, which already holds that;
-- without one, the area of the holder 'pickArea' prefers.
windowArea :: ServerState -> Window -> STM Size
windowArea st win = do
    sessions <- Map.elems <$> readTVar st.sessions
    holdings <- fmap concat . forM sessions $ \sess -> do
        ws <- Map.toList <$> readTVar sess.windows
        cur <- readTVar sess.currentIx
        cs <- sessionClients st sess.id
        stamps <- mapM (\c -> readTVar c.lastActive) cs
        let latest = if null stamps then Nothing else Just (maximum stamps)
        pure [ (AreaClaim (ix == cur) latest, sess)
             | (ix, w) <- ws, w.id == win.id ]
    case holdings of
        [(AreaClaim True _, sess)] -> readTVar sess.lastSize
        _ -> windowSize st win >>= \case
            Just sz -> pure sz
            Nothing -> case pickHolder holdings of
                Nothing -> pure Size { rows = 24, cols = 80 }
                Just sess -> readTVar sess.lastSize

-- | How strongly a session holding a window claims to size it: whether
-- the window is its current one, and when its clients were last active.
data AreaClaim = AreaClaim
    { viewing :: Bool
    , active  :: Maybe Int
    }
    deriving (Eq, Show)

-- | The area a window held by several sessions (linked into them, or
-- shared by a session group) is laid out at, so they never pull its panes
-- to different sizes: the area of the session whose client most recently
-- viewed it, else whose client was most recently active, else the first
-- holder's.
pickArea :: [(AreaClaim, Size)] -> Size
pickArea = fromMaybe Size { rows = 24, cols = 80 } . pickHolder

-- | The strongest claimant, by 'pickArea''s order.
pickHolder :: [(AreaClaim, a)] -> Maybe a
pickHolder = \case
    [] -> Nothing
    first : rest -> Just (snd (foldl stronger first rest))
  where
    stronger a b = if rank (fst b) > rank (fst a) then b else a
    rank c = (c.viewing && isJust c.active, c.active)

rectSize :: Rect -> Size
rectSize r = Size
    { rows = fromIntegral (max 1 (r.endRow - r.startRow))
    , cols = fromIntegral (max 1 (r.endCol - r.startCol))
    }

-- | A window's layout resized to the area it is laid out at, kept as its
-- layout from now on: tmux resizes a window's layout in place, so a later
-- split or resize builds on the cells the panes really have.
windowLayoutAt :: Size -> Window -> STM Layout
windowLayoutAt eff win = do
    lay <- readTVar win.layout
    let lay' = fitLayout eff lay
    if lay' == lay then pure lay else lay' <$ writeTVar win.layout lay'

-- | Pane rects and borders for a window, honoring zoom and leaving room
-- for the window's pane status line: the tiled panes, then the floating
-- ones back to front. Arranging is pure in the window rect and the layout
-- tree, so the last result is kept until either input changes — renderers
-- hit this every frame.
windowArrange
    :: ServerState -> Size -> Window -> STM ([(PaneId, Rect)], [(Pos, Char)])
windowArrange st eff win = do
    mz <- readTVar win.zoomed
    ps <- readTVar win.panes
    case mz of
        Just zpid | Map.member zpid ps -> do
            hidden <- zoomHidesChrome win
            (side, bars) <- paneScrollbars st win
            status <- windowPaneStatus st win
            pure ( [ (zpid, if hidden then sizeRect eff else
                        scrollbarReserve side (Map.findWithDefault 0 zpid bars)
                            (statusAdjust status (fromIntegral eff.rows) (sizeRect eff))) ]
                 , [] )
        _ -> unzoomedArrange' st eff win

-- | Whether the window's zoomed pane shows a mode that leaves out its
-- pane status line and scrollbar ('modeHidesChrome').
zoomHidesChrome :: Window -> STM Bool
zoomHidesChrome win = readTVar win.zoomed >>= \case
    Nothing -> pure False
    Just zpid -> do
        ps <- readTVar win.panes
        case Map.lookup zpid ps of
            Nothing -> pure False
            Just p -> maybe False modeHidesChrome <$> topMode p

-- | Pane rects as 'windowArrange' would give them were the window not
-- zoomed.
unzoomedArrange :: ServerState -> Size -> Window -> STM [(PaneId, Rect)]
unzoomedArrange st eff win = fst <$> unzoomedArrange' st eff win

unzoomedArrange'
    :: ServerState -> Size -> Window -> STM ([(PaneId, Rect)], [(Pos, Char)])
unzoomedArrange' st eff win = do
    lay <- readTVar win.layout
    status <- windowPaneStatus st win
    (side, bars) <- paneScrollbars st win
    (rects, borders) <- cachedArrange eff win lay
    floats <- floatingPanes st win
    pure ( [ (p, scrollbarReserve side (Map.findWithDefault 0 p bars)
                    (statusAdjust status (fromIntegral eff.rows) r))
           | (p, r) <- rects ]
           <> [ (f.pane, f.rect) | f <- floats ]
         , borders )

-- | A window's floating panes as its scene draws them, back to front; none
-- while it is zoomed, as tmux's zoom leaves no pane floating.
windowFloats :: ServerState -> Window -> STM [FloatingPane]
windowFloats st win = do
    mz <- readTVar win.zoomed
    case mz of
        Just _ -> pure []
        Nothing -> floatingPanes st win

floatingPanes :: ServerState -> Window -> STM [FloatingPane]
floatingPanes st win = do
    lay <- readTVar win.layout
    fs <- readTVar win.floats
    ps <- readTVar win.panes
    (side, bars) <- paneScrollbars st win
    fmap concat . forM (reverse (stacking fs lay)) $ \pid ->
        case (floatGeom pid lay, Map.lookup pid ps) of
            (Just g, Just p) -> do
                popts <- resolvePaneTables st win p
                let lineSet = popts.paneBorderLines
                    raw = popts.paneBorderStatus
                    box = Rect { startRow = g.top, endRow = g.top + g.height
                               , startCol = g.left, endCol = g.left + g.width }
                pure [ FloatingPane
                    { pane = pid
                    , rect = scrollbarReserve side (Map.findWithDefault 0 pid bars) box
                    , lineSet
                    , status = floatingStatus lineSet raw } ]
            _ -> pure []

-- | A floating pane's @pane-border-status@ (tmux's
-- @window_pane_get_pane_status@): its own, the floating-only positions
-- included; none without a border to carry it.
floatingStatus :: BorderLines -> Text -> PaneStatus
floatingStatus BorderNone _ = PaneStatusOff
floatingStatus _ v = case v of
    "top" -> PaneStatusTop
    "top-floating" -> PaneStatusTop
    "bottom" -> PaneStatusBottom
    "bottom-floating" -> PaneStatusBottom
    _ -> PaneStatusOff

-- | 'arrange' through the window's cache of its last result.
cachedArrange :: Size -> Window -> Layout -> STM ([(PaneId, Rect)], [(Pos, Char)])
cachedArrange eff win lay = do
    let rect = sizeRect eff
    hit <- readTVar win.arrangeCache
    case hit of
        Just (k, res) | k == (rect, lay) -> pure res
        _ -> do
            let res = arrange rect lay
            writeTVar win.arrangeCache (Just ((rect, lay), res))
            pure res

-- | A window's @pane-border-status@.
windowPaneStatus :: ServerState -> Window -> STM PaneStatus
windowPaneStatus st win = do
    wopts <- resolveWindowTables st win
    pure $ case wopts.paneBorderStatus of
        "top" -> PaneStatusTop
        "bottom" -> PaneStatusBottom
        _ -> PaneStatusOff

-- | Which side a window's scrollbars take, and the width (padding
-- included) of each pane's scrollbar beside it (tmux's
-- @window_pane_scrollbar_reserve@): @pane-scrollbars on@ shows one for
-- every pane not on its alternate screen.
paneScrollbars :: ServerState -> Window -> STM (ScrollbarSide, Map.Map PaneId Int)
paneScrollbars st win = do
    wopts <- resolveWindowTables st win
    let side = if wopts.paneScrollbarsPosition == "left"
            then ScrollbarLeft else ScrollbarRight
    if wopts.paneScrollbars /= "on" then pure (side, Map.empty) else do
        ps <- Map.elems <$> readTVar win.panes
        bars <- fmap concat $ forM ps $ \p -> do
            alt <- readTVar p.onAlternate
            popts <- resolvePaneTables st win p
            let sb = scrollbarStyle terminalTheme popts.paneScrollbarsStyle
            pure [ (p.id, sb.width + sb.pad) | not alt ]
        pure (side, Map.fromList bars)

-- | A pane's content once its scrollbar takes its columns (tmux's
-- @layout_fix_panes@), leaving at least one.
scrollbarReserve :: ScrollbarSide -> Int -> Rect -> Rect
scrollbarReserve _ 0 r = r
scrollbarReserve side w r
    | sx - w < 1 = case side of
        ScrollbarLeft -> r { startCol = r.endCol - 1 }
        ScrollbarRight -> r { endCol = r.startCol + 1 }
    | otherwise = case side of
        ScrollbarLeft -> r { startCol = r.startCol + w }
        ScrollbarRight -> r { endCol = r.endCol - w }
  where
    sx = r.endCol - r.startCol
