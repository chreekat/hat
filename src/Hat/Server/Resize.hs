-- | Keeping pane and session geometry in agreement with the layout: the
-- server-wide reconcile loop that is the sole writer of @pane.size@, the
-- per-session effective-size recompute clients drive, and the barrier a
-- command waits on before reporting that a resize has reached the child.
module Hat.Server.Resize
    ( resizeModeOf
    , applySessionSize
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
    , windowPaneStatus
    , windowLayoutAt
    ) where

import Control.Concurrent.STM
import Control.Exception (IOException, handle)
import Control.Monad (forM, forM_, when)
import Data.Word (Word16)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text qualified as T
import Data.Text.Read qualified as TR

import Hat.Geometry
import Hat.Log
import Hat.Model
import Hat.Model.Options
import Hat.Server.ControlTypes (ControlState (..))
import Hat.Server.Layout
import Hat.Server.Hooks (PayloadItem (..), notifyPane)
import Hat.Server.Scene (PaneStatus (..), statusAdjust)
import Hat.Term.Emulator qualified as Emu
import Hat.Term.Pty qualified

-- | How a window is sized: @window-size@. tmux's @aggressive-resize@ only
-- narrows which clients count to those whose current window it is; hat sizes
-- all of a session's windows together from that session's own clients, so
-- the distinction has nothing to select here.
resizeModeOf :: Options -> WindowSizeMode
resizeModeOf opts = case optionText "window-size" opts of
    "largest" -> SizeLargest
    "smallest" -> SizeSmallest
    "manual" -> SizeManual
    _ -> SizeLatest

-- | Recompute a session's effective window size from its clients (honoring
-- @window-size@) and mark the clients for a full redraw. Pane pty and
-- emulator sizes are not touched here: 'reconcileLoop' pulls them into
-- agreement with the layout off the same dirty tick this bumps, so a session
-- resized here — or a layout changed anywhere else — reconciles uniformly.
applySessionSize :: ServerState -> SessionId -> IO ()
applySessionSize st sid = atomically $ do
    msess <- Map.lookup sid <$> readTVar st.sessions
    forM_ msess $ \sess -> do
        cs <- sessionClients st sid
        eff <- sessionArea st sess Nothing
        writeTVar sess.lastSize eff
        forM_ cs $ \c -> writeTVar c.needsFull True
        bumpDirty st

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

-- | The area a session's clients size its windows to (@window-size@), or
-- for one window, with control clients' sizes for that window standing in
-- for their own. Without clients it keeps its last size.
sessionArea :: ServerState -> Session -> Maybe WindowId -> STM Size
sessionArea st sess mwid = do
    cs <- sessionClients st sess.id
    lastSz <- readTVar sess.lastSize
    mwin <- currentWindow sess
    opts <- maybe (resolveGlobal st) (resolveForWindow st sess) mwin
    claims <- catMaybes <$> mapM (clientClaim opts.statusLines mwid) cs
    pure (sessionWindowArea (resizeModeOf opts) lastSz claims)

-- | What a client offers toward a session's window size: its activity
-- stamp, its size, and the status rows it draws (@statusLines@). A control
-- client draws no status line and offers nothing until @refresh-client -C@
-- sizes it (or one of its windows); its size is 80x24 until then, and its
-- size for the given window, if it set one, replaces it.
clientClaim :: Int -> Maybe WindowId -> Client -> STM (Maybe (Int, Size, Int))
clientClaim statusLines mwid c = do
    stamp <- readTVar c.lastActive
    case c.control of
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

-- | Every control client's own size for a window, among the clients of
-- the given sessions.
windowOverrides :: ServerState -> [Session] -> WindowId -> STM [Size]
windowOverrides st sessions wid = fmap concat . forM sessions $ \sess -> do
    cs <- sessionClients st sess.id
    fmap concat . forM cs $ \c -> case c.control of
        Nothing -> pure []
        Just ctl -> maybe [] (maybe [] pure . Map.lookup wid)
            <$> readTVar ctl.windowSizes

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
        (rects, _) <- windowArrange st eff win
        ps <- readTVar win.panes
        pure [ (p, rectSize rect)
             | (pidL, rect) <- rects
             , Just p <- [Map.lookup pidL ps]
             ]

-- | Every window once, with the window area its panes are laid out at.
windowAreas :: ServerState -> STM [(Window, Size)]
windowAreas st = do
    sessions <- Map.elems <$> readTVar st.sessions
    wins <- Map.elems . Map.fromList . concatMap (map (\w -> (w.id, w)))
        <$> mapM (fmap Map.elems . readTVar . (.windows)) sessions
    forM wins $ \win -> (,) win <$> windowArea st win

-- | The area a window's panes are laid out at: that of the session holding
-- it that 'pickArea' prefers, reworked for any control client's size for
-- this window, which it never exceeds (tmux's per-window client sizes).
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
    case pickHolder holdings of
        Nothing -> pure Size { rows = 24, cols = 80 }
        Just sess -> do
            overrides <- windowOverrides st (map snd holdings) win.id
            if null overrides
                then readTVar sess.lastSize
                else do
                    sz <- sessionArea st sess (Just win.id)
                    pure (foldl clampTo sz overrides)
  where
    clampTo sz o = Size
        { rows = if o.rows /= 0 then min sz.rows o.rows else sz.rows
        , cols = if o.cols /= 0 then min sz.cols o.cols else sz.cols }

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
-- for the window's pane status line. Arranging is pure in the window rect
-- and the layout tree, so the last result is kept until either input
-- changes — renderers hit this every frame.
windowArrange
    :: ServerState -> Size -> Window -> STM ([(PaneId, Rect)], [(Pos, Char)])
windowArrange st eff win = do
    mz <- readTVar win.zoomed
    lay <- readTVar win.layout
    ps <- readTVar win.panes
    status <- windowPaneStatus st win
    let adjust (rects, borders) =
            ([ (p, statusAdjust status (fromIntegral eff.rows) r) | (p, r) <- rects ], borders)
    case mz of
        Just zpid | Map.member zpid ps -> pure (adjust ([(zpid, sizeRect eff)], []))
        _ -> do
            let rect = sizeRect eff
            hit <- readTVar win.arrangeCache
            case hit of
                Just (k, res) | k == (rect, lay) -> pure (adjust res)
                _ -> do
                    let res = arrange rect lay
                    writeTVar win.arrangeCache (Just ((rect, lay), res))
                    pure (adjust res)

-- | A window's @pane-border-status@: its own value, else the global
-- window table's, else the default.
windowPaneStatus :: ServerState -> Window -> STM PaneStatus
windowPaneStatus st win = do
    own <- lookupDelta "pane-border-status" <$> readTVar win.options
    global <- lookupDelta "pane-border-status" <$> readTVar st.globalWindowOptions
    pure $ case maybe "off" valueText (maybe global Just own) of
        "top" -> PaneStatusTop
        "bottom" -> PaneStatusBottom
        _ -> PaneStatusOff
