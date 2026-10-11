-- | Keeping pane and session geometry in agreement with the layout: the
-- server-wide reconcile loop that is the sole writer of @pane.size@, the
-- per-session effective-size recompute clients drive, and the barrier a
-- command waits on before reporting that a resize has reached the child.
module Hat.Server.Resize
    ( resizeModeOf
    , applySessionSize
    , clientWindowArea
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
    ) where

import Control.Concurrent.STM
import Control.Exception (IOException, handle)
import Control.Monad (forM, forM_, when)
import Data.Word (Word16)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Data.Text.Read qualified as TR

import Hat.Geometry
import Hat.Log
import Hat.Model
import Hat.Model.Options
import Hat.Server.Layout
import Hat.Server.Hooks (PayloadItem (..), notifyPane)
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
        stamps <- mapM (\c -> (,) <$> readTVar c.lastActive
                                   <*> readTVar c.size) cs
        lastSz <- readTVar sess.lastSize
        mwin <- currentWindow sess
        opts <- maybe (resolveGlobal st) (resolveForWindow st sess) mwin
        let eff = sessionWindowArea opts.statusLines (resizeModeOf opts) lastSz stamps
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
    stamps <- mapM (\c -> (,) <$> readTVar c.lastActive <*> readTVar c.size) cs
    mwin <- currentWindow sess
    opts <- maybe (resolveForSession st sess) (resolveForWindow st sess) mwin
    pure (clampWindowSize
        (sessionWindowArea opts.statusLines mode (defaultSizeOf opts) stamps))

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
        (rects, _) <- windowArrange eff win
        ps <- readTVar win.panes
        pure [ (p, rectSize rect)
             | (pidL, rect) <- rects
             , Just p <- [Map.lookup pidL ps]
             ]

-- | Every window once, with the window area its panes are laid out at:
-- that of the session holding it that 'pickArea' prefers.
windowAreas :: ServerState -> STM [(Window, Size)]
windowAreas st = do
    sessions <- Map.elems <$> readTVar st.sessions
    holdings <- forM sessions $ \sess -> do
        eff <- readTVar sess.lastSize
        cur <- readTVar sess.currentIx
        ws <- Map.toList <$> readTVar sess.windows
        cs <- sessionClients st sess.id
        stamps <- mapM (\c -> readTVar c.lastActive) cs
        let latest = if null stamps then Nothing else Just (maximum stamps)
        pure [ (win, AreaClaim (ix == cur) latest, eff) | (ix, win) <- ws ]
    let claims = Map.fromListWith (flip (<>))
            [ (win.id, [(claim, eff)]) | (win, claim, eff) <- concat holdings ]
        wins = Map.fromList [ (win.id, win) | (win, _, _) <- concat holdings ]
    pure [ (win, pickArea cs)
         | (wid, cs) <- Map.toList claims, Just win <- [Map.lookup wid wins] ]

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
pickArea = \case
    [] -> Size { rows = 24, cols = 80 }
    first : rest -> snd (foldl stronger first rest)
  where
    stronger a b = if rank (fst b) > rank (fst a) then b else a
    rank c = (c.viewing && isJust c.active, c.active)

rectSize :: Rect -> Size
rectSize r = Size
    { rows = fromIntegral (max 1 (r.endRow - r.startRow))
    , cols = fromIntegral (max 1 (r.endCol - r.startCol))
    }

-- | Pane rects and borders for a window, honoring zoom. Arranging is
-- pure in the window rect and the layout tree, so the last result is
-- kept until either input changes — renderers hit this every frame.
windowArrange :: Size -> Window -> STM ([(PaneId, Rect)], [(Pos, Char)])
windowArrange eff win = do
    mz <- readTVar win.zoomed
    lay <- readTVar win.layout
    ps <- readTVar win.panes
    case mz of
        Just zpid | Map.member zpid ps -> pure ([(zpid, sizeRect eff)], [])
        _ -> do
            let rect = sizeRect eff
            hit <- readTVar win.arrangeCache
            case hit of
                Just (k, res) | k == (rect, lay) -> pure res
                _ -> do
                    let res = arrange rect lay
                    writeTVar win.arrangeCache (Just ((rect, lay), res))
                    pure res
