-- | Keeping pane and session geometry in agreement with the layout: the
-- server-wide reconcile loop that is the sole writer of @pane.size@, the
-- per-session effective-size recompute clients drive, and the barrier a
-- command waits on before reporting that a resize has reached the child.
module Hat.Server.Resize
    ( resizeModeOf
    , applySessionSize
    , reconcileLoop
    , awaitReconciled
    , reconcilePaneSizes
    , paneSizeTargets
    , rectSize
    ) where

import Control.Concurrent.STM
import Control.Exception (IOException, handle)
import Control.Monad (forM, forM_, when)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Hat.Geometry
import Hat.Log
import Hat.Model
import Hat.Model.Options
import Hat.Server.Layout
import Hat.Server.View (windowArrange)
import qualified Hat.Term.Emulator as Emu
import qualified Hat.Term.Pty

-- | The resize mode @aggressive-resize@ selects: on, follow the active
-- client; off, fit the smallest. See 'resizeModeFor'.
resizeModeOf :: Options -> ResizeMode
resizeModeOf opts = if opts.aggressiveResize then ActiveClient else SmallestClient

-- | Recompute a session's effective window size from its clients (honoring
-- @aggressive-resize@) and mark the clients for a full redraw. Pane pty and
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
        gen <- atomically $ do
            g <- readTVar st.dirty
            check (g /= lastGen)
            pure g
        reconcilePaneSizes st
        atomically (writeTVar st.reconciled gen)
        loop gen

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

-- | The size the current layout assigns to every live pane across all
-- sessions. See 'reconcilePaneSizes'.
paneSizeTargets :: ServerState -> STM [(Pane, Size)]
paneSizeTargets st = do
    sessions <- Map.elems <$> readTVar st.sessions
    fmap concat $ forM sessions $ \sess -> do
        eff <- readTVar sess.lastSize
        ws <- Map.elems <$> readTVar sess.windows
        fmap concat $ forM ws $ \win -> do
            (rects, _) <- windowArrange (eff) win
            ps <- readTVar win.panes
            pure [ (p, rectSize rect)
                 | (pidL, rect) <- rects
                 , Just p <- [Map.lookup pidL ps]
                 ]

rectSize :: Rect -> Size
rectSize r = Size
    { rows = fromIntegral (max 1 (r.endRow - r.startRow))
    , cols = fromIntegral (max 1 (r.endCol - r.startCol))
    }
