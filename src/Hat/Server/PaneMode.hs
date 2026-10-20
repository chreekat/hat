-- | A pane's mode stack (tmux's @window_pane_set_mode@ \/
-- @window_pane_reset_mode@): entering a mode pushes it (or raises it, if
-- the pane already holds that mode further down), leaving one pops it and
-- resumes the next. Every change fires @pane-mode-entered@\/@-exited@ and
-- @pane-mode-changed@, and a mode entered with @-k@ kills its pane on exit.
module Hat.Server.PaneMode
    ( SetMode (..)
    , setPaneMode
    , resetPaneMode
    , resetPaneModeAll
    , modifyTopMode
    , zoomForMode
    , enterClockMode
    , dropModesForReload
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM_, void, when)
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (getPOSIXTime)

import Hat.Model
import Hat.Server.Hooks (notifyWindow)
import Hat.Server.ModeStack
import Hat.Server.Locate (locatePane)
import Hat.Server.Pane (killPaneLocs)
import Hat.Server.Mru (recordVisit)
import Hat.Server.Resize (applySessionSize)

-- | Leave the pane's top mode, resuming the one under it; a mode entered
-- with @-k@ kills the pane as it goes.
resetPaneMode :: ServerState -> Pane -> IO ()
resetPaneMode st pane = popPaneMode st pane $ \p -> do
    mloc <- atomically (locatePane st p.id)
    forM_ mloc $ \(sid, win) -> killPaneLocs st [(sid, win, p)]

-- | Leave every mode the pane holds, innermost first.
resetPaneModeAll :: ServerState -> Pane -> IO ()
resetPaneModeAll st pane = do
    held <- not . null <$> readTVarIO pane.modes
    when held $ resetPaneMode st pane >> resetPaneModeAll st pane

-- | Zoom the pane for a mode entered with @-Z@ (tmux's @mode_tree_zoom@):
-- 'ModeZoomed' when this did the zooming, so leaving the mode unzooms;
-- a window already zoomed, or with nothing to zoom over, is left alone.
zoomForMode :: ServerState -> Session -> Window -> Pane -> IO ModeZoom
zoomForMode st sess win pane = do
    zoomed <- atomically $ do
        z <- readTVar win.zoomed
        n <- Map.size <$> readTVar win.panes
        if z /= Nothing || n < 2 then pure False else do
            active <- readTVar win.activeId
            when (active /= pane.id) $ do
                modifyTVar' win.paneHist (recordVisit active pane.id)
                writeTVar win.activeId pane.id
            writeTVar win.zoomed (Just pane.id)
            True <$ bumpDirty st
    if not zoomed then pure ModeLeftZoom else do
        applySessionSize st sess.id
        notifyWindow st "window-zoomed" (Just sess.id) win []
        pure ModeZoomed

-- | @clock-mode@: clock mode becomes the pane's top mode, redrawn each
-- second while the pane holds it.
enterClockMode :: ServerState -> Pane -> IO ()
enterClockMode st pane = do
    ticking <- atomically (holdsClock pane)
    _ <- setPaneMode st pane "clock-mode" (pure ClockMode) KeepPane
    if ticking then pure () else void (forkIO tick)
  where
    tick = do
        now <- getPOSIXTime
        let toNext = 1 - (realToFrac now - fromIntegral (floor now :: Integer)) :: Double
        threadDelay (max 1000 (round (toNext * 1_000_000)))
        on <- atomically $ do
            held <- holdsClock pane
            when held (bumpDirty st)
            pure held
        when on tick

holdsClock :: Pane -> STM Bool
holdsClock pane = any ((== ClockMode) . (.mode)) <$> readTVar pane.modes

-- | Clear every pane's modes ahead of a reload, which carries no mode state:
-- each mode's own effect on its window (a @-Z@ zoom) is undone so the
-- handover captures the tree as it stands outside the modes. A @-k@ mode
-- leaves its pane alive and no mode hook fires: the panes outlive the reload,
-- only the modes do not.
dropModesForReload :: ServerState -> IO ()
dropModesForReload st = do
    panes <- atomically $ do
        sessions <- Map.elems <$> readTVar st.sessions
        wins <- concat <$> mapM (fmap Map.elems . readTVar . (.windows)) sessions
        concat <$> mapM (fmap Map.elems . readTVar . (.panes)) wins
    forM_ panes $ \pane -> do
        held <- atomically $ do
            es <- readTVar pane.modes
            writeTVar pane.modes []
            pure es
        mapM_ (leaveMode st pane . (.mode)) held
