-- | The stack operations a pane's modes share: entering or raising a mode,
-- changing the one on top, and leaving it. See "Hat.Server.PaneMode" for
-- leaving one with its @-k@ kill.
module Hat.Server.ModeStack
    ( SetMode (..)
    , setPaneMode
    , popPaneMode
    , leaveMode
    , unzoomPaneWindow
    , modifyTopMode
    , fireModeChange
    ) where

import Control.Concurrent.STM
import Control.Monad (forM_, when)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Model
import Hat.Server.HookTypes (HooksState (..))
import Hat.Server.Hooks (PayloadItem (..), notifyPane, notifyWindow)
import Hat.Server.Locate (locatePane)
import Hat.Server.Resize (applySessionSize)

-- | Whether 'setPaneMode' entered the mode or found it already showing.
data SetMode = ModeEntered | ModeAlreadyShown
    deriving (Eq, Show)

-- | Show the mode named @name@ in the pane: a no-op when it already shows;
-- raised to the top (taking the new @exit@) when the pane holds it further
-- down; otherwise built by @start@ and pushed. A top mode that does not
-- stack ('modeStacks') is left first.
setPaneMode
    :: ServerState -> Pane -> Text -> IO PaneMode -> ModeExit -> IO SetMode
setPaneMode st pane name start exit = do
    top <- atomically (topMode pane)
    case top of
        Just m | modeName m /= name, not (modeStacks m) -> popPaneMode st pane killByCommand
        _ -> pure ()
    es <- readTVarIO pane.modes
    case es of
        e : _ | modeName e.mode == name -> pure ModeAlreadyShown
        _ -> do
            let oname = modeName . (.mode) <$> listToMaybe es
            fresh <- case filter ((== name) . modeName . (.mode)) es of
                [] -> Just <$> start
                _ -> pure Nothing
            placed <- atomically $ do
                cur <- readTVar pane.modes
                let place e rest = do
                        writeTVar pane.modes (e : rest)
                        bumpDirty st
                        pure True
                case (break ((== name) . modeName . (.mode)) cur, fresh) of
                    ((pre, e : post), _) -> place e { onExit = exit } (pre <> post)
                    ((_, []), Just m) -> place ModeEntry { mode = m, onExit = exit } cur
                    -- Raced out from under us since the first read.
                    ((_, []), Nothing) -> pure False
            if not placed then setPaneMode st pane name start exit else do
                fire "pane-mode-entered" oname (Just name) 1
                fire "pane-mode-changed" oname (Just name) 1
                pure ModeEntered
  where
    fire = fireModeChange st pane
    -- Killing a pane is above this module, so the command does it.
    killByCommand p = do
        run <- readTVarIO st.hooks.runner
        run ("kill-pane -t %" <> T.pack (show (rawPane p.id)))

-- | Leave the pane's top mode, resuming the one under it; a mode entered
-- with @-k@ hands its pane to @kill@ as it goes.
popPaneMode :: ServerState -> Pane -> (Pane -> IO ()) -> IO ()
popPaneMode st pane kill = do
    popped <- atomically $ do
        es <- readTVar pane.modes
        case es of
            [] -> pure Nothing
            e : rest -> do
                writeTVar pane.modes rest
                bumpDirty st
                pure (Just (e, modeName . (.mode) <$> listToMaybe rest))
    forM_ popped $ \(e, next) -> do
        leaveMode st pane e.mode
        let fire = fireModeChange st pane
        fire "pane-mode-exited" (Just (modeName e.mode)) next 0
        fire "pane-mode-changed" (Just (modeName e.mode)) next 0
        when (e.onExit == KillPane) (kill pane)

-- | What leaving a mode undoes: the zoom it made, the screen view mode's
-- text was written to.
leaveMode :: ServerState -> Pane -> PaneMode -> IO ()
leaveMode st pane = \case
    CopyMode d | d.kind == ModeView -> atomically $ writeTVar pane.viewBacking Nothing
    TreeMode t | t.zoom == ModeZoomed -> unzoomPaneWindow st pane
    -- Its window's chrome comes back either way.
    PanesMode p
        | p.zoom == ModeZoomed -> unzoomPaneWindow st pane
        | otherwise -> atomically (locatePane st pane.id) >>= mapM_ (applySessionSize st . fst)
    _ -> pure ()

-- | Unzoom the window holding the pane, if it is zoomed.
unzoomPaneWindow :: ServerState -> Pane -> IO ()
unzoomPaneWindow st pane = do
    mloc <- atomically (locatePane st pane.id)
    forM_ mloc $ \(sid, win) -> do
        was <- atomically $ do
            z <- readTVar win.zoomed
            writeTVar win.zoomed Nothing
            when (z /= Nothing) (bumpDirty st)
            pure z
        when (was /= Nothing) $ do
            applySessionSize st sid
            notifyWindow st "window-unzoomed" (Just sid) win []

-- | Rewrite the pane's top mode in place (a mode's own state change: a
-- cursor motion, a selection), leaving the stack's shape alone.
modifyTopMode :: ServerState -> Pane -> (PaneMode -> PaneMode) -> STM ()
modifyTopMode st pane f = do
    es <- readTVar pane.modes
    case es of
        e : rest -> do
            writeTVar pane.modes (ModeEntry { mode = f e.mode, onExit = e.onExit } : rest)
            bumpDirty st
        [] -> pure ()

fireModeChange
    :: ServerState -> Pane -> Text -> Maybe Text -> Maybe Text -> Int -> IO ()
fireModeChange st pane hook previous current entered =
    notifyPane st hook pane $
        [ ("current_mode", PText m) | Just m <- [current] ]
        <> [ ("previous_mode", PText m) | Just m <- [previous] ]
        <> [ ("mode_entered", PInt entered) ]
