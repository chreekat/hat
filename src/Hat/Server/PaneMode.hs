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
    ) where

import Control.Concurrent.STM
import Control.Monad (forM_, when)
import Data.Maybe (listToMaybe)
import Data.Text (Text)

import Hat.Model
import Hat.Server.Hooks (PayloadItem (..), notifyPane)
import Hat.Server.Locate (locatePane)
import Hat.Server.Pane (killPaneLocs)

-- | Whether 'setPaneMode' entered the mode or found it already showing.
data SetMode = ModeEntered | ModeAlreadyShown
    deriving (Eq, Show)

-- | Show the mode named @name@ in the pane: a no-op when it already shows;
-- raised to the top (taking the new @exit@) when the pane holds it further
-- down; otherwise built by @start@ and pushed.
setPaneMode
    :: ServerState -> Pane -> Text -> IO PaneMode -> ModeExit -> IO SetMode
setPaneMode st pane name start exit = do
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

-- | Leave the pane's top mode, resuming the one under it; a mode entered
-- with @-k@ kills the pane as it goes.
resetPaneMode :: ServerState -> Pane -> IO ()
resetPaneMode st pane = do
    popped <- atomically $ do
        es <- readTVar pane.modes
        case es of
            [] -> pure Nothing
            e : rest -> do
                writeTVar pane.modes rest
                bumpDirty st
                pure (Just (e, modeName . (.mode) <$> listToMaybe rest))
    forM_ popped $ \(e, next) -> do
        let fire = fireModeChange st pane
        fire "pane-mode-exited" (Just (modeName e.mode)) next 0
        fire "pane-mode-changed" (Just (modeName e.mode)) next 0
        when (e.onExit == KillPane) $ do
            mloc <- atomically (locatePane st pane.id)
            forM_ mloc $ \(sid, win) -> killPaneLocs st [(sid, win, pane)]

-- | Leave every mode the pane holds, innermost first.
resetPaneModeAll :: ServerState -> Pane -> IO ()
resetPaneModeAll st pane = do
    held <- not . null <$> readTVarIO pane.modes
    when held $ resetPaneMode st pane >> resetPaneModeAll st pane

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
