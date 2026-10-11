-- | The server side of the mouse: what a client shows for placing a report
-- ('Hat.Server.Mouse.checkMouse' decides the rest), and sending an event on
-- to the pane under it.
module Hat.Server.MouseIO
    ( mouseViewFor
    , mouseEventPane
    , sendMouse
    , paneBoxOf
    , mouseDragUpdate
    , clientMouseEvent
    , startDrag
    , resizeBorderTo
    , mouseFormatVars
    , startSelectionDrag
    , moveCopyCursor
    ) where

import Control.Concurrent.STM
import Control.Monad (forM, forM_, unless, when)
import Data.IORef (modifyIORef', readIORef)
import Data.List (nub)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Vector qualified as V

import Hat.Geometry
import Hat.Model
import Hat.Model.Options (Options (..), optionText)
import Hat.Server.CopyMode qualified as CopyMode
import Hat.Server.FormatDraw qualified as FD
import Hat.Server.Hooks (notifyWindow)
import Hat.Server.Layout (Orientation (..), parentOrientation, resizeCellAt, searchByBorder)
import Hat.Server.Keys (KeyState (..))
import Hat.Server.Locate (clientView)
import Hat.Server.Mouse
import Hat.Server.Pane (typeIntoPane)
import Hat.Server.Resize (applySessionSize, windowArrange, windowLayoutAt)
import Hat.Server.View (statusLayout, statusRangesFor)
import Hat.Term.Emulator qualified as Emu

-- | What the client shows, for placing a report whose rows (now and before)
-- are @ys@: its window's visible panes and its status line, with the ranges
-- of the status lines those rows fall on.
mouseViewFor :: ServerState -> Client -> [Int] -> IO (Maybe MouseView)
mouseViewFor st client ys = do
    mv <- atomically (clientView st client)
    case mv of
        Nothing -> pure Nothing
        Just (sess, win) -> do
            sopts <- atomically (resolveForSession st sess)
            csize <- readTVarIO client.size
            eff <- readTVarIO sess.lastSize
            (rects, _) <- atomically (windowArrange st eff win)
            ws <- readTVarIO sess.windows
            let (_, mStatus) = statusLayout sopts.statusPosition sopts.statusLines
                    (fromIntegral csize.rows)
                width = fromIntegral csize.cols
                lines' = [ y - at | Just at <- [mStatus], y <- nub ys
                         , y >= at, y < at + sopts.statusLines ]
            ranges <- forM lines' $ \line ->
                (,) line <$> statusRangesFor st sess sopts width line
            let rangeAt x line = do
                    row <- List.lookup line ranges
                    r <- row V.!? x
                    statusRangeOf ws r
            pure $ Just MouseView
                { session = sess.id, window = win.id
                , statusAt = mStatus, statusLines = sopts.statusLines
                , statusRange = rangeAt
                , width = fromIntegral eff.cols, height = fromIntegral eff.rows
                , boxes = map (uncurry boxOf) rects }
  where
    -- An unranged column is the status line's default area; a range naming
    -- a window that is gone makes no key.
    statusRangeOf ws = \case
        FD.RangeNone -> Nothing
        FD.RangeLeft -> Just RangeLeft
        FD.RangeRight -> Just RangeRight
        FD.RangePane n -> Just (RangePane (PaneId n))
        FD.RangeWindow ix -> Just (maybe RangeNone (\w -> RangeWindow w.id) (Map.lookup ix ws))
        FD.RangeSession n -> Just (RangeSession (SessionId n))
        FD.RangeUser _ -> Just RangeUser
        FD.RangeControl n -> Just (RangeControl n)

boxOf :: PaneId -> Rect -> PaneBox
boxOf pid r = PaneBox
    { pane = pid, xoff = r.startCol, yoff = r.startRow
    , sx = r.endCol - r.startCol, sy = r.endRow - r.startRow }

-- | The pane an event is over, with its window and its box there when it
-- is visible (tmux's @cmd_mouse_pane@).
mouseEventPane :: ServerState -> MouseEvent -> IO (Maybe (Session, Window, Pane, Maybe PaneBox))
mouseEventPane st ev = do
    sessions <- readTVarIO st.sessions
    case Map.lookup ev.onSession sessions of
        Nothing -> pure Nothing
        Just sess -> do
            ws <- Map.elems <$> readTVarIO sess.windows
            cur <- atomically (currentWindow sess)
            let mwin = case ev.onWindow of
                    Just wid -> List.find (\w -> w.id == wid) ws
                    Nothing -> cur
            case mwin of
                Nothing -> pure Nothing
                Just win -> do
                    ps <- readTVarIO win.panes
                    active <- readTVarIO win.activeId
                    let pid = fromMaybe active ev.onPane
                    case Map.lookup pid ps of
                        Nothing -> pure Nothing
                        Just pane -> do
                            box <- paneBoxOf st sess win pid
                            pure (Just (sess, win, pane, box))

-- | A visible pane's box in its window.
paneBoxOf :: ServerState -> Session -> Window -> PaneId -> IO (Maybe PaneBox)
paneBoxOf st sess win pid = do
    eff <- readTVarIO sess.lastSize
    (rects, _) <- atomically (windowArrange st eff win)
    pure (boxOf pid <$> List.lookup pid rects)

-- | The mouse event the client's key binding is acting on, if the event
-- happened in the session the client shows.
clientMouseEvent :: Maybe Client -> IO (Maybe MouseEvent)
clientMouseEvent = \case
    Nothing -> pure Nothing
    Just c -> do
        mev <- (.mouseEvent) <$> readIORef c.keyState
        sid <- readTVarIO c.session
        pure $ case mev of
            Just ev | ev.onSession == sid -> Just ev
            _ -> Nothing

-- | Hand the client's next drag updates to @dr@ (tmux's
-- @mouse_drag_update@).
startDrag :: Client -> Drag -> IO ()
startDrag c dr = modifyIORef' c.keyState (\k -> k { drag = Just dr })

-- | tmux's @window_copy_start_drag@: a copy-mode selection starts where the
-- drag did, and follows it from here.
startSelectionDrag :: ServerState -> Client -> MouseEvent -> IO ()
startSelectionDrag st client ev = do
    mp <- mouseEventPane st ev
    forM_ mp $ \(sess, win, pane, mbox) -> forM_ mbox $ \box ->
        forM_ (mouseAt True ev box) $ \(px, py) -> do
            atomically $ do
                onText st sess win pane px $ \x -> CopyMode.startDragSelection x py
                bumpDirty st
            startDrag client (DragSelection pane.id)
            mouseDragUpdate st (DragSelection pane.id) ev

-- | tmux's @window_copy_move_mouse@: a copy-mode command acting on a mouse
-- event (but the wheel) first puts the cursor where the mouse is.
moveCopyCursor :: ServerState -> MouseEvent -> IO ()
moveCopyCursor st ev = unless (wheel ev.input.b) $ do
    mp <- mouseEventPane st ev
    forM_ mp $ \(sess, win, pane, mbox) -> forM_ mbox $ \box ->
        forM_ (mouseAt False ev box) $ \(px, py) -> atomically $
            onText st sess win pane px $ \x -> CopyMode.cursorToScreen x py

-- | A drag in progress moves on (tmux's @mouse_drag_update@).
mouseDragUpdate :: ServerState -> Drag -> MouseEvent -> IO ()
mouseDragUpdate st dr ev = case dr of
    DragBorder -> resizeBorderTo st ev
    DragSelection pid -> do
        mp <- mouseEventPane st ev { onPane = Just pid }
        forM_ mp $ \(sess, win, pane, mbox) -> forM_ mbox $ \box ->
            forM_ (mouseAt False ev box) $ \(px, py) -> atomically $ do
                onText st sess win pane px $ \x -> CopyMode.dragSelectionTo x py
                bumpDirty st

-- | Apply a copy-mode change at a pane column, as the text column it shows
-- beside any line numbers.
onText
    :: ServerState -> Session -> Window -> Pane -> Int
    -> (Int -> PaneMode -> PaneMode) -> STM ()
onText st sess win pane px f = do
    opts <- resolveForPane st sess win pane
    let numbering = CopyMode.lineNumberModeOf (optionText "copy-mode-line-numbers" opts)
    modifyTVar' pane.mode $ fmap $ \pm -> f (CopyMode.textColumn numbering pm px) pm

-- | tmux's @cmd_resize_pane_mouse_resize_tiled@: the borders next to where
-- the drag was move to where it is now.
resizeBorderTo :: ServerState -> MouseEvent -> IO ()
resizeBorderTo st ev = do
    mp <- mouseEventPane st ev
    forM_ mp $ \(sess, win, _, _) -> do
        eff <- readTVarIO sess.lastSize
        let row r | ev.statusAt == Just 0, r >= ev.statusLines = r - ev.statusLines
                  | Just at <- ev.statusAt, at > 0, r >= at = at - 1
                  | otherwise = r
            (x, y) = (ev.input.x, row ev.input.y)
            (lx, ly) = (ev.lastX, row ev.lastY)
        changed <- atomically $ do
            lay <- windowLayoutAt eff win
            let paths = nub
                    [ p | (dx, dy) <- [(0, 0), (0, 1), (1, 0), (0, -1), (-1, 0)]
                    , Just p <- [searchByBorder (lx + dx) (ly + dy) lay] ]
                step l p = case parentOrientation p l of
                    Just TopBottom | y /= ly -> resizeCellAt p (y - ly) l
                    Just LeftRight | x /= lx -> resizeCellAt p (x - lx) l
                    _ -> l
                lay' = foldl step lay paths
            if lay' == lay then pure False else do
                writeTVar win.layout lay'
                bumpDirty st
                pure True
        when changed $ do
            applySessionSize st sess.id
            notifyWindow st "window-layout-changed" (Just sess.id) win []

-- | tmux's @input_key_mouse@: an event goes to the pane under it, in the
-- encoding its app asked for, when it is visible and the app tracks the
-- mouse; a timed double click never does.
sendMouse :: ServerState -> MouseEvent -> IO ()
sendMouse st ev = unless ev.ignore $ do
    mp <- mouseEventPane st ev
    forM_ mp $ \(_, _, pane, mbox) -> forM_ mbox $ \box ->
        forM_ (mouseAt False ev box) $ \pos -> do
            ms <- Emu.modes pane.emulator
            (sgr', utf8') <- Emu.mouseEncoding pane.emulator
            let mode = PaneMouseMode
                    { tracking = case ms.mouse of
                        Emu.MouseOff -> 0
                        Emu.MouseClick -> 1
                        Emu.MouseDrag -> 2
                        Emu.MouseMove -> 3
                    , sgr = sgr', utf8 = utf8' }
            forM_ (mouseBytes mode ev pos) (typeIntoPane pane)

-- | The @mouse_*@ format variables for the event a binding acts on (tmux's
-- @format_cb_mouse_*@): where it is in the pane under it, or on the status
-- line, and the word, line and hyperlink there.
mouseFormatVars :: ServerState -> Client -> MouseEvent -> IO [(Text, Text)]
mouseFormatVars st client ev = do
    mp <- mouseEventPane st ev
    let inPane = do
            (_, _, pane, mbox) <- mp
            box <- mbox
            (,) pane <$> mouseAt False ev box
        statusLine = case ev.statusAt of
            Just 0 | ev.input.y < ev.statusLines -> Just ev.input.y
            Just at | at > 0, ev.input.y >= at -> Just (ev.input.y - at)
            _ -> Nothing
    paneVars <- case inPane of
        Nothing -> pure []
        Just (pane, (x, y)) -> do
            seps <- (.wordSeparators) <$> readTVarIO st.options
            mmode <- readTVarIO pane.mode
            (g, row) <- case mmode of
                Just pm -> pure
                    ( CopyMode.frozenGrid pm.frozen
                    , pm.frozen.fgHsize - pm.copyState.viewportOffY + y )
                Nothing -> do
                    scr <- Emu.snapshot pane.emulator
                    pure (CopyMode.frozenGrid FrozenGrid
                        { fgHsize = 0, fgSy = V.length scr.cells
                        , fgSx = fromIntegral scr.size.cols, fgRows = scr.cells }, y)
            word <- CopyMode.gridWord seps g row x
            line <- CopyMode.gridLine g row
            link <- case mmode of
                Just _ -> pure Nothing
                Nothing -> Emu.screenLink pane.emulator x y
            pure $
                [ ("mouse_x", tshow x), ("mouse_y", tshow y)
                , ("mouse_word", word), ("mouse_line", line) ]
                <> [ ("mouse_hyperlink", l) | Just l <- [link] ]
    rangeVars <- case statusLine of
        Nothing -> pure []
        Just line -> do
            msess <- Map.lookup ev.onSession <$> readTVarIO st.sessions
            ranges <- forM (maybeToList msess) $ \sess -> do
                sopts <- atomically (resolveForSession st sess)
                csize <- readTVarIO client.size
                statusRangesFor st sess sopts (fromIntegral csize.cols) line
            let name = \case
                    FD.RangeLeft -> Just "left"
                    FD.RangeRight -> Just "right"
                    FD.RangePane _ -> Just "pane"
                    FD.RangeWindow _ -> Just "window"
                    FD.RangeSession _ -> Just "session"
                    FD.RangeUser u -> Just u
                    FD.RangeControl _ -> Just "control"
                    FD.RangeNone -> Nothing
            pure $
                [ ("mouse_status_line", tshow line) ]
                <> [ ("mouse_x", tshow ev.input.x) | Nothing <- [inPane] ]
                <> [ ("mouse_y", tshow line) | Nothing <- [inPane] ]
                <> [ ("mouse_status_range", r)
                   | rs <- ranges, Just c <- [rs V.!? ev.input.x], Just r <- [name c] ]
    pure $ paneVars <> rangeVars
        <> [ ("mouse_pane", "%" <> tshow (rawPane pane.id)) | Just (_, _, pane, _) <- [mp] ]
