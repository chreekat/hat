-- | The per-client rendering and presentation layer: turns the model
-- tree into frames, the status line, and the chooser\/copy-mode overlays.
module Hat.Server.View
    ( renderLoop
    , framePeriod
    , awaitRenderable
    , renderOnce
    , statusCells
    , flashTarget
    , statusLayout  -- ^ exported for the status-position effect test
    , messageLine  -- ^ exported for the message-line effect test
    , statusRangesFor
    , promptCursorOf
    , decscusr
    ) where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.STM
import Control.Exception (evaluate)
import Control.Monad (foldM, forM, join, when)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.IORef
import Data.Word (Word8)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.LocalTime (getCurrentTimeZone, getZonedTime, zonedTimeToLocalTime)
import Data.Vector qualified as V

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.PtrEq (samePtr)
import Hat.Server.ColorScheme (flashStyle, previewLabelStyle)
import Hat.Server.Clock qualified as Clock
import Hat.Server.CopyMode qualified as CopyMode
import Hat.Server.ClientIO (hasRoom, send, sendAs)
import Hat.Server.Outbound (Delivery (..), Slot (..))
import Hat.Server.DisplayPanes qualified as DisplayPanes
import Hat.Server.Flash (startLingerClock)
import Hat.Server.Format (clockTick, formatClock, formatReadsLoops, formatStrftimes)
import Hat.Server.Format qualified as F
import Hat.Server.HookTypes (HooksState (..))
import Hat.Server.FormatDraw (colourValue, formatDraw, styleCell)
import Hat.Server.FormatEnv
    (FormatScope (..), FormatTarget (..), Wanted (..), wantedNames,
     clientTheme, demanding, expandAt, formatCtxFor, frameMemo, keepMemo,
     memoExpander, Expand (..), sessionFormatEnv, sessionVars, windowOwnEnv, windowOwnVars)
import Hat.Server.Layout (paneOrder, twoPaneSplit)
import Hat.Server.Resize (paneScrollbars, windowArrange, windowFloats, windowPaneStatus)
import Hat.Server.Locate (locatePane, paneIndexOf)
import Hat.Server.PaneColours (cursorColourOf, paneDefaultColours, panePalette, paneStyleOption)
import Hat.Server.Scene
import Hat.Server.Viewport (viewportFor)
import Hat.Server.Theme (Theme, colourRgb)
import Hat.Server.Picker qualified as Picker
import Hat.Server.Render
import Hat.Server.Menu
    (MenuLook (..), MenuMouse (..), WindowMenu (..), menuGrid, menuHeight, menuWidth
    , overrideColours)
import Hat.Server.Message (PromptView (..), drawMessage, drawPrompt)
import Hat.Server.Popup (PopupState (..), popupCursor, popupGrid)
import Hat.Server.Mouse qualified as Mouse
import Hat.Server.FormatDraw (DrawRange)
import Hat.Server.Status
    (StatusTexts (..), drawStatus, statusFormats, statusRanges, statusRows, statusTexts)
import Hat.Term.Cell qualified as Cell
import Hat.Term.Emulator qualified as Emu
import Hat.Transport.Wire

-- Rendering ---------------------------------------------------------------

-- | Minimum gap between frames, in microseconds (~80 fps): the pacing
-- 'renderLoop' is spawned with in production.
framePeriod :: Int
framePeriod = 12500

-- | Paint each dirty generation, at most one frame per @period@: the wait
-- runs after the paint, so dirty ticks landing inside it coalesce into the
-- next frame while an idle client's next event still paints immediately.
renderLoop :: Int -> ServerState -> Client -> IO ()
renderLoop period st client = loop (-1)
  where
    loop lastGen = do
        animated <- maybe False (.animated) <$> readIORef client.lastStatus
        gen <- if animated
            then awaitOrFrame lastGen
            else atomically (awaitRenderable st client lastGen)
        renderOnce st client
        threadDelay period
        loop gen
    -- An animated status line repaints at least every animation period
    -- (tmux's cycle timer), dirty or not.
    awaitOrFrame lastGen = do
        timer <- registerDelay animationPeriod
        atomically $ awaitRenderable st client lastGen
            `orElse` (lastGen <$ (readTVar timer >>= check))

-- | How often an animated (@#{A:…}@) status line advances, in
-- microseconds: tmux's @FORMAT_CYCLE_PERIOD@.
animationPeriod :: Int
animationPeriod = 100000

-- | The dirty generation 'renderLoop' paints next: newer than the one last
-- painted (or a forced full redraw via @needsFull@), but only once
-- 'reconcileLoop' has resized the panes through it. Gating on @reconciled@
-- keeps a layout change — a zoom, an unzoom, a split, a client resize — off
-- the screen until the emulators behind it grow or shrink to match; painting
-- earlier flashes one frame of the old grid composited into the new geometry.
-- Blocks until such a generation exists and the client has room for it.
awaitRenderable :: ServerState -> Client -> Int -> STM Int
awaitRenderable st client lastGen = do
    g <- readTVar st.dirty
    full <- readTVar client.needsFull
    check (g /= lastGen || full)
    -- A locked client's terminal is its lock command's.
    check . (/= TtyStopped) =<< readTVar client.ttyState
    done <- readTVar st.reconciled
    check (done >= g)
    check =<< hasRoom client  -- See Note [Falling behind]
    pure g

-- | Where the status bar sits in a client of @rows@ rows: the vertical offset
-- the window content takes below a top bar, and the row the bar occupies
-- (@Nothing@ when @status@ is off — no bar, content fills the height). See
-- 'renderOnce'.
statusLayout :: StatusPosition -> Int -> Int -> (Int, Maybe Int)
statusLayout _   0     _    = (0, Nothing)
statusLayout pos count rows = case pos of
    StatusTop    -> (count, Just 0)
    StatusBottom -> (0, Just (rows - count))

-- | The rect whose edge cells the prefix highlight tints: the active pane's,
-- whatever the layout — a lone pane, a split, or a zoomed pane all qualify.
flashTarget :: [(PaneId, Rect)] -> PaneId -> Maybe Rect
flashTarget rects active = List.lookup active rects

-- | Before drawing to a terminal just handed back, forget what the last
-- frames set on it: it comes back with none of the server's mouse
-- tracking or cursor colour and shape. Render thread only.
freshTerminalDrawn :: Client -> IO ()
freshTerminalDrawn c = do
    restarted <- atomically $ readTVar c.ttyState >>= \case
        TtyRestarted -> True <$ writeTVar c.ttyState TtyStarted
        _ -> pure False
    when restarted $ do
        writeIORef c.lastMouseMode 0
        writeIORef c.lastCursorColour Nothing
        writeIORef c.lastCursorShape CursorDefault

renderOnce :: ServerState -> Client -> IO ()
renderOnce st client = do
    freshTerminalDrawn client
    -- One transaction: the frame is built from this size, so 'fullFlag' must
    -- decide against the same size.
    (csize, fullFlag) <- atomically $
        (,) <$> readTVar client.size <*> swapTVar client.needsFull False
    opts <- readTVarIO st.options
    memo <- frameMemo client
    let expand = memoExpander st memo
    view <- atomically $ do
        sid <- readTVar client.session
        msess <- Map.lookup sid <$> readTVar st.sessions
        case msess of
            Nothing -> pure Nothing
            Just sess -> do
                mwin <- currentWindow sess
                case mwin of
                    Nothing -> pure Nothing
                    Just win -> do
                        eff <- readTVar sess.lastSize
                        (rects, _) <- windowArrange st eff win
                        floats <- windowFloats st win
                        ps <- readTVar win.panes
                        active <- readTVar win.activeId
                        pure (Just (sess, win, rects, (eff, floats), ps, active))
    sopts <- case view of
        Just (sess, _, _, _, _, _) -> resolveForSessionIO st sess
        Nothing -> pure opts
    let (rowOff, mStatusRowIx) =
            statusLayout sopts.statusPosition sopts.statusLines
                (fromIntegral csize.rows)
    (frame, origins, cursor, mActiveRect, viewOrigin, panned) <- case view of
        Nothing -> pure
            ( blankFrame csize
            , V.replicate (fromIntegral csize.rows) VolatileRow
            , (Pos 0 0, False), Nothing, Pos 0 0, False )
        Just (sess, win, rects, (eff, floats), ps, active) -> do
            -- Snapshot each visible pane once; its cells and its cursor both
            -- come from that one read.
            th <- clientTheme st
            painted <- fmap concat $ forM rects $ \(pidL, _) ->
                case Map.lookup pidL ps of
                    Nothing -> pure []
                    Just pane -> do
                        pv <- paneView st pane
                        popts <- resolveForPaneIO st sess win pane
                        dflt <- paneDefaultColours st expand th sess win pane popts
                        palette <- panePalette th pane popts
                        pure [(pidL, (withPaneColours dflt palette pv, dflt, popts))]
            let paneViews = Map.fromList [ (p, pv) | (p, (pv, _, _)) <- painted ]
                defaults = Map.fromList [ (p, d) | (p, (_, d, _)) <- painted ]
                paneOpts = Map.fromList [ (p, o) | (p, (_, _, o)) <- painted ]
            let sceneArea = csize
                    { rows = fromIntegral (max 0 (fromIntegral csize.rows - sopts.statusLines)) }
                shownCursor = do
                    pv <- Map.lookup active paneViews
                    case placePaneCursor (paneOrigin rects active) 0 pv of
                        (p, True) -> Just p
                        _ -> Nothing
            vp <- atomically $ do
                prev <- readTVar client.viewport
                let next = viewportFor win.id eff sceneArea shownCursor prev
                when (next /= prev) $ writeTVar client.viewport next
                pure next
            drawnOrigin <- readIORef client.lastViewOrigin
            writeIORef client.lastViewOrigin vp.origin
            let o = vp.origin
                moved = o /= drawnOrigin
                toView p = Pos { row = p.row - o.row, col = p.col - o.col }
                inScene p = p.row >= 0 && p.row < fromIntegral sceneArea.rows
                    && p.col >= 0 && p.col < fromIntegral sceneArea.cols
                shiftRect r = r
                    { startRow = r.startRow + rowOff - o.row
                    , endRow = r.endRow + rowOff - o.row
                    , startCol = r.startCol - o.col
                    , endCol = r.endCol - o.col
                    }
            prevFrame <- readIORef client.lastFrame
            -- A pane slice's provenance does not record which window
            -- columns it showed, so no row survives the view moving.
            prevOrigins <- if moved then pure V.empty else readIORef client.lastOrigins
            chrome <- cachedChrome st expand th client sess win eff sceneArea rects floats
                defaults paneOpts active rowOff o
            let layers =
                    [ PaneLayer { pane = pidL, rect = shiftRect shown
                                , cells = dropGrid skip pv.cells
                                , gens = V.drop skip.row <$> pv.rowGens }
                    | (pidL, rect) <- rects
                    , let (shown, skip)
                            | pidL `elem` map (.pane) floats = clipToWindow eff rect
                            | otherwise = (rect, Pos 0 0)
                    , Just pv <- [Map.lookup pidL paneViews] ]
                (base, baseOrigins) = composeRows csize
                    chrome layers prevFrame prevOrigins
            mflash <- readTVarIO client.flash
            scheme <- readTVarIO st.colorScheme
            let (flashed, flashedOrigins)
                    | isJust mflash
                    , Just r <- flashTarget rects active =
                        ( tintInnerRing (flashStyle scheme) base (shiftRect r)
                        , volatileRows (shiftRect r) baseOrigins )
                    | otherwise = (base, baseOrigins)
            mprompt <- readTVarIO client.prompt
            mtoast <- readTVarIO client.toast
            let w = fromIntegral csize.cols
                -- A transient prompt or message borrows the bottom line even
                -- when the persistent bar is off; the bar itself draws only
                -- when status is on.
                bottomIx = fromIntegral csize.rows - 1
                msgIx = maybe bottomIx (+ messageLine sopts) mStatusRowIx
                fits = csize.rows >= 2
            barRows <- case mStatusRowIx of
                Just ix | fits -> zip [ix ..]
                    <$> statusRowsCached st client sess sopts w
                _ -> pure []
            -- The message line draws over whatever status line it takes.
            let under = fromMaybe (V.replicate w Cell.blankCell)
                    (List.lookup msgIx barRows)
            msgRow <- case (mprompt, mtoast) of
                _ | not fits -> pure []
                (Just pr, _) -> do
                    ctx <- messageCtx st sess
                    let (row, cx) = drawPrompt ctx sopts (promptView pr) under
                    pure [(msgIx, row, Just cx)]
                (Nothing, Just t) -> do
                    ctx <- messageCtx st sess
                    pure [(msgIx, drawMessage ctx sopts t.markup t.text under, Nothing)]
                (Nothing, Nothing) -> pure []
            let overRows = [ r | r@(ix, _) <- barRows <> [ (ix, row) | (ix, row, _) <- msgRow ]
                               , ix >= 0, ix < V.length flashed ]
                (withStatus, statusOrigins) =
                    ( flashed V.// overRows
                    , flashedOrigins V.//
                        [ (ix, VolatileRow) | (ix, _) <- overRows
                        , ix < V.length flashedOrigins ] )
            cur <- case (mprompt, msgRow) of
                (Just _, (ix, _, Just cx) : _) ->
                    pure (Pos { row = ix, col = min (w - 1) cx }, True)
                _ -> pure $ case Map.lookup active paneViews of
                    Just pv
                        | Just _ <- pv.cursor
                        , (p, visible) <- placePaneCursor (paneOrigin rects active) 0 pv
                        , inScene (toView p) ->
                            (let q = toView p in q { row = q.row + rowOff }, visible)
                    _ -> (Pos 0 0, False)
            pure ( withStatus, statusOrigins, cur
                 , shiftRect <$> List.lookup active rects, o, moved )
    -- A chooser overlay, when open, is drawn in the active pane's rect
    -- (or the whole window under -Z), with a live preview of the
    -- highlighted node's pane beside the list.
    mpicker <- readTVarIO client.picker
    (frame', origins', cursor') <- case mpicker of
        Nothing -> pure (frame, origins, cursor)
        Just pk -> do
            let region = Picker.pickerRegion pk.fill csize rowOff mActiveRect
                width = region.endCol - region.startCol
                rows = region.endRow - region.startRow
            mPreview <- case Picker.pickerSplit width of
                Just listW -> pickerPreviewCells st pk Size
                    { rows = fromIntegral (max 0 rows)
                    , cols = fromIntegral (max 0 (width - listW - 1)) }
                Nothing -> pure Nothing
            pure ( overlayPicker region pk mPreview frame
                 , volatileRows region origins
                 , (Pos 0 0, False) )
    -- The display-panes overlay stamps each pane's number over its rect.
    mdpanes <- readTVarIO client.displayPanes
    (frame'', origins'') <- case (mdpanes, view) of
        (Just _, Just (sess, win, rects, _, ps, active)) -> do
            th <- clientTheme st
            wopts <- resolveForWindowIO st sess win
            wix <- atomically (readTVar sess.currentIx)
            let shiftR r = r { startRow = r.startRow + rowOff - viewOrigin.row
                             , endRow = r.endRow + rowOff - viewOrigin.row
                             , startCol = r.startCol - viewOrigin.col
                             , endCol = r.endCol - viewOrigin.col }
                colourFor pid = fromMaybe Cell.DefaultColor $ join $ colourValue th $
                    optionText (if pid == active then "display-panes-active-colour"
                                else "display-panes-colour") wopts
                fmt = optionText "display-panes-format" wopts
            stamps <- fmap concat $ forM rects $ \(pid, rect0) -> case Map.lookup pid ps of
                Nothing -> pure []
                Just pane -> do
                    n <- paneIndexOf st win pane
                    let r = shiftR rect0
                        sx = r.endCol - r.startCol
                        fgc = Cell.defaultStyle { Cell.fg = colourFor pid }
                    mformat <- if T.null fmt then pure Nothing else do
                        expanded <- expand.format
                            (FormatTarget (InPane sess wix win n pane) Map.empty) fmt
                        pure (Just (drawnCells (formatDraw th fgc expanded) sx))
                    pure [ (Pos (p.row + r.startRow) (p.col + r.startCol), cell)
                         | (p, cell) <- DisplayPanes.panesStamp (colourFor pid) n
                             mformat sx (r.endRow - r.startRow) ]
            pure ( DisplayPanes.stampCells stamps frame'
                 , List.foldl' (\o (_, rect0) -> volatileRows (shiftR rect0) o)
                     origins' rects )
        _ -> pure (frame', origins')
    -- A menu's screen has no cursor.
    menuShown <- case view of
        Just (_, win, _, _, _, _) -> isJust <$> readTVarIO win.menu
        Nothing -> pure False
    -- The popup draws over everything, and its command's cursor is the
    -- client's.
    mpopup <- readTVarIO client.popup
    (frame3, origins3, cursor'') <- case (mpopup, view) of
        (Just p, Just (sess, win, _, _, _, _)) -> do
            (grid, rect, cur) <- popupView st expand sess win p
            pure (overlayGrid frame'' rect grid, volatileRows rect origins'', cur)
        _ -> pure (frame'', origins'', if menuShown then (fst cursor', False) else cursor')
    keepMemo client memo
    old <- readIORef client.lastFrame
    oldOrigins <- readIORef client.lastOrigins
    oldCursor <- readIORef client.lastCursor
    -- A diff is only valid between same-sized frames; force full on any
    -- dimension change.
    let full = fullFlag || panned || frameDims old /= frameDims frame3
        known r = case (oldOrigins V.!? r, origins3 V.!? r) of
            (Just a, Just b) -> sameOrigin a b
            _ -> False
        ops = if full then fullRedraw frame3 else diffFrameKnown known old frame3
        cursorOp = CursorAt (fst cursor'') (snd cursor'')
        needSend = not (null ops) || cursor'' /= oldCursor || full
    writeIORef client.lastFrame frame3
    writeIORef client.lastOrigins origins3
    writeIORef client.lastCursor cursor''
    when needSend $ send client (Draw (ops <> [cursorOp]))
    -- The moved-to pane is now on screen, so a pending move-linger starts its
    -- countdown from here rather than from the command.
    startLingerClock st client
    -- @cursor-colour@: mirror the viewed active pane's resolved colour onto
    -- the outer terminal's cursor (OSC 12), resetting it (OSC 112) when the
    -- pane in view no longer has one.
    -- While a prompt is open the cursor is the prompt's, as its options were
    -- when it opened.
    th <- clientTheme st
    inPrompt <- fmap promptCursorOf <$> readTVarIO client.prompt
    colour <- case (inPrompt, view) of
        (Just pc, _) -> pure (colourValue th pc.colour >>= id >>= colourRgb)
        (Nothing, Just (sess, win, _, _, ps, active))
            | Just pane <- Map.lookup active ps -> do
                app <- Emu.appCursorColour pane.emulator
                popts <- resolveForPaneIO st sess win pane
                pure (cursorColourOf th app popts.cursorColour)
        _ -> pure Nothing
    oldColour <- readIORef client.lastCursorColour
    when (colour /= oldColour) $ do
        writeIORef client.lastCursorColour colour
        -- See Note [Falling behind]
        sendAs (Superseding CursorColourSlot) client $ Notify $ case colour of
            Nothing -> "\ESC]112\a"
            Just (r, g, b) -> TE.encodeUtf8 $ T.concat
                [ "\ESC]12;rgb:", hex2 r, "/", hex2 g, "/", hex2 b, "\a" ]
    let shape = maybe CursorDefault (.shape) inPrompt
    oldShape <- readIORef client.lastCursorShape
    when (shape /= oldShape) $ do
        writeIORef client.lastCursorShape shape
        -- See Note [Falling behind]
        sendAs (Superseding CursorShapeSlot) client (Notify (decscusr shape))
    syncMouseMode st client (fmap (\(sess, win, _, _, ps, active) -> (sess, win, ps, active)) view)
  where
    frameDims f = (V.length f, maybe 0 V.length (f V.!? 0))


-- | A client's popup drawn (tmux's @popup_draw_cb@): its box and its
-- command's screen in @popup-style@ and @popup-border-style@, re-read on
-- every frame in the client's current window's context, under the colours
-- its flags give; where it goes; and its command's cursor.
popupView
    :: ServerState -> Expand -> Session -> Window -> Popup
    -> IO (V.Vector (V.Vector Cell.Cell), Rect, (Pos, Bool))
popupView st expand sess win p = do
    th <- clientTheme st
    ps <- readTVarIO p.state
    scr <- Emu.snapshot p.screen
    (wix, wopts) <- atomically $ (,) <$> readTVar sess.currentIx <*> resolveForWindow st sess win
    let styleOf name = do
            let raw = optionText name wopts
            expanded <- if "#{" `T.isInfixOf` raw
                then expand.format (FormatTarget (InWindow sess wix win) Map.empty) raw
                else pure raw
            expand.style th expanded
    dflt <- (\c -> overrideColours th c ps.style) <$> styleOf "popup-style"
    bdr <- (\c -> overrideColours th c ps.borderStyle) <$> styleOf "popup-border-style"
    let colours c = Cell.defaultStyle { Cell.fg = c.fg, Cell.bg = c.bg }
        grid = paintDefaults (dflt.fg, dflt.bg)
            (popupGrid th (colours bdr) ps scr.cells)
        rect = Rect { startRow = ps.py, endRow = ps.py + ps.sy
                    , startCol = ps.px, endCol = ps.px + ps.sx }
        (cx, cy) = popupCursor ps (scr.cursor.col, scr.cursor.row)
    pure (grid, rect, (Pos { row = cy, col = cx }, scr.cursorVisible))

-- | The cells a drawing writes into an @n@-cell row, by column: what it
-- leaves untouched is no cell at all.
drawnCells :: (V.Vector Cell.Cell -> V.Vector Cell.Cell) -> Int -> [(Int, Cell.Cell)]
drawnCells draw n =
    [ (c, cell) | (c, cell) <- zip [0 ..] (V.toList (draw untouched)), cell /= hole ]
  where
    hole = Cell.glyphCell '\0' Cell.defaultStyle
    untouched = V.replicate n hole

-- | The cursor of an open prompt: its command-mode one in vi command mode.
promptCursorOf :: PromptState -> PromptCursor
promptCursorOf pr = case pr.promptMode of
    PromptCommand -> pr.promptKeys.commandCursor
    PromptInsert -> pr.promptKeys.insertCursor

-- | The DECSCUSR sequence setting a cursor shape.
decscusr :: CursorShape -> B.ByteString
decscusr shape = "\ESC[" <> B8.pack (show (fromEnum shape)) <> " q"

hex2 :: Word8 -> Text
hex2 w = T.pack [digit (w `div` 16), digit (w `mod` 16)]
  where
    digit d = "0123456789abcdef" !! fromIntegral d
-- | Keep the outer terminal's mouse tracking where the client's view needs
-- it. A pane in a mode tracks nothing, as its mode's screen does in tmux.
syncMouseMode :: ServerState -> Client -> Maybe (Session, Window, Map.Map PaneId Pane, PaneId) -> IO ()
syncMouseMode st client view = do
    mmenu <- maybe (pure Nothing) (\(_, win, _, _) -> readTVarIO win.menu) view
    mpopup <- readTVarIO client.popup
    want <- case view of
        Nothing -> pure 0
        Just (sess, _, _, _) | Just p <- mpopup -> do
            o <- resolveForSessionIO st sess
            ms <- Emu.modes p.screen
            let own = case ms.mouse of
                    Emu.MouseOff -> 0
                    Emu.MouseClick -> 1
                    Emu.MouseDrag -> 2
                    Emu.MouseMove -> 3
            pure $ Mouse.overlayMouseMode o.mouseEnabled o.focusFollowsMouse own
        Just (sess, _, _, _) | Just wm <- mmenu -> do
            o <- resolveForSessionIO st sess
            pure $ Mouse.overlayMouseMode o.mouseEnabled o.focusFollowsMouse
                (if wm.mouse == MenuMouseOn then 3 else 0)
        Just (sess, win, ps, active) -> do
            tracking <- forM (Map.toList ps) $ \(pid, pane) -> do
                inMode <- isJust <$> readTVarIO pane.mode
                level <- if inMode then pure 0 else do
                    ms <- Emu.modes pane.emulator
                    pure $ case ms.mouse of
                        Emu.MouseOff -> 0
                        Emu.MouseClick -> 1
                        Emu.MouseDrag -> 2
                        Emu.MouseMove -> 3
                pure (pid, level)
            o <- maybe (resolveForSessionIO st sess)
                (\pane -> resolveForPaneIO st sess win pane) (Map.lookup active ps)
            pure $ Mouse.outerMouseMode o.mouseEnabled o.focusFollowsMouse
                (fromMaybe 0 (List.lookup active tracking)) (map snd tracking)
    old <- readIORef client.lastMouseMode
    when (want /= old) $ do
        writeIORef client.lastMouseMode want
        -- See Note [Falling behind]
        send client (Notify (Mouse.mouseModeBytes want))

-- | Drop the pane provenance of the rect's rows: whatever repainted them
-- owns them now.
volatileRows :: Rect -> V.Vector RowOrigin -> V.Vector RowOrigin
volatileRows rect o = o V.//
    [ (r, VolatileRow)
    | r <- [max 0 rect.startRow .. min (V.length o) rect.endRow - 1] ]

-- | The rendered cells previewing the highlighted node, sized to the
-- preview column (@size@): a single pane's contents, a whole window
-- composited in its split layout, or a session's windows. 'Nothing' when
-- the node has no preview, or its target no longer exists.
pickerPreviewCells
    :: ServerState -> PickerState -> Size
    -> IO (Maybe (V.Vector (V.Vector Cell.Cell)))
pickerPreviewCells st pk size = case Picker.selectedPreview pk of
    Nothing -> pure Nothing
    Just (PreviewPane pid) -> do
        mpane <- atomically (findPaneById st (rawPane pid))
        traverse (paneViewCells st) mpane
    Just (PreviewWindow wid) -> do
        mwin <- atomically (findWindowById st wid)
        traverse (\w -> windowCompositeCells st w size) mwin
    Just (PreviewSession sid) -> do
        msess <- atomically (findSessionById st sid)
        traverse (\s -> sessionPreviewCells st s size) msess

-- | Composite a whole window — every pane painted into its layout rect,
-- with the borders between them — into a @size@-sized grid. This is what
-- makes a window preview show its splits, not just the active pane.
windowCompositeCells
    :: ServerState -> Window -> Size -> IO (V.Vector (V.Vector Cell.Cell))
windowCompositeCells st win size = do
    opts <- readTVarIO st.options
    th <- clientTheme st
    (rects, floats, ps, active, status) <- atomically $ do
        (r, _) <- windowArrange st size win
        fl     <- windowFloats st win
        ps     <- readTVar win.panes
        a      <- readTVar win.activeId
        s      <- windowPaneStatus st win
        pure (r, fl, ps, a, s)
    let input = SceneInput
            { window = size, area = size, panes = tiledOf floats rects, paneStatus = status
            , twoPane = Nothing, scrollbars = Map.empty, scrollbarSide = ScrollbarRight
            , floats, menu = Nothing }
        look = ChromeLook
            { borderLines = opts.paneBorderLines
            , lineSets = Map.fromList [ (f.pane, f.lineSet) | f <- floats ]
            , windowStyle = styleCell th (optionText "pane-border-style" opts)
            , paneStyles = Map.singleton active
                (styleCell th (optionText "pane-active-border-style" opts))
            , scrollbarLooks = Map.empty
            , active = Just active, marked = Nothing, numbers = Map.empty
            , paneLines = Map.empty
            , fillInside = Cell.blankCell, fillOutside = Cell.blankCell
            , arrows = Map.empty, menuCells = V.empty }
    panes <- foldM (\acc (pid, rect) -> case Map.lookup pid ps of
              Nothing   -> pure acc
              Just pane -> do
                  cells <- paneViewCells st pane
                  let (shown, skip) = clipToWindow size rect
                  pure (overlayGrid acc shown (dropGrid skip cells)))
        (blankFrame size) rects
    pure (applyBorders panes (sceneChrome look (buildScene input)))

-- | Preview a session as a vertical stack of its windows: each window's
-- index\/name on a coloured label bar dividing it from the window above,
-- then a thumbnail of that window's composited split layout. Windows that do not fit are omitted (the list still names
-- them).
sessionPreviewCells
    :: ServerState -> Session -> Size -> IO (V.Vector (V.Vector Cell.Cell))
sessionPreviewCells st sess size = do
    wins <- Map.toAscList <$> readTVarIO sess.windows
    labelStyle <- previewLabelStyle <$> readTVarIO st.colorScheme
    let slices = Picker.stackThumbnails (fromIntegral size.rows) (length wins)
    foldM (place labelStyle) (blankFrame size) (zip wins slices)
  where
    place labelStyle acc ((ix, win), (labelRow, bodyTop, bodyH)) = do
        wname <- readTVarIO win.name
        thumb <- windowCompositeCells st win
            (Size { rows = fromIntegral bodyH, cols = size.cols })
        let width = fromIntegral size.cols
            label = lineCells labelStyle width (tshow ix <> ":" <> wname)
            body  = Rect { startRow = bodyTop, endRow = bodyTop + bodyH
                         , startCol = 0, endCol = width }
        pure (overlayGrid (acc V.// [(labelRow, label)]) body thumb)

-- | Paint a chooser into @region@: the list on the left and, when wide
-- enough, a preview of the highlighted node's pane on the right, divided
-- by a vertical rule. Cells outside @region@ (other panes, borders, the
-- status line) are left untouched.
overlayPicker
    :: Rect -> PickerState -> Maybe (V.Vector (V.Vector Cell.Cell))
    -> Frame -> Frame
overlayPicker region pk mPreview frame = overlayGrid frame region grid
  where
    rows = region.endRow - region.startRow
    width = region.endCol - region.startCol
    rendered = Picker.pickerLines rows pk
    padded = take rows (rendered <> repeat (Picker.UnselectedRow, ""))
    -- Split only when a preview pane exists and the width allows it.
    split = case (mPreview, Picker.pickerSplit width) of
        (Just previewCells, Just listW) -> Just (listW, previewCells)
        _                               -> Nothing
    grid = V.fromList [ rowCells k | k <- [0 .. rows - 1] ]
    rowCells k =
        let (sel, txt) = padded !! k
            sty = case sel of
                Picker.SelectedRow   -> pickerSelStyle
                Picker.UnselectedRow -> pickerStyle
        in case split of
            Nothing -> lineCells sty width txt
            Just (listW, previewCells) ->
                lineCells sty listW txt
                    <> V.singleton dividerCell
                    <> previewRow previewCells k (width - listW - 1)

-- | Row @k@ of a preview pane's cells, padded or clipped to @w@ columns.
previewRow :: V.Vector (V.Vector Cell.Cell) -> Int -> Int -> V.Vector Cell.Cell
previewRow grid k w =
    let row = fromMaybe V.empty (grid V.!? k)
    in V.generate w (\c -> fromMaybe Cell.blankCell (row V.!? c))

dividerCell :: Cell.Cell
dividerCell = Cell.glyphCell '\x2502' pickerStyle

pickerStyle :: Cell.Style
pickerStyle = Cell.defaultStyle

pickerSelStyle :: Cell.Style
pickerSelStyle = Cell.defaultStyle { Cell.reverse = True }

paneOrigin :: [(PaneId, Rect)] -> PaneId -> Pos
paneOrigin rects pidL = case List.lookup pidL rects of
    Just r -> Pos { row = r.startRow, col = r.startCol }
    Nothing -> Pos 0 0

-- | The chrome around a client's current window — borders, pane status
-- lines and fill — through the client's render-side cache: the scene is
-- rebuilt only when its inputs change, and the chrome restamped only when
-- its cells differ, so an unchanged chrome keeps its 'Chrome' generation
-- and every row's chrome provenance stays matchable.
cachedChrome
    :: ServerState -> Expand -> Theme -> Client -> Session -> Window -> Size -> Size
    -> [(PaneId, Rect)] -> [FloatingPane] -> Map.Map PaneId (Cell.Color, Cell.Color)
    -> Map.Map PaneId Options -> PaneId -> Int -> Pos -> IO Chrome
cachedChrome st expand th client sess win eff sceneArea rects floats defaults paneOpts active rowOff origin = do
    mcc <- readIORef client.lastChrome
    mmenu <- readTVarIO win.menu
    windowStyle <- expand.style th . (.paneBorderStyle)
        =<< resolveForWindowIO st sess win
    (input, look2, panes) <- atomically
        (chromeInputs st windowStyle sess win eff sceneArea rects floats paneOpts active mmenu)
    bars <- forM [ (cp, w) | cp <- panes, Just w <- [Map.lookup cp.pane.id input.scrollbars] ] $
        \(cp, _) -> do
            let height = maybe 0 (\r -> r.endRow - r.startRow) (List.lookup cp.pane.id rects)
                (dfg, dbg) = Map.findWithDefault (Cell.DefaultColor, Cell.DefaultColor)
                    cp.pane.id defaults
            slider <- paneSlider cp.pane height
            pure (cp.pane.id, ScrollbarLook
                { style = scrollbarStyle th cp.options.paneScrollbarsStyle
                , slider
                , padStyle = Cell.defaultStyle { Cell.fg = dfg, Cell.bg = dbg }
                , onLeft = input.scrollbarSide == ScrollbarLeft })
    let look1 = look2 { scrollbarLooks = Map.fromList bars }
    let sameInput = maybe False (\cc -> cc.input == input) mcc
        (scene, kinds) = case mcc of
            Just cc | sameInput -> (cc.scene, cc.kinds)
            _ -> let s = buildScene input in (s, sceneKinds s)
    -- A scene with no borders draws no pane's border style; a status line
    -- is drawn over a border.
    styles <- if Set.disjoint kinds (Set.fromList [BorderChrome, StatusChrome])
        then pure []
        else forM panes $ \cp -> (,) cp.pane.id <$> paneStyleOption st expand th sess win cp.pane
            (if cp.pane.id == active then cp.options.paneActiveBorderStyle
             else cp.options.paneBorderStyle)
    let look0 = look1 { paneStyles = Map.fromList styles }
    look <- withStatusLines expand th sess win scene kinds panes look0
        >>= withFills st expand th sess win kinds
        >>= withMenu st expand th sess win mmenu
    case mcc of
        Just cc | sameInput, cc.look == look, cc.rowOff == rowOff, cc.origin == origin ->
            pure cc.chrome
        _ -> do
            let shown p = p.row >= 0 && p.row < fromIntegral sceneArea.rows
                    && p.col >= 0 && p.col < fromIntegral sceneArea.cols
                cells =
                    [ (q { row = q.row + rowOff }, c)
                    | (p, c) <- sceneChrome look scene
                    , let q = Pos { row = p.row - origin.row, col = p.col - origin.col }
                    , shown q ]
                gen
                    | Just cc <- mcc, cc.cells == cells = cc.chrome.gen
                    | otherwise = maybe 0 (\cc -> cc.chrome.gen) mcc + 1
                chrome = Chrome { gen, byRow = (chromeFromCells cells).byRow }
            writeIORef client.lastChrome $ Just ChromeCache
                { input, scene, kinds, look, rowOff, origin, cells, chrome }
            pure chrome

-- | A pane's view drawn in its default colours and its palette. A pane
-- with colours of its own gives up its row stamps: they do not cover the
-- colours.
withPaneColours
    :: (Cell.Color, Cell.Color) -> Map.Map Int Cell.Color -> PaneView -> PaneView
withPaneColours dflt palette pv
    | dflt == (Cell.DefaultColor, Cell.DefaultColor), Map.null palette = pv
    | otherwise = pv
        { cells = paintPalette palette (paintDefaults dflt pv.cells), rowGens = Nothing }

-- | Where a pane's scrollbar slider sits in a scrollbar @height@ rows
-- tall: over its history and screen, or its copy-mode viewport.
paneSlider :: Pane -> Int -> IO (Int, Int)
paneSlider pane height = do
    mmode <- readTVarIO pane.mode
    case mmode of
        Just pm -> pure $ sliderOf height pm.frozen.fgSy pm.frozen.fgHsize
            (Just (pm.frozen.fgHsize - pm.copyState.viewportOffY, pm.frozen.fgHsize))
        Nothing -> do
            sz <- readTVarIO pane.size
            hsize <- Emu.scrollbackLength pane.emulator
            pure (sliderOf height (fromIntegral sz.rows) hsize Nothing)

-- | One visible pane's part in its window's chrome.
data ChromePane = ChromePane
    { pane :: Pane
    , options :: Options
    , number :: Int
    }

-- | The scene a window's panes make and how its chrome looks, before any
-- pane status line is drawn.
chromeInputs
    :: ServerState -> Cell.Style -> Session -> Window -> Size -> Size -> [(PaneId, Rect)]
    -> [FloatingPane] -> Map.Map PaneId Options -> PaneId -> Maybe WindowMenu
    -> STM (SceneInput, ChromeLook, [ChromePane])
chromeInputs st windowStyle sess win eff sceneArea rects floats paneOpts active mmenu = do
    wopts <- resolveForWindow st sess win
    status <- windowPaneStatus st win
    (side, bars) <- paneScrollbars st win
    lay <- readTVar win.layout
    ps <- readTVar win.panes
    marked <- readTVar st.markedPane
    let order = paneOrder lay ps
        indicators = wopts.paneBorderIndicators
        colourSplit = indicators `elem` [IndicatorsColour, IndicatorsBoth]
        twoPane = if colourSplit then twoPaneSplit lay else Nothing
    panes <- fmap concat $ forM rects $ \(pid, _) -> case Map.lookup pid ps of
        Nothing -> pure []
        Just pane -> do
            -- The frame resolved its panes' options as it painted them.
            popts <- maybe (resolveForPane st sess win pane) pure (Map.lookup pid paneOpts)
            pure [ ChromePane
                { pane, options = popts
                , number = wopts.paneBaseIndex + fromMaybe 0 (List.elemIndex pid order) } ]
    let useArrows = indicators `elem` [IndicatorsArrows, IndicatorsBoth]
        input = SceneInput
            { window = eff, area = sceneArea, panes = tiledOf floats rects
            , paneStatus = status, twoPane, scrollbars = bars, scrollbarSide = side
            , floats, menu = menuRect <$> mmenu }
        look = ChromeLook
            { borderLines = wopts.paneBorderLines
            , lineSets = Map.fromList [ (f.pane, f.lineSet) | f <- floats ]
            , windowStyle
            , paneStyles = Map.empty
            , scrollbarLooks = Map.empty
            , active = Just active
            , marked
            , numbers = Map.fromList [ (cp.pane.id, cp.number) | cp <- panes ]
            , paneLines = Map.empty
            , fillInside = Cell.blankCell
            , fillOutside = Cell.blankCell
            , arrows = if useArrows
                then maybe Map.empty edgeArrows (List.lookup active rects)
                else Map.empty
            , menuCells = V.empty }
    pure (input, look, panes)
  where
    menuRect wm = Rect
        { startRow = wm.py, endRow = wm.py + menuHeight wm.menu
        , startCol = wm.px, endCol = wm.px + menuWidth wm.menu }

-- | The window's menu drawn, in its styles: @menu-style@,
-- @menu-selected-style@ and @menu-border-style@, re-read on every frame
-- (as tmux's @menu_reapply_styles@) and expanded in the menu's target's
-- context, under the colours its flags give.
withMenu
    :: ServerState -> Expand -> Theme -> Session -> Window -> Maybe WindowMenu -> ChromeLook
    -> IO ChromeLook
withMenu st expand th sess win mmenu look = case mmenu of
    Nothing -> pure look
    Just wm -> do
        (wix, wopts, ps) <- atomically $ (,,)
            <$> readTVar sess.currentIx <*> resolveForWindow st sess win <*> readTVar win.panes
        let (_, _, pid) = wm.target
        scope <- case Map.lookup pid ps of
            Just pane -> do
                pix <- paneIndexOf st win pane
                pure (InPane sess wix win pix pane)
            Nothing -> pure (InWindow sess wix win)
        let styleOf name = do
                let raw = optionText name wopts
                expanded <- if "#{" `T.isInfixOf` raw
                    then expand.format (FormatTarget scope Map.empty) raw
                    else pure raw
                expand.style th expanded
        sty <- styleOf "menu-style"
        sel <- styleOf "menu-selected-style"
        bdr <- styleOf "menu-border-style"
        let mlook = MenuLook
                { style = overrideColours th sty wm.style
                , selected = overrideColours th sel wm.selectedStyle
                , border = overrideColours th bdr wm.borderStyle }
        pure look { menuCells = menuGrid th mlook wm }

-- | The window's @fill-character@ cells, when the scene has room for them.
withFills
    :: ServerState -> Expand -> Theme -> Session -> Window -> Set ChromeKind -> ChromeLook
    -> IO ChromeLook
withFills st expand th sess win kinds look
    | FillChrome `Set.notMember` kinds = pure look
    | otherwise = do
        (wix, wopts) <- atomically $
            (,) <$> readTVar sess.currentIx <*> resolveForWindow st sess win
        let fill inside = fillCell th look.windowStyle <$> expand.format
                (FormatTarget (InWindow sess wix win) (Map.fromList
                    [ ("is_inside", flag inside), ("is_outside", flag (not inside)) ]))
                (optionText "fill-character" wopts)
            flag b = if b then "1" else "0"
        fillInside <- fill True
        fillOutside <- fill False
        pure look { fillInside, fillOutside }

-- | Draw each pane's @pane-border-format@ over the border its status line
-- replaces (tmux's @window_make_pane_status@).
withStatusLines
    :: Expand -> Theme -> Session -> Window -> Scene -> Set ChromeKind
    -> [ChromePane] -> ChromeLook -> IO ChromeLook
withStatusLines expand th sess win scene kinds panes look
    | StatusChrome `Set.notMember` kinds = pure look
    | otherwise = do
        wix <- atomically (readTVar sess.currentIx)
        lines' <- forM panes $ \cp -> do
            let sty = Map.findWithDefault look.windowStyle cp.pane.id look.paneStyles
                lineSet = Map.findWithDefault look.borderLines cp.pane.id look.lineSets
            case statusBase lineSet sty cp.number scene cp.pane.id of
                Nothing -> pure []
                Just base -> do
                    expanded <- expand.format
                        (FormatTarget (InPane sess wix win cp.number cp.pane) Map.empty)
                        cp.options.paneBorderFormat
                    pure [ (cp.pane.id, formatDraw th sty expanded base) ]
        pure look { paneLines = Map.fromList (concat lines') }

-- | The tiled panes' rects, in the order their scene marks them.
tiledOf :: [FloatingPane] -> [(PaneId, Rect)] -> [(PaneId, Rect)]
tiledOf floats rects = markingOrder [ r | r@(p, _) <- rects, p `notElem` map (.pane) floats ]

-- | The part of a floating pane's content the window shows, and where in
-- its content that part starts.
clipToWindow :: Size -> Rect -> (Rect, Pos)
clipToWindow area r =
    ( Rect { startRow = max 0 r.startRow, endRow = min (fromIntegral area.rows) r.endRow
           , startCol = max 0 r.startCol, endCol = min (fromIntegral area.cols) r.endCol }
    , Pos { row = max 0 (negate r.startRow), col = max 0 (negate r.startCol) } )

-- | A grid less its first @skip.row@ rows and @skip.col@ columns.
dropGrid :: Pos -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
dropGrid skip g
    | skip == Pos 0 0 = g
    | otherwise = V.map (V.drop skip.col) (V.drop skip.row g)

-- | An arrow at the midpoint of each of a pane's four border edges,
-- pointing inward.
edgeArrows :: Rect -> Map.Map Pos Char
edgeArrows r = Map.fromList $ concat
    [ vEdge (r.startCol - 1) '\x25b6'  -- ▶ on the left border
    , vEdge r.endCol         '\x25c0'  -- ◀ on the right border
    , hEdge (r.startRow - 1) '\x25bc'  -- ▼ on the top border
    , hEdge r.endRow         '\x25b2'  -- ▲ on the bottom border
    ]
  where
    vEdge col arr = case midOf [r.startRow .. r.endRow - 1] of
        Just row -> [(Pos { row = row, col = col }, arr)]
        Nothing  -> []
    hEdge row arr = case midOf [r.startCol .. r.endCol - 1] of
        Just col -> [(Pos { row = row, col = col }, arr)]
        Nothing  -> []

midOf :: [a] -> Maybe a
midOf xs = case drop (length xs `div` 2) xs of
    (x : _) -> Just x
    []      -> Nothing

-- | A pane's contribution to a frame, from ONE emulator snapshot: its cells
-- and its pane-local cursor. Normally the live screen; in copy mode, a
-- viewport over scrollback+screen with the selection reverse-videoed and the
-- copy cursor (absent when scrolled off the viewport).
data PaneView = PaneView
    { cells :: V.Vector (V.Vector Cell.Cell)
    , rowGens :: Maybe (V.Vector Int)
        -- ^ 'Emu.snapshotWithGens' stamps, one per row of 'cells';
        -- 'Nothing' when the cells are not a plain snapshot (copy mode)
    , cursor :: Maybe Pos
    , cursorVisible :: Bool
    }

paneView :: ServerState -> Pane -> IO PaneView
paneView st pane = do
    (mmode, clockTop) <- atomically $ (,) <$> readTVar pane.mode <*> paneClockOnTop pane
    case mmode of
        _ | clockTop -> do
            cells <- clockView st pane
            pure PaneView
                { cells, rowGens = Nothing, cursor = Nothing, cursorVisible = False }
        Nothing -> do
            (scr, gens) <- Emu.snapshotWithGens pane.emulator
            pure PaneView
                { cells = scr.cells
                , rowGens = Just gens
                , cursor = Just scr.cursor
                , cursorVisible = scr.cursorVisible
                }
        Just pm -> do
            look <- copyLook st pane
            let (cells, cur) = CopyMode.copyView look pm
            pure PaneView
                { cells
                , rowGens = Nothing
                , cursor = cur
                , cursorVisible = isJust cur
                }

-- | Clock mode's screen for a pane, in its options' colour and style.
clockView :: ServerState -> Pane -> IO (V.Vector (V.Vector Cell.Cell))
clockView st pane = do
    th <- clientTheme st
    global <- readTVarIO st.options
    opts <- maybe global id <$> paneOptionsNow st pane
    size <- readTVarIO pane.size
    now <- zonedTimeToLocalTime <$> getZonedTime
    let colour = fromMaybe Cell.DefaultColor
            (join (colourValue th (optionText "clock-mode-colour" opts)))
    pure (Clock.clockCells colour (Clock.clockStyleOf (optionText "clock-mode-style" opts)) size now)

-- | How copy mode draws in a pane: its options' styles and position format,
-- formats expanded in the pane's context as tmux's @window_copy_write_line@
-- does.
copyLook :: ServerState -> Pane -> IO CopyMode.CopyLook
copyLook st pane = do
    th <- clientTheme st
    global <- readTVarIO st.options
    mhome <- atomically $ do
        cached <- readTVar pane.home  -- See Note [Pane home]
        home <- maybe (locatePane st pane.id) (pure . Just) cached
        sessions <- readTVar st.sessions
        case home of
            Just (sid, win) | Just sess <- Map.lookup sid sessions -> do
                popts <- resolveForPane st sess win pane
                wix <- readTVar sess.currentIx
                pure (Just (sess, win, popts, wix))
            _ -> pure Nothing
    (opts, expand) <- case mhome of
        Nothing -> pure (global, pure)
        Just (sess, win, popts, wix) -> do
            pix <- paneIndexOf st win pane
            pure (popts, expandAt st (FormatTarget (InPane sess wix win pix pane) Map.empty))
    let raw name = optionText name opts
        style name
            | "#{" `T.isInfixOf` raw name = styleCell th <$> expand (raw name)
            | otherwise = pure (styleCell th (raw name))
    selection <- style "copy-mode-selection-style"
    match <- style "copy-mode-match-style"
    current <- style "copy-mode-current-match-style"
    markSty <- style "copy-mode-mark-style"
    line <- style "copy-mode-current-line-style"
    number <- style "copy-mode-line-number-style"
    currentNumber <- style "copy-mode-current-line-number-style"
    positionSty <- style "copy-mode-position-style"
    position <- expand (raw "copy-mode-position-format")
    mmode <- readTVarIO pane.mode
    pure CopyMode.CopyLook
        { keys = opts.modeKeys
        , numbers = CopyMode.lineNumberModeOf (raw "copy-mode-line-numbers")
        , selectionStyle = selection
        , matchStyle = match
        , currentMatchStyle = current
        , markStyle = markSty
        , currentLineStyle =
            if raw "copy-mode-current-line-style" == "default" then Nothing else Just line
        , lineNumberStyle = number
        , currentLineNumberStyle = currentNumber
        , drawPosition = \area ->
            if T.null position then area else formatDraw th positionSty position area
        , searchPattern = mmode >>= CopyMode.lastPattern . (.copyState)
        }

-- | The cells a pane contributes, discarding the cursor: for previews and
-- the chooser, which draw no cursor.
paneViewCells :: ServerState -> Pane -> IO (V.Vector (V.Vector Cell.Cell))
paneViewCells st pane = (.cells) <$> paneView st pane

-- | Place a pane's local cursor into screen coordinates, or hide it when the
-- pane has none (a copy viewport scrolled past the cursor).
placePaneCursor :: Pos -> Int -> PaneView -> (Pos, Bool)
placePaneCursor origin rowOff pv = case pv.cursor of
    Nothing -> (Pos 0 0, False)
    Just p -> (place p, pv.cursorVisible)
  where
    place p = Pos { row = p.row + origin.row + rowOff
                  , col = p.col + origin.col }

-- Status line -------------------------------------------------------------

lineCells :: Cell.Style -> Int -> Text -> V.Vector Cell.Cell
lineCells style width line = V.fromList (take width (cells <> repeat blank))
  where
    cells = [ Cell.glyphCell ch style | ch <- T.unpack line ]
    blank = Cell.glyphCell ' ' style

-- | The context a message or the prompt expands its formats in: the
-- session's, as tmux's @format_create_defaults@ for the client.
messageCtx :: ServerState -> Session -> IO F.FormatCtx
messageCtx st sess = do
    env <- sessionFormatEnv st sess
    formatCtxFor st env

-- | What the prompt draws: see 'drawPrompt'.
promptView :: PromptState -> PromptView
promptView pr = PromptView
    { label = pr.promptLabel, input = pr.input, cursor = pr.cursor
    , commandMode = pr.promptMode == PromptCommand }

-- | The status line's rows for a session at @width@ cells (see
-- 'Hat.Server.Status.statusRows').
statusCells :: ServerState -> Session -> Int -> IO [V.Vector Cell.Cell]
statusCells st sess width = do
    sopts <- resolveForSessionIO st sess
    (env, winEnvs) <- gatherStatus st sess
    renderStatus st sopts env winEnvs width

-- | 'statusCells' through the client's render-side cache: the rows are
-- re-expanded only when something their expansion reads changed. The
-- cache holds every variable its formats can read, with the session's
-- and each window's values of them, and what they read of the clock
-- ('formatClock'); a frame rebuilds just those, and only a miss reads the
-- windows' full variables and expands. Bypassed inside a hook's ambient
-- context, and for formats that read the live tree beyond the window list
-- ('formatReadsLoops'), where the inputs cannot be enumerated.
statusRowsCached
    :: ServerState -> Client -> Session -> Options -> Int
    -> IO [V.Vector Cell.Cell]
statusRowsCached st client sess sopts width = do
    tid <- myThreadId
    mamb <- Map.lookup tid <$> readTVarIO st.hooks.ambient
    shells <- readTVarIO st.shellCache
    now <- getPOSIXTime
    zone <- getCurrentTimeZone
    ws <- Map.toAscList <$> readTVarIO sess.windows
    let ownVars w = mapM (\(ix, win) -> windowOwnVars st sess ix win w) ws
    mc <- readIORef client.lastStatus
    hit <- case mc of
        Just c | isNothing mamb, c.plain
               , samePtr c.opts sopts, samePtr c.shells shells, c.width == width
               , fmap (clockTick zone now) c.clockPeriod == c.clock -> do
            vars <- sessionVars st sess (OnlyVars c.ownNames)
            wins <- if vars == c.vars then ownVars (OnlyVars c.names) else pure []
            pure (if vars == c.vars && wins == c.wins then Just c.rowCells else Nothing)
        _ -> pure Nothing
    case hit of
        Just rows -> pure rows
        Nothing -> do
            winEnvs <- ownVars AllVars
            let formats = optionText "status-style" sopts : statusFormats sopts
                    <> [ v | e <- winEnvs, v <- Map.elems e, "#" `T.isInfixOf` v ]
            (names, vars, texts) <- demanding st (sessionVars st sess) formats $ \ctx -> do
                let texts = statusTexts
                        ctx { F.windows = winEnvs, F.animate = F.Animate } sopts
                _ <- evaluate texts.style
                mapM_ (mapM_ evaluate) texts.lines
                pure texts
            th <- clientTheme st
            let rows = drawStatus th sopts texts width
                values = Map.toList vars <> concatMap Map.toList winEnvs
                reached = formats <> [ v | (_, v) <- values, "#" `T.isInfixOf` v ]
                strftimed = foldMap formatStrftimes reached
                readsAny toks = any (formatReadsLoops toks) reached
                plain = not (readsAny ["S", "P", "L", "N", "C", "A"])
                animated = readsAny ["A"]
                clockPeriod = formatClock
                    (reached <> [ v | (n, v) <- values, n `Set.member` strftimed ])
                -- An option's value stands while the options do.
                ownNames = Set.filter (\n -> isNothing (optionFormat n sopts)) names
            when (isNothing mamb) $ writeIORef client.lastStatus $
                Just StatusCache
                    { opts = sopts, shells, clockPeriod
                    , clock = clockTick zone now <$> clockPeriod, width
                    , names = wantedNames names, ownNames = wantedNames ownNames
                    , vars = Map.restrictKeys vars ownNames
                    , wins = map (`Map.restrictKeys` names) winEnvs
                    , plain, animated, rowCells = rows }
            pure $! foldr seq rows rows

-- | Everything the status rows' expansion consumes beyond the options,
-- read fresh: the session's format environment and each window's own
-- (the @W:@ loop's items).
gatherStatus :: ServerState -> Session -> IO (Map.Map Text Text, [Map.Map Text Text])
gatherStatus st sess = do
    env <- sessionFormatEnv st sess
    ws <- Map.toAscList <$> readTVarIO sess.windows
    wins <- mapM (\(ix, win) -> windowOwnEnv st sess ix win) ws
    pure (env, wins)

-- | Expand and draw the status rows from gathered inputs; see
-- 'statusCells'.
renderStatus
    :: ServerState -> Options -> Map.Map Text Text -> [Map.Map Text Text]
    -> Int -> IO [V.Vector Cell.Cell]
renderStatus st sopts env wins width = do
    ctx <- formatCtxFor st env
    let rows = statusRows ctx { F.windows = wins, F.animate = F.Animate }
            sopts width
    pure $! foldr seq rows rows

-- | The ranges across status line @i@ as the client sees it, for a mouse
-- click there.
statusRangesFor :: ServerState -> Session -> Options -> Int -> Int -> IO (V.Vector DrawRange)
statusRangesFor st sess sopts width i = do
    (env, winEnvs) <- gatherStatus st sess
    ctx <- formatCtxFor st env
    pure (statusRanges ctx { F.windows = winEnvs } sopts width i)

-- | The status line (0-based) the prompt and messages take: @message-line@,
-- or the last line when there are fewer.
messageLine :: Options -> Int
messageLine sopts = max 0 (min sopts.messageLine (sopts.statusLines - 1))
