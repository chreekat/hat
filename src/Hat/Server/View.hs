-- | The per-client rendering and presentation layer: turns the model
-- tree into frames, the status line, and the chooser\/copy-mode overlays.
module Hat.Server.View
    ( windowArrange
    , renderLoop
    , renderOnce
    , sessionFormatEnv
    , paneModeEnv
    , resolveShell
    , expandFormat
    , WindowFlagState (..)
    , windowFlags
    , statusCells
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (foldM, forM, void, when)
import Data.IORef
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Time.LocalTime (getZonedTime)
import qualified Data.Vector as V
import System.Exit (ExitCode (..))
import System.Posix.Unistd (SystemID (nodeName), getSystemID)
import System.Process
    (CreateProcess (..), readCreateProcessWithExitCode, shell)

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import qualified Hat.Server.CopyMode as CopyMode
import Hat.Server.ClientIO (send)
import Hat.Server.ColorScheme (schemeName)
import Hat.Server.Format (FormatEnv, renderFormat)
import Hat.Server.Layout (arrange, sizeRect)
import qualified Hat.Server.Picker as Picker
import Hat.Server.Render
import qualified Hat.Term.Cell as Cell
import qualified Hat.Term.Emulator as Emu
import Hat.Transport.Wire

-- Pane rects and borders for a window, honoring zoom.
windowArrange :: Size -> Window -> STM ([(PaneId, Rect)], [(Pos, Char)])
windowArrange eff win = do
    mz <- readTVar win.zoomed
    lay <- readTVar win.layout
    ps <- readTVar win.panes
    pure $ case mz of
        Just zpid | Map.member zpid ps -> ([(zpid, sizeRect eff)], [])
        _ -> arrange (sizeRect eff) lay

-- Rendering ---------------------------------------------------------------

renderLoop :: ServerState -> Client -> IO ()
renderLoop st client = loop (-1)
  where
    loop lastGen = do
        gen <- atomically $ do
            g <- readTVar st.dirty
            full <- readTVar client.needsFull
            check (g /= lastGen || full)
            pure g
        renderOnce st client
        loop gen

renderOnce :: ServerState -> Client -> IO ()
renderOnce st client = do
    csize <- readTVarIO client.size
    opts <- readTVarIO st.options
    let rowOff = case opts.statusPosition of
            StatusTop -> 1
            StatusBottom -> 0
        statusRowIx = case opts.statusPosition of
            StatusTop -> 0
            StatusBottom -> fromIntegral csize.rows - 1
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
                        (rects, borders) <- windowArrange (windowArea eff) win
                        ps <- readTVar win.panes
                        active <- readTVar win.activeId
                        pure (Just (sess, rects, borders, ps, active))
    (frame, cursor, mActiveRect) <- case view of
        Nothing -> pure (blankFrame csize, (Pos 0 0, False), Nothing)
        Just (sess, rects, borders, ps, active) -> do
            let shiftRect r = r
                    { startRow = r.startRow + rowOff
                    , endRow = r.endRow + rowOff
                    }
                base0 = applyBorders (blankFrame csize)
                    (borderCells opts (List.lookup active rects) rowOff borders)
            base <- foldM' base0 rects $ \acc (pidL, rect) ->
                case Map.lookup pidL ps of
                    Nothing -> pure acc
                    Just pane -> do
                        cells <- paneViewCells st pane
                        pure (overlayGrid acc (shiftRect rect) cells)
            mprompt <- readTVarIO client.prompt
            mtoast <- readTVarIO client.toast
            statusRow <- case (mprompt, mtoast) of
                (Just pr, _) -> pure (promptCells pr (fromIntegral csize.cols))
                (Nothing, Just t) -> pure (toastCells t (fromIntegral csize.cols))
                (Nothing, Nothing) -> statusCells st sess (fromIntegral csize.cols)
            let withStatus
                    | csize.rows >= 2 = base V.// [(statusRowIx, statusRow)]
                    | otherwise = base
            cur <- case mprompt of
                Just pr | csize.rows >= 2 ->
                    pure (Pos { row = statusRowIx
                              , col = min (fromIntegral csize.cols - 1)
                                          (promptCursorCol pr) }, True)
                _ -> case Map.lookup active ps of
                    Nothing -> pure (Pos 0 0, False)
                    Just pane -> do
                        let origin = paneOrigin rects active
                        paneCursor pane origin rowOff
            pure (withStatus, cur, shiftRect <$> List.lookup active rects)
    -- A chooser overlay, when open, is drawn in the active pane's rect
    -- (or the whole window under -Z), with a live preview of the
    -- highlighted node's pane beside the list.
    mpicker <- readTVarIO client.picker
    (frame', cursor') <- case mpicker of
        Nothing -> pure (frame, cursor)
        Just pk -> do
            let region = Picker.pickerRegion pk.fill csize rowOff mActiveRect
                width = region.endCol - region.startCol
                rows = region.endRow - region.startRow
            mPreview <- case Picker.pickerSplit width of
                Just listW -> pickerPreviewCells st pk Size
                    { rows = fromIntegral (max 0 rows)
                    , cols = fromIntegral (max 0 (width - listW - 1)) }
                Nothing -> pure Nothing
            pure (overlayPicker region pk mPreview frame, (Pos 0 0, False))
    full <- atomically (swapTVar client.needsFull False)
    old <- readIORef client.lastFrame
    oldCursor <- readIORef client.lastCursor
    let ops = if full then fullRedraw frame' else diffFrame old frame'
        cursorOp = CursorAt (fst cursor') (snd cursor')
        needSend = not (null ops) || cursor' /= oldCursor || full
    writeIORef client.lastFrame frame'
    writeIORef client.lastCursor cursor'
    when needSend $ send client (Draw (ops <> [cursorOp]))
  where
    foldM' z xs f = foldM f z xs

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
    (rects, borders, ps, active) <- atomically $ do
        (r, b) <- windowArrange size win
        ps     <- readTVar win.panes
        a      <- readTVar win.activeId
        pure (r, b, ps, a)
    let base = applyBorders (blankFrame size)
            (borderCells opts (List.lookup active rects) 0 borders)
    foldM (\acc (pid, rect) -> case Map.lookup pid ps of
              Nothing   -> pure acc
              Just pane -> do
                  cells <- paneViewCells st pane
                  pure (overlayGrid acc rect cells)) base rects

-- | Preview a session as a vertical stack of its windows: each window's
-- index\/name on a label row, then a thumbnail of that window's composited
-- split layout. Windows that do not fit are omitted (the list still names
-- them).
sessionPreviewCells
    :: ServerState -> Session -> Size -> IO (V.Vector (V.Vector Cell.Cell))
sessionPreviewCells st sess size = do
    wins <- Map.toAscList <$> readTVarIO sess.windows
    let slices = Picker.stackThumbnails (fromIntegral size.rows) (length wins)
    foldM place (blankFrame size) (zip wins slices)
  where
    place acc ((ix, win), (labelRow, bodyTop, bodyH)) = do
        wname <- readTVarIO win.name
        thumb <- windowCompositeCells st win
            (Size { rows = fromIntegral bodyH, cols = size.cols })
        let width = fromIntegral size.cols
            label = lineCells pickerStyle width (tshow ix <> ":" <> wname)
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
dividerCell = Cell.Cell { Cell.text = "\x2502", Cell.width = 1, Cell.style = pickerStyle }

pickerStyle :: Cell.Style
pickerStyle = Cell.defaultStyle

pickerSelStyle :: Cell.Style
pickerSelStyle = Cell.defaultStyle { Cell.reverse = True }

paneOrigin :: [(PaneId, Rect)] -> PaneId -> Pos
paneOrigin rects pidL = case List.lookup pidL rects of
    Just r -> Pos { row = r.startRow, col = r.startCol }
    Nothing -> Pos 0 0

-- | Turn @arrange@'s raw border glyphs into styled cells: the active
-- pane's border takes @pane-active-border-style@ (when
-- @pane-border-indicators@ colours it) and gains direction arrows (when
-- it uses arrows); everything else takes @pane-border-style@. Glyphs are
-- remapped per @pane-border-lines@. Positions are shifted by @rowOff@ for
-- a top status line.
borderCells
    :: Options -> Maybe Rect -> Int -> [(Pos, Char)] -> [(Pos, Cell.Cell)]
borderCells opts mActive rowOff borders =
    [ (p { row = p.row + rowOff }, cellAt p ch) | (p, ch) <- borders ]
  where
    (useColor, useArrows) = case opts.paneBorderIndicators of
        IndicatorsOff     -> (False, False)
        IndicatorsColour  -> (True, False)
        IndicatorsArrows  -> (False, True)
        IndicatorsBoth    -> (True, True)
    activeAt p = maybe False (`onPerimeter` p) mActive
    arrows = if useArrows then maybe Map.empty edgeArrows mActive else Map.empty
    cellAt p ch =
        let active = activeAt p
            sty | active && useColor = opts.paneActiveBorderStyle
                | otherwise          = opts.paneBorderStyle
            glyph = case (active, Map.lookup p arrows) of
                (True, Just arr) -> arr
                _                -> mapGlyph opts.paneBorderLines ch
        in Cell.Cell { Cell.text = T.singleton glyph, Cell.width = 1, Cell.style = sty }

-- | Is a position on the (border) perimeter just outside a pane's rect?
onPerimeter :: Rect -> Pos -> Bool
onPerimeter r p =
    ((p.col == r.endCol || p.col == r.startCol - 1)
        && p.row >= r.startRow && p.row < r.endRow)
    || ((p.row == r.endRow || p.row == r.startRow - 1)
        && p.col >= r.startCol && p.col < r.endCol)

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

-- | Remap the single-line glyphs @arrange@ emits to the chosen line set.
mapGlyph :: BorderLines -> Char -> Char
mapGlyph bl ch = case bl of
    BorderSingle -> ch
    BorderHeavy  -> heavy ch
    BorderDouble -> dbl ch
    BorderSimple -> simple ch
  where
    -- Straight runs and every junction the layout can emit (see
    -- 'Hat.Server.Layout.junction': │ ─ ┼ ┤ ├ ┬ ┴).
    heavy '\x2502' = '\x2503'; heavy '\x2500' = '\x2501'
    heavy '\x253c' = '\x254b'; heavy '\x2524' = '\x252b'
    heavy '\x251c' = '\x2523'; heavy '\x252c' = '\x2533'
    heavy '\x2534' = '\x253b'; heavy c = c
    dbl '\x2502' = '\x2551'; dbl '\x2500' = '\x2550'
    dbl '\x253c' = '\x256c'; dbl '\x2524' = '\x2563'
    dbl '\x251c' = '\x2560'; dbl '\x252c' = '\x2566'
    dbl '\x2534' = '\x2569'; dbl c = c
    simple '\x2502' = '|'; simple '\x2500' = '-'
    simple '\x253c' = '+'; simple '\x2524' = '+'; simple '\x251c' = '+'
    simple '\x252c' = '+'; simple '\x2534' = '+'; simple c = c

-- | The cells a pane contributes to a frame. Normally its live screen;
-- in copy mode, a viewport over scrollback+screen with the selection
-- reverse-videoed.
paneViewCells :: ServerState -> Pane -> IO (V.Vector (V.Vector Cell.Cell))
paneViewCells st pane = do
    mmode <- readTVarIO pane.mode
    case mmode of
        Nothing -> (.cells) <$> Emu.snapshot pane.emulator
        Just pm -> do
            opts <- readTVarIO st.options
            let s = pm.copyState
                fg = pm.frozen
                hsize = fg.fgHsize
                top = hsize - s.viewportOffY
                rows = [ viewportRow fg (top + i) | i <- [0 .. fg.fgSy - 1] ]
                overlaid = CopyMode.overlaySelection opts.modeStyle opts.modeKeys top s
                    (V.fromList rows)
                label = "[" <> tshow s.viewportOffY <> "/" <> tshow hsize <> "]"
            pure (stampTopRight label copyIndicatorStyle overlaid)
  where
    viewportRow fg a =
        let row = fromMaybe V.empty (fg.fgRows V.!? a)
        in V.generate fg.fgSx (\c -> fromMaybe Cell.blankCell (row V.!? c))

-- | tmux's copy-mode position indicator: black on yellow, like the
-- default @mode-style@.
copyIndicatorStyle :: Cell.Style
copyIndicatorStyle = Cell.defaultStyle
    { Cell.fg = Cell.Indexed 0, Cell.bg = Cell.Indexed 3 }

-- | Overlay a label onto the top-right corner of a grid (e.g. the
-- @[scroll/history]@ copy-mode indicator), clipped to the first row.
stampTopRight
    :: Text -> Cell.Style
    -> V.Vector (V.Vector Cell.Cell) -> V.Vector (V.Vector Cell.Cell)
stampTopRight label sty grid
    | V.null grid = grid
    | otherwise = grid V.// [(0, row0 V.// updates)]
  where
    row0 = grid V.! 0
    w = V.length row0
    start = max 0 (w - T.length label)
    updates =
        [ (start + i, cell c)
        | (i, c) <- zip [0 ..] (T.unpack label), start + i < w ]
    cell c = Cell.Cell { Cell.text = T.singleton c, Cell.width = 1, Cell.style = sty }

-- | The cursor a pane shows: its shell cursor, or the copy cursor when
-- in copy mode (hidden when scrolled off the viewport).
paneCursor :: Pane -> Pos -> Int -> IO (Pos, Bool)
paneCursor pane origin rowOff = do
    mmode <- readTVarIO pane.mode
    case mmode of
        Nothing -> do
            scr <- Emu.snapshot pane.emulator
            pure (place scr.cursor, scr.cursorVisible)
        Just pm -> do
            let s = pm.copyState
                top = pm.frozen.fgHsize - s.viewportOffY
            case CopyMode.copyCursorPos top pm.frozen.fgSy s of
                Just p -> pure (place p, True)
                Nothing -> pure (Pos 0 0, False)
  where
    place p = Pos { row = p.row + origin.row + rowOff
                  , col = p.col + origin.col }

-- Status line -------------------------------------------------------------

lineCells :: Cell.Style -> Int -> Text -> V.Vector Cell.Cell
lineCells style width line = V.fromList (take width (cells <> repeat blank))
  where
    cells = [ Cell.Cell { Cell.text = T.singleton ch
                        , Cell.width = 1
                        , Cell.style = style }
            | ch <- T.unpack line ]
    blank = Cell.Cell { Cell.text = " ", Cell.width = 1, Cell.style = style }

toastCells :: Text -> Int -> V.Vector Cell.Cell
toastCells t width = lineCells toastStyle width t
  where
    toastStyle = Cell.defaultStyle
        { Cell.fg = Cell.Indexed 0, Cell.bg = Cell.Indexed 3 }

-- | The command-prompt status row: the @:@ prefix followed by the line
-- being edited, styled like tmux's message line.
promptCells :: PromptState -> Int -> V.Vector Cell.Cell
promptCells pr width = lineCells promptStyle width (pr.promptLabel <> pr.input)
  where
    promptStyle = Cell.defaultStyle
        { Cell.fg = Cell.Indexed 0, Cell.bg = Cell.Indexed 3 }

-- | The screen column of the prompt's edit cursor.
promptCursorCol :: PromptState -> Int
promptCursorCol pr = T.length pr.promptLabel + pr.cursor

-- Session-level format environment for the active window and pane.
sessionFormatEnv :: ServerState -> Session -> IO FormatEnv
sessionFormatEnv st sess = do
    hostname <- nodeName <$> getSystemID
    (sname, wEnv, mactive, nclients, nwindows) <- atomically $ do
        sname <- readTVar sess.name
        mwin <- currentWindow sess
        cur <- readTVar sess.currentIx
        nwindows <- Map.size <$> readTVar sess.windows
        wEnv <- case mwin of
            Nothing -> pure []
            Just win -> do
                wname <- readTVar win.name
                pure [ ("window_index", tshow cur)
                     , ("window_name", wname)
                     ]
        mactive <- maybe (pure Nothing) activePane mwin
        cs <- sessionClients st sess.id
        pure (sname, wEnv, mactive, length cs, nwindows)
    pEnv <- case mactive of
        Nothing -> pure []
        Just pane -> do
            dir <- paneCurrentPath pane
            title <- Emu.title pane.emulator
            modeEnv <- paneModeEnv pane
            pure $ [ ("pane_current_path", T.pack dir)
                   , ("pane_title", title)
                   , ("pane_id", "%" <> tshow (rawPane pane.id))
                   ] <> modeEnv
    sz <- readTVarIO sess.lastSize
    -- @-options are readable as #{@foo}, so if-shell theme conditionals
    -- (@#{@pane-theme}@) resolve.
    userOpts <- (.user) <$> readTVarIO st.options
    msch <- readTVarIO st.colorScheme
    pure . Map.union userOpts . Map.fromList $
        [ ("session_name", sname)
        , ("session_attached", tshow nclients)
        , ("session_windows", tshow nwindows)
        , ("host", T.pack hostname)
        , ("window_active_clients", tshow nclients)
        , ("window_width", tshow sz.cols)
        , ("window_height", tshow sz.rows)
        , ("color_scheme", maybe "" schemeName msch)
        ]
        <> wEnv <> pEnv

-- | Copy-mode format variables for a pane: @pane_in_mode@/@pane_mode@,
-- plus @copy_cursor_{x,y,line}@ while in mode.
paneModeEnv :: Pane -> IO [(Text, Text)]
paneModeEnv pane = do
    mmode <- readTVarIO pane.mode
    case mmode of
        Nothing -> pure [("pane_in_mode", "0"), ("pane_mode", "")]
        Just pm -> do
            let s = pm.copyState
                top = pm.frozen.fgHsize - s.viewportOffY
            pure [ ("pane_in_mode", "1")
                 , ("pane_mode", "copy-mode")
                 , ("copy_cursor_x", tshow s.cursorCol)
                 , ("copy_cursor_y", tshow (s.cursorRow - top))
                 , ("copy_cursor_line", tshow s.cursorRow)
                 ]

-- Resolve #(cmd) through a 15-second cache; refreshes happen in the
-- background so the status line never blocks on a slow script.
resolveShell :: ServerState -> Text -> IO Text
resolveShell st cmdText = do
    now <- getCurrentTime
    cache <- readTVarIO st.shellCache
    case Map.lookup cmdText cache of
        Just (at, val)
            | diffUTCTime now at < 15 -> pure val
            | otherwise -> refresh now val
        Nothing -> refresh now ""
  where
    refresh now oldVal = do
        -- Optimistically bump the timestamp so only one refresh runs.
        atomically $ modifyTVar' st.shellCache
            (Map.insert cmdText (now, oldVal))
        void . forkIO $ do
            r <- try (readCreateProcessWithExitCode
                (shell (T.unpack cmdText)) { close_fds = True } "")
            let val = case r of
                    Right (ExitSuccess, out, _) ->
                        T.strip (T.takeWhile (/= '\n') (T.pack out))
                    Right (ExitFailure _, _, _) -> ""
                    Left (_ :: SomeException) -> ""
            done <- getCurrentTime
            atomically $ modifyTVar' st.shellCache
                (Map.insert cmdText (done, val))
            atomically (bumpDirty st)
        pure oldVal

-- | Expand a format string fully: #{...}, cached #(...), then strftime.
expandFormat :: ServerState -> FormatEnv -> Text -> IO Text
expandFormat st env fmt = do
    -- Pre-resolve shell segments so `evaluate` stays pure.
    resolved <- newIORef Map.empty
    let collect t = case T.breakOn "#(" t of
            (_, rest) | T.null rest -> pure ()
            (_, rest) -> do
                let inner = fst (breakBalanced (T.drop 2 rest))
                val <- resolveShell st inner
                modifyIORef' resolved (Map.insert inner val)
                collect (T.drop (2 + T.length inner + 1) rest)
    collect fmt
    vals <- readIORef resolved
    now <- getZonedTime
    pure (renderFormat env (\c -> Map.findWithDefault "" c vals) now fmt)
  where
    breakBalanced = go (0 :: Int) ""
      where
        go depth acc t = case T.uncons t of
            Nothing -> (acc, "")
            Just (')', rest) | depth == 0 -> (acc, rest)
            Just (c, rest)
                | c == '(' -> go (depth + 1) (acc <> T.singleton c) rest
                | c == ')' -> go (depth - 1) (acc <> T.singleton c) rest
                | otherwise -> go depth (acc <> T.singleton c) rest

-- | The conditions that produce a window's @#{window_flags}@ string.
data WindowFlagState = WindowFlagState
    { flagCurrent  :: Bool
    , flagLast     :: Bool
    , flagBell     :: Bool
    , flagActivity :: Bool
    , flagZoomed   :: Bool
    }

-- | Render the window-status flags in tmux's order: current (@*@) or
-- last (@-@), then bell (@!@) and activity (@#@), and finally zoom
-- (@Z@) when the window has a pane zoomed to fill it.
windowFlags :: WindowFlagState -> Text
windowFlags s = T.concat
    [ if s.flagCurrent then "*"
      else if s.flagLast then "-" else ""
    , if s.flagBell then "!" else ""
    , if s.flagActivity then "#" else ""
    , if s.flagZoomed then "Z" else ""
    ]

statusCells :: ServerState -> Session -> Int -> IO (V.Vector Cell.Cell)
statusCells st sess width = do
    opts <- readTVarIO st.options
    env <- sessionFormatEnv st sess
    let leftFmt = opts.statusLeft
        rightFmt = opts.statusRight
        winFmt = opts.windowStatusFormat
        winCurFmt = opts.windowStatusCurrentFormat
    entries <- do
        ws <- readTVarIO sess.windows
        cur <- readTVarIO sess.currentIx
        mlast <- readTVarIO sess.lastIx
        clientCount <- length <$> atomically (sessionClients st sess.id)
        forM (Map.toAscList ws) $ \(ix, win) -> do
            (wname, bell, act, zoom) <- atomically $ (,,,)
                <$> readTVar win.name <*> readTVar win.bellFlag
                <*> readTVar win.activity <*> readTVar win.zoomed
            let flags = windowFlags WindowFlagState
                    { flagCurrent = ix == cur
                    , flagLast = Just ix == mlast
                    , flagBell = bell
                    , flagActivity = act
                    , flagZoomed = isJust zoom
                    }
                -- A session's clients all view its current window, so only
                -- that window has active clients; the rest have none.
                activeClients = if ix == cur then clientCount else 0
                wenv = Map.union (Map.fromList
                    [ ("window_index", tshow ix)
                    , ("window_name", wname)
                    , ("window_flags", flags)
                    , ("window_active_clients", tshow activeClients)
                    ]) env
                fmt = if ix == cur then winCurFmt else winFmt
                style | ix == cur = opts.windowStatusCurrentStyle
                      | bell      = opts.windowStatusBellStyle
                      | otherwise = opts.windowStatusStyle
            txt <- expandFormat st wenv fmt
            pure (txt, style)
    left <- T.take opts.statusLeftLength <$> expandFormat st env leftFmt
    right <- T.take opts.statusRightLength <$> expandFormat st env rightFmt
    let sty = opts.statusStyle
        blank = blankOf sty
        sep = styledCells sty " "
        leftCells = styledCells sty left
        rightCells = styledCells sty right
        entryCells =
            List.intercalate sep [ styledCells est etxt | (etxt, est) <- entries ]
        body = leftCells <> entryCells
        pad = width - length body - length rightCells
        cells
            | pad >= 0 = body <> replicate pad blank <> rightCells
            | otherwise = take width (body <> sep <> rightCells)
    pure (V.fromList (take width (cells <> repeat blank)))

-- | One cell per character, all in the given style.
styledCells :: Cell.Style -> Text -> [Cell.Cell]
styledCells sty t =
    [ Cell.Cell { Cell.text = T.singleton c, Cell.width = 1, Cell.style = sty }
    | c <- T.unpack t ]

blankOf :: Cell.Style -> Cell.Cell
blankOf sty = Cell.Cell { Cell.text = " ", Cell.width = 1, Cell.style = sty }
