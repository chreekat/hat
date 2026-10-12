-- | Building the @#{…}@ format environment — session, window, and pane —
-- and expanding formats against it: the shared substrate the status line,
-- listing, choose, capture, and display commands format against.
module Hat.Server.FormatEnv
    ( windowFormatEnv
    , windowOwnEnv
    , paneFormatEnv
    , paneEnvById
    , refreshAutoNames
    , autoName
    , activeClientCounts
    , sessionFormatEnv
    , clientContextFormatEnv
    , paneModeEnv
    , resolveShell
    , expandFormat
    , formatCtxFor
    , clientTheme
    , reachableValues
    , WindowFlagState (..)
    , windowFlags
    , clientFormatVars
    ) where

import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM, forM_, void, when)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, isNothing, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Time.LocalTime (getZonedTime)
import System.Exit (ExitCode (..))
import System.Posix.Process (getProcessID)
import System.Posix.Unistd (SystemID (nodeName), getSystemID)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import System.Process
    (CreateProcess (..), readCreateProcessWithExitCode, shell)
import Data.Vector qualified as V

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.ColorScheme (schemeName)
import Hat.Server.Format
    (FormatCtx (..), FormatEnv, evaluateCtx, formatCtx, renderFormatCtx)
import Hat.Server.HookTypes (HookAmbient (..), HooksState (..))
import Hat.Server.Floating (FloatState (..), modalPane, paneZ, stacking)
import Hat.Server.Layout
import Hat.Server.LayoutString (emitLayout, emitLayoutZ)
import Hat.Server.Locate (locatePane, paneIndexOf)
import Hat.Server.ControlTypes (ControlFlags (..), ControlState (..), OutputMode (..))
import Hat.Server.Resize (unzoomedArrange, windowArea)
import Hat.Server.CopyMode qualified as CopyMode
import Hat.Server.FormatDraw (styleCell)
import Hat.Server.Render (paneDefaults)
import Hat.Server.Theme (Theme, clientColours, resolveTheme)
import Hat.Transport.Wire (ExitWait (..))
import Hat.Server.Winlink (windowLinks)
import Hat.Term.Cell qualified as Cell
import Hat.Term.Emulator qualified as Emu
import Hat.Term.Pty qualified

windowFormatEnv :: ServerState -> Session -> Int -> Window -> IO FormatEnv
windowFormatEnv st sess ix win =
    Map.union <$> windowOwnEnv st sess ix win <*> sessionFormatEnv st sess

-- | A window's own variables (and its window options), without its
-- session's: what a @W:@ loop item adds over the session's environment.
windowOwnEnv :: ServerState -> Session -> Int -> Window -> IO FormatEnv
windowOwnEnv st sess ix win = do
    eff <- atomically (windowArea st win)
    (wname, lay, cur, mlast, bell, act, auto, zoom) <- atomically $ (,,,,,,,)
        <$> readTVar win.name <*> readTVar win.layout
        <*> readTVar sess.currentIx <*> (listToMaybe <$> readTVar sess.windowHist)
        <*> readTVar win.bellFlag <*> readTVar win.activity
        <*> readTVar win.autoRename <*> readTVar win.zoomed
    sil <- readTVarIO win.silenceFlag
    fs <- readTVarIO win.floats
    ps <- readTVarIO win.panes
    (links, linkNames) <- atomically $ do
        links <- map fst <$> windowLinks st win
        (,) links <$> mapM (\s -> readTVar s.name) links
    let flags = windowFlags WindowFlagState
            { flagCurrent = ix == cur
            , flagLast = Just ix == mlast
            , flagBell = bell
            , flagActivity = act
            , flagSilence = sil
            , flagModal = isJust fs.modal
            , flagZoomed = isJust zoom
            }
    winUser <- deltaFormats <$> readTVarIO win.options
    viewers <- Map.findWithDefault 0 win.id <$> atomically (activeClientCounts st)
    pure $ Map.union winUser $ Map.fromList
        [ ("window_index", tshow ix)
        , ("window_active_clients", tshow viewers)
        , ("window_id", "@" <> tshow (rawWindow win.id))
        , ("window_name", wname)
        , ("window_width", tshow eff.cols)
        , ("window_height", tshow eff.rows)
        , ("window_layout", emitLayoutZ (stacking fs lay) (fitLayout eff lay))
        , ("window_visible_layout", emitLayout
            (fitLayout eff (maybe lay singlePane zoom)))
        , ("window_raw_flags", flags)
        , ("window_active", if ix == cur then "1" else "0")
        , ("window_flags", flags)
        , ("window_panes", tshow (Map.size ps))
        , ("window_zoomed_flag", if isJust zoom then "1" else "0")
        , ("window_modal_pane", maybe "" (\p -> "%" <> tshow (rawPane p)) (modalPane fs))
        , ("window_bell_flag", if bell then "1" else "0")
        , ("window_activity_flag", if act then "1" else "0")
        , ("window_silence_flag", if sil then "1" else "0")
        , ("automatic_rename", if auto then "1" else "0")
        , ("window_linked", if length links > 1 then "1" else "0")
        -- A session group shares one window map, so it counts once.
        , ("window_linked_sessions", tshow (length
            (List.nubBy (\a b -> a.windows == b.windows) links)))
        , ("window_linked_sessions_list", T.intercalate "," linkNames)
        , ("window_last_flag", if Just ix == mlast then "1" else "0")
        ]

-- | The full format environment for a specific pane, including the
-- fields tmux-resurrect's @save.sh@ dumps (pid, command, cursor,
-- history, cwd).
-- | A pane's @pane_pid@: empty unless its program is still running (tmux:
-- an open fd), so it also shadows the session's active-pane value.
panePid :: Pane -> IO Text
panePid pane = do
    mexit <- readTVarIO pane.exited
    pure $ if pane.kind == ProgramPane && isNothing mexit
        then tshow (Hat.Term.Pty.pid pane.pty)
        else ""

paneFormatEnv
    :: ServerState -> Session -> Int -> Window -> Int -> Pane -> IO FormatEnv
paneFormatEnv st sess wix win pix pane = do
    wenv <- windowFormatEnv st sess wix win
    dir <- paneCurrentPath pane
    cmd <- paneCommandName pane
    scr <- Emu.snapshot pane.emulator
    hsize <- Emu.scrollbackLength pane.emulator
    pendingWrap <- Emu.cursorPendingWrap pane.emulator
    active <- readTVarIO win.activeId
    title <- Emu.title pane.emulator
    sz <- readTVarIO pane.size
    area <- atomically (windowArea st win)
    (geom, floatVars) <- atomically $ do
        zoomed <- readTVar win.zoomed
        lay <- fitLayout area <$> readTVar win.layout
        fs <- readTVar win.floats
        hist <- readTVar win.paneHist
        -- tmux's zoom swaps the whole tree for the zoomed pane alone, so
        -- nothing floats while a window is zoomed.
        let floating = isNothing zoomed && isFloating pane.id lay
            modal = modalPane fs == Just pane.id
            letters = T.concat
                [ if pane.id == active then "*" else ""
                , if listToMaybe hist == Just pane.id then "-" else ""
                , if zoomed == Just pane.id then "Z" else ""
                , if floating then "F" else ""
                , if modal then "O" else "" ]
        pure ( if zoomed == Just pane.id
                then Just (0, 0, fromIntegral area.cols, fromIntegral area.rows)
                else (\c -> (c.xoff, c.yoff, c.sx, c.sy)) <$> paneCell pane.id lay
             , [ ("pane_floating_flag", if floating then "1" else "0")
               , ("pane_modal_flag", if modal then "1" else "0")
               , ("pane_z", tshow (paneZ fs lay pane.id))
               , ("pane_flags", letters) ] )
    unzoomed <- atomically (List.lookup pane.id <$> unzoomedArrange st area win)
    (dfg, dbg) <- paneColourVars st sess win pane
    paneUser <- deltaFormats <$> readTVarIO pane.options
    mexit <- readTVarIO pane.exited
    (marked, inputOff) <- atomically $ (,) <$> readTVar st.markedPane <*> readTVar pane.inputOff
    synced <- optionFlag "synchronize-panes"
        <$> atomically (resolveForPane st sess win pane)
    -- A signal is reported by number, as tmux does where libc has no
    -- @sys_signame@ (glibc).
    let deadVars = case mexit of
            Nothing -> [("pane_dead", "0")]
            Just e ->
                [ ("pane_dead", "1")
                , ("pane_dead_time", tshow (floor e.diedAt :: Integer)) ]
                <> [ ("pane_dead_status", tshow n) | Just n <- [e.status] ]
                <> [ ("pane_dead_signal", tshow n) | Just n <- [e.signal] ]
    pid <- panePid pane
    let pidVars = [ ("pane_pid", pid) ]
    ms <- Emu.modes pane.emulator
    (sgr, utf8) <- Emu.mouseEncoding pane.emulator
    let flag b = if b then "1" else "0" :: Text
        mouseVars =
            [ ("mouse_any_flag", flag (ms.mouse /= Emu.MouseOff))
            , ("mouse_standard_flag", flag (ms.mouse == Emu.MouseClick))
            , ("mouse_button_flag", flag (ms.mouse == Emu.MouseDrag))
            , ("mouse_all_flag", flag (ms.mouse == Emu.MouseMove))
            , ("mouse_sgr_flag", flag sgr)
            , ("mouse_utf8_flag", flag utf8)
            , ("alternate_on", flag ms.altScreen) ]
    pure $ Map.union paneUser $ Map.union (Map.fromList $ deadVars <> pidVars <> mouseVars <>
        [ ("pane_id", "%" <> tshow (rawPane pane.id))
        , ("pane_index", tshow pix)
        , ("pane_current_path", T.pack dir)
        , ("pane_current_command", cmd)
        , ("pane_active", if pane.id == active then "1" else "0")
        , ("pane_marked", if marked == Just pane.id then "1" else "0")
        , ("pane_marked_set", if isJust marked then "1" else "0")
        , ("pane_input_off", if inputOff then "1" else "0")
        , ("pane_synchronized", if synced then "1" else "0")
        , ("pane_title", title)
        , ("cursor_x", tshow (scr.cursor.col + fromEnum pendingWrap))
        , ("cursor_y", tshow scr.cursor.row)
        , ("cursor_character", case (Emu.screenCell scr scr.cursor).content of
              Cell.Glyph c cs _ -> T.pack (c : cs)
              Cell.Continuation -> "")
        , ("history_size", tshow hsize)
        , ("pane_width", tshow sz.cols)
        , ("pane_height", tshow sz.rows)
        , ("pane_fg", dfg)
        , ("pane_bg", dbg)
        ]
        <> concat [ [ ("pane_unzoomed_width", tshow (r.endCol - r.startCol))
                    , ("pane_unzoomed_height", tshow (r.endRow - r.startRow)) ]
                  | Just r <- [unzoomed] ] <> floatVars <> panePositionVars area geom) wenv

-- | @pane_fg@ and @pane_bg@: the colours a pane's default-coloured cells
-- draw in (see 'Hat.Server.Render.paneDefaults'), its styles read as
-- written.
paneColourVars :: ServerState -> Session -> Window -> Pane -> IO (Text, Text)
paneColourVars st sess win pane = do
    th <- clientTheme st
    (popts, active) <- atomically $
        (,) <$> resolveForPane st sess win pane <*> readTVar win.activeId
    (pfg, pbg) <- Emu.appDefaultColours pane.emulator
    let rgb = maybe Cell.DefaultColor (\(r, g, b) -> Cell.RGB r g b)
        style n = styleCell th (optionText n popts)
        (fg, bg) = paneDefaults (pane.id == active) (rgb pfg, rgb pbg)
            (style "window-style") (style "window-active-style")
    pure (colourName fg, colourName bg)

-- | tmux's @colour_tostring@ for a cell colour.
colourName :: Cell.Color -> Text
colourName = \case
    Cell.DefaultColor -> "default"
    Cell.Indexed n
        | n < 8 -> names !! fromIntegral n
        | n < 16 -> "bright" <> names !! fromIntegral (n - 8)
        | otherwise -> "colour" <> tshow n
    Cell.RGB r g b -> T.pack ('#' : concatMap hex2 [r, g, b])
  where
    names = ["black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"]
    hex2 w = [digit (w `div` 16), digit (w `mod` 16)]
    digit d = "0123456789abcdef" !! fromIntegral d

-- | A pane's position in its window (tmux's @pane_left@ … @pane_at_bottom@),
-- from its cell: @(xoff, yoff, width, height)@.
panePositionVars :: Size -> Maybe (Int, Int, Int, Int) -> [(Text, Text)]
panePositionVars area = \case
    Nothing -> []
    Just (x, y, w, h) ->
        [ ("pane_left", tshow x)
        , ("pane_top", tshow y)
        , ("pane_x", tshow x)
        , ("pane_y", tshow y)
        , ("pane_right", tshow (x + w - 1))
        , ("pane_bottom", tshow (y + h - 1))
        , ("pane_at_left", flag (x == 0))
        , ("pane_at_top", flag (y == 0))
        , ("pane_at_right", flag (x + w == fromIntegral area.cols))
        , ("pane_at_bottom", flag (y + h == fromIntegral area.rows))
        ]
  where
    flag b = if b then "1" else "0"

-- | 'paneFormatEnv' for a pane named only by id: locate its window and
-- session first.
paneEnvById :: ServerState -> PaneId -> IO (Maybe FormatEnv)
paneEnvById st pid = do
    mctx <- atomically $ do
        mloc <- locatePane st pid
        case mloc of
            Nothing -> pure Nothing
            Just (sid, win) -> do
                msess <- Map.lookup sid <$> readTVar st.sessions
                case msess of
                    Nothing -> pure Nothing
                    Just sess -> do
                        ws <- readTVar sess.windows
                        ps <- readTVar win.panes
                        let mwix = listToMaybe
                                [ i | (i, w) <- Map.toList ws, w.id == win.id ]
                        pure $ (,,,) sess <$> mwix <*> Just win
                            <*> Map.lookup pid ps
    case mctx of
        Nothing -> pure Nothing
        Just (sess, wix, win, pane) -> do
            pix <- paneIndexOf st win pane
            Just <$> paneFormatEnv st sess wix win pix pane

-- | Recompute the names of every @automatic-rename@ window from its
-- active pane's foreground command, bumping the render generation on any
-- change. Driven by a periodic poll so no-output commands (an idle
-- @less@, a waiting @cat@) still get picked up.
refreshAutoNames :: ServerState -> IO ()
refreshAutoNames st = do
    fmt <- (.automaticRenameFormat) <$> readTVarIO st.options
    sessions <- Map.elems <$> readTVarIO st.sessions
    forM_ sessions $ \sess -> do
        ws <- Map.toAscList <$> readTVarIO sess.windows
        forM_ ws $ \(ix, win) -> do
            auto <- readTVarIO win.autoRename
            when auto $ do
                mnew <- autoName st sess ix win fmt
                forM_ mnew $ \newName -> atomically $ do
                    cur <- readTVar win.name
                    when (cur /= newName && not (T.null newName)) $ do
                        writeTVar win.name newName
                        bumpDirty st

-- | The name an @automatic-rename@ window should currently take: the
-- @automatic-rename-format@ expanded against the active pane. The default
-- format is just @#{pane_current_command}@, so it takes a cheap path.
autoName :: ServerState -> Session -> Int -> Window -> Text -> IO (Maybe Text)
autoName st sess ix win fmt = do
    mpane <- atomically (activePane win)
    case mpane of
        Nothing -> pure Nothing
        Just pane
            | fmt == "#{pane_current_command}" -> Just <$> paneCommandName pane
            | otherwise -> do
                pbase <- (.paneBaseIndex) <$> readTVarIO st.options
                env <- paneFormatEnv st sess ix win pbase pane
                Just <$> expandFormat st env fmt

-- | tmux's @window_active_clients@, per window: a client counts for the window
-- its session currently shows, keyed by window identity so a window linked
-- into several sessions counts the viewers of every one.
activeClientCounts :: ServerState -> STM (Map.Map WindowId Int)
activeClientCounts st = do
    cs <- Map.elems <$> readTVar st.clients
    sessions <- readTVar st.sessions
    fmap (Map.fromListWith (+) . catMaybes) . forM cs $ \c -> do
        sid <- readTVar c.session
        case Map.lookup sid sessions of
            Nothing -> pure Nothing
            Just sess -> fmap (\w -> (w.id, 1)) <$> currentWindow sess

-- Session-level format environment for the active window and pane.
sessionFormatEnv :: ServerState -> Session -> IO FormatEnv
sessionFormatEnv st sess = do
    server <- serverFormatVars
    (sname, wEnv, mactive, nclients, nwindows) <- atomically $ do
        sname <- readTVar sess.name
        mwin <- currentWindow sess
        cur <- readTVar sess.currentIx
        nwindows <- Map.size <$> readTVar sess.windows
        counts <- activeClientCounts st
        wEnv <- case mwin of
            Nothing -> pure []
            Just win -> do
                wname <- readTVar win.name
                pure [ ("window_index", tshow cur)
                     , ("window_id", "@" <> tshow (rawWindow win.id))
                     , ("window_name", wname)
                     , ( "window_active_clients"
                       , tshow (Map.findWithDefault 0 win.id counts) )
                     ]
        mactive <- maybe (pure Nothing) activePane mwin
        cs <- sessionClients st sess.id
        gEnv <- groupEnv
        alerts <- sessionAlerts sess
        pure (sname, wEnv <> gEnv <> alerts, mactive, length cs, nwindows)
    pEnv <- case mactive of
        Nothing -> pure []
        Just pane -> do
            dir <- paneCurrentPath pane
            title <- Emu.title pane.emulator
            cmd <- paneCommandName pane
            modeEnv <- paneModeEnv st pane
            pid <- panePid pane
            pure $ [ ("pane_current_path", T.pack dir)
                   , ("pane_current_command", cmd)
                   , ("pane_pid", pid)
                   , ("pane_active", "1")
                   , ("pane_title", title)
                   , ("pane_id", "%" <> tshow (rawPane pane.id))
                   ] <> modeEnv
    sz <- readTVarIO sess.lastSize
    -- Options are readable as #{status-left}, #{@foo}, … so if-shell theme
    -- conditionals (@#{@pane-theme}@) resolve.
    sessOpts <- optionFormats <$> atomically (resolveForSession st sess)
    msch <- readTVarIO st.colorScheme
    pure . Map.union sessOpts . Map.fromList $ server <>
        [ ("session_name", sname)
        , ("session_id", "$" <> tshow (rawSession sess.id))
        , ("session_attached", tshow nclients)
        , ("session_windows", tshow nwindows)
        , ("session_created", tshow (floor sess.createdAt :: Integer))
        , ("session_restores", tshow sess.restores)
        , ("window_active_clients", "0")
        , ("window_width", tshow sz.cols)
        , ("window_height", tshow sz.rows)
        , ("color_scheme", maybe "" schemeName msch)
        ]
        <> wEnv <> pEnv
  where
    groupEnv = readTVar sess.group >>= \case
        Nothing -> pure [("session_grouped", "0")]
        Just g -> do
            ss <- Map.elems <$> readTVar st.sessions
            members <- filterM (fmap (== Just g) . readTVar . (.group)) ss
            names <- mapM (\m -> readTVar m.name) members
            attached <- sum <$> mapM (fmap length . sessionClients st . (.id)) members
            pure [ ("session_grouped", "1")
                 , ("session_group", g)
                 , ("session_group_size", tshow (length members))
                 , ("session_group_list", T.intercalate "," names)
                 , ("session_group_attached", tshow attached)
                 , ("session_group_many_attached", if attached > 1 then "1" else "0")
                 ]

-- | tmux's @session_alert@ (each alert kind once, in @#!~@ order) and
-- @session_alerts@ (each alerted window's index and its alert marks).
sessionAlerts :: Session -> STM [(Text, Text)]
sessionAlerts sess = do
    ws <- Map.toAscList <$> readTVar sess.windows
    marks <- forM ws $ \(ix, win) -> do
        (act, bell, sil) <- (,,) <$> readTVar win.activity
            <*> readTVar win.bellFlag <*> readTVar win.silenceFlag
        pure (ix, [ c | (on, c) <- [(act, '#'), (bell, '!'), (sil, '~')], on ])
    let alerted = filter (not . null . snd) marks
        kinds = List.foldl' (\seen c -> if c `elem` seen then seen else seen <> [c])
            [] (concatMap snd alerted)
    pure [ ("session_alert", T.pack kinds)
         , ("session_alerts", T.intercalate ","
             [ tshow ix <> T.pack cs | (ix, cs) <- alerted ]) ]

-- | Copy-mode format variables for a pane: @pane_in_mode@/@pane_mode@,
-- plus the copy-mode variables while in mode.
paneModeEnv :: ServerState -> Pane -> IO [(Text, Text)]
paneModeEnv st pane = do
    (mmode, clockTop) <- atomically $ (,) <$> readTVar pane.mode <*> paneClockOnTop pane
    case mmode of
        _ | clockTop -> pure [("pane_in_mode", "1"), ("pane_mode", "clock-mode")]
        Nothing -> pure [("pane_in_mode", "0"), ("pane_mode", "")]
        Just pm -> do
            global <- readTVarIO st.options
            opts <- maybe global id <$> paneOptionsNow st pane
            let numbering = CopyMode.lineNumberModeOf (optionText "copy-mode-line-numbers" opts)
            pure $ [ ("pane_in_mode", "1"), ("pane_mode", "copy-mode") ]
                <> CopyMode.copyModeFormats global.wordSeparators numbering pm

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

-- | tmux's format context for a command with no target: the issuing
-- client's session when it has one, else only the global user options.
clientContextFormatEnv :: ServerState -> Maybe Client -> IO FormatEnv
clientContextFormatEnv st mclient = do
    msess <- case mclient of
        Nothing -> pure Nothing
        Just c -> atomically $ do
            sid <- readTVar c.session
            Map.lookup sid <$> readTVar st.sessions
    case msess of
        Just sess -> sessionFormatEnv st sess
        Nothing -> Map.union <$> ((.user) <$> readTVarIO st.options)
            <*> (Map.fromList <$> serverFormatVars)

-- | The formats that describe the server process itself.
serverFormatVars :: IO [(Text, Text)]
serverFormatVars = do
    hostname <- nodeName <$> getSystemID
    pid <- getProcessID
    pure [("host", T.pack hostname), ("pid", tshow pid)]

-- | Expand a format string fully: #{...}, cached #(...), then strftime.
-- Inside a hook, the event's payload formats (#{hook}, #{hook_pane}, …)
-- overlay the caller's environment.
expandFormat :: ServerState -> FormatEnv -> Text -> IO Text
expandFormat st env fmt = do
    ctx <- formatCtxFor st env
    pure $! renderFormatCtx ctx fmt

-- | The evaluation context for expanding formats against @env0@: the hook
-- overlay, @#(…)@ commands resolved through the cache, and the loop and
-- search data. Each is read only if an expansion reaches it, so a job in
-- an untaken branch never runs (as in tmux).
formatCtxFor :: ServerState -> FormatEnv -> IO FormatCtx
formatCtxFor st env0 = do
    tid <- myThreadId
    mamb <- Map.lookup tid <$> readTVarIO st.hooks.ambient
    let env = maybe env0 (\amb -> Map.union amb.formats env0) mamb
    now <- getZonedTime
    -- A command is itself a format, expanded (as tmux's format_job_get
    -- does) without jobs, animation or strftime. The caller's strict use
    -- of the expansion keeps the deferred reads inside its own call.
    let jobCtx = formatCtx env (const "") now
        job inner = unsafePerformIO (resolveShell st (evaluateCtx jobCtx inner))
    sessionItems <- unsafeInterleaveIO (loopSessionEnvs st)
    windowItems <- unsafeInterleaveIO (loopWindowEnvs st env)
    paneItems <- unsafeInterleaveIO (loopPaneEnvs st env)
    clientItems <- unsafeInterleaveIO (loopClientEnvs st)
    visibleLines <- unsafeInterleaveIO (targetPaneLines st env)
    th <- clientTheme st
    pure (formatCtx env job now)
        { theme = th
        , sessions = sessionItems
        , windows = windowItems
        , panes = paneItems
        , clients = clientItems
        , paneLines = visibleLines
        }

-- | The theme a hat client draws with (see 'resolveTheme'): every client
-- shares the server's colour scheme and draws RGB colours.
clientTheme :: ServerState -> IO Theme
clientTheme st = do
    opts <- readTVarIO st.options
    scheme <- readTVarIO st.colorScheme
    now <- getZonedTime
    let ctx = formatCtx (Map.singleton "client_colours" clientColours) (const "") now
    pure (resolveTheme (evaluateCtx ctx) opts scheme)

-- | The formats plus every variable value satisfying @p@ that they name
-- (as @{name}@ or @:name}@), transitively: what their @E:@ or @T:@
-- expansions can reach.
reachableValues :: (Text -> Bool) -> FormatEnv -> [Text] -> [Text]
reachableValues p env = go Set.empty
  where
    wanted = Map.filter p env
    names k t = ("{" <> k <> "}") `T.isInfixOf` t || (":" <> k <> "}") `T.isInfixOf` t
    go _ [] = []
    go seen (t : ts) =
        let named = [ k | k <- Map.keys wanted, not (Set.member k seen)
                        , names k t ]
            seen' = foldr Set.insert seen named
        in t : go seen' (ts <> [ v | k <- named, Just v <- [Map.lookup k wanted] ])

-- | The conditions that produce a window's @#{window_flags}@ string.
data WindowFlagState = WindowFlagState
    { flagCurrent  :: Bool
    , flagLast     :: Bool
    , flagBell     :: Bool
    , flagActivity :: Bool
    , flagSilence  :: Bool
    , flagModal    :: Bool  -- ^ the window has a modal pane
    , flagZoomed   :: Bool
    }

-- | Render the window-status flags in tmux's order: current (@*@) or
-- last (@-@), then bell (@!@), activity (@#@) and silence (@~@), and
-- a modal pane (@O@), and finally zoom (@Z@) when the window has a pane
-- zoomed to fill it.
windowFlags :: WindowFlagState -> Text
windowFlags s = T.concat
    [ if s.flagCurrent then "*"
      else if s.flagLast then "-" else ""
    , if s.flagBell then "!" else ""
    , if s.flagActivity then "#" else ""
    , if s.flagSilence then "~" else ""
    , if s.flagModal then "O" else ""
    , if s.flagZoomed then "Z" else ""
    ]

-- One env per session, for the S loop and N/s.
loopSessionEnvs :: ServerState -> IO [FormatEnv]
loopSessionEnvs st = do
    ss <- Map.elems <$> readTVarIO st.sessions
    mapM (sessionFormatEnv st) ss

-- The target session's windows, for the W loop and N/w.
loopWindowEnvs :: ServerState -> FormatEnv -> IO [FormatEnv]
loopWindowEnvs st env = do
    msess <- targetSessionOf st env
    case msess of
        Nothing -> pure []
        Just sess -> do
            ws <- Map.toAscList <$> readTVarIO sess.windows
            mapM (\(ix, win) -> windowFormatEnv st sess ix win) ws

-- The target window's panes in creation order, for the P loop.
loopPaneEnvs :: ServerState -> FormatEnv -> IO [FormatEnv]
loopPaneEnvs st env = do
    mwin <- targetWindowOf st env
    case mwin of
        Nothing -> pure []
        Just (sess, wix, win) -> do
            ps <- Map.elems <$> readTVarIO win.panes
            mapM (\pane -> do
                pix <- paneIndexOf st win pane
                paneFormatEnv st sess wix win pix pane) ps

-- One env per attached client, for the L loop.
loopClientEnvs :: ServerState -> IO [FormatEnv]
loopClientEnvs st = do
    cs <- Map.elems <$> readTVarIO st.clients
    mapM (fmap Map.fromList . clientFormatVars) [ c | c <- cs, c.role == Attached ]

-- | A client's own format variables.
clientFormatVars :: Client -> IO [(Text, Text)]
clientFormatVars c = do
    controlFlags <- case c.control of
        Nothing -> pure []
        Just cs -> do
            fl <- readTVarIO cs.flags
            pure $ ["control-mode"]
                <> [ "no-output" | fl.output == NoOutput ]
                <> [ "wait-exit" | fl.exitWait == AwaitEmptyLine ]
                <> [ "pause-after=" <> tshow (age `div` 1000)
                   | Just age <- [fl.pauseAfter] ]
    focused <- readTVarIO c.outerFocused
    pure
        [ ("client_name", "client" <> tshow (rawClient c.id))
        , ("client_colours", clientColours)
        , ("client_control_mode", if isJust c.control then "1" else "0")
        , ("client_flags", T.intercalate "," $
            ["attached"] <> [ "focused" | focused ] <> controlFlags)
        ]

-- The target pane's visible lines, for the C search modifier.
targetPaneLines :: ServerState -> FormatEnv -> IO [Text]
targetPaneLines st env = case idNum '%' "pane_id" env of
    Nothing -> pure []
    Just n -> do
        mloc <- atomically (locatePane st (PaneId n))
        case mloc of
            Nothing -> pure []
            Just (_, win) -> do
                ps <- readTVarIO win.panes
                case Map.lookup (PaneId n) ps of
                    Nothing -> pure []
                    Just pane -> do
                        scr <- Emu.snapshot pane.emulator
                        pure [ Emu.screenRowText scr r
                             | r <- [0 .. V.length scr.cells - 1] ]

targetSessionOf :: ServerState -> FormatEnv -> IO (Maybe Session)
targetSessionOf st env = case idNum '$' "session_id" env of
    Nothing -> pure Nothing
    Just n -> Map.lookup (SessionId n) <$> readTVarIO st.sessions

targetWindowOf :: ServerState -> FormatEnv -> IO (Maybe (Session, Int, Window))
targetWindowOf st env = do
    msess <- targetSessionOf st env
    case (msess, idNum '@' "window_id" env) of
        (Just sess, Just wid) -> do
            ws <- Map.toAscList <$> readTVarIO sess.windows
            pure (listToMaybe
                [ (sess, ix, win) | (ix, win) <- ws, win.id == WindowId wid ])
        _ -> pure Nothing

-- A "$N"/"@N"/"%N" id variable's number.
idNum :: Char -> Text -> FormatEnv -> Maybe Int
idNum pre key env = do
    v <- Map.lookup key env
    rest <- T.stripPrefix (T.singleton pre) v
    case TR.decimal rest of
        Right (n, "") -> Just n
        _ -> Nothing

