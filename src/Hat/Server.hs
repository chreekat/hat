-- | The server: owns PTYs, emulators, and the state tree; accepts
-- clients, streams frame diffs at them, and runs the command engine
-- that configs, bindings, and @hat <command>@ all share.
module Hat.Server
    ( runServer
    , setOption  -- ^ exported for the config-load burn-down test
    ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async (race, withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
    (IOException, SomeException, bracket, catch, finally, try)
import Control.Monad (foldM, forM, forM_, forever, unless, void, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.IORef
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Time.LocalTime (getZonedTime)
import qualified Data.Vector as V
import qualified Network.Socket as N
import System.Directory (doesFileExist, removeFile)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..), exitSuccess)
import System.FilePath (takeDirectory)
import System.IO (Handle, SeekMode (AbsoluteSeek), hClose, hFlush)
import qualified System.Posix.Files as PFiles
import qualified System.Posix.IO as PIO
import System.Posix.Process (getProcessID)
import System.Posix.Unistd (SystemID (nodeName), getSystemID)
import System.Process
    (CreateProcess (..), StdStream (..), createProcess,
     readCreateProcessWithExitCode, shell, terminateProcess, waitForProcess)

import Hat.Command.Parser (parseCommandLine, parseConfig)
import Hat.Geometry
import Hat.Log
import Hat.Model
import Hat.Model.Options
import qualified Hat.Pty
import qualified Hat.Server.CopyMode as CopyMode
import Hat.Server.Format (FormatEnv, renderFormat)
import Hat.Server.Keys
import Hat.Server.Layout
import qualified Hat.Server.Prompt as Prompt
import Hat.Server.Render
import Hat.Server.Target (PaneTarget (..), parsePaneTarget)
import Hat.Socket (ensureSocketDir, listenOn)
import qualified Hat.Term.Cell as Cell
import qualified Hat.Term.Emulator as Emu
import Hat.Wire

runServer :: FilePath -> Maybe FilePath -> IO ()
runServer path mconfig = do
    -- The lock and log files live next to the socket; the directory
    -- must exist before any of them are touched.
    ensureSocketDir path
    locked <- acquireLock (path <> ".lock")
    unless locked exitSuccess  -- another server won the race
    lg <- newLogger (takeDirectory path <> "/server.log")
    st <- newServerState defaultKeymap lg path
    bracket (listenOn path) N.close $ \lsock -> do
        logEvent lg ServerStarted { socket = path }
        -- Load the config in a background thread so shell conditions
        -- like `if '$TMUX run ...' ...` can reach the accept loop while
        -- the config is still running. configLoading suppresses the
        -- empty-idle exit until the config has drained.
        atomically (writeTVar st.configLoading True)
        _ <- forkIO $ do
            loadConfig st mconfig
            -- Grace period so a `hat -f<conf> start` client whose fork
            -- lost the race with a fast config still finds us listening.
            threadDelay 500_000
            atomically (writeTVar st.configLoading False)
        -- Keep status-line clocks fresh.
        _ <- forkIO $ forever $ do
            threadDelay 15_000_000
            atomically (bumpDirty st)
        r <- race (acceptLoop st lsock) (waitIdle st)
        case r of
            Left () -> pure ()
            Right () -> do
                logEvent lg ServerStopping { reason = "no sessions left" }
                removeFile path `catch` \(_ :: IOException) -> pure ()

-- flock-style: O_CREAT + posix write lock, held for the server's life.
acquireLock :: FilePath -> IO Bool
acquireLock lockPath = do
    fd <- PIO.createFile lockPath 0o600
    r <- try $ PIO.setLock fd (PIO.WriteLock, AbsoluteSeek, 0, 0)
    pure $ case r of
        Left (_ :: IOException) -> False
        Right () -> True

-- Exit once every session is gone AND every attached client has
-- drained and disconnected, so nobody's final Exited message is cut off.
waitIdle :: ServerState -> IO ()
waitIdle st = atomically $ do
    armed <- readTVar st.everAttached
    loading <- readTVar st.configLoading
    sess <- readTVar st.sessions
    cs <- readTVar st.clients
    check (armed && not loading && Map.null sess && Map.null cs)

-- Configuration --------------------------------------------------------

defaultKeymap :: Keymap
defaultKeymap = Map.fromList
    [ ("prefix", Map.fromList (map bindArgv prefixBindings))
    , ("root", Map.empty)
    , ("copy-mode", Map.fromList (map copyBind copyModeBindings))
    , ("copy-mode-vi", Map.fromList (map copyBind copyModeViBindings))
    ]
  where
    bindArgv (k, cmd) = (k, [cmd])
    -- A copy-mode key runs a single @send-keys -X <name>@ command.
    copyBind (k, name) = (k, [["send-keys", "-X", name]])
    prefixBindings =
        [ ("d", ["detach-client"])
        , ("c", ["new-window"])
        , ("%", ["split-window", "-h"])
        , ("\"", ["split-window", "-v"])
        , ("x", ["kill-pane"])
        , ("&", ["kill-window"])
        , ("z", ["resize-pane", "-Z"])
        , ("n", ["next-window"])
        , ("p", ["previous-window"])
        , ("l", ["last-window"])
        , (";", ["last-pane"])
        , ("C-b", ["send-prefix"])
        , ("[", ["copy-mode"])
        , ("]", ["paste-buffer"])
        , (":", ["command-prompt"])
        , ("Left", ["select-pane", "-L"])
        , ("Right", ["select-pane", "-R"])
        , ("Up", ["select-pane", "-U"])
        , ("Down", ["select-pane", "-D"])
        ]
        <> [ (tshow i, ["select-window", "-t", tshow (i :: Int)])
           | i <- [0 .. 9]
           ]
    copyModeViBindings =
        [ ("h", "cursor-left"), ("j", "cursor-down")
        , ("k", "cursor-up"), ("l", "cursor-right")
        , ("0", "start-of-line"), ("$", "end-of-line")
        , ("w", "next-word"), ("b", "previous-word"), ("e", "next-word-end")
        , ("W", "next-space"), ("B", "previous-space"), ("E", "next-space-end")
        , ("g", "history-top")
        , ("v", "begin-selection"), ("Escape", "clear-selection")
        , ("y", "copy-selection-and-cancel")
        , ("Enter", "copy-selection-and-cancel")
        , ("q", "cancel")
        , ("Left", "cursor-left"), ("Right", "cursor-right")
        , ("Up", "cursor-up"), ("Down", "cursor-down")
        ]
    copyModeBindings =
        [ ("C-b", "cursor-left"), ("C-f", "cursor-right")
        , ("C-p", "cursor-up"), ("C-n", "cursor-down")
        , ("C-a", "start-of-line"), ("C-e", "end-of-line")
        , ("M-f", "next-word-end"), ("M-b", "previous-word")
        , ("M-<", "history-top")
        , ("Space", "begin-selection"), ("C-g", "clear-selection")
        , ("M-w", "copy-selection-and-cancel")
        , ("Enter", "copy-selection-and-cancel")
        , ("q", "cancel"), ("Escape", "cancel")
        , ("Left", "cursor-left"), ("Right", "cursor-right")
        , ("Up", "cursor-up"), ("Down", "cursor-down")
        ]

loadConfig :: ServerState -> Maybe FilePath -> IO ()
loadConfig st mconfig =
    forM_ mconfig $ \p -> do
        exists <- doesFileExist p
        when exists $ do
            contents <- TIO.readFile p
            case parseConfig contents of
                Left err -> logEvent st.logger ConfigError
                    { file = p, err = err }
                Right cmds -> forM_ cmds $ \argv -> do
                    replies <- runArgv st Nothing argv
                    forM_ [e | RErr e <- replies] $ \e ->
                        logEvent st.logger ConfigError { file = p, err = e }

-- Connections ----------------------------------------------------------

acceptLoop :: ServerState -> N.Socket -> IO ()
acceptLoop st lsock = forever $ do
    (conn, _) <- N.accept lsock
    void . forkIO $
        handleConn st conn
            `catch` (\(e :: SomeException) ->
                logEvent st.logger ServerCrash { err = T.pack (show e) })
            `finally` (N.close conn `catch` \(_ :: SomeException) -> pure ())

handleConn :: ServerState -> N.Socket -> IO ()
handleConn st conn = do
    m <- recvMessage conn
    case m of
        Just (Right (ClientHello h))
            | h.protoVersion == protocolVersion -> welcome st conn h
            | otherwise -> sendMessage conn $ ServerError $
                "protocol mismatch: server " <> tshow protocolVersion
                <> ", client " <> tshow h.protoVersion
        _ -> sendMessage conn (ServerError "expected hello")

welcome :: ServerState -> N.Socket -> Hello -> IO ()
welcome st conn h = do
    client <- newClient st conn h
    case h.intent of
        ControlIntent -> do
            atomically $ modifyTVar' st.clients (Map.insert client.id client)
            sendMessage conn (Welcome "")
            controlLoop st client `finally` removeClient st client
        AttachIntent -> do
            sess <- ensureSession st client
            sname <- readTVarIO sess.name
            atomically $ do
                writeTVar client.session sess.id
                modifyTVar' st.clients (Map.insert client.id client)
                writeTVar st.everAttached True
            applySessionSize st sess.id
            sendMessage conn (Welcome sname)
            logEvent st.logger ClientConnected
                { client = rawClient client.id, term = h.term }
            withAsync (renderLoop st client) $ \_ ->
                inputLoop st client
                    `finally` removeClient st client

newClient :: ServerState -> N.Socket -> Hello -> IO Client
newClient st conn h = do
    cid <- atomically (freshId st.nextClient)
    sendLock <- newMVar ()
    sizeVar <- newTVarIO h.size
    sessVar <- newTVarIO (SessionId (-1))
    lastSessVar <- newTVarIO Nothing
    keyVar <- newIORef NoPrefix
    frameVar <- newIORef (blankFrame h.size)
    cursorVar <- newIORef (Pos 0 0, True)
    fullVar <- newTVarIO True
    toastVar <- newTVarIO Nothing
    promptVar <- newTVarIO Nothing
    pure Client
        { id = ClientId cid
        , sock = conn
        , sendLock = sendLock
        , size = sizeVar
        , session = sessVar
        , lastSession = lastSessVar
        , keyState = keyVar
        , lastFrame = frameVar
        , lastCursor = cursorVar
        , needsFull = fullVar
        , toast = toastVar
        , prompt = promptVar
        , env = h.env
        , cwd = h.cwd
        }

removeClient :: ServerState -> Client -> IO ()
removeClient st client = do
    sid <- readTVarIO client.session
    atomically $ modifyTVar' st.clients (Map.delete client.id)
    applySessionSize st sid
    logEvent st.logger ClientDetached
        { client = rawClient client.id, reason = "gone" }

send :: Client -> ServerToClient -> IO ()
send client msg =
    withMVar client.sendLock (\_ -> sendMessage client.sock msg)
        `catch` \(_ :: SomeException) -> pure ()

broadcast :: ServerState -> SessionId -> ServerToClient -> IO ()
broadcast st sid msg = do
    cs <- atomically (sessionClients st sid)
    forM_ cs $ \c -> send c msg

-- Sessions --------------------------------------------------------------

ensureSession :: ServerState -> Client -> IO Session
ensureSession st client = do
    existing <- readTVarIO st.sessions
    case Map.lookupMin existing of
        Just (_, sess) -> pure sess
        Nothing -> createSession st Nothing Nothing client.env
            (T.unpack client.cwd) =<< readTVarIO client.size

createSession
    :: ServerState -> Maybe Text -> Maybe Text -> [(Text, Text)]
    -> FilePath -> Size -> IO Session
createSession st mname mrun environ dir sz = do
    sid <- atomically (freshId st.nextSession)
    opts <- readTVarIO st.options
    let shellCmd = maybe "/bin/sh" T.unpack (List.lookup "SHELL" environ)
    (win, pane) <- newWindowWithPane st (SessionId sid) shellCmd mrun
        dir environ (windowArea sz)
    nameVar <- newTVarIO (fromMaybe (tshow sid) mname)
    windowsVar <- newTVarIO (Map.singleton opts.baseIndex win)
    currentVar <- newTVarIO opts.baseIndex
    lastVar <- newTVarIO Nothing
    sizeVar <- newTVarIO sz
    let sess = Session
            { id = SessionId sid
            , name = nameVar
            , windows = windowsVar
            , currentIx = currentVar
            , lastIx = lastVar
            , lastSize = sizeVar
            , environ = environ
            , startCwd = dir
            }
    atomically $ modifyTVar' st.sessions (Map.insert sess.id sess)
    startPaneReader st sess.id win pane
    pure sess

newWindowWithPane
    :: ServerState -> SessionId -> FilePath -> Maybe Text -> FilePath
    -> [(Text, Text)] -> Size -> IO (Window, Pane)
newWindowWithPane st sid shellCmd mrun dir environ sz = do
    (wid, pid) <- atomically $
        (,) <$> freshId st.nextWindow <*> freshId st.nextPane
    pane <- spawnPane st (PaneId pid) sid shellCmd mrun dir environ sz
    nameVar <- newTVarIO $ case mrun of
        Just cmd -> T.takeWhile (/= ' ') cmd
        Nothing -> T.pack (baseName shellCmd)
    layoutVar <- newTVarIO (Leaf pane.id)
    panesVar <- newTVarIO (Map.singleton pane.id pane)
    activeVar <- newTVarIO pane.id
    lastActiveVar <- newTVarIO Nothing
    bellVar <- newTVarIO False
    zoomVar <- newTVarIO Nothing
    let win = Window
            { id = WindowId wid
            , name = nameVar
            , layout = layoutVar
            , panes = panesVar
            , activeId = activeVar
            , lastActive = lastActiveVar
            , bellFlag = bellVar
            , zoomed = zoomVar
            }
    pure (win, pane)
  where
    baseName = Prelude.reverse . takeWhile (/= '/') . Prelude.reverse

spawnPane
    :: ServerState -> PaneId -> SessionId -> FilePath -> Maybe Text
    -> FilePath -> [(Text, Text)] -> Size -> IO Pane
spawnPane st pid sid shellCmd mrun dir environ sz = do
    serverPid <- getProcessID
    opts <- readTVarIO st.options
    let cleanEnv =
            [ (T.unpack k, T.unpack v)
            | (k, v) <- environ
            , k `notElem` ["TERM", "TMUX", "TMUX_PANE", "HAT", "HAT_PANE"]
            ]
        hatVar = st.sockPath <> "," <> show serverPid <> ","
            <> show (rawSession sid)
        term = T.unpack opts.defaultTerminal
        paneEnv = cleanEnv <>
            [ ("TERM", term)
            , ("TMUX", hatVar)
            , ("HAT", hatVar)
            , ("TMUX_PANE", "%" <> show (rawPane pid))
            , ("HAT_PANE", "%" <> show (rawPane pid))
            ]
        (cmd, args) = case mrun of
            Nothing -> (shellCmd, [])
            Just run -> ("/bin/sh", ["-c", T.unpack run])
    pty <- Hat.Pty.spawn Hat.Pty.Spawn
        { cmd = cmd
        , args = args
        , env = paneEnv
        , cwd = Just dir
        , size = sz
        }
    emu <- Emu.newEmulator sz opts.historyLimit
    sizeVar <- newTVarIO sz
    deadVar <- newTVarIO False
    modeVar <- newTVarIO Nothing
    pipeVar <- newTVarIO Nothing
    logEvent st.logger PaneSpawned
        { pane = rawPane pid, cmd = T.pack cmd }
    pure Pane
        { id = pid
        , pty = pty
        , emulator = emu
        , size = sizeVar
        , dead = deadVar
        , startCwd = dir
        , mode = modeVar
        , pipe = pipeVar
        }

startPaneReader :: ServerState -> SessionId -> Window -> Pane -> IO ()
startPaneReader st sid win pane = void . forkIO $ loop
  where
    loop = do
        bs <- Hat.Pty.readPty pane.pty
        if B8.null bs
            then paneDied st sid win pane
            else do
                forwardToPipe pane bs
                events <- Emu.feed pane.emulator bs
                forM_ events $ \case
                    Emu.Output out -> Hat.Pty.writePty pane.pty out
                    Emu.TitleChanged t -> broadcast st sid (SetTitle t)
                    Emu.Bell -> do
                        atomically $ do
                            writeTVar win.bellFlag True
                            bumpDirty st
                        broadcast st sid RingBell
                    Emu.ScreenChanged -> atomically (bumpDirty st)
                loop

-- Pane rects and borders for a window, honoring zoom.
windowArrange :: Size -> Window -> STM ([(PaneId, Rect)], [(Pos, Char)])
windowArrange eff win = do
    mz <- readTVar win.zoomed
    lay <- readTVar win.layout
    ps <- readTVar win.panes
    pure $ case mz of
        Just zpid | Map.member zpid ps -> ([(zpid, sizeRect eff)], [])
        _ -> arrange (sizeRect eff) lay

paneDied :: ServerState -> SessionId -> Window -> Pane -> IO ()
paneDied st sid win pane = do
    stopPipe pane
    _ <- Hat.Pty.waitExit pane.pty
    Hat.Pty.closePty pane.pty
    logEvent st.logger PaneExited { pane = rawPane pane.id }
    sessionGone <- atomically $ do
        writeTVar pane.dead True
        modifyTVar' win.panes (Map.delete pane.id)
        lay <- readTVar win.layout
        mz <- readTVar win.zoomed
        when (mz == Just pane.id) $ writeTVar win.zoomed Nothing
        case removeLeaf pane.id lay of
            Just lay' -> do
                writeTVar win.layout lay'
                active <- readTVar win.activeId
                when (active == pane.id) $
                    case layoutPanes lay' of
                        (next : _) -> writeTVar win.activeId next
                        [] -> pure ()
                bumpDirty st
                pure Nothing
            Nothing -> do
                msess <- Map.lookup sid <$> readTVar st.sessions
                case msess of
                    Nothing -> pure Nothing
                    Just sess -> do
                        ws <- readTVar sess.windows
                        let ws' = Map.filter (\w -> w.id /= win.id) ws
                        writeTVar sess.windows ws'
                        if Map.null ws'
                            then do
                                modifyTVar' st.sessions (Map.delete sid)
                                pure (Just sess)
                            else do
                                cur <- readTVar sess.currentIx
                                case Map.lookupMin ws' of
                                    Just (ix, _) | not (Map.member cur ws') ->
                                        writeTVar sess.currentIx ix
                                    _ -> pure ()
                                bumpDirty st
                                pure Nothing
    forM_ sessionGone $ \_ -> broadcast st sid Exited
    applySessionSize st sid

-- Sizing ----------------------------------------------------------------

-- | The pane area of the screen: everything except the status line.
windowArea :: Size -> Size
windowArea sz = sz { rows = max 1 (sz.rows - 1) }

-- Effective session size = smallest attached client; panes follow.
applySessionSize :: ServerState -> SessionId -> IO ()
applySessionSize st sid = do
    work <- atomically $ do
        msess <- Map.lookup sid <$> readTVar st.sessions
        case msess of
            Nothing -> pure Nothing
            Just sess -> do
                cs <- sessionClients st sid
                sizes <- mapM (\c -> readTVar c.size) cs
                eff <- case sizes of
                    [] -> readTVar sess.lastSize
                    _ -> pure Size
                        { rows = minimum (map (.rows) sizes)
                        , cols = minimum (map (.cols) sizes)
                        }
                writeTVar sess.lastSize eff
                ws <- readTVar sess.windows
                resizes <- forM (Map.elems ws) $ \win -> do
                    (rects, _) <- windowArrange (windowArea eff) win
                    ps <- readTVar win.panes
                    pure [ (p, rectSize rect)
                         | (pidL, rect) <- rects
                         , Just p <- [Map.lookup pidL ps]
                         ]
                forM_ cs $ \c -> writeTVar c.needsFull True
                bumpDirty st
                pure (Just (concat resizes))
    forM_ (fromMaybe [] work) $ \(pane, sz) -> do
        old <- readTVarIO pane.size
        when (old /= sz) $ do
            atomically $ writeTVar pane.size sz
            Hat.Pty.resize pane.pty sz
            Emu.resize pane.emulator sz
            atomically (bumpDirty st)

rectSize :: Rect -> Size
rectSize r = Size
    { rows = fromIntegral (max 1 (r.endRow - r.startRow))
    , cols = fromIntegral (max 1 (r.endCol - r.startCol))
    }

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
    (frame, cursor) <- case view of
        Nothing -> pure (blankFrame csize, (Pos 0 0, False))
        Just (sess, rects, borders, ps, active) -> do
            let shiftedBorders =
                    [ (p { row = p.row + rowOff }, ch) | (p, ch) <- borders ]
                shiftRect r = r
                    { startRow = r.startRow + rowOff
                    , endRow = r.endRow + rowOff
                    }
                base0 = applyBorders (blankFrame csize) shiftedBorders
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
            pure (withStatus, cur)
    full <- atomically (swapTVar client.needsFull False)
    old <- readIORef client.lastFrame
    oldCursor <- readIORef client.lastCursor
    let ops = if full then fullRedraw frame else diffFrame old frame
        cursorOp = CursorAt (fst cursor) (snd cursor)
        needSend = not (null ops) || cursor /= oldCursor || full
    writeIORef client.lastFrame frame
    writeIORef client.lastCursor cursor
    when needSend $ send client (Draw (ops <> [cursorOp]))
  where
    foldM' z xs f = foldM f z xs

paneOrigin :: [(PaneId, Rect)] -> PaneId -> Pos
paneOrigin rects pidL = case List.lookup pidL rects of
    Just r -> Pos { row = r.startRow, col = r.startCol }
    Nothing -> Pos 0 0

-- | The cells a pane contributes to a frame. Normally its live screen;
-- in copy mode, a viewport over scrollback+screen with the selection
-- reverse-videoed.
paneViewCells :: ServerState -> Pane -> IO (V.Vector (V.Vector Cell.Cell))
paneViewCells st pane = do
    scr <- Emu.snapshot pane.emulator
    mmode <- readTVarIO pane.mode
    case mmode of
        Nothing -> pure scr.cells
        Just s -> do
            hsize <- Emu.scrollbackLength pane.emulator
            opts <- readTVarIO st.options
            let sy = V.length scr.cells
                sx = fromIntegral scr.size.cols
                top = hsize - s.viewportOffY
            rows <- mapM (viewportRow pane scr hsize sx top) [0 .. sy - 1]
            let overlaid = CopyMode.overlaySelection opts.modeKeys top s
                    (V.fromList rows)
                label = "[" <> tshow s.viewportOffY <> "/" <> tshow hsize <> "]"
            pure (stampTopRight label copyIndicatorStyle overlaid)
  where
    padTo sx v = V.generate sx (\c -> fromMaybe Cell.blankCell (v V.!? c))
    viewportRow pane' scr hsize sx top i =
        let a = top + i
        in if a >= hsize
            then pure (padTo sx (fromMaybe V.empty (scr.cells V.!? (a - hsize))))
            else do
                mline <- Emu.scrollbackLine pane'.emulator a
                pure (padTo sx (maybe V.empty V.fromList mline))

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
    scr <- Emu.snapshot pane.emulator
    mmode <- readTVarIO pane.mode
    case mmode of
        Nothing -> pure (place scr.cursor, scr.cursorVisible)
        Just s -> do
            hsize <- Emu.scrollbackLength pane.emulator
            let top = hsize - s.viewportOffY
            case CopyMode.copyCursorPos top (V.length scr.cells) s of
                Just p -> pure (place p, True)
                Nothing -> pure (Pos 0 0, False)
  where
    place p = Pos { row = p.row + origin.row + rowOff
                  , col = p.col + origin.col }

-- Status line -------------------------------------------------------------

statusStyle :: Cell.Style
statusStyle = Cell.defaultStyle
    { Cell.fg = Cell.Indexed 0
    , Cell.bg = Cell.Indexed 2
    }

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
promptCells pr width = lineCells promptStyle width (promptPrefix <> pr.input)
  where
    promptStyle = Cell.defaultStyle
        { Cell.fg = Cell.Indexed 0, Cell.bg = Cell.Indexed 3 }

-- | The screen column of the prompt's edit cursor.
promptCursorCol :: PromptState -> Int
promptCursorCol pr = T.length promptPrefix + pr.cursor

promptPrefix :: Text
promptPrefix = ":"

-- Session-level format environment for the active window and pane.
sessionFormatEnv :: ServerState -> Session -> IO FormatEnv
sessionFormatEnv st sess = do
    hostname <- nodeName <$> getSystemID
    (sname, wEnv, mactive, nclients) <- atomically $ do
        sname <- readTVar sess.name
        mwin <- currentWindow sess
        cur <- readTVar sess.currentIx
        wEnv <- case mwin of
            Nothing -> pure []
            Just win -> do
                wname <- readTVar win.name
                pure [ ("window_index", tshow cur)
                     , ("window_name", wname)
                     ]
        mactive <- maybe (pure Nothing) activePane mwin
        cs <- sessionClients st sess.id
        pure (sname, wEnv, mactive, length cs)
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
    pure . Map.fromList $
        [ ("session_name", sname)
        , ("host", T.pack hostname)
        , ("window_active_clients", tshow nclients)
        , ("window_width", tshow sz.cols)
        , ("window_height", tshow sz.rows)
        ]
        <> wEnv <> pEnv

-- | Copy-mode format variables for a pane: @pane_in_mode@/@pane_mode@,
-- plus @copy_cursor_{x,y,line}@ while in mode.
paneModeEnv :: Pane -> IO [(Text, Text)]
paneModeEnv pane = do
    mmode <- readTVarIO pane.mode
    case mmode of
        Nothing -> pure [("pane_in_mode", "0"), ("pane_mode", "")]
        Just s -> do
            hsize <- Emu.scrollbackLength pane.emulator
            let top = hsize - s.viewportOffY
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
                (shell (T.unpack cmdText)) "")
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
            (wname, bell) <- atomically $
                (,) <$> readTVar win.name <*> readTVar win.bellFlag
            let flags = T.concat
                    [ if ix == cur then "*"
                      else if Just ix == mlast then "-" else ""
                    , if bell then "!" else ""
                    ]
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
            expandFormat st wenv fmt
    left <- T.take opts.statusLeftLength <$> expandFormat st env leftFmt
    right <- T.take opts.statusRightLength <$> expandFormat st env rightFmt
    let body = left <> T.intercalate " " entries
        pad = width - T.length body - T.length right
        line
            | pad >= 0 = body <> T.replicate pad " " <> right
            | otherwise = T.take width (body <> " " <> right)
    pure (lineCells statusStyle width line)

-- Input ---------------------------------------------------------------------

inputLoop :: ServerState -> Client -> IO ()
inputLoop st client = loop
  where
    loop = do
        m <- recvMessage client.sock
        case m of
            Nothing -> pure ()
            Just (Left err) -> logEvent st.logger ProtocolError
                { client = rawClient client.id, err = T.pack err }
            Just (Right msg) -> case msg of
                Input bs -> do
                    handleInput st client bs
                    loop
                Resize sz -> do
                    atomically (writeTVar client.size sz)
                    sid <- readTVarIO client.session
                    applySessionSize st sid
                    loop
                Detach -> send client DetachOk
                Command cmds -> do
                    replies <- runCommands st (Just client) cmds
                    forM_ replies $ \case
                        ROutput out -> showToast st client out
                        RErr e -> showToast st client ("error: " <> e)
                    loop
                ClientHello {} -> pure ()

handleInput :: ServerState -> Client -> B.ByteString -> IO ()
handleInput st client bs = do
    mprompt <- readTVarIO client.prompt
    case mprompt of
        Just pr -> handlePromptInput st client pr bs
        Nothing -> handleKeys st client bs

-- | While the command prompt is open it owns every keystroke: the line
-- editor consumes them until Enter (run and close) or Escape (close).
handlePromptInput
    :: ServerState -> Client -> PromptState -> B.ByteString -> IO ()
handlePromptInput st client pr0 bs = do
    history <- readTVarIO st.cmdHistory
    let step acc k = case acc of
            Prompt.Editing pr -> Prompt.editPrompt history pr k
            done -> done
        result = List.foldl' step (Prompt.Editing pr0) (tokenizeKeys bs)
    case result of
        Prompt.Editing pr -> atomically $ do
            writeTVar client.prompt (Just pr)
            bumpDirty st
        Prompt.Cancel -> atomically $ do
            writeTVar client.prompt Nothing
            bumpDirty st
        Prompt.Submit line -> do
            atomically $ do
                writeTVar client.prompt Nothing
                modifyTVar' st.cmdHistory (Prompt.pushHistory line)
                bumpDirty st
            unless (T.null (T.strip line)) $ do
                replies <- runCommandText st (Just client) line
                forM_ replies $ \case
                    ROutput out -> showToast st client out
                    RErr e -> showToast st client ("error: " <> e)

handleKeys :: ServerState -> Client -> B.ByteString -> IO ()
handleKeys st client bs = do
    opts <- readTVarIO st.options
    km <- readTVarIO st.keymap
    st0 <- readIORef client.keyState
    mpane <- clientActivePane st client
    modeTable <- case mpane of
        Just pane -> fmap (fmap (.keyTable)) (readTVarIO pane.mode)
        Nothing -> pure Nothing
    keys <- mapM (reencodeCursor mpane) (tokenizeKeys bs)
    let (st1, actions) = routeKeys opts.prefix km modeTable st0 keys
    writeIORef client.keyState st1
    forM_ actions $ \case
        Passthrough raw ->
            forM_ mpane $ \pane -> Hat.Pty.writePty pane.pty raw
        RunCommands cmds -> forM_ cmds $ \argv -> do
            replies <- runArgv st (Just client) argv
            forM_ replies $ \case
                ROutput out -> showToast st client out
                RErr e -> showToast st client ("error: " <> e)

-- | Cursor keys ('\ESC[A' vs '\ESCOA') depend on the pane's DECCKM mode,
-- so re-encode them via the pane's emulator instead of forwarding the raw
-- bytes the client's terminal happened to send.
reencodeCursor :: Maybe Pane -> Key -> IO Key
reencodeCursor mpane key = case (mpane, cursorKeyOf key.name) of
    (Just pane, Just ck) -> do
        enc <- Emu.encodeKey pane.emulator ck
        pure Key { name = key.name, raw = enc }
    _ -> pure key
  where
    cursorKeyOf n = case n of
        "Up"    -> Just Emu.CursorUp
        "Down"  -> Just Emu.CursorDown
        "Left"  -> Just Emu.CursorLeft
        "Right" -> Just Emu.CursorRight
        "Home"  -> Just Emu.CursorHome
        "End"   -> Just Emu.CursorEnd
        _       -> Nothing

showToast :: ServerState -> Client -> Text -> IO ()
showToast st client t = do
    atomically $ do
        writeTVar client.toast (Just t)
        bumpDirty st
    void . forkIO $ do
        threadDelay 3_000_000
        atomically $ do
            cur <- readTVar client.toast
            when (cur == Just t) $ do
                writeTVar client.toast Nothing
                bumpDirty st

clientActivePane :: ServerState -> Client -> IO (Maybe Pane)
clientActivePane st client = atomically $ do
    mv <- clientView st client
    case mv of
        Nothing -> pure Nothing
        Just (_, win) -> activePane win

-- | The session and current window of a client, if both exist.
clientView :: ServerState -> Client -> STM (Maybe (Session, Window))
clientView st client = do
    sid <- readTVar client.session
    msess <- Map.lookup sid <$> readTVar st.sessions
    case msess of
        Nothing -> pure Nothing
        Just sess -> do
            mwin <- currentWindow sess
            pure ((,) sess <$> mwin)

-- The command engine ---------------------------------------------------------

data Reply = ROutput Text | RErr Text

runCommandText :: ServerState -> Maybe Client -> Text -> IO [Reply]
runCommandText st mclient input = case parseCommandLine input of
    Left err -> pure [RErr err]
    Right cmds -> runCommands st mclient cmds

runCommands :: ServerState -> Maybe Client -> [[Text]] -> IO [Reply]
runCommands st mclient cmds = concat <$> mapM (runArgv st mclient) cmds

runArgv :: ServerState -> Maybe Client -> [Text] -> IO [Reply]
runArgv _ _ [] = pure []
runArgv st mclient (name : args) = do
    forM_ mclient $ \c -> logEvent st.logger CommandRun
        { client = rawClient c.id, command = T.unwords (name : args) }
    case Map.lookup name commandTable of
        Nothing -> pure [RErr ("unknown command: " <> name)]
        Just impl -> impl st mclient args
            `catch` \(e :: SomeException) ->
                pure [RErr (name <> ": " <> T.pack (show e))]

type CommandImpl = ServerState -> Maybe Client -> [Text] -> IO [Reply]

commandTable :: Map.Map Text CommandImpl
commandTable = Map.fromList $ concatMap expand
    [ (["bind-key", "bind"], cmdBind)
    , (["unbind-key", "unbind"], cmdUnbind)
    , (["set-option", "set", "set-window-option", "setw"], cmdSet)
    , (["show-options", "show", "show-option"], cmdShow)
    , (["source-file", "source"], cmdSourceFile)
    , (["new-window", "neww"], cmdNewWindow)
    , (["select-window", "selectw"], cmdSelectWindow)
    , (["next-window", "next"], cmdNextWindow)
    , (["previous-window", "prev"], cmdPrevWindow)
    , (["last-window", "last"], cmdLastWindow)
    , (["kill-window", "killw"], cmdKillWindow)
    , (["rename-window", "renamew"], cmdRenameWindow)
    , (["split-window", "splitw"], cmdSplitWindow)
    , (["select-pane", "selectp"], cmdSelectPane)
    , (["kill-pane", "killp"], cmdKillPane)
    , (["resize-pane", "resizep"], cmdResizePane)
    , (["last-pane", "lastp"], cmdLastPane)
    , (["detach-client", "detach"], cmdDetachClient)
    , (["send-prefix"], cmdSendPrefix)
    , (["send-keys", "send"], cmdSendKeys)
    , (["copy-mode"], cmdCopyMode)
    , (["command-prompt"], cmdCommandPrompt)
    , (["show-buffer", "showb"], cmdShowBuffer)
    , (["set-buffer", "setb"], cmdSetBuffer)
    , (["list-buffers", "lsb"], cmdListBuffers)
    , (["delete-buffer", "deleteb"], cmdDeleteBuffer)
    , (["save-buffer", "saveb"], cmdSaveBuffer)
    , (["paste-buffer", "pasteb"], cmdPasteBuffer)
    , (["pipe-pane", "pipep"], cmdPipePane)
    , (["new-session", "new"], cmdNewSession)
    , (["attach-session", "attach"], cmdAttachSession)
    , (["kill-session"], cmdKillSession)
    , (["has-session", "has"], cmdHasSession)
    , (["start-server", "start"], cmdStartServer)
    , (["rename-session", "rename"], cmdRenameSession)
    , (["list-sessions", "ls"], cmdListSessions)
    , (["list-windows", "lsw"], cmdListWindows)
    , (["list-panes", "lsp"], cmdListPanes)
    , (["capture-pane", "capturep"], cmdCapturePane)
    , (["resize-window", "resizew"], cmdResizeWindow)
    , (["switch-client", "switchc"], cmdSwitchClient)
    , (["kill-server"], cmdKillServer)
    , (["display-message", "display"], cmdDisplayMessage)
    , (["run-shell", "run"], cmdRunShell)
    , (["if-shell", "if"], cmdIfShell)
    ]
  where
    expand (names, impl) = [(n, impl) | n <- names]

-- getopt-style flag parser: @spec@ lists the letters that take a
-- value. Bundled forms work like tmux: @-dsfoo@ is @-d -s foo@.
-- Returns (value flags as ("-s", value), boolean flags as "-d",
-- positional args).
parseArgs :: [Char] -> [Text] -> ([(Text, Text)], [Text], [Text])
parseArgs spec = go [] []
  where
    go opts flags = \case
        [] -> (opts, flags, [])
        ("--" : rest) -> (opts, flags, rest)   -- end-of-flags separator
        (a : rest)
            | Just bundle <- T.stripPrefix "-" a
            , not (T.null bundle)
            , not (isNumber a) ->
                let (opts', flags', rest') = scanBundle bundle rest
                in go (opts' <> opts) (flags' <> flags) rest'
            | otherwise -> (opts, flags, a : rest)
    scanBundle bundle rest = case T.uncons bundle of
        Nothing -> ([], [], rest)
        Just (c, more)
            | c `elem` spec ->
                let val = fromMaybe more (T.stripPrefix "=" more)
                in case (T.null val, rest) of
                    (False, _) -> ([(dash c, val)], [], rest)
                    (True, v : rest') -> ([(dash c, v)], [], rest')
                    (True, []) -> ([(dash c, "")], [], [])
            | otherwise ->
                let (opts', flags', rest') = scanBundle more rest
                in (opts', dash c : flags', rest')
    dash c = T.pack ['-', c]
    isNumber a = case TR.signed TR.decimal a of
        Right (_ :: Int, restT) -> T.null restT
        Left _ -> False

targetSession :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe Session)
targetSession st mclient mtarget = atomically $ do
    sessions <- readTVar st.sessions
    case mtarget of
        Just t -> do
            let stripped = fromMaybe t (T.stripSuffix ":" t)
            found <- forM (Map.elems sessions) $ \sess -> do
                nm <- readTVar sess.name
                pure $ if nm == stripped
                    || tshow (rawSession sess.id) == stripped
                    || ("$" <> tshow (rawSession sess.id)) == stripped
                    then Just sess else Nothing
            pure (foldr (\m acc -> maybe acc Just m) Nothing found)
        Nothing -> case mclient of
            Just client -> do
                sid <- readTVar client.session
                case Map.lookup sid sessions of
                    Just sess -> pure (Just sess)
                    Nothing -> pure (snd <$> Map.lookupMax sessions)
            Nothing -> pure (snd <$> Map.lookupMax sessions)

withTargetSession
    :: ServerState -> Maybe Client -> Maybe Text
    -> (Session -> IO [Reply]) -> IO [Reply]
withTargetSession st mclient mtarget body = do
    msess <- targetSession st mclient mtarget
    case msess of
        Nothing -> pure [RErr "no such session"]
        Just sess -> body sess

withCurrentWindow
    :: ServerState -> Maybe Client
    -> (Session -> Window -> IO [Reply]) -> IO [Reply]
withCurrentWindow st mclient body = do
    mv <- atomically (maybe (pure Nothing) (clientView st) mclient)
    view <- case mv of
        Just v -> pure (Just v)
        Nothing -> do
            msess <- targetSession st mclient Nothing
            case msess of
                Nothing -> pure Nothing
                Just sess -> do
                    mwin <- atomically (currentWindow sess)
                    pure ((,) sess <$> mwin)
    case view of
        Nothing -> pure [RErr "no current window"]
        Just (sess, win) -> body sess win

-- Command implementations.

cmdBind :: CommandImpl
cmdBind st _ args = do
    let (opts, flags, pos) = parseArgs "TN" args
        table
            | "-n" `elem` flags = "root"
            | Just t <- lookup "-T" opts = t
            | otherwise = "prefix"
    case pos of
        (keyName : rest)
            | Just key <- parseKeyName keyName
            , not (null rest) -> do
                let cmds = splitBinding rest
                atomically $ modifyTVar' st.keymap $
                    Map.insertWith Map.union table
                        (Map.singleton key.name cmds)
                pure []
        (keyName : _) ->
            pure [RErr ("bind: bad key or command: " <> keyName)]
        _ -> pure [RErr "usage: bind [-n] [-T table] key command..."]

-- A binding's command part: one brace block to re-parse, or argv split
-- on ";" tokens (from escaped semicolons).
splitBinding :: [Text] -> [[Text]]
splitBinding = \case
    [block] | T.any (\c -> c == ' ' || c == ';' || c == '\n') block ->
        case parseConfig block of
            Right cmds -> cmds
            Left _ -> [[block]]
    rest -> filter (not . null) (splitOnSemis rest)
  where
    splitOnSemis xs = case break (== ";") xs of
        (before, []) -> [before]
        (before, _ : after) -> before : splitOnSemis after

cmdUnbind :: CommandImpl
cmdUnbind st _ args = do
    let (opts, flags, pos) = parseArgs "T" args
        table
            | "-n" `elem` flags = "root"
            | Just t <- lookup "-T" opts = t
            | otherwise = "prefix"
    case pos of
        [keyName] | Just key <- parseKeyName keyName -> do
            atomically $ modifyTVar' st.keymap $
                Map.adjust (Map.delete key.name) table
            pure []
        _ -> pure [RErr "usage: unbind [-n] [-T table] key"]

cmdSet :: CommandImpl
cmdSet st _ args = do
    let (_, flags, pos) = parseArgs "t" args
        append = "-a" `elem` flags
    case pos of
        (nameT : rest) -> do
            let value = T.unwords rest
            r <- atomically $ do
                opts <- readTVar st.options
                case setOption append opts nameT value of
                    Left err -> pure (Just err)
                    Right opts' -> do
                        writeTVar st.options opts'
                        bumpDirty st
                        pure Nothing
            pure $ case r of
                Just err -> [RErr err]
                Nothing -> []
        [] -> pure [RErr "usage: set [-g] option value"]

cmdShow :: CommandImpl
cmdShow st _ args = do
    let (_, flags, pos) = parseArgs "t" args
        valueOnly = "-v" `elem` flags
        quiet = "-q" `elem` flags
    opts <- readTVarIO st.options
    case pos of
        [name] -> case lookupOption opts name of
            Just v -> pure [ROutput (if valueOnly then v else name <> " " <> v)]
            Nothing
                | quiet -> pure []
                | otherwise -> pure [RErr ("unknown option: " <> name)]
        _ -> pure [RErr "usage: show-options [-gsvq] name"]

lookupOption :: Options -> Text -> Maybe Text
lookupOption opts name = case name of
    "prefix" -> Just opts.prefix
    "base-index" -> Just (tshow opts.baseIndex)
    "pane-base-index" -> Just (tshow opts.paneBaseIndex)
    "history-limit" -> Just (tshow opts.historyLimit)
    "default-terminal" -> Just opts.defaultTerminal
    "word-separators" -> Just opts.wordSeparators
    "status-position" -> Just (case opts.statusPosition of
        StatusTop -> "top"; StatusBottom -> "bottom")
    "mode-keys" -> Just (case opts.modeKeys of
        KeysVi -> "vi"; KeysEmacs -> "emacs")
    "status-left" -> Just opts.statusLeft
    "status-left-length" -> Just (tshow opts.statusLeftLength)
    "status-right" -> Just opts.statusRight
    "status-right-length" -> Just (tshow opts.statusRightLength)
    "window-status-format" -> Just opts.windowStatusFormat
    "window-status-current-format" -> Just opts.windowStatusCurrentFormat
    _
        | "@" `T.isPrefixOf` name -> Map.lookup name opts.user
        | otherwise -> Nothing

-- | Apply a @set-option@. @append@ is tmux's @-a@: for string-valued
-- options it concatenates onto the current value (used to build up
-- @status-right@ across several lines). Unknown non-@\@@ options are
-- rejected so a config never looks supported when its behavior is not
-- yet implemented.
setOption :: Bool -> Options -> Text -> Text -> Either Text Options
setOption append opts name value = case name of
    "prefix" -> case parseKeyName value of
        Just k -> Right opts { prefix = k.name }
        Nothing -> Left ("bad prefix key: " <> value)
    "base-index" -> withInt $ \n -> opts { baseIndex = n }
    "pane-base-index" -> withInt $ \n -> opts { paneBaseIndex = n }
    "history-limit" -> withInt $ \n -> opts { historyLimit = n }
    "default-terminal" -> Right opts { defaultTerminal = value }
    "word-separators" -> Right opts { wordSeparators = value }
    "status-position" -> case value of
        "top" -> Right opts { statusPosition = StatusTop }
        "bottom" -> Right opts { statusPosition = StatusBottom }
        _ -> Left "status-position: top or bottom"
    "mode-keys" -> case value of
        "vi" -> Right opts { modeKeys = KeysVi }
        "emacs" -> Right opts { modeKeys = KeysEmacs }
        _ -> Left "mode-keys: vi or emacs"
    "status-left" -> Right opts { statusLeft = withAppend opts.statusLeft }
    "status-left-length" -> withInt $ \n -> opts { statusLeftLength = n }
    "status-right" -> Right opts { statusRight = withAppend opts.statusRight }
    "status-right-length" -> withInt $ \n -> opts { statusRightLength = n }
    "window-status-format" ->
        Right opts { windowStatusFormat = withAppend opts.windowStatusFormat }
    "window-status-current-format" ->
        Right opts { windowStatusCurrentFormat =
            withAppend opts.windowStatusCurrentFormat }
    _
        | "@" `T.isPrefixOf` name ->
            Right opts { user = Map.insert name value opts.user }
        | otherwise -> Left ("unimplemented option: " <> name)
  where
    withInt f = case TR.decimal value of
        Right (n, restT) | T.null restT -> Right (f n)
        _ -> Left (name <> ": not a number: " <> value)
    withAppend old = if append then old <> value else value

cmdSourceFile :: CommandImpl
cmdSourceFile st mclient args = case pos of
    [path] -> do
        let p = T.unpack path
        exists <- doesFileExist p
        if not exists
            then if "-q" `elem` flags
                then pure []
                else pure [RErr ("no such file: " <> path)]
            else do
                contents <- TIO.readFile p
                case parseConfig contents of
                    Left err -> pure [RErr err]
                    Right cmds
                        -- -n: check syntax, do not execute
                        | "-n" `elem` flags -> pure []
                        | otherwise ->
                            concat <$> mapM (runArgv st mclient) cmds
    _ -> pure [RErr "usage: source-file path"]
  where
    (_, flags, pos) = parseArgs "" args

cmdNewWindow :: CommandImpl
cmdNewWindow st mclient args = do
    let (opts, flags, pos) = parseArgs "nct" args
    withTargetSession st mclient Nothing $ \sess -> do
        eff <- readTVarIO sess.lastSize
        srvOpts <- readTVarIO st.options
        let shellCmd = maybe "/bin/sh" T.unpack
                (List.lookup "SHELL" sess.environ)
            mrun = case pos of
                [] -> Nothing
                ws -> Just (T.unwords ws)
        dir <- case lookup "-c" opts of
            Nothing -> pure sess.startCwd
            Just d -> do
                env <- sessionFormatEnv st sess
                T.unpack <$> expandFormat st env d
        (win, pane) <- newWindowWithPane st sess.id shellCmd mrun dir
            sess.environ (windowArea eff)
        forM_ (lookup "-n" opts) $ \nm -> atomically (writeTVar win.name nm)
        atomically $ do
            ws <- readTVar sess.windows
            cur <- readTVar sess.currentIx
            let requested = do
                    t <- lookup "-t" opts
                    case TR.decimal t of
                        Right (n, restT) | T.null restT -> Just n
                        _ -> Nothing
                nextFreeFrom n = until (\i -> not (Map.member i ws)) (+ 1) n
                ix = case requested of
                    Just n
                        | "-a" `elem` flags -> nextFreeFrom (n + 1)
                        | otherwise -> n
                    Nothing
                        | "-a" `elem` flags -> nextFreeFrom (cur + 1)
                        | otherwise -> nextFreeFrom srvOpts.baseIndex
                ix' = if Map.member ix ws then nextFreeFrom srvOpts.baseIndex else ix
            modifyTVar' sess.windows (Map.insert ix' win)
            unless ("-d" `elem` flags) $ do
                writeTVar sess.lastIx (Just cur)
                writeTVar sess.currentIx ix'
            bumpDirty st
        startPaneReader st sess.id win pane
        applySessionSize st sess.id
        pure []

cmdSelectWindow :: CommandImpl
cmdSelectWindow st mclient args = do
    let (opts, _, pos) = parseArgs "t" args
        target = case (lookup "-t" opts, pos) of
            (Just t, _) -> Just t
            (Nothing, [t]) -> Just t
            _ -> Nothing
    mres <- resolveWindowTarget st mclient target
    case mres of
        Nothing -> pure [RErr "usage: select-window -t index"]
        Just (sess, ix) -> do
            atomically (switchTo st sess ix)
            pure []

-- Accepts @[session][:window]@ where session may be a name or @$id@ and
-- window may be a number, @$@ for the last window, or omitted to mean
-- the session's current window. A bare token without @:@ is a window
-- spec in the current session (or a session spec if it starts with @$@).
resolveWindowTarget
    :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe (Session, Int))
resolveWindowTarget st mclient mtarget = case mtarget of
    Nothing -> do
        msess <- targetSession st mclient Nothing
        traverse currentPair msess
    Just t
        | ":" `T.isInfixOf` t -> withColon t
        | "$" `T.isPrefixOf` t -> do
            msess <- targetSession st mclient (Just t)
            traverse currentPair msess
        | otherwise -> do  -- bare window spec in current session
            msess <- targetSession st mclient Nothing
            case msess of
                Nothing -> pure Nothing
                Just sess -> do
                    mix <- parseWinIx sess t
                    pure $ (,) sess <$> mix
  where
    withColon t =
        let (s, rest) = T.break (== ':') t
            w = T.drop 1 rest
        in do
            msess <- targetSession st mclient
                (if T.null s then Nothing else Just s)
            case msess of
                Nothing -> pure Nothing
                Just sess
                    | T.null w -> Just <$> currentPair sess
                    | otherwise -> do
                        mix <- parseWinIx sess w
                        pure $ (,) sess <$> mix
    currentPair sess = (,) sess <$> readTVarIO sess.currentIx
    parseWinIx sess "$" = do
        ws <- readTVarIO sess.windows
        pure (fst <$> Map.lookupMax ws)
    parseWinIx _ w = pure $ case TR.decimal w of
        Right (n, rest) | T.null rest -> Just n
        _ -> Nothing

switchTo :: ServerState -> Session -> Int -> STM ()
switchTo st sess ix = do
    ws <- readTVar sess.windows
    cur <- readTVar sess.currentIx
    when (ix /= cur) $ forM_ (Map.lookup ix ws) $ \win -> do
        writeTVar sess.lastIx (Just cur)
        writeTVar sess.currentIx ix
        writeTVar win.bellFlag False
        bumpDirty st

cmdNextWindow, cmdPrevWindow, cmdLastWindow :: CommandImpl
cmdNextWindow st mclient _ = cycleWindow st mclient 1
cmdPrevWindow st mclient _ = cycleWindow st mclient (-1)
cmdLastWindow st mclient _ =
    withTargetSession st mclient Nothing $ \sess -> do
        atomically $ do
            mlast <- readTVar sess.lastIx
            forM_ mlast (switchTo st sess)
        pure []

cycleWindow :: ServerState -> Maybe Client -> Int -> IO [Reply]
cycleWindow st mclient step =
    withTargetSession st mclient Nothing $ \sess -> do
        atomically $ do
            ws <- readTVar sess.windows
            cur <- readTVar sess.currentIx
            let ixs = Map.keys ws
            case ixs of
                [] -> pure ()
                _ -> do
                    let n = length ixs
                        curPos = fromMaybe 0 (List.elemIndex cur ixs)
                        ix = ixs !! ((curPos + step + n) `mod` n)
                    switchTo st sess ix
        pure []

-- Only @-p@ (print to stdout, plain text) is supported so far; escape,
-- range, and hyperlink flags are ignored — enough for the light-touch
-- @capturep -p@ uses in upstream tests.
cmdCapturePane :: CommandImpl
cmdCapturePane st mclient _ = do
    withCurrentWindow st mclient $ \_ win -> do
        mactive <- atomically (activePane win)
        case mactive of
            Nothing -> pure []
            Just pane -> do
                scr <- Emu.snapshot pane.emulator
                let rows = V.toList scr.cells
                    rowText r = T.stripEnd . T.concat
                        $ [ c.text | c <- V.toList r ]
                    body = T.intercalate "\n" (map rowText rows)
                pure [ROutput body]

cmdResizeWindow :: CommandImpl
cmdResizeWindow st mclient args = do
    let (opts, _, _) = parseArgs "txy" args
        parseInt t = case TR.decimal t of
            Right (n, rest) | T.null rest -> Just n
            _ -> Nothing
    withTargetSession st mclient (lookup "-t" opts) $ \sess -> do
        current <- readTVarIO sess.lastSize
        let sz = current
                { cols = fromMaybe current.cols (parseInt =<< lookup "-x" opts)
                , rows = fromMaybe current.rows (parseInt =<< lookup "-y" opts)
                }
        atomically $ writeTVar sess.lastSize sz
        applySessionSize st sess.id
        pure []

cmdKillWindow :: CommandImpl
cmdKillWindow st mclient _ =
    withCurrentWindow st mclient $ \_ win -> do
        ps <- readTVarIO win.panes
        forM_ (Map.elems ps) $ \p -> Hat.Pty.closePty p.pty
        pure []

cmdRenameWindow :: CommandImpl
cmdRenameWindow st mclient args = case args of
    [nm] -> withCurrentWindow st mclient $ \_ win -> do
        atomically $ do
            writeTVar win.name nm
            bumpDirty st
        pure []
    _ -> pure [RErr "usage: rename-window name"]

cmdSplitWindow :: CommandImpl
cmdSplitWindow st mclient args = do
    let (opts, flags, pos) = parseArgs "ctlp" args
        orient
            | "-h" `elem` flags = LeftRight
            | otherwise = TopBottom
        before = "-b" `elem` flags
        mrun = case pos of
            [] -> Nothing
            ws -> Just (T.unwords ws)
    withCurrentWindow st mclient $ \sess win -> do
        mactive <- atomically (activePane win)
        case mactive of
            Nothing -> pure [RErr "no active pane"]
            Just active -> do
                eff <- readTVarIO sess.lastSize
                (rects, _) <- atomically (windowArrange (windowArea eff) win)
                let mrect = List.lookup active.id rects
                    fits = case (orient, mrect) of
                        (LeftRight, Just r) -> r.endCol - r.startCol >= 5
                        (TopBottom, Just r) -> r.endRow - r.startRow >= 5
                        _ -> False
                if not fits
                    then pure [RErr "create pane failed: pane too small"]
                    else do
                        pid <- PaneId <$> atomically (freshId st.nextPane)
                        dir <- case lookup "-c" opts of
                            Just d -> do
                                env <- sessionFormatEnv st sess
                                T.unpack <$> expandFormat st env d
                            Nothing -> paneCurrentPath active
                        let shellCmd = maybe "/bin/sh" T.unpack
                                (List.lookup "SHELL" sess.environ)
                        pane <- spawnPane st pid sess.id shellCmd mrun dir
                            sess.environ (windowArea eff)
                        atomically $ do
                            modifyTVar' win.panes (Map.insert pane.id pane)
                            modifyTVar' win.layout
                                (splitLeaf active.id orient before pane.id)
                            lastA <- readTVar win.activeId
                            writeTVar win.lastActive (Just lastA)
                            writeTVar win.activeId pane.id
                            writeTVar win.zoomed Nothing
                            bumpDirty st
                        startPaneReader st sess.id win pane
                        applySessionSize st sess.id
                        pure []

-- | Where is a pane's child process now? /proc, with a fallback.
paneCurrentPath :: Pane -> IO FilePath
paneCurrentPath pane = do
    r <- try (PFiles.readSymbolicLink
        ("/proc/" <> show (Hat.Pty.pid pane.pty) <> "/cwd"))
    pure $ case r of
        Left (_ :: IOException) -> pane.startCwd
        Right dir -> dir

cmdSelectPane :: CommandImpl
cmdSelectPane st mclient args = do
    let (opts, flags, _) = parseArgs "tT" args
        mdir
            | "-L" `elem` flags = Just DirLeft
            | "-R" `elem` flags = Just DirRight
            | "-U" `elem` flags = Just DirUp
            | "-D" `elem` flags = Just DirDown
            | otherwise = Nothing
        parseNum t = case TR.decimal t of
            Right (n, rest) | T.null rest -> Just n
            _ -> Nothing
        -- Bare @-t N@ picks the Nth pane in the current window (0-based).
        mIndex = lookup "-t" opts >>= parseNum
    case mdir of
        Nothing
            | "-M" `elem` flags -> do
                atomically $ writeTVar st.markedPane Nothing >> bumpDirty st
                pure []
            | "-m" `elem` flags -> do
                mp <- targetPane st mclient (lookup "-t" opts)
                forM_ mp $ \pane -> atomically $
                    writeTVar st.markedPane (Just pane.id) >> bumpDirty st
                pure []
            | "-l" `elem` flags -> cmdLastPane st mclient []
            | Just n <- mIndex ->
                withCurrentWindow st mclient $ \_ win -> do
                    atomically $ do
                        ps <- readTVar win.panes
                        let ordered = Map.elems ps
                        case drop n ordered of
                            (p : _) -> do
                                active <- readTVar win.activeId
                                writeTVar win.lastActive (Just active)
                                writeTVar win.activeId p.id
                                bumpDirty st
                            [] -> pure ()
                    pure []
            | otherwise -> pure [RErr "usage: select-pane -L|-R|-U|-D|-l|-t index"]
        Just dir -> withCurrentWindow st mclient $ \sess win -> do
            atomically $ do
                eff <- readTVar sess.lastSize
                (rects, _) <- windowArrange (windowArea eff) win
                active <- readTVar win.activeId
                forM_ (neighbor rects active dir) $ \next -> do
                    writeTVar win.lastActive (Just active)
                    writeTVar win.activeId next
                    bumpDirty st
            pure []

cmdLastPane :: CommandImpl
cmdLastPane st mclient _ =
    withCurrentWindow st mclient $ \_ win -> do
        atomically $ do
            mlast <- readTVar win.lastActive
            ps <- readTVar win.panes
            forM_ mlast $ \lastP -> when (Map.member lastP ps) $ do
                cur <- readTVar win.activeId
                writeTVar win.lastActive (Just cur)
                writeTVar win.activeId lastP
                bumpDirty st
        pure []

cmdKillPane :: CommandImpl
cmdKillPane st mclient _ =
    withCurrentWindow st mclient $ \_ win -> do
        mpane <- atomically (activePane win)
        forM_ mpane $ \pane -> Hat.Pty.closePty pane.pty
        pure []

cmdResizePane :: CommandImpl
cmdResizePane st mclient args = do
    let (opts, flags, pos) = parseArgs "t" args
        delta = case pos of
            (n : _) | Right (v, restT) <- TR.decimal n, T.null restT -> v
            _ -> 1
    if "-Z" `elem` flags
        then zoomTarget st mclient (lookup "-t" opts) >> pure []
        else do
            let mdir
                    | "-L" `elem` flags = Just DirLeft
                    | "-R" `elem` flags = Just DirRight
                    | "-U" `elem` flags = Just DirUp
                    | "-D" `elem` flags = Just DirDown
                    | otherwise = Nothing
            case mdir of
                Nothing ->
                    pure [RErr "usage: resize-pane -L|-R|-U|-D [n] | -Z"]
                Just dir -> withCurrentWindow st mclient $ \sess win -> do
                    atomically $ do
                        eff <- readTVar sess.lastSize
                        active <- readTVar win.activeId
                        modifyTVar' win.layout
                            (resizeSplit active dir delta
                                (sizeRect (windowArea eff)))
                        bumpDirty st
                    applySessionSize st sess.id
                    pure []

-- | Toggle zoom on the caller's current window. With a @-t@ target the
-- targeted pane becomes active first, so @resize-pane -t ! -Z@ zooms the
-- alternate pane (as the config's @Z@ binding intends).
zoomTarget :: ServerState -> Maybe Client -> Maybe Text -> IO ()
zoomTarget st mclient mtok = do
    mtarget <- targetPane st mclient mtok
    void . withCurrentWindow st mclient $ \sess win -> do
        atomically $ do
            ps <- readTVar win.panes
            forM_ mtarget $ \pane -> when (Map.member pane.id ps) $ do
                active <- readTVar win.activeId
                when (active /= pane.id) $ do
                    writeTVar win.lastActive (Just active)
                    writeTVar win.activeId pane.id
            mz <- readTVar win.zoomed
            newActive <- readTVar win.activeId
            writeTVar win.zoomed $ case mz of
                Just _ -> Nothing
                Nothing -> Just newActive
            bumpDirty st
        applySessionSize st sess.id
        pure []

cmdDetachClient :: CommandImpl
cmdDetachClient _ mclient _ = do
    forM_ mclient $ \client -> send client DetachOk
    pure []

cmdSendPrefix :: CommandImpl
cmdSendPrefix st mclient _ = do
    forM_ mclient $ \client -> do
        opts <- readTVarIO st.options
        forM_ (parseKeyName opts.prefix) $ \key -> do
            mpane <- clientActivePane st client
            forM_ mpane $ \pane -> Hat.Pty.writePty pane.pty key.raw
    pure []

cmdSendKeys :: CommandImpl
cmdSendKeys st mclient args = do
    let (opts, flags, pos) = parseArgs "tN" args
        literal = "-l" `elem` flags
        modeCmd = "-X" `elem` flags
    mpane <- targetPane st mclient (lookup "-t" opts)
    forM_ mpane $ \pane ->
        if modeCmd
            then case pos of
                (name : cmdArgs) -> runCopyModeCommand st pane name cmdArgs
                [] -> pure ()
            else Hat.Pty.writePty pane.pty
                (B.concat (map (argBytes literal) pos))
    pure []
  where
    argBytes True a = TE.encodeUtf8 a
    argBytes False a = case parseKeyName a of
        Just k -> k.raw
        Nothing -> TE.encodeUtf8 a

runCopyModeCommand :: ServerState -> Pane -> Text -> [Text] -> IO ()
runCopyModeCommand st pane name cmdArgs = do
    mmode <- readTVarIO pane.mode
    case mmode of
        Nothing -> pure ()  -- not in copy mode; -X is a no-op
        Just state -> case Map.lookup name CopyMode.handlers of
            Nothing -> pure ()
            Just h -> do
                result <- h st pane state cmdArgs
                result' <- traverse (scrollPaneToCursor pane) result
                atomically $ do
                    writeTVar pane.mode result'
                    bumpDirty st

-- | Re-center a pane's copy-mode viewport on its cursor after a motion.
scrollPaneToCursor :: Pane -> CopyModeState -> IO CopyModeState
scrollPaneToCursor pane s = do
    hsize <- Emu.scrollbackLength pane.emulator
    scr <- Emu.snapshot pane.emulator
    pure (CopyMode.scrollToCursor hsize (V.length scr.cells) s)

-- | Resolve the pane a command should act on from its @-t target@.
-- @!@ is the current window's last-active pane, @~@/@{marked}@ the
-- marked pane, @%N@ a pane by id anywhere; otherwise the current pane
-- of the caller's window.
targetPane :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe Pane)
targetPane st mclient mtok = case parsePaneTarget mtok of
    PaneById n -> atomically (findPaneById st n)
    PaneMarked -> atomically $ do
        mpid <- readTVar st.markedPane
        maybe (pure Nothing) (findPaneById st . rawPane) mpid
    tgt -> do
        mwin <- currentWindowOf st mclient
        case mwin of
            Nothing -> pure Nothing
            Just win -> atomically $ do
                ps <- readTVar win.panes
                case tgt of
                    PaneLast -> do
                        ml <- readTVar win.lastActive
                        pure (ml >>= (`Map.lookup` ps))
                    _ -> do
                        a <- readTVar win.activeId
                        pure (Map.lookup a ps)

-- | The window a command acts in: the caller's current window, or (for
-- a clientless control command) the current window of the most-recent
-- session.
currentWindowOf :: ServerState -> Maybe Client -> IO (Maybe Window)
currentWindowOf st mclient = do
    mv <- atomically (maybe (pure Nothing) (clientView st) mclient)
    case mv of
        Just (_, win) -> pure (Just win)
        Nothing -> do
            msess <- targetSession st mclient Nothing
            case msess of
                Nothing -> pure Nothing
                Just sess -> atomically (currentWindow sess)

-- | Find a pane by its numeric id across every session and window.
findPaneById :: ServerState -> Int -> STM (Maybe Pane)
findPaneById st n = do
    sessions <- readTVar st.sessions
    panes <- fmap concat . forM (Map.elems sessions) $ \sess -> do
        ws <- readTVar sess.windows
        fmap concat . forM (Map.elems ws) $ windowPanes
    pure (List.find (\p -> rawPane p.id == n) panes)

cmdCopyMode :: CommandImpl
cmdCopyMode st mclient args = do
    let (opts, flags, _) = parseArgs "st" args
        quit = "-q" `elem` flags
    mpane <- targetPane st mclient (lookup "-t" opts)
    case mpane of
        Nothing -> pure []
        Just pane
            | quit -> do
                atomically $ do
                    writeTVar pane.mode Nothing
                    bumpDirty st
                pure []
            | otherwise -> do
                scr <- Emu.snapshot pane.emulator
                hsize <- Emu.scrollbackLength pane.emulator
                srvOpts <- readTVarIO st.options
                let table = case srvOpts.modeKeys of
                        KeysVi -> "copy-mode-vi"
                        KeysEmacs -> "copy-mode"
                    startRow = hsize + scr.cursor.row
                    startCol = scr.cursor.col
                    state = CopyModeState
                        { cursorRow = startRow
                        , cursorCol = startCol
                        , selection = Nothing
                        , keyTable = table
                        , viewportOffY = 0
                        }
                atomically $ do
                    writeTVar pane.mode (Just state)
                    bumpDirty st
                pure []

-- | Open the interactive command prompt on the invoking client.
cmdCommandPrompt :: CommandImpl
cmdCommandPrompt st mclient _args = do
    forM_ mclient $ \client -> atomically $ do
        writeTVar client.prompt (Just Prompt.emptyPrompt)
        bumpDirty st
    pure []

cmdShowBuffer :: CommandImpl
cmdShowBuffer st _ args = do
    let (opts, _, _) = parseArgs "b" args
    bufs <- readTVarIO st.buffers
    pure $ case bufferBody (lookup "-b" opts) bufs of
        Nothing -> [RErr "no buffers"]
        Just body -> [ROutput body]

cmdSetBuffer :: CommandImpl
cmdSetBuffer st _ args = do
    let (opts, flags, pos) = parseArgs "bn" args
        appendMode = "-a" `elem` flags
        mname = lookup "-b" opts
        body = T.unwords pos
    if null pos
        then pure [RErr "usage: set-buffer [-a] [-b name] data"]
        else atomically $ do
            bufs <- readTVar st.buffers
            case mname of
                Just name -> do
                    let existing = lookupBuffer name bufs
                        newBody = case (appendMode, existing) of
                            (True, Just prev) -> prev <> body
                            _ -> body
                        others = Seq.filter ((/= name) . fst) bufs
                    writeTVar st.buffers ((name, newBody) Seq.<| others)
                Nothing -> do
                    n <- readTVar st.nextBuffer
                    writeTVar st.nextBuffer (n + 1)
                    let name = "buffer" <> T.pack (show n)
                    writeTVar st.buffers ((name, body) Seq.<| bufs)
            bumpDirty st
            pure []

cmdListBuffers :: CommandImpl
cmdListBuffers st _ _ = do
    bufs <- readTVarIO st.buffers
    pure . map row $ toList' bufs
  where
    row (name, body) =
        ROutput (name <> ": " <> tshow (T.length body) <> " bytes")
    toList' s = case Seq.viewl s of
        Seq.EmptyL -> []
        x Seq.:< xs -> x : toList' xs

cmdDeleteBuffer :: CommandImpl
cmdDeleteBuffer st _ args = do
    let (opts, _, _) = parseArgs "b" args
    atomically $ do
        bufs <- readTVar st.buffers
        writeTVar st.buffers (dropBuffer (lookup "-b" opts) bufs)
        pure []

-- | Write the top (or named) buffer to a file. @-a@ appends; the path
-- may start with @~/@.
cmdSaveBuffer :: CommandImpl
cmdSaveBuffer st _ args = do
    let (opts, flags, pos) = parseArgs "b" args
        appendMode = "-a" `elem` flags
    case pos of
        [] -> pure [RErr "usage: save-buffer [-a] [-b name] path"]
        (rawPath : _) -> do
            bufs <- readTVarIO st.buffers
            case bufferBody (lookup "-b" opts) bufs of
                Nothing -> pure [RErr "no buffers"]
                Just body -> do
                    path <- expandTilde (T.unpack rawPath)
                    let write = if appendMode then TIO.appendFile else TIO.writeFile
                    r <- try (write path body)
                    pure $ case r of
                        Left (e :: IOException) -> [RErr (T.pack (show e))]
                        Right () -> []

-- | Paste the top (or named) buffer into a pane's pty. @-d@ deletes the
-- buffer afterwards, @-p@ wraps it in bracketed-paste markers, @-r@
-- turns carriage returns into newlines.
cmdPasteBuffer :: CommandImpl
cmdPasteBuffer st mclient args = do
    let (opts, flags, _) = parseArgs "bt" args
        del = "-d" `elem` flags
        bracketed = "-p" `elem` flags
        crToNl = "-r" `elem` flags
        mname = lookup "-b" opts
    bufs <- readTVarIO st.buffers
    case bufferBody mname bufs of
        Nothing -> pure [RErr "no buffers"]
        Just body0 -> do
            mpane <- targetPane st mclient (lookup "-t" opts)
            case mpane of
                Nothing -> pure [RErr "no target pane"]
                Just pane -> do
                    let body = if crToNl
                            then T.map (\c -> if c == '\r' then '\n' else c) body0
                            else body0
                        payload
                            | bracketed = "\ESC[200~" <> body <> "\ESC[201~"
                            | otherwise = body
                    Hat.Pty.writePty pane.pty (TE.encodeUtf8 payload)
                    when del $ atomically $ do
                        cur <- readTVar st.buffers
                        writeTVar st.buffers (dropBuffer mname cur)
                    pure []

-- | @pipe-pane [-IOo] [-t target] [command]@. With no command (or @-o@
-- while already piping) it stops the pane's pipe. Otherwise it spawns
-- @sh -c command@: @-O@ (the default) feeds pane output to the process's
-- stdin, @-I@ feeds the process's stdout back into the pane.
cmdPipePane :: CommandImpl
cmdPipePane st mclient args = do
    let (opts, flags, pos) = parseArgs "t" args
        hasI = "-I" `elem` flags
        hasO = "-O" `elem` flags
        wantO = hasO || not hasI   -- default direction is -O
        toggle = "-o" `elem` flags
        cmd = T.strip (T.unwords pos)
    mpane <- targetPane st mclient (lookup "-t" opts)
    case mpane of
        Nothing -> pure []
        Just pane -> do
            wasPiping <- isJust <$> readTVarIO pane.pipe
            stopPipe pane
            if T.null cmd || (toggle && wasPiping)
                then pure []
                else startPipe pane (T.unpack cmd) wantO hasI >> pure []

-- Spawn the pipe subprocess and record it on the pane.
startPipe :: Pane -> String -> Bool -> Bool -> IO ()
startPipe pane cmd wantO wantI = do
    (mIn, mOut, _, ph) <- createProcess (shell cmd)
        { std_in  = if wantO then CreatePipe else Inherit
        , std_out = if wantI then CreatePipe else Inherit
        }
    rtid <- case (wantI, mOut) of
        (True, Just hout) -> Just <$> forkIO (pumpPipeOutput pane hout)
        _ -> pure Nothing
    atomically $ writeTVar pane.pipe $ Just PipeHandle
        { process = ph
        , toStdin = if wantO then mIn else Nothing
        , reader = rtid
        }

-- Read the process's stdout and write it into the pane's pty (@-I@).
pumpPipeOutput :: Pane -> Handle -> IO ()
pumpPipeOutput pane hout = loop `catch` \(_ :: SomeException) -> pure ()
  where
    loop = do
        chunk <- B8.hGetSome hout 4096
        unless (B8.null chunk) $ do
            Hat.Pty.writePty pane.pty chunk
            loop

-- Feed a chunk of pane output to the pipe subprocess (@-O@).
forwardToPipe :: Pane -> B.ByteString -> IO ()
forwardToPipe pane bs = do
    mp <- readTVarIO pane.pipe
    forM_ mp $ \ph -> forM_ ph.toStdin $ \hdl ->
        (B8.hPut hdl bs >> hFlush hdl)
            `catch` \(_ :: SomeException) -> pure ()

-- Stop and reap any pipe subprocess on the pane.
stopPipe :: Pane -> IO ()
stopPipe pane = do
    mp <- atomically $ do
        m <- readTVar pane.pipe
        writeTVar pane.pipe Nothing
        pure m
    forM_ mp $ \ph -> do
        forM_ ph.reader killThread
        forM_ ph.toStdin $ \hdl ->
            hClose hdl `catch` \(_ :: SomeException) -> pure ()
        terminateProcess ph.process `catch` \(_ :: SomeException) -> pure ()
        void . forkIO $
            void (waitForProcess ph.process)
                `catch` \(_ :: SomeException) -> pure ()

-- | @HOME@-relative @~/@ prefix expansion for buffer paths.
expandTilde :: FilePath -> IO FilePath
expandTilde ('~' : '/' : rest) = do
    env <- getEnvironment
    pure $ maybe ('~' : '/' : rest) (\h -> h <> "/" <> rest) (lookup "HOME" env)
expandTilde p = pure p

-- | The top buffer, or a named one.
bufferBody :: Maybe Text -> Seq (Text, Text) -> Maybe Text
bufferBody mname bufs = case mname of
    Just name -> lookupBuffer name bufs
    Nothing -> case bufs of
        Seq.Empty -> Nothing
        (_, body) Seq.:<| _ -> Just body

-- | Drop the top buffer, or a named one.
dropBuffer :: Maybe Text -> Seq (Text, Text) -> Seq (Text, Text)
dropBuffer mname bufs = case mname of
    Just name -> Seq.filter ((/= name) . fst) bufs
    Nothing -> case bufs of
        Seq.Empty -> bufs
        _ Seq.:<| rest -> rest

lookupBuffer :: Text -> Seq (Text, Text) -> Maybe Text
lookupBuffer name = go
  where
    go s = case Seq.viewl s of
        Seq.EmptyL -> Nothing
        (n, b) Seq.:< rest
            | n == name -> Just b
            | otherwise -> go rest

cmdNewSession :: CommandImpl
cmdNewSession st mclient args = do
    let (opts, flags, pos) = parseArgs "sctnxy" args
        mname = lookup "-s" opts
        mrun = case pos of
            [] -> Nothing
            ws -> Just (T.unwords ws)
    dup <- case mname of
        Nothing -> pure False
        Just nm -> atomically $ do
            sessions <- readTVar st.sessions
            names <- mapM (\s -> readTVar s.name) (Map.elems sessions)
            pure (nm `elem` names)
    if dup
        then pure [RErr ("duplicate session: " <> fromMaybe "" mname)]
        else do
            (environ, dir, baseSz) <- case mclient of
                Just c -> do
                    csz <- readTVarIO c.size
                    pure (c.env, T.unpack c.cwd, csz)
                Nothing -> do
                    -- Config-loaded or otherwise clientless: inherit the
                    -- server process env so shells find PATH, SHELL, etc.
                    procEnv <- getEnvironment
                    pure ( [(T.pack k, T.pack v) | (k, v) <- procEnv]
                         , "/"
                         , Size { rows = 24, cols = 80 } )
            let dir' = maybe dir T.unpack (lookup "-c" opts)
                parseInt t = case TR.decimal t of
                    Right (n, rest) | T.null rest -> Just n
                    _ -> Nothing
                sz = baseSz
                    { cols = fromMaybe baseSz.cols (parseInt =<< lookup "-x" opts)
                    , rows = fromMaybe baseSz.rows (parseInt =<< lookup "-y" opts)
                    }
            sess <- createSession st mname mrun environ dir' sz
            atomically $ do
                writeTVar st.everAttached True
                forM_ (lookup "-n" opts) $ \wname -> do
                    ws <- readTVar sess.windows
                    forM_ (Map.elems ws) $ \w -> writeTVar w.name wname
            unless ("-d" `elem` flags) $
                forM_ mclient $ \client -> switchClientTo st client sess
            pure []

switchClientTo :: ServerState -> Client -> Session -> IO ()
switchClientTo st client sess = do
    old <- readTVarIO client.session
    atomically $ do
        when (old /= sess.id) $ do
            writeTVar client.lastSession (Just old)
            writeTVar client.session sess.id
        writeTVar client.needsFull True
        bumpDirty st
    applySessionSize st old
    applySessionSize st sess.id

cmdAttachSession :: CommandImpl
cmdAttachSession st mclient args = do
    let (opts, _, _) = parseArgs "tc" args
    withTargetSession st mclient (lookup "-t" opts) $ \sess ->
        case mclient of
            Just client -> switchClientTo st client sess >> pure []
            Nothing -> pure [RErr "no client to attach"]

cmdKillSession :: CommandImpl
cmdKillSession st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    withTargetSession st mclient (lookup "-t" opts) $ \sess -> do
        panes <- atomically $ do
            ws <- readTVar sess.windows
            fmap concat . forM (Map.elems ws) $ windowPanes
        forM_ panes $ \p -> Hat.Pty.closePty p.pty
        pure []

cmdHasSession :: CommandImpl
cmdHasSession st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    msess <- targetSession st mclient (lookup "-t" opts)
    pure $ case msess of
        Just _ -> []
        Nothing -> [RErr $ "can't find session: "
            <> fromMaybe "" (lookup "-t" opts)]

-- The server is necessarily running by the time this executes.
cmdStartServer :: CommandImpl
cmdStartServer _ _ _ = pure []

cmdRenameSession :: CommandImpl
cmdRenameSession st mclient args = case args of
    [nm] -> withTargetSession st mclient Nothing $ \sess -> do
        atomically $ do
            writeTVar sess.name nm
            bumpDirty st
        pure []
    _ -> pure [RErr "usage: rename-session name"]

cmdListSessions :: CommandImpl
cmdListSessions st _ args = do
    let (opts, _, _) = parseArgs "F" args
    sessions <- Map.elems <$> readTVarIO st.sessions
    lines' <- case lookup "-F" opts of
        Just fmt -> forM sessions $ \sess -> do
            env <- sessionFormatEnv st sess
            expandFormat st env fmt
        Nothing -> forM sessions $ \sess -> atomically $ do
            nm <- readTVar sess.name
            ws <- readTVar sess.windows
            pure (nm <> ": " <> tshow (Map.size ws) <> " windows")
    pure (map ROutput lines')

cmdListWindows :: CommandImpl
cmdListWindows st mclient args = do
    let (opts, flags, _) = parseArgs "Ft" args
        mfmt = lookup "-F" opts
        allSessions = "-a" `elem` flags
    sessions <- if allSessions
        then Map.elems <$> readTVarIO st.sessions
        else do
            msess <- targetSession st mclient (lookup "-t" opts)
            pure (maybe [] pure msess)
    fmap (map ROutput . concat) . forM sessions $ \sess -> do
        ws <- Map.toAscList <$> readTVarIO sess.windows
        cur <- readTVarIO sess.currentIx
        forM ws $ \(ix, win) -> case mfmt of
            Just fmt -> do
                env <- windowFormatEnv st sess ix win
                expandFormat st env fmt
            Nothing -> atomically $ do
                nm <- readTVar win.name
                ps <- readTVar win.panes
                let mark = if ix == cur then "*" else ""
                pure $ tshow ix <> ": " <> nm <> mark
                    <> " (" <> tshow (Map.size ps) <> " panes)"

windowFormatEnv :: ServerState -> Session -> Int -> Window -> IO FormatEnv
windowFormatEnv st sess ix win = do
    base <- sessionFormatEnv st sess
    wname <- readTVarIO win.name
    pure $ Map.insert "window_index" (tshow ix)
         $ Map.insert "window_name" wname base

cmdListPanes :: CommandImpl
cmdListPanes st mclient _ =
    withTargetSession st mclient Nothing $ \sess -> do
        lines' <- atomically $ do
            mwin <- currentWindow sess
            case mwin of
                Nothing -> pure []
                Just win -> do
                    ps <- readTVar win.panes
                    forM (Map.elems ps) $ \p -> do
                        sz <- readTVar p.size
                        pure $ "%" <> tshow (rawPane p.id) <> ": ["
                            <> tshow sz.cols <> "x" <> tshow sz.rows <> "]"
        pure (map ROutput lines')

cmdSwitchClient :: CommandImpl
cmdSwitchClient st mclient args = do
    let (opts, flags, _) = parseArgs "t" args
    case mclient of
        Nothing -> pure [RErr "no client"]
        Just client
            | "-l" `elem` flags -> do
                mlast <- readTVarIO client.lastSession
                sessions <- readTVarIO st.sessions
                case mlast >>= (`Map.lookup` sessions) of
                    Nothing -> pure [RErr "no last session"]
                    Just sess -> switchClientTo st client sess >> pure []
            | otherwise ->
                withTargetSession st mclient (lookup "-t" opts) $ \sess ->
                    switchClientTo st client sess >> pure []

cmdKillServer :: CommandImpl
cmdKillServer st mclient _ = do
    sessions <- readTVarIO st.sessions
    forM_ (Map.keys sessions) $ \sid -> broadcast st sid Exited
    forM_ mclient $ \client -> send client Exited
    panes <- atomically $ do
        sess <- readTVar st.sessions
        fmap concat . forM (Map.elems sess) $ \s -> do
            ws <- readTVar s.windows
            fmap concat . forM (Map.elems ws) $ windowPanes
    forM_ panes $ \p -> Hat.Pty.closePty p.pty
    atomically $ do
        writeTVar st.sessions Map.empty
        writeTVar st.everAttached True
    pure []

cmdDisplayMessage :: CommandImpl
cmdDisplayMessage st mclient args = do
    let (_, flags, pos) = parseArgs "t" args
        raw = T.unwords pos
    msess <- targetSession st mclient Nothing
    text <- case msess of
        Nothing -> pure raw
        Just sess -> do
            env <- sessionFormatEnv st sess
            expandFormat st env raw
    if "-p" `elem` flags
        then pure [ROutput text]
        else do
            forM_ mclient $ \client -> showToast st client text
            pure []

cmdRunShell :: CommandImpl
cmdRunShell st mclient args = do
    let (_, _, pos) = parseArgs "t" args
        cmdText = T.unwords pos
    void . forkIO $ do
        (code, out, errOut) <- readCreateProcessWithExitCode
            (shell (T.unpack cmdText)) ""
        let firstLine = T.strip . T.takeWhile (/= '\n') . T.pack
        case code of
            ExitSuccess ->
                forM_ mclient $ \client ->
                    unless (null out) $ showToast st client (firstLine out)
            ExitFailure n ->
                forM_ mclient $ \client ->
                    showToast st client $
                        "run-shell exited " <> tshow n <> ": "
                        <> firstLine (out <> errOut)
    pure []

cmdIfShell :: CommandImpl
cmdIfShell st mclient args = do
    let (_, _, pos) = parseArgs "t" args
    case pos of
        (cond : thenCmd : rest) -> do
            (code, _, _) <- readCreateProcessWithExitCode
                (shell (T.unpack cond)) ""
            let chosen = case (code, rest) of
                    (ExitSuccess, _) -> Just thenCmd
                    (ExitFailure _, [elseCmd]) -> Just elseCmd
                    _ -> Nothing
            case chosen of
                Nothing -> pure []
                Just cmdText -> runCommandText st mclient cmdText
        _ -> pure [RErr "usage: if-shell condition command [command]"]

-- Control clients ------------------------------------------------------------

controlLoop :: ServerState -> Client -> IO ()
controlLoop st client = do
    m <- recvMessage client.sock
    case m of
        Just (Right (Command cmds)) -> do
            replies <- runCommands st (Just client) cmds
            forM_ replies $ \case
                ROutput out -> send client (Message out)
                RErr e -> send client (ServerError e)
            send client CommandDone
            controlLoop st client
        Just (Right Detach) -> pure ()
        Nothing -> pure ()
        _ -> controlLoop st client

-- Helpers ------------------------------------------------------------

rawClient :: ClientId -> Int
rawClient (ClientId n) = n

rawPane :: PaneId -> Int
rawPane (PaneId n) = n

rawSession :: SessionId -> Int
rawSession (SessionId n) = n

tshow :: Show a => a -> Text
tshow = T.pack . show
