-- | The server: owns PTYs, emulators, and the state tree; accepts
-- clients and streams frame diffs at them.
module Hat.Server
    ( runServer
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (race, withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
    (IOException, SomeException, bracket, catch, finally, try)
import Control.Monad (forM, forM_, forever, unless, void, when)
import qualified Data.ByteString.Char8 as B8
import Data.IORef
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import qualified Data.Vector as V
import Data.Word (Word8)
import qualified Network.Socket as N
import System.Directory (removeFile)
import System.Exit (exitSuccess)
import System.FilePath (takeDirectory)
import System.IO (SeekMode (AbsoluteSeek))
import qualified System.Posix.IO as PIO
import qualified System.Posix.Files as PFiles
import System.Posix.Process (getProcessID)
import System.Posix.Unistd (SystemID (nodeName), getSystemID)

import Hat.Geometry
import Hat.Log
import Hat.Model
import qualified Hat.Pty
import Hat.Server.Input
import Hat.Server.Layout
import Hat.Server.Render
import Hat.Socket (listenOn)
import qualified Hat.Term.Cell as Cell
import qualified Hat.Term.Emulator as Emu
import Hat.Wire

-- M6 makes this configurable; C-b until then.
prefixByte :: Word8
prefixByte = 0x02

defaultHistoryLimit :: Int
defaultHistoryLimit = 50000

runServer :: FilePath -> IO ()
runServer path = do
    locked <- acquireLock (path <> ".lock")
    unless locked exitSuccess  -- another server won the race
    lg <- newLogger (takeDirectory path <> "/server.log")
    st <- newServerState lg path
    bracket (listenOn path) N.close $ \lsock -> do
        logEvent lg ServerStarted { socket = path }
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

waitIdle :: ServerState -> IO ()
waitIdle st = atomically $ do
    armed <- readTVar st.everAttached
    sess <- readTVar st.sessions
    check (armed && Map.null sess)

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
        Just (Right h@Hello {})
            | h.protoVersion == protocolVersion -> welcome st conn h
            | otherwise -> sendMessage conn $ ServerError $
                "protocol mismatch: server " <> tshow protocolVersion
                <> ", client " <> tshow h.protoVersion
        _ -> sendMessage conn (ServerError "expected hello")

welcome :: ServerState -> N.Socket -> ClientToServer -> IO ()
welcome st conn h = do
    client <- newClient st conn h
    case h.intent of
        ControlIntent -> do
            sendMessage conn (Welcome "")
            controlLoop st client
        AttachIntent -> do
            sess <- ensureSession st h
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

newClient :: ServerState -> N.Socket -> ClientToServer -> IO Client
newClient st conn h = do
    cid <- atomically (freshId st.nextClient)
    sendLock <- newMVar ()
    sizeVar <- newTVarIO h.size
    sessVar <- newTVarIO (SessionId (-1))
    keyVar <- newIORef Normal
    frameVar <- newIORef (blankFrame h.size)
    cursorVar <- newIORef (Pos 0 0, True)
    fullVar <- newTVarIO True
    pure Client
        { id = ClientId cid
        , sock = conn
        , sendLock = sendLock
        , size = sizeVar
        , session = sessVar
        , keyState = keyVar
        , lastFrame = frameVar
        , lastCursor = cursorVar
        , needsFull = fullVar
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

-- Sessions -----------------------------------------------------------

ensureSession :: ServerState -> ClientToServer -> IO Session
ensureSession st h = do
    existing <- readTVarIO st.sessions
    case Map.lookupMin existing of
        Just (_, sess) -> pure sess
        Nothing -> createSession st h

createSession :: ServerState -> ClientToServer -> IO Session
createSession st h = do
    sid <- atomically (freshId st.nextSession)
    let shell = maybe "/bin/sh" T.unpack (List.lookup "SHELL" h.env)
        dir = T.unpack h.cwd
    (win, pane) <- newWindowWithPane st (SessionId sid) shell dir h.env h.size
    nameVar <- newTVarIO (tshow sid)
    windowsVar <- newTVarIO (Map.singleton windowBaseIndex win)
    currentVar <- newTVarIO windowBaseIndex
    lastVar <- newTVarIO Nothing
    sizeVar <- newTVarIO h.size
    let sess = Session
            { id = SessionId sid
            , name = nameVar
            , windows = windowsVar
            , currentIx = currentVar
            , lastIx = lastVar
            , lastSize = sizeVar
            , environ = h.env
            , startCwd = dir
            }
    atomically $ modifyTVar' st.sessions (Map.insert sess.id sess)
    startPaneReader st sess.id win pane
    pure sess

windowBaseIndex :: Int
windowBaseIndex = 0

newWindowWithPane
    :: ServerState -> SessionId -> FilePath -> FilePath
    -> [(Text, Text)] -> Size -> IO (Window, Pane)
newWindowWithPane st sid shell dir env sz = do
    (wid, pid) <- atomically $
        (,) <$> freshId st.nextWindow <*> freshId st.nextPane
    pane <- spawnPane st (PaneId pid) sid shell dir env sz
    nameVar <- newTVarIO (T.pack (baseName shell))
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
    :: ServerState -> PaneId -> SessionId -> FilePath -> FilePath
    -> [(Text, Text)] -> Size -> IO Pane
spawnPane st pid sid shell dir env sz = do
    serverPid <- getProcessID
    let cleanEnv =
            [ (T.unpack k, T.unpack v)
            | (k, v) <- env
            , k `notElem` ["TERM", "TMUX", "TMUX_PANE", "HAT", "HAT_PANE"]
            ]
        hatVar = st.sockPath <> "," <> show serverPid <> ","
            <> show (rawSession sid)
        paneEnv = cleanEnv <>
            [ ("TERM", "screen-256color")
            , ("TMUX", hatVar)
            , ("HAT", hatVar)
            , ("TMUX_PANE", "%" <> show (rawPane pid))
            , ("HAT_PANE", "%" <> show (rawPane pid))
            ]
    pty <- Hat.Pty.spawn Hat.Pty.Spawn
        { cmd = shell
        , args = []
        , env = paneEnv
        , cwd = Just dir
        , size = sz
        }
    emu <- Emu.newEmulator sz defaultHistoryLimit
    sizeVar <- newTVarIO sz
    deadVar <- newTVarIO False
    logEvent st.logger PaneSpawned
        { pane = rawPane pid, cmd = T.pack shell }
    pure Pane
        { id = pid
        , pty = pty
        , emulator = emu
        , size = sizeVar
        , dead = deadVar
        , startCwd = dir
        }

startPaneReader :: ServerState -> SessionId -> Window -> Pane -> IO ()
startPaneReader st sid win pane = void . forkIO $ loop
  where
    loop = do
        bs <- Hat.Pty.readPty pane.pty
        if B8.null bs
            then paneDied st sid win pane
            else do
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
                when (active == pane.id) $ do
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

-- Sizing -------------------------------------------------------------

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

-- Rendering ----------------------------------------------------------

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
            base0 <- pure (applyBorders (blankFrame csize) borders)
            base <- foldM' base0 rects $ \acc (pidL, rect) ->
                case Map.lookup pidL ps of
                    Nothing -> pure acc
                    Just pane -> do
                        scr <- Emu.snapshot pane.emulator
                        pure (overlayGrid acc rect scr.cells)
            statusRow <- statusCells st sess (fromIntegral csize.cols)
            let withStatus
                    | csize.rows >= 2 =
                        base V.// [(fromIntegral csize.rows - 1, statusRow)]
                    | otherwise = base
            cur <- case Map.lookup active ps of
                Nothing -> pure (Pos 0 0, False)
                Just pane -> do
                    scr <- Emu.snapshot pane.emulator
                    let origin = paneOrigin rects active
                    pure ( Pos { row = scr.cursor.row + origin.row
                               , col = scr.cursor.col + origin.col }
                         , scr.cursorVisible )
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
    foldM' z xs f = go z xs where
        go acc [] = pure acc
        go acc (x : rest) = f acc x >>= \acc' -> go acc' rest

paneOrigin :: [(PaneId, Rect)] -> PaneId -> Pos
paneOrigin rects pidL = case List.lookup pidL rects of
    Just r -> Pos { row = r.startRow, col = r.startCol }
    Nothing -> Pos 0 0

-- Input --------------------------------------------------------------

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
                    ks <- readIORef client.keyState
                    let (ks', acts) = routeInput prefixByte ks bs
                    writeIORef client.keyState ks'
                    continue <- performAll acts
                    when continue loop
                Resize sz -> do
                    atomically (writeTVar client.size sz)
                    sid <- readTVarIO client.session
                    applySessionSize st sid
                    loop
                Detach -> send client DetachOk
                Command t -> do
                    runCommand st client t
                    loop
                Hello {} -> pure ()
    performAll [] = pure True
    performAll (a : rest) = do
        continue <- perform a
        if continue then performAll rest else pure False
    perform = \case
        ToPane bs -> do
            mpane <- clientActivePane st client
            forM_ mpane $ \pane -> Hat.Pty.writePty pane.pty bs
            pure True
        Prefixed key -> performPrefixed st client key

clientActivePane :: ServerState -> Client -> IO (Maybe Pane)
clientActivePane st client = atomically $ do
    sid <- readTVar client.session
    msess <- Map.lookup sid <$> readTVar st.sessions
    case msess of
        Nothing -> pure Nothing
        Just sess -> do
            mwin <- currentWindow sess
            case mwin of
                Nothing -> pure Nothing
                Just win -> activePane win

-- The hardcoded binding table. M6 replaces this with a real keymap.
performPrefixed :: ServerState -> Client -> Word8 -> IO Bool
performPrefixed st client key = case toEnum (fromIntegral key) of
    'd' -> do
        send client DetachOk
        pure False
    '%' -> splitActive st client LeftRight >> pure True
    '"' -> splitActive st client TopBottom >> pure True
    'h' -> selectDir st client DirLeft >> pure True
    'j' -> selectDir st client DirDown >> pure True
    'k' -> selectDir st client DirUp >> pure True
    'l' -> selectDir st client DirRight >> pure True
    ';' -> selectLastPane st client >> pure True
    'x' -> killActivePane st client >> pure True
    'z' -> toggleZoom st client >> pure True
    'H' -> resizeActive st client DirLeft 5 >> pure True
    'J' -> resizeActive st client DirDown 5 >> pure True
    'K' -> resizeActive st client DirUp 5 >> pure True
    'L' -> resizeActive st client DirRight 5 >> pure True
    'c' -> newWindowAction st client >> pure True
    'n' -> cycleWindow st client 1 >> pure True
    'p' -> cycleWindow st client (-1) >> pure True
    'a' -> selectLastWindow st client >> pure True
    ch | ch >= '0' && ch <= '9' ->
        selectWindowIx st client (fromEnum ch - fromEnum '0') >> pure True
    _ | key == prefixByte -> do
        mpane <- clientActivePane st client
        forM_ mpane $ \pane ->
            Hat.Pty.writePty pane.pty (B8.pack [toEnum (fromIntegral key)])
        pure True
    _ -> pure True  -- unbound key: ignore

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

-- | Where is a pane's child process now? /proc, with a fallback.
paneCurrentPath :: Pane -> IO FilePath
paneCurrentPath pane = do
    r <- try (PFiles.readSymbolicLink
        ("/proc/" <> show (Hat.Pty.pid pane.pty) <> "/cwd"))
    pure $ case r of
        Left (_ :: IOException) -> pane.startCwd
        Right dir -> dir

splitActive :: ServerState -> Client -> Orientation -> IO ()
splitActive st client orient = do
    mview <- atomically $ do
        mv <- clientView st client
        case mv of
            Nothing -> pure Nothing
            Just (sess, win) -> do
                mpane <- activePane win
                eff <- readTVar sess.lastSize
                pure ((,,,) sess win eff <$> mpane)
    forM_ mview $ \(sess, win, eff, active) -> do
        -- Reject splits that would leave either side under 2 cells.
        (rects, _) <- atomically (windowArrange eff win)
        let mrect = List.lookup active.id rects
            fits = case (orient, mrect) of
                (LeftRight, Just r) -> r.endCol - r.startCol >= 5
                (TopBottom, Just r) -> r.endRow - r.startRow >= 5
                _ -> False
        when fits $ do
            pid <- PaneId <$> atomically (freshId st.nextPane)
            dir <- paneCurrentPath active
            let shell = maybe "/bin/sh" T.unpack
                    (List.lookup "SHELL" sess.environ)
            pane <- spawnPane st pid sess.id shell dir sess.environ eff
            atomically $ do
                modifyTVar' win.panes (Map.insert pane.id pane)
                modifyTVar' win.layout
                    (splitLeaf active.id orient False pane.id)
                lastA <- readTVar win.activeId
                writeTVar win.lastActive (Just lastA)
                writeTVar win.activeId pane.id
                writeTVar win.zoomed Nothing
                bumpDirty st
            startPaneReader st sess.id win pane
            applySessionSize st sess.id

selectDir :: ServerState -> Client -> Direction -> IO ()
selectDir st client dir = atomically $ do
    mv <- clientView st client
    forM_ mv $ \(sess, win) -> do
        eff <- readTVar sess.lastSize
        (rects, _) <- windowArrange eff win
        active <- readTVar win.activeId
        forM_ (neighbor rects active dir) $ \next -> do
            writeTVar win.lastActive (Just active)
            writeTVar win.activeId next
            bumpDirty st

selectLastPane :: ServerState -> Client -> IO ()
selectLastPane st client = atomically $ do
    mv <- clientView st client
    forM_ mv $ \(_, win) -> do
        mlast <- readTVar win.lastActive
        ps <- readTVar win.panes
        forM_ mlast $ \lastP -> when (Map.member lastP ps) $ do
            cur <- readTVar win.activeId
            writeTVar win.lastActive (Just cur)
            writeTVar win.activeId lastP
            bumpDirty st

killActivePane :: ServerState -> Client -> IO ()
killActivePane st client = do
    mpane <- clientActivePane st client
    -- closePty hangs up the child; the pane reader sees EOF and takes
    -- care of the tree.
    forM_ mpane $ \pane -> Hat.Pty.closePty pane.pty

toggleZoom :: ServerState -> Client -> IO ()
toggleZoom st client = do
    changed <- atomically $ do
        mv <- clientView st client
        case mv of
            Nothing -> pure Nothing
            Just (sess, win) -> do
                mz <- readTVar win.zoomed
                active <- readTVar win.activeId
                writeTVar win.zoomed $ case mz of
                    Just _ -> Nothing
                    Nothing -> Just active
                bumpDirty st
                pure (Just sess.id)
    forM_ changed (applySessionSize st)

resizeActive :: ServerState -> Client -> Direction -> Int -> IO ()
resizeActive st client dir delta = do
    msid <- atomically $ do
        mv <- clientView st client
        case mv of
            Nothing -> pure Nothing
            Just (sess, win) -> do
                eff <- readTVar sess.lastSize
                active <- readTVar win.activeId
                modifyTVar' win.layout
                    (resizeSplit active dir delta (sizeRect eff))
                bumpDirty st
                pure (Just sess.id)
    forM_ msid (applySessionSize st)

-- Commands (a stub until M6's engine) --------------------------------

runCommand :: ServerState -> Client -> Text -> IO ()
runCommand st client cmdline = do
    logEvent st.logger CommandRun
        { client = rawClient client.id, command = cmdline }
    case T.words cmdline of
        ["kill-server"] -> do
            sessions <- readTVarIO st.sessions
            forM_ (Map.keys sessions) $ \sid -> broadcast st sid Exited
            send client Exited
            panes <- allPanes st
            forM_ panes $ \p -> Hat.Pty.closePty p.pty
            atomically $ do
                writeTVar st.sessions Map.empty
                writeTVar st.everAttached True
        ["detach"] -> send client DetachOk
        _ -> send client (ServerError ("unknown command: " <> cmdline))

allPanes :: ServerState -> IO [Pane]
allPanes st = atomically $ do
    sessions <- readTVar st.sessions
    fmap concat . forM (Map.elems sessions) $ \sess -> do
        ws <- readTVar sess.windows
        fmap concat . forM (Map.elems ws) $ windowPanes

controlLoop :: ServerState -> Client -> IO ()
controlLoop st client = do
    m <- recvMessage client.sock
    case m of
        Just (Right (Command t)) -> do
            runCommand st client t
            controlLoop st client
        Just (Right Detach) -> pure ()
        Nothing -> pure ()
        _ -> controlLoop st client

-- Windows --------------------------------------------------------------

-- | The pane area of the screen: everything above the status line.
windowArea :: Size -> Size
windowArea sz = sz { rows = max 1 (sz.rows - 1) }

newWindowAction :: ServerState -> Client -> IO ()
newWindowAction st client = do
    mv <- atomically (clientView st client)
    forM_ mv $ \(sess, _) -> do
        eff <- readTVarIO sess.lastSize
        let shell = maybe "/bin/sh" T.unpack (List.lookup "SHELL" sess.environ)
        (win, pane) <- newWindowWithPane st sess.id shell sess.startCwd
            sess.environ (windowArea eff)
        atomically $ do
            ws <- readTVar sess.windows
            let ix = head [i | i <- [windowBaseIndex ..], not (Map.member i ws)]
            modifyTVar' sess.windows (Map.insert ix win)
            cur <- readTVar sess.currentIx
            writeTVar sess.lastIx (Just cur)
            writeTVar sess.currentIx ix
            bumpDirty st
        startPaneReader st sess.id win pane
        applySessionSize st sess.id

selectWindowIx :: ServerState -> Client -> Int -> IO ()
selectWindowIx st client ix = atomically $ do
    mv <- clientView st client
    forM_ mv $ \(sess, _) -> switchTo st sess ix

switchTo :: ServerState -> Session -> Int -> STM ()
switchTo st sess ix = do
    ws <- readTVar sess.windows
    cur <- readTVar sess.currentIx
    when (ix /= cur) $ forM_ (Map.lookup ix ws) $ \win -> do
        writeTVar sess.lastIx (Just cur)
        writeTVar sess.currentIx ix
        writeTVar win.bellFlag False
        bumpDirty st

cycleWindow :: ServerState -> Client -> Int -> IO ()
cycleWindow st client step = atomically $ do
    mv <- clientView st client
    forM_ mv $ \(sess, _) -> do
        ws <- readTVar sess.windows
        cur <- readTVar sess.currentIx
        let ixs = Map.keys ws
        case ixs of
            [] -> pure ()
            _ -> do
                let n = length ixs
                    curPos = maybe 0 (\x -> x) (List.elemIndex cur ixs)
                    ix = ixs !! ((curPos + step + n) `mod` n)
                switchTo st sess ix

selectLastWindow :: ServerState -> Client -> IO ()
selectLastWindow st client = atomically $ do
    mv <- clientView st client
    forM_ mv $ \(sess, _) -> do
        mlast <- readTVar sess.lastIx
        forM_ mlast (switchTo st sess)

-- Status line ----------------------------------------------------------

statusStyle :: Cell.Style
statusStyle = Cell.defaultStyle
    { Cell.fg = Cell.Indexed 0
    , Cell.bg = Cell.Indexed 2
    }

-- The default status line; the real format engine arrives in M7.
statusCells :: ServerState -> Session -> Int -> IO (V.Vector Cell.Cell)
statusCells _st sess width = do
    (sname, entries) <- atomically $ do
        sname <- readTVar sess.name
        ws <- readTVar sess.windows
        cur <- readTVar sess.currentIx
        mlast <- readTVar sess.lastIx
        entries <- forM (Map.toAscList ws) $ \(ix, win) -> do
            wname <- readTVar win.name
            bell <- readTVar win.bellFlag
            let flag
                    | ix == cur = "*"
                    | Just ix == mlast = "-"
                    | otherwise = ""
                bellFlag = if bell then "!" else ""
            pure (tshow ix <> ":" <> wname <> flag <> bellFlag)
        pure (sname, entries)
    now <- getZonedTime
    hostname <- nodeName <$> getSystemID
    let left = "[" <> sname <> "] " <> T.intercalate " " entries
        time = T.pack (formatTime defaultTimeLocale "%H:%M %d-%b-%y" now)
        right = time <> " " <> T.pack hostname <> " "
        pad = width - T.length left - T.length right
        line
            | pad >= 0 = left <> T.replicate pad " " <> right
            | otherwise = T.take (fromIntegral width) (left <> " " <> right)
        cells = [ Cell.Cell { Cell.text = T.singleton ch
                            , Cell.width = 1
                            , Cell.style = statusStyle }
                | ch <- T.unpack line ]
        padded = take width (cells <> repeat blankStatus)
        blankStatus = Cell.Cell { Cell.text = " ", Cell.width = 1
                                , Cell.style = statusStyle }
    pure (V.fromList padded)

-- Helpers ------------------------------------------------------------

rawClient :: ClientId -> Int
rawClient (ClientId n) = n

rawPane :: PaneId -> Int
rawPane (PaneId n) = n

rawSession :: SessionId -> Int
rawSession (SessionId n) = n

tshow :: Show a => a -> Text
tshow = T.pack . show
