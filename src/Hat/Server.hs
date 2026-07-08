-- | The server: owns PTYs, emulators, and the state tree; accepts
-- clients and streams frame diffs at them.
module Hat.Server
    ( runServer
    ) where

import Control.Concurrent (forkIO)
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
import Data.Word (Word8)
import qualified Network.Socket as N
import System.Directory (removeFile)
import System.Exit (exitSuccess)
import System.FilePath (takeDirectory)
import System.IO (SeekMode (AbsoluteSeek))
import qualified System.Posix.IO as PIO
import System.Posix.Process (getProcessID)

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

paneDied :: ServerState -> SessionId -> Window -> Pane -> IO ()
paneDied st sid win pane = do
    _ <- Hat.Pty.waitExit pane.pty
    Hat.Pty.closePty pane.pty
    logEvent st.logger PaneExited { pane = rawPane pane.id }
    sessionGone <- atomically $ do
        writeTVar pane.dead True
        modifyTVar' win.panes (Map.delete pane.id)
        remaining <- readTVar win.panes
        if not (Map.null remaining)
            then do
                -- M4: also drop the pane from the split tree.
                writeTVar win.activeId (fst (Map.findMin remaining))
                bumpDirty st
                pure Nothing
            else do
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
                    lay <- readTVar win.layout
                    ps <- readTVar win.panes
                    pure [ (p, rectSize rect)
                         | (pidL, rect) <- paneRects eff lay
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
                        lay <- readTVar win.layout
                        ps <- readTVar win.panes
                        active <- readTVar win.activeId
                        pure (Just (eff, lay, ps, active))
    (frame, cursor) <- case view of
        Nothing -> pure (blankFrame csize, (Pos 0 0, False))
        Just (eff, lay, ps, active) -> do
            let rects = paneRects eff lay
            base <- foldM' (blankFrame csize) rects $ \acc (pidL, rect) ->
                case Map.lookup pidL ps of
                    Nothing -> pure acc
                    Just pane -> do
                        scr <- Emu.snapshot pane.emulator
                        pure (overlayGrid acc rect scr.cells)
            cur <- case Map.lookup active ps of
                Nothing -> pure (Pos 0 0, False)
                Just pane -> do
                    scr <- Emu.snapshot pane.emulator
                    let origin = paneOrigin rects active
                    pure ( Pos { row = scr.cursor.row + origin.row
                               , col = scr.cursor.col + origin.col }
                         , scr.cursorVisible )
            pure (base, cur)
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

-- The M2 binding table. M6 replaces this with a real keymap.
performPrefixed :: ServerState -> Client -> Word8 -> IO Bool
performPrefixed st client key = case key of
    0x64 {- d -} -> do
        send client DetachOk
        pure False
    _ | key == prefixByte -> do
        mpane <- clientActivePane st client
        forM_ mpane $ \pane -> Hat.Pty.writePty pane.pty (B8.pack [toEnum (fromIntegral key)])
        pure True
    _ -> pure True  -- unbound key: ignore

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

-- Helpers ------------------------------------------------------------

rawClient :: ClientId -> Int
rawClient (ClientId n) = n

rawPane :: PaneId -> Int
rawPane (PaneId n) = n

rawSession :: SessionId -> Int
rawSession (SessionId n) = n

tshow :: Show a => a -> Text
tshow = T.pack . show
