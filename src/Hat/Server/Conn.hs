-- | A client connection's whole life: accept it, shake hands, attach it to
-- a session, feed its keystrokes to the pane or the key tables, and take it
-- down. What a key ends up running arrives as a 'Dispatch', so this side
-- never needs the command table itself.
module Hat.Server.Conn
    ( acceptLoop
    , welcome
    , controlLoop
    , currentSession
    , refreshSessionEnv
    , ensureSession
    , pickAttachSession
    , deliversKey
    , escTiming
    , reencodeCursor
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (race, withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM_, forever, void, when)
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.Socket as N
import Hat.Geometry
import Hat.Log
import Hat.Server.Environ
import Hat.Model
import Hat.Model.Options
import qualified Hat.Term.Pty
import Hat.Server.Command.CopyMode (runCopyModeCommand)
import Hat.Server.Command.Types
import Hat.Server.ClientIO (send)
import Hat.Server.Overlay
import Hat.Server.Startup
import Hat.Server.Toast
import Hat.Server.Keys
import Hat.Server.Locate
import Hat.Server.Pane
import Hat.Server.Render
import Hat.Server.Resize
import Hat.Server.View
import qualified Hat.Term.Emulator as Emu
import Hat.Transport.Wire


-- Connections ----------------------------------------------------------

acceptLoop :: Dispatch -> ServerState -> N.Socket -> IO ()
acceptLoop d st lsock = forever $ do
    (conn, _) <- N.accept lsock
    -- The autostarting client has now reached us; the idle-exit may
    -- consider draining (see 'waitIdle'). This is what lets us drop the
    -- old fixed-delay grace period without racing that client.
    atomically $ writeTVar st.served True
    void . forkIO $
        handleConn d st conn
            `catch` (\(e :: SomeException) ->
                logEvent st.logger ServerCrash { err = T.pack (show e) })
            `finally` (N.close conn `catch` \(_ :: SomeException) -> pure ())

handleConn :: Dispatch -> ServerState -> N.Socket -> IO ()
handleConn d st conn = do
    m <- recvMessage conn
    case m of
        Just (Known (ClientHello h)) -> case negotiate h.protoVersion of
            Right _ -> welcome d st conn h
            Left e  -> sendMessage conn (ServerError e)
        _ -> sendMessage conn (ServerError "expected hello")

welcome :: Dispatch -> ServerState -> N.Socket -> Hello -> IO ()
welcome d st conn h = do
    client <- newClient st conn h
    case h.intent of
        ControlIntent -> do
            atomically $ modifyTVar' st.clients (Map.insert client.id client)
            sendMessage conn (Welcome "")
            -- After Welcome: a client validates its one greeting strictly,
            -- then skips unknown tags.
            sendMessage conn (ServerVersion protocolVersion)
            atomically $ writeTVar client.ready True
            controlLoop d st client `finally` removeClient st client
        AttachIntent setupCmds -> do
            -- Register early so the setup commands (new-session,
            -- attach-session -t) act on a live client and can switch it.
            atomically $ modifyTVar' st.clients (Map.insert client.id client)
            -- Gate here, not only in ensureSession: setup commands run
            -- directly, so an autostarting `hat new` would otherwise race
            -- its own server's config load (upstream if-shell-TERM.sh).
            awaitStartup st client.autostart setupCmds
            setupErr <- attachSetup d st client setupCmds
            msess <- case setupErr of
                Just _ -> pure Nothing
                Nothing -> currentSession st client
            case (setupErr, msess) of
                (Just e, _) ->
                    sendMessage conn (ServerError e) >> removeClient st client
                (_, Nothing) -> do
                    sendMessage conn (ServerError "no session to attach")
                    removeClient st client
                (_, Just sess) -> do
                    refreshSessionEnv st sess client
                    sname <- readTVarIO sess.name
                    atomically $ writeTVar st.everAttached True
                    applySessionSize st sess.id
                    sendMessage conn (Welcome sname)
                    sendMessage conn (ServerVersion protocolVersion)
                    atomically $ writeTVar client.ready True
                    logEvent st.logger ClientConnected
                        { client = rawClient client.id, term = h.term }
                    withAsync (renderLoop st client) $ \_ ->
                        inputLoop d st client
                            `finally` removeClient st client

-- | Run an attaching client's setup commands, leaving @client.session@
-- pointing at the session it should render. An empty command list means a
-- plain attach: reuse an existing session or create one. Returns the
-- first error, if any, so 'welcome' can reject the attach.
attachSetup :: Dispatch -> ServerState -> Client -> [[Text]] -> IO (Maybe Text)
attachSetup _ st client [] = do
    sess <- ensureSession st client
    atomically $ do
        writeTVar client.session sess.id
        writeTVar st.lastActiveSession (Just sess.id)
        -- Adopt the server-wide alternate (e.g. one carried across a reload) so
        -- @switch-client -l@ still returns to it. See 'switchClientTo'.
        gLast <- readTVar st.lastSession
        forM_ gLast $ \g -> when (g /= sess.id) $
            writeTVar client.sessionHist [g]
    pure Nothing
attachSetup d st client cmds = do
    replies <- d.runCommands st (Just client) cmds
    pure (listToMaybe [e | RErr e <- replies])

-- | The session @client.session@ currently names, if it still exists.
currentSession :: ServerState -> Client -> IO (Maybe Session)
currentSession st client = do
    sid <- readTVarIO client.session
    Map.lookup sid <$> readTVarIO st.sessions

newClient :: ServerState -> N.Socket -> Hello -> IO Client
newClient st conn h = do
    cid <- atomically (freshId st.nextClient)
    sendLock <- newMVar ()
    sizeVar <- newTVarIO h.size
    activeVar <- newTVarIO 0
    sessVar <- newTVarIO (SessionId (-1))
    sessionHistVar <- newTVarIO []
    keyVar <- newIORef NoPrefix
    escVar <- newIORef NoEscPending
    frameVar <- newIORef (blankFrame h.size)
    cursorVar <- newIORef (Pos 0 0, True)
    cursorColourVar <- newIORef ""
    fullVar <- newTVarIO True
    toastVar <- newTVarIO Nothing
    promptVar <- newTVarIO Nothing
    pickerVar <- newTVarIO Nothing
    readyVar <- newTVarIO False
    focusVar <- newTVarIO True
    envImportVar <- newTVarIO ImportEnv
    pure Client
        { id = ClientId cid
        , role = case h.intent of
            AttachIntent {} -> Attached
            ControlIntent   -> Control
        , autostart = if h.autostarted then Autostarted else Joined
        , sock = conn
        , wireLevel = min protocolVersion h.protoVersion
        , sendLock = sendLock
        , size = sizeVar
        , lastActive = activeVar
        , session = sessVar
        , sessionHist = sessionHistVar
        , ready = readyVar
        , keyState = keyVar
        , escState = escVar
        , lastFrame = frameVar
        , lastCursor = cursorVar
        , lastCursorColour = cursorColourVar
        , needsFull = fullVar
        , toast = toastVar
        , prompt = promptVar
        , picker = pickerVar
        , outerFocused = focusVar
        , envImport = envImportVar
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

-- Sessions --------------------------------------------------------------

-- | On attach, import the @update-environment@ vars from the attaching
-- client's env into the session ('environUpdate'), so panes spawned
-- afterward see fresh values (e.g. a new @DISPLAY@ after reconnecting
-- over @ssh -X@). An @attach-session -E@ marks the client to skip this.
refreshSessionEnv :: ServerState -> Session -> Client -> IO ()
refreshSessionEnv st sess client = readTVarIO client.envImport >>= \case
    SkipEnvImport -> pure ()
    ImportEnv -> do
        vars <- (.updateEnvironment) <$> readTVarIO st.options
        atomically $ modifyTVar' sess.environ (environUpdate vars client.env)

ensureSession :: ServerState -> Client -> IO Session
ensureSession st client = do
    -- Let startup land first, so we attach to the configured, restored tree
    -- rather than racing it and creating a redundant fresh session.
    atomically $ readTVar st.startupPhase >>= \p -> when (p /= Ready) retry
    (existing, laId) <- atomically $
        (,) <$> readTVar st.sessions <*> readTVar st.lastActiveSession
    case pickAttachSession laId existing of
        Just (_, sess) -> pure sess
        Nothing -> createSession st Nothing Nothing client.env
            (T.unpack client.cwd) =<< readTVarIO client.size

-- | Which existing session a fresh client attaches to: the last-active one
-- if it still exists, else the first-created (lowest-id) session. After a
-- restore 'lastActiveSession' names the session that was focused, so a reboot
-- returns there rather than always landing on @$1@.
pickAttachSession
    :: Ord k => Maybe k -> Map.Map k v -> Maybe (k, v)
pickAttachSession lastActive m =
    case lastActive >>= \k -> (,) k <$> Map.lookup k m of
        Just kv -> Just kv
        Nothing -> Map.lookupMin m

-- Input ---------------------------------------------------------------------

inputLoop :: Dispatch -> ServerState -> Client -> IO ()
inputLoop d st client = loop
  where
    loop = do
        -- A lone trailing ESC held by escape-time races the next chunk: if
        -- nothing arrives within the window, flush it as the Escape key.
        held <- readIORef client.escState
        m <- case held of
            NoEscPending -> Right <$> recvMessage client.sock
            EscPending -> do
                opts <- clientOptions st client
                timer <- registerDelay (opts.escapeTime * 1000)
                race (atomically (readTVar timer >>= check)) (recvMessage client.sock)
        case m of
            Left () -> flushHeldEscape d st client >> loop
            Right Nothing -> pure ()
            Right (Just (Malformed err)) -> logEvent st.logger ProtocolError
                { client = rawClient client.id, err = T.pack err }
            Right (Just (UnknownTag _)) -> loop
            Right (Just (Known msg)) -> case msg of
                Input bs -> do
                    handleInput d st client bs
                    loop
                Resize sz -> do
                    atomically (writeTVar client.size sz)
                    sid <- readTVarIO client.session
                    applySessionSize st sid
                    loop
                Detach -> send client DetachOk
                Command cmds -> do
                    replies <- d.runCommands st (Just client) cmds
                    forM_ replies $ \case
                        ROutput out -> showToast st client out
                        RErr e -> showToast st client ("error: " <> e)
                    loop
                ClientHello {} -> pure ()

-- | Whether @client@ already holds the largest activity stamp among the
-- clients attached to its session (so it is the one an aggressive-resize
-- window currently follows).
isMostActive :: ServerState -> Client -> STM Bool
isMostActive st client = do
    sid <- readTVar client.session
    cs <- sessionClients st sid
    mine <- readTVar client.lastActive
    others <- mapM (\c -> readTVar c.lastActive)
        (filter (\c -> c.id /= client.id) cs)
    pure (all (< mine) others)

-- | Record activity from a client and, when it becomes the newly
-- most-recently-active one on its session, re-apply the session size so an
-- aggressive-resize window follows it. A no-op resize when nothing aggressive
-- is in play (the smallest-client size is independent of who is active).
noteClientActivity :: ServerState -> Client -> IO ()
noteClientActivity st client = do
    becameActive <- atomically $ do
        already <- isMostActive st client
        markActive st client
        pure (not already)
    when becameActive $ do
        sid <- readTVarIO client.session
        applySessionSize st sid

-- | Whether a key event is delivered to the focused pane's program. Focus
-- in/out reports reach the pane only when @focus-events@ is on and the pane's
-- app enabled focus reporting (?1004); a bare shell never asks, so its focus
-- report is dropped rather than echoed as a stray "^[[I". Every other key is
-- always delivered.
deliversKey :: Options -> Bool -> Key -> Bool
deliversKey opts paneFocusReport k
    | k.name `notElem` ["FocusIn", "FocusOut"] = True
    | otherwise = opts.focusEvents && paneFocusReport

handleInput :: Dispatch -> ServerState -> Client -> B.ByteString -> IO ()
handleInput d st client bs = do
    noteClientActivity st client
    mpicker <- readTVarIO client.picker
    mprompt <- readTVarIO client.prompt
    case (mpicker, mprompt) of
        (Just pk, _) -> handlePickerInput d st client pk (tokenizeKeys bs)
        (_, Just pr) -> handlePromptInput d st client pr bs
        _            -> handleKeys d st client bs

-- Keys are routed and run ONE AT A TIME, re-resolving the active pane and
-- its copy-mode table before each. A key that enters or leaves copy mode
-- therefore changes how the very next key in the same input chunk is
-- routed, so @prefix [@ followed immediately by motions never leaks the
-- motions to the shell (and vice versa on exit).
-- escape-time coalescing: 0 forwards a lone trailing ESC as Escape at once
-- ('EscImmediate'); a non-zero value holds it ('EscBuffered') for 'inputLoop'
-- to coalesce with the next chunk or flush on timeout.
escTiming :: Options -> EscTiming
escTiming opts = if opts.escapeTime <= 0 then EscImmediate else EscBuffered

-- | Tokenize a chunk under the client's escape-time, coalescing any ESC held
-- from the previous chunk, then route the resulting keys. A fresh lone
-- trailing ESC is left in 'escState' for 'inputLoop' to resolve.
handleKeys :: Dispatch -> ServerState -> Client -> B.ByteString -> IO ()
handleKeys d st client bs = do
    opts <- clientOptions st client
    held <- readIORef client.escState
    let toks = feedKeys (escTiming opts) held bs
    writeIORef client.escState toks.escPending
    runKeys d st client toks.escKeys

-- | The escape-time timer fired: forward the held lone ESC as Escape and clear
-- the buffer. A no-op if the ESC was already coalesced by an arriving chunk.
flushHeldEscape :: Dispatch -> ServerState -> Client -> IO ()
flushHeldEscape d st client = do
    held <- readIORef client.escState
    writeIORef client.escState NoEscPending
    runKeys d st client (flushEscape held)

-- Keys are routed and run ONE AT A TIME, re-resolving the active pane and
-- its copy-mode table before each. A key that enters or leaves copy mode
-- therefore changes how the very next key in the same input chunk is
-- routed, so @prefix [@ followed immediately by motions never leaks the
-- motions to the shell (and vice versa on exit).
runKeys :: Dispatch -> ServerState -> Client -> [Key] -> IO ()
runKeys d st client keys = do
    opts <- clientOptions st client
    km <- readTVarIO st.keymap
    let loop kst [] = writeIORef client.keyState kst
        loop kst (k0 : rest) = do
            dismissToast st client
            mpane <- clientActivePane st client
            -- A pending vi char search (f/F/t/T) captures this key as its
            -- target, ahead of any keymap lookup.
            searchFed <- maybe (pure False) (feedPendingSearch k0) mpane
            if searchFed then loop kst rest else do
              modeTable <- case mpane of
                Just pane -> fmap (fmap (.copyState.keyTable)) (readTVarIO pane.mode)
                Nothing -> pure Nothing
              k <- reencodeCursor mpane k0
              -- The outer terminal's ?1004 focus reports track whether the
              -- user is watching this client, independent of whether the
              -- pane's app asked for them (the 'keepKey' gate below).
              noteOuterFocus st client k
              keep <- keepKey opts mpane k
              if not keep
                then loop kst rest
                else do
                    let (kst', actions) =
                            routeKeys opts.prefix km modeTable kst [k]
                    forM_ actions $ \case
                        Passthrough raw ->
                            forM_ mpane $ \pane -> Hat.Term.Pty.writePty pane.pty raw
                        RunCommands cmds -> withCommandBatch st $
                            forM_ cmds $ \argv ->
                                toastReplies st client
                                    =<< d.runArgv st (Just client) argv
                    loop kst' rest
    st0 <- readIORef client.keyState
    loop st0 keys
  where
    -- Reads the pane's live focus-reporting mode, then defers the actual
    -- keep/drop decision to the pure 'deliversKey'.
    keepKey opts mpane k
        | k.name `notElem` ["FocusIn", "FocusOut"] = pure True
        | otherwise = do
            report <- maybe (pure False)
                (\pane -> (.focusReport) <$> Emu.modes pane.emulator) mpane
            pure (deliversKey opts report k)
    -- When the pane's copy mode is waiting for a char-search target, this
    -- key IS the target: a single printable char runs the search, anything
    -- else (Escape, Enter, an arrow) cancels it. Returns whether it was
    -- consumed here.
    feedPendingSearch key pane = do
        mmode <- readTVarIO pane.mode
        case mmode of
            Just s | Just _ <- s.copyState.pendingSearch -> do
                -- Neither command can fail, so the replies are empty.
                void $ if T.length key.name == 1
                    then runCopyModeCommand st pane "apply-search" [key.name]
                    else runCopyModeCommand st pane "cancel-search" []
                pure True
            _ -> pure False

-- | Forward a recognized key as the pane's advertised terminal expects, not
-- as the outer terminal's incidental bytes. The arrows are DECCKM-dependent
-- (@\ESC[A@ vs @\ESCOA@), so the pane's emulator encodes them; every other
-- named key forwards its tmux-256color terminfo bytes from 'namedKeys'.
-- Unrecognized keys keep their raw bytes.
reencodeCursor :: Maybe Pane -> Key -> IO Key
reencodeCursor mpane key = case arrowOf key.name of
    Just ck | Just pane <- mpane -> do
        enc <- Emu.encodeKey pane.emulator ck
        pure key { raw = enc }
    _ | Just canon <- lookup key.name namedKeys -> pure key { raw = canon }
    _ -> pure key
  where
    arrowOf n = case n of
        "Up"    -> Just Emu.CursorUp
        "Down"  -> Just Emu.CursorDown
        "Left"  -> Just Emu.CursorLeft
        "Right" -> Just Emu.CursorRight
        _       -> Nothing

controlLoop :: Dispatch -> ServerState -> Client -> IO ()
controlLoop d st client = do
    m <- recvMessage client.sock
    case m of
        Just (Known (Command cmds)) -> do
            awaitStartup st client.autostart cmds
            replies <- d.runCommands st (Just client) cmds
            forM_ replies $ \case
                ROutput out -> send client (Message out)
                RErr e -> send client (ServerError e)
            -- Report done only once any layout change the command made has
            -- been reconciled into the panes, so a caller that immediately
            -- inspects a pane's size never races the pty resize.
            awaitReconciled st
            send client CommandDone
            controlLoop d st client
        Just (Known Detach) -> pure ()
        Just (Malformed _) -> pure ()
        Nothing -> pure ()
        _ -> controlLoop d st client


-- | @kill-server [-a]@: drain every session (a normal shutdown). @-a@ keeps
-- the store's saved tree; a bare kill drops it. See 'saveNow'.
