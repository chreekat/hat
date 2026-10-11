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
    , keyContext
    , reencodeKey
    , handleInput  -- ^ exported for the display-panes key tests
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (link, race, wait, withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (foldM, forM_, forever, unless, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.IORef
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket qualified as N
import Hat.Geometry
import Hat.Log
import Hat.Server.Environ
import Hat.Model
import Hat.Model.Options
import Hat.Server.Command.CopyMode (runCopyModeCommand)
import Hat.Server.Command.Session (checkUnattached)
import Hat.Server.Command.Types
import Hat.Server.ClientIO (send)
import Hat.Server.Control
    ( BlockOrigin (..), controlWriter, feedControlInput, runControlCommands
    , subscriptionLoop )
import Hat.Server.ControlQueue (requestExit)
import Hat.Server.ControlTypes (ControlState, newControlState)
import Hat.Server.Overlay
import Hat.Server.Startup
import Hat.Server.DisplayPanes
import Hat.Server.Flash
import Hat.Server.Hooks (PayloadItem (..), clientNameOf, notify, sessionTarget)
import Hat.Server.Toast
import Hat.Server.Keys
import Hat.Server.Locate
import Hat.Server.Mouse
    (Click (..), ClickStart (..), MouseInput (..), MouseKey (..), MouseState (..), Placement (..), checkMouse, clickExpired
    , parseMouseInput, stripMods)
import Hat.Server.MouseIO (mouseDragUpdate, mouseEventPane, mouseViewFor, sendMouse)
import Hat.Server.Pane
import Hat.Server.Render
import Hat.Server.Resize
import Hat.Server.View
import Hat.Term.Emulator qualified as Emu
import Hat.Transport.Wire


-- Connections ----------------------------------------------------------

-- | Serve connections until 'acceptGate' closes; while parked, arrivals sit
-- in the listen backlog (for the next image, across a reload's exec) until
-- the gate reopens. Once retiring, it closes the listener and turns away
-- whatever is still in the backlog. See Note [Retiring the socket].
acceptLoop :: Dispatch -> ServerState -> N.Socket -> IO ()
acceptLoop d st lsock = forever $ do
    r <- race awaitGate (N.accept lsock)
    case r of
        Left AcceptRetiring -> retire
        Left _ -> do
            atomically $ writeTVar st.acceptGate AcceptParked
            gate <- atomically $ do
                g <- readTVar st.acceptGate
                check (g == AcceptOpen || g == AcceptRetiring)
                pure g
            when (gate == AcceptRetiring) retire
        Right (conn, _) -> do
          gate <- readTVarIO st.acceptGate
          if gate == AcceptRetiring then N.close conn else do
            -- Counting the handler as in-flight here, before its fork, is what
            -- lets us drop the old fixed-delay grace period without racing the
            -- autostarting client — and, since the count falls again when the
            -- handler ends, lets a client that connects and dies before
            -- creating a session drain (bug 28). Whether it counts as 'served'
            -- waits on its hello (see 'handleConn').
            atomically $ modifyTVar' st.activeConns (+ 1)
            void . forkIO $
                handleConn d st conn
                    `catch` (\(e :: SomeException) ->
                        logEvent st.logger ServerCrash { err = T.pack (show e) })
                    `finally` do
                        atomically $ modifyTVar' st.activeConns (subtract 1)
                        N.close conn `catch` \(_ :: SomeException) -> pure ()
  where
    awaitGate = atomically $ do
        g <- readTVar st.acceptGate
        check (g == AcceptClosing || g == AcceptRetiring)
        pure g
    -- Idles until the server's exit cancels the loop.
    retire = N.close lsock >> forever (threadDelay maxBound)

-- Note [Retiring the socket]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- kill-server unlinks the socket path before it replies, so every client
-- that runs after it finds no server and autostarts a fresh one instead of
-- joining, or being reset by, the old one while it drains its panes; a
-- client that raced in before the unlink is turned away ('acceptLoop'). The
-- retired server keeps the lock until it exits, so the fresh server cannot
-- touch the store or bind the path early: it waits for a lock holder that
-- has no path bound ('awaitLock'), and the autostarting client's connect
-- poll covers the wait.

helloOrigin :: Hello -> Autostart
helloOrigin h = if h.autostarted then Autostarted else Joined

handleConn :: Dispatch -> ServerState -> N.Socket -> IO ()
handleConn d st conn = do
    re <- newReadEnd conn
    m <- recvMessage re
    atomically $ do
        phase <- readTVar st.startupPhase
        let origin = case m of
                Just (Known (ClientHello h)) -> Just (helloOrigin h)
                _ -> Nothing
        when (idleArming phase origin == ArmsIdleExit) $
            writeTVar st.served True
    case m of
        Just (Known (ClientHello h)) -> case negotiate h.protoVersion of
            Right _ -> welcome d st re h
            Left e  -> sendMessage conn (ServerError e)
        _ -> sendMessage conn (ServerError "expected hello")

welcome :: Dispatch -> ServerState -> ReadEnd -> Hello -> IO ()
welcome d st re h = do
    let conn = re.sock
    client <- newClient st conn h
    case h.intent of
        ControlIntent -> do
            atomically $ modifyTVar' st.clients (Map.insert client.id client)
            sendMessage conn (Welcome "")
            -- After Welcome: a client validates its one greeting strictly,
            -- then skips unknown tags.
            sendMessage conn (ServerVersion protocolVersion)
            atomically $ writeTVar client.ready True
            controlLoop d st re client `finally` removeClient st client
        ControlModeIntent setupCmds -> case client.control of
            Nothing -> sendMessage conn (ServerError "control state missing")
            Just cs -> do
                atomically $ modifyTVar' st.clients (Map.insert client.id client)
                sendMessage conn (Welcome "")
                sendMessage conn (ServerVersion protocolVersion)
                atomically $ writeTVar client.ready True
                logEvent st.logger ClientConnected
                    { client = rawClient client.id, term = h.term }
                controlModeSession d st re client cs setupCmds
                    `finally` removeClient st client
        AttachIntent setupCmds -> do
            -- Register early so the setup commands (new-session,
            -- attach-session -t) act on a live client and can switch it.
            atomically $ modifyTVar' st.clients (Map.insert client.id client)
            -- Gate here, not only in ensureSession: setup commands run
            -- directly, so an autostarting `hat new` would otherwise race
            -- its own server's config load (upstream if-shell-TERM.sh).
            awaitStartup st client.autostart setupCmds
            -- A reattaching client carries its MRU session history by name (it
            -- re-execs across a restart with a fresh identity). Resolve it back
            -- to ids now the tree is restored (awaitStartup held for it), before
            -- the attach records this session's visit, so @switch-client -l@
            -- survives the restart.
            atomically . writeTVar client.sessionHist
                =<< atomically (resolveSessionNames st h.sessionHist)
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
                    applySessionSize st sess.id
                    sendMessage conn (Welcome sname)
                    sendMessage conn (ServerVersion protocolVersion)
                    atomically $ writeTVar client.ready True
                    logEvent st.logger ClientConnected
                        { client = rawClient client.id, term = h.term }
                    -- Linked: a dying render loop must end the connection
                    -- loudly, never leave a live client rendering nothing.
                    withAsync (renderLoop framePeriod st client) $ \a -> do
                        link a
                        inputLoop d st re client
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
    keyVar <- newIORef initialKeyState
    escVar <- newIORef NoEscPending
    frameVar <- newIORef (blankFrame h.size)
    originsVar <- newIORef V.empty
    chromeVar <- newIORef Nothing
    statusVar <- newIORef Nothing
    mouseModeVar <- newIORef 0
    cursorVar <- newIORef (Pos 0 0, True)
    cursorColourVar <- newIORef Nothing
    cursorShapeVar <- newIORef CursorDefault
    fullVar <- newTVarIO True
    toastVar <- newTVarIO Nothing
    flashVar <- newTVarIO Nothing
    promptVar <- newTVarIO Nothing
    pickerVar <- newTVarIO Nothing
    displayPanesVar <- newTVarIO Nothing
    readyVar <- newTVarIO False
    focusVar <- newTVarIO True
    envImportVar <- newTVarIO ImportEnv
    controlState <- case h.intent of
        ControlModeIntent {} -> Just <$> newControlState
        _ -> pure Nothing
    pure Client
        { id = ClientId cid
        , role = case h.intent of
            AttachIntent {} -> Attached
            ControlIntent   -> Control
            ControlModeIntent {} -> Attached
        , autostart = helloOrigin h
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
        , lastOrigins = originsVar
        , lastChrome = chromeVar
        , lastStatus = statusVar
        , lastMouseMode = mouseModeVar
        , lastCursor = cursorVar
        , lastCursorColour = cursorColourVar
        , lastCursorShape = cursorShapeVar
        , needsFull = fullVar
        , toast = toastVar
        , flash = flashVar
        , prompt = promptVar
        , picker = pickerVar
        , displayPanes = displayPanesVar
        , outerFocused = focusVar
        , envImport = envImportVar
        , env = h.env
        , cwd = h.cwd
        , stdinSource = NoStdin
        , control = controlState
        }

-- | Resolve MRU session names to the ids of the sessions that still exist,
-- keeping MRU order and dropping duplicates and names with no live session.
resolveSessionNames :: ServerState -> [Text] -> STM [SessionId]
resolveSessionNames st names = do
    sessMap <- readTVar st.sessions
    named <- mapM (\(sid, s) -> (, sid) <$> readTVar s.name) (Map.toList sessMap)
    let byName = Map.fromList named
    pure (nub [ sid | nm <- names, Just sid <- [Map.lookup nm byName] ])

removeClient :: ServerState -> Client -> IO ()
removeClient st client = do
    sid <- readTVarIO client.session
    atomically $ modifyTVar' st.clients (Map.delete client.id)
    attached <- atomically (findSessionById st sid)
    forM_ attached $ \sess -> do
        sname <- readTVarIO sess.name
        notify st "client-detached" (sessionTarget sid)
            [ ("client", PClientRef (clientNameOf (Just client)))
            , ("session", PSessionRef sid sname) ]
    applySessionSize st sid
    logEvent st.logger ClientDetached
        { client = rawClient client.id, reason = "gone" }
    checkUnattached st

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

inputLoop :: Dispatch -> ServerState -> ReadEnd -> Client -> IO ()
inputLoop d st re client = loop
  where
    loop = do
        -- Each timer races the next chunk: a lone trailing ESC held by
        -- escape-time flushes as the Escape key, and a closing repeat
        -- window drops the client back to its default key table.
        held <- readIORef client.escState
        ks <- readIORef client.keyState
        now <- monotonicMs
        opts <- clientOptions st client
        let escWake = case held of
                EscPending -> [(opts.escapeTime, flushHeldEscape d st client)]
                NoEscPending -> []
            repeatWake = case ks.repeating of
                Just r -> [(max 0 (r.expires - now), expireKeyRepeat st client)]
                Nothing -> []
            -- A click waiting for the next one lapses: a second click
            -- becomes a double click.
            clickWake = case ks.mouse.click of
                AwaitSecond c -> [(max 0 (c.expires - now), expireClick d st client)]
                AwaitThird c -> [(max 0 (c.expires - now), expireClick d st client)]
                NoClick -> []
        m <- case sortOn fst (escWake <> repeatWake <> clickWake) of
            [] -> Right <$> recvMessage re
            (ms, fire) : _ -> do
                timer <- registerDelay (ms * 1000)
                either (const (Left fire)) Right <$>
                    race (atomically (readTVar timer >>= check)) (recvMessage re)
        case m of
            Left fire -> fire >> loop
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
                        RData bytes -> showToast st client (TE.decodeUtf8Lenient bytes)
                    loop
                ClientHello {} -> pure ()
                -- Never requested of an attached client.
                StdinData _ -> loop
                StdinEnd -> loop

-- | The click timer lapsed (see 'clickExpired').
expireClick :: Dispatch -> ServerState -> Client -> IO ()
expireClick d st client = do
    now <- monotonicMs
    ks <- readIORef client.keyState
    let (ms, mk) = clickExpired now ks.mouse
    writeIORef client.keyState ks { mouse = ms }
    forM_ mk $ \k -> runInputs d st client [Right k]

-- | The repeat window closed before another key: the client drops back to
-- its default key table.
expireKeyRepeat :: ServerState -> Client -> IO ()
expireKeyRepeat st client = do
    now <- monotonicMs
    before <- readIORef client.keyState
    let after = expireRepeat now before
    writeIORef client.keyState after
    when (prefixArmed before && not (prefixArmed after)) $
        disarmFlash st client False

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

-- | Whether a key event is delivered to the focused pane's program. A sequence
-- the tokenizer could not name is dropped, so bytes hat does not understand
-- never reach an app. Focus in/out reports reach the pane only when
-- @focus-events@ is on and the pane's app enabled focus reporting (?1004); a
-- bare shell never asks, so its focus report is dropped rather than echoed as
-- a stray "^[[I". Every other key is always delivered.
deliversKey :: Options -> Bool -> Key -> Bool
deliversKey opts paneFocusReport k
    | k.name == "Unknown" = False
    | k.name `notElem` ["FocusIn", "FocusOut"] = True
    | otherwise = opts.focusEvents && paneFocusReport

handleInput :: Dispatch -> ServerState -> Client -> B.ByteString -> IO ()
handleInput d st client bs = do
    noteClientActivity st client
    owner <- keyOwner st client
    case owner of
        KeyTables -> handleKeys d st client bs
        _ -> deliverKeys d st client (tokenizeKeys bs)

-- | What a client's keystrokes go to: an open overlay, else its key tables.
data KeyOwner
    = MenuOverlay Window
    | PanesOverlay DisplayPanesState
    | PickerOverlay PickerState
    | PromptOverlay PromptState
    | KeyTables

keyOwner :: ServerState -> Client -> IO KeyOwner
keyOwner st client = do
    menuWin <- atomically $ clientView st client >>= \case
        Just (_, win) -> (\m -> win <$ m) <$> readTVar win.menu
        Nothing -> pure Nothing
    mpanes <- readTVarIO client.displayPanes
    mpicker <- readTVarIO client.picker
    mprompt <- readTVarIO client.prompt
    pure $ case (menuWin, mpanes, mpicker, mprompt) of
        (Just win, _, _, _) -> MenuOverlay win
        (_, Just dp, _, _) -> PanesOverlay dp
        (_, _, Just pk, _) -> PickerOverlay pk
        (_, _, _, Just pr) -> PromptOverlay pr
        _               -> KeyTables

-- | Hand each key to whatever owns the keyboard once the keys before it
-- have run.
deliverKeys :: Dispatch -> ServerState -> Client -> [Key] -> IO ()
deliverKeys _ _ _ [] = pure ()
deliverKeys d st client keys@(k : ks) = keyOwner st client >>= \case
    MenuOverlay win -> do
        handleMenuInput d st client win k
        deliverKeys d st client ks
    PanesOverlay dp -> handlePanesInput d st client dp keys
    PickerOverlay pk -> deliverKeys d st client =<< handlePickerInput d st client pk keys
    PromptOverlay pr -> deliverKeys d st client =<< handlePromptInput d st client pr keys
    KeyTables -> runKeys d st client keys

-- | Keys while the @display-panes@ overlay shows: each key is decided by
-- 'panesKeyAction'; once the overlay closes, the rest of the chunk flows
-- through normal key handling.
handlePanesInput
    :: Dispatch -> ServerState -> Client -> DisplayPanesState -> [Key] -> IO ()
handlePanesInput d st client dp = go
  where
    go [] = pure ()
    go (k : ks) = case panesKeyAction dp.hold k.name of
        PanesSelect num -> do
            dismissPanes st client
            selectPaneNumber d st client dp.template num
            deliverKeys d st client ks
        PanesDismiss -> do
            dismissPanes st client
            deliverKeys d st client ks
        PanesDismissForward -> do
            dismissPanes st client
            deliverKeys d st client (k : ks)
        PanesForward -> do
            runKeys d st client [k]
            go ks

-- | Run the overlay's template against the pane showing @num@ in the
-- client's current window (@%%@ = that pane's id); showing no such
-- number, the digit just closes the overlay.
selectPaneNumber
    :: Dispatch -> ServerState -> Client -> Text -> Int -> IO ()
selectPaneNumber d st client template num = do
    opts <- clientOptions st client
    mwin <- currentWindowOf st (Just client)
    forM_ mwin $ \win -> do
        order <- atomically (map (.id) <$> windowPanes win)
        let numbered = zip [opts.paneBaseIndex ..] order
        forM_ (lookup num numbered) $ \pid -> do
            let target = "%" <> tshow (rawPane pid)
            toastReplies st client
                =<< d.runCommandText st (Just client)
                    (T.replace "%%" target template)

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

-- | How a key arriving at @now@ (ms) is routed, from the client's session
-- options and its active pane's mode table.
keyContext :: Options -> Maybe Text -> Int -> KeyContext
keyContext opts modeTable now = KeyContext
    { prefixKeys = filter (/= "None") [opts.prefix, optionText "prefix2" opts]
    , defaultTable = case optionText "key-table" opts of
        "" -> "root"
        t -> t
    , modeTable = modeTable
    , prefixTimeout = optionNumber "prefix-timeout" opts
    , repeatTime = optionNumber "repeat-time" opts
    , initialRepeatTime = optionNumber "initial-repeat-time" opts
    , mouseOn = optionFlag "mouse" opts
    , now = now
    }

monotonicMs :: IO Int
monotonicMs = fromIntegral . (`div` 1000000) <$> getMonotonicTimeNSec

-- | Tokenize a chunk under the client's escape-time, coalescing any ESC held
-- from the previous chunk, then route the resulting keys. A fresh lone
-- trailing ESC is left in 'escState' for 'inputLoop' to resolve.
handleKeys :: Dispatch -> ServerState -> Client -> B.ByteString -> IO ()
handleKeys d st client bs = do
    opts <- clientOptions st client
    held <- readIORef client.escState
    let users = case optionValue "user-keys" opts of
            OVArray m -> userKeys m
            OVScalar _ -> []
        toks = feedKeys (escTiming opts) users held bs
    writeIORef client.escState toks.escPending
    runKeys d st client toks.escKeys

-- | The escape-time timer fired: forward the held lone ESC as Escape and clear
-- the buffer. A no-op if the ESC was already coalesced by an arriving chunk.
flushHeldEscape :: Dispatch -> ServerState -> Client -> IO ()
flushHeldEscape d st client = do
    held <- readIORef client.escState
    writeIORef client.escState NoEscPending
    deliverKeys d st client (flushEscape held)

-- Keys are routed and run ONE AT A TIME, re-resolving the active pane and
-- its copy-mode table before each. A key that enters or leaves copy mode
-- therefore changes how the very next key in the same input chunk is
-- routed, so @prefix [@ followed immediately by motions never leaks the
-- motions to the shell (and vice versa on exit).
runKeys :: Dispatch -> ServerState -> Client -> [Key] -> IO ()
runKeys d st client keys = runInputs d st client (map Left keys)

-- | Route keys, and mouse keys already placed (a click timer's double
-- click), in order.
runInputs :: Dispatch -> ServerState -> Client -> [Either Key MouseKey] -> IO ()
runInputs d st client keys = do
    opts <- clientOptions st client
    km <- readTVarIO st.keymap
    let loop kst [] = writeIORef client.keyState kst
        loop kst (Right mk : rest) = mouseKey kst mk rest
        loop kst (Left k0 : rest)
            | k0.name == "Mouse" = do
                (kst1, mkey) <- placeMouse kst k0
                case mkey of
                    Nothing -> loop kst1 rest
                    Just mk -> mouseKey kst1 mk rest
            | otherwise = do
                dismissToast st client
                mpane <- clientActivePane st client
                -- A pending vi char search (f/F/t/T) captures this key as its
                -- target, ahead of any keymap lookup.
                searchFed <- maybe (pure False) (feedPendingSearch k0) mpane
                if searchFed then loop kst rest else routed kst mpane k0 Nothing rest
        mouseKey kst1 mk rest
            -- A drag in progress moves what it drags, bypassing the key
            -- tables (tmux's @KEYC_DRAGGING@).
            | mk.dragging, Just dr <- kst1.drag = do
                mouseDragUpdate st dr mk.event
                loop kst1 rest
            | otherwise = do
                let kst2 | "MouseDragEnd" `T.isInfixOf` mk.name = kst1 { drag = Nothing }
                         | otherwise = kst1
                mp <- mouseEventPane st mk.event
                let mpane = fmap (\(_, _, p, _) -> p) mp
                -- focus-follows-mouse: moving over a pane makes it active.
                when (stripMods mk.name == "MouseMovePane") $
                    forM_ mp $ \(sess, win, pane, _) -> do
                        o <- atomically (resolveForPane st sess win pane)
                        active <- readTVarIO win.activeId
                        when (optionFlag "focus-follows-mouse" o && pane.id /= active) $
                            void $ d.runArgv st (Just client)
                                ["select-pane", "-t", "%" <> tshow (rawPane pane.id)]
                routed kst2 mpane (Key { name = mk.name, raw = "" }) (Just mk.event) rest
        -- Route one key with the pane it is for: the active pane, or the one
        -- under a mouse event.
        routed kst mpane k0 mev rest = do
            modeTable <- case mpane of
                Just pane -> atomically $ do
                    top <- paneClockOnTop pane
                    -- clock mode has no key table of its own
                    if top then pure Nothing
                        else fmap (.copyState.keyTable) <$> readTVar pane.mode
                Nothing -> pure Nothing
            k <- reencodeKey (keyEncoding opts) (fmap (.emulator) mpane) k0
            -- The outer terminal's ?1004 focus reports track whether the
            -- user is watching this client, independent of whether the
            -- pane's app asked for them (the 'keepKey' gate below).
            noteOuterFocus st client k
            keep <- keepKey opts mpane k
            if not keep
              then do
                when (k.name == "Unknown") $
                    logEvent st.logger UnknownKeySequence
                        { client = rawClient client.id
                        , input = T.pack (show k.raw) }
                loop kst rest
              else do
                now <- monotonicMs
                let (kst', actions) =
                        routeKey (keyContext opts modeTable now) km kst k
                kst'' <- foldM (act mpane k0 mev) kst' actions
                -- Arming lights the highlight; disarming clears it, unless
                -- the command just moved the active pane — then it lingers
                -- on the new pane to trace the motion.
                flashTransition mpane kst kst''
                -- A key that opened an overlay hands it the rest; overlays
                -- take no mouse input.
                keyOwner st client >>= \case
                    KeyTables -> loop kst'' rest
                    _ -> do
                        writeIORef client.keyState kst''
                        deliverKeys d st client [ key | Left key <- rest ]
        act mpane k0 mev kst = \case
            Passthrough _
                | Just ev <- mev -> kst <$ do
                    spent <- if "MouseMove" `T.isInfixOf` k0.name then pure False
                        else maybe (pure False) (clockModeKey st) mpane
                    unless spent (sendMouse st ev)
                | otherwise -> kst <$ forM_ mpane (\pane -> do
                    spent <- if k0.name `elem` ["FocusIn", "FocusOut"]
                        then pure False
                        else clockModeKey st pane >>= \case
                            True -> pure True
                            False -> dismissDeadPane st pane
                    unless spent $ typeKeyIntoPane st pane k0)
            Paste -> kst <$ forM_ mpane (\pane -> do
                inMode <- isJust <$> readTVarIO pane.mode
                -- The brackets reach only an app that asked for them
                -- (?2004).
                bracketsOk <- if k0.name `elem` ["PasteStart", "PasteEnd"]
                    then Emu.bracketedPaste pane.emulator
                    else pure True
                unless (inMode || not bracketsOk) $
                    typeIntoPaneSynced st pane k0.raw)
            -- The commands see the key's mouse event, and may change the
            -- key state (a drag they start).
            RunCommands cmds -> do
                writeIORef client.keyState kst { mouseEvent = mev }
                withCommandBatch st $ forM_ cmds $ \argv ->
                    toastReplies st client =<< d.runArgv st (Just client) argv
                after <- readIORef client.keyState
                pure after { mouseEvent = Nothing, commandState = Nothing }
    st0 <- readIORef client.keyState
    loop st0 keys
  where
    -- Place a mouse report (see 'checkMouse') against what the client shows.
    placeMouse kst k0 = case parseMouseInput k0.raw of
        Nothing -> pure (kst, Nothing)
        Just inp -> do
            mview <- mouseViewFor st client [inp.y, kst.mouse.prevY]
            case mview of
                Nothing -> pure (kst, Nothing)
                Just view -> do
                    now <- monotonicMs
                    case checkMouse now view kst.mouse inp of
                        (ms, Keyed mk) -> pure (kst { mouse = ms }, Just mk)
                        (ms, Unplaced) -> pure (kst { mouse = ms }, Nothing)
                        -- Outside the modal pane: the drag ends, and a
                        -- click may close the modal pane.
                        (ms, Swallowed mclose) -> do
                            forM_ mclose $ \pid -> void $ d.runArgv st (Just client)
                                ["kill-pane", "-t", "%" <> tshow (rawPane pid)]
                            pure (kst { mouse = ms, drag = Nothing }, Nothing)
    flashTransition mpane before after = case (prefixArmed before, prefixArmed after) of
        (False, True) -> armFlash st client
        (True, False) -> do
            moved <- fmap (fmap (.id)) (clientActivePane st client)
            disarmFlash st client (moved /= fmap (.id) mpane)
        _ -> pure ()
    -- Reads the pane's live focus-reporting mode, then defers the actual
    -- keep/drop decision to the pure 'deliversKey'.
    keepKey opts mpane k
        | k.name `elem` ["FocusIn", "FocusOut"] = do
            report <- maybe (pure False)
                (\pane -> (.focusReport) <$> Emu.modes pane.emulator) mpane
            pure (deliversKey opts report k)
        | otherwise = pure (deliversKey opts False k)
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

controlLoop :: Dispatch -> ServerState -> ReadEnd -> Client -> IO ()
controlLoop d st re client = do
    m <- recvMessage re
    case m of
        Just (Known (Command cmds)) -> do
            awaitStartup st client.autostart cmds
            syncAllPanes st
            open <- newTVarIO True
            let granted = client { stdinSource = StdinFrom (requestStdin re client open) }
            replies <- d.runCommands st (Just granted) cmds
                `finally` atomically (writeTVar open False)
            forM_ replies $ \case
                ROutput out -> send client (Message out)
                RErr e -> send client (ServerError e)
                RData bytes -> forM_ (chunksOf stdoutChunk bytes) $
                    send client . WriteStdout
            -- Report done only once any layout change the command made has
            -- been reconciled into the panes, so a caller that immediately
            -- inspects a pane's size never races the pty resize.
            awaitReconciled st
            send client CommandDone
            controlLoop d st re client
        Just (Known Detach) -> pure ()
        Just (Malformed _) -> pure ()
        Nothing -> pure ()
        _ -> controlLoop d st re client


-- | A control-mode client's life: run the command it was started with
-- (normally an attach), then its command lines, until it exits — by an
-- empty line, the end of its input, a detach, or its session ending — and
-- its writer has sent everything queued and the farewell.
controlModeSession
    :: Dispatch -> ServerState -> ReadEnd -> Client -> ControlState -> [[Text]]
    -> IO ()
controlModeSession d st re client cs setup = do
    awaitStartup st client.autostart setup
    withAsync (subscriptionLoop st client cs) $ \subs -> do
        link subs
        withAsync (controlWriter client cs) $ \writer -> do
            runControlCommands d st client cs SetupCommand (Right setup)
            msess <- currentSession st client
            case msess of
                -- tmux's client that is left unattached by its command exits.
                Nothing -> atomically (requestExit cs Nothing)
                Just sess -> refreshSessionEnv st sess client
            pendingRef <- newIORef B.empty
            void (race (reader pendingRef) (wait writer))
  where
    reader pendingRef = recvMessage re >>= \case
        Nothing -> pure ()
        Just (Malformed err) -> logEvent st.logger ProtocolError
            { client = rawClient client.id, err = T.pack err }
        Just (Known (Input bs)) -> do
            feedControlInput d st client cs pendingRef bs
            reader pendingRef
        -- The client's input ended.
        Just (Known Detach) -> do
            atomically (requestExit cs Nothing)
            reader pendingRef
        Just _ -> reader pendingRef

-- | Collect a command client's standard input over its connection. Only
-- valid while its command batch runs (@open@): the connection's reader is
-- otherwise 'controlLoop'.
requestStdin :: ReadEnd -> Client -> TVar Bool -> IO (Either Text ByteString)
requestStdin re client open = do
    live <- readTVarIO open
    if not live || client.wireLevel < 8
        then pure (Left "Bad file descriptor: -")
        else send client ReadStdin >> collect []
  where
    collect acc = recvMessage re >>= \case
        Just (Known (StdinData bytes)) -> collect (bytes : acc)
        Just (Known StdinEnd) -> pure (Right (B.concat (reverse acc)))
        Just (UnknownTag _) -> collect acc
        _ -> pure (Left "client went away while sending standard input")

-- | 'WriteStdout' payload size, well under the wire's frame limit.
stdoutChunk :: Int
stdoutChunk = 1024 * 1024

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf n bytes
    | B.null bytes = []
    | otherwise = let (h, t) = B.splitAt n bytes in h : chunksOf n t

-- | @kill-server [-a]@: drain every session (a normal shutdown). @-a@ keeps
-- the store's saved tree; a bare kill drops it. See 'saveNow'.
