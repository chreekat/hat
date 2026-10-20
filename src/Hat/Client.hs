-- | The thin client: raw tty, byte shuttling, resize events. All the
-- multiplexer logic lives on the other side of the socket.
module Hat.Client
    ( ExitReason (..)
    , Autostart (..)  -- ^ re-exported for 'Main.connectOrStart'
    , runClient
    , runControl
    , captureControl
    , ControlFlavour (..)
    , runControlMode
    , awaitEmptyLine
    , nestsOwnServer
    , TerminalMode (..)
    , terminalMode
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception
    ( AsyncException (UserInterrupt), IOException, SomeException, catch, bracket_
    , throwIO, try )
import Control.Monad (forever, void, when)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Word (Word16)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.Socket (Socket)
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment, lookupEnv)
import System.IO
import System.Posix.IO (stdInput, stdOutput)
import System.Posix.Signals (Handler (Catch), installHandler)
import System.Posix.Terminal (queryTerminal)
import System.Process (CreateProcess (..), createProcess, shell, waitForProcess)

import Hat.Client.Draw
import Hat.Client.Terminfo (readTermCaps)
import Hat.Client.Tty
import Hat.Server.Command.Session (newSessionValueFlags)
import Hat.Server.Command.Types (parseArgs)
import Hat.Term.Caps (termFromWire)
import Hat.Term.Pty (sigWinch)
import Hat.Transport.Wire

data ExitReason
    = Detached
    | SessionEnded
    | ServerDied
    | Rejected Text
    | RestartRequested (Maybe Text) [Text]
        -- ^ server asked us to re-exec in place, naming the session to
        --   rejoin (when it knows it) and the MRU session history to carry
        --   back; see 'Main.attach'
    deriving (Eq, Show)

versionMismatch :: Text
versionMismatch = "unexpected greeting — mismatched hat versions?"

-- | Whether attaching to @target@ from inside a pane would nest a client
-- in its own server: @$TMUX@'s first comma-field is the enclosing server's
-- socket path. Attaching to a different server from a pane is fine (tmux
-- allows it too; its check matches the client tty against its own panes).
nestsOwnServer :: String -> FilePath -> Bool
nestsOwnServer tmuxVar target =
    not (null tmuxVar) && takeWhile (/= ',') tmuxVar == target

-- | Whether a command line grabs this terminal to render, or runs and exits.
data TerminalMode
    = GrabsTerminal  -- ^ establish the session server-side, then attach and render
    | FireAndForget  -- ^ run as a control command and exit
    deriving (Eq, Show)

-- | Verbs that grab this terminal and render, running server-side first to
-- establish the session: @new-session@/@new@ (unless detached with @-d@)
-- and @attach-session@/@attach@ with an explicit target. A bare @new@ still
-- attaches; a @new -d@ stays fire-and-forget.
terminalMode :: [[Text]] -> TerminalMode
terminalMode [verb : args]
    | verb `elem` ["new-session", "new"] =
        let (_, flags, _) = parseArgs newSessionValueFlags args
        in if "-d" `notElem` flags then GrabsTerminal else FireAndForget
    | verb `elem` ["attach-session", "attach"] = GrabsTerminal
terminalMode _ = FireAndForget

-- | Run with the hello, or answer why this terminal cannot be used: as in
-- tmux, a client whose standard input is a terminal reads its @TERM@'s
-- terminfo entry first, and fails if there is none.
withHello
    :: Word16 -> Autostart -> Intent -> [Text] -> (Text -> r)
    -> (ClientToServer -> IO r) -> IO r
withHello v origin intent hist refuse body = do
    mterm <- lookupEnv "TERM"
    tty <- queryTerminal stdInput
    ecaps <- case mterm of
        Just t | tty, not (null t) -> readTermCaps (T.pack t)
        _ -> pure (Right [])
    env0 <- getEnvironment
    sz <- ttySize
    dir <- getCurrentDirectory
    case ecaps of
        Left e -> pure (refuse e)
        Right caps -> body $ ClientHello Hello
            { protoVersion = v
            , term = maybe "" T.pack mterm
            , env = [(T.pack k, T.pack v') | (k, v') <- env0]
            , size = sz
            , cwd = T.pack dir
            , intent = intent
            , autostarted = origin == Autostarted
            , sessionHist = hist
            , termCaps = Just caps
            }

-- | Attach to the server and shuttle bytes until detach or death. The
-- setup commands (e.g. a @new-session@ or @attach-session -t@ typed at a
-- shell) run server-side to establish which session we render. @hist@ seeds
-- @switch-client -l@ across a restart and is empty for a fresh attach.
-- @answered@ runs once the server first answers.
runClient :: Socket -> Autostart -> IO () -> [[Text]] -> [Text] -> IO ExitReason
runClient sock origin answered setup hist = do
    inp <- probeTerminal stdInput
    out <- probeTerminal stdOutput
    case diagnoseTerminal inp out of
        Just msg -> pure (Rejected msg)
        Nothing -> attachClient protocolVersion sock origin answered setup hist

attachClient
    :: Word16 -> Socket -> Autostart -> IO () -> [[Text]] -> [Text] -> IO ExitReason
attachClient v sock origin answered setup hist =
    withHello v origin (AttachIntent setup) hist Rejected $ \h -> do
    re <- newReadEnd sock
    sendMessage sock h
    greeting <- recvMessage re
    answered
    awaitWelcome sock re legacyDialect greeting

-- | Read the greeting, drawing in the dialect of the terminal the server
-- says it opened ahead of it; a server that predates that says nothing.
awaitWelcome
    :: Socket -> ReadEnd -> Dialect -> Maybe (Inbound ServerToClient) -> IO ExitReason
awaitWelcome sock re d greeting =
    case greeting of
        Just (Known (OpenedTerminal caps feats)) ->
            awaitWelcome sock re (dialectOf (termFromWire "" caps feats))
                =<< recvMessage re
        Just (Known (Welcome _)) -> do
            dialect <- newIORef d
            -- withRawMode also owns the stdio buffering switch; see the
            -- ordering note there.
            withRawMode $ \cooked ->
                bracket_
                    (B.hPut stdout (enterAltScreen d))
                    (B.hPut stdout . leaveAltScreen =<< readIORef dialect)
                    (shuttle dialect cooked sock re)
        Just (Known (ServerError e)) -> pure (Rejected e)
        Just (Known _) -> pure (Rejected "unexpected greeting")
        Just (UnknownTag _) -> pure (Rejected versionMismatch)
        Just (Malformed _) -> pure (Rejected versionMismatch)
        Nothing -> pure ServerDied

shuttle :: IORef Dialect -> CookedMode -> Socket -> ReadEnd -> IO ExitReason
shuttle dialect cooked sock re = do
    outbox <- newTQueueIO
    pump <- newTVarIO Pumping
    size <- newIORef =<< ttySize
    _ <- installHandler sigWinch
        (Catch $ do
            sz <- ttySize
            writeIORef size sz
            atomically (writeTQueue outbox (Resize sz)))
        Nothing
    -- One writer owns the socket; stdin and SIGWINCH both feed it.
    r <- race (receiver size pump outbox `catch` \(_ :: SomeException) -> pure ServerDied)
              (race (stdinReader pump outbox) (sender outbox))
    pure $ case r of
        Left reason -> reason
        Right _ -> ServerDied
  where
    receiver size pump outbox = do
        let next = receiver size pump outbox
        m <- recvMessage re
        case m of
            Nothing -> pure ServerDied
            Just (Malformed _) -> pure ServerDied
            Just (UnknownTag _) -> next
            Just (Known msg) -> case msg of
                Draw ops -> do
                    d <- readIORef dialect
                    sz <- readIORef size
                    B.hPut stdout (opsToAnsi d sz ops)
                    next
                SetTitle t -> do
                    d <- readIORef dialect
                    B.hPut stdout (titleBytes d t)
                    next
                RingBell -> B.hPut stdout "\BEL" >> next
                Notify raw -> B.hPut stdout raw >> next
                Message _ -> next  -- server renders toasts into frames
                DetachOk -> pure Detached
                RestartClient -> pure (RestartRequested Nothing [])
                RestartClientTo t hist -> pure (RestartRequested (Just t) hist)
                CommandDone -> next
                Exited -> pure SessionEnded
                ServerError e -> pure (Rejected e)
                Welcome _ -> next
                -- The terminal identified itself after attach and the
                -- server learned more of what it can do.
                OpenedTerminal caps feats -> do
                    let d = dialectOf (termFromWire "" caps feats)
                    writeIORef dialect d
                    B.hPut stdout (featuresOn d)
                    next
                -- Informational: our decoder already reads every dialect
                -- ≤ ours, and nothing we send upstream is dialect-sensitive.
                ServerVersion _ -> next
                -- Command-client traffic; an attached client's stdin and
                -- stdout are its terminal.
                ReadStdin -> next
                WriteStdout _ -> next
                ControlExit {} -> pure SessionEnded
                LockClient cmd -> do
                    d <- readIORef dialect
                    pausingInput pump (lockTerminal d cooked cmd)
                    atomically (writeTQueue outbox LockDone)
                    next
    -- A read in flight when a pause is asked is abandoned, so no keystroke
    -- meant for a lock command is taken first.
    stdinReader pump outbox = forever $ do
        open <- atomically $ readTVar pump >>= \case
            Pumping -> pure True
            PauseAsked -> False <$ writeTVar pump Paused
            Paused -> retry
        when open $ do
            r <- race (atomically (readTVar pump >>= check . (/= Pumping)))
                (B.hGetSome stdin 4096)
            case r of
                Left () -> pure ()
                Right bs
                    | B.null bs -> atomically (writeTQueue outbox Detach)
                    | otherwise -> atomically (writeTQueue outbox (Input bs))
    sender outbox = forever $ do
        msg <- atomically (readTQueue outbox)
        sendMessage sock msg

-- | Whether 'shuttle' reads the terminal: a lock command has it while
-- paused.
data InputPump = Pumping | PauseAsked | Paused
    deriving (Eq, Show)

-- | Run an action with the terminal's input left unread.
pausingInput :: TVar InputPump -> IO a -> IO a
pausingInput pump = bracket_
    (do atomically (writeTVar pump PauseAsked)
        atomically (readTVar pump >>= check . (== Paused)))
    (atomically (writeTVar pump Pumping))

-- | Hand the terminal to the lock command until it exits (tmux's client
-- @MSG_LOCK@): out of raw mode and the server's screen, onto 'lockScreen';
-- back again however the command ends. As with tmux's @system@, the
-- command's own exit status is no concern of ours.
lockTerminal :: Dialect -> CookedMode -> Text -> IO ()
lockTerminal d cooked cmd =
    bracket_ (B.hPut stdout (leaveAltScreen d)) (B.hPut stdout (enterAltScreen d)) $
        withCookedMode cooked $ do
            B.hPut stdout lockScreen
            r <- try $ do
                (_, _, _, ph) <- createProcess (shell (T.unpack cmd)) { delegate_ctlc = True }
                -- delegate_ctlc reports a command a Ctrl-C ended as a
                -- UserInterrupt.
                void (waitForProcess ph) `catch` \case
                    UserInterrupt -> pure ()
                    e -> throwIO e
            case r of
                Left (e :: IOException) ->
                    hPutStrLn stderr ("hat: lock-command: " <> show e)
                Right () -> pure ()

-- | How a control-mode client talks to its terminal: plain lines (@-C@),
-- or wrapped in tmux's DCS for a terminal that drives it (@-CC@).
data ControlFlavour = ControlLines | ControlDcs
    deriving (Eq, Show)

-- | tmux control mode (@hat -C@): forward standard input to the server as
-- command lines and copy the protocol it sends to standard output, until
-- the server's farewell; then print @%exit@ and, if asked, linger for an
-- empty input line. Returns the exit status the server gave.
runControlMode
    :: Socket -> Autostart -> ControlFlavour -> [[Text]] -> IO (Either ExitReason Int)
runControlMode sock origin flavour setup =
    withHello protocolVersion origin (ControlModeIntent setup) [] (Left . Rejected) $ \h -> do
    re <- newReadEnd sock
    sendMessage sock h
    greeting <- recvMessage re
    case greeting of
        Just (Known (Welcome _)) -> do
            hSetBinaryMode stdin True
            hSetBinaryMode stdout True
            hSetBuffering stdout (BlockBuffering Nothing)
            withControlTty flavour $ do
                r <- race (receiver re) pumpStdin
                case r of
                    Left (Just (reason, wait, status)) -> do
                        B8.putStr (exitLine reason)
                        hFlush stdout
                        case wait of
                            AwaitEmptyLine -> awaitEmptyLine stdin
                            ExitNow -> pure ()
                        pure (Right status)
                    _ -> pure (Left ServerDied)
        Just (Known (ServerError e)) -> pure (Left (Rejected e))
        Just (Known _) -> pure (Left (Rejected "unexpected greeting"))
        Just (UnknownTag _) -> pure (Left (Rejected versionMismatch))
        Just (Malformed _) -> pure (Left (Rejected versionMismatch))
        Nothing -> pure (Left ServerDied)
  where
    receiver re = recvMessage re >>= \case
        Just (Known (WriteStdout bs)) -> do
            B.hPut stdout bs
            hFlush stdout
            receiver re
        Just (Known (ControlExit reason wait status)) ->
            pure (Just (reason, wait, status))
        Just (Known _) -> receiver re
        Just (UnknownTag _) -> receiver re
        Just (Malformed _) -> pure Nothing
        Nothing -> pure Nothing
    -- The end of input is the end of commands; the server decides when
    -- the farewell comes, so the pump then just waits for it.
    pumpStdin = do
        bs <- B.hGetSome stdin 4096
        if B.null bs
            then sendMessage sock Detach >> forever (threadDelay maxBound)
            else sendMessage sock (Input bs) >> pumpStdin
    exitLine reason = "%exit" <> maybe "" ((" " <>) . TE.encodeUtf8) reason <> "\n"

-- | Bracket a control-mode session's terminal: @-CC@ opens tmux's DCS
-- (@\\eP1000p@) before and closes it (@\\e\\\\@) after.
withControlTty :: ControlFlavour -> IO a -> IO a
withControlTty flavour act = case flavour of
    ControlLines -> act
    ControlDcs -> do
        B.hPut stdout "\ESCP1000p" >> hFlush stdout
        r <- act
        B.hPut stdout "\ESC\\" >> hFlush stdout
        pure r

-- | Read input until an empty line or its end — lines already read count,
-- however many arrived together.
awaitEmptyLine :: Handle -> IO ()
awaitEmptyLine h = go B.empty
  where
    go buf = case B8.elemIndex '\n' buf of
        Just 0 -> pure ()
        Just i -> go (B.drop (i + 1) buf)
        Nothing -> do
            chunk <- B.hGetSome h 4096
            if B.null chunk then pure () else go (buf <> chunk)

-- | Answer 'ReadStdin': stream standard input to the server until EOF. An
-- unreadable stdin ends the stream where it failed.
sendStdin :: Socket -> IO ()
sendStdin sock = do
    hSetBinaryMode stdin True
    let pump = do
            chunk <- B.hGetSome stdin (1024 * 1024)
            if B.null chunk
                then pure ()
                else sendMessage sock (StdinData chunk) >> pump
    pump `catch` \(e :: IOException) ->
        hPutStrLn stderr ("hat: reading standard input: " <> show e)
    sendMessage sock StdinEnd

-- | Where a command client's output goes: its text lines, and its raw
-- standard-output bytes.
data Output = Output
    { line :: Text -> IO ()
    , bytes :: B.ByteString -> IO ()
    }

-- | Send one command line and print the responses until the server
-- closes or answers. Used by @hat <command>@ from a shell. @answered@ runs
-- once the server first answers.
runControl :: Socket -> Autostart -> IO () -> [[Text]] -> IO ExitReason
runControl sock origin answered =
    controlAt protocolVersion sock origin answered Output
        { line = B8.putStrLn . TE.encodeUtf8
        , bytes = B.hPut stdout
        }

-- | 'runControl', handing back the commands' output instead of printing it.
captureControl :: Socket -> Autostart -> [[Text]] -> IO (Either ExitReason B.ByteString)
captureControl sock origin cmds = do
    acc <- newIORef []
    let keep bs = modifyIORef' acc (bs :)
    r <- controlAt protocolVersion sock origin (pure ()) Output
        { line = keep . (<> "\n") . TE.encodeUtf8
        , bytes = keep
        } cmds
    case r of
        SessionEnded -> Right . B.concat . reverse <$> readIORef acc
        other -> pure (Left other)

controlAt :: Word16 -> Socket -> Autostart -> IO () -> Output -> [[Text]] -> IO ExitReason
controlAt v sock origin answered out cmds =
    withHello v origin ControlIntent [] Rejected $ \h -> do
    re <- newReadEnd sock
    sendMessage sock h
    -- A server exiting as we connect resets the connection: it never saw us.
    greeting <- recvMessage re `catch` \(_ :: IOException) -> pure Nothing
    answered
    case greeting of
        Just (Known (Welcome _)) -> do
            sendMessage sock (Command cmds)
            drain re
        Just (Known (ServerError e)) -> pure (Rejected e)
        Just (Known _) -> pure (Rejected "unexpected greeting")
        Just (UnknownTag _) -> pure (Rejected versionMismatch)
        Just (Malformed _) -> pure (Rejected versionMismatch)
        Nothing -> pure ServerDied
  where
    drain re = do
        m <- recvMessage re
        case m of
            Nothing -> pure SessionEnded
            Just (Known (Message t)) -> out.line t >> drain re
            Just (Known (WriteStdout bs)) -> out.bytes bs >> drain re
            Just (Known ReadStdin) -> sendStdin re.sock >> drain re
            Just (Known (ServerError e)) -> pure (Rejected e)
            Just (Known CommandDone) -> pure SessionEnded
            Just (Known Exited) -> pure SessionEnded
            Just (Known _) -> drain re
            Just (UnknownTag _) -> drain re
            Just (Malformed err) ->
                pure (Rejected ("wire protocol error: " <> T.pack err
                    <> " — mismatched hat versions?"))
