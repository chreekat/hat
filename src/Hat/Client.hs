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
    , forwardToServer
    , ClientRender (..)
    , Step (..)
    , renderStep
    , receiveServer
    , declinedLock
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, catch, bracket_, try)
import Control.Monad (forever, unless)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
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
import System.Posix.Terminal (getTerminalName, queryTerminal)

import Hat.Client.Draw
import Hat.Client.Terminfo (readTermCaps)
import Hat.Client.Tty
import Hat.Geometry (Size)
import Hat.Command.Arg (Arg (..), argBytes, argLossy)
import Hat.Server.Command.Session (newSessionValueFlags)
import Hat.Server.Command.Types (parseArgs)
import Hat.Term.Caps (termFromWire)
import Hat.Term.Pty (sigWinch)
import Hat.Transport.Wire

data ExitReason
    = Detached
    | SessionEnded
    | ServerDied
    | Rejected B.ByteString  -- ^ why, for standard error verbatim
    | RestartRequested (Maybe Text) [Text]
        -- ^ server asked us to re-exec in place, naming the session to
        --   rejoin (when it knows it) and the MRU session history to carry
        --   back; see 'Main.attach'
    | CommandsFinished Int  -- ^ the commands ran; exit with this status
    deriving (Eq, Show)

versionMismatch :: Text
versionMismatch = "unexpected greeting — mismatched hat versions?"

-- | 'Rejected' for a reason in text.
rejected :: Text -> ExitReason
rejected = Rejected . TE.encodeUtf8

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
-- terminfo entry first, and fails if there is none. @cmds@ are the
-- commands the connection carries, whose argv the hello holds when their
-- text is not exact.
withHello
    :: Word16 -> Autostart -> Intent -> [[Arg]] -> [Text] -> (Text -> r)
    -> (ClientToServer -> IO r) -> IO r
withHello v origin intent cmds hist refuse body = do
    mterm <- lookupEnv "TERM"
    tty <- queryTerminal stdInput
    ecaps <- case mterm of
        Just t | tty, not (null t) -> readTermCaps (T.pack t)
        _ -> pure (Right [])
    env0 <- getEnvironment
    sz <- ttySize
    dir <- getCurrentDirectory
    mtty <- if tty then Just <$> getTerminalName stdInput else pure Nothing
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
            , argvBytes = case mtty of
                -- A terminal's path travels only beside raw argv.
                Just _ -> RawArgv (map (map argBytes) cmds)
                Nothing | all (all isText) cmds -> TextIsExact
                Nothing -> RawArgv (map (map argBytes) cmds)
            , ttyName = T.pack <$> mtty
            }
  where
    isText = \case
        Utf8Arg _ -> True
        RawArg _ -> False

-- | Attach to the server and shuttle bytes until detach or death. The
-- setup commands (e.g. a @new-session@ or @attach-session -t@ typed at a
-- shell) run server-side to establish which session we render. @hist@ seeds
-- @switch-client -l@ across a restart and is empty for a fresh attach.
-- @answered@ runs once the server first answers.
runClient :: Socket -> Autostart -> IO () -> [[Arg]] -> [Text] -> IO ExitReason
runClient sock origin answered setup hist = do
    inp <- probeTerminal stdInput
    out <- probeTerminal stdOutput
    case diagnoseTerminal inp out of
        Just msg -> pure (rejected msg)
        Nothing -> attachClient protocolVersion sock origin answered setup hist

attachClient
    :: Word16 -> Socket -> Autostart -> IO () -> [[Arg]] -> [Text] -> IO ExitReason
attachClient v sock origin answered setup hist =
    withHello v origin (AttachIntent (lossy setup)) setup hist rejected $ \h -> do
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
            sz <- ttySize
            render <- newIORef ClientRender { dialect = d, size = sz }
            -- withRawMode also owns the stdio buffering switch; see the
            -- ordering note there.
            withRawMode $
                bracket_
                    (B.hPut stdout (enterAltScreen d))
                    (B.hPut stdout . leaveAltScreen . (.dialect) =<< readIORef render)
                    (shuttle render sock re)
        Just (Known (ServerError e)) -> pure (rejected e)
        Just (Known (ServerErrorBytes e)) -> pure (Rejected e)
        Just (Known _) -> pure (Rejected "unexpected greeting")
        Just (UnknownTag _) -> pure (rejected versionMismatch)
        Just (Malformed _) -> pure (rejected versionMismatch)
        Nothing -> pure ServerDied

shuttle :: IORef ClientRender -> Socket -> ReadEnd -> IO ExitReason
shuttle render sock re = do
    outbox <- newTQueueIO
    _ <- installHandler sigWinch
        (Catch $ do
            sz <- ttySize
            atomicModifyIORef' render $ \r ->
                (ClientRender { dialect = r.dialect, size = sz }, ())
            atomically (writeTQueue outbox (Resize sz)))
        Nothing
    -- One writer owns the socket; stdin and SIGWINCH both feed it.
    r <- race (receiveServer (B.hPut stdout) render outbox re
                `catch` \(_ :: SomeException) -> pure ServerDied)
              (race (stdinReader outbox) (forwardToServer sock outbox))
    pure $ case r of
        Left reason -> reason
        Right _ -> ServerDied
  where
    stdinReader outbox = forever $ do
        bs <- B.hGetSome stdin 4096
        if B.null bs
            then atomically (writeTQueue outbox Detach)
            else atomically (writeTQueue outbox (Input bs))

-- | What an attached client keeps between the server's messages.
data ClientRender = ClientRender
    { dialect :: Dialect  -- ^ how its terminal is spoken to
    , size :: Size        -- ^ its terminal's size, as of the last SIGWINCH
    }

-- | What one of the server's messages does at an attached client.
data Step = Step
    { render :: ClientRender       -- ^ the client afterwards
    , output :: B.ByteString       -- ^ for its terminal
    , replies :: [ClientToServer]  -- ^ for the server
    , outcome :: Maybe ExitReason  -- ^ 'Just' once the attachment is over
    }

-- | Act on one of the server's messages to an attached client. The loop
-- around it ('receiveServer') reads and writes; this decides.
renderStep :: ClientRender -> ServerToClient -> Step
renderStep r = \case
    Draw ops -> writes (opsToAnsi r.dialect r.size ops)
    SetTitle t -> writes (titleBytes r.dialect t)
    RingBell -> writes "\BEL"
    Notify raw -> writes raw
    Message _ -> silent  -- server renders toasts into frames
    DetachOk -> ends Detached
    RestartClient -> ends (RestartRequested Nothing [])
    RestartClientTo t hist -> ends (RestartRequested (Just t) hist)
    CommandDone -> silent
    Exited -> ends SessionEnded
    ServerError e -> ends (rejected e)
    ServerErrorBytes e -> ends (Rejected e)
    Welcome _ -> silent
    -- The terminal identified itself after attach and the server learned
    -- more of what it can do.
    OpenedTerminal caps feats ->
        let d = dialectOf (termFromWire "" caps feats)
        in (writes (featuresOn d)) { render = r { dialect = d } }
    -- Informational: our decoder already reads every dialect ≤ ours, and
    -- nothing we send upstream is dialect-sensitive.
    ServerVersion _ -> silent
    -- Command-client traffic; an attached client's stdin and stdout are
    -- its terminal.
    ReadStdin -> silent
    WriteStdout _ -> silent
    ControlExit {} -> ends SessionEnded
    ExitStatus _ -> silent
    LockClient _ -> silent { replies = declinedLock }
  where
    silent = Step { render = r, output = "", replies = [], outcome = Nothing }
    writes bs = silent { output = bs }
    ends why = silent { outcome = Just why }

-- | Act on what the server sends an attached client, writing to its
-- terminal through @out@ and answering through @outbox@, until the
-- server ends the attachment.
receiveServer
    :: (B.ByteString -> IO ()) -> IORef ClientRender
    -> TQueue ClientToServer -> ReadEnd -> IO ExitReason
receiveServer out render outbox re = next
  where
    next = recvMessage re >>= \case
        Nothing -> pure ServerDied
        Just (Malformed _) -> pure ServerDied
        Just (UnknownTag _) -> next
        Just (Known msg) -> do
            -- Stepped atomically against the SIGWINCH handler's size writes.
            step <- atomicModifyIORef' render $ \r ->
                let s = renderStep r msg in (s.render, s)
            unless (B.null step.output) (out step.output)
            unless (null step.replies) $
                atomically (mapM_ (writeTQueue outbox) step.replies)
            maybe next pure step.outcome

-- | The answer to a server old enough to lock clients: the terminal back
-- at once, which that server draws to afresh, and a message saying why.
declinedLock :: [ClientToServer]
declinedLock =
    [ LockDone
    , Command [["display-message", "hat: locking is not supported by this client"]] ]

-- | The one writer of a client's socket: send what the client queues for
-- the server until its end is gone, then drop the rest. How the session
-- ended is the receiver's to say, from what the server sent last.
forwardToServer :: Socket -> TQueue ClientToServer -> IO ()
forwardToServer sock outbox = do
    msg <- atomically (readTQueue outbox)
    try (sendMessage sock msg) >>= \case
        Right () -> forwardToServer sock outbox
        Left (_ :: IOException) -> forever (atomically (readTQueue outbox))

-- | How a control-mode client talks to its terminal: plain lines (@-C@),
-- or wrapped in tmux's DCS for a terminal that drives it (@-CC@).
data ControlFlavour = ControlLines | ControlDcs
    deriving (Eq, Show)

-- | tmux control mode (@hat -C@): forward standard input to the server as
-- command lines and copy the protocol it sends to standard output, until
-- the server's farewell; then print @%exit@ and, if asked, linger for an
-- empty input line. Returns the exit status the server gave.
runControlMode
    :: Socket -> Autostart -> ControlFlavour -> [[Arg]] -> IO (Either ExitReason Int)
runControlMode sock origin flavour setup =
    withHello protocolVersion origin (ControlModeIntent (lossy setup)) setup [] (Left . rejected) $ \h -> do
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
        Just (Known (ServerError e)) -> pure (Left (rejected e))
        Just (Known (ServerErrorBytes e)) -> pure (Left (Rejected e))
        Just (Known _) -> pure (Left (Rejected "unexpected greeting"))
        Just (UnknownTag _) -> pure (Left (rejected versionMismatch))
        Just (Malformed _) -> pure (Left (rejected versionMismatch))
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
runControl :: Socket -> Autostart -> IO () -> [[Arg]] -> IO ExitReason
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
        } (map (map Utf8Arg) cmds)
    case r of
        SessionEnded -> Right . B.concat . reverse <$> readIORef acc
        other -> pure (Left other)

controlAt :: Word16 -> Socket -> Autostart -> IO () -> Output -> [[Arg]] -> IO ExitReason
controlAt v sock origin answered out cmds =
    withHello v origin ControlIntent cmds [] rejected $ \h -> do
    re <- newReadEnd sock
    sendMessage sock h
    -- A server exiting as we connect resets the connection: it never saw us.
    greeting <- recvMessage re `catch` \(_ :: IOException) -> pure Nothing
    answered
    case greeting of
        Just (Known (Welcome _)) -> do
            sendMessage sock (Command (lossy cmds))
            drain Nothing re
        Just (Known (ServerError e)) -> pure (rejected e)
        Just (Known (ServerErrorBytes e)) -> pure (Rejected e)
        Just (Known _) -> pure (Rejected "unexpected greeting")
        Just (UnknownTag _) -> pure (rejected versionMismatch)
        Just (Malformed _) -> pure (rejected versionMismatch)
        Nothing -> pure ServerDied
  where
    drain status re = do
        let next = drain status re
            done = pure (maybe SessionEnded CommandsFinished status)
        m <- recvMessage re
        case m of
            Nothing -> done
            Just (Known (Message t)) -> out.line t >> next
            Just (Known (WriteStdout bs)) -> out.bytes bs >> next
            Just (Known ReadStdin) -> sendStdin re.sock >> next
            Just (Known (ExitStatus n)) -> drain (Just n) re
            Just (Known (ServerError e)) -> pure (rejected e)
            Just (Known (ServerErrorBytes e)) -> pure (Rejected e)
            Just (Known CommandDone) -> done
            Just (Known Exited) -> done
            Just (Known _) -> next
            Just (UnknownTag _) -> next
            Just (Malformed err) ->
                pure (rejected ("wire protocol error: " <> T.pack err
                    <> " — mismatched hat versions?"))

-- | The commands as the wire's text carriers hold them.
lossy :: [[Arg]] -> [[Text]]
lossy = map (map argLossy)
