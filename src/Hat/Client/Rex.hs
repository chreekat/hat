-- | @hat attach --rex@: this process as a client of a Rex server. It mirrors
-- the focused pane of a session into a local emulator and draws it over the
-- whole terminal, read-only. No hat server is involved, and nothing is kept
-- between attaches: each starts from the server's snapshot.
module Hat.Client.Rex
    ( runRexAttach
    , DetachKeys (..)
    , detachKeys
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception (bracket, bracket_, finally)
import Control.Monad (forM_)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import Network.Socket (Socket, close)
import System.Environment (lookupEnv)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (BufferMode (NoBuffering), IOMode (WriteMode), hSetBuffering, stdin, stdout, withBinaryFile)
import System.Info (os)
import System.Posix.IO (stdInput, stdOutput)
import System.Posix.Signals (Handler (Catch), installHandler)

import Hat.Client (ExitReason (..))
import Hat.Client.Draw
import Hat.Client.RexArgs (RexTarget (..))
import Hat.Client.Terminfo (readTermCaps)
import Hat.Client.Tty
import Hat.Geometry (Pos (..), Size (..))
import Hat.Rex.Conn
import Hat.Rex.Control
import Hat.Rex.Data
import Hat.Rex.Session
import Hat.Rex.Stream
import Hat.Rex.Log
import Hat.Rex.View (BlockId (..), SessionId)
import Hat.Server.Render (composeFrame, diffFrame, fullRedraw)
import Hat.Server.View (framePeriod)
import Hat.Term.Caps (createTerm)
import Hat.Term.Emulator (Emulator, Screen (..))
import Hat.Term.Emulator qualified as Emu
import Hat.Term.Pty (sigWinch)
import Hat.Transport.Socket (ConnectFailure (..), connectTo)
import Hat.Transport.Wire (DrawOp (..))

-- | An attach that ends before or after it began, or goes on.
type Attempt a = IO (Either ExitReason a)

refuse :: Text -> ExitReason
refuse = Rejected . TE.encodeUtf8

-- | Go on with an attempt's result, or end with why there is none.
andThen :: Attempt a -> (a -> IO ExitReason) -> IO ExitReason
andThen attempt k = attempt >>= either pure k

-- | Attach to a Rex server and show the pane until the user detaches, the
-- pane or its session ends, or the server is lost.
runRexAttach :: RexTarget -> IO ExitReason
runRexAttach target = do
    inp <- probeTerminal stdInput
    out <- probeTerminal stdOutput
    case diagnoseTerminal inp out of
        Just msg -> pure (refuse msg)
        Nothing -> localDialect `andThen` \dialect -> do
            enc <- getFileSystemEncoding
            path <- B.useAsCStringLen target.socket (GHC.peekCStringLen enc)
            logDir <- lookupEnv "HAT_REX_LOG"
            withLog logDir "control.jsonl" $ \controlLog ->
                withConnection path "/api/control" "rex.control.v1" $ \ws -> do
                let control = Logged
                        { conn = ws
                        , record = \dir -> controlLog . controlLine dir }
                attachPane control target.session `andThen` \(sid, bid, ticket, early) ->
                    withLog logDir (streamLogName bid) $ \streamLog ->
                    withConnection path "/api/data" "rex.data.terminal.v1" $ \stream ->
                    openStream stream streamLog ticket `andThen` \(nonce, emu, leftover) ->
                        showPane dialect control sid bid early
                            (recvStream stream streamLog) nonce emu leftover

-- | A control connection, and where its messages are recorded.
data Logged = Logged
    { conn :: WsConn
    , record :: Direction -> ByteString -> IO ()
    }

-- | Send one control message.
sendControl :: Logged -> Value -> IO ()
sendControl c v = do
    let bs = BL.toStrict (Aeson.encode v)
    c.record ClientToServer bs
    sendText c.conn bs

-- | The next thing from the control connection.
recvControl :: Logged -> IO Received
recvControl c = do
    r <- recvMessage c.conn
    case r of
        GotText bs -> c.record ServerToClient bs
        _ -> pure ()
    pure r

-- | The next thing from a pane's stream after its handshake, its bytes
-- recorded.
recvStream :: WsConn -> (ByteString -> IO ()) -> IO Received
recvStream stream record = do
    r <- recvMessage stream
    case r of
        GotBinary bs -> record bs
        _ -> pure ()
    pure r

-- | Run with a writer to a file of the directory @HAT_REX_LOG@ names; with
-- no directory the writer does nothing.
withLog :: Maybe FilePath -> FilePath -> ((ByteString -> IO ()) -> IO a) -> IO a
withLog mdir name body = case mdir of
    Nothing -> body (\_ -> pure ())
    Just dir -> do
        createDirectoryIfMissing True dir
        withBinaryFile (dir </> name) WriteMode $ \h -> do
            hSetBuffering h NoBuffering
            body (B.hPut h)

-- | The file a pane's stream is recorded in.
streamLogName :: BlockId -> FilePath
streamLogName (BlockId bid) = "stream-" <> map safe (T.unpack bid) <> ".bin"
  where
    safe c = if c == ':' || c == '/' then '-' else c

-- | How this terminal is drawn to, from its terminfo entry alone.
localDialect :: Attempt Dialect
localDialect = do
    mterm <- lookupEnv "TERM"
    colorterm <- lookupEnv "COLORTERM"
    case mterm of
        Just t | not (null t) -> do
            caps <- readTermCaps (T.pack t)
            pure $ either (Left . refuse) (Right . dialectOf) $
                caps >>= \c -> createTerm (T.pack t) c [] [] (T.pack <$> colorterm)
        _ -> pure (Left (refuse "TERM is not set"))

-- | Run with a WebSocket to the server's socket open at a path, closed
-- with a normal goodbye however the action ends.
withConnection :: FilePath -> String -> ByteString -> (WsConn -> IO ExitReason) -> IO ExitReason
withConnection path endpoint subprotocol body = connectTo path >>= \case
    Left NoServer -> pure (refuse ("no Rex server at " <> T.pack path))
    Left (ConnectFailed why) ->
        pure (refuse ("error connecting to " <> T.pack path <> " (" <> T.pack why <> ")"))
    Right sock -> upgraded sock `finally` close sock
  where
    upgraded :: Socket -> IO ExitReason
    upgraded sock = openWs sock endpoint subprotocol >>= \case
        Left why -> pure (refuse ("the Rex server refused the connection: " <> why))
        Right conn -> body conn `finally` closeWs conn 1000

-- | How a connection that has ended is reported.
ended :: Received -> ExitReason
ended = \case
    PeerClosed c | not (T.null c.reason) ->
        refuse ("the Rex server closed the connection: " <> c.reason)
    FramingBroken why -> refuse why
    _ -> ServerDied

-- | Greet the server, pick the session and its focused pane, and get a
-- ticket for the pane's stream. Also gives the events that arrived on the
-- way, oldest first.
attachPane :: Logged -> Maybe Text -> Attempt (SessionId, BlockId, Ticket, [Event])
attachPane control wanted = do
    early <- newIORef []
    let -- The next message that is not an event; events are kept.
        next = recvControl control >>= \case
            GotText bs -> case decodeServerMessage bs of
                Left e -> pure (Left (refuse ("unreadable Rex message: " <> e)))
                Right (EventMessage _ ev) -> modifyIORef' early (ev :) >> next
                Right (UnknownMessage _) -> next
                Right msg -> pure (Right msg)
            GotBinary _ -> next
            other -> pure (Left (ended other))
        call :: Int -> Call -> (Value -> Either Text a) -> Attempt a
        call n c decode = do
            sendControl control (request n c)
            let await = next >>= \case
                    Right (ResponseMessage r) | r.id == T.pack (show n) -> pure $ case r.result of
                        Left err -> Left (refuse (c.method <> ": " <> err.message))
                        Right v -> either (Left . refuse . ((c.method <> ": ") <>)) Right (decode v)
                    Right _ -> await
                    Left e -> pure (Left e)
            await
        orEnd :: Attempt a -> (a -> Attempt b) -> Attempt b
        orEnd attempt k = attempt >>= either (pure . Left) k
        checked = either (Left . refuse) Right
    sendControl control (hello platform)
    next `orEnd` \case
        WelcomeMessage w | Right () <- checkWelcome w ->
            call 1 sessionList decodeSessionList `orEnd` \sessions ->
            pure (checked (chooseSession wanted sessions)) `orEnd` \session ->
            call 2 (sessionAttach session.sessionId) decodeAttach `orEnd` \view ->
            pure (checked (focusedPane view)) `orEnd` \bid ->
            call 3 (dataConnect session.sessionId bid) (\v -> decodeTicket v >>= ticketFromText)
                `orEnd` \ticket -> do
                    events <- reverse <$> readIORef early
                    pure (Right (session.sessionId, bid, ticket, events))
        WelcomeMessage w -> pure (Left (refuse (either id (const "") (checkWelcome w))))
        _ -> pure (Left (refuse "the Rex server did not start with a welcome"))
  where
    platform = case os of
        "linux" -> "linux"
        "darwin" -> "macos"
        "freebsd" -> "freebsd"
        _ -> "other" :: Text

-- | Open a pane's stream with its ticket and decode the snapshot at its
-- head: the channel nonce, the pane's terminal, and the stream bytes
-- already read past the snapshot.
openStream :: WsConn -> (ByteString -> IO ()) -> Ticket -> Attempt (Nonce, Emulator, ByteString)
openStream stream record ticket = do
    sendBinary stream (encodeHello ticket)
    recvMessage stream >>= \case
        GotBinary reply -> case decodeHandshakeReply reply of
            Left e -> pure (Left (refuse e))
            Right (DataReject why) -> pure (Left (refuse (rejectText why)))
            Right (DataWelcome nonce) -> do
                end <- newIORef Nothing
                let chunk = recvStream stream record >>= \case
                        GotBinary bs | B.null bs -> chunk
                                     | otherwise -> pure bs
                        GotText _ -> chunk
                        other -> B.empty <$ writeIORef end (Just other)
                Emu.fromSnapshot historyLimit chunk >>= \case
                    Right (emu, leftover) -> pure (Right (nonce, emu, leftover))
                    Left e -> Left . maybe (refuse e) streamOutcome <$> readIORef end
        GotText _ -> pure (Left (refuse "the Rex server answered the data handshake with text"))
        other -> pure (Left (ended other))
  where
    -- tmux's default @history-limit@.
    historyLimit = 2000

-- | How the end of a pane's data connection ends the attach.
streamOutcome :: Received -> ExitReason
streamOutcome = \case
    PeerClosed c -> case streamEnd c of
        PaneGone -> SessionEnded
        StreamBroken why -> refuse why
    other -> ended other

-- | Draw the pane over the whole terminal and follow its stream until
-- something ends the attach.
showPane
    :: Dialect -> Logged -> SessionId -> BlockId -> [Event]
    -> IO Received -> Nonce -> Emulator -> ByteString -> IO ExitReason
showPane dialect control sid bid early nextStream nonce emu leftover = do
    size <- newTVarIO =<< ttySize
    -- Bumped by whatever changes what a frame would show.
    dirty <- newTVarIO (1 :: Int)
    let touch = modifyTVar' dirty (+ 1)
        onWinch = Catch $ do
            sz <- ttySize
            atomically (writeTVar size sz >> touch)
        mirror carry bs = do
            let (carry', pieces) = splitRex nonce carry bs
            failure <- newIORef Nothing
            forM_ pieces $ \case
                TermBytes t -> Emu.feedMirror emu t
                RexMessage (Resize sz) -> Emu.resize emu sz
                RexMessage (UnknownStreamMessage _) -> pure ()
                StreamError e -> writeIORef failure (Just e)
            atomically touch
            maybe (Right carry') Left <$> readIORef failure
        follow carry = nextStream >>= \case
            GotBinary bs -> mirror carry bs >>= either (pure . refuse) follow
            GotText _ -> follow carry
            other -> pure (streamOutcome other)
        events = \case
            ev : _ | endsAttach sid bid ev -> pure SessionEnded
            _ : rest -> events rest
            [] -> recvControl control >>= \case
                GotText bs -> case decodeServerMessage bs of
                    Left e -> pure (refuse ("unreadable Rex message: " <> e))
                    Right (EventMessage _ ev) -> events [ev]
                    Right _ -> events []
                GotBinary _ -> events []
                other -> pure (ended other)
        keys st = do
            bs <- B.hGetSome stdin 4096
            if B.null bs then pure Detached else case detachKeys st bs of
                Nothing -> pure Detached
                Just st' -> keys st'
        draw prev seen = do
            gen <- atomically $ do
                g <- readTVar dirty
                g <$ check (g /= seen)
            sz <- readTVarIO size
            scr <- Emu.snapshot emu
            let frame = composeFrame sz scr.cells
                ops = case prev of
                    Just (psz, pframe) | psz == sz -> diffFrame pframe frame
                    _ -> fullRedraw frame
                shown = scr.cursorVisible
                    && scr.cursor.row < fromIntegral sz.rows
                    && scr.cursor.col < fromIntegral sz.cols
            B.hPut stdout (opsToAnsi dialect sz (ops <> [CursorAt scr.cursor shown]))
            threadDelay framePeriod
            draw (Just (sz, frame)) gen
    withRawMode $
        bracket_ (B.hPut stdout (enterAltScreen dialect)) (B.hPut stdout (leaveAltScreen dialect)) $
        bracket (installHandler sigWinch onWinch Nothing)
                (\old -> installHandler sigWinch old Nothing) $ \_ ->
            either id (either id (either id id)) <$>
                race (mirror noCarry leftover >>= either (pure . refuse) follow)
                    (race (events early)
                        (race (keys AwaitingKey) (draw Nothing 0)))

-- | Where the detach chord stands.
data DetachKeys
    = AwaitingKey
    | AfterPrefix  -- ^ @C-b@ was the last key
    deriving (Eq, Show)

-- | Follow typed bytes for a detach: tmux's @C-b d@, or a bare @q@ while
-- the pane is read-only. 'Nothing' once one is typed.
detachKeys :: DetachKeys -> ByteString -> Maybe DetachKeys
detachKeys st bs = case B.uncons bs of
    Nothing -> Just st
    Just (b, rest) -> case (st, b) of
        (AfterPrefix, 0x64) -> Nothing  -- d
        (AfterPrefix, _) -> detachKeys AwaitingKey rest
        (AwaitingKey, 0x02) -> detachKeys AfterPrefix rest  -- C-b
        (AwaitingKey, 0x71) -> Nothing  -- q
        (AwaitingKey, _) -> detachKeys AwaitingKey rest
