-- | The thin client: raw tty, byte shuttling, resize events. All the
-- multiplexer logic lives on the other side of the socket.
module Hat.Client
    ( ExitReason (..)
    , Autostart (..)  -- ^ re-exported for 'Main.connectOrStart'
    , runClient
    , runControl
    , nestsOwnServer
    ) where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, bracket_)
import Control.Monad (forever)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import Data.Word (Word16)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Socket (Socket)
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment, lookupEnv)
import System.IO
import System.Posix.IO (stdInput, stdOutput)
import System.Posix.Signals (Handler (Catch), installHandler)

import Hat.Client.Draw
import Hat.Client.Tty
import Hat.Term.Pty (sigWinch)
import Hat.Transport.Wire

data ExitReason
    = Detached
    | SessionEnded
    | ServerDied
    | Rejected Text
    | RestartRequested  -- ^ server asked us to re-exec in place; see 'Main.attach'
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

hello :: Word16 -> Autostart -> Intent -> IO ClientToServer
hello v origin intent = do
    term <- maybe "xterm" T.pack <$> lookupEnv "TERM"
    env0 <- getEnvironment
    sz <- ttySize
    dir <- getCurrentDirectory
    pure $ ClientHello Hello
        { protoVersion = v
        , term = term
        , env = [(T.pack k, T.pack v') | (k, v') <- env0]
        , size = sz
        , cwd = T.pack dir
        , intent = intent
        , autostarted = origin == Autostarted
        }

-- | Attach to the server and shuttle bytes until detach or death. The
-- setup commands (e.g. a @new-session@ or @attach-session -t@ typed at a
-- shell) run server-side to establish which session we render.
runClient :: Socket -> Autostart -> [[Text]] -> IO ExitReason
runClient sock origin setup = do
    inp <- probeTerminal stdInput
    out <- probeTerminal stdOutput
    case diagnoseTerminal inp out of
        Just msg -> pure (Rejected msg)
        Nothing -> attachClient protocolVersion sock origin setup

attachClient :: Word16 -> Socket -> Autostart -> [[Text]] -> IO ExitReason
attachClient v sock origin setup = do
    sendMessage sock =<< hello v origin (AttachIntent setup)
    greeting <- recvMessage sock
    case greeting of
        Just (Known (Welcome _)) ->
            -- withRawMode also owns the stdio buffering switch; see the
            -- ordering note there.
            withRawMode $
                bracket_
                    (B.hPut stdout enterAltScreen)
                    (B.hPut stdout leaveAltScreen)
                    (shuttle sock)
        Just (Known (ServerError e)) -> pure (Rejected e)
        Just (Known _) -> pure (Rejected "unexpected greeting")
        Just (UnknownTag _) -> pure (Rejected versionMismatch)
        Just (Malformed _) -> pure (Rejected versionMismatch)
        Nothing -> pure ServerDied

shuttle :: Socket -> IO ExitReason
shuttle sock = do
    outbox <- newTQueueIO
    _ <- installHandler sigWinch
        (Catch $ ttySize >>= atomically . writeTQueue outbox . Resize)
        Nothing
    -- One writer owns the socket; stdin and SIGWINCH both feed it.
    r <- race (receiver `catch` \(_ :: SomeException) -> pure ServerDied)
              (race (stdinReader outbox) (sender outbox))
    pure $ case r of
        Left reason -> reason
        Right _ -> ServerDied
  where
    receiver = do
        m <- recvMessage sock
        case m of
            Nothing -> pure ServerDied
            Just (Malformed _) -> pure ServerDied
            Just (UnknownTag _) -> receiver
            Just (Known msg) -> case msg of
                Draw ops -> B.hPut stdout (opsToAnsi ops) >> receiver
                SetTitle t -> do
                    B.hPut stdout ("\ESC]0;" <> TE.encodeUtf8 t <> "\BEL")
                    receiver
                RingBell -> B.hPut stdout "\BEL" >> receiver
                Notify raw -> B.hPut stdout raw >> receiver
                Message _ -> receiver  -- server renders toasts into frames
                DetachOk -> pure Detached
                RestartClient -> pure RestartRequested
                CommandDone -> receiver
                Exited -> pure SessionEnded
                ServerError e -> pure (Rejected e)
                Welcome _ -> receiver
                -- Informational: our decoder already reads every dialect
                -- ≤ ours, and nothing we send upstream is dialect-sensitive.
                ServerVersion _ -> receiver
    stdinReader outbox = forever $ do
        bs <- B.hGetSome stdin 4096
        if B.null bs
            then atomically (writeTQueue outbox Detach)
            else atomically (writeTQueue outbox (Input bs))
    sender outbox = forever $ do
        msg <- atomically (readTQueue outbox)
        sendMessage sock msg

-- | Send one command line and print the responses until the server
-- closes or answers. Used by @hat <command>@ from a shell.
runControl :: Socket -> Autostart -> [[Text]] -> IO ExitReason
runControl sock origin cmds = controlAt protocolVersion sock origin cmds

controlAt :: Word16 -> Socket -> Autostart -> [[Text]] -> IO ExitReason
controlAt v sock origin cmds = do
    sendMessage sock =<< hello v origin ControlIntent
    greeting <- recvMessage sock
    case greeting of
        Just (Known (Welcome _)) -> do
            sendMessage sock (Command cmds)
            drain
        Just (Known (ServerError e)) -> pure (Rejected e)
        Just (Known _) -> pure (Rejected "unexpected greeting")
        Just (UnknownTag _) -> pure (Rejected versionMismatch)
        Just (Malformed _) -> pure (Rejected versionMismatch)
        Nothing -> pure ServerDied
  where
    drain = do
        m <- recvMessage sock
        case m of
            Nothing -> pure SessionEnded
            Just (Known (Message t)) -> B8.putStrLn (TE.encodeUtf8 t) >> drain
            Just (Known (ServerError e)) -> pure (Rejected e)
            Just (Known CommandDone) -> pure SessionEnded
            Just (Known Exited) -> pure SessionEnded
            Just (Known _) -> drain
            Just (UnknownTag _) -> drain
            Just (Malformed err) ->
                pure (Rejected ("wire protocol error: " <> T.pack err
                    <> " — mismatched hat versions?"))
