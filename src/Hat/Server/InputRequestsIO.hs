-- | The server's side of "Hat.Server.InputRequests": asking a client's
-- terminal on a pane's behalf, writing the pane's replies in the order its
-- program asked for them, and lapsing requests no terminal answers.
module Hat.Server.InputRequestsIO
    ( paneReply
    , askTerminal
    , terminalAnswered
    , expireLoop
    , forgetClientRequests
    , forgetPaneRequests
    , paneHolders
    ) where

import Control.Concurrent (threadDelay, withMVar)
import Control.Concurrent.STM
import Control.Monad (filterM, forM_, forever, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Tuple (swap)
import GHC.Clock (getMonotonicTimeNSec)

import Hat.Model
import Hat.Server.ClientIO (send)
import Hat.Server.InputRequests
import Hat.Server.Locate (locatePane)
import Hat.Term.HostProtocol (OscTerm (..))
import Hat.Term.Pty qualified as Pty
import Hat.Transport.Wire (ServerToClient (..))

-- | Write a reply to @pane@'s program (tmux's @input_reply@): at once, or
-- held behind the requests it awaits.
paneReply :: ServerState -> Pane -> ByteString -> IO ()
paneReply st pane bs = withMVar pane.replyLock $ \_ -> do
    now <- monotonicMs
    out <- atomically $ stateTVar st.inputRequests (swap . reply now pane.id bs)
    forM_ out (Pty.writePty pane.pty)

-- | Forward @pane@'s query to the terminal of the client that most recently
-- used a session holding its window (tmux's @input_add_request@); with no
-- such client it goes unanswered.
askTerminal :: ServerState -> Pane -> Asked -> OscTerm -> IO ()
askTerminal st pane a term = do
    now <- monotonicMs
    mc <- withMVar pane.replyLock $ \_ -> atomically $ do
        mc <- requestClient st pane
        forM_ mc $ \c -> modifyTVar' st.inputRequests (ask now pane.id c.id a term)
        pure mc
    -- See Note [Falling behind]
    forM_ mc $ \c -> send c (Notify (queryBytes a))

-- | The query a terminal is asked: tmux's own spellings.
queryBytes :: Asked -> ByteString
queryBytes = \case
    AskPalette n -> "\ESC]4;" <> B8.pack (show n) <> ";?\ESC\\"
    AskClipboard -> "\ESC]52;;?\a"

-- | The attached terminal whose session holds @pane@'s window and that was
-- most recently active, if any.
requestClient :: ServerState -> Pane -> STM (Maybe Client)
requestClient st pane = do
    holders <- paneHolders st pane
    terminals <- filterM
        (\c -> if c.role == Attached && isNothing c.control
            then readTVar c.ready else pure False)
        holders
    stamped <- mapM (\c -> (,) <$> readTVar c.lastActive <*> pure c) terminals
    pure (snd <$> foldl newer Nothing stamped)
  where
    newer best x@(stamp, _) = case best of
        Just (b, _) | b >= stamp -> best
        _ -> Just x

-- | @client@'s terminal answered: write what the request's settling
-- releases to the pane that asked, its answer as @render@ puts it ('Nothing'
-- writes none).
terminalAnswered
    :: ServerState -> Client -> Answer -> (OscTerm -> Answer -> IO (Maybe ByteString))
    -> IO ()
terminalAnswered st client ans render = do
    target <- atomically $ do
        rs <- readTVar st.inputRequests
        case answered client.id ans rs of
            (_, Just (pid, _)) -> fmap Just <$> lookupPane st pid
            (rs', Nothing) -> Nothing <$ writeTVar st.inputRequests rs'
    forM_ target $ \case
        -- The pane closed: its queue goes with it.
        Nothing -> pure ()
        Just pane -> do
            settled <- withMVar pane.replyLock $ \_ -> do
                released <- atomically $ do
                    (rs', out) <- answered client.id ans <$> readTVar st.inputRequests
                    case out of
                        Just (pid, rels) | pid == pane.id ->
                            Just rels <$ writeTVar st.inputRequests rs'
                        _ -> pure Nothing
                forM_ released $ mapM_ $ \case
                    Write bs -> Pty.writePty pane.pty bs
                    Reply term a -> render term a >>= mapM_ (Pty.writePty pane.pty)
                pure released
            -- Another request took the answer meanwhile: settle that one.
            when (isNothing settled) (terminalAnswered st client ans render)

-- | A pane in the tree, by id.
lookupPane :: ServerState -> PaneId -> STM (Maybe Pane)
lookupPane st pid = locatePane st pid >>= \case
    Nothing -> pure Nothing
    Just (_, win) -> Map.lookup pid <$> readTVar win.panes

-- | Lapse requests no terminal answered (tmux's request timer), every tenth
-- of a second while any wait.
expireLoop :: ServerState -> IO ()
expireLoop st = forever $ do
    atomically (readTVar st.inputRequests >>= check . waiting)
    threadDelay 100_000
    now <- monotonicMs
    pids <- Map.keys . (.panes) <$> readTVarIO st.inputRequests
    forM_ pids $ \pid -> do
        mpane <- atomically (lookupPane st pid)
        case mpane of
            Nothing -> atomically (modifyTVar' st.inputRequests (forgetPane pid))
            Just pane -> withMVar pane.replyLock $ \_ -> do
                out <- atomically $
                    stateTVar st.inputRequests (swap . expire now pid)
                mapM_ (Pty.writePty pane.pty) out

-- | Drop a departing client's requests.
forgetClientRequests :: ServerState -> Client -> STM ()
forgetClientRequests st client =
    modifyTVar' st.inputRequests (forgetClient client.id)

-- | Drop the requests of a pane whose program is gone.
forgetPaneRequests :: ServerState -> Pane -> STM ()
forgetPaneRequests st pane = modifyTVar' st.inputRequests (forgetPane pane.id)

-- | The clients whose session holds @pane@'s window, current or not.
paneHolders :: ServerState -> Pane -> STM [Client]
paneHolders st pane = do
    mhome <- readTVar pane.home  -- See Note [Pane home]
    case mhome of
        Nothing -> pure []
        Just (_, win) -> do
            cs <- Map.elems <$> readTVar st.clients
            sessions <- readTVar st.sessions
            flip filterM cs $ \c -> do
                sid <- readTVar c.session
                case Map.lookup sid sessions of
                    Nothing -> pure False
                    Just sess -> any (\w -> w.id == win.id) . Map.elems
                        <$> readTVar sess.windows

monotonicMs :: IO Int
monotonicMs = fromIntegral . (`div` 1000000) <$> getMonotonicTimeNSec
