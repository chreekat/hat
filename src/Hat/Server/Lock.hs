-- | Locking clients (tmux's @server_lock_client@ and the session lock
-- timer): the server stops drawing to a client's terminal and hands it to
-- @lock-command@, which the client runs in its own terminal; the client
-- hands the terminal back when the command exits ('LockDone'), and the
-- server draws it afresh.
module Hat.Server.Lock
    ( LockOutcome (..)
    , lockClient
    , lockSessionClients
    , refusedLocks
    , ttyHandedBack
    , armLockTimer
    , lockDeadline
    , lockTimerLoop
    ) where

import Control.Concurrent.STM
import Control.Monad (forM, forM_, forever, when)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16, Word64)
import GHC.Clock (getMonotonicTimeNSec)

import Hat.Model
import Hat.Model.Options (optionNumber, optionText)
import Hat.Server.ClientIO (send)
import Hat.Server.Resize (applySessionSize)
import Hat.Server.Toast (showToast)
import Hat.Transport.Wire (ServerToClient (LockClient))

-- | What 'lockClient' did with one client.
data LockOutcome
    = Locked
    | Passed  -- ^ nothing to lock, as tmux passes over it: a control client,
              --   a terminal already locked, an empty @lock-command@
    | TooOld Word16  -- ^ the client's hat, at this dialect, cannot hand
                     --   over its terminal
    deriving (Eq, Show)

-- | The dialect 'LockClient' arrived in.
lockLevel :: Word16
lockLevel = 10

-- | Lock one client: hand its terminal to its session's @lock-command@ and
-- draw nothing to it until it hands it back.
lockClient :: ServerState -> Client -> IO LockOutcome
lockClient st c
    | c.role /= Attached || isJust c.control = pure Passed
    | otherwise = do
        msess <- atomically $ do
            sid <- readTVar c.session
            Map.lookup sid <$> readTVar st.sessions
        case msess of
            Nothing -> pure Passed
            Just sess -> do
                cmd <- optionText "lock-command" <$> resolveForSessionIO st sess
                if T.null cmd then pure Passed
                    else if c.wireLevel < lockLevel then pure (TooOld c.wireLevel)
                    else do
                        stopped <- atomically $ readTVar c.ttyState >>= \case
                            TtyStopped -> pure False
                            _ -> True <$ writeTVar c.ttyState TtyStopped
                        if not stopped then pure Passed else do
                            send c (LockClient cmd)
                            applySessionSize st sess.id
                            pure Locked

-- | Lock every terminal client of a session.
lockSessionClients :: ServerState -> SessionId -> IO [(Client, LockOutcome)]
lockSessionClients st sid = do
    cs <- atomically (sessionClients st sid)
    forM cs $ \c -> (c,) <$> lockClient st c

-- | The error for each client a lock could not reach.
refusedLocks :: [(Client, LockOutcome)] -> [Text]
refusedLocks outcomes =
    [ clientName c <> " cannot be locked: its hat is too old (protocol "
        <> tshow lvl <> ")"
    | (c, TooOld lvl) <- outcomes ]

-- | The client's lock command exited (tmux's @MSG_UNLOCK@): its terminal
-- is the server's again, to redraw in full, and it counts as activity.
ttyHandedBack :: ServerState -> Client -> IO ()
ttyHandedBack st c = do
    wasStopped <- atomically $ readTVar c.ttyState >>= \case
        TtyStopped -> do
            writeTVar c.ttyState TtyRestarted
            writeTVar c.needsFull True
            markActive st c
            bumpDirty st
            pure True
        _ -> pure False
    when wasStopped $ do
        sid <- readTVarIO c.session
        applySessionSize st sid
        armLockTimer st sid

-- | Restart a session's lock timer (tmux's @session_update_activity@): a
-- session with clients locks once idle for @lock-after-time@.
armLockTimer :: ServerState -> SessionId -> IO ()
armLockTimer st sid = do
    now <- getMonotonicTimeNSec
    atomically $ do
        msess <- Map.lookup sid <$> readTVar st.sessions
        due <- case msess of
            Nothing -> pure Nothing
            Just sess -> do
                attached <- any (\c -> c.role == Attached) <$> sessionClients st sid
                secs <- optionNumber "lock-after-time" <$> resolveForSession st sess
                pure (lockDeadline attached secs now)
        modifyTVar' st.lockTimers (Map.alter (const due) sid)

-- | When a session last active at @now@ (monotonic ns) locks: after
-- @lock-after-time@ seconds, if it has clients and the option is not 0.
lockDeadline :: Bool -> Int -> Word64 -> Maybe Word64
lockDeadline attached secs now
    | attached && secs > 0 = Just (now + fromIntegral secs * 1_000_000_000)
    | otherwise = Nothing

-- | Lock each session whose timer runs out (tmux's @session_lock_timer@).
-- A timer fires once; activity re-arms it.
lockTimerLoop :: ServerState -> IO ()
lockTimerLoop st = forever $ do
    timers <- readTVarIO st.lockTimers
    now <- getMonotonicTimeNSec
    let (due, pending) = Map.partition (<= now) timers
    forM_ (Map.toList due) $ \(sid, deadline) -> do
        fire <- atomically $ do
            cur <- Map.lookup sid <$> readTVar st.lockTimers
            if cur /= Just deadline then pure False
                else True <$ modifyTVar' st.lockTimers (Map.delete sid)
        when fire (lockIdleSession st sid)
    when (Map.null due) $ do
        let changed = readTVar st.lockTimers >>= check . (/= timers)
        case Map.elems pending of
            [] -> atomically changed
            ds -> do
                let wait = (minimum ds - now) `div` 1000 + 1
                timer <- registerDelay (fromIntegral wait)
                atomically ((readTVar timer >>= check) `orElse` changed)

-- | Lock an idle session's clients; one a lock cannot reach is told why.
lockIdleSession :: ServerState -> SessionId -> IO ()
lockIdleSession st sid = do
    outcomes <- lockSessionClients st sid
    forM_ outcomes $ \o@(c, _) -> forM_ (refusedLocks [o]) (showToast st c)
