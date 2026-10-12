-- | The startup barrier: which phase the server is in, and whether a
-- command batch may run yet or must park until the tree is whole.
module Hat.Server.Startup
    ( finallyReady
    , phaseAfterConfig
    , StartupGate (..)
    , Lineage (..)
    , startupGate
    , awaitStartup
    , descendsFrom
    , IdleArming (..)
    , idleArming
    ) where

import Control.Concurrent.STM
import Control.Exception (IOException, catch, finally)
import Data.ByteString.Char8 qualified as B8
import Data.Text (Text)
import Network.Socket (getPeerCredential)
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessID)

import Hat.Model
import Hat.Transport.Wire (Autostart (..))

-- | Run the startup action (config load + restore), then land the phase at
-- 'Ready' — always, even if it throws. A phase stuck short of Ready parks
-- every attach forever on 'ensureSession'\'s retry, so the landing must be
-- structural (a @finally@), never a line a crash can skip.
finallyReady :: ServerState -> IO a -> IO a
finallyReady st act =
    act `finally` atomically (writeTVar st.startupPhase Ready)

-- | The phase after the config has drained: straight to 'Ready' unless a
-- persisted tree or reload handover still has to be rebuilt.
phaseAfterConfig :: Bool -> StartupPhase
phaseAfterConfig hasRestoreWork = if hasRestoreWork then Restoring else Ready

-- | What 'startupGate' decides for a command batch.
data StartupGate = Proceed | Hold
    deriving (Eq, Show)

-- | Whether a client is the server's own descendant: one its config, or a
-- job it ran, started.
data Lineage = ServerSpawned | Outsider
    deriving (Eq, Show)

-- | Whether a client's command batch may run in the given startup phase.
-- During 'LoadingConfig' only a client the config itself spawned (a nested
-- @hat run@ in an @if-shell@ condition) is served, as it must be or config
-- load deadlocks on its own child; any other — the client that spawned the
-- server, racing the config for the right to create the first session
-- (upstream if-shell-TERM.sh), or one that would find a reload's tree not
-- yet rebuilt — waits. 'Restoring' holds everyone: a command must see the
-- whole restored tree, not one mid-rebuild. A reload batch
-- (@restart-server@/@restart@) always proceeds — 'cmdReload' must REJECT
-- an in-flight reload, and holding it here would turn that reject into a
-- silent wait.
startupGate :: StartupPhase -> Autostart -> Lineage -> [[Text]] -> StartupGate
startupGate phase origin lineage cmds
    | any isReload cmds = Proceed
    | otherwise = case (phase, origin) of
        (Ready, _) -> Proceed
        (Restoring, _) -> Hold
        (LoadingConfig, Autostarted) -> Hold
        (LoadingConfig, Joined)
            | attaches || lineage == Outsider -> Hold
            | otherwise -> Proceed
  where
    isReload (name : _) = name `elem` ["restart-server", "restart"]
    isReload []         = False
    -- A batch that resolves or creates sessions needs the finished tree:
    -- an empty batch is the bare attach (reuse-or-create), and a targeted
    -- attach served early rejects "no such session" instead of waiting.
    attaches = null cmds || any isAttach cmds
    isAttach (name : _) =
        name `elem` ["attach", "attach-session", "new", "new-session"]
    isAttach [] = False

-- | Park a client's command batch until 'startupGate' lets it through.
awaitStartup :: ServerState -> Client -> [[Text]] -> IO ()
awaitStartup st client cmds = do
    phase <- readTVarIO st.startupPhase
    lineage <- if phase /= LoadingConfig then pure Outsider else do
        (mpid, _, _) <- getPeerCredential client.sock
            `catch` \(_ :: IOException) -> pure (Nothing, Nothing, Nothing)
        self <- getProcessID
        spawned <- maybe (pure False) (\p -> descendsFrom self (fromIntegral p)) mpid
        pure (if spawned then ServerSpawned else Outsider)
    atomically $ do
        now <- readTVar st.startupPhase
        case startupGate now client.autostart lineage cmds of
            Proceed -> pure ()
            Hold -> retry

-- | Whether process @pid@ descends from @ancestor@, by its parents in @/proc@.
descendsFrom :: ProcessID -> ProcessID -> IO Bool
descendsFrom ancestor = go (64 :: Int)
  where
    go 0 _ = pure False
    go n pid
        | pid <= 1 = pure False
        | otherwise = do
            mppid <- parentOf pid
            case mppid of
                Just ppid | ppid == ancestor -> pure True
                          | otherwise -> go (n - 1) ppid
                Nothing -> pure False
    -- the field after the state, past the command name's last ')'
    parentOf pid = do
        stat <- (Just <$> B8.readFile ("/proc/" <> show pid <> "/stat"))
            `catch` \(_ :: IOException) -> pure Nothing
        pure $ case B8.words . snd . B8.breakEnd (== ')') <$> stat of
            Just (_ : ppid : _) -> fromIntegral . fst <$> B8.readInt ppid
            _ -> Nothing

-- | Whether a new connection arms the idle-exit (see 'Hat.Server.waitIdle').
data IdleArming = ArmsIdleExit | LeavesIdleExit
    deriving (Eq, Show)

-- | A client the config spawned mid-load (a nested @hat run@ in an
-- @if-shell@) can come and go before the autostarting client is accepted, so
-- it must not arm the idle-exit. The hello, absent when the connection never
-- sent a valid one, tells the two apart.
idleArming :: StartupPhase -> Maybe Autostart -> IdleArming
idleArming LoadingConfig (Just Joined) = LeavesIdleExit
idleArming _ _ = ArmsIdleExit
