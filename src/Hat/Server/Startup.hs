-- | The startup barrier: which phase the server is in, and whether a
-- command batch may run yet or must park until the tree is whole.
module Hat.Server.Startup
    ( finallyReady
    , phaseAfterConfig
    , StartupGate (..)
    , startupGate
    , awaitStartup
    ) where

import Control.Concurrent.STM
import Control.Exception (finally)
import Data.Text (Text)

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

-- | Whether a client's command batch may run in the given startup phase.
-- During 'LoadingConfig' only the client that spawned the server is held —
-- it raced the config for the right to create the first session (upstream
-- if-shell-TERM.sh), while a client the config itself spawned (a nested
-- @hat run@ in an @if-shell@ condition) must be served or config load
-- deadlocks on its own child. 'Restoring' holds everyone: a command must
-- see the whole restored tree, not one mid-rebuild. A reload batch
-- (@restart-server@/@restart@) always proceeds — 'cmdReload' must REJECT
-- an in-flight reload, and holding it here would turn that reject into a
-- silent wait.
startupGate :: StartupPhase -> Autostart -> [[Text]] -> StartupGate
startupGate phase origin cmds
    | any isReload cmds = Proceed
    | otherwise = case (phase, origin) of
        (Ready, _) -> Proceed
        (Restoring, _) -> Hold
        (LoadingConfig, Autostarted) -> Hold
        (LoadingConfig, Joined) -> Proceed
  where
    isReload (name : _) = name `elem` ["restart-server", "restart"]
    isReload []         = False

-- | Park a command batch until 'startupGate' lets it through.
awaitStartup :: ServerState -> Autostart -> [[Text]] -> IO ()
awaitStartup st origin cmds = atomically $ do
    phase <- readTVar st.startupPhase
    case startupGate phase origin cmds of
        Proceed -> pure ()
        Hold -> retry
