-- | @lock-server@, @lock-session@ and @lock-client@ (tmux's
-- @cmd-lock-server.c@). See "Hat.Server.Lock".
module Hat.Server.Command.Lock
    ( cmdLockServer
    , cmdLockSession
    , cmdLockClient
    ) where

import Control.Concurrent.STM (readTVarIO)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Hat.Model
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs, unknownFlag)
import Hat.Server.Lock (lockClient, lockSessionClients, refusedLocks)
import Hat.Server.Locate (formatClient, withTargetSession)

-- | @lock-server@: lock every client.
cmdLockServer :: CommandImpl
cmdLockServer st _ args = checked "lock-server" "" args $ \_ -> do
    sids <- Map.keys <$> readTVarIO st.sessions
    outcomes <- concat <$> mapM (lockSessionClients st) sids
    pure (map RErr (refusedLocks outcomes))

-- | @lock-session [-t target-session]@: lock the session's clients.
cmdLockSession :: CommandImpl
cmdLockSession st mclient args = checked "lock-session" "t" args $ \opts ->
    withTargetSession st mclient (lookup "-t" opts) $ \sess ->
        map RErr . refusedLocks <$> lockSessionClients st sess.id

-- | @lock-client [-t target-client]@: lock one client.
cmdLockClient :: CommandImpl
cmdLockClient st mclient args = checked "lock-client" "t" args $ \opts -> do
    target <- formatClient st mclient Nothing (lookup "-t" opts)
    case target of
        Left e -> pure [RErr e]
        Right c -> do
            outcome <- lockClient st c
            pure (map RErr (refusedLocks [(c, outcome)]))

-- | Run a command that takes no arguments and no flags but the @valued@
-- ones.
checked :: Text -> [Char] -> [Text] -> ([(Text, Text)] -> IO [Reply]) -> IO [Reply]
checked name valued args body
    | Just e <- unknownFlag name "" flags = pure [RErr e]
    | not (null pos) =
        pure [RErr ("command " <> name <> ": too many arguments (need at most 0)")]
    | otherwise = body opts
  where
    (opts, flags, pos) = parseArgs valued args
