-- | @wait-for@: block a client on a named channel until another command
-- signals it (@-S@), lock/unlock counting (@-L@/@-U@), waking one waiter by
-- name (@-w@), listing waiters (@-l@), and blocking on hook events (@-E@,
-- with an optional payload filter). A blocked waiter leaves its command
-- batch so pane reconciliation keeps running while it sleeps.
module Hat.Server.Command.Wait
    ( cmdWaitFor
    ) where

import Control.Concurrent.STM
import Control.Exception (bracket_)
import Control.Monad (join)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Hat.Model
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import Hat.Server.HookTypes
import Hat.Server.Hooks (clientNameOf, validEventName)

cmdWaitFor :: CommandImpl
cmdWaitFor st mclient args = do
    let (opts, flags, pos) = parseArgs "Fw" args
        has f = f `elem` flags
    case pos of
        [name]
            | has "-E" -> waitEvent st mclient opts flags name
            | has "-l" -> listChannel st name
            | Just who <- lookup "-w" opts -> wakeChannel st name who
            | has "-S" -> signalChannel st name
            | has "-L" -> lockChannel st mclient name
            | has "-U" -> unlockChannel st name
            | otherwise -> waitChannel st mclient name
        _ -> pure [RErr "usage: wait-for [-ELSUlv] [-F format] [-w waiter] name"]

-- Block outside the command batch, so a sleeping waiter does not stall
-- reconciliation for every other client.
blockOn :: ServerState -> TMVar a -> IO a
blockOn st mv = bracket_
    (atomically (modifyTVar' st.commandDepth (subtract 1)))
    (atomically (modifyTVar' st.commandDepth (+ 1)))
    (atomically (takeTMVar mv))

channelsVar :: ServerState -> TVar (Map.Map Text Channel)
channelsVar st = st.hooks.channels

waitChannel :: ServerState -> Maybe Client -> Text -> IO [Reply]
waitChannel st mclient name = case mclient of
    Nothing -> pure [RErr "not able to wait"]
    Just _ -> join . atomically $ do
        chans <- readTVar (channelsVar st)
        case Map.lookup name chans of
            Just ch | ch.woken -> do
                writeTVar (channelsVar st) $ if ch.locked
                    then chans
                    else Map.delete name chans
                pure (pure [])
            _ -> do
                mv <- newEmptyTMVar
                let w = Waiter { clientName = clientNameOf mclient, wake = mv }
                    ch = Map.findWithDefault emptyChannel name chans
                writeTVar (channelsVar st)
                    (Map.insert name ch { waiters = ch.waiters <> [w] } chans)
                pure ([] <$ blockOn st mv)

signalChannel :: ServerState -> Text -> IO [Reply]
signalChannel st name = atomically $ do
    chans <- readTVar (channelsVar st)
    let ch = Map.findWithDefault emptyChannel name chans
    if null ch.waiters
        then writeTVar (channelsVar st)
            (Map.insert name ch { woken = True } chans)
        else do
            mapM_ (\w -> putTMVar w.wake ()) ch.waiters
            writeTVar (channelsVar st)
                (Map.insert name ch { waiters = [] } chans)
    pure []

listChannel :: ServerState -> Text -> IO [Reply]
listChannel st name = do
    chans <- readTVarIO (channelsVar st)
    pure $ case Map.lookup name chans of
        Nothing -> []
        Just ch -> [ ROutput w.clientName | w <- ch.waiters <> ch.lockers ]

wakeChannel :: ServerState -> Text -> Text -> IO [Reply]
wakeChannel st name who = atomically $ do
    chans <- readTVar (channelsVar st)
    case Map.lookup name chans of
        Nothing -> pure []
        Just ch -> do
            let pick ws = case List.partition ((== who) . (.clientName)) ws of
                    (hit : rest, keep) -> (Just hit, rest <> keep)
                    ([], keep) -> (Nothing, keep)
                (mw, waiters') = pick ch.waiters
            case mw of
                Just w -> do
                    putTMVar w.wake ()
                    writeTVar (channelsVar st)
                        (Map.insert name ch { waiters = waiters' } chans)
                Nothing -> do
                    let (ml, lockers') = pick ch.lockers
                    case ml of
                        Just w -> do
                            putTMVar w.wake ()
                            writeTVar (channelsVar st)
                                (Map.insert name ch { lockers = lockers' } chans)
                        Nothing -> pure ()
            pure []

lockChannel :: ServerState -> Maybe Client -> Text -> IO [Reply]
lockChannel st mclient name = case mclient of
    Nothing -> pure [RErr "not able to lock"]
    Just _ -> join . atomically $ do
        chans <- readTVar (channelsVar st)
        let ch = Map.findWithDefault emptyChannel name chans
        if ch.locked
            then do
                mv <- newEmptyTMVar
                let w = Waiter { clientName = clientNameOf mclient, wake = mv }
                writeTVar (channelsVar st)
                    (Map.insert name ch { lockers = ch.lockers <> [w] } chans)
                pure ([] <$ blockOn st mv)
            else do
                writeTVar (channelsVar st)
                    (Map.insert name ch { locked = True } chans)
                pure (pure [])

unlockChannel :: ServerState -> Text -> IO [Reply]
unlockChannel st name = atomically $ do
    chans <- readTVar (channelsVar st)
    case Map.lookup name chans of
        Just ch | ch.locked -> case ch.lockers of
            (next : rest) -> do
                putTMVar next.wake ()
                writeTVar (channelsVar st)
                    (Map.insert name ch { lockers = rest } chans)
                pure []
            [] -> do
                writeTVar (channelsVar st)
                    (Map.insert name ch { locked = False } chans)
                pure []
        _ -> pure [RErr ("channel " <> name <> " not locked")]

-- Events ----------------------------------------------------------------

waitEvent
    :: ServerState -> Maybe Client -> [(Text, Text)] -> [Text] -> Text
    -> IO [Reply]
waitEvent st mclient opts flags name = do
    ok <- validEventName st name
    if not ok
        then pure [RErr ("invalid event: " <> name)]
        else case () of
            _ | "-l" `elem` flags -> do
                    ws <- readTVarIO st.hooks.eventWaiters
                    pure [ ROutput w.clientName | w <- ws, w.name == name ]
              | Just who <- lookup "-w" opts -> join . atomically $ do
                    ws <- readTVar st.hooks.eventWaiters
                    let match w = w.name == name && w.clientName == who
                    case List.partition match ws of
                        (hit : extra, keep) -> do
                            writeTVar st.hooks.eventWaiters (extra <> keep)
                            putTMVar hit.wake []
                            pure (pure [])
                        ([], _) -> pure
                            (pure [RErr ("waiter " <> who <> " not found")])
              | Nothing <- mclient -> pure [RErr "not able to wait"]
              | otherwise -> do
                    mv <- newEmptyTMVarIO
                    let w = EventWaiter
                            { name = name
                            , clientName = clientNameOf mclient
                            , filter = lookup "-F" opts
                            , wake = mv }
                    atomically (modifyTVar' st.hooks.eventWaiters (<> [w]))
                    payload <- blockOn st mv
                    pure $ if "-v" `elem` flags
                        then map ROutput payload
                        else []
