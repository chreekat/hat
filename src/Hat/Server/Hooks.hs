-- | The hooks engine: named command lists bound with @set-hook@, fired by
-- explicit 'notify' calls at server events (tmux's model — no event bus).
-- A fired event wakes @wait-for -E@ waiters and runs the hook bound
-- nearest its target (session, then global, then pane, then window); the
-- hook's commands run under an ambient context ('HookAmbient') that
-- carries the event's payload formats and target.
module Hat.Server.Hooks
    ( NotifyTarget (..)
    , noTarget
    , sessionTarget
    , PayloadItem (..)
    , notify
    , notifyPane
    , notifyWindow
    , fireMonitorHook
    , runHookNow
    , fireUserEvent
    , eventPayloadLines
    , setHook
    , unsetHook
    , registerEvent
    , isRegisteredEvent
    , hookEntriesAt
    , validSetHookName
    , validEventName
    , withAmbient
    , ambientFor
    , inHook
    , clientNameOf
    , installHookEngine
    ) where

import Control.Concurrent (myThreadId)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forM_, when)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)

import Hat.Model
import Hat.Model.Options (hookNames)
import Hat.Server.Format (FormatEnv, evaluate)
import Hat.Server.HookTypes
import Hat.Server.Locate (locatePane)

-- | The object an event happened to, as the scope-chain lookup and the
-- hook commands' ambient context see it.
data NotifyTarget = NotifyTarget
    { session :: Maybe SessionId
    , window :: Maybe WindowId
    , pane :: Maybe PaneId
    }
    deriving (Eq, Show)

noTarget :: NotifyTarget
noTarget = NotifyTarget Nothing Nothing Nothing

sessionTarget :: SessionId -> NotifyTarget
sessionTarget sid = NotifyTarget (Just sid) Nothing Nothing

-- | One event payload entry. Refs print in tmux's id syntax; session and
-- window refs also expose a @_name@ variant to hook formats.
data PayloadItem
    = PText Text
    | PInt Int
    | PSessionRef SessionId Text
    | PWindowRef WindowId Text
    | PPaneRef PaneId
    | PClientRef Text
    deriving (Eq, Show)

printItem :: PayloadItem -> Text
printItem = \case
    PText t -> t
    PInt n -> tshow n
    PSessionRef sid _ -> "$" <> tshow (rawSession sid)
    PWindowRef wid _ -> "@" <> tshow (rawWindow wid)
    PPaneRef pid -> "%" <> tshow (rawPane pid)
    PClientRef n -> n

-- | The bare payload map events expose to @wait-for -E@ filters and @-v@
-- output: each item under its own key, plus @event@.
barePayload :: Text -> [(Text, PayloadItem)] -> Map Text Text
barePayload name payload = Map.fromList $
    ("event", name) : [ (k, printItem v) | (k, v) <- payload ]

-- | The same payload as hook commands see it: each key under @hook_@, a
-- @_name@ variant for session/window refs, and @hook@ = the event name.
hookFormats :: Text -> [(Text, PayloadItem)] -> Map Text Text
hookFormats name payload = Map.fromList $
    ("hook", name) : concatMap fmt payload
  where
    fmt (k, v) = ("hook_" <> k, printItem v) : case v of
        PSessionRef _ n -> [("hook_" <> k <> "_name", n)]
        PWindowRef _ n -> [("hook_" <> k <> "_name", n)]
        _ -> []

-- | The name a client waits and is listed under (tmux's tty-less client
-- naming); a clientless command has none.
clientNameOf :: Maybe Client -> Text
clientNameOf = maybe "" (\c -> "client-" <> tshow (rawClient c.id))

-- Ambient context -------------------------------------------------------

-- | The hook context the current thread runs under, if any. Its presence
-- means "inside a hook": format expansion overlays its payload and
-- clientless target resolution follows its target.
ambientFor :: ServerState -> IO (Maybe HookAmbient)
ambientFor st = do
    tid <- myThreadId
    Map.lookup tid <$> readTVarIO st.hooks.ambient

inHook :: ServerState -> IO Bool
inHook st = maybe False (const True) <$> ambientFor st

-- | Run an action under a hook context, restoring the previous one after
-- (a hook fired from inside a hook command nests).
withAmbient :: ServerState -> HookAmbient -> IO a -> IO a
withAmbient st amb act = do
    tid <- myThreadId
    old <- atomically $ do
        m <- readTVar st.hooks.ambient
        writeTVar st.hooks.ambient (Map.insert tid amb m)
        pure (Map.lookup tid m)
    act `finally` atomically
        (modifyTVar' st.hooks.ambient (Map.alter (const old) tid))

-- Binding and validation ------------------------------------------------

-- | Whether @set-hook@ accepts this hook name: a user @\@hook@, a built-in
-- hook, or @after-@ a known command.
validSetHookName :: ServerState -> Text -> IO Bool
validSetHookName st name
    | "@" `T.isPrefixOf` name = pure True
    | name `elem` hookNames = pure True
    | Just cmd <- T.stripPrefix "after-" name =
        Set.member cmd <$> readTVarIO st.hooks.knownCommands
    | otherwise = pure False

-- | Whether @wait-for -E@ accepts this event name (same set as
-- 'validSetHookName'; user events need no prior registration).
validEventName :: ServerState -> Text -> IO Bool
validEventName = validSetHookName

-- | Bind (or, with append, extend) a hook at a scope. A user @\@hook@ set
-- through @set-hook@ also becomes a fireable event.
setHook :: ServerState -> HookScope -> Text -> Bool -> Text -> IO ()
setHook st scope name append cmd = do
    when ("@" `T.isPrefixOf` name) (registerEvent st name)
    atomically $ modifyTVar' st.hooks.table $
        Map.insertWith merge scope (Map.singleton name fresh)
  where
    fresh = HookEntry { commands = [cmd], fireCount = 0, fireTime = Nothing }
    merge _ old = Map.alter place name old
    place = \case
        Just e | append -> Just e { commands = e.commands <> [cmd] }
        _ -> Just fresh

unsetHook :: ServerState -> HookScope -> Text -> IO ()
unsetHook st scope name = atomically $ modifyTVar' st.hooks.table $
    Map.adjust (Map.delete name) scope

registerEvent :: ServerState -> Text -> IO ()
registerEvent st name =
    atomically $ modifyTVar' st.hooks.events (Set.insert name)

isRegisteredEvent :: ServerState -> Text -> IO Bool
isRegisteredEvent st name = Set.member name <$> readTVarIO st.hooks.events

-- | A scope's own hook bindings, for @show-hooks@.
hookEntriesAt :: ServerState -> HookScope -> IO (Map Text HookEntry)
hookEntriesAt st scope =
    Map.findWithDefault Map.empty scope <$> readTVarIO st.hooks.table

-- Firing ----------------------------------------------------------------

-- | Fire a built-in event: wake @wait-for -E@ waiters, then (unless the
-- current thread is already inside a hook) run the nearest bound hook.
notify :: ServerState -> Text -> NotifyTarget -> [(Text, PayloadItem)] -> IO ()
notify st name tgt payload = fire st name tgt payload True

-- | Fire a pane event with tmux's pane payload (the pane and its window),
-- targeted at where the pane lives.
notifyPane :: ServerState -> Text -> Pane -> [(Text, PayloadItem)] -> IO ()
notifyPane st name pane extra = do
    mloc <- atomically (locatePane st pane.id)
    case mloc of
        Nothing -> notify st name
            noTarget { pane = Just pane.id }
            (("pane", PPaneRef pane.id) : extra)
        Just (sid, win) -> do
            wname <- readTVarIO win.name
            notify st name
                (NotifyTarget (Just sid) (Just win.id) (Just pane.id))
                ([ ("pane", PPaneRef pane.id)
                 , ("window", PWindowRef win.id wname) ] <> extra)

-- | Fire a window event with tmux's window payload, targeted at a session
-- holding the window.
notifyWindow
    :: ServerState -> Text -> Maybe SessionId -> Window
    -> [(Text, PayloadItem)] -> IO ()
notifyWindow st name msid win extra = do
    wname <- readTVarIO win.name
    notify st name
        (NotifyTarget msid (Just win.id) Nothing)
        (("window", PWindowRef win.id wname) : extra)

-- | Fire a user event (@set-hook -E@): waiters always wake; hooks run only
-- for a name some @set-hook@ has registered.
fireUserEvent
    :: ServerState -> Text -> NotifyTarget -> [(Text, PayloadItem)] -> IO ()
fireUserEvent st name tgt payload = do
    registered <- isRegisteredEvent st name
    fire st name tgt payload registered

fire
    :: ServerState -> Text -> NotifyTarget -> [(Text, PayloadItem)] -> Bool
    -> IO ()
fire st name tgt payload runHooks = do
    let bare = barePayload name payload
    wakeEventWaiters st name bare
    suppressed <- inHook st
    when (runHooks && not suppressed) $
        fireHook st name tgt (hookFormats name payload)

-- | @set-hook -R@: run the hook bound to @name@ right now, with only the
-- caller's target as context (no event payload).
runHookNow :: ServerState -> Text -> NotifyTarget -> IO ()
runHookNow st name tgt =
    fireHook st name tgt (Map.singleton "hook" name)

-- Look up the chain, bump the found entry's fire bookkeeping, and run its
-- commands under the event's ambient context.
fireHook :: ServerState -> Text -> NotifyTarget -> Map Text Text -> IO ()
fireHook st name tgt formats = do
    now <- getPOSIXTime
    mcmds <- atomically $ do
        tbl <- readTVar st.hooks.table
        let chain =
                [ HookSession s | Just s <- [tgt.session] ]
                <> [HookGlobal]
                <> [ HookPane p | Just p <- [tgt.pane] ]
                <> [ HookWindow w | Just w <- [tgt.window] ]
            hit = List.find
                (\sc -> Map.member name (Map.findWithDefault Map.empty sc tbl))
                chain
        case hit of
            Nothing -> pure Nothing
            Just sc -> takeEntry st sc name now
    forM_ mcmds $ \cmds -> do
        run <- readTVarIO st.hooks.runner
        withAmbient st (ambientOf tgt formats) (mapM_ run cmds)

-- Bump one scope's entry and return its commands.
takeEntry :: ServerState -> HookScope -> Text -> POSIXTime -> STM (Maybe [Text])
takeEntry st sc name now = do
    tbl <- readTVar st.hooks.table
    case Map.lookup name (Map.findWithDefault Map.empty sc tbl) of
        Nothing -> pure Nothing
        Just e -> do
            let bumped = HookEntry
                    { commands = e.commands
                    , fireCount = e.fireCount + 1
                    , fireTime = Just now }
            writeTVar st.hooks.table
                (Map.adjust (Map.insert name bumped) sc tbl)
            pure (Just e.commands)

ambientOf :: NotifyTarget -> Map Text Text -> HookAmbient
ambientOf tgt formats = HookAmbient
    { formats = formats
    , targetSession = tgt.session
    , targetWindow = tgt.window
    , targetPane = tgt.pane
    }

-- | A monitor's change event: wake waiters, then run only the hook bound
-- at the monitor's own scope, pre-expanding each command in the change's
-- context (payload formats included) before it is parsed.
fireMonitorHook
    :: ServerState -> HookScope -> Text -> NotifyTarget
    -> [(Text, PayloadItem)] -> FormatEnv -> IO ()
fireMonitorHook st scope name tgt payload env = do
    wakeEventWaiters st name (barePayload name payload)
    now <- getPOSIXTime
    mcmds <- atomically (takeEntry st scope name now)
    forM_ mcmds $ \cmds -> do
        run <- readTVarIO st.hooks.runner
        expand <- readTVarIO st.hooks.expander
        withAmbient st (ambientOf tgt (hookFormats name payload)) $
            mapM_ (\c -> expand env c >>= run) cmds

-- Event waiters ---------------------------------------------------------

-- | The lines a @wait-for -E -v@ waiter prints: the payload sorted by key,
-- private (underscore) entries omitted.
eventPayloadLines :: Map Text Text -> [Text]
eventPayloadLines bare =
    [ k <> "=" <> v
    | (k, v) <- Map.toAscList bare, not ("_" `T.isPrefixOf` k) ]

-- Wake every @wait-for -E@ waiter whose name and filter match the payload.
wakeEventWaiters :: ServerState -> Text -> Map Text Text -> IO ()
wakeEventWaiters st name bare = atomically $ do
    ws <- readTVar st.hooks.eventWaiters
    let (woken, keep) = List.partition matches ws
    writeTVar st.hooks.eventWaiters keep
    forM_ woken $ \w -> putTMVar w.wake (eventPayloadLines bare)
  where
    matches w = w.name == name && maybe True passes w.filter
    passes f =
        let v = T.replace "%%" "%" (evaluate bare (const "") f)
        in not (T.null v) && v /= "0"

-- | Wire the engine to the command runner and the command-name set; called
-- once at startup (and by tests) before any hook can fire.
installHookEngine
    :: ServerState -> (Text -> IO ()) -> (FormatEnv -> Text -> IO Text)
    -> [Text] -> IO ()
installHookEngine st run expand cmdNames = atomically $ do
    writeTVar st.hooks.runner run
    writeTVar st.hooks.expander expand
    writeTVar st.hooks.knownCommands (Set.fromList cmdNames)
