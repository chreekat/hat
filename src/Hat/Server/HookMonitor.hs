-- | @set-hook -B@ format monitors: each one periodically samples a format
-- in its target context and, when the value changes, fires its name as an
-- event and runs the hook bound at its own scope (exact scope only — a
-- monitor never walks the chain). Commands bound to a monitor are
-- format-expanded before parsing, so @#{hook_value}@ works without @-F@.
module Hat.Server.HookMonitor
    ( parseMonitorSpec
    , monitorTargetText
    , addMonitor
    , removeMonitor
    , monitorsAt
    , monitorLoop
    , sampleMonitors
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, forever)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Time.Clock.POSIX (getPOSIXTime)

import Hat.Log
import Hat.Model
import Hat.Model.Options (Options (..))
import Hat.Server.Format (FormatEnv)
import Hat.Server.FormatEnv (paneEnvById, windowFormatEnv)
import Hat.Server.HookTypes
import Hat.Server.Hooks
import Hat.Server.View (expandFormat, sessionFormatEnv)

-- | Parse a @-B name:target:format@ spec. The target selects what is
-- sampled: empty (or anything else) the session, @%N@/@%*@ panes,
-- @\@N@/@\@*@ windows.
parseMonitorSpec :: Text -> Either Text (Text, MonitorTarget, Text)
parseMonitorSpec spec = case T.breakOn ":" spec of
    (_, "") -> Left ("invalid subscription: " <> spec)
    (name, rest0) -> case T.breakOn ":" (T.drop 1 rest0) of
        (_, "") -> Left ("invalid subscription: " <> spec)
        (what, rest1) -> Right (name, kind what, T.drop 1 rest1)
  where
    kind what
        | what == "%*" = MonAllPanes
        | what == "@*" = MonAllWindows
        | Just n <- idNum "%" what = MonPane n
        | Just n <- idNum "@" what = MonWindow n
        | otherwise = MonSession
    idNum prefix t = do
        digits <- T.stripPrefix prefix t
        case TR.decimal digits of
            Right (n, "") | n >= (0 :: Int) -> Just n
            _ -> Nothing

-- | The target column of @show-hooks -B@ output.
monitorTargetText :: MonitorTarget -> Text
monitorTargetText = \case
    MonSession -> ""
    MonPane n -> "%" <> tshow n
    MonAllPanes -> "%*"
    MonWindow n -> "@" <> tshow n
    MonAllWindows -> "@*"

addMonitor
    :: ServerState -> HookScope -> Text -> MonitorTarget -> Text
    -> Maybe SessionId -> IO ()
addMonitor st scope name target fmt msid = do
    lastVar <- newTVarIO Map.empty
    countVar <- newTVarIO 0
    timeVar <- newTVarIO Nothing
    let mon = Monitor
            { format = fmt
            , target = target
            , session = msid
            , lastValues = lastVar
            , fireCount = countVar
            , fireTime = timeVar
            }
    atomically $ modifyTVar' st.hooks.monitors (Map.insert (scope, name) mon)

removeMonitor :: ServerState -> HookScope -> Text -> IO ()
removeMonitor st scope name =
    atomically $ modifyTVar' st.hooks.monitors (Map.delete (scope, name))

-- | A scope's monitors, for @show-hooks -B@.
monitorsAt :: ServerState -> HookScope -> IO [(Text, Monitor)]
monitorsAt st scope = do
    monitors <- readTVarIO st.hooks.monitors
    pure [ (n, m) | ((sc, n), m) <- Map.toAscList monitors, sc == scope ]

-- | The sampling daemon; see 'sampleMonitors'.
monitorLoop :: ServerState -> IO ()
monitorLoop st = forever $ do
    threadDelay 400_000
    sampleMonitors st `catch` \(e :: SomeException) ->
        logEvent st.logger DaemonFault
            { daemon = "hook-monitor", err = tshow e }

-- | Sample every monitor once, firing those whose value changed since the
-- previous sample (the first sample only records).
sampleMonitors :: ServerState -> IO ()
sampleMonitors st = do
    monitors <- readTVarIO st.hooks.monitors
    forM_ (Map.toList monitors) $ \((scope, name), mon) ->
        sampleOne st scope name mon

-- The context a change was observed in, for the fired event's payload.
data MonCtx = MonCtx
    { session :: Maybe (SessionId, Text)
    , window :: Maybe (WindowId, Text, Int)
    , pane :: Maybe PaneId
    }

sampleOne :: ServerState -> HookScope -> Text -> Monitor -> IO ()
sampleOne st scope name mon = do
    msess <- monitorSession st mon.session
    forM_ msess $ \sess -> case mon.target of
        MonSession -> do
            env <- sessionFormatEnv st sess
            sname <- readTVarIO sess.name
            let ctx = MonCtx (Just (sess.id, sname)) Nothing Nothing
            checkValue st scope name mon KeySession ctx env
        MonPane n -> do
            mpane <- atomically (findPaneById st n)
            forM_ mpane $ \pane -> samplePane st scope name mon sess pane
        MonAllPanes -> do
            ws <- readTVarIO sess.windows
            forM_ (Map.elems ws) $ \win -> do
                ps <- readTVarIO win.panes
                forM_ (Map.elems ps) $ \pane ->
                    samplePane st scope name mon sess pane
        MonWindow n -> do
            mwin <- atomically (findWindowById st (WindowId n))
            forM_ mwin $ \win -> sampleWindow st scope name mon sess win
        MonAllWindows -> do
            ws <- readTVarIO sess.windows
            forM_ (Map.elems ws) $ \win ->
                sampleWindow st scope name mon sess win

samplePane
    :: ServerState -> HookScope -> Text -> Monitor -> Session -> Pane -> IO ()
samplePane st scope name mon sess pane = do
    menv <- paneEnvById st pane.id
    forM_ menv $ \env0 -> do
        mloc <- atomically (locateCtx st sess pane)
        -- Pane-scoped user options resolve as #{@foo} in a pane context.
        userOpts <- case mloc of
            Just (_, win, _) ->
                (.user) <$> atomically (resolveForPane st sess win pane)
            Nothing -> pure Map.empty
        let env = Map.union userOpts env0
            ctx = case mloc of
                Just (sname, win, (wix, wname)) -> MonCtx
                    (Just (sess.id, sname))
                    (Just (win.id, wname, wix))
                    (Just pane.id)
                Nothing -> MonCtx Nothing Nothing (Just pane.id)
        checkValue st scope name mon (KeyPane (rawPane pane.id)) ctx env

-- The session/window naming details a pane payload carries.
locateCtx
    :: ServerState -> Session -> Pane
    -> STM (Maybe (Text, Window, (Int, Text)))
locateCtx _ sess pane = do
    sname <- readTVar sess.name
    ws <- readTVar sess.windows
    hits <- mapM
        (\(ix, win) -> do
            ps <- readTVar win.panes
            if Map.member pane.id ps
                then do
                    wname <- readTVar win.name
                    pure [(sname, win, (ix, wname))]
                else pure [])
        (Map.toList ws)
    pure (listToMaybe (concat hits))

sampleWindow
    :: ServerState -> HookScope -> Text -> Monitor -> Session -> Window -> IO ()
sampleWindow st scope name mon sess win = do
    ws <- readTVarIO sess.windows
    forM_ [ ix | (ix, w) <- Map.toList ws, w.id == win.id ] $ \wix -> do
        env <- windowFormatEnv st sess wix win
        (sname, wname) <- atomically
            ((,) <$> readTVar sess.name <*> readTVar win.name)
        let ctx = MonCtx (Just (sess.id, sname))
                (Just (win.id, wname, wix)) Nothing
        checkValue st scope name mon (KeyWindow (rawWindow win.id)) ctx env

-- Compare a sampled value against the stored one; the first sample only
-- records, a change fires.
checkValue
    :: ServerState -> HookScope -> Text -> Monitor -> MonitorKey -> MonCtx
    -> FormatEnv -> IO ()
checkValue st scope name mon key ctx env = do
    value <- expandFormat st env mon.format
    mprev <- atomically $ do
        lasts <- readTVar mon.lastValues
        writeTVar mon.lastValues (Map.insert key value lasts)
        pure (Map.lookup key lasts)
    case mprev of
        Just prev | prev /= value ->
            fireChange st scope name mon ctx env value prev
        _ -> pure ()

fireChange
    :: ServerState -> HookScope -> Text -> Monitor -> MonCtx -> FormatEnv
    -> Text -> Text -> IO ()
fireChange st scope name mon ctx env value prev = do
    now <- getPOSIXTime
    atomically $ do
        modifyTVar' mon.fireCount (+ 1)
        writeTVar mon.fireTime (Just now)
    let payload =
            [ ("value", PText value), ("last", PText prev) ]
            <> [ ("session", PSessionRef sid sname)
               | Just (sid, sname) <- [ctx.session] ]
            <> concat
               [ [ ("window", PWindowRef wid wname)
                 , ("window_index", PInt wix) ]
               | Just (wid, wname, wix) <- [ctx.window] ]
            <> [ ("pane", PPaneRef pid) | Just pid <- [ctx.pane] ]
        tgt = NotifyTarget
            { session = fst <$> ctx.session
            , window = (\(wid, _, _) -> wid) <$> ctx.window
            , pane = ctx.pane
            }
    fireMonitorHook st scope name tgt payload env

-- The session a monitor samples in: its pinned one if it still exists, a
-- global monitor follows the alphabetically-first session.
monitorSession :: ServerState -> Maybe SessionId -> IO (Maybe Session)
monitorSession st = \case
    Just sid -> Map.lookup sid <$> readTVarIO st.sessions
    Nothing -> do
        sessions <- readTVarIO st.sessions
        named <- atomically $
            mapM (\s -> (,) <$> readTVar s.name <*> pure s)
                (Map.elems sessions)
        pure (snd <$> listToMaybe (List.sortOn fst named))
