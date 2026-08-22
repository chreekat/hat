-- | The session commands: creating, attaching, renaming, killing, and
-- listing sessions, windows, clients, and panes, plus @switch-client@ and
-- @kill-server@.
module Hat.Server.Command.Session
    ( cmdNewSession
    , cmdAttachSession
    , cmdKillSession
    , cmdHasSession
    , cmdStartServer
    , cmdRenameSession
    , cmdListClients
    , cmdListSessions
    , cmdListWindows
    , cmdListPanes
    , cmdSwitchClient
    ) where

import Control.Concurrent.STM
import Control.Monad (forM, forM_, unless, when)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import System.Environment (getEnvironment)

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.Mru (recordVisit)
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import Hat.Server.FormatEnv (paneFormatEnv, windowFormatEnv)
import Hat.Server.ClientIO (broadcast)
import Hat.Server.Hooks (PayloadItem (..), noTarget, notify, sessionTarget)
import Hat.Server.Locate (findTarget, paneIndexOf, targetSession, withTargetSession)
import Hat.Server.Pane
    (LifecycleNotify (..), createSession, killPaneLocsWith)
import Hat.Server.Resize (applySessionSize)
import Hat.Server.FormatEnv (expandFormat, sessionFormatEnv)
import Hat.Server.Target qualified as Target
import Hat.Transport.Wire (ServerToClient (Exited))

cmdNewSession :: CommandImpl
cmdNewSession st mclient args
    | Just t <- lookup "-t" (fst3 (parseArgs "sctnxyF" args)) =
        pure [RErr ("new-session -t " <> t
            <> ": session groups not supported")]
    | otherwise = do
    let (opts, flags, pos) = parseArgs "sctnxyF" args
        mname = lookup "-s" opts
        mrun = case pos of
            [] -> Nothing
            ws -> Just (T.unwords ws)
    dup <- case mname of
        Nothing -> pure False
        Just nm -> atomically $ do
            sessions <- readTVar st.sessions
            names <- mapM (\s -> readTVar s.name) (Map.elems sessions)
            pure (nm `elem` names)
    if dup
        then pure [RErr ("duplicate session: " <> fromMaybe "" mname)]
        else do
            (environ, dir, baseSz) <- case mclient of
                Just c -> do
                    csz <- readTVarIO c.size
                    pure (c.env, T.unpack c.cwd, csz)
                Nothing -> do
                    -- Config-loaded or otherwise clientless: inherit the
                    -- server process env so shells find PATH, SHELL, etc.
                    procEnv <- getEnvironment
                    pure ( [(T.pack k, T.pack v) | (k, v) <- procEnv]
                         , "/"
                         , Size { rows = 24, cols = 80 } )
            let dir' = maybe dir T.unpack (lookup "-c" opts)
                parseInt t = case TR.decimal t of
                    Right (n, rest) | T.null rest -> Just n
                    _ -> Nothing
                sz = baseSz
                    { cols = fromMaybe baseSz.cols (parseInt =<< lookup "-x" opts)
                    , rows = fromMaybe baseSz.rows (parseInt =<< lookup "-y" opts)
                    }
            sess <- createSession st mname mrun environ dir' sz
            atomically $ do
                writeTVar st.everAttached True
                forM_ (lookup "-n" opts) $ \wname -> do
                    ws <- readTVar sess.windows
                    forM_ (Map.elems ws) $ \w -> do
                        writeTVar w.name wname
                        writeTVar w.autoRename False
            unless ("-d" `elem` flags) $
                forM_ mclient $ \client -> switchClientTo st client sess
            if "-P" `elem` flags
                then do
                    mctx <- atomically $ do
                        mwin <- currentWindow sess
                        case mwin of
                            Nothing -> pure Nothing
                            Just win -> do
                                cur <- readTVar sess.currentIx
                                mpane <- activePane win
                                pure ((,,) cur win <$> mpane)
                    case mctx of
                        Nothing -> pure []
                        Just (wix, win, pane) -> do
                            let fmt = fromMaybe "#{session_name}:"
                                    (lookup "-F" opts)
                            pix <- paneIndexOf st win pane
                            env <- paneFormatEnv st sess wix win pix pane
                            out <- expandFormat st env fmt
                            pure [ROutput out]
                else pure []

switchClientTo :: ServerState -> Client -> Session -> IO ()
switchClientTo st client sess = do
    old <- readTVarIO client.session
    atomically $ do
        when (old /= sess.id) $ do
            modifyTVar' client.sessionHist (recordVisit old sess.id)
            writeTVar st.lastSession (Just old)
            writeTVar client.session sess.id
        writeTVar st.lastActiveSession (Just sess.id)
        markActive st client
        writeTVar client.needsFull True
        bumpDirty st
    applySessionSize st old
    applySessionSize st sess.id

-- @-c@ re-anchors the session's default working directory for new
-- windows, so it is useful (and valid) even without a client to attach.
cmdAttachSession :: CommandImpl
cmdAttachSession st mclient args = do
    let (opts, flags, _) = parseArgs "tc" args
    withTargetSession st mclient (lookup "-t" opts) $ \sess -> do
        -- -E: skip the update-environment import this attach would run.
        when ("-E" `elem` flags) $ forM_ mclient $ \client ->
            atomically (writeTVar client.envImport SkipEnvImport)
        forM_ (lookup "-c" opts) $ \d -> do
            env <- sessionFormatEnv st sess
            dir <- T.unpack <$> expandFormat st env d
            atomically $ do
                writeTVar sess.startCwd dir
                bumpDirty st
        case mclient of
            Just client -> switchClientTo st client sess >> pure []
            Nothing
                | isJust (lookup "-c" opts) -> pure []
                | otherwise -> pure [RErr "no client to attach"]

-- | @kill-session@: remove the session, then announce in tmux's order —
-- session-closed first, then window-unlinked per window. A window still
-- linked into another session is only unlinked; its panes survive.
cmdKillSession :: CommandImpl
cmdKillSession st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    withTargetSession st mclient (lookup "-t" opts) $ \sess -> do
        (sname, winInfo) <- atomically $ do
            sname <- readTVar sess.name
            allSess <- readTVar st.sessions
            ws <- Map.toAscList <$> readTVar sess.windows
            winInfo <- forM ws $ \(_, win) -> do
                wname <- readTVar win.name
                shared <- fmap Prelude.or . forM
                    [ other | (osid, other) <- Map.toList allSess
                    , osid /= sess.id ] $ \other -> do
                        ows <- readTVar other.windows
                        pure (any (\w -> w.id == win.id) (Map.elems ows))
                ps <- Map.elems <$> readTVar win.panes
                pure (win, wname, shared, ps)
            writeTVar st.sessions (Map.delete sess.id allSess)
            pure (sname, winInfo)
        notify st "session-closed" noTarget
            [ ("session", PSessionRef sess.id sname) ]
        forM_ winInfo $ \(win, wname, _, _) ->
            notify st "window-unlinked" noTarget
                [ ("session", PSessionRef sess.id sname)
                , ("window", PWindowRef win.id wname) ]
        killPaneLocsWith QuietLifecycle st
            [ (sess.id, win, p)
            | (win, _, shared, ps) <- winInfo, not shared, p <- ps ]
        broadcast st sess.id Exited
        pure []

-- | @has-session -t target@: resolve strictly and report cmd-find's
-- error; upstream targets.sh probes error paths through it.
cmdHasSession :: CommandImpl
cmdHasSession st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    res <- findTarget st mclient Target.FindSession (lookup "-t" opts)
    pure $ case res of
        Right _ -> []
        Left e -> [RErr e]

-- The server is necessarily running by the time this executes.
cmdStartServer :: CommandImpl
cmdStartServer _ _ _ = pure []

cmdRenameSession :: CommandImpl
cmdRenameSession st mclient args = do
    let (opts, _, pos) = parseArgs "t" args
    case pos of
        [nm] -> withTargetSession st mclient (lookup "-t" opts) $ \sess -> do
            dup <- atomically $ do
                sessions <- readTVar st.sessions
                names <- mapM (\s -> readTVar s.name)
                    (filter (\s -> s.id /= sess.id) (Map.elems sessions))
                pure (nm `elem` names)
            if dup
                then pure [RErr ("duplicate session: " <> nm)]
                else do
                    atomically $ do
                        writeTVar sess.name nm
                        bumpDirty st
                    notify st "session-renamed" (sessionTarget sess.id)
                        [ ("session", PSessionRef sess.id nm) ]
                    pure []
        _ -> pure [RErr "usage: rename-session [-t target] name"]

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | tmux @list-clients@: one line per attached client (control connections
-- are not clients), filtered by @-t@ and shaped by @-F@.
cmdListClients :: CommandImpl
cmdListClients st mclient args = do
    let (opts, _, _) = parseArgs "tF" args
    efilter <- case lookup "-t" opts of
        Nothing -> pure (Right Nothing)
        Just t -> do
            msess <- targetSession st mclient (Just t)
            pure $ case msess of
                Nothing -> Left ("no such session: " <> t)
                Just sess -> Right (Just sess.id)
    case efilter of
        Left err -> pure [RErr err]
        Right msid -> do
            (cs, sessions) <- atomically $
                (,) <$> readTVar st.clients <*> readTVar st.sessions
            fmap concat . forM (Map.elems cs) $ \c -> do
                sid <- readTVarIO c.session
                case Map.lookup sid sessions of
                    Just sess
                        | c.role == Attached
                        , maybe True (== sid) msid -> do
                            env <- sessionFormatEnv st sess
                            out <- expandFormat st env $ fromMaybe
                                "#{session_name}" (lookup "-F" opts)
                            pure [ROutput out]
                    _ -> pure []

cmdListSessions :: CommandImpl
cmdListSessions st _ args = do
    let (opts, _, _) = parseArgs "F" args
    sessions <- Map.elems <$> readTVarIO st.sessions
    lines' <- case lookup "-F" opts of
        Just fmt -> forM sessions $ \sess -> do
            env <- sessionFormatEnv st sess
            expandFormat st env fmt
        Nothing -> forM sessions $ \sess -> atomically $ do
            nm <- readTVar sess.name
            ws <- readTVar sess.windows
            pure (nm <> ": " <> tshow (Map.size ws) <> " windows")
    pure (map ROutput lines')

cmdListWindows :: CommandImpl
cmdListWindows st mclient args = do
    let (opts, flags, _) = parseArgs "Ft" args
        mfmt = lookup "-F" opts
        allSessions = "-a" `elem` flags
    esessions <- if allSessions
        then Right . Map.elems <$> readTVarIO st.sessions
        else do
            msess <- targetSession st mclient (lookup "-t" opts)
            pure $ case (msess, lookup "-t" opts) of
                (Just sess, _) -> Right [sess]
                -- An explicit target that resolves nowhere fails loud.
                (Nothing, Just t) -> Left ("no such session: " <> t)
                (Nothing, Nothing) -> Right []
    case esessions of
      Left e -> pure [RErr e]
      Right sessions ->
        fmap (map ROutput . concat) . forM sessions $ \sess -> do
            ws <- Map.toAscList <$> readTVarIO sess.windows
            cur <- readTVarIO sess.currentIx
            forM ws $ \(ix, win) -> case mfmt of
                Just fmt -> do
                    env <- windowFormatEnv st sess ix win
                    expandFormat st env fmt
                Nothing -> atomically $ do
                    nm <- readTVar win.name
                    ps <- readTVar win.panes
                    let mark = if ix == cur then "*" else ""
                    pure $ tshow ix <> ": " <> nm <> mark
                        <> " (" <> tshow (Map.size ps) <> " panes)"

cmdListPanes :: CommandImpl
cmdListPanes st mclient args = do
    let (opts, flags, _) = parseArgs "Ft" args
        mfmt = lookup "-F" opts
        allSessions = "-a" `elem` flags
    -- Which (session, window-index, window) triples to list panes from:
    -- -a covers every window of every session; otherwise the target
    -- session's current window.
    targets <- if allSessions
        then do
            sessions <- Map.elems <$> readTVarIO st.sessions
            fmap concat . forM sessions $ \sess -> do
                ws <- Map.toAscList <$> readTVarIO sess.windows
                pure [ (sess, ix, win) | (ix, win) <- ws ]
        else do
            msess <- targetSession st mclient (lookup "-t" opts)
            case msess of
                Nothing -> pure []
                Just sess -> do
                    cur <- readTVarIO sess.currentIx
                    mwin <- atomically (currentWindow sess)
                    pure [ (sess, cur, win) | win <- maybe [] pure mwin ]
    pbase <- (.paneBaseIndex) <$> readTVarIO st.options
    fmap (map ROutput . concat) . forM targets $ \(sess, wix, win) -> do
        ps <- Map.elems <$> readTVarIO win.panes
        forM (zip [pbase ..] ps) $ \(pix, pane) -> case mfmt of
            Just fmt -> do
                env <- paneFormatEnv st sess wix win pix pane
                expandFormat st env fmt
            Nothing -> do
                sz <- readTVarIO pane.size
                pure $ "%" <> tshow (rawPane pane.id) <> ": ["
                    <> tshow sz.cols <> "x" <> tshow sz.rows <> "]"

cmdSwitchClient :: CommandImpl
cmdSwitchClient st mclient args = do
    let (opts, flags, _) = parseArgs "t" args
    case mclient of
        Nothing -> pure [RErr "no client"]
        Just client
            | "-l" `elem` flags -> do
                hist <- readTVarIO client.sessionHist
                sessions <- readTVarIO st.sessions
                -- Skip any sessions killed since they were visited.
                case mapMaybe (`Map.lookup` sessions) hist of
                    []         -> pure [RErr "no last session"]
                    (sess : _) -> switchClientTo st client sess >> pure []
            | otherwise ->
                withTargetSession st mclient (lookup "-t" opts) $ \sess ->
                    switchClientTo st client sess >> pure []
