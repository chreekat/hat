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
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import System.Environment (getEnvironment)

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.Mru (recordVisit)
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import Hat.Server.FormatEnv (paneFormatEnv, windowFormatEnv)
import Hat.Server.Locate (findTarget, targetSession, withTargetSession)
import Hat.Server.Pane (createSession, killPaneLocs)
import Hat.Server.Resize (applySessionSize)
import Hat.Server.View (expandFormat, sessionFormatEnv)
import qualified Hat.Server.Target as Target

cmdNewSession :: CommandImpl
cmdNewSession st mclient args = do
    let (opts, flags, pos) = parseArgs "sctnxy" args
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
            pure []

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

cmdKillSession :: CommandImpl
cmdKillSession st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    withTargetSession st mclient (lookup "-t" opts) $ \sess -> do
        locs <- atomically $ do
            ws <- Map.elems <$> readTVar sess.windows
            fmap concat . forM ws $ \win -> do
                ps <- Map.elems <$> readTVar win.panes
                pure [(sess.id, win, p) | p <- ps]
        killPaneLocs st locs
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
                    pure []
        _ -> pure [RErr "usage: rename-session [-t target] name"]

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
    sessions <- if allSessions
        then Map.elems <$> readTVarIO st.sessions
        else do
            msess <- targetSession st mclient (lookup "-t" opts)
            pure (maybe [] pure msess)
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
