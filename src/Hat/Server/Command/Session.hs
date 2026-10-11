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
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text qualified as T
import System.Environment (getEnvironment)

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.Mru (recordVisit)
import Hat.Server.Command.Types
    (CommandImpl, Reply (..), parseArgs, strtonum, unknownFlag)
import Hat.Server.FormatEnv (paneFormatEnv, windowFormatEnv)
import Hat.Server.ClientIO (broadcast, send)
import Hat.Server.Format (truthy)
import Hat.Server.Hooks
    ( PayloadItem (..), noTarget, notify, notifyGroupChange, sessionTarget )
import Hat.Server.Locate (findTarget, paneIndexOf, targetSession, withTargetSession)
import Hat.Server.Pane
    ( LifecycleNotify (..), createGroupedSession, createSession, killPaneLocsWith, rehomePanes
    , windowHolders )
import Hat.Server.Resize (applySessionSize, defaultSizeOf)
import Hat.Server.FormatEnv (expandFormat, sessionFormatEnv)
import Hat.Server.Target qualified as Target
import Hat.Transport.Wire (ServerToClient (DetachOk, Exited))

-- | @new-session@: create a session, or with @-t@ a member of the target's
-- session group (sharing its windows), or with @-A@ attach to the named
-- session when it already exists.
cmdNewSession :: CommandImpl
cmdNewSession st mclient args
    | Just e <- unknownFlag "new-session" "AdDPX" flags = pure [RErr e]
    | isJust (lookup "-t" opts), not (null pos) || isJust (lookup "-n" opts) =
        pure [RErr "command or window name given with target"]
    | Just f <- firstOf ["-e", "-f"] = pure [RErr ("new-session " <> f
        <> ": not supported")]
    | otherwise = do
        existing <- if "-A" `elem` flags
            then case lookup "-s" opts of
                Just nm -> sessionNamed nm
                Nothing -> targetSession st mclient (lookup "-t" opts)
            else pure Nothing
        case existing of
            Just sess -> attachExisting sess
            Nothing -> do
                dup <- maybe (pure Nothing) sessionNamed (lookup "-s" opts)
                case (dup, lookup "-s" opts) of
                    (Just _, Just nm) -> pure [RErr ("duplicate session: " <> nm)]
                    _ -> case lookup "-t" opts of
                        Just t -> joinGroup t
                        Nothing -> createNew Nothing
  where
    (opts, flags, pos) = parseArgs "sctnxyFef" args
    firstOf fs = listToMaybe [f | f <- fs, isJust (lookup f opts)]
    sessionNamed nm = atomically $ do
        ss <- Map.elems <$> readTVar st.sessions
        named <- mapM (\s -> (,) s <$> readTVar s.name) ss
        pure (listToMaybe [s | (s, n) <- named, n == nm])
    -- tmux's attach path: a client without a terminal cannot attach.
    attachExisting sess = case mclient of
        Just client | client.role == Attached -> do
            if "-X" `elem` flags
                then pure [RErr "new-session -X: not supported"]
                else do
                    when ("-D" `elem` flags) $ detachOthers client sess
                    forM_ (lookup "-c" opts) $ \d -> atomically $ do
                        writeTVar sess.startCwd (T.unpack d)
                        bumpDirty st
                    switchClientTo st client sess
                    pure []
        _ -> pure [RErr "open terminal failed: not a terminal"]
    detachOthers client sess = do
        cs <- Map.elems <$> readTVarIO st.clients
        forM_ cs $ \c -> do
            sid <- readTVarIO c.session
            when (c.id /= client.id && c.role == Attached && sid == sess.id) $
                send c DetachOk
    -- A target that is a session joins (or founds) its group; one naming a
    -- group joins it; any other name founds an empty group of that name.
    joinGroup t = do
        mtarget <- either (const Nothing) Just
            <$> findTarget st mclient Target.FindSession (Just t)
        mleader <- case mtarget of
            Just (sess, _, _, _) -> pure (Just sess)
            Nothing -> atomically $ do
                ss <- Map.elems <$> readTVar st.sessions
                grouped <- mapM (\s -> (,) s <$> readTVar s.group) ss
                pure (listToMaybe [s | (s, Just g) <- grouped, g == t])
        case mleader of
            Nothing -> createNew (Just t)
            Just leader -> do
                (environ, dir, _) <- clientContext
                sess <- createGroupedSession st leader (lookup "-s" opts)
                    environ (maybe dir T.unpack (lookup "-c" opts))
                finish sess
    clientContext = case mclient of
        Just c -> do
            csz <- readTVarIO c.size
            pure (c.env, T.unpack c.cwd, Just csz)
        Nothing -> do
            -- Config-loaded or otherwise clientless: inherit the server
            -- process env so shells find PATH, SHELL, etc.
            procEnv <- getEnvironment
            pure ([(T.pack k, T.pack v) | (k, v) <- procEnv], "/", Nothing)
    createNew mgroup = do
        (environ, dir, mclientSz) <- clientContext
        gopts <- atomically (resolveGlobal st)
        let dir' = maybe dir T.unpack (lookup "-c" opts)
            detached = "-d" `elem` flags
            mrun = case pos of
                [] -> Nothing
                ws -> Just (T.unwords ws)
            -- @-x -@ / @-y -@ take the creating client's own dimension.
            dimension flag name fromClient fallback =
                case lookup flag opts of
                    Nothing -> Right Nothing
                    Just "-" -> Right (Just
                        (maybe fallback fromClient mclientSz))
                    Just t -> either (Left . ((name <> " ") <>))
                        (Right . Just . fromIntegral)
                        (strtonum 1 65535 t)
            baseSz = case mclientSz of
                Just csz | not detached -> csz
                _ -> defaultSizeOf gopts
        case (,) <$> dimension "-x" "width" (.cols) 80
                 <*> dimension "-y" "height" (.rows) 24 of
          Left e -> pure [RErr e]
          Right (mx, my) -> do
            let sz = baseSz
                    { cols = fromMaybe baseSz.cols mx
                    , rows = fromMaybe baseSz.rows my }
            sess <- createSession st (lookup "-s" opts) mrun environ dir' sz
            forM_ mgroup $ \g -> do
                sname <- readTVarIO sess.name
                notifyGroupChange st "session-added-to-group"
                    (sessionTarget sess.id) sess.id sname g 1
            atomically $ do
                writeTVar sess.group mgroup
                -- An explicit size becomes the session's default-size.
                when (isJust mx || isJust my) $
                    modifyTVar' sess.options $ insertDelta "default-size" $
                        OVScalar (T.pack (show sz.cols <> "x" <> show sz.rows))
                forM_ (lookup "-n" opts) $ \wname -> do
                    ws <- readTVar sess.windows
                    forM_ (Map.elems ws) $ \w -> do
                        writeTVar w.name wname
                        writeTVar w.autoRename False
            finish sess
    finish sess = do
        unless ("-d" `elem` flags) $
            forM_ mclient $ \client -> switchClientTo st client sess
        if "-P" `elem` flags
            then do
                mctx <- atomically $ do
                    mwin <- currentWindow sess
                    cur <- readTVar sess.currentIx
                    mpane <- maybe (pure Nothing) activePane mwin
                    pure ((,,) cur <$> mwin <*> mpane)
                env <- case mctx of
                    Nothing -> sessionFormatEnv st sess
                    Just (wix, win, pane) -> do
                        pix <- paneIndexOf st win pane
                        paneFormatEnv st sess wix win pix pane
                out <- expandFormat st env
                    (fromMaybe "#{session_name}:" (lookup "-F" opts))
                pure [ROutput out]
            else pure []

switchClientTo :: ServerState -> Client -> Session -> IO ()
switchClientTo st client sess = do
    old <- readTVarIO client.session
    atomically $ do
        when (old /= sess.id) $ do
            -- Record a visit only from a real prior session. A first attach
            -- (the sentinel/absent session) has nothing to push, but must
            -- still drop the destination from any carried history so the
            -- current session is never in its own history.
            sessMap <- readTVar st.sessions
            if Map.member old sessMap
                then do
                    modifyTVar' client.sessionHist (recordVisit old sess.id)
                    writeTVar st.lastSession (Just old)
                else modifyTVar' client.sessionHist (filter (/= sess.id))
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

-- | @kill-session@: destroy the target session (@-g@: its whole session
-- group; @-a@: every other session passing the @-f@ filter), or under
-- @-C@ only clear its windows' alerts.
cmdKillSession :: CommandImpl
cmdKillSession st mclient args
    | Just e <- unknownFlag "kill-session" "aCg" flags = pure [RErr e]
    | isJust (lookup "-f" opts), "-a" `notElem` flags || "-C" `elem` flags =
        pure [RErr "-f only valid with -a"]
    | otherwise = do
        res <- findTarget st mclient Target.FindSession (lookup "-t" opts)
        case res of
            Left e -> pure [RErr e]
            Right (sess, _, _, _)
                | "-C" `elem` flags -> [] <$ clearAlerts sess
                | "-a" `elem` flags -> do
                    others <- filter (\s -> s.id /= sess.id) . Map.elems
                        <$> readTVarIO st.sessions
                    doomed <- filterM passes others
                    [] <$ mapM_ (killSession st) doomed
                | "-g" `elem` flags -> do
                    grp <- readTVarIO sess.group
                    members <- case grp of
                        Nothing -> pure [sess]
                        Just g -> atomically $ do
                            ss <- Map.elems <$> readTVar st.sessions
                            filterM (fmap (== Just g) . readTVar . (.group)) ss
                    [] <$ mapM_ (killSession st) members
                | otherwise -> [] <$ killSession st sess
  where
    (opts, flags, _) = parseArgs "ft" args
    passes s = case lookup "-f" opts of
        Nothing -> pure True
        Just f -> do
            env <- sessionFormatEnv st s
            truthy <$> expandFormat st env f
    clearAlerts sess = atomically $ do
        ws <- Map.elems <$> readTVar sess.windows
        forM_ ws $ \w -> do
            writeTVar w.bellFlag False
            writeTVar w.activity False
            writeTVar w.silenceFlag False
        bumpDirty st

-- | Remove a session, then announce in tmux's order — session-closed
-- first, then window-unlinked per window. A window still linked into
-- another session (a group member's included) is only unlinked; its panes
-- survive.
killSession :: ServerState -> Session -> IO ()
killSession st sess = do
    (sname, mgroup, winInfo) <- atomically $ do
        sname <- readTVar sess.name
        grp <- readTVar sess.group
        mgroup <- forM grp $ \g -> do
            ss <- Map.elems <$> readTVar st.sessions
            (,) g . length <$> filterM (fmap (== grp) . readTVar . (.group)) ss
        ws <- Map.toAscList <$> readTVar sess.windows
        modifyTVar' st.sessions (Map.delete sess.id)
        winInfo <- forM ws $ \(_, win) -> do
            wname <- readTVar win.name
            -- a window linked elsewhere survives, homed in a survivor
            holders <- windowHolders st win
            forM_ (listToMaybe holders) $ \other ->
                rehomePanes other.id win  -- See Note [Pane home]
            ps <- Map.elems <$> readTVar win.panes
            pure (win, wname, not (null holders), ps)
        pure (sname, mgroup, winInfo)
    notify st "session-closed" noTarget
        [ ("session", PSessionRef sess.id sname) ]
    forM_ mgroup $ \(g, size) -> notifyGroupChange st
        "session-removed-from-group" noTarget sess.id sname g size
    forM_ winInfo $ \(win, wname, _, _) ->
        notify st "window-unlinked" noTarget
            [ ("session", PSessionRef sess.id sname)
            , ("window", PWindowRef win.id wname) ]
    killPaneLocsWith QuietLifecycle st
        [ (sess.id, win, p)
        | (win, _, shared, ps) <- winInfo, not shared, p <- ps ]
    broadcast st sess.id Exited

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

-- | @rename-session@: the new name is format-expanded against the target;
-- renaming to the current name does nothing.
cmdRenameSession :: CommandImpl
cmdRenameSession st mclient args
    | Just e <- unknownFlag "rename-session" "" flags = pure [RErr e]
    | [nm0] <- pos = do
        res <- findTarget st mclient Target.FindSession (lookup "-t" opts)
        case res of
          Left e -> pure [RErr e]
          Right (sess, _, _, _) -> do
            nm <- if "#" `T.isInfixOf` nm0
                then flip (expandFormat st) nm0 =<< sessionFormatEnv st sess
                else pure nm0
            out <- atomically $ do
                old <- readTVar sess.name
                sessions <- readTVar st.sessions
                names <- mapM (\s -> readTVar s.name)
                    (filter (\s -> s.id /= sess.id) (Map.elems sessions))
                case () of
                    _ | old == nm -> pure (Right Nothing)
                      | nm `elem` names ->
                          pure (Left ("duplicate session: " <> nm))
                      | otherwise -> do
                          writeTVar sess.name nm
                          bumpDirty st
                          pure (Right (Just old))
            case out of
                Left e -> pure [RErr e]
                Right Nothing -> pure []
                Right (Just old) -> do
                    notify st "session-renamed" (sessionTarget sess.id)
                        [ ("session", PSessionRef sess.id nm)
                        , ("old_name", PText old)
                        , ("new_name", PText nm) ]
                    pure []
    | otherwise = pure [RErr "usage: rename-session [-t target] new-name"]
  where
    (opts, flags, pos) = parseArgs "t" args

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
                            senv <- sessionFormatEnv st sess
                            sname <- readTVarIO sess.name
                            let env = Map.union (Map.fromList
                                    [ ("client_session", sname)
                                    , ("client_name", "client"
                                        <> tshow (rawClient c.id)) ]) senv
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
    -- Which (session, window-index, window) triples to list panes from:
    -- -a covers every window of every session, -s every window of the
    -- target session; otherwise the target window.
    etargets <- if allSessions
        then do
            sessions <- Map.elems <$> readTVarIO st.sessions
            Right . concat <$> mapM sessionWindows sessions
        else if "-s" `elem` flags
            then do
                res <- findTarget st mclient Target.FindSession (lookup "-t" opts)
                either (pure . Left) (\(sess, _, _, _) -> Right <$> sessionWindows sess) res
            else do
                res <- findTarget st mclient Target.FindWindow (lookup "-t" opts)
                pure (fmap (\(sess, ix, win, _) -> [(sess, ix, win)]) res)
    case etargets of
      Left e -> pure [RErr e]
      Right targets -> listPanesOf targets
  where
    sessionWindows sess = do
        ws <- Map.toAscList <$> readTVarIO sess.windows
        pure [ (sess, ix, win) | (ix, win) <- ws ]
    (opts, flags, _) = parseArgs "Ft" args
    mfmt = lookup "-F" opts
    allSessions = "-a" `elem` flags
    listPanesOf targets = do
        pbase <- (.paneBaseIndex) <$> readTVarIO st.options
        fmap (map ROutput . concat) . forM targets $ \(sess, wix, win) -> do
            ps <- atomically (windowPanes win)
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
