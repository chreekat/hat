-- | Resolving a command's @-t@ target against a live snapshot of the
-- server tree: which session, window, or pane a token names, the
-- cmd-find grammar wrappers, and the option chain in effect for a
-- resolved target. Read-only over the tree — it finds, it never mutates.
module Hat.Server.Locate
    ( clientView
    , clientActivePane
    , targetSession
    , targetWorld
    , findTarget
    , findWindowIndexTarget
    , paneIndexOf
    , targetPane
    , markedTarget
    , currentWindowOf
    , targetCurrentWindow
    , targetPaneScoped
    , withTargetSession
    , withCurrentWindow
    , currentResolved
    , clientOptions
    , noSuchTarget
    , locatePane
    , siblingPane
    , findTargetClient
    , setCurrentPane
    ) where

import Control.Applicative ((<|>))
import Control.Concurrent (myThreadId)
import Control.Concurrent.STM
import Control.Monad (forM, forM_)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.IORef (modifyIORef', readIORef)

import Hat.Model
import Hat.Model.Options
import Hat.Server.HookTypes (HookAmbient (..), HooksState (..))
import Hat.Server.Command.Types (Reply (..))
import Hat.Server.Keys (KeyState (..))
import Hat.Server.Layout
import Hat.Server.Mouse (MouseEvent (..))
import Hat.Server.Target (PaneTarget (..), parsePaneTarget)
import Hat.Server.Target qualified as Target

clientView :: ServerState -> Client -> STM (Maybe (Session, Window))
clientView st client = do
    sid <- readTVar client.session
    msess <- Map.lookup sid <$> readTVar st.sessions
    case msess of
        Nothing -> pure Nothing
        Just sess -> do
            mwin <- currentWindow sess
            pure ((,) sess <$> mwin)

clientActivePane :: ServerState -> Client -> IO (Maybe Pane)
clientActivePane st client = atomically $ do
    mv <- clientView st client
    case mv of
        Nothing -> pure Nothing
        Just (_, win) -> activePane win

targetSession :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe Session)
targetSession st mclient mtarget = do
    mambSid <- ambientSession st mclient
    atomically $ do
      sessions <- readTVar st.sessions
      case mtarget of
        Just t -> do
            let stripped = fromMaybe t (T.stripSuffix ":" t)
            found <- forM (Map.elems sessions) $ \sess -> do
                nm <- readTVar sess.name
                pure $ if nm == stripped
                    || tshow (rawSession sess.id) == stripped
                    || ("$" <> tshow (rawSession sess.id)) == stripped
                    then Just sess else Nothing
            pure (foldr (\m acc -> maybe acc Just m) Nothing found)
        Nothing -> case mclient of
            Just client -> do
                sid <- readTVar client.session
                case Map.lookup sid sessions of
                    Just sess -> pure (Just sess)
                    Nothing -> pure (snd <$> Map.lookupMax sessions)
            Nothing -> case mambSid >>= (`Map.lookup` sessions) of
                Just sess -> pure (Just sess)
                Nothing -> pure (snd <$> Map.lookupMax sessions)

-- | The hook context's target session, for clientless resolution: a hook
-- command with no client and no -t acts on the event's session.
ambientSession :: ServerState -> Maybe Client -> IO (Maybe SessionId)
ambientSession st = \case
    Just _ -> pure Nothing
    Nothing -> do
        tid <- myThreadId
        mamb <- Map.lookup tid <$> readTVarIO st.hooks.ambient
        pure (mamb >>= (.targetSession))

-- | Like 'ambientSession', for the event's window and pane.
ambientWindowPane
    :: ServerState -> Maybe Client -> IO (Maybe WindowId, Maybe PaneId)
ambientWindowPane st = \case
    Just _ -> pure (Nothing, Nothing)
    Nothing -> do
        tid <- myThreadId
        mamb <- Map.lookup tid <$> readTVarIO st.hooks.ambient
        pure (mamb >>= (.targetWindow), mamb >>= (.targetPane))

-- | Snapshot the server tree for the pure cmd-find core
-- ('Hat.Server.Target.resolveTarget'). The current state is the client's
-- attached view, else the session its @TMUX_PANE@ lives in, else the
-- newest session (matching 'targetSession').
targetWorld :: ServerState -> Maybe Client -> IO Target.World
targetWorld st mclient = do
    sessMap <- readTVarIO st.sessions
    entries <- forM (Map.toAscList sessMap) $ \(sid, sess) -> do
        (nm, cur, lastIx, eff) <- atomically $ (,,,)
            <$> readTVar sess.name <*> readTVar sess.currentIx
            <*> (listToMaybe <$> readTVar sess.windowHist) <*> readTVar sess.lastSize
        ws <- readTVarIO sess.windows
        wentries <- forM (Map.toAscList ws) $ \(ix, win) -> do
            (wname, lay, act, lastP, ps) <- atomically $ (,,,,)
                <$> readTVar win.name <*> readTVar win.layout
                <*> readTVar win.activeId <*> (listToMaybe <$> readTVar win.paneHist)
                <*> readTVar win.panes
            let warea = eff
                rects = fst (arrange (sizeRect warea) lay)
            pure ( ix
                 , Target.WindowEntry
                     { windowId = win.id
                     , name = wname
                     , panes =
                         [ (pid, fromMaybe (sizeRect warea) (List.lookup pid rects))
                         | pid <- paneOrder lay ps ]
                     , activePane = act
                     , lastPane = lastP
                     , area = warea
                     } )
        pure Target.SessionEntry
            { sessionId = sid, name = nm, windows = wentries
            , currentIx = cur, lastIx = lastIx }
    pbase <- (.paneBaseIndex) <$> readTVarIO st.options
    mmarked <- readTVarIO st.markedPane
    msid <- traverse (\c -> readTVarIO c.session) mclient
    mambS <- ambientSession st mclient
    (mambW, mambP) <- ambientWindowPane st mclient
    let entryById i = List.find (\se -> se.sessionId == i) entries
        -- A clientless hook command's current state is its event's target.
        ambientEntries = maybe entries
            (\i -> filter ((== i) . (.sessionId)) entries <> entries) mambS
        ambientCur = case (mambP, mambW) of
            (Just p, _) -> Target.paneFound ambientEntries p
            (Nothing, Just w) -> listToMaybe
                [ Target.Found se.sessionId ix we.activePane
                | se <- ambientEntries, (ix, we) <- se.windows, we.windowId == w ]
            (Nothing, Nothing) -> Target.sessionCurrent =<< entryById =<< mambS
        clientCur = Target.sessionCurrent =<< entryById =<< msid
        tmuxPaneCur = do
            client <- mclient
            v <- List.lookup "TMUX_PANE" client.env
            n <- T.stripPrefix "%" v >>= parseDec
            Target.sessionCurrentFound entries (PaneId n)
        bestCur = Target.sessionCurrent =<< listToMaybe (reverse entries)
    (mev, mstate) <- case mclient of
        Nothing -> pure (Nothing, Nothing)
        Just c -> (\ks -> (ks.mouseEvent, ks.commandState)) <$> readIORef c.keyState
    -- tmux's cmd_mouse_pane: the event's window (or its session's current
    -- one) and pane (or that window's active one).
    let mouseFound = do
            ev <- mev
            se <- entryById ev.onSession
            (ix, we) <- case ev.onWindow of
                Just wid -> List.find ((== wid) . (.windowId) . snd) se.windows
                Nothing -> List.find ((== se.currentIx) . fst) se.windows
            pid <- case ev.onPane of
                Nothing -> Just we.activePane
                Just p | p `elem` map fst we.panes -> Just p
                       | otherwise -> Nothing
            pure (Target.Found se.sessionId ix pid)
        stateFound = do
            (sid, wid, pid) <- mstate
            se <- entryById sid
            (ix, we) <- List.find ((== wid) . (.windowId) . snd) se.windows
            if pid `elem` map fst we.panes then Just (Target.Found sid ix pid) else Nothing
    pure Target.World
        { sessions = entries
        , paneBase = pbase
        , marked = Target.paneFound entries =<< mmarked
        , clientCurrent = clientCur
        -- A mouse binding's commands start from the pane under the mouse,
        -- as tmux's key callback sets their state.
        , current = stateFound <|> mouseFound <|> clientCur <|> tmuxPaneCur <|> ambientCur
            <|> bestCur
        , mouse = mouseFound
        }
  where
    parseDec t = case TR.decimal t of
        Right (n, rest) | T.null rest -> Just n
        _ -> Nothing

-- | Resolve a full @-t@ target to live structures; errors carry
-- cmd-find's texts.
findTarget
    :: ServerState -> Maybe Client -> Target.FindType -> Maybe Text
    -> IO (Either Text (Session, Int, Window, Pane))
findTarget st mclient ftype mtarget = do
    world <- targetWorld st mclient
    case Target.resolveTarget ftype world mtarget of
        Left e -> pure (Left e)
        Right f -> do
            got <- atomically $ do
                sessions <- readTVar st.sessions
                case Map.lookup f.session sessions of
                    Nothing -> pure Nothing
                    Just sess -> do
                        ws <- readTVar sess.windows
                        case Map.lookup f.windowIx ws of
                            Nothing -> pure Nothing
                            Just win -> do
                                ps <- readTVar win.panes
                                pure $ (,,,) sess f.windowIx win
                                    <$> Map.lookup f.pane ps
            -- The snapshot raced a concurrent mutation; treat as missing.
            pure (maybe (Left "no such target") Right got)

-- | Resolve a destination window index (@new-window@\/@move-window@ style
-- @-t@): the session plus an index that may not exist yet ('Nothing' =
-- the command picks a free slot).
findWindowIndexTarget
    :: ServerState -> Maybe Client -> Maybe Text
    -> IO (Either Text (Session, Maybe Int))
findWindowIndexTarget st mclient mtarget = do
    world <- targetWorld st mclient
    case Target.resolveWindowIndex world mtarget of
        Left e -> pure (Left e)
        Right (sid, mix) -> do
            sessions <- readTVarIO st.sessions
            pure $ case Map.lookup sid sessions of
                Nothing -> Left "no such target"
                Just sess -> Right (sess, mix)

-- | A pane's tmux index: its position in the window's creation order,
-- offset by @pane-base-index@.
paneIndexOf :: ServerState -> Window -> Pane -> IO Int
paneIndexOf st win pane = do
    pbase <- (.paneBaseIndex) <$> readTVarIO st.options
    order <- atomically (map (.id) <$> windowPanes win)
    pure (maybe pbase (pbase +) (List.elemIndex pane.id order))

-- | The marked pane as a source target, while it still exists (tmux's
-- @CMD_FIND_DEFAULT_MARKED@).
markedTarget :: ServerState -> IO (Maybe Text)
markedTarget st = atomically $ do
    mpid <- readTVar st.markedPane
    case mpid of
        Nothing -> pure Nothing
        Just pid -> fmap (const ("%" <> tshow (rawPane pid))) <$> locatePane st pid

-- | Resolve a @-t@ pane token: @!@ the last pane, @{marked}@ the
-- marked pane, @%N@ a pane by id anywhere, any other target through
-- cmd-find ('findTarget'); with none, the current pane of the caller's
-- window.
targetPane :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe Pane)
targetPane st mclient mtok = case parsePaneTarget mtok of
    PaneById n -> atomically (findPaneById st n)
    PaneMarked -> atomically $ do
        mpid <- readTVar st.markedPane
        maybe (pure Nothing) (findPaneById st . rawPane) mpid
    PaneCurrent | Just t <- mtok -> do
        res <- findTarget st mclient Target.FindPane (Just t)
        pure (either (const Nothing) (\(_, _, _, p) -> Just p) res)
    tgt -> do
        (_, mambPane) <- ambientWindowPane st mclient
        case (mtok, mambPane) of
          (Nothing, Just pid) -> atomically (findPaneById st (rawPane pid))
          _ -> do
            mwin <- currentWindowOf st mclient
            case mwin of
                Nothing -> pure Nothing
                Just win -> atomically $ do
                    ps <- readTVar win.panes
                    case tgt of
                        PaneLast -> do
                            hist <- readTVar win.paneHist
                            pure (listToMaybe hist >>= (`Map.lookup` ps))
                        _ -> do
                            a <- readTVar win.activeId
                            pure (Map.lookup a ps)

-- | The window a command acts in: the caller's current window, or (for
-- a clientless control command) the current window of the most-recent
-- session.
currentWindowOf :: ServerState -> Maybe Client -> IO (Maybe Window)
currentWindowOf st mclient = do
    mv <- atomically (maybe (pure Nothing) (clientView st) mclient)
    case mv of
        Just (_, win) -> pure (Just win)
        Nothing -> do
            (mambWin, _) <- ambientWindowPane st mclient
            mw <- case mambWin of
                Just wid -> atomically (findWindowById st wid)
                Nothing -> pure Nothing
            case mw of
                Just win -> pure (Just win)
                Nothing -> do
                    msess <- targetSession st mclient Nothing
                    case msess of
                        Nothing -> pure Nothing
                        Just sess -> atomically (currentWindow sess)

-- | The current window of the target session (or the client's).
targetCurrentWindow
    :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe Window)
targetCurrentWindow st mclient mtarget = do
    msess <- targetSession st mclient mtarget
    case msess of
        Nothing -> pure Nothing
        Just sess -> atomically (currentWindow sess)

-- | The pane a @-p@ option scope acts on: a pane id (or @!@/@~@) via
-- 'targetPane', or the active pane of the target session's current window —
-- an unresolvable @-t@ is the caller's error, never a silent fallback to
-- the current pane.
targetPaneScoped :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe Pane)
targetPaneScoped st mclient mtarget = case (mtarget, parsePaneTarget mtarget) of
    (Just _, PaneCurrent) -> do
        mwin <- targetCurrentWindow st mclient mtarget
        case mwin of
            Nothing -> pure Nothing
            Just win -> atomically $ do
                ps <- readTVar win.panes
                a <- readTVar win.activeId
                pure (Map.lookup a ps)
    _ -> targetPane st mclient mtarget

withTargetSession
    :: ServerState -> Maybe Client -> Maybe Text
    -> (Session -> IO [Reply]) -> IO [Reply]
withTargetSession st mclient mtarget body = do
    msess <- targetSession st mclient mtarget
    case msess of
        Nothing -> pure [RErr "no such session"]
        Just sess -> body sess

withCurrentWindow
    :: ServerState -> Maybe Client
    -> (Session -> Window -> IO [Reply]) -> IO [Reply]
withCurrentWindow st mclient body = do
    mv <- atomically (maybe (pure Nothing) (clientView st) mclient)
    view <- case mv of
        Just v -> pure (Just v)
        Nothing -> do
            msess <- targetSession st mclient Nothing
            case msess of
                Nothing -> pure Nothing
                Just sess -> do
                    mwin <- atomically (currentWindow sess)
                    pure ((,) sess <$> mwin)
    case view of
        Nothing -> pure [RErr "no current window"]
        Just (sess, win) -> body sess win

-- | The options in effect for the target session (or the global chain when
-- there is none): the base an @-a@ append concatenates onto.
currentResolved :: ServerState -> Maybe Client -> Maybe Text -> IO Options
currentResolved st mclient mtarget = do
    msess <- targetSession st mclient mtarget
    atomically $ maybe (resolveGlobal st) (resolveForSession st) msess

-- | The options in effect for a client's current session, so a bare @set@
-- there is honored.
clientOptions :: ServerState -> Client -> IO Options
clientOptions st client = do
    sid <- readTVarIO client.session
    msess <- Map.lookup sid <$> readTVarIO st.sessions
    maybe (atomically (resolveGlobal st)) (resolveForSessionIO st) msess

-- | The scope-specific error for an option target that did not resolve.
noSuchTarget :: Text -> Maybe Text -> Text
noSuchTarget kind = \case
    Just t -> "no such " <> kind <> ": " <> t
    Nothing -> "no current " <> kind

-- | The session and window a live pane belongs to. See 'cmdKillPane'.
locatePane :: ServerState -> PaneId -> STM (Maybe (SessionId, Window))
locatePane st pid = do
    sessions <- readTVar st.sessions
    hits <- forM (Map.toList sessions) $ \(sid, sess) -> do
        ws <- Map.elems <$> readTVar sess.windows
        winHits <- forM ws $ \win -> do
            ps <- readTVar win.panes
            pure [(sid, win) | Map.member pid ps]
        pure (concat winHits)
    pure (listToMaybe (concat hits))

-- | The pane @step@ positions from the active one in layout order.
siblingPane :: ServerState -> Window -> Int -> IO (Maybe Pane)
siblingPane _ win step = atomically $ do
    lay <- readTVar win.layout
    ps <- readTVar win.panes
    active <- readTVar win.activeId
    let order = layoutPanes lay
    pure $ case List.elemIndex active order of
        Just i | not (null order) ->
            let pid = order !! ((i + step + length order) `mod` length order)
            in Map.lookup pid ps
        _ -> Nothing

-- | The client a @-c target-client@ names (tmux's @cmd_find_client@): an
-- attached terminal client by name; without a name, the issuing client
-- when it is one, else the most recently active one viewing @msid@ (or any
-- session).
findTargetClient
    :: ServerState -> Maybe Client -> Maybe SessionId -> Maybe Text -> IO (Either Text Client)
findTargetClient st mclient msid mname = atomically $ do
    cs <- filter terminal . Map.elems <$> readTVar st.clients
    live <- readTVar st.sessions
    attached <- fmap concat $ forM cs $ \c -> do
        sid <- readTVar c.session
        stamp <- readTVar c.lastActive
        pure [ (c, sid, stamp) | Map.member sid live ]
    pure $ case mname of
        Just n -> case [ c | (c, _, _) <- attached, clientName c == n ] of
            c : _ -> Right c
            [] -> Left ("can't find client: " <> n)
        Nothing
            | Just c <- mclient, any (\(a, _, _) -> a.id == c.id) attached -> Right c
            | otherwise ->
                let inSession = [ a | a@(_, sid, _) <- attached, Just sid == msid ]
                    pool = if null inSession then attached else inSession
                in case List.sortOn (\(_, _, stamp) -> negate stamp) pool of
                    (c, _, _) : _ -> Right c
                    [] -> Left "no current client"
  where
    terminal c = c.role == Attached && isNothing c.control
    clientName c = "client" <> tshow (rawClient c.id)

-- | A command made a pane the one the commands after it act on (tmux's
-- @cmd_find_from_winlink_pane@ on the queue's current state). Without a
-- state of its own, the issuing client's current pane already follows
-- what the command changed; only a state set apart from it — a menu's, or
-- a mouse binding's — needs moving.
setCurrentPane :: Maybe Client -> SessionId -> WindowId -> PaneId -> IO ()
setCurrentPane mclient sid wid pid = forM_ mclient $ \c ->
    modifyIORef' c.keyState $ \ks ->
        if isJust ks.commandState || isJust ks.mouseEvent
            then ks { commandState = Just (sid, wid, pid) }
            else ks
