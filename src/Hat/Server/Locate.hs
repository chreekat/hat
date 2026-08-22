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
    ) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM
import Control.Monad (forM)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR

import Hat.Model
import Hat.Model.Options
import Hat.Server.Command.Types (Reply (..))
import Hat.Server.Layout
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
targetSession st mclient mtarget = atomically $ do
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
            Nothing -> pure (snd <$> Map.lookupMax sessions)

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
                         | pid <- Map.keys ps ]
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
    let entryById i = List.find (\se -> se.sessionId == i) entries
        clientCur = Target.sessionCurrent =<< entryById =<< msid
        tmuxPaneCur = do
            client <- mclient
            v <- List.lookup "TMUX_PANE" client.env
            n <- T.stripPrefix "%" v >>= parseDec
            Target.sessionCurrentFound entries (PaneId n)
        bestCur = Target.sessionCurrent =<< listToMaybe (reverse entries)
    pure Target.World
        { sessions = entries
        , paneBase = pbase
        , marked = Target.paneFound entries =<< mmarked
        , clientCurrent = clientCur
        , current = clientCur <|> tmuxPaneCur <|> bestCur
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
    ps <- readTVarIO win.panes
    pure (maybe pbase (pbase +) (Map.lookupIndex pane.id ps))

-- | Resolve a @-t@ pane token: @!@ the last pane, @{marked}@ the
-- marked pane, @%N@ a pane by id anywhere; otherwise the current pane
-- of the caller's window.
targetPane :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe Pane)
targetPane st mclient mtok = case parsePaneTarget mtok of
    PaneById n -> atomically (findPaneById st n)
    PaneMarked -> atomically $ do
        mpid <- readTVar st.markedPane
        maybe (pure Nothing) (findPaneById st . rawPane) mpid
    tgt -> do
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
clientOptions st client = atomically $ do
    sid <- readTVar client.session
    msess <- Map.lookup sid <$> readTVar st.sessions
    maybe (resolveGlobal st) (resolveForSession st) msess

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
