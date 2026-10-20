-- | Rebuilding a captured session tree in the live server: the walk shared
-- by the persistence restore (spawning fresh panes) and the reload handover
-- (adopting inherited ones), parameterized by how a pane comes back. The
-- capture types stay each boundary's own; the walk asks only for the fields
-- they share ('CapturedSession').
module Hat.Server.Rebuild
    ( CapturedSession
    , CapturedWindow
    , Rebuilt
    , newRebuilt
    , rebuildSession
    ) where

import Control.Concurrent.STM
import Control.Monad (forM, forM_, unless)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import GHC.Records (HasField)

import Hat.Geometry
import Hat.Model
import Hat.Server.Environ (environFromPairs)
import Hat.Server.Floating (FloatState (..), Modal (..), noFloats)
import Hat.Server.Mouse (ClickOutside (..))
import Hat.Server.Layout
import Hat.Server.LayoutString (capturedArea, restoreLayout)
import Hat.Server.Persist (ModalSnap (..))
import Hat.Server.Pane (startPaneReader)

-- | The session fields every capture format shares.
type CapturedSession s w p =
    ( CapturedWindow w p
    , HasField "name" s Text
    , HasField "startCwd" s Text
    , HasField "currentIx" s Int
    , HasField "windowHist" s [Int]
    , HasField "createdAt" s (Maybe POSIXTime)
    , HasField "restores" s Int
    , HasField "group" s (Maybe Text)
    , HasField "windows" s [w]
    )

-- | The window fields every capture format shares.
type CapturedWindow w p =
    ( HasField "ix" w Int
    , HasField "name" w Text
    , HasField "layout" w Text
    , HasField "active" w Int
    , HasField "paneHist" w [Int]
    , HasField "autoRename" w Bool
    , HasField "key" w (Maybe Int)
    , HasField "linkOf" w (Maybe Int)
    , HasField "modal" w (Maybe ModalSnap)
    , HasField "size" w (Maybe Size)
    , HasField "panes" w [p]
    )

-- | What one rebuild of a captured tree has built so far, for the links and
-- group members later in the capture to share: windows by their capture
-- 'key', and group window maps by group name. See Note [Captured links].
data Rebuilt = Rebuilt
    { windows :: IORef (Map.Map Int Window)
    , groups  :: IORef (Map.Map Text (TVar (Map.Map Int Window)))
    }

newRebuilt :: IO Rebuilt
newRebuilt = Rebuilt <$> newIORef Map.empty <*> newIORef Map.empty

-- | Rebuild one captured session in the live tree, bringing each pane back
-- with @mkPane@. Each window comes up at its captured size — a capture
-- predating window sizes gave every window its session's, the area its
-- layouts were fitted to ('capturedArea') — so the pre-attach reconcile
-- finds every pane already at its layout size. A rebuild counts as one
-- restore; a capture without a creation stamp adopts now as the session's
-- creation time. A link entry shares the window it names, and a later
-- member of a group the group's window map, both from @done@; a session
-- left with no window is dropped.
rebuildSession
    :: CapturedSession s w p
    => ServerState
    -> Rebuilt                              -- ^ the capture's walk so far
    -> [(Text, Text)]                       -- ^ environment seed for the session
    -> (SessionId -> Size -> p -> IO Pane)  -- ^ bring one pane back
    -> s -> IO ()
rebuildSession st done env mkPane csess = do
    sid <- SessionId <$> atomically (freshId st.nextSession)
    shared <- case csess.group of
        Nothing -> pure Nothing
        Just g -> Map.lookup g <$> readIORef done.groups
    (windowsVar, built) <- case shared of
        Just var -> pure (var, [])
        Nothing -> do
            let owned = filter (not . null . (.panes)) csess.windows
                sessionSz = capturedArea (map (.layout) owned)
            built <- forM owned $ \cwin -> do
                (win, panes) <- rebuildWindow st (mkPane sid)
                    (fromMaybe sessionSz cwin.size) cwin
                forM_ cwin.key $ \k ->
                    modifyIORef' done.windows (Map.insert k win)
                pure (cwin.ix, win, panes)
            known <- readIORef done.windows
            let linked =
                    [ (cwin.ix, win)
                    | cwin <- csess.windows, null cwin.panes
                    , Just k <- [cwin.linkOf], Just win <- [Map.lookup k known] ]
            var <- newTVarIO (Map.fromList
                ([(wix, win) | (wix, win, _) <- built] <> linked))
            forM_ csess.group $ \g ->
                modifyIORef' done.groups (Map.insert g var)
            pure (var, built)
    winMap <- readTVarIO windowsVar
    unless (Map.null winMap) $ do
        let curIx | Map.member csess.currentIx winMap = csess.currentIx
                  | otherwise = maybe csess.currentIx fst (Map.lookupMin winMap)
            -- Keep the MRU history to surviving windows other than the current
            -- one; nothing else is a meaningful "last" to return to.
            winHist = List.nub
                [l | l <- csess.windowHist, l /= curIx, Map.member l winMap]
        now <- getPOSIXTime
        nameVar    <- newTVarIO csess.name
        currentVar <- newTVarIO curIx
        windowHistVar <- newTVarIO winHist
        environVar <- newTVarIO (environFromPairs env)
        cwdVar     <- newTVarIO (T.unpack csess.startCwd)
        optionsVar <- newOwnOptionsIO
        groupVar   <- newTVarIO csess.group
        activityVar <- newTVarIO =<< atomically (tickActivity st)
        holdVar    <- newTVarIO HeldUntilAttached
        let sess = Session
                { id = sid
                , createdAt = fromMaybe now csess.createdAt
                , restores = csess.restores + 1
                , name = nameVar, windows = windowsVar
                , currentIx = currentVar, windowHist = windowHistVar
                , environ = environVar
                , startCwd = cwdVar, options = optionsVar
                , group = groupVar
                , activity = activityVar, destroyHold = holdVar }
        atomically $ modifyTVar' st.sessions (Map.insert sid sess)
        forM_ built $ \(_, win, panes) ->
            forM_ panes (startPaneReader st sid win)

rebuildWindow
    :: CapturedWindow w p
    => ServerState -> (Size -> p -> IO Pane) -> Size -> w -> IO (Window, [Pane])
rebuildWindow st mkPane sz cwin = do
    wid <- WindowId <$> atomically (freshId st.nextWindow)
    panes <- forM cwin.panes (mkPane sz)
    let pids = map (.id) panes
        paneMap = Map.fromList [(p.id, p) | p <- panes]
        -- Our own emitted string round-trips; the named layout is only a
        -- fallback for a corrupt string, and still contains every pane.
        (lay, zorder) = either (const (fallback, [])) id (restoreLayout cwin.layout pids)
        paneAt o = if o >= 0 && o < length pids then Just (pids !! o) else Nothing
        modal = do
            m <- cwin.modal
            p <- paneAt m.pane
            pure Modal
                { pane = p, returnTo = m.returnTo >>= paneAt, rezoom = m.rezoom
                , outside = if m.closeOnClick then CloseModal else KeepModal }
        fallback = case pids of
            p : _ -> fromMaybe (singlePane p)
                (namedLayout defaultNamedSizes sz EvenHorizontal pids)
            [] -> error "rebuildWindow: a captured window with no panes"
        -- A modal pane holds the window's focus however the capture
        -- recorded it.
        activePid = maybe (pids !! max 0 (min (length pids - 1) cwin.active)) (.pane) modal
        -- Map the MRU history's ordinals back to surviving pane ids, dropping
        -- the active pane and any out-of-range ordinal; keep order, drop dups.
        paneHistPids = List.nub
            [ pids !! o | o <- cwin.paneHist
            , o >= 0, o < length pids, pids !! o /= activePid ]
    nameVar       <- newTVarIO cwin.name
    layoutVar     <- newTVarIO lay
    layoutNameVar <- newTVarIO Nothing
    panesVar      <- newTVarIO paneMap
    activeVar     <- newTVarIO activePid
    paneHistVar   <- newTVarIO paneHistPids
    bellVar       <- newTVarIO False
    activityVar   <- newTVarIO False
    zoomVar       <- newTVarIO Nothing
    -- Auto-rename status survives the round-trip: a window renaming
    -- automatically keeps tracking its active pane, a manually-named one
    -- keeps its pinned name.
    autoRenameVar <- newTVarIO cwin.autoRename
    optionsVar    <- newOwnOptionsIO
    silenceVar    <- newTVarIO False
    activityAtVar <- newTVarIO =<< getPOSIXTime
    arrangeVar    <- newTVarIO Nothing
    floatsVar     <- newTVarIO noFloats { zorder, modal }
    menuVar       <- newTVarIO Nothing
    menuLastVar   <- newTVarIO (0, 0)
    sizeVar       <- newTVarIO sz
    manualVar     <- newTVarIO sz
    latestVar     <- newTVarIO Nothing
    let win = Window
            { id = wid, name = nameVar, layout = layoutVar
            , layoutName = layoutNameVar
            , panes = panesVar, activeId = activeVar
            , paneHist = paneHistVar, bellFlag = bellVar
            , activity = activityVar, zoomed = zoomVar
            , silenceFlag = silenceVar, activityAt = activityAtVar
            , autoRename = autoRenameVar, options = optionsVar
            , arrangeCache = arrangeVar, floats = floatsVar
            , menu = menuVar, menuLast = menuLastVar
            , size = sizeVar, manualSize = manualVar, latest = latestVar }
    pure (win, panes)
