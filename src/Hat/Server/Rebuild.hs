-- | Rebuilding a captured session tree in the live server: the walk shared
-- by the persistence restore (spawning fresh panes) and the reload handover
-- (adopting inherited ones), parameterized by how a pane comes back. The
-- capture types stay each boundary's own; the walk asks only for the fields
-- they share ('CapturedSession').
module Hat.Server.Rebuild
    ( CapturedSession
    , CapturedWindow
    , rebuildSession
    ) where

import Control.Concurrent.STM
import Control.Monad (forM, forM_, unless)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Records (HasField)

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.Environ (environFromPairs)
import Hat.Server.Layout
import Hat.Server.LayoutString (capturedArea, layoutFromString)
import Hat.Server.Pane (startPaneReader)

-- | The session fields every capture format shares.
type CapturedSession s w p =
    ( CapturedWindow w p
    , HasField "name" s Text
    , HasField "startCwd" s Text
    , HasField "currentIx" s Int
    , HasField "windowHist" s [Int]
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
    , HasField "panes" w [p]
    )

-- | Rebuild one captured session in the live tree, bringing each pane back
-- with @mkPane@. The session comes up at its captured window area
-- ('capturedArea'), so the pre-attach reconcile finds every pane already at
-- its layout size.
rebuildSession
    :: CapturedSession s w p
    => ServerState
    -> [(Text, Text)]                       -- ^ environment seed for the session
    -> (SessionId -> Size -> p -> IO Pane)  -- ^ bring one pane back
    -> s -> IO ()
rebuildSession st env mkPane csess = do
    let wins = filter (not . null . (.panes)) csess.windows
    unless (null wins) $ do
        sid <- SessionId <$> atomically (freshId st.nextSession)
        let sz = capturedArea (map (.layout) wins)
        built <- forM wins $ \cwin -> do
            (win, panes) <- rebuildWindow st (mkPane sid) sz cwin
            pure (cwin.ix, win, panes)
        let winMap = Map.fromList [(wix, win) | (wix, win, _) <- built]
            curIx | Map.member csess.currentIx winMap = csess.currentIx
                  | otherwise = maybe csess.currentIx fst (Map.lookupMin winMap)
            -- Keep the MRU history to surviving windows other than the current
            -- one; nothing else is a meaningful "last" to return to.
            winHist = List.nub
                [l | l <- csess.windowHist, l /= curIx, Map.member l winMap]
        nameVar    <- newTVarIO csess.name
        windowsVar <- newTVarIO winMap
        currentVar <- newTVarIO curIx
        windowHistVar <- newTVarIO winHist
        sizeVar    <- newTVarIO sz
        environVar <- newTVarIO (environFromPairs env)
        cwdVar     <- newTVarIO (T.unpack csess.startCwd)
        optionsVar <- newTVarIO emptyDelta
        let sess = Session
                { id = sid, name = nameVar, windows = windowsVar
                , currentIx = currentVar, windowHist = windowHistVar
                , lastSize = sizeVar, environ = environVar
                , startCwd = cwdVar, options = optionsVar }
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
        lay = fromMaybe (namedLayout EvenHorizontal (1 % 2) pids)
                        (layoutFromString cwin.layout pids)
        activePid = pids !! max 0 (min (length pids - 1) cwin.active)
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
    optionsVar    <- newTVarIO emptyDelta
    silenceVar    <- newTVarIO False
    activityAtVar <- newTVarIO =<< getPOSIXTime
    let win = Window
            { id = wid, name = nameVar, layout = layoutVar
            , layoutName = layoutNameVar
            , panes = panesVar, activeId = activeVar
            , paneHist = paneHistVar, bellFlag = bellVar
            , activity = activityVar, zoomed = zoomVar
            , silenceFlag = silenceVar, activityAt = activityAtVar
            , autoRename = autoRenameVar, options = optionsVar }
    pure (win, panes)
