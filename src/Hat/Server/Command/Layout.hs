-- | The layout commands: @break-pane@\/@join-pane@ (moving a pane between
-- windows) and the named-layout commands (@select-layout@, @next-layout@,
-- @previous-layout@) that rearrange a window's panes.
module Hat.Server.Command.Layout
    ( cmdBreakPane
    , cmdJoinPane
    , cmdSelectLayout
    , cmdNextLayout
    , cmdPreviousLayout
    , parseLayoutName
    , mainPaneRatio
    ) where

import Control.Concurrent.STM
import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Text (Text)

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import Hat.Server.Command.Window (nextFreeWindowIndex)
import Hat.Server.Layout
import Hat.Server.LayoutString (layoutFromString)
import Hat.Server.Locate (targetPane, withCurrentWindow)
import Hat.Server.Pane (removePaneFromTree, wrapPaneInWindow)
import Hat.Server.Resize (applySessionSize)

-- | @break-pane [-d] [-t]@: move the active pane into a new window of
-- its own. No-op when it is the window's only pane.
cmdBreakPane :: CommandImpl
cmdBreakPane st mclient args = do
    let (_, flags, _) = parseArgs "t" args
    withCurrentWindow st mclient $ \sess win -> do
        mactive <- atomically (activePane win)
        ps <- readTVarIO win.panes
        case mactive of
            Just pane | Map.size ps > 1 -> do
                win2 <- wrapPaneInWindow st pane
                srvOpts <- readTVarIO st.options
                atomically $ do
                    removePaneFromTree st pane.id
                    ws <- readTVar sess.windows
                    let ix = nextFreeWindowIndex srvOpts.baseIndex ws
                    modifyTVar' sess.windows (Map.insert ix win2)
                    unless ("-d" `elem` flags) $ do
                        cur <- readTVar sess.currentIx
                        writeTVar sess.lastIx (Just cur)
                        writeTVar sess.currentIx ix
                    bumpDirty st
                applySessionSize st sess.id
                pure []
            _ -> pure [RErr "can't break with only one pane"]

-- | @join-pane [-h|-v] [-b] -s src [-t dst]@: move the @src@ pane into
-- the destination window (default: the current one), splitting its
-- active pane. Backs the config's @choose-window 'join-pane -?s "%%"'@.
cmdJoinPane :: CommandImpl
cmdJoinPane st mclient args = do
    let (opts, flags, _) = parseArgs "st" args
        orient | "-h" `elem` flags = LeftRight
               | otherwise = TopBottom
        placement = if "-b" `elem` flags then Before else After
    msrc <- targetPane st mclient (lookup "-s" opts)
    withCurrentWindow st mclient $ \sess dstWin -> do
        dstPanes <- readTVarIO dstWin.panes
        case msrc of
            Just src | not (Map.member src.id dstPanes) -> do
                atomically $ do
                    dstActive <- readTVar dstWin.activeId
                    removePaneFromTree st src.id
                    modifyTVar' dstWin.panes (Map.insert src.id src)
                    modifyTVar' dstWin.layout
                        (splitLeaf dstActive orient placement src.id)
                    writeTVar dstWin.lastActive (Just dstActive)
                    writeTVar dstWin.activeId src.id
                    writeTVar dstWin.zoomed Nothing
                    bumpDirty st
                applySessionSize st sess.id
                pure []
            _ -> pure [RErr "no source pane"]

-- | @select-layout <name>@: rearrange the current window's panes into a
-- named layout (@main-vertical@, @even-horizontal@, @tiled@, …). The
-- @main-*@ layouts size their main pane from @main-pane-width@/@-height@.
cmdSelectLayout :: CommandImpl
cmdSelectLayout st mclient args = do
    let (_, _, pos) = parseArgs "t" args
    case pos of
        (nameT : _) -> case parseLayoutName nameT of
            Just lname -> applyNamedLayout st mclient lname
            Nothing -> applyLayoutString st mclient nameT
        [] -> pure [RErr "usage: select-layout name"]

-- | Reshape the current window to a saved tmux layout string, mapping
-- its geometry onto the window's panes in order (resurrect's restore).
applyLayoutString :: ServerState -> Maybe Client -> Text -> IO [Reply]
applyLayoutString st mclient str =
    withCurrentWindow st mclient $ \sess win -> do
        ok <- atomically $ do
            pids <- layoutPanes <$> readTVar win.layout
            case layoutFromString str pids of
                Just lay -> do
                    writeTVar win.layout lay
                    writeTVar win.layoutName Nothing
                    writeTVar win.zoomed Nothing
                    bumpDirty st
                    pure True
                Nothing -> pure False
        if ok
            then applySessionSize st sess.id >> pure []
            else pure [RErr ("invalid layout: " <> str)]

parseLayoutName :: Text -> Maybe LayoutName
parseLayoutName = \case
    "main-vertical"   -> Just MainVertical
    "main-horizontal" -> Just MainHorizontal
    "even-horizontal" -> Just EvenHorizontal
    "even-vertical"   -> Just EvenVertical
    "tiled"           -> Just Tiled
    _                 -> Nothing

applyNamedLayout :: ServerState -> Maybe Client -> LayoutName -> IO [Reply]
applyNamedLayout st mclient lname =
    withCurrentWindow st mclient $ \sess win ->
        arrangeNamed st sess win lname >> pure []

-- | The split ratio a named layout gives its main pane. @main-pane-width@ and
-- @main-pane-height@ are absolute cell counts (tmux semantics); expressed here
-- as a fraction of the window along the layout's axis, clamped so both the main
-- pane and the rest keep room. Non-main layouts split evenly.
mainPaneRatio :: LayoutName -> Options -> Size -> Rational
mainPaneRatio lname opts area = case lname of
    MainVertical   -> ratioOf opts.mainPaneWidth area.cols
    MainHorizontal -> ratioOf opts.mainPaneHeight area.rows
    _              -> 1 % 2
  where
    clampR r = max (1 % 10) (min (9 % 10) r) :: Rational
    ratioOf num den = clampR (toInteger num % max 1 (toInteger den))

-- | Rearrange one window into a named layout, sizing the @main-*@ pane
-- from @main-pane-width@/@-height@, and remember the name so
-- @next-layout@ can cycle onward from it.
arrangeNamed :: ServerState -> Session -> Window -> LayoutName -> IO ()
arrangeNamed st sess win lname = do
    eff <- readTVarIO sess.lastSize
    opts <- readTVarIO st.options
    let mainRatio = mainPaneRatio lname opts (eff)
    atomically $ do
        pids <- layoutPanes <$> readTVar win.layout
        unless (null pids) $ do
            writeTVar win.layout (namedLayout lname mainRatio pids)
            writeTVar win.layoutName (Just lname)
            writeTVar win.zoomed Nothing
            bumpDirty st
    applySessionSize st sess.id

-- | @next-layout@ (default @<prefix> Space@): rearrange the current
-- window into the next of tmux's five named layouts, cycling from the
-- last one applied. @previous-layout@ walks the cycle the other way.
cmdNextLayout :: CommandImpl
cmdNextLayout = cycleLayout nextLayoutName

cmdPreviousLayout :: CommandImpl
cmdPreviousLayout = cycleLayout previousLayoutName

cycleLayout :: (Maybe LayoutName -> LayoutName) -> CommandImpl
cycleLayout step st mclient _ =
    withCurrentWindow st mclient $ \sess win -> do
        cur <- readTVarIO win.layoutName
        arrangeNamed st sess win (step cur)
        pure []
