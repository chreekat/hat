-- | Pane scrollbars drawn over a pane's own content (@pane-scrollbars modal@
-- and @auto-hide@): which panes show one, and the reveal that mouse hover
-- and copy-mode scrolling trigger, hidden again @pane-scrollbars-timeout@
-- later (tmux's @window_pane_scrollbar_show@ and its timer).
module Hat.Server.Scrollbars
    ( ScrollbarsMode (..)
    , scrollbarsModeOf
    , PaneScreen (..)
    , showsScrollbar
    , overlaysScrollbar
    , hidesOnTimer
    , scrollbarSideOf
    , inScrollbarArea
    , coversColumn
    , scrollShowsScrollbar
    , paneOverlays
    , revealScrollbar
    , hoverScrollbars
    , hideScrollbars
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM, forM_, void, when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

import Hat.Geometry (Rect (..))
import Hat.Model
import Hat.Model.Options (Options (..), optionNumber)
import Hat.Server.Locate (clientView)
import Hat.Server.Resize (windowArrange)
import Hat.Server.Scene (ScrollbarSide (..), ScrollbarStyle (..), scrollbarStyle)
import Hat.Server.Theme (terminalTheme)

-- | @pane-scrollbars@.
data ScrollbarsMode = ScrollbarsOff | ScrollbarsModal | ScrollbarsOn | ScrollbarsAutoHide
    deriving (Eq, Show)

scrollbarsModeOf :: Text -> ScrollbarsMode
scrollbarsModeOf = \case
    "modal" -> ScrollbarsModal
    "on" -> ScrollbarsOn
    "auto-hide" -> ScrollbarsAutoHide
    _ -> ScrollbarsOff

-- | What a pane's screen shows, as its scrollbar sees it.
data PaneScreen = PaneScreen
    { alternate :: Bool  -- ^ the program's alternate screen
    , inMode :: Bool     -- ^ a mode (copy or view) over the program
    }
    deriving (Eq, Show)

-- | Whether a pane has a scrollbar at all (tmux's
-- @window_pane_show_scrollbar@).
showsScrollbar :: ScrollbarsMode -> PaneScreen -> Bool
showsScrollbar mode scr
    | scr.alternate = False
    | otherwise = case mode of
        ScrollbarsOn -> True
        ScrollbarsAutoHide -> True
        ScrollbarsModal -> scr.inMode
        ScrollbarsOff -> False

-- | Whether a mode's scrollbars draw over their panes and show only once
-- revealed (tmux's @window_pane_scrollbar_auto_hide@).
hidesOnTimer :: ScrollbarsMode -> Bool
hidesOnTimer mode = mode == ScrollbarsModal || mode == ScrollbarsAutoHide

-- | Whether a pane's scrollbar is one drawn over it (tmux's
-- @window_pane_scrollbar_overlay@).
overlaysScrollbar :: ScrollbarsMode -> PaneScreen -> Bool
overlaysScrollbar mode scr = hidesOnTimer mode && showsScrollbar mode scr

-- | @pane-scrollbars-position@.
scrollbarSideOf :: Options -> ScrollbarSide
scrollbarSideOf o = if o.paneScrollbarsPosition == "left" then ScrollbarLeft else ScrollbarRight

-- | Whether a pane column is under an overlay scrollbar @w@ wide on an
-- @sx@-column pane, its padding aside.
coversColumn :: ScrollbarSide -> Int -> Int -> Int -> Bool
coversColumn side w sx x = case side of
    ScrollbarLeft -> x < bar
    ScrollbarRight -> x >= sx - bar
  where
    bar = min w sx

-- | Whether a window point is where hovering reveals a pane's overlay
-- scrollbar (tmux's @server_client_in_scrollbar_area@): the columns its
-- style's @(width, pad)@ takes at the pane's edge, or the whole pane when
-- they do not fit.
inScrollbarArea :: ScrollbarSide -> (Int, Int) -> Rect -> (Int, Int) -> Bool
inScrollbarArea side (w, pad) r (px, py) =
    py >= r.startRow && py < r.endRow && px >= start && px < start + total
  where
    sx = r.endCol - r.startCol
    total = if w + pad == 0 || w + pad > sx then sx else w + pad
    start = case side of
        ScrollbarLeft -> r.startCol
        ScrollbarRight -> r.endCol - total

-- | Whether a copy-mode command reveals the scrollbar, given the viewport's
-- offset before and after: a page scroll always, anything else that moved
-- it (tmux's @window_copy_pageup@, @window_copy_scroll_up@ and the rest).
scrollShowsScrollbar :: Text -> Int -> Int -> Bool
scrollShowsScrollbar name before after = before /= after || name `elem` pages
  where
    pages =
        [ "page-up", "page-down", "page-down-and-cancel"
        , "halfpage-up", "halfpage-down", "halfpage-down-and-cancel" ]

paneScreen :: Pane -> STM PaneScreen
paneScreen p = PaneScreen <$> readTVar p.onAlternate <*> paneInMode p

-- | Each pane of a window showing its overlay scrollbar, with its style's
-- width and padding.
paneOverlays :: ServerState -> Window -> STM (Map PaneId (Int, Int))
paneOverlays st win = do
    wopts <- resolveWindowTables st win
    let mode = scrollbarsModeOf wopts.paneScrollbars
    if not (hidesOnTimer mode) then pure Map.empty else do
        ps <- readTVar win.panes
        fmap (Map.fromList . catMaybes) . forM (Map.elems ps) $ \p -> do
            scr <- paneScreen p
            shown <- isJust <$> readTVar p.scrollbarShown
            if overlaysScrollbar mode scr && shown
                then do
                    popts <- resolvePaneTables st win p
                    let sb = scrollbarStyle terminalTheme popts.paneScrollbarsStyle
                    pure (Just (p.id, (sb.width, sb.pad)))
                else pure Nothing

-- | Reveal a pane's overlay scrollbar for @pane-scrollbars-timeout@ from
-- now (tmux's @window_pane_scrollbar_show@ with its timer).
revealScrollbar :: ServerState -> Pane -> IO ()
revealScrollbar st pane = do
    mhome <- readTVarIO pane.home  -- See Note [Pane home]
    forM_ mhome $ \(_, win) -> do
        (wopts, scr) <- atomically $ (,) <$> resolveWindowTables st win <*> paneScreen pane
        when (overlaysScrollbar (scrollbarsModeOf wopts.paneScrollbars) scr) $ do
            deadline <- deadlineAfter wopts
            fresh <- atomically $ do
                old <- readTVar pane.scrollbarShown
                writeTVar pane.scrollbarShown (Just deadline)
                when (isNothing old) (bumpDirty st)
                pure (isNothing old)
            when fresh $ void (forkIO (expireScrollbar st pane))

-- | Push a shown overlay scrollbar's hiding back to the full timeout (tmux's
-- @window_pane_scrollbar_start_timer@).
restartScrollbarTimer :: ServerState -> Window -> Pane -> IO ()
restartScrollbarTimer st win pane = do
    wopts <- atomically (resolveWindowTables st win)
    when (hidesOnTimer (scrollbarsModeOf wopts.paneScrollbars)) $ do
        deadline <- deadlineAfter wopts
        atomically $ readTVar pane.scrollbarShown >>= \case
            Just _ -> writeTVar pane.scrollbarShown (Just deadline)
            Nothing -> pure ()

deadlineAfter :: Options -> IO Word64
deadlineAfter wopts = do
    now <- getMonotonicTimeNSec
    pure (now + fromIntegral (optionNumber "pane-scrollbars-timeout" wopts) * 1000000)

-- | Hide a revealed scrollbar once its deadline passes, following it as
-- reveals push it back.
expireScrollbar :: ServerState -> Pane -> IO ()
expireScrollbar st pane = do
    now <- getMonotonicTimeNSec
    next <- atomically $ readTVar pane.scrollbarShown >>= \case
        Just d | d <= now -> Nothing <$ (writeTVar pane.scrollbarShown Nothing >> bumpDirty st)
        other -> pure other
    forM_ next $ \d -> do
        -- round up so the wakeup lands past the deadline
        threadDelay (fromIntegral ((d - now + 999) `div` 1000))
        expireScrollbar st pane

-- | A mouse move over a client's window (tmux's
-- @server_client_update_scrollbar_hover@): each visible pane's overlay
-- scrollbar is revealed while the mouse is in its area, and otherwise keeps
-- the full timeout from now. 'Nothing' is a move off the window area.
hoverScrollbars :: ServerState -> Client -> Maybe (Int, Int) -> IO ()
hoverScrollbars st client mpoint = do
    mv <- atomically (clientView st client)
    forM_ mv $ \(_, win) -> do
        (panes, wopts) <- atomically $ do
            eff <- readTVar win.size
            (rects, _) <- windowArrange st eff win
            ps <- readTVar win.panes
            wopts <- resolveWindowTables st win
            panes <- fmap catMaybes . forM rects $ \(pid, r) -> case Map.lookup pid ps of
                Nothing -> pure Nothing
                Just p -> do
                    scr <- paneScreen p
                    popts <- resolvePaneTables st win p
                    pure (Just (p, r, scr, popts))
            pure (panes, wopts)
        let mode = scrollbarsModeOf wopts.paneScrollbars
            side = scrollbarSideOf wopts
        forM_ panes $ \(p, r, scr, popts) -> do
            let sb = scrollbarStyle terminalTheme popts.paneScrollbarsStyle
                hovered = overlaysScrollbar mode scr
                    && maybe False (inScrollbarArea side (sb.width, sb.pad) r) mpoint
            if hovered then revealScrollbar st p else restartScrollbarTimer st win p

-- | Hide every pane's overlay scrollbar, as setting @pane-scrollbars@ does.
hideScrollbars :: ServerState -> IO ()
hideScrollbars st = atomically $ do
    sessions <- readTVar st.sessions
    forM_ sessions $ \sess -> do
        wins <- readTVar sess.windows
        forM_ wins $ \win -> do
            ps <- readTVar win.panes
            forM_ ps $ \p -> writeTVar p.scrollbarShown Nothing
    bumpDirty st
