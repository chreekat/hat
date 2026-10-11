-- | Window links: a session's window map entries (tmux's winlinks). A
-- window may be linked into several sessions, or several times into one;
-- a session group's members share one window map. These are the
-- primitives every command that links, unlinks, or renumbers windows is
-- built from, so the sessions' current windows and last-window stacks stay
-- valid however the maps change.
module Hat.Server.Winlink
    ( reselect
    , shuffleUp
    , groupMembers
    , resyncGroup
    , windowLinks
    , attachWindow
    , detachWindow
    , unlinkEverywhere
    , dropSessionGroup
    ) where

import Control.Concurrent.STM
import Control.Monad (forM, forM_, when)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import Hat.Model
import Hat.Server.Mru (popOnClose)

-- | A session's current window and last-window stack once its window map
-- holds only the indices in @alive@ (tmux's @session_detach@): a surviving
-- current window stays; a vanished one gives way to the last window, else
-- the previous index (wrapping to the highest). 'Nothing' when nothing
-- survives.
reselect :: Set Int -> Int -> [Int] -> Maybe (Int, [Int])
reselect alive cur hist
    | Set.null alive = Nothing
    | Set.member cur alive = Just (cur, filter (\i -> ok i && i /= cur) hist)
    | otherwise = popOnClose ok (Just previous) hist
  where
    ok i = Set.member i alive
    previous = maybe (Set.findMax alive) id (Set.lookupLT cur alive)

-- | tmux's @winlink_shuffle_up@: make room at @ix@ by moving the occupied
-- run starting there up by one. The renumberings run highest source first
-- (see 'Hat.Server.Command.Window.applyShifts').
shuffleUp :: Int -> Map.Map Int a -> [(Int, Int)]
shuffleUp ix ws = [(i, i + 1) | i <- [free - 1, free - 2 .. ix]]
  where
    free = until (`Map.notMember` ws) (+ 1) ix

-- | The sessions sharing @sess@'s window map — its session group — with
-- @sess@ first; just @[sess]@ for an ungrouped session.
groupMembers :: ServerState -> Session -> STM [Session]
groupMembers st sess = do
    ss <- Map.elems <$> readTVar st.sessions
    pure (sess : [s | s <- ss, s.id /= sess.id, s.windows == sess.windows])

-- | Re-validate the current window and last-window stack of every session
-- sharing @sess@'s window map against what the map now holds ('reselect').
resyncGroup :: ServerState -> Session -> STM ()
resyncGroup st sess = do
    alive <- Map.keysSet <$> readTVar sess.windows
    members <- groupMembers st sess
    forM_ members $ \s -> do
        cur <- readTVar s.currentIx
        hist <- readTVar s.windowHist
        forM_ (reselect alive cur hist) $ \(cur', hist') -> do
            writeTVar s.currentIx cur'
            writeTVar s.windowHist hist'

-- | Every link of @win@: each session and index holding it. A grouped
-- window counts once per member, as tmux's window references do.
windowLinks :: ServerState -> Window -> STM [(Session, Int)]
windowLinks st win = do
    ss <- Map.elems <$> readTVar st.sessions
    concat <$> forM ss (\s -> do
        ws <- readTVar s.windows
        pure [(s, ix) | (ix, w) <- Map.toList ws, w.id == win.id])

-- | Link @win@ into @sess@ at @ix@ (tmux's @session_attach@).
attachWindow :: ServerState -> Session -> Int -> Window -> STM (Either Text ())
attachWindow st sess ix win = do
    ws <- readTVar sess.windows
    if Map.member ix ws
        then pure (Left ("index in use: " <> tshow ix))
        else do
            writeTVar sess.windows (Map.insert ix win ws)
            bumpDirty st
            pure (Right ())

-- | Unlink index @ix@ from @sess@ (tmux's @session_detach@), reselecting
-- every group member's current window. 'True' when the map is left empty:
-- the caller must then destroy the group ('dropSessionGroup').
detachWindow :: ServerState -> Session -> Int -> STM Bool
detachWindow st sess ix = do
    modifyTVar' sess.windows (Map.delete ix)
    resyncGroup st sess
    bumpDirty st
    Map.null <$> readTVar sess.windows

-- | Unlink @win@ from every session holding it, as its destruction does
-- (tmux's @server_kill_window@). Yields the sessions it emptied, already
-- removed from the server ('dropSessionGroup').
unlinkEverywhere :: ServerState -> Window -> STM [Session]
unlinkEverywhere st win = do
    links <- windowLinks st win
    emptied <- forM links $ \(s, ix) -> do
        ws <- readTVar s.windows
        -- A group member's link may already be gone with an earlier one.
        case Map.lookup ix ws of
            Just w | w.id == win.id -> do
                empty <- detachWindow st s ix
                if empty then dropSessionGroup st s else pure []
            _ -> pure []
    pure (List.nubBy (\a b -> a.id == b.id) (concat emptied))

-- | Remove @sess@ and every session sharing its window map from the server
-- (tmux's @server_destroy_session_group@), returning the removed sessions.
dropSessionGroup :: ServerState -> Session -> STM [Session]
dropSessionGroup st sess = do
    members <- groupMembers st sess
    live <- readTVar st.sessions
    let gone = [s | s <- members, Map.member s.id live]
    when (not (null gone)) $ do
        writeTVar st.sessions (List.foldl' (flip (Map.delete . (.id))) live gone)
        bumpDirty st
    pure gone
