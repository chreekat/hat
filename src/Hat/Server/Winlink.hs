{-# LANGUAGE RoleAnnotations #-}

-- | Window links: a session's window map entries (tmux's winlinks). A
-- window may be linked into several sessions, or several times into one;
-- a session group's members share one window map. These are the
-- primitives every command that links, unlinks, or renumbers windows is
-- built from, so the sessions' current windows and last-window stacks stay
-- valid however the maps change.
module Hat.Server.Winlink
    ( reselect
    , Step (..)
    , stepWindow
    , shuffleUp
    , Renumbered (..)
    , renumberSession
    , repackSession
    , renumberIfSet
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
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import Hat.Model
import Hat.Model.Options (Options (..), optionFlag)
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

-- | A relative window move: @next-window@, @previous-window@, or
-- @last-window@.
data Step = StepNext | StepPrevious | StepLast
    deriving (Eq, Show)

-- | The index a step selects from @cur@ among the session's window
-- indices (tmux's @session_next@\/@session_previous@\/@session_last@),
-- wrapping at the ends; with @alerted@, only windows carrying an alert
-- qualify. 'Nothing' when the step finds no window other than the current
-- one, which tmux reports as an error.
stepWindow :: Step -> Maybe (Set Int) -> [Int] -> Int -> [Int] -> Maybe Int
stepWindow step alerted ixs cur hist = case step of
    StepLast -> pick (take 1 hist)
    StepNext -> pick (pass after <> pass ixs)
    StepPrevious -> pick (pass (reverse before) <> pass (reverse ixs))
  where
    (before, after) = (filter (< cur) ixs, filter (> cur) ixs)
    pass = maybe (take 1) (\flagged -> take 1 . filter (`Set.member` flagged)) alerted
    pick cands = case take 1 cands of
        [ix] | ix /= cur -> Just ix
        _ -> Nothing

-- | tmux's @winlink_shuffle_up@: make room at @ix@ by moving the occupied
-- run starting there up by one. The renumberings run highest source first
-- (see 'Hat.Server.Command.Window.applyShifts').
shuffleUp :: Int -> Map.Map Int a -> [(Int, Int)]
shuffleUp ix ws = [(i, i + 1) | i <- [free - 1, free - 2 .. ix]]
  where
    free = until (`Map.notMember` ws) (+ 1) ix

-- | A session's window state after a renumber: the re-keyed window map,
-- the current window's new index, and the remapped last-window stack.
type role Renumbered representational
data Renumbered a = Renumbered
    { windows :: Map.Map Int a
    , current :: Int
    , history :: [Int]
    }
    deriving (Eq, Show)

-- | tmux's @session_renumber_windows@: the windows keep their order and
-- take consecutive indices from @base-index@, closing every gap. The
-- current window and the last-window stack follow their windows; a history
-- entry naming no window drops out rather than aliasing a renumbered one.
renumberSession :: Int -> [Int] -> Int -> Map.Map Int a -> Renumbered a
renumberSession cur hist base ws = Renumbered
    { windows = Map.fromList (zip fresh (Map.elems ws))
    , current = Map.findWithDefault cur cur remap
    , history = mapMaybe (`Map.lookup` remap) hist
    }
  where
    fresh = [base ..]
    remap = Map.fromList (zip (Map.keys ws) fresh)

-- | 'renumberSession' applied to @sess@ and every member of its group,
-- from @sess@'s @base-index@.
repackSession :: ServerState -> Session -> STM ()
repackSession st sess = do
    sopts <- resolveForSession st sess
    members <- groupMembers st sess
    forM_ members $ \s -> do
        ws <- readTVar s.windows
        cur <- readTVar s.currentIx
        hist <- readTVar s.windowHist
        let r = renumberSession cur hist sopts.baseIndex ws
        writeTVar s.windows r.windows
        writeTVar s.currentIx r.current
        writeTVar s.windowHist r.history
    bumpDirty st

-- | tmux's @server_renumber_session@: 'repackSession' when @sess@ has
-- @renumber-windows@ on.
renumberIfSet :: ServerState -> Session -> STM ()
renumberIfSet st sess = do
    sopts <- resolveForSession st sess
    when (optionFlag "renumber-windows" sopts) (repackSession st sess)

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
-- (tmux's @server_kill_window@), renumbering each survivor that asks for it
-- ('renumberIfSet'). Yields the sessions it emptied, already removed from
-- the server ('dropSessionGroup').
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
    let gone = List.nubBy (\a b -> a.id == b.id) (concat emptied)
        survivors = List.nubBy (\a b -> a.id == b.id)
            [ s | (s, _) <- links, s.id `notElem` map (.id) gone ]
    mapM_ (renumberIfSet st) survivors
    pure gone

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
