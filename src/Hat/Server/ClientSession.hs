{-# LANGUAGE RoleAnnotations #-}

-- | Which session an attached client shows: moving it to another, and
-- where its clients go when a session is destroyed under them.
module Hat.Server.ClientSession
    ( switchClientTo
    , DetachOnDestroy (..)
    , detachOnDestroy
    , SessionChoice (..)
    , destroyDestination
    , sendClientsAway
    ) where

import Control.Concurrent.STM
import Control.Monad (forM, forM_, when)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Hat.Model
import Hat.Model.Options (optionText)
import Hat.Server.ClientIO (send)
import Hat.Server.Hooks
    (PayloadItem (..), notifyClient)
import Hat.Server.Mru (recordVisit)
import Hat.Server.Lock (armLockTimer)
import Hat.Server.Resize (applySessionSize)
import Hat.Transport.Wire (ServerToClient (Exited))

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
        mwin <- currentWindow sess
        forM_ mwin $ \w -> modifyTVar' st.windowLatest (Map.insert w.id client.id)
        writeTVar client.needsFull True
        bumpDirty st
    applySessionSize st old
    applySessionSize st sess.id
    armLockTimer st sess.id
    when (old /= sess.id) $ do
        sname <- readTVarIO sess.name
        mold <- atomically (findSessionById st old)
        oldItem <- forM mold $ \o -> ("old_session",) . PSessionRef o.id <$> readTVarIO o.name
        notifyClient st "client-session-changed" client $
            ("new_session", PSessionRef sess.id sname) : maybe [] pure oldItem

-- | @detach-on-destroy@: where a destroyed session's clients go.
data DetachOnDestroy
    = DestroyDetach        -- ^ @on@: they detach
    | DestroySwitch        -- ^ @off@: to the most recently active session
    | DestroyNoDetached    -- ^ @no-detached@: to the most recently active unattached one
    | DestroyPrevious      -- ^ @previous@: to the session before it by name
    | DestroyNext          -- ^ @next@: to the session after it by name
    deriving (Eq, Show)

detachOnDestroy :: Text -> DetachOnDestroy
detachOnDestroy = \case
    "off" -> DestroySwitch
    "no-detached" -> DestroyNoDetached
    "previous" -> DestroyPrevious
    "next" -> DestroyNext
    _ -> DestroyDetach

-- | A surviving session as 'destroyDestination' weighs it.
type role SessionChoice representational
data SessionChoice a = SessionChoice
    { session  :: a
    , name     :: Text
    , activity :: Int   -- ^ 'Hat.Model.Session'\'s activity stamp
    , attached :: Bool  -- ^ whether any client is attached to it
    }

-- | tmux's @server_destroy_session@ choice: the session the clients of the
-- destroyed session @dying@ move to, among the @survivors@, or 'Nothing' to
-- detach them. Activity ties go to the first name, as tmux's name-ordered
-- scan keeps the first newest; @previous@ and @next@ wrap around the names.
destroyDestination :: DetachOnDestroy -> Text -> [SessionChoice a] -> Maybe a
destroyDestination mode dying survivors = (.session) <$> case mode of
    DestroyDetach -> Nothing
    DestroySwitch -> newest byName
    DestroyNoDetached -> newest (filter (not . (.attached)) byName)
    DestroyPrevious -> case filter ((< dying) . (.name)) byName of
        [] -> lastOf byName
        before -> lastOf before
    DestroyNext -> case filter ((> dying) . (.name)) byName of
        [] -> firstOf byName
        after -> firstOf after
  where
    byName = List.sortOn (.name) survivors
    newest = List.foldl' (\best c -> case best of
        Just b | b.activity >= c.activity -> best
        _ -> Just c) Nothing
    firstOf = \case
        [] -> Nothing
        c : _ -> Just c
    lastOf = firstOf . reverse

-- | Send away the clients of sessions already destroyed: each attached
-- client moves to the session its dying session's @detach-on-destroy@
-- picks ('destroyDestination'), or exits; any other connection exits.
sendClientsAway :: ServerState -> [Session] -> IO ()
sendClientsAway st gone = forM_ gone $ \dying -> do
    (dest, cs) <- atomically $ do
        o <- resolveForSession st dying
        dname <- readTVar dying.name
        live <- Map.elems <$> readTVar st.sessions
        choices <- forM live $ \s -> do
            nm <- readTVar s.name
            act <- readTVar s.activity
            viewers <- filter (\c -> c.role == Attached)
                <$> sessionClients st s.id
            pure SessionChoice
                { session = s, name = nm, activity = act
                , attached = not (null viewers) }
        cs <- sessionClients st dying.id
        pure (destroyDestination
            (detachOnDestroy (optionText "detach-on-destroy" o)) dname choices, cs)
    forM_ cs $ \c -> case dest of
        Just s | c.role == Attached -> switchClientTo st c s
        _ -> send c Exited
