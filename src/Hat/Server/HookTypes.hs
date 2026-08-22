-- | State carried by the hooks engine: bindings, monitors, wait-for
-- channels, and the ambient context hook commands run under. The engine
-- itself lives in "Hat.Server.Hooks"; this module only defines the state so
-- 'Hat.Model.ServerState' can hold it without a module cycle.
module Hat.Server.HookTypes
    ( HookScope (..)
    , HookEntry (..)
    , MonitorTarget (..)
    , MonitorKey (..)
    , Monitor (..)
    , Waiter (..)
    , Channel (..)
    , emptyChannel
    , EventWaiter (..)
    , HookAmbient (..)
    , HooksState (..)
    , newHooksState
    ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)

import Hat.Model.Ids

-- | Where a hook binding lives. Lookup order on notify is session, global,
-- pane, window — see 'Hat.Server.Hooks.notify'.
data HookScope
    = HookGlobal
    | HookSession SessionId
    | HookWindow WindowId
    | HookPane PaneId
    deriving (Eq, Ord, Show)

-- | One bound hook: its command strings (one per array item) and fire
-- bookkeeping for @show-hooks -F@.
data HookEntry = HookEntry
    { commands :: [Text]
    , fireCount :: Int
    , fireTime :: Maybe POSIXTime
    }
    deriving (Eq, Show)

-- | What a @set-hook -B@ monitor samples its format against.
data MonitorTarget
    = MonSession
    | MonPane Int
    | MonAllPanes
    | MonWindow Int
    | MonAllWindows
    deriving (Eq, Show)

-- | Which sampled context a per-context last value belongs to.
data MonitorKey = KeySession | KeyPane Int | KeyWindow Int
    deriving (Eq, Ord, Show)

-- | A @set-hook -B@ format monitor. 'session' pins the sampling context;
-- 'Nothing' (a global monitor) follows the alphabetically-first session.
data Monitor = Monitor
    { format :: Text
    , target :: MonitorTarget
    , session :: Maybe SessionId
    , lastValues :: TVar (Map MonitorKey Text)
    , fireCount :: TVar Int
    , fireTime :: TVar (Maybe POSIXTime)
    }

-- | A client blocked on a wait-for channel.
data Waiter = Waiter
    { clientName :: Text
    , wake :: TMVar ()
    }

-- | One wait-for channel: its signal/lock state plus blocked clients.
data Channel = Channel
    { woken :: Bool
    , locked :: Bool
    , waiters :: [Waiter]
    , lockers :: [Waiter]
    }

emptyChannel :: Channel
emptyChannel = Channel
    { woken = False, locked = False, waiters = [], lockers = [] }

-- | A client blocked on @wait-for -E@ for a named event. Waking delivers
-- the payload lines a @-v@ waiter prints.
data EventWaiter = EventWaiter
    { name :: Text
    , clientName :: Text
    , filter :: Maybe Text
    , wake :: TMVar [Text]
    }

-- | The context hook commands run under, keyed by the running thread:
-- the event's payload formats and its target. Its presence also marks the
-- thread as inside a hook, which suppresses further hook firing.
data HookAmbient = HookAmbient
    { formats :: Map Text Text
    , targetSession :: Maybe SessionId
    , targetWindow :: Maybe WindowId
    , targetPane :: Maybe PaneId
    }

data HooksState = HooksState
    { table :: TVar (Map HookScope (Map Text HookEntry))
    , events :: TVar (Set Text)
        -- ^ user @-hook names registered as events by set-hook
    , monitors :: TVar (Map (HookScope, Text) Monitor)
    , channels :: TVar (Map Text Channel)
    , eventWaiters :: TVar [EventWaiter]
    , ambient :: TVar (Map ThreadId HookAmbient)
    , runner :: TVar (Text -> IO ())
        -- ^ runs one hook command line; wired to the command engine at
        -- server startup (see 'Hat.Server.Hooks.runHookCommands')
    , expander :: TVar (Map Text Text -> Text -> IO Text)
        -- ^ expands one hook command against a format env before it is
        -- parsed; wired alongside 'runner'
    , knownCommands :: TVar (Set Text)
        -- ^ canonical command names, for validating @after-*@ hook names;
        -- wired alongside 'runner'
    }

newHooksState :: IO HooksState
newHooksState = HooksState
    <$> newTVarIO Map.empty
    <*> newTVarIO Set.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO []
    <*> newTVarIO Map.empty
    <*> newTVarIO (const (pure ()))
    <*> newTVarIO (\_ t -> pure t)
    <*> newTVarIO Set.empty
