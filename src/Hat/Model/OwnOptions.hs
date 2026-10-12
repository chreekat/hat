-- | One scope's own options (a server, session, window or pane @set-option@
-- table) and the resolutions computed through it. The entries are written
-- only through 'modifyOwn', which advances the options epoch, so a
-- resolution cached at any scope is reused until some scope's options
-- change ('resolveThrough').
module Hat.Model.OwnOptions
    ( OwnOptions
    , ChainKey (..)
    , newOwnOptions
    , newOwnOptionsIO
    , readOwn
    , readOwnIO
    , modifyOwn
    , writeOwn
    , resolveThrough
    ) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.IO.Unsafe (unsafePerformIO)

import Hat.Model.Ids (SessionId, WindowId)
import Hat.Model.Options (Options, OptionsDelta, emptyDelta)

data OwnOptions = OwnOptions
    { entries  :: TVar OptionsDelta
    , resolved :: TVar Resolutions
    }

-- | How many times any scope's options have been written, process-wide: one
-- counter for every server, so no scope can count its writes apart from
-- the chains that read it.
optionsEpoch :: TVar Int
optionsEpoch = unsafePerformIO (newTVarIO 0)
{-# NOINLINE optionsEpoch #-}

-- | The chains resolved through a scope as of one options epoch.
data Resolutions = Resolutions
    { epoch   :: !Int
    , byChain :: !(Map ChainKey Options)
    }

-- | Which chain a resolution through a scope followed: the session and
-- window above it, where they take part.
data ChainKey = ChainKey (Maybe SessionId) (Maybe WindowId)
    deriving (Eq, Ord, Show)

newOwnOptions :: STM OwnOptions
newOwnOptions = OwnOptions <$> newTVar emptyDelta <*> newTVar (Resolutions (-1) Map.empty)

newOwnOptionsIO :: IO OwnOptions
newOwnOptionsIO = atomically newOwnOptions

readOwn :: OwnOptions -> STM OptionsDelta
readOwn o = readTVar o.entries

readOwnIO :: OwnOptions -> IO OptionsDelta
readOwnIO o = readTVarIO o.entries

modifyOwn :: OwnOptions -> (OptionsDelta -> OptionsDelta) -> STM ()
modifyOwn o f = do
    modifyTVar' o.entries f
    modifyTVar' optionsEpoch (+ 1)

writeOwn :: OwnOptions -> OptionsDelta -> STM ()
writeOwn o = modifyOwn o . const

-- | The options @resolve@ computes for a chain through this scope, computed
-- once per options epoch: every input to a resolution is some scope's
-- entries, and every write to those advances the epoch.
resolveThrough :: OwnOptions -> ChainKey -> STM Options -> STM Options
resolveThrough o key resolve = do
    now <- readTVar optionsEpoch
    r <- readTVar o.resolved
    let current = if r.epoch == now then r.byChain else Map.empty
    case Map.lookup key current of
        Just opts -> pure opts
        Nothing -> do
            opts <- resolve
            writeTVar o.resolved Resolutions { epoch = now, byChain = Map.insert key opts current }
            pure opts
