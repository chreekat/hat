-- | Value interning
module Hat.Intern
    ( shareVals
    ) where

import Data.IORef (IORef, modifyIORef', readIORef)
import Data.Map (Map)
import Data.Map qualified as Map
import System.Mem.Weak (Weak)
import System.Mem.Weak qualified as Weak

-- | Given a value we want to share, return the shared/interned version of that
-- value.
--
-- E.g. if two actions produce different instances of a, and a1==a2, this lets
-- us GC one of them. This can save a lot of space if we are creating many
-- instances of a that are all equal to each other!
shareVals :: Ord a => IORef (Map a (Weak a)) -> a -> IO a
shareVals intern newA = do
    m <- readIORef intern
    case Map.lookup newA m of
        -- We still have a Weak ptr keyed by newA
        Just weak -> do
            mv <- Weak.deRefWeak weak
            case mv of
                -- We still have the interned value! Return that.
                -- Assuming nothing else is holding on to newA, it is now free
                -- to be GCd, and we're left with just one copy of this value:
                -- the old one.
                Just oldV -> pure oldV
                Nothing -> insert
        Nothing -> insert
  where
    -- We never put this in the map, or we've caught it with its pants down (the
    -- key is dead, but the finalizer hasn't fired yet). Insert or replace the
    -- value.
    insert = do
        weak <- Weak.mkWeak newA newA (Just (modifyIORef' intern (Map.delete newA)))
        newA <$ modifyIORef' intern (Map.insert newA weak)
