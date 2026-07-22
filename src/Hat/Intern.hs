-- | Value interning: collapse equal values to one shared heap representative,
-- so many equal copies cost a single allocation.
module Hat.Intern
    ( shareVals
    ) where

import Data.IORef (IORef, modifyIORef', readIORef)
import Data.Map (Map)
import Data.Map qualified as Map
import System.Mem.Weak (Weak)
import System.Mem.Weak qualified as Weak

-- | Return the interned representative of @x@: the first value passed through
-- the same table that was '==' to @x@, so all equal values collapse to one
-- shared heap object. On a miss (or once a previous representative has been
-- collected), @x@ itself becomes the representative. Each table entry refers
-- to its value through a 'Weak' whose finalizer drops the entry.
shareVals :: Ord a => IORef (Map a (Weak a)) -> a -> IO a
shareVals intern x = do
    m <- readIORef intern
    case Map.lookup x m of
        Just weak -> do
            mv <- Weak.deRefWeak weak
            case mv of
                Just v -> pure v
                Nothing -> insert
        Nothing -> insert
  where
    insert = do
        weak <- Weak.mkWeak x x (Just (modifyIORef' intern (Map.delete x)))
        x <$ modifyIORef' intern (Map.insert x weak)
