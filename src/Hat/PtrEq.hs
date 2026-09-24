{-# LANGUAGE MagicHash #-}

-- | Object identity as a cheap equality witness.
module Hat.PtrEq
    ( samePtr
    ) where

import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)

-- | True only for the very same heap object, which makes equality certain
-- without reading it. False proves nothing. Both sides are forced first:
-- pointer identity is only meaningful between evaluated values — an
-- unforced argument would compare as its own thunk and never match.
samePtr :: a -> a -> Bool
samePtr !a !b = isTrue# (reallyUnsafePtrEquality# a b)
