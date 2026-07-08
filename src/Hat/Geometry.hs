module Hat.Geometry
    ( Size (..)
    ) where

import Data.Word (Word16)

data Size = Size
    { rows :: Word16
    , cols :: Word16
    }
    deriving (Eq, Ord, Show)
