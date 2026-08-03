module Hat.Geometry
    ( Size (..)
    , Pos (..)
    , Rect (..)
    ) where

import Data.Word (Word16)

data Size = Size
    { rows :: Word16
    , cols :: Word16
    }
    deriving (Eq, Ord, Show)

data Pos = Pos
    { row :: Int
    , col :: Int
    }
    deriving (Eq, Ord, Show)

-- | Half-open on both ends: rows in [startRow, endRow), cols in
-- [startCol, endCol).
data Rect = Rect
    { startRow :: Int
    , endRow   :: Int
    , startCol :: Int
    , endCol   :: Int
    }
    deriving (Eq, Show)
