-- | Cell, style, and color types shared by the emulator, renderer, and
-- wire protocol.

-- Everything here has no business being lazy.
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Hat.Term.Cell
    ( Color (..)
    , Style (..)
    , Cell (..)
    , defaultStyle
    , blankCell
    ) where

import Codec.Serialise (Serialise)
import Data.Text (Text)
import Data.Word (Word8)
import GHC.Generics (Generic)

-- The 'Serialise' instances serve the era-gated reload handover payload only
-- (see 'Hat.Server.Reload'); the client wire keeps its own explicit-tag
-- encoding and never leans on this Generic derivation.
data Color
    = DefaultColor
    | Indexed Word8
    | RGB Word8 Word8 Word8
    deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (Serialise)

data Style = Style
    { fg        :: Color
    , bg        :: Color
    , bold      :: Bool
    , underline :: Bool
    , italic    :: Bool
    , reverse   :: Bool
    , strike    :: Bool
    , blink     :: Bool
    }
    deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (Serialise)

defaultStyle :: Style
defaultStyle = Style
    { fg = DefaultColor
    , bg = DefaultColor
    , bold = False
    , underline = False
    , italic = False
    , reverse = False
    , strike = False
    , blink = False
    }

-- | One grid cell. A wide character occupies one 'Cell' with @width = 2@;
-- the following grid column is a continuation and is skipped when drawing.
data Cell = Cell
    { text  :: Text
    , width :: Int
    , style :: Style
    }
    deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (Serialise)

blankCell :: Cell
blankCell = Cell { text = " ", width = 1, style = defaultStyle }
