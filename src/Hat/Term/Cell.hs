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
    , encodeStyleAt
    ) where

import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (decodeListLen, decodeWord)
import Codec.Serialise.Encoding (Encoding, encodeListLen, encodeWord)
import Data.Text qualified as T
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)

-- The 'Serialise' instances serve two version-gated boundaries: the era-gated
-- reload handover payload ('Hat.Server.Reload') and the leaf types the client
-- wire embeds inside a 'Hat.Transport.Wire.DrawOp'. 'Color' and 'Cell' derive
-- theirs; 'Style' is hand-written so a trailing style attribute can be appended
-- additively (a short list defaults it) rather than orphaning an old payload.
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
    , faint     :: Bool  -- ^ SGR 2, appended last: see the 'Serialise' instance
    }
    deriving stock (Eq, Show, Ord, Generic)

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
    , faint = False
    }

-- | 'encode' emits the current full form; 'decode' reads every dialect ≤ ours,
-- defaulting fields a shorter list omits. Older peers are written via
-- 'encodeStyleAt'.
instance Serialise Style where
    encode = encodeStyleAt maxBound
    decode = do
        len <- decodeListLen
        _   <- decodeWord
        fg'        <- decode
        bg'        <- decode
        bold'      <- decode
        underline' <- decode
        italic'    <- decode
        reverse'   <- decode
        strike'    <- decode
        blink'     <- decode
        faint'     <- if len >= 10 then decode else pure False
        pure Style
            { fg = fg', bg = bg', bold = bold', underline = underline'
            , italic = italic', reverse = reverse', strike = strike'
            , blink = blink', faint = faint' }

-- | Encode for a peer at wire dialect @lvl@: ≤ 4 gets the nine-element
-- pre-faint list, ≥ 5 the full ten. Each level's bytes are frozen forever
-- (dialect corpus in @WireSpec@).
encodeStyleAt :: Word16 -> Style -> Encoding
encodeStyleAt lvl s =
       encodeListLen (if hasFaint then 10 else 9)
    <> encodeWord 0
    <> encode s.fg
    <> encode s.bg
    <> encode s.bold
    <> encode s.underline
    <> encode s.italic
    <> encode s.reverse
    <> encode s.strike
    <> encode s.blink
    <> (if hasFaint then encode s.faint else mempty)
  where
    hasFaint = lvl >= 5

-- | One grid cell. A wide character occupies one 'Cell' with @width = 2@;
-- the following grid column is a continuation ('width' 0, 'char' a
-- don't-care @' '@) and is skipped when drawing.
data Cell = Cell
    { char  :: Char
    , width :: Int
    , style :: Style
    }
    deriving stock (Eq, Show, Ord, Generic)

-- | Byte-compatible with the Text-field encoding every era wrote: the char
-- rides as a string, empty for a continuation cell; decode keeps a string's
-- first char and defaults the rest.
instance Serialise Cell where
    encode c =
           encodeListLen 4
        <> encodeWord 0
        <> encode (if c.width == 0 then T.empty else T.singleton c.char)
        <> encode c.width
        <> encode c.style
    decode = do
        _   <- decodeListLen
        _   <- decodeWord
        txt <- decode
        w   <- decode
        s   <- decode
        pure Cell
            { char = maybe ' ' fst (T.uncons txt)
            , width = w
            , style = s
            }

blankCell :: Cell
blankCell = Cell { char = ' ', width = 1, style = defaultStyle }
