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

import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (decodeListLen, decodeWord)
import Codec.Serialise.Encoding (encodeListLen, encodeWord)
import Data.Text (Text)
import Data.Word (Word8)
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

-- | Encode 'Style' as @[tag, fg, bg, bold, underline, italic, reverse, strike,
-- blink, faint]@, mirroring the Generic layout the other leaf types keep but
-- with 'faint' appended. A pre-faint payload is a nine-element list; the decoder
-- defaults 'faint' off when the list is short, so a new build reads an old
-- reload/wire payload additively — the point being that this ships WITHOUT a
-- 'Hat.Transport.Wire.protocolVersion' bump (which would break 'restart-server'
-- across the upgrade; see the note there). A reader that genuinely can't handle
-- the longer list rejects it cleanly (CBOR arity mismatch → 'Malformed'/'Left'),
-- never misreads. The reload handover still bumps
-- 'Hat.Server.Reload.reloadEra' for an accurate newer-than-me signal on
-- rollback, but decodes old and new payloads with this one lenient reader.
instance Serialise Style where
    encode s =
           encodeListLen 10
        <> encodeWord 0
        <> encode s.fg
        <> encode s.bg
        <> encode s.bold
        <> encode s.underline
        <> encode s.italic
        <> encode s.reverse
        <> encode s.strike
        <> encode s.blink
        <> encode s.faint
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
