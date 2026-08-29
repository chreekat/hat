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
    , Content (..)
    , Width (..)
    , defaultStyle
    , blankCell
    , glyphCell
    , cellWidth
    , cluster
    , baseChar
    , encodeStyleAt
    ) where

import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (Decoder, decodeListLen, decodeWord)
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

-- | One grid cell.
data Cell = Cell
    { content :: Content
    , style   :: Style
    }
    deriving stock (Eq, Show, Ord, Generic)

-- | What a cell holds: the spacer column behind a wide glyph, or a glyph —
-- its base char, the combining codepoints completing the grapheme cluster,
-- and how many columns it spans.
data Content
    = Continuation
    | Glyph Char [Char] Width
    deriving stock (Eq, Show, Ord, Generic)

data Width = Narrow | Wide
    deriving stock (Eq, Show, Ord, Generic)

-- | Columns a cell occupies: 0 for a continuation, 1 or 2 for a glyph.
cellWidth :: Cell -> Int
cellWidth c = case c.content of
    Continuation     -> 0
    Glyph _ _ Narrow -> 1
    Glyph _ _ Wide   -> 2

-- | The cell's full grapheme cluster; empty for a continuation.
cluster :: Cell -> [Char]
cluster c = case c.content of
    Continuation   -> []
    Glyph ch mks _ -> ch : mks

-- | The char shown at the cell's column; @' '@ for a continuation.
baseChar :: Cell -> Char
baseChar c = case c.content of
    Continuation  -> ' '
    Glyph ch _ _  -> ch

-- | Byte-compatible with the field encoding every era wrote: the whole
-- cluster rides as one string (empty for a continuation) beside a 0\/1\/2
-- column count; decode rebuilds the 'Content' from the pair.
instance Serialise Cell where
    encode c =
           encodeListLen 4
        <> encodeWord 0
        <> encode (T.pack (cluster c))
        <> encode (cellWidth c)
        <> encode c.style
    decode = do
        _   <- decodeListLen
        _   <- decodeWord
        txt <- decode
        w   <- decode :: Decoder s Int
        s   <- decode
        let wd = if w >= 2 then Wide else Narrow
            ct = case T.unpack txt of
                _ | w == 0 -> Continuation
                []         -> Glyph ' ' [] wd
                ch : mks   -> Glyph ch mks wd
        pure Cell { content = ct, style = s }

blankCell :: Cell
blankCell = glyphCell ' ' defaultStyle

-- | A narrow, markless glyph cell.
glyphCell :: Char -> Style -> Cell
glyphCell ch sty = Cell { content = Glyph ch [] Narrow, style = sty }
