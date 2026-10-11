{-# LANGUAGE StrictData #-}

-- | The emulator's grid as libghostty-vt records it, cell for cell, for
-- @capture-pane -R@'s debugging dump. Unlike 'Hat.Term.Cell.Cell' nothing is
-- folded to what the renderer needs, and nothing here crosses a process or
-- version boundary: it is read, formatted, and dropped within one command.
module Hat.Term.RawGrid
    ( RawGrid (..)
    , RawLine (..)
    , RawCell (..)
    , RawWidth (..)
    , RawColor (..)
    , Underline (..)
    , RawAttrs (..)
    , noAttrs
    , blankRawCell
    ) where

import Data.Text (Text)
import Data.Word (Word8)

data RawGrid = RawGrid
    { cols   :: Int
    , rows   :: Int
    , hsize  :: Int
    , hlimit :: Int
    , lines  :: [RawLine]  -- ^ history (oldest first) then the screen
    }
    deriving stock (Eq, Show)

data RawLine = RawLine
    { wrapped :: Bool
    , prompt  :: Bool  -- ^ an OSC 133 primary prompt line
    , cells   :: [RawCell]
    }
    deriving stock (Eq, Show)

data RawWidth
    = RawNarrow
    | RawWide
    | SpacerTail  -- ^ the column behind a wide glyph
    | SpacerHead  -- ^ the last column left empty when a wide glyph wrapped
    deriving stock (Eq, Show)

data RawColor = RawDefault | RawPalette Word8 | RawRgb Word8 Word8 Word8
    deriving stock (Eq, Show)

data Underline = NoUnderline | Single | Double | Curly | Dotted | Dashed
    deriving stock (Eq, Show, Enum, Bounded)

data RawAttrs = RawAttrs
    { bold, faint, italic, blink, inverse, invisible, strike, overline :: Bool
    , underline :: Underline
    }
    deriving stock (Eq, Show)

noAttrs :: RawAttrs
noAttrs = RawAttrs False False False False False False False False NoUnderline

data RawCell = RawCell
    { text    :: [Char]  -- ^ the grapheme cluster; empty for a blank cell
    , width   :: RawWidth
    , bgOnly  :: Bool
        -- ^ a blank holding only a background, which libghostty writes only
        --   when an erase runs under a colored background
    , attrs   :: RawAttrs
    , fg, bg, us :: RawColor
    , link    :: Maybe Text  -- ^ the OSC 8 hyperlink URI
    }
    deriving stock (Eq, Show)

blankRawCell :: RawCell
blankRawCell = RawCell
    { text = [], width = RawNarrow, bgOnly = False, attrs = noAttrs
    , fg = RawDefault, bg = RawDefault, us = RawDefault, link = Nothing }
