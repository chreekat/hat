-- | Display-column widths for characters and text: a pure @wcwidth@
-- approximation. Standalone (no @Hat.Server@ deps) — the format engine
-- uses it for width-aware truncation, padding, and fuzzy-match columns.
module Hat.TextWidth
    ( charWidth
    , textWidth
    ) where

import Data.Char (GeneralCategory (..), generalCategory, ord)
import Data.Text (Text)
import Data.Text qualified as T

-- | Columns a character occupies in a terminal: 0 for controls, combining
-- marks and zero-width characters; 2 for East Asian wide\/fullwidth and
-- emoji; 1 otherwise.
charWidth :: Char -> Int
charWidth c
    | n < 0x20 || n == 0x7f = 0
    | n < 0x7f = 1
    | generalCategory c `elem` [NonSpacingMark, EnclosingMark, Format] = 0
    | any (\(lo, hi) -> n >= lo && n <= hi) wideRanges = 2
    | otherwise = 1
  where
    n = ord c

textWidth :: Text -> Int
textWidth = T.foldl' (\acc c -> acc + charWidth c) 0

-- East Asian Wide/Fullwidth blocks plus emoji-presentation blocks.
wideRanges :: [(Int, Int)]
wideRanges =
    [ (0x1100, 0x115F)    -- Hangul Jamo
    , (0x2329, 0x232A)
    , (0x2E80, 0x303E)    -- CJK radicals, punctuation
    , (0x3041, 0x33FF)    -- kana, CJK symbols
    , (0x3400, 0x4DBF)    -- CJK ext A
    , (0x4E00, 0x9FFF)    -- CJK unified
    , (0xA000, 0xA4CF)    -- Yi
    , (0xA960, 0xA97F)
    , (0xAC00, 0xD7A3)    -- Hangul syllables
    , (0xF900, 0xFAFF)    -- CJK compatibility
    , (0xFE10, 0xFE19)
    , (0xFE30, 0xFE6F)
    , (0xFF00, 0xFF60)    -- fullwidth forms
    , (0xFFE0, 0xFFE6)
    , (0x1B000, 0x1B2FB)  -- kana supplements
    , (0x1F300, 0x1F64F)  -- emoji, emoticons
    , (0x1F680, 0x1F6FF)  -- transport emoji
    , (0x1F900, 0x1F9FF)  -- supplemental emoji
    , (0x1FA70, 0x1FAFF)  -- extended emoji
    , (0x20000, 0x2FFFD)  -- CJK ext B..F
    , (0x30000, 0x3FFFD)
    ]
