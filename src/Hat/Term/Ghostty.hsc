-- | The raw libghostty-vt shim binding: foreign imports, 'GhostShimCell'
-- field accessors, and the enum scalars the calls take — everything hsc2hs
-- must generate and nothing else, so a C-header change rebuilds only this
-- module. The emulator logic lives in "Hat.Term.Emulator".
module Hat.Term.Ghostty
    ( CTerm
    , CRender
    , c_new
    , c_free
    , c_write
    , c_resize
    , c_get
    , c_get_title
    , c_mode
    , c_row_cells
    , c_paint_row
    , c_format_history
    , c_format_release
    , c_graphemes
    , c_row_wrapped
    , c_raw_row
    , c_cell_link
    , c_row_prompt
    , c_render_new
    , c_render_free
    , c_render_snapshot
    , c_pen
    , c_encode_key
    , c_key_modes
    , c_set
    , c_get_into
    , WritePtyFn
    , BellFn
    , wrapWritePty
    , wrapBell
    , shimCellSize
    , peekCellCodepoint
    , peekCellGrapheme
    , peekCellWidth
    , peekCellFlags
    , peekCellFgTag
    , peekCellFgVal
    , peekCellBgTag
    , peekCellBgVal
    , RawCellFields (..)
    , rawCellSize
    , peekRawCell
    , tagActive
    , tagHistory
    , dataCols
    , dataRows
    , dataCursorX
    , dataCursorY
    , dataCursorPendingWrap
    , dataCursorVisible
    , dataActiveScreen
    , dataScrollbackRows
    , dataColorCursor
    , dataColorForeground
    , dataColorBackground
    , dataColorPalette
    , dataColorPaletteDefault
    , optColorPalette
    , optWritePty
    , optBell
    , screenAlternate
    , resSuccess
    , resOutOfSpace
    ) where

#include <ghostty/vt.h>
#include "ghostty_shim.h"

import Foreign
import Foreign.C.Types

data CTerm
data CRender

foreign import ccall unsafe "ghost_shim_new"
    c_new :: CUShort -> CUShort -> CSize -> IO (Ptr CTerm)
foreign import ccall unsafe "ghost_shim_free"
    c_free :: Ptr CTerm -> IO ()
foreign import ccall safe "ghost_shim_write"
    c_write :: Ptr CTerm -> Ptr Word8 -> CSize -> IO ()
foreign import ccall safe "ghost_shim_resize"
    c_resize :: Ptr CTerm -> CUShort -> CUShort -> IO ()
foreign import ccall unsafe "ghost_shim_get"
    c_get :: Ptr CTerm -> CInt -> IO CLong
foreign import ccall unsafe "ghost_shim_get_title"
    c_get_title :: Ptr CTerm -> Ptr Word8 -> CSize -> IO CLong
foreign import ccall unsafe "ghost_shim_mode"
    c_mode :: Ptr CTerm -> CUShort -> CInt -> IO CInt
foreign import ccall unsafe "ghost_shim_row_cells"
    c_row_cells :: Ptr CTerm -> CInt -> CUInt -> CUShort -> Ptr () -> IO CInt
foreign import ccall unsafe "ghost_shim_paint_row"
    c_paint_row :: Ptr CTerm -> CInt -> CUInt -> CUShort
                -> Ptr Word8 -> CSize -> IO CLong
foreign import ccall safe "ghost_shim_format_history"
    c_format_history :: Ptr CTerm -> CUInt -> CUInt -> CUShort
                     -> Ptr (Ptr Word8) -> IO CLong
foreign import ccall unsafe "ghost_shim_format_release"
    c_format_release :: Ptr Word8 -> CSize -> IO ()
foreign import ccall unsafe "ghost_shim_cell_graphemes"
    c_graphemes :: Ptr CTerm -> CInt -> CUShort -> CUInt
                -> Ptr Word32 -> CSize -> Ptr CSize -> IO CInt
foreign import ccall unsafe "ghost_shim_row_wrapped"
    c_row_wrapped :: Ptr CTerm -> CInt -> CUInt -> IO CInt
foreign import ccall unsafe "ghost_shim_raw_row"
    c_raw_row :: Ptr CTerm -> CInt -> CUInt -> CUShort -> Ptr () -> IO CInt
foreign import ccall unsafe "ghost_shim_cell_link"
    c_cell_link :: Ptr CTerm -> CInt -> CUShort -> CUInt -> Ptr Word8 -> CSize
                -> IO CLong
foreign import ccall unsafe "ghost_shim_row_prompt"
    c_row_prompt :: Ptr CTerm -> CInt -> CUInt -> IO CInt
foreign import ccall unsafe "ghost_shim_render_new"
    c_render_new :: IO (Ptr CRender)
foreign import ccall unsafe "ghost_shim_render_free"
    c_render_free :: Ptr CRender -> IO ()
foreign import ccall safe "ghost_shim_render_snapshot"
    c_render_snapshot :: Ptr CRender -> Ptr CTerm -> CUShort -> CUShort
                      -> Ptr () -> Ptr Word8 -> CUChar -> IO CInt
foreign import ccall unsafe "ghost_shim_pen"
    c_pen :: Ptr CTerm -> Ptr () -> IO CInt
foreign import ccall unsafe "ghost_shim_encode_key"
    c_encode_key :: Ptr CTerm -> CUInt -> CUInt -> Ptr Word8 -> CSize -> IO CLong
foreign import ccall unsafe "ghost_shim_key_modes"
    c_key_modes :: Ptr CTerm -> Ptr Word8 -> IO CInt
foreign import ccall unsafe "ghostty_terminal_set"
    c_set :: Ptr CTerm -> CInt -> Ptr () -> IO CInt
-- | @ghostty_terminal_get@ into a caller-supplied buffer, for data wider
-- than 'c_get' reads (colours, the palette).
foreign import ccall unsafe "ghostty_terminal_get"
    c_get_into :: Ptr CTerm -> CInt -> Ptr () -> IO CInt

-- libghostty invokes these synchronously inside 'ghost_shim_write'; the
-- closures registered in 'Hat.Term.Emulator.newEmulator' capture the state
-- IORef and land the pty write-back and the bell in its accumulators.
type WritePtyFn = Ptr CTerm -> Ptr () -> Ptr Word8 -> CSize -> IO ()
type BellFn     = Ptr CTerm -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapWritePty :: WritePtyFn -> IO (FunPtr WritePtyFn)
foreign import ccall "wrapper" wrapBell     :: BellFn -> IO (FunPtr BellFn)

-- | Bytes of one 'GhostShimCell', as 'c_row_cells' and friends fill them.
shimCellSize :: Int
shimCellSize = #{size GhostShimCell}

-- One accessor per GhostShimCell field; see the struct in ghostty_shim.h.
peekCellCodepoint :: Ptr () -> IO Word32
peekCellCodepoint = #{peek GhostShimCell, codepoint}
{-# INLINE peekCellCodepoint #-}
peekCellGrapheme :: Ptr () -> IO CInt
peekCellGrapheme = #{peek GhostShimCell, grapheme}
{-# INLINE peekCellGrapheme #-}
peekCellWidth :: Ptr () -> IO CInt
peekCellWidth = #{peek GhostShimCell, width}
{-# INLINE peekCellWidth #-}
peekCellFlags :: Ptr () -> IO CUInt
peekCellFlags = #{peek GhostShimCell, flags}
{-# INLINE peekCellFlags #-}
peekCellFgTag :: Ptr () -> IO CInt
peekCellFgTag = #{peek GhostShimCell, fg_tag}
{-# INLINE peekCellFgTag #-}
peekCellFgVal :: Ptr () -> IO Word32
peekCellFgVal = #{peek GhostShimCell, fg_val}
{-# INLINE peekCellFgVal #-}
peekCellBgTag :: Ptr () -> IO CInt
peekCellBgTag = #{peek GhostShimCell, bg_tag}
{-# INLINE peekCellBgTag #-}
peekCellBgVal :: Ptr () -> IO Word32
peekCellBgVal = #{peek GhostShimCell, bg_val}
{-# INLINE peekCellBgVal #-}

-- | One 'GhostShimRawCell' as read: every field verbatim, decoded by
-- "Hat.Term.Emulator".
data RawCellFields = RawCellFields
    { rfCodepoint :: Word32
    , rfContent   :: CInt
    , rfWide      :: CInt
    , rfHasLink   :: CInt
    , rfAttrs     :: CUInt
    , rfUnderline :: CInt
    , rfFgTag     :: CInt
    , rfFgVal     :: Word32
    , rfBgTag     :: CInt
    , rfBgVal     :: Word32
    , rfUsTag     :: CInt
    , rfUsVal     :: Word32
    }

-- | Bytes of one 'GhostShimRawCell', as 'c_raw_row' fills them.
rawCellSize :: Int
rawCellSize = #{size GhostShimRawCell}

peekRawCell :: Ptr () -> IO RawCellFields
peekRawCell p = RawCellFields
    <$> #{peek GhostShimRawCell, codepoint} p
    <*> #{peek GhostShimRawCell, content} p
    <*> #{peek GhostShimRawCell, wide} p
    <*> #{peek GhostShimRawCell, has_link} p
    <*> #{peek GhostShimRawCell, attrs} p
    <*> #{peek GhostShimRawCell, underline} p
    <*> #{peek GhostShimRawCell, fg_tag} p
    <*> #{peek GhostShimRawCell, fg_val} p
    <*> #{peek GhostShimRawCell, bg_tag} p
    <*> #{peek GhostShimRawCell, bg_val} p
    <*> #{peek GhostShimRawCell, us_tag} p
    <*> #{peek GhostShimRawCell, us_val} p

-- Point tags for 'c_row_cells', 'c_graphemes', and 'c_row_wrapped'.
tagActive, tagHistory :: CInt
tagActive  = #{const GHOST_SHIM_ACTIVE}
tagHistory = #{const GHOST_SHIM_HISTORY}

-- GhosttyTerminalData selectors for 'c_get'.
dataCols, dataRows, dataCursorX, dataCursorY, dataCursorPendingWrap,
    dataCursorVisible,
    dataActiveScreen, dataScrollbackRows :: CInt
dataCols           = #{const GHOSTTY_TERMINAL_DATA_COLS}
dataRows           = #{const GHOSTTY_TERMINAL_DATA_ROWS}
dataCursorX        = #{const GHOSTTY_TERMINAL_DATA_CURSOR_X}
dataCursorY        = #{const GHOSTTY_TERMINAL_DATA_CURSOR_Y}
dataCursorPendingWrap = #{const GHOSTTY_TERMINAL_DATA_CURSOR_PENDING_WRAP}
dataCursorVisible  = #{const GHOSTTY_TERMINAL_DATA_CURSOR_VISIBLE}
dataActiveScreen   = #{const GHOSTTY_TERMINAL_DATA_ACTIVE_SCREEN}
dataScrollbackRows = #{const GHOSTTY_TERMINAL_DATA_SCROLLBACK_ROWS}

-- Colour selectors for 'c_get_into': a @GhosttyColorRgb@ (three bytes) or
-- a palette of 256 of them.
dataColorCursor, dataColorForeground, dataColorBackground, dataColorPalette,
    dataColorPaletteDefault :: CInt
dataColorCursor = #{const GHOSTTY_TERMINAL_DATA_COLOR_CURSOR}
dataColorForeground = #{const GHOSTTY_TERMINAL_DATA_COLOR_FOREGROUND}
dataColorBackground = #{const GHOSTTY_TERMINAL_DATA_COLOR_BACKGROUND}
dataColorPalette = #{const GHOSTTY_TERMINAL_DATA_COLOR_PALETTE}
dataColorPaletteDefault = #{const GHOSTTY_TERMINAL_DATA_COLOR_PALETTE_DEFAULT}

-- GhosttyTerminalOption selectors for 'c_set'.
optWritePty, optBell :: CInt
optWritePty = #{const GHOSTTY_TERMINAL_OPT_WRITE_PTY}
optBell     = #{const GHOSTTY_TERMINAL_OPT_BELL}

-- | Sets the default 256-colour palette (OSC 4 overrides survive it).
optColorPalette :: CInt
optColorPalette = #{const GHOSTTY_TERMINAL_OPT_COLOR_PALETTE}

-- | What 'c_get' 'dataActiveScreen' reports on the alternate screen.
screenAlternate :: CLong
screenAlternate = #{const GHOSTTY_TERMINAL_SCREEN_ALTERNATE}

-- GhosttyResult codes 'c_graphemes' answers with.
resSuccess, resOutOfSpace :: CInt
resSuccess    = #{const GHOSTTY_SUCCESS}
resOutOfSpace = #{const GHOSTTY_OUT_OF_SPACE}
