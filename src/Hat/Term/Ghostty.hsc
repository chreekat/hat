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
    , c_graphemes
    , c_row_wrapped
    , c_render_new
    , c_render_free
    , c_render_snapshot
    , c_pen
    , c_encode_key
    , c_key_modes
    , c_set
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
    , tagActive
    , tagHistory
    , dataCols
    , dataRows
    , dataCursorX
    , dataCursorY
    , dataCursorVisible
    , dataActiveScreen
    , dataScrollbackRows
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
foreign import ccall unsafe "ghost_shim_cell_graphemes"
    c_graphemes :: Ptr CTerm -> CInt -> CUShort -> CUInt
                -> Ptr Word32 -> CSize -> Ptr CSize -> IO CInt
foreign import ccall unsafe "ghost_shim_row_wrapped"
    c_row_wrapped :: Ptr CTerm -> CInt -> CUInt -> IO CInt
foreign import ccall unsafe "ghost_shim_render_new"
    c_render_new :: IO (Ptr CRender)
foreign import ccall unsafe "ghost_shim_render_free"
    c_render_free :: Ptr CRender -> IO ()
foreign import ccall safe "ghost_shim_render_snapshot"
    c_render_snapshot :: Ptr CRender -> Ptr CTerm -> CUShort -> CUShort
                      -> Ptr () -> Ptr Word8 -> IO CInt
foreign import ccall unsafe "ghost_shim_pen"
    c_pen :: Ptr CTerm -> Ptr () -> IO CInt
foreign import ccall unsafe "ghost_shim_encode_key"
    c_encode_key :: Ptr CTerm -> CUInt -> CUInt -> Ptr Word8 -> CSize -> IO CLong
foreign import ccall unsafe "ghost_shim_key_modes"
    c_key_modes :: Ptr CTerm -> Ptr Word8 -> IO CInt
foreign import ccall unsafe "ghostty_terminal_set"
    c_set :: Ptr CTerm -> CInt -> Ptr () -> IO CInt

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

-- Point tags for 'c_row_cells', 'c_graphemes', and 'c_row_wrapped'.
tagActive, tagHistory :: CInt
tagActive  = #{const GHOST_SHIM_ACTIVE}
tagHistory = #{const GHOST_SHIM_HISTORY}

-- GhosttyTerminalData selectors for 'c_get'.
dataCols, dataRows, dataCursorX, dataCursorY, dataCursorVisible,
    dataActiveScreen, dataScrollbackRows :: CInt
dataCols           = #{const GHOSTTY_TERMINAL_DATA_COLS}
dataRows           = #{const GHOSTTY_TERMINAL_DATA_ROWS}
dataCursorX        = #{const GHOSTTY_TERMINAL_DATA_CURSOR_X}
dataCursorY        = #{const GHOSTTY_TERMINAL_DATA_CURSOR_Y}
dataCursorVisible  = #{const GHOSTTY_TERMINAL_DATA_CURSOR_VISIBLE}
dataActiveScreen   = #{const GHOSTTY_TERMINAL_DATA_ACTIVE_SCREEN}
dataScrollbackRows = #{const GHOSTTY_TERMINAL_DATA_SCROLLBACK_ROWS}

-- GhosttyTerminalOption selectors for 'c_set'.
optWritePty, optBell :: CInt
optWritePty = #{const GHOSTTY_TERMINAL_OPT_WRITE_PTY}
optBell     = #{const GHOSTTY_TERMINAL_OPT_BELL}

-- | What 'c_get' 'dataActiveScreen' reports on the alternate screen.
screenAlternate :: CLong
screenAlternate = #{const GHOSTTY_TERMINAL_SCREEN_ALTERNATE}

-- GhosttyResult codes 'c_graphemes' answers with.
resSuccess, resOutOfSpace :: CInt
resSuccess    = #{const GHOSTTY_SUCCESS}
resOutOfSpace = #{const GHOSTTY_OUT_OF_SPACE}
