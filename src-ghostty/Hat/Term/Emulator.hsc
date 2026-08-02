{-# LANGUAGE StrictData #-}

-- | The terminal emulator, libghostty-vt backend (bug 17). Selected by the
-- @ghostty@ cabal flag in place of the libvterm backend; both present the same
-- interface (this module's exports mirror the vterm one) and share the pure
-- helpers in "Hat.Term.Emulator.Types".
--
-- Unlike libvterm, libghostty owns the grid and the scrollback internally, so
-- there is no cell cache and no scrollback 'Seq' here: 'snapshot' reads the
-- live grid back through the scalar shim on demand. Each pane is touched by one
-- thread at a time; an internal lock also makes the terminal-touching
-- operations safe to call concurrently.
module Hat.Term.Emulator
    ( Emulator
    , Event (..)
    , PropKind (..)
    , OscColorTarget (..)
    , OscTerm (..)
    , Screen (..)
    , Modes (..)
    , MouseMode (..)
    , CursorKey (..)
    , newEmulator
    , feed
    , encodeKey
    , resize
    , snapshot
    , currentPen
    , modes
    , modeReplayBytes
    , restoreBytes
    , cellSgr
    , seedScrollback
    , title
    , scrollbackLength
    , scrollbackLine
    , setScrollbackLimit
    , clearScrollback
    , screenRowText
    , screenCell
    , iconName
    ) where

#include <ghostty/vt.h>
#include "ghostty_shim.h"

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Char (chr)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Foreign
import Foreign.C.Types

import Hat.Geometry
import Hat.Term.Cell
import Hat.Term.Emulator.Types

data CTerm

foreign import ccall unsafe "ghost_shim_new"
    c_new :: CUShort -> CUShort -> CSize -> IO (Ptr CTerm)
foreign import ccall unsafe "&ghost_shim_free"
    p_free :: FunPtr (Ptr CTerm -> IO ())
foreign import ccall safe "ghost_shim_write"
    c_write :: Ptr CTerm -> Ptr Word8 -> CSize -> IO ()
foreign import ccall safe "ghost_shim_resize"
    c_resize :: Ptr CTerm -> CUShort -> CUShort -> IO ()
foreign import ccall unsafe "ghost_shim_get"
    c_get :: Ptr CTerm -> CInt -> IO CLong
foreign import ccall unsafe "ghost_shim_mode"
    c_mode :: Ptr CTerm -> CUShort -> CInt -> IO CInt
foreign import ccall unsafe "ghost_shim_cell"
    c_cell :: Ptr CTerm -> CInt -> CUShort -> CUInt -> Ptr () -> IO CInt

data Emulator = Emulator
    { term    :: ForeignPtr CTerm
        -- ^ owns the libghostty terminal; freed by a finalizer once unreachable
    , lock    :: MVar ()
        -- ^ serializes terminal-touching operations so a reader observes the
        --   grid only between them
    , sbLimit :: IORef Int
    , state   :: IORef EmulatorState
    }

-- | The Haskell-side state libghostty does not hold: the color-scheme
-- subscription hat tracks itself (?2031, not fed to the terminal), and the
-- title\/icon-name hat scrubs out of the stream.
data EmulatorState = EmulatorState
    { colorReport :: Bool
    , title       :: Text
    , iconName    :: Text
    }

newEmulator :: Size -> Int -> IO Emulator
newEmulator sz limit = do
    t <- c_new (fromIntegral sz.cols) (fromIntegral sz.rows) (fromIntegral limit)
    if t == nullPtr then error "ghostty_terminal_new failed" else pure ()
    fp <- newForeignPtr p_free t
    lk <- newMVar ()
    lr <- newIORef limit
    st <- newIORef EmulatorState { colorReport = False, title = "", iconName = "" }
    pure Emulator { term = fp, lock = lk, sbLimit = lr, state = st }

-- | Feed pty output into the emulator; returns what happened.
--
-- TODO(bug17 M2): route the HostProtocol scrubbers and the libghostty
-- write_pty\/bell\/title callbacks through here so the query, notification,
-- bell, and title events come back. For now it drives the grid and reports a
-- coarse repaint.
feed :: Emulator -> ByteString -> IO [Event]
feed e bs = withMVar e.lock $ \_ -> withForeignPtr e.term $ \t -> do
    BU.unsafeUseAsCStringLen bs $ \(p, n) ->
        c_write t (castPtr p) (fromIntegral n)
    pure [ScreenChanged | not (B.null bs)]

-- | Resize the terminal; libghostty reflows the primary screen itself.
resize :: Emulator -> Size -> IO ()
resize e sz = withMVar e.lock $ \_ -> withForeignPtr e.term $ \t ->
    c_resize t (fromIntegral (max 1 sz.cols)) (fromIntegral (max 1 sz.rows))

-- | Take an immutable 'Screen' by reading the live grid straight from
-- libghostty: its cols\/rows, cursor, and every active-area cell.
snapshot :: Emulator -> IO Screen
snapshot e = withMVar e.lock $ \_ -> withForeignPtr e.term readGrid

readGrid :: Ptr CTerm -> IO Screen
readGrid t = do
    cols <- fromIntegral <$> c_get t #{const GHOSTTY_TERMINAL_DATA_COLS}
    rows <- fromIntegral <$> c_get t #{const GHOSTTY_TERMINAL_DATA_ROWS}
    cx   <- fromIntegral <$> c_get t #{const GHOSTTY_TERMINAL_DATA_CURSOR_X}
    cy   <- fromIntegral <$> c_get t #{const GHOSTTY_TERMINAL_DATA_CURSOR_Y}
    vis  <- c_get t #{const GHOSTTY_TERMINAL_DATA_CURSOR_VISIBLE}
    grid <- allocaBytes #{size GhostShimCell} $ \cellp ->
        V.generateM rows $ \r ->
            V.generateM cols $ \c -> do
                _ <- c_cell t #{const GHOST_SHIM_ACTIVE}
                        (fromIntegral c) (fromIntegral r) cellp
                peekShimCell cellp
    pure Screen
        { size = Size { rows = fromIntegral rows, cols = fromIntegral cols }
        , cells = grid
        , cursor = Pos { row = cy, col = cx }
        , cursorVisible = vis /= 0
        }

-- | Marshal one 'GhostShimCell' the shim just filled into a 'Cell': a
-- zero-width continuation renders as empty, a zero codepoint as a space, and
-- the flag bitmask and tagged colors decode as in the shim's header.
peekShimCell :: Ptr () -> IO Cell
peekShimCell p = do
    cp    <- #{peek GhostShimCell, codepoint} p :: IO Word32
    w     <- #{peek GhostShimCell, width} p :: IO CInt
    flags <- #{peek GhostShimCell, flags} p :: IO CUInt
    fgT   <- #{peek GhostShimCell, fg_tag} p :: IO CInt
    fgV   <- #{peek GhostShimCell, fg_val} p :: IO Word32
    bgT   <- #{peek GhostShimCell, bg_tag} p :: IO CInt
    bgV   <- #{peek GhostShimCell, bg_val} p :: IO Word32
    let width = fromIntegral w :: Int
        txt | width == 0 = ""
            | cp == 0    = " "
            | otherwise  = T.singleton (chr (fromIntegral cp))
        has m = flags .&. m /= 0
    pure $! Cell
        { text = txt
        , width = width
        , style = Style
            { fg = color fgT fgV
            , bg = color bgT bgV
            , bold = has 1
            , underline = has 2
            , italic = has 4
            , reverse = has 8
            , strike = has 16
            , blink = has 32
            , faint = has 64
            }
        }
  where
    color :: CInt -> Word32 -> Color
    color tag val
        | tag == 1  = Indexed (fromIntegral val)
        | tag == 2  = RGB (chan 16) (chan 8) (chan 0)
        | otherwise = DefaultColor
      where chan s = fromIntegral ((val `shiftR` s) .&. 0xff)

-- | The mode flags apps have toggled. libghostty tracks the alternate screen,
-- focus reporting, and mouse tracking; the color-scheme subscription hat tracks
-- itself (its ?2031 toggle is never fed to the terminal).
modes :: Emulator -> IO Modes
modes e = withMVar e.lock $ \_ -> do
    cr <- (.colorReport) <$> readIORef e.state
    withForeignPtr e.term $ \t -> do
        alt   <- c_get t #{const GHOSTTY_TERMINAL_DATA_ACTIVE_SCREEN}
        foc   <- c_mode t 1004 0
        m1000 <- c_mode t 1000 0
        m1002 <- c_mode t 1002 0
        m1003 <- c_mode t 1003 0
        let mouse | m1003 /= 0 = MouseMove
                  | m1002 /= 0 = MouseDrag
                  | m1000 /= 0 = MouseClick
                  | otherwise  = MouseOff
        pure Modes
            { altScreen = alt == #{const GHOSTTY_TERMINAL_SCREEN_ALTERNATE}
            , mouse = mouse
            , focusReport = foc /= 0
            , colorReport = cr
            }

-- | Encode a cursor key per the terminal's DECCKM state: application cursor
-- keys get @ESC O _@, normal mode @ESC [ _@.
encodeKey :: Emulator -> CursorKey -> IO ByteString
encodeKey e key = withMVar e.lock $ \_ -> withForeignPtr e.term $ \t -> do
    app <- c_mode t 1 0
    let intro = if app /= 0 then "\ESCO" else "\ESC["
        final = case key of
            CursorUp    -> "A"
            CursorDown  -> "B"
            CursorRight -> "C"
            CursorLeft  -> "D"
            CursorHome  -> "H"
            CursorEnd   -> "F"
    pure (intro <> final)

-- | The current window title, as scrubbed from the stream.
title :: Emulator -> IO Text
title e = (.title) <$> readIORef e.state

-- | The current OSC 1 icon name.
iconName :: Emulator -> IO Text
iconName e = (.iconName) <$> readIORef e.state

-- | The emulator's live pen (the style the next glyph would take).
--
-- TODO(bug17 M3): read it from libghostty's cursor style (the @formatter@'s
-- style emit), for now the default.
currentPen :: Emulator -> IO Style
currentPen _ = pure defaultStyle

-- Scrollback: libghostty owns history internally (read via the HISTORY point
-- tag, seeded by byte-replay). TODO(bug17 M3): wire these to it.

scrollbackLength :: Emulator -> IO Int
scrollbackLength _ = pure 0

scrollbackLine :: Emulator -> Int -> IO (Maybe (V.Vector Cell))
scrollbackLine _ _ = pure Nothing

setScrollbackLimit :: Emulator -> Int -> IO ()
setScrollbackLimit e limit = writeIORef e.sbLimit limit

clearScrollback :: Emulator -> IO ()
clearScrollback _ = pure ()

seedScrollback :: Emulator -> [V.Vector Cell] -> IO ()
seedScrollback _ _ = pure ()
