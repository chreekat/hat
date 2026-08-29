{-# LANGUAGE StrictData #-}

-- | The terminal emulator: libghostty-vt wrapped behind a narrow interface
-- (bug 17). The event/mode vocabulary and the pure byte-synthesizers live in
-- "Hat.Term.Emulator.Types".
--
-- libghostty owns the grid and the scrollback internally, so there is no cell
-- cache and no scrollback 'Seq' here: 'snapshot' reads the live grid back
-- through the scalar shim on demand, and history is read from libghostty's
-- HISTORY point tag. Each pane is touched by one thread at a time; an internal
-- lock also makes the terminal-touching operations safe to call concurrently.
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
    , KeyPress (..)
    , KeyModes (..)
    , modifyOtherKeysBytes
    , keyModeReplayBytes
    , newEmulator
    , feed
    , encodeKey
    , encodeKeyPress
    , keyModes
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
    , scrollbackLineWrapped
    , screenRowWrapped
    , setScrollbackLimit
    , clearScrollback
    , screenRowText
    , screenCell
    ) where

#include <ghostty/vt.h>
#include "ghostty_shim.h"

import Control.Concurrent.MVar
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BU
import Data.Char (chr)
import Data.IORef
import Data.List (intersperse)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Foreign
import Foreign.Concurrent qualified as FC
import Foreign.C.Types
import System.Mem.Weak (Weak)

import Hat.Geometry
import Hat.Intern (shareVals)
import Hat.Term.Cell
import Hat.Term.Emulator.Types
import Hat.Term.HostProtocol

data CTerm

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
foreign import ccall unsafe "ghost_shim_cell"
    c_cell :: Ptr CTerm -> CInt -> CUShort -> CUInt -> Ptr () -> IO CInt
foreign import ccall unsafe "ghost_shim_cell_graphemes"
    c_graphemes :: Ptr CTerm -> CInt -> CUShort -> CUInt
                -> Ptr Word32 -> CSize -> Ptr CSize -> IO CInt
foreign import ccall unsafe "ghost_shim_row_wrapped"
    c_row_wrapped :: Ptr CTerm -> CInt -> CUInt -> IO CInt
foreign import ccall unsafe "ghost_shim_pen"
    c_pen :: Ptr CTerm -> Ptr () -> IO CInt
foreign import ccall unsafe "ghost_shim_encode_key"
    c_encode_key :: Ptr CTerm -> CUInt -> CUInt -> Ptr Word8 -> CSize -> IO CLong
foreign import ccall unsafe "ghost_shim_key_modes"
    c_key_modes :: Ptr CTerm -> Ptr Word8 -> IO CInt
foreign import ccall unsafe "ghostty_terminal_set"
    c_set :: Ptr CTerm -> CInt -> Ptr () -> IO CInt

-- libghostty invokes these synchronously inside 'ghost_shim_write'; the
-- closures registered in 'newEmulator' capture the state IORef and land the
-- pty write-back and the bell in its accumulators.
type WritePtyFn = Ptr CTerm -> Ptr () -> Ptr Word8 -> CSize -> IO ()
type BellFn     = Ptr CTerm -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapWritePty :: WritePtyFn -> IO (FunPtr WritePtyFn)
foreign import ccall "wrapper" wrapBell     :: BellFn -> IO (FunPtr BellFn)

-- | Per-emulator cell intern table; see 'peekShimCell'.
type CellIntern = IORef (Map Cell (Weak Cell))

data Emulator = Emulator
    { term    :: ForeignPtr CTerm
        -- ^ owns the libghostty terminal; freed by a finalizer once unreachable
    , lock    :: MVar ()
        -- ^ serializes terminal-touching operations so a reader observes the
        --   grid only between them
    , sbLimit :: IORef Int
    , state   :: IORef EmulatorState
    , cellIntern :: CellIntern
        -- ^ shared representatives for cells read out of libghostty
    }

-- | The Haskell-side state libghostty does not hold: the color-scheme
-- subscription hat tracks itself (?2031, not fed to the terminal), the title
-- hat scrubs out of the stream (ESC k), and the two cross-chunk scrubber states
-- for tmux passthrough and screen\/tmux ESC k titles.
data EmulatorState = EmulatorState
    { colorSub    :: Bool
    , title       :: Text
    , passthrough :: PassState
    , screenTitle :: StitleState
    , output      :: [ByteString]  -- ^ write_pty callback bytes, reversed
    , events      :: [Event]       -- ^ bell callback events, reversed
    }

newEmulator :: Size -> Int -> IO Emulator
newEmulator sz limit = do
    -- libghostty's max_scrollback is a byte budget, not a row count, so the row
    -- limit is enforced on the read side ('scrollbackLength'). Give it bytes
    -- generously proportional to the limit so the row cap is never starved.
    let budget = max 65536 (fromIntegral limit * 512) :: CSize
    t <- c_new (fromIntegral sz.cols) (fromIntegral sz.rows) budget
    if t == nullPtr then error "ghostty_terminal_new failed" else pure ()
    lk <- newMVar ()
    lr <- newIORef limit
    st <- newIORef EmulatorState
        { colorSub = False
        , title = ""
        , passthrough = Outside ""
        , screenTitle = StOutside ""
        , output = []
        , events = []
        }

    writePtyW <- wrapWritePty $ \_ _ p n -> do
        bs <- B.packCStringLen (castPtr p, fromIntegral n)
        modifyIORef' st $ \s -> s { output = bs : s.output }
    bellW <- wrapBell $ \_ _ ->
        modifyIORef' st $ \s -> s { events = Bell : s.events }
    _ <- c_set t #{const GHOSTTY_TERMINAL_OPT_WRITE_PTY} (castFunPtrToPtr writePtyW)
    _ <- c_set t #{const GHOSTTY_TERMINAL_OPT_BELL} (castFunPtrToPtr bellW)

    -- Free the terminal and both callback FunPtrs once the emulator is
    -- unreachable, so a closed pane leaks neither.
    fp <- FC.newForeignPtr t $ do
        c_free t
        freeHaskellFunPtr writePtyW
        freeHaskellFunPtr bellW
    ci <- newIORef Map.empty
    pure Emulator
        { term = fp, lock = lk, sbLimit = lr, state = st, cellIntern = ci }

-- | Feed pty output into the emulator; returns what happened. The
-- host-protocol scrubbers (tmux passthrough, screen\/tmux ESC k titles, the
-- OSC 10\/11 and DEC 2031 queries hat answers, OSC 9\/777 notifications) run
-- here as pure byte-scanning above the grid engine, and only the leftover
-- bytes reach libghostty. libghostty's own write_pty\/bell callbacks land the 'Output'
-- (CPR\/DA replies) and 'Bell' events, and the OSC 0\/2 title is polled back
-- after the write.
feed :: Emulator -> ByteString -> IO [Event]
feed e bs0 = withMVar e.lock $ \_ -> do
    s0 <- readIORef e.state
    let (pass1, depassed, passPayloads) = scrubPassthrough s0.passthrough bs0
        (stitle1, scrubbed, stitles) = scrubStitle s0.screenTitle depassed
        screenTitles = map TE.decodeUtf8Lenient stitles
        latestTitle = case screenTitles of
            [] -> s0.title
            ts -> last ts
    modifyIORef' e.state $ \s -> s
        { passthrough = pass1
        , screenTitle = stitle1
        , title = latestTitle
        , output = []
        , events = []
        }
    -- Split each passthrough payload into the host queries hat answers and the
    -- leftover it surfaces as UnhandledPassthrough for the reader to log.
    let parts = map partitionPassthrough passPayloads
        unhandled = [UnhandledPassthrough r | (_, r) <- parts, not (B.null r)]
    passEvs <- concat <$> mapM (applySignal e) (concatMap fst parts)
    (interleaved, oscTitleEvs) <- withForeignPtr e.term $ \t -> do
        ievs <- feedSegments e t scrubbed
        (,) ievs <$> pollTitle e t
    s1 <- readIORef e.state
    pure $ passEvs
        <> unhandled
        <> map TitleChanged screenTitles
        <> interleaved
        <> reverse s1.events
        <> oscTitleEvs
        <> [ScreenChanged | not (B.null scrubbed) || not (null screenTitles)]

-- | Feed a chunk to libghostty piecewise, splitting at each color query hat
-- answers itself so the query's event lands in stream order among the bytes
-- around it.
feedSegments :: Emulator -> Ptr CTerm -> ByteString -> IO [Event]
feedSegments e t = go
  where
    go bs = case nextQuery bs of
        Nothing -> writeSeg bs
        Just (before, sig, rest) -> do
            outEvs <- writeSeg before
            sigEvs <- applySignal e sig
            ((outEvs <> sigEvs) <>) <$> go rest
    -- After feeding a segment, collect what libghostty wrote back to the pty
    -- (a DA/CPR reply).
    writeSeg seg
        | B.null seg = pure []
        | otherwise = do
            BU.unsafeUseAsCStringLen seg $ \(p, n) ->
                c_write t (castPtr p) (fromIntegral n)
            s <- readIORef e.state
            writeIORef e.state (s { output = [] })
            let outs = B.concat (reverse s.output)
            pure [Output outs | not (B.null outs)]

-- | Turn a host query hat answers itself into its 'Event', recording the DEC
-- 2031 subscription a @CSI ? 2031 h@\/@l@ toggles (never fed to libghostty, so
-- hat is its sole tracker). The same answer serves an inline query and one
-- wrapped in tmux passthrough.
applySignal :: Emulator -> QuerySignal -> IO [Event]
applySignal e sig = case sig of
    SigColor CsEnable  -> [ColorSchemeQuery]
        <$ modifyIORef' e.state (\s -> s { colorSub = True })
    SigColor CsDisable -> []
        <$ modifyIORef' e.state (\s -> s { colorSub = False })
    SigColor CsQuery   -> pure [ColorSchemeQuery]
    SigOsc target term -> pure [OscColorQuery target term]
    SigNotify raw      -> pure [DesktopNotification raw]

-- | libghostty swallows an OSC 0\/2 title and stores it rather than exposing a
-- string in its callback, so read it back after a feed and raise a
-- 'TitleChanged' when it actually changed.
pollTitle :: Emulator -> Ptr CTerm -> IO [Event]
pollTitle e t = do
    newT <- readTitle t
    cur <- (.title) <$> readIORef e.state
    if not (T.null newT) && newT /= cur
        then [TitleChanged newT] <$ modifyIORef' e.state (\s -> s { title = newT })
        else pure []

readTitle :: Ptr CTerm -> IO Text
readTitle t = allocaBytes cap $ \buf -> do
    n <- c_get_title t buf (fromIntegral cap)
    if n <= 0
        then pure ""
        else TE.decodeUtf8Lenient
            <$> B.packCStringLen (castPtr buf, min (fromIntegral n) cap)
  where
    cap = 4096

-- | Resize the terminal; libghostty reflows the primary screen itself.
resize :: Emulator -> Size -> IO ()
resize e sz = withMVar e.lock $ \_ -> withForeignPtr e.term $ \t ->
    c_resize t (fromIntegral (max 1 sz.cols)) (fromIntegral (max 1 sz.rows))

-- | Take an immutable 'Screen' by reading the live grid straight from
-- libghostty: its cols\/rows, cursor, and every active-area cell.
snapshot :: Emulator -> IO Screen
snapshot e = withMVar e.lock $ \_ -> withForeignPtr e.term (readGrid e.cellIntern)

readGrid :: CellIntern -> Ptr CTerm -> IO Screen
readGrid ci t = do
    cols <- fromIntegral <$> c_get t #{const GHOSTTY_TERMINAL_DATA_COLS}
    rows <- fromIntegral <$> c_get t #{const GHOSTTY_TERMINAL_DATA_ROWS}
    cx   <- fromIntegral <$> c_get t #{const GHOSTTY_TERMINAL_DATA_CURSOR_X}
    cy   <- fromIntegral <$> c_get t #{const GHOSTTY_TERMINAL_DATA_CURSOR_Y}
    vis  <- c_get t #{const GHOSTTY_TERMINAL_DATA_CURSOR_VISIBLE}
    grid <- V.generateM rows $ \r -> readRow ci t #{const GHOST_SHIM_ACTIVE} r cols
    pure Screen
        { size = Size { rows = fromIntegral rows, cols = fromIntegral cols }
        , cells = grid
        , cursor = Pos { row = cy, col = cx }
        , cursorVisible = vis /= 0
        }

-- | Read one row's @cols@ cells under a point tag (active or history) into a
-- vector of 'Cell's.
readRow :: CellIntern -> Ptr CTerm -> CInt -> Int -> Int -> IO (V.Vector Cell)
readRow ci t tag y cols = allocaBytes #{size GhostShimCell} $ \cellp ->
    V.generateM cols $ \c -> do
        _ <- c_cell t tag (fromIntegral c) (fromIntegral y) cellp
        peekShimCell ci (graphemeMarks t tag c y) cellp

-- | The combining codepoints of a cluster cell: the full cluster minus its
-- base. Retries once with the exact size if a cluster outgrows the buffer.
graphemeMarks :: Ptr CTerm -> CInt -> Int -> Int -> IO [Char]
graphemeMarks t tag x y = go 16
  where
    go cap = allocaArray cap $ \buf -> alloca $ \lenp -> do
        r <- c_graphemes t tag (fromIntegral x) (fromIntegral y)
                buf (fromIntegral cap) lenp
        n <- fromIntegral <$> peek lenp
        case () of
            _ | r == #{const GHOSTTY_SUCCESS} ->
                    map (chr . fromIntegral) . drop 1 <$> peekArray n buf
              | r == #{const GHOSTTY_OUT_OF_SPACE}, n > cap -> go n
              | otherwise -> pure []

-- | Marshal one 'GhostShimCell' the shim just filled into a 'Cell': a
-- zero-width continuation renders as empty, a zero codepoint as a space, and
-- the flag bitmask and tagged colors decode as in the shim's header. The fresh
-- cell is interned ('shareVals') so equal cells across the grid and scrollback
-- collapse to one shared heap object.
peekShimCell :: CellIntern -> IO [Char] -> Ptr () -> IO Cell
peekShimCell ci fetchMarks p = do
    cp    <- #{peek GhostShimCell, codepoint} p :: IO Word32
    g     <- #{peek GhostShimCell, grapheme} p :: IO CInt
    w     <- #{peek GhostShimCell, width} p :: IO CInt
    flags <- #{peek GhostShimCell, flags} p :: IO CUInt
    fgT   <- #{peek GhostShimCell, fg_tag} p :: IO CInt
    fgV   <- #{peek GhostShimCell, fg_val} p :: IO Word32
    bgT   <- #{peek GhostShimCell, bg_tag} p :: IO CInt
    bgV   <- #{peek GhostShimCell, bg_val} p :: IO Word32
    mks <- if g /= 0 then fetchMarks else pure []
    let width = fromIntegral w :: Int
        ch | width == 0 || cp == 0 = ' '
           | otherwise             = chr (fromIntegral cp)
        has m = flags .&. m /= 0
    shareVals ci $! Cell
        { char = ch
        , marks = mks
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
    cr <- (.colorSub) <$> readIORef e.state
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

-- | Encode a modified key press as the pane's app expects it: the legacy bytes
-- when it turned no key protocol on (modifiers with no legacy encoding are
-- dropped, keeping alt, as tmux does), the modifyOtherKeys form once it sent
-- @CSI > 4 ; 2 m@, the kitty CSI-u form once it pushed kitty flags.
-- 'Nothing' when the encoder cannot spell the key, leaving the caller its
-- tokenized bytes.
encodeKeyPress :: Emulator -> KeyPress -> IO (Maybe ByteString)
encodeKeyPress e kp = withMVar e.lock $ \_ -> withForeignPtr e.term $ \t -> do
    km <- readKeyModes t
    -- libghostty keeps a legacy encoding under modifyOtherKeys where xterm and
    -- tmux escape it, and a half-escaped app reads the legacy byte as unmapped.
    if km.modifyOtherKeys && km.kittyFlags == 0
        then pure (Just (modifyOtherKeysBytes kp))
        else allocaBytes cap $ \buf -> do
            n <- c_encode_key t (fromIntegral kp.code) (fromIntegral kp.mods)
                    buf (fromIntegral cap)
            if n < 0
                then pure Nothing
                else Just <$> B.packCStringLen (castPtr buf, fromIntegral n)
  where
    cap = 64 :: Int

-- | The key protocols the pane's app has turned on, for the reload carry.
keyModes :: Emulator -> IO KeyModes
keyModes e = withMVar e.lock $ \_ -> withForeignPtr e.term readKeyModes

readKeyModes :: Ptr CTerm -> IO KeyModes
readKeyModes t = alloca $ \kfp -> do
    poke kfp 0
    mok <- c_key_modes t kfp
    kf <- peek kfp
    pure KeyModes
        { modifyOtherKeys = mok > 0, kittyFlags = fromIntegral (kf :: Word8) }

-- | The current window title: an OSC 0\/2 title polled from libghostty, or a
-- screen\/tmux ESC k title scrubbed from the stream.
title :: Emulator -> IO Text
title e = (.title) <$> readIORef e.state

-- | The emulator's live pen: the style the next glyph would take. libghostty
-- surfaces it only through the shim's formatter round-trip.
currentPen :: Emulator -> IO Style
currentPen e = withMVar e.lock $ \_ -> withForeignPtr e.term $ \t ->
    allocaBytes #{size GhostShimCell} $ \cellp -> do
        _ <- c_pen t cellp
        (.style) <$> peekShimCell e.cellIntern (pure []) cellp

-- Scrollback lives inside libghostty (the HISTORY point tag), so the row limit
-- hat exposes is enforced here on the read side, over libghostty's byte-bounded
-- history. 'scrollbackLine' 0 is the oldest of the newest 'limit' rows.

-- | How many scrollback rows hat exposes: libghostty's physical count capped
-- to the live row limit.
scrollbackLength :: Emulator -> IO Int
scrollbackLength e = withMVar e.lock $ \_ -> withForeignPtr e.term $ \t -> do
    lim <- readIORef e.sbLimit
    phys <- physicalScrollback t
    pure (min phys (max 0 lim))

-- | Scrollback line by age within the exposed window: 0 is the oldest.
scrollbackLine :: Emulator -> Int -> IO (Maybe (V.Vector Cell))
scrollbackLine e i = withMVar e.lock $ \_ -> withForeignPtr e.term $ \t -> do
    lim <- readIORef e.sbLimit
    phys <- physicalScrollback t
    let exposed = min phys (max 0 lim)
    if i < 0 || i >= exposed
        then pure Nothing
        else do
            cols <- fromIntegral <$> c_get t #{const GHOSTTY_TERMINAL_DATA_COLS}
            Just <$> readRow e.cellIntern t #{const GHOST_SHIM_HISTORY} (phys - exposed + i) cols

-- | Whether a scrollback row (indexed as 'scrollbackLine') soft-wraps onto
-- the next row.
scrollbackLineWrapped :: Emulator -> Int -> IO Bool
scrollbackLineWrapped e i = withMVar e.lock $ \_ -> withForeignPtr e.term $ \t -> do
    lim <- readIORef e.sbLimit
    phys <- physicalScrollback t
    let exposed = min phys (max 0 lim)
    if i < 0 || i >= exposed
        then pure False
        else (/= 0) <$> c_row_wrapped t #{const GHOST_SHIM_HISTORY}
            (fromIntegral (phys - exposed + i))

-- | Whether a live-screen row soft-wraps onto the next row.
screenRowWrapped :: Emulator -> Int -> IO Bool
screenRowWrapped e r = withMVar e.lock $ \_ -> withForeignPtr e.term $ \t ->
    if r < 0
        then pure False
        else (/= 0) <$> c_row_wrapped t #{const GHOST_SHIM_ACTIVE} (fromIntegral r)

-- | Change the live row cap. Read-side only: libghostty already bounds the
-- history's memory, so a lowered limit hides the oldest rows and a raised one
-- reveals rows still retained, without touching libghostty.
setScrollbackLimit :: Emulator -> Int -> IO ()
setScrollbackLimit e limit = writeIORef e.sbLimit limit

-- | Drop all scrollback (the live screen is untouched): libghostty clears its
-- history on ED 3 (@CSI 3 J@).
clearScrollback :: Emulator -> IO ()
clearScrollback e = withMVar e.lock $ \_ -> withForeignPtr e.term $ \t ->
    feedBytes t "\ESC[3J"

-- | Seed a fresh emulator's history with captured lines (oldest first), trimmed
-- to the current limit. libghostty owns scrollback and offers no way to inject
-- history rows directly, so replay them as bytes: paint each line, then scroll
-- the whole block up out of the viewport with SU (@CSI n S@), which pushes
-- primary-screen rows into history and leaves a blank screen for a following
-- 'restoreBytes'. The reload-restore companion to it.
seedScrollback :: Emulator -> [V.Vector Cell] -> IO ()
seedScrollback e ls = withMVar e.lock $ \_ -> withForeignPtr e.term $ \t -> do
    lim <- readIORef e.sbLimit
    let kept = drop (length ls - lim) ls
        n = length kept
    unless (n == 0) $ do
        rows <- fromIntegral <$> c_get t #{const GHOSTTY_TERMINAL_DATA_ROWS}
        feedBytes t (seedBytes (min n rows) kept)

-- | The bytes 'seedScrollback' feeds: each line painted under a leading reset,
-- CRLF-separated (no trailing CRLF, so the last line is not left one row low),
-- then SU by @su@ to scroll every line into history, then a final pen reset.
seedBytes :: Int -> [V.Vector Cell] -> ByteString
seedBytes su ls = BL.toStrict $ BB.toLazyByteString $
       mconcat (intersperse (BB.byteString "\r\n") (map paintLine ls))
    <> BB.byteString "\ESC[" <> BB.intDec su <> BB.char8 'S'
    <> BB.byteString "\ESC[0m"
  where
    paintLine row =
        BB.byteString "\ESC[0m" <> paintRow defaultStyle (rtrimBlank (V.toList row))

-- | libghostty's physical scrollback row count (total rows minus the viewport).
physicalScrollback :: Ptr CTerm -> IO Int
physicalScrollback t =
    max 0 . fromIntegral <$> c_get t #{const GHOSTTY_TERMINAL_DATA_SCROLLBACK_ROWS}

feedBytes :: Ptr CTerm -> ByteString -> IO ()
feedBytes t bs = BU.unsafeUseAsCStringLen bs $ \(p, n) ->
    c_write t (castPtr p) (fromIntegral n)
