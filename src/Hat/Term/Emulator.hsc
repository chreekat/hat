{-# LANGUAGE StrictData #-}

-- | The terminal emulator: libvterm wrapped behind a narrow interface.
--
-- Each pane owns one 'Emulator' and is touched by one thread at a time;
-- an internal lock also makes 'feed' / 'snapshot' / 'resize' safe to call
-- concurrently. libvterm's callbacks fire synchronously inside
-- @vterm_input_write@ and land in the 'EmulatorState' held behind a single
-- IORef in this module.
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

#include "hat_shim.h"

import Control.Concurrent.MVar
import Control.Monad (foldM, forM, forM_, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Char (chr, ord)
import Data.IORef
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Foreign
import qualified Foreign.Concurrent as FC
import Foreign.C.String (CString)
import Foreign.C.Types

import Hat.Geometry
import Hat.Intern (shareVals)
import Hat.Term.Cell
import Hat.Term.Emulator.Types
import Hat.Term.HostProtocol

data CVTerm
data CVTermScreen
data CVTermCells
data CHatCell
data CHatCallbacks

foreign import ccall unsafe "vterm_new"
    c_vterm_new :: CInt -> CInt -> IO (Ptr CVTerm)
foreign import ccall unsafe "vterm_free"
    c_vterm_free :: Ptr CVTerm -> IO ()
foreign import ccall unsafe "vterm_set_utf8"
    c_vterm_set_utf8 :: Ptr CVTerm -> CInt -> IO ()
foreign import ccall safe "vterm_set_size"
    c_vterm_set_size :: Ptr CVTerm -> CInt -> CInt -> IO ()
foreign import ccall unsafe "vterm_obtain_screen"
    c_vterm_obtain_screen :: Ptr CVTerm -> IO (Ptr CVTermScreen)
foreign import ccall safe "vterm_input_write"
    c_vterm_input_write :: Ptr CVTerm -> CString -> CSize -> IO CSize
foreign import ccall safe "vterm_keyboard_key"
    c_vterm_keyboard_key :: Ptr CVTerm -> CInt -> CInt -> IO ()
foreign import ccall safe "vterm_screen_flush_damage"
    c_flush_damage :: Ptr CVTermScreen -> IO ()
foreign import ccall unsafe "vterm_screen_set_damage_merge"
    c_set_damage_merge :: Ptr CVTermScreen -> CInt -> IO ()
foreign import ccall unsafe "vterm_screen_enable_altscreen"
    c_enable_altscreen :: Ptr CVTermScreen -> CInt -> IO ()
foreign import ccall safe "vterm_screen_reset"
    c_screen_reset :: Ptr CVTermScreen -> CInt -> IO ()
foreign import ccall unsafe "hat_setup"
    c_hat_setup :: Ptr CVTerm -> Ptr CHatCallbacks -> IO ()
foreign import ccall unsafe "hat_get_cell"
    c_hat_get_cell :: Ptr CVTermScreen -> CInt -> CInt -> Ptr CHatCell -> IO CInt
foreign import ccall unsafe "hat_get_pen"
    c_hat_get_pen :: Ptr CVTerm -> Ptr CHatCell -> IO CInt
foreign import ccall unsafe "hat_flatten_cell_at"
    c_flatten_cell_at :: Ptr CVTermCells -> CInt -> Ptr CHatCell -> IO ()
foreign import ccall unsafe "hat_unflatten_cell_at"
    c_unflatten_cell_at :: Ptr CVTermCells -> CInt -> Ptr CHatCell -> IO ()

type DamageFn = CInt -> CInt -> CInt -> CInt -> IO ()
type MoveCursorFn = CInt -> CInt -> CInt -> IO ()
type PropBoolFn = CInt -> CInt -> IO ()
type PropIntFn = CInt -> CInt -> IO ()
type PropStrFn = CInt -> CString -> CSize -> CInt -> IO ()
type BellFn = IO ()
type PushlineFn = CInt -> Ptr CVTermCells -> IO ()
type PoplineFn = CInt -> Ptr CVTermCells -> IO CInt
type OutputFn = CString -> CSize -> IO ()

foreign import ccall "wrapper" wrapDamage :: DamageFn -> IO (FunPtr DamageFn)
foreign import ccall "wrapper" wrapMoveCursor :: MoveCursorFn -> IO (FunPtr MoveCursorFn)
foreign import ccall "wrapper" wrapPropBool :: PropBoolFn -> IO (FunPtr PropBoolFn)
foreign import ccall "wrapper" wrapPropInt :: PropIntFn -> IO (FunPtr PropIntFn)
foreign import ccall "wrapper" wrapPropStr :: PropStrFn -> IO (FunPtr PropStrFn)
foreign import ccall "wrapper" wrapBell :: BellFn -> IO (FunPtr BellFn)
foreign import ccall "wrapper" wrapPushline :: PushlineFn -> IO (FunPtr PushlineFn)
foreign import ccall "wrapper" wrapPopline :: PoplineFn -> IO (FunPtr PoplineFn)
foreign import ccall "wrapper" wrapOutput :: OutputFn -> IO (FunPtr OutputFn)

data Emulator = Emulator
    { vt              :: ForeignPtr CVTerm
        -- ^ owns the libvterm object; a finalizer frees it along with the
        --   callback struct and the callback FunPtrs (see 'newEmulator').
    , screen          :: Ptr CVTermScreen  -- ^ borrowed from 'vt', valid while it lives
    , lock            :: MVar ()
        -- ^ serializes whole operations, so other threads observe the grid
        --   only between them, never mid-update.
    , scrollbackLimit :: IORef Int
        -- ^ the live scrollback cap; 'setScrollbackLimit' updates it so an
        --   already-open pane honors a changed @history-limit@.
    , state           :: IORef EmulatorState
        -- ^ a bare IORef, not data guarded by 'lock': the libvterm callbacks
        --   write it synchronously from within a 'feed' that already holds
        --   'lock', so they must reach it without taking a lock themselves.
    }

-- | An emulator's mutable state, held behind the single IORef in 'Emulator'.
-- It embeds the very 'Screen' and 'Modes' that 'snapshot' and 'modes' hand
-- back (see 'newEmulator').
data EmulatorState = EmulatorState
    { view         :: Screen         -- the live grid, size, and cursor
    , modeFlags    :: Modes
    , title        :: Text
    , pendingTitle :: ByteString     -- title fragments awaiting their final chunk
    , iconName     :: Text           -- OSC 1 icon name (VTERM_PROP_ICONNAME)
    , pendingIcon  :: ByteString     -- icon-name fragments awaiting their final chunk
    , dirty        :: Bool
    , damage       :: [Rect]         -- reversed
    , events       :: [Event]        -- reversed
    , output       :: [ByteString]   -- reversed
    , passthrough  :: PassState      -- tmux-passthrough scrubber state
    , screenTitle  :: StitleState    -- screen/tmux title scrubber state
    , scrollback   :: Seq (V.Vector Cell)
    }

-- | Build a fresh emulator for a pane: a libvterm instance sized to 'Size'
-- with UTF-8 input and the alternate screen enabled, its screen callbacks
-- wired to update the single 'EmulatorState' ref, and a finalizer that frees
-- every C resource once the emulator becomes unreachable. The 'Int' caps how
-- many scrollback lines are retained.
newEmulator :: Size -> Int -> IO Emulator
newEmulator sz historyLimit = do
    cellIntern <- newIORef Map.empty
    vtp <- c_vterm_new (fromIntegral sz.rows) (fromIntegral sz.cols)
    c_vterm_set_utf8 vtp 1
    scr <- c_vterm_obtain_screen vtp
    c_enable_altscreen scr 1
    c_set_damage_merge scr #{const VTERM_DAMAGE_SCROLL}

    lockVar <- newMVar ()
    limitR <- newIORef historyLimit
    stateR <- newIORef EmulatorState
        { view = Screen
            { size = sz
            , cells = blankGrid sz
            , cursor = Pos { row = 0, col = 0 }
            , cursorVisible = True
            }
        , modeFlags = Modes
            { altScreen = False
            , mouse = MouseOff
            , focusReport = False
            , colorReport = False
            }
        , title = ""
        , pendingTitle = ""
        , iconName = ""
        , pendingIcon = ""
        , dirty = False
        , damage = []
        , events = []
        , output = []
        , passthrough = Outside ""
        , screenTitle = StOutside ""
        , scrollback = Seq.empty
        }

    damageW <- wrapDamage $ \sr er sc ec ->
        modifyIORef' stateR $ \s -> s
            { damage = Rect
                { startRow = fromIntegral sr
                , endRow = fromIntegral er
                , startCol = fromIntegral sc
                , endCol = fromIntegral ec
                } : s.damage
            , dirty = True
            }
    moveW <- wrapMoveCursor $ \r c vis ->
        modifyIORef' stateR $ \s -> s
            { view = s.view
                { cursor = Pos { row = fromIntegral r, col = fromIntegral c }
                , cursorVisible = vis /= 0
                }
            , dirty = True
            }
    propBoolW <- wrapPropBool $ \prop val -> case prop of
        #{const VTERM_PROP_CURSORVISIBLE} ->
            modifyIORef' stateR $ \s ->
                s { view = s.view { cursorVisible = val /= 0 }, dirty = True }
        #{const VTERM_PROP_ALTSCREEN} ->
            modifyIORef' stateR $ \s ->
                s { modeFlags = s.modeFlags { altScreen = val /= 0 }, dirty = True }
        #{const VTERM_PROP_FOCUSREPORT} ->
            modifyIORef' stateR $ \s ->
                s { modeFlags = s.modeFlags { focusReport = val /= 0 } }
        _ -> emitUnknownProp stateR PropBool prop
    propIntW <- wrapPropInt $ \prop val -> case prop of
        #{const VTERM_PROP_MOUSE} ->
            let m = case val of
                    1 -> MouseClick
                    2 -> MouseDrag
                    3 -> MouseMove
                    _ -> MouseOff
            in modifyIORef' stateR $ \s -> s { modeFlags = s.modeFlags { mouse = m } }
        _ -> emitUnknownProp stateR PropInt prop
    propStrW <- wrapPropStr $ \prop str len final -> case prop of
        #{const VTERM_PROP_TITLE} -> do
            frag <- B.packCStringLen (str, fromIntegral len)
            if final /= 0
                then modifyIORef' stateR $ \s ->
                    let t = TE.decodeUtf8Lenient (s.pendingTitle <> frag)
                    in s { pendingTitle = "", title = t
                         , events = TitleChanged t : s.events }
                else modifyIORef' stateR $ \s ->
                    s { pendingTitle = s.pendingTitle <> frag }
        -- The OSC 1 icon name: record it as emulator state (assembled from its
        -- fragments) but raise no event — hat surfaces only the pane title.
        #{const VTERM_PROP_ICONNAME} -> do
            frag <- B.packCStringLen (str, fromIntegral len)
            if final /= 0
                then modifyIORef' stateR $ \s ->
                    s { pendingIcon = ""
                      , iconName = TE.decodeUtf8Lenient (s.pendingIcon <> frag) }
                else modifyIORef' stateR $ \s ->
                    s { pendingIcon = s.pendingIcon <> frag }
        -- libvterm delivers a string prop in fragments; surface it once,
        -- when the terminating fragment arrives, not per fragment.
        _ -> if final /= 0 then emitUnknownProp stateR PropStr prop else pure ()
    bellW <- wrapBell $ modifyIORef' stateR $ \s -> s { events = Bell : s.events }
    -- The alt screen has no scrollback of its own. Shrinking it (unzooming a
    -- pane while a full-screen app runs) must not push the app's rows into ours
    -- -- else a later grow of the primary pops them back up over the shell --
    -- and growing it must not pull the primary's history in (the app repaints
    -- on SIGWINCH). So while on the alt screen our scrollback is frozen.
    pushW <- wrapPushline $ \ncols cellsPtr -> do
        onAlt <- (.modeFlags.altScreen) <$> readIORef stateR
        unless onAlt $ do
            line <- V.generateM (fromIntegral ncols - 1 :: Int) $ \i -> do
                freshCell <- allocaBytes #{size HatCell} $ \hc -> do
                    c_flatten_cell_at cellsPtr (fromIntegral i) hc
                    peekHatCell hc
                shareVals cellIntern freshCell
            limit <- readIORef limitR
            modifyIORef' stateR $ \s ->
                let sb' = s.scrollback Seq.|> line
                in s { scrollback = Seq.drop (Seq.length sb' - limit) sb' }
    popW <- wrapPopline $ \ncols cellsPtr -> do
        s <- readIORef stateR
        case (s.modeFlags.altScreen, Seq.viewr s.scrollback) of
            (False, rest Seq.:> line) -> do
                writeIORef stateR s { scrollback = rest }
                let padded = take (fromIntegral ncols) (V.toList line ++ repeat blankCell)
                allocaBytes #{size HatCell} $ \hc ->
                    forM_ (zip [0 ..] padded) $ \(i, cell) -> do
                        pokeHatCell hc cell
                        c_unflatten_cell_at cellsPtr i hc
                pure 1
            _ -> pure 0
    outputW <- wrapOutput $ \str len -> do
        bs <- B.packCStringLen (str, fromIntegral len)
        modifyIORef' stateR $ \s -> s { output = bs : s.output }

    cbs <- mallocBytes #{size HatCallbacks}
    #{poke HatCallbacks, damage} cbs damageW
    #{poke HatCallbacks, movecursor} cbs moveW
    #{poke HatCallbacks, settermprop_bool} cbs propBoolW
    #{poke HatCallbacks, settermprop_int} cbs propIntW
    #{poke HatCallbacks, settermprop_str} cbs propStrW
    #{poke HatCallbacks, bell} cbs bellW
    #{poke HatCallbacks, sb_pushline} cbs pushW
    #{poke HatCallbacks, sb_popline} cbs popW
    #{poke HatCallbacks, output} cbs outputW
    c_hat_setup vtp cbs
    c_screen_reset scr 1

    -- Release every C resource once the emulator becomes unreachable, so
    -- a closed pane's libvterm object is never leaked and never freed
    -- while a render still holds it.
    let funptrs =
            [ castFunPtr damageW, castFunPtr moveW, castFunPtr propBoolW
            , castFunPtr propIntW, castFunPtr propStrW, castFunPtr bellW
            , castFunPtr pushW, castFunPtr popW, castFunPtr outputW
            ]
    vtFP <- FC.newForeignPtr vtp $ do
        c_vterm_free vtp
        free cbs
        mapM_ freeHaskellFunPtr funptrs

    let e = Emulator
            { vt = vtFP
            , screen = scr
            , lock = lockVar
            , scrollbackLimit = limitR
            , state = stateR
            }
    refreshGrid e
    pure e

-- | Record that libvterm reported a terminal property hat does not handle,
-- so the unhandled prop is surfaced to the reader loop (which logs it)
-- rather than silently dropped. Runs inside a libvterm callback, so it
-- only touches the bare state IORef.
emitUnknownProp :: IORef EmulatorState -> PropKind -> CInt -> IO ()
emitUnknownProp stateR kind prop =
    modifyIORef' stateR $ \s ->
        s { events = UnknownProp kind (fromIntegral prop) : s.events }

-- | Feed pty output into the emulator; returns what happened. Query events
-- and 'Output' replies are emitted in stream order: apps fence their color
-- probes with DA/CPR queries and match replies to probes by arrival order,
-- so hat must answer serially like a real terminal would.
feed :: Emulator -> ByteString -> IO [Event]
feed e bs0 = withMVar e.lock $ \_ -> do
    s0 <- readIORef e.state
    let (pass1, depassed, passPayloads) = scrubPassthrough s0.passthrough bs0
        (stitle1, scrubbed, stitles) = scrubStitle s0.screenTitle depassed
        screenTitles = map TE.decodeUtf8Lenient stitles
        latestTitle = case screenTitles of
            [] -> s0.title
            ts -> last ts
    -- Store the scrubber states and the newest screen title, and clear the
    -- accumulators the callbacks are about to fill.
    modifyIORef' e.state $ \s -> s
        { passthrough = pass1
        , screenTitle = stitle1
        , title = latestTitle
        , events = []
        , output = []
        , damage = []
        , dirty = False
        }
    -- Split each passthrough payload into the host queries hat answers (a
    -- wrapped OSC 10/11 or DEC 2031 query, yielding the same events an inline
    -- query would) and the leftover it does not handle, surfaced as an
    -- UnhandledPassthrough so the reader logs it rather than dropping it.
    let parts = map partitionPassthrough passPayloads
        unhandled = [UnhandledPassthrough r | (_, r) <- parts, not (B.null r)]
    passEvs <- concat <$> mapM (applySignal e) (concatMap fst parts)
    interleaved <- withForeignPtr e.vt $ \vtp -> do
        ievs <- feedSegments e vtp scrubbed
        ievs <$ c_flush_damage e.screen
    applyDamage e
    s1 <- readIORef e.state
    pure $ passEvs
        <> unhandled
        <> map TitleChanged screenTitles
        <> interleaved
        <> reverse s1.events
        <> [ScreenChanged | s1.dirty]

-- | Feed a chunk to libvterm piecewise, splitting at each color query hat
-- answers itself, so the query's event lands between the 'Output' replies
-- libvterm generates for the bytes before and after it.
feedSegments :: Emulator -> Ptr CVTerm -> ByteString -> IO [Event]
feedSegments e vtp = go
  where
    go bs = case nextQuery bs of
        Nothing -> writeSeg bs
        Just (before, sig, rest) -> do
            outEvs <- writeSeg before
            sigEvs <- applySignal e sig
            ((outEvs <> sigEvs) <>) <$> go rest
    writeSeg seg
        | B.null seg = pure []
        | otherwise = do
            _ <- BU.unsafeUseAsCStringLen seg $ \(p, n) ->
                c_vterm_input_write vtp p (fromIntegral n)
            s <- readIORef e.state
            writeIORef e.state (s { output = [] })
            let outs = dropDecxcprReply (B.concat (reverse s.output))
            pure [Output outs | not (B.null outs)]

-- | Turn a host query hat answers itself into its 'Event', recording the DEC
-- 2031 subscription a @CSI ? 2031 h@\/@l@ toggles so a scheme change knows to
-- report to this pane (see 'Hat.Server.notifySubscribedPanes'). The same
-- answer serves a query the inner app sent inline and one it wrapped in tmux
-- passthrough (see 'Hat.Term.HostProtocol.honoredSignals').
applySignal :: Emulator -> QuerySignal -> IO [Event]
applySignal e sig = case sig of
    SigColor CsEnable  -> [ColorSchemeQuery]
        <$ modifyIORef' e.state (\s -> s { modeFlags = s.modeFlags { colorReport = True } })
    SigColor CsDisable -> []
        <$ modifyIORef' e.state (\s -> s { modeFlags = s.modeFlags { colorReport = False } })
    SigColor CsQuery   -> pure [ColorSchemeQuery]
    SigOsc target term -> pure [OscColorQuery target term]
    SigNotify raw      -> pure [DesktopNotification raw]

-- | Encode a cursor key the way this pane currently expects it: libvterm
-- consults its own DECCKM state, so @man@/@less@ (application cursor keys)
-- get @\\ESC O A@ while normal mode gets @\\ESC [ A@.
encodeKey :: Emulator -> CursorKey -> IO ByteString
encodeKey e key = withMVar e.lock $ \_ -> do
    modifyIORef' e.state (\s -> s { output = [] })
    withForeignPtr e.vt $ \vtp ->
        c_vterm_keyboard_key vtp (keyCode key) #{const VTERM_MOD_NONE}
    B.concat . reverse . (.output) <$> readIORef e.state
  where
    keyCode k = case k of
        CursorUp    -> #{const VTERM_KEY_UP}
        CursorDown  -> #{const VTERM_KEY_DOWN}
        CursorLeft  -> #{const VTERM_KEY_LEFT}
        CursorRight -> #{const VTERM_KEY_RIGHT}
        CursorHome  -> #{const VTERM_KEY_HOME}
        CursorEnd   -> #{const VTERM_KEY_END}

-- | Resize the terminal, flushing the damage libvterm reports as it reflows
-- and rebuilding the cached grid at the new size.
resize :: Emulator -> Size -> IO ()
resize e sz = withMVar e.lock $ \_ -> do
    withForeignPtr e.vt $ \vtp -> do
        -- Neutralize the pen across the resize. libvterm erases the cells it
        -- reflows -- in the primary AND alternate buffers, which share one pen
        -- -- with the live pen, so a full-screen app's background would bleed
        -- onto the primary screen behind it. Erase with the default pen (as a
        -- real terminal does), then restore the app's pen for its next output.
        saved <- allocaBytes #{size HatCell} $ \hc -> do
            _ <- c_hat_get_pen vtp hc
            (.style) <$> peekHatCell hc
        writeParser vtp (cellSgr defaultStyle)
        c_vterm_set_size vtp (fromIntegral sz.rows) (fromIntegral sz.cols)
        writeParser vtp (cellSgr saved)
        c_flush_damage e.screen
    modifyIORef' e.state $ \s -> s { view = s.view { size = sz }, damage = [] }
    refreshGrid e
  where
    writeParser vtp bs = BU.unsafeUseAsCStringLen bs $ \(p, n) ->
        () <$ c_vterm_input_write vtp p (fromIntegral n)

-- | Take an immutable 'Screen' of the visible grid, cursor position, and
-- cursor visibility as they stand now.
snapshot :: Emulator -> IO Screen
snapshot e = withMVar e.lock $ \_ -> (.view) <$> readIORef e.state

-- | The emulator's live pen: the style the next glyph written would take.
-- Captured across a reload ('Hat.Server.captureReloadScreen') so a program's
-- echoed input keeps its colour instead of inheriting the last painted cell's.
currentPen :: Emulator -> IO Style
currentPen e = withMVar e.lock $ \_ -> withForeignPtr e.vt $ \vtp ->
    allocaBytes #{size HatCell} $ \hc -> do
        _ <- c_hat_get_pen vtp hc
        (.style) <$> peekHatCell hc

-- | The mode flags apps have toggled: alternate screen, mouse tracking,
-- focus reporting, and color-scheme reporting.
modes :: Emulator -> IO Modes
modes e = (.modeFlags) <$> readIORef e.state

-- | The current window title, as last set by an OSC 0\/2 or the screen\/tmux
-- @ESC k@ escape.
title :: Emulator -> IO Text
title e = (.title) <$> readIORef e.state

-- | The current OSC 1 icon name (VTERM_PROP_ICONNAME), as last set.
iconName :: Emulator -> IO Text
iconName e = (.iconName) <$> readIORef e.state

-- | How many scrollback lines are currently retained.
scrollbackLength :: Emulator -> IO Int
scrollbackLength e = Seq.length . (.scrollback) <$> readIORef e.state

-- | Scrollback line by age: 0 is the oldest.
scrollbackLine :: Emulator -> Int -> IO (Maybe (V.Vector Cell))
scrollbackLine e i = Seq.lookup i . (.scrollback) <$> readIORef e.state

-- | Change the scrollback cap of an already-open emulator, trimming any
-- lines already over the new limit. Future scrolled-off lines are capped at
-- the new value too. Backs a live @history-limit@ change for open panes.
setScrollbackLimit :: Emulator -> Int -> IO ()
setScrollbackLimit e limit = do
    writeIORef e.scrollbackLimit limit
    modifyIORef' e.state $ \s ->
        s { scrollback = Seq.drop (Seq.length s.scrollback - limit) s.scrollback }

-- | Drop all scrollback (the live screen is untouched). Backs
-- @clear-history@.
clearScrollback :: Emulator -> IO ()
clearScrollback e = modifyIORef' e.state $ \s -> s { scrollback = Seq.empty }

-- | Seed a fresh emulator's scrollback with captured history lines (oldest
-- first), trimmed to the current limit. The reload-restore companion to
-- 'restoreBytes': scrollback lives here, not in libvterm, so it is restored by
-- setting the sequence directly rather than by replaying bytes.
seedScrollback :: Emulator -> [V.Vector Cell] -> IO ()
seedScrollback e ls = do
    limit <- readIORef e.scrollbackLimit
    modifyIORef' e.state $ \s ->
        s { scrollback = Seq.fromList (drop (length ls - limit) ls) }

-- internal --

-- | Re-read every cell touched by accumulated damage into the grid cache.
applyDamage :: Emulator -> IO ()
applyDamage e = do
    s <- readIORef e.state
    case s.damage of
        [] -> pure ()
        rects -> do
            grid' <- foldM (applyRect e s.view.size) s.view.cells rects
            modifyIORef' e.state $ \s' ->
                s' { view = s'.view { cells = grid' }, damage = [] }

-- | Refresh the grid cache over one damaged rectangle, clamped to the
-- current screen size, by re-reading each covered cell from libvterm.
applyRect :: Emulator -> Size
          -> V.Vector (V.Vector Cell) -> Rect -> IO (V.Vector (V.Vector Cell))
applyRect e sz grid rect = do
    let r0 = max 0 rect.startRow
        r1 = min (fromIntegral sz.rows) rect.endRow - 1
        c0 = max 0 rect.startCol
        c1 = min (fromIntegral sz.cols) rect.endCol - 1
    updated <- forM [r0 .. r1] $ \r -> do
        newCells <- forM [c0 .. c1] $ \c -> readCell e r c
        let old = fromMaybe (V.replicate (fromIntegral sz.cols) blankCell)
                            (grid V.!? r)
        pure (r, old V.// zip [c0 .. c1] newCells)
    pure (grid V.// updated)

-- | Rebuild the whole grid cache from libvterm's current screen, used after
-- a resize where per-cell damage can't be relied on.
refreshGrid :: Emulator -> IO ()
refreshGrid e = do
    s <- readIORef e.state
    grid <- V.generateM (fromIntegral s.view.size.rows) $ \r ->
        V.generateM (fromIntegral s.view.size.cols) $ \c -> readCell e r c
    modifyIORef' e.state $ \s' -> s' { view = s'.view { cells = grid } }

-- | Read the single cell at (row, col) from libvterm's screen into a 'Cell'.
readCell :: Emulator -> Int -> Int -> IO Cell
readCell e r c = withForeignPtr e.vt $ \_ ->
    allocaBytes #{size HatCell} $ \hc -> do
        _ <- c_hat_get_cell e.screen (fromIntegral r) (fromIntegral c) hc
        peekHatCell hc

-- | Marshal one 'CHatCell' the shim just filled into a 'Cell': decode the
-- code-point array -- honoring the wide-char continuation sentinel and the
-- blank-as-space case -- the cell width, the foreground and background
-- colors by kind (indexed, RGB, or terminal default), and the attribute
-- bitmask.
peekHatCell :: Ptr CHatCell -> IO Cell
peekHatCell p = do
    chars <- peekArray #{const VTERM_MAX_CHARS_PER_CELL}
        (#{ptr HatCell, chars} p) :: IO [Word32]
    w <- #{peek HatCell, width} p :: IO CInt
    flags <- #{peek HatCell, flags} p :: IO CUInt
    fgKind <- #{peek HatCell, fg_kind} p :: IO CInt
    fgIdx <- #{peek HatCell, fg_idx} p :: IO CInt
    fgR <- #{peek HatCell, fg_r} p :: IO CInt
    fgG <- #{peek HatCell, fg_g} p :: IO CInt
    fgB <- #{peek HatCell, fg_b} p :: IO CInt
    bgKind <- #{peek HatCell, bg_kind} p :: IO CInt
    bgIdx <- #{peek HatCell, bg_idx} p :: IO CInt
    bgR <- #{peek HatCell, bg_r} p :: IO CInt
    bgG <- #{peek HatCell, bg_g} p :: IO CInt
    bgB <- #{peek HatCell, bg_b} p :: IO CInt
    let cps = takeWhile (\x -> x /= 0 && x <= 0x10FFFF) chars
        continuation = case chars of
            (x : _) -> x == 0xFFFFFFFF
            [] -> False
        txt
            | continuation = ""
            | null cps = " "
            | otherwise = T.pack (map (chr . fromIntegral) cps)
        color kind idx rr gg bb
            | kind == 1 = Indexed (fromIntegral idx)
            | kind == 2 = RGB (fromIntegral rr) (fromIntegral gg) (fromIntegral bb)
            | otherwise = DefaultColor
        has mask = flags .&. mask /= 0
    pure $! Cell
        { text = txt
        , width = if continuation then 0 else fromIntegral (max 1 w)
        , style = Style
            { fg = color fgKind fgIdx fgR fgG fgB
            , bg = color bgKind bgIdx bgR bgG bgB
            , bold = has 1
            , underline = has 2
            , italic = has 4
            , reverse = has 8
            , strike = has 16
            , blink = has 32
            , faint = has 64
            }
        }

-- | Marshal a 'Cell' into a 'CHatCell' buffer, the inverse of 'peekHatCell',
-- so a stored scrollback line can be handed back to libvterm as it reflows on
-- resize. A zero-width cell is re-encoded with the 0xFFFFFFFF wide-char
-- continuation sentinel 'peekHatCell' recognizes.
pokeHatCell :: Ptr CHatCell -> Cell -> IO ()
pokeHatCell p cell = do
    let cps
            | cell.width == 0 = [0xFFFFFFFF]
            | otherwise = map (fromIntegral . ord) (T.unpack cell.text)
    pokeArray (#{ptr HatCell, chars} p)
        (take #{const VTERM_MAX_CHARS_PER_CELL} (cps ++ repeat 0) :: [Word32])
    #{poke HatCell, width} p (fromIntegral cell.width :: CInt)
    let s = cell.style
        onFlag b m = if b then m else 0
        flags = onFlag s.bold 1 .|. onFlag s.underline 2 .|. onFlag s.italic 4
            .|. onFlag s.reverse 8 .|. onFlag s.strike 16 .|. onFlag s.blink 32
            .|. onFlag s.faint 64
    #{poke HatCell, flags} p (flags :: CUInt)
    let (fgK, fgI, fgR, fgG, fgB) = colorFields s.fg
        (bgK, bgI, bgR, bgG, bgB) = colorFields s.bg
    #{poke HatCell, fg_kind} p fgK
    #{poke HatCell, fg_idx} p fgI
    #{poke HatCell, fg_r} p fgR
    #{poke HatCell, fg_g} p fgG
    #{poke HatCell, fg_b} p fgB
    #{poke HatCell, bg_kind} p bgK
    #{poke HatCell, bg_idx} p bgI
    #{poke HatCell, bg_r} p bgR
    #{poke HatCell, bg_g} p bgG
    #{poke HatCell, bg_b} p bgB

-- | A 'Color' split into the @(kind, idx, r, g, b)@ fields of a 'CHatCell',
-- matching the encoding 'peekHatCell' decodes: kind 0 default, 1 indexed, 2
-- rgb.
colorFields :: Color -> (CInt, CInt, CInt, CInt, CInt)
colorFields DefaultColor = (0, 0, 0, 0, 0)
colorFields (Indexed i)  = (1, fromIntegral i, 0, 0, 0)
colorFields (RGB r g b)  = (2, 0, fromIntegral r, fromIntegral g, fromIntegral b)
