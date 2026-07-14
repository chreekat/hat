-- | The terminal emulator: libvterm wrapped behind a narrow interface.
--
-- Each pane owns one 'Emulator' and is touched by one thread at a time;
-- an internal lock also makes 'feed' / 'snapshot' / 'resize' safe to call
-- concurrently. libvterm's callbacks fire synchronously inside
-- @vterm_input_write@ and land in IORefs owned by this module.
module Hat.Term.Emulator
    ( Emulator
    , Event (..)
    , Screen (..)
    , Modes (..)
    , MouseMode (..)
    , CursorKey (..)
    , newEmulator
    , feed
    , encodeKey
    , resize
    , snapshot
    , modes
    , title
    , scrollbackLength
    , scrollbackLine
    , clearScrollback
    , screenRowText
    , screenCell
    ) where

#include "hat_shim.h"

import Control.Concurrent.MVar
import Control.Monad (foldM, forM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Char (chr)
import Data.IORef
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
import Hat.Term.Cell

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
foreign import ccall unsafe "hat_flatten_cell_at"
    c_flatten_cell_at :: Ptr CVTermCells -> CInt -> Ptr CHatCell -> IO ()

type DamageFn = CInt -> CInt -> CInt -> CInt -> IO ()
type MoveCursorFn = CInt -> CInt -> CInt -> IO ()
type PropBoolFn = CInt -> CInt -> IO ()
type PropIntFn = CInt -> CInt -> IO ()
type PropStrFn = CInt -> CString -> CSize -> CInt -> IO ()
type BellFn = IO ()
type PushlineFn = CInt -> Ptr CVTermCells -> IO ()
type OutputFn = CString -> CSize -> IO ()

foreign import ccall "wrapper" wrapDamage :: DamageFn -> IO (FunPtr DamageFn)
foreign import ccall "wrapper" wrapMoveCursor :: MoveCursorFn -> IO (FunPtr MoveCursorFn)
foreign import ccall "wrapper" wrapPropBool :: PropBoolFn -> IO (FunPtr PropBoolFn)
foreign import ccall "wrapper" wrapPropInt :: PropIntFn -> IO (FunPtr PropIntFn)
foreign import ccall "wrapper" wrapPropStr :: PropStrFn -> IO (FunPtr PropStrFn)
foreign import ccall "wrapper" wrapBell :: BellFn -> IO (FunPtr BellFn)
foreign import ccall "wrapper" wrapPushline :: PushlineFn -> IO (FunPtr PushlineFn)
foreign import ccall "wrapper" wrapOutput :: OutputFn -> IO (FunPtr OutputFn)

data Event
    = TitleChanged Text
    | Bell
    | Output ByteString  -- ^ bytes to write back to the pty
    | ScreenChanged
    deriving (Eq, Show)

data MouseMode = MouseOff | MouseClick | MouseDrag | MouseMove
    deriving (Eq, Show)

-- | Keys whose byte encoding depends on terminal mode (DECCKM), so the
-- pane must be asked how to encode them rather than forwarding raw bytes.
data CursorKey
    = CursorUp | CursorDown | CursorLeft | CursorRight | CursorHome | CursorEnd
    deriving (Eq, Show)

data Modes = Modes
    { altScreen   :: Bool
    , mouse       :: MouseMode
    , focusReport :: Bool  -- ^ app enabled focus reporting (?1004)
    }
    deriving (Eq, Show)

-- | Immutable view of the visible grid.
data Screen = Screen
    { size          :: Size
    , cells         :: V.Vector (V.Vector Cell)
    , cursor        :: Pos
    , cursorVisible :: Bool
    }

data Emulator = Emulator
    { vt        :: ForeignPtr CVTerm
        -- ^ owns the libvterm object: a finalizer frees it, the callback
        --   struct and the callback FunPtrs when the emulator is GC'd, so
        --   a dead pane's C resources are released without a manual call.
    , screen    :: Ptr CVTermScreen  -- ^ borrowed from 'vt', valid while it lives
    , lock      :: MVar ()
    , sizeRef   :: IORef Size
    , gridRef   :: IORef (V.Vector (V.Vector Cell))
    , cursorRef :: IORef Pos
    , curVisRef :: IORef Bool
    , titleRef  :: IORef Text
    , titleAcc  :: IORef ByteString
    , altRef    :: IORef Bool
    , mouseRef  :: IORef MouseMode
    , focusRef  :: IORef Bool
    , dirtyRef  :: IORef Bool
    , damageRef :: IORef [Rect]
    , eventsRef :: IORef [Event]       -- reversed
    , outRef    :: IORef [ByteString]  -- reversed
    , passRef   :: IORef PassState     -- ^ tmux-passthrough scrubber state
    , sbRef     :: IORef (Seq [Cell])
    , sbLimit   :: Int
    }

-- | Scrubber state for DCS tmux passthrough, carried across 'feed' chunks:
-- outside a wrapper (holding back a partial @ESC Ptmux;@ prefix that ends
-- the chunk), or inside one (discarding until its ST).
data PassState
    = Outside ByteString  -- ^ carry: proper prefix of @ESC Ptmux;@ at chunk end
    | Inside ByteString   -- ^ carry: a trailing @ESC@ that may start the ST

newEmulator :: Size -> Int -> IO Emulator
newEmulator sz historyLimit = do
    vtp <- c_vterm_new (fromIntegral sz.rows) (fromIntegral sz.cols)
    c_vterm_set_utf8 vtp 1
    scr <- c_vterm_obtain_screen vtp
    c_enable_altscreen scr 1
    c_set_damage_merge scr #{const VTERM_DAMAGE_SCROLL}

    lockVar <- newMVar ()
    sizeR <- newIORef sz
    gridR <- newIORef (blankGrid sz)
    cursorR <- newIORef Pos { row = 0, col = 0 }
    curVisR <- newIORef True
    titleR <- newIORef ""
    titleA <- newIORef ""
    altR <- newIORef False
    mouseR <- newIORef MouseOff
    focusR <- newIORef False
    dirtyR <- newIORef False
    damageR <- newIORef []
    eventsR <- newIORef []
    outR <- newIORef []
    passR <- newIORef (Outside "")
    sbR <- newIORef Seq.empty

    damageW <- wrapDamage $ \sr er sc ec -> do
        modifyIORef' damageR (Rect
            { startRow = fromIntegral sr
            , endRow = fromIntegral er
            , startCol = fromIntegral sc
            , endCol = fromIntegral ec
            } :)
        writeIORef dirtyR True
    moveW <- wrapMoveCursor $ \r c vis -> do
        writeIORef cursorR Pos { row = fromIntegral r, col = fromIntegral c }
        writeIORef curVisR (vis /= 0)
        writeIORef dirtyR True
    propBoolW <- wrapPropBool $ \prop val -> case prop of
        #{const VTERM_PROP_CURSORVISIBLE} -> do
            writeIORef curVisR (val /= 0)
            writeIORef dirtyR True
        #{const VTERM_PROP_ALTSCREEN} -> do
            writeIORef altR (val /= 0)
            writeIORef dirtyR True
        #{const VTERM_PROP_FOCUSREPORT} -> writeIORef focusR (val /= 0)
        _ -> pure ()
    propIntW <- wrapPropInt $ \prop val -> case prop of
        #{const VTERM_PROP_MOUSE} -> writeIORef mouseR $ case val of
            1 -> MouseClick
            2 -> MouseDrag
            3 -> MouseMove
            _ -> MouseOff
        _ -> pure ()
    propStrW <- wrapPropStr $ \prop str len final -> case prop of
        #{const VTERM_PROP_TITLE} -> do
            frag <- B.packCStringLen (str, fromIntegral len)
            modifyIORef' titleA (<> frag)
            if final /= 0
                then do
                    full <- readIORef titleA
                    writeIORef titleA ""
                    let t = TE.decodeUtf8Lenient full
                    writeIORef titleR t
                    modifyIORef' eventsR (TitleChanged t :)
                else pure ()
        _ -> pure ()
    bellW <- wrapBell $ modifyIORef' eventsR (Bell :)
    pushW <- wrapPushline $ \ncols cellsPtr -> do
        line <- forM [0 .. fromIntegral ncols - 1 :: Int] $ \i ->
            allocaBytes #{size HatCell} $ \hc -> do
                c_flatten_cell_at cellsPtr (fromIntegral i) hc
                peekHatCell hc
        modifyIORef' sbR $ \sb ->
            let sb' = sb Seq.|> line
            in Seq.drop (Seq.length sb' - historyLimit) sb'
    outputW <- wrapOutput $ \str len -> do
        bs <- B.packCStringLen (str, fromIntegral len)
        modifyIORef' outR (bs :)

    cbs <- mallocBytes #{size HatCallbacks}
    #{poke HatCallbacks, damage} cbs damageW
    #{poke HatCallbacks, movecursor} cbs moveW
    #{poke HatCallbacks, settermprop_bool} cbs propBoolW
    #{poke HatCallbacks, settermprop_int} cbs propIntW
    #{poke HatCallbacks, settermprop_str} cbs propStrW
    #{poke HatCallbacks, bell} cbs bellW
    #{poke HatCallbacks, sb_pushline} cbs pushW
    #{poke HatCallbacks, output} cbs outputW
    c_hat_setup vtp cbs
    c_screen_reset scr 1

    -- Release every C resource once the emulator becomes unreachable, so
    -- a closed pane's libvterm object is never leaked and never freed
    -- while a render still holds it.
    let funptrs =
            [ castFunPtr damageW, castFunPtr moveW, castFunPtr propBoolW
            , castFunPtr propIntW, castFunPtr propStrW, castFunPtr bellW
            , castFunPtr pushW, castFunPtr outputW
            ]
    vtFP <- FC.newForeignPtr vtp $ do
        c_vterm_free vtp
        free cbs
        mapM_ freeHaskellFunPtr funptrs

    let e = Emulator
            { vt = vtFP
            , screen = scr
            , lock = lockVar
            , sizeRef = sizeR
            , gridRef = gridR
            , cursorRef = cursorR
            , curVisRef = curVisR
            , titleRef = titleR
            , titleAcc = titleA
            , altRef = altR
            , mouseRef = mouseR
            , focusRef = focusR
            , dirtyRef = dirtyR
            , damageRef = damageR
            , eventsRef = eventsR
            , outRef = outR
            , passRef = passR
            , sbRef = sbR
            , sbLimit = historyLimit
            }
    refreshGrid e
    pure e

-- | Feed pty output into the emulator; returns what happened.
feed :: Emulator -> ByteString -> IO [Event]
feed e bs0 = withMVar e.lock $ \_ -> do
    st0 <- readIORef e.passRef
    let (st1, bs) = scrubPassthrough st0 bs0
    writeIORef e.passRef st1
    writeIORef e.eventsRef []
    writeIORef e.outRef []
    writeIORef e.damageRef []
    writeIORef e.dirtyRef False
    withForeignPtr e.vt $ \vtp -> do
        _ <- BU.unsafeUseAsCStringLen bs $ \(p, n) ->
            c_vterm_input_write vtp p (fromIntegral n)
        c_flush_damage e.screen
    applyDamage e
    dirty <- readIORef e.dirtyRef
    evs <- reverse <$> readIORef e.eventsRef
    outs <- dropDecxcprReply . B.concat . reverse <$> readIORef e.outRef
    pure $ evs
        <> [Output outs | not (B.null outs)]
        <> [ScreenChanged | dirty]

-- | Remove DCS tmux passthrough (@ESC Ptmux; … ESC \\@) from a pane's
-- output before libvterm parses it. A tmux-aware app (claude) sees $TMUX
-- set and wraps sequences meant for the outer terminal in this DCS;
-- libvterm's parser aborts on the wrapper's doubled inner ESCs and spills
-- the payload onto the screen as text (the \"11;?9;4;0;\" garbage). tmux's
-- default (@allow-passthrough off@) ignores these sequences entirely; do
-- the same. A wrapper can span pty reads, so the state carries across
-- 'feed' chunks.
scrubPassthrough :: PassState -> ByteString -> (PassState, ByteString)
scrubPassthrough st0 chunk = case st0 of
    Outside carry -> outside [] (carry <> chunk)
    Inside carry  -> inside [] (carry <> chunk)
  where
    intro = "\ESCPtmux;"
    finish acc = B.concat (reverse acc)
    outside acc bs = case B.breakSubstring intro bs of
        (before, r)
            | B.null r ->
                -- No wrapper here; hold back a chunk-final partial intro
                -- (e.g. a trailing bare ESC) until the next read decides.
                let held = introSuffix before
                    emit = B.take (B.length before - B.length held) before
                in (Outside held, finish (emit : acc))
            | otherwise -> inside (before : acc) (B.drop (B.length intro) r)
    -- Inside the wrapper everything is discarded; only the terminator
    -- matters. The wrapping doubles inner ESCs, so a doubled pair is
    -- content and ST is a lone ESC followed by backslash.
    inside acc bs = case B.elemIndex 0x1b bs of
        Nothing -> (Inside "", finish acc)
        Just i -> case B.uncons (B.drop (i + 1) bs) of
            Nothing           -> (Inside "\ESC", finish acc)
            Just (0x1b, rest) -> inside acc rest
            Just (0x5c, rest) -> outside acc rest
            Just (_, rest)    -> inside acc rest
    -- The longest proper prefix of the intro that this chunk ends with.
    introSuffix bs =
        let cap = min (B.length intro - 1) (B.length bs)
            ks = [ k | k <- [cap, cap - 1 .. 1]
                 , B.take k intro `B.isSuffixOf` bs ]
        in case ks of
            (k : _) -> B.drop (B.length bs - k) bs
            []      -> ""

-- | Strip DEC-private cursor reports (DECXCPR, @CSI ? … R@) from the
-- emulator's replies. Most terminals — ghostty included — ignore
-- @CSI ? 6 n@, so libvterm's answer to it arrives unexpected at the shell
-- when the inner app exits or resumes, and the line editor spills its bare
-- parameters as visible \"9;4;0\" garbage. Plain CPR (@CSI … R@) and every
-- other reply pass through untouched.
dropDecxcprReply :: ByteString -> ByteString
dropDecxcprReply bs =
    case B.breakSubstring "\ESC[?" bs of
        (before, rest)
            | B.null rest -> bs
            | otherwise ->
                let afterIntro = B.drop 3 rest            -- past ESC [ ?
                    (_params, tailB) = B.span isParam afterIntro
                in case B.uncons tailB of
                    Just (0x52, more) -> before <> dropDecxcprReply more
                    _ -> before <> "\ESC[?" <> dropDecxcprReply afterIntro
  where
    isParam b = (b >= 0x30 && b <= 0x39) || b == 0x3b   -- 0-9 or ';'

-- | Encode a cursor key the way this pane currently expects it: libvterm
-- consults its own DECCKM state, so @man@/@less@ (application cursor keys)
-- get @\\ESC O A@ while normal mode gets @\\ESC [ A@.
encodeKey :: Emulator -> CursorKey -> IO ByteString
encodeKey e key = withMVar e.lock $ \_ -> do
    writeIORef e.outRef []
    withForeignPtr e.vt $ \vtp ->
        c_vterm_keyboard_key vtp (keyCode key) #{const VTERM_MOD_NONE}
    B.concat . reverse <$> readIORef e.outRef
  where
    keyCode k = case k of
        CursorUp    -> #{const VTERM_KEY_UP}
        CursorDown  -> #{const VTERM_KEY_DOWN}
        CursorLeft  -> #{const VTERM_KEY_LEFT}
        CursorRight -> #{const VTERM_KEY_RIGHT}
        CursorHome  -> #{const VTERM_KEY_HOME}
        CursorEnd   -> #{const VTERM_KEY_END}

resize :: Emulator -> Size -> IO ()
resize e sz = withMVar e.lock $ \_ -> do
    withForeignPtr e.vt $ \vtp -> do
        c_vterm_set_size vtp (fromIntegral sz.rows) (fromIntegral sz.cols)
        c_flush_damage e.screen
    writeIORef e.sizeRef sz
    writeIORef e.damageRef []
    refreshGrid e

snapshot :: Emulator -> IO Screen
snapshot e = withMVar e.lock $ \_ -> do
    sz <- readIORef e.sizeRef
    grid <- readIORef e.gridRef
    cur <- readIORef e.cursorRef
    vis <- readIORef e.curVisRef
    pure Screen { size = sz, cells = grid, cursor = cur, cursorVisible = vis }

modes :: Emulator -> IO Modes
modes e = Modes <$> readIORef e.altRef <*> readIORef e.mouseRef
    <*> readIORef e.focusRef

title :: Emulator -> IO Text
title e = readIORef e.titleRef

scrollbackLength :: Emulator -> IO Int
scrollbackLength e = Seq.length <$> readIORef e.sbRef

-- | Scrollback line by age: 0 is the oldest.
scrollbackLine :: Emulator -> Int -> IO (Maybe [Cell])
scrollbackLine e i = Seq.lookup i <$> readIORef e.sbRef

-- | Drop all scrollback (the live screen is untouched). Backs
-- @clear-history@.
clearScrollback :: Emulator -> IO ()
clearScrollback e = writeIORef e.sbRef Seq.empty

screenRowText :: Screen -> Int -> Text
screenRowText scr r = case scr.cells V.!? r of
    Nothing -> ""
    Just row -> T.concat [c.text | c <- V.toList row]

screenCell :: Screen -> Pos -> Cell
screenCell scr p = fromMaybe blankCell $ do
    row <- scr.cells V.!? p.row
    row V.!? p.col

-- internal --

blankGrid :: Size -> V.Vector (V.Vector Cell)
blankGrid sz = V.replicate (fromIntegral sz.rows)
    (V.replicate (fromIntegral sz.cols) blankCell)

-- Re-read every cell touched by accumulated damage into the grid cache.
applyDamage :: Emulator -> IO ()
applyDamage e = do
    rects <- readIORef e.damageRef
    writeIORef e.damageRef []
    case rects of
        [] -> pure ()
        _ -> do
            sz <- readIORef e.sizeRef
            grid <- readIORef e.gridRef
            grid' <- foldM (applyRect e sz) grid rects
            writeIORef e.gridRef grid'

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

refreshGrid :: Emulator -> IO ()
refreshGrid e = do
    sz <- readIORef e.sizeRef
    grid <- V.generateM (fromIntegral sz.rows) $ \r ->
        V.generateM (fromIntegral sz.cols) $ \c -> readCell e r c
    writeIORef e.gridRef grid

readCell :: Emulator -> Int -> Int -> IO Cell
readCell e r c = withForeignPtr e.vt $ \_ ->
    allocaBytes #{size HatCell} $ \hc -> do
        _ <- c_hat_get_cell e.screen (fromIntegral r) (fromIntegral c) hc
        peekHatCell hc

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
    pure Cell
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
            }
        }
