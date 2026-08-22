{-# LANGUAGE StrictData #-}

-- | The pure surface of the terminal emulator: the event and mode vocabulary
-- it reports in, the immutable 'Screen' view, and the byte-synthesizers that
-- turn captured state back into the VT stream a fresh emulator replays on
-- reload. None of this touches libghostty; the stateful wrapper in
-- "Hat.Term.Emulator" imports it.
module Hat.Term.Emulator.Types
    ( Event (..)
    , PropKind (..)
    , OscColorTarget (..)
    , OscTerm (..)
    , Screen (..)
    , Modes (..)
    , MouseMode (..)
    , CursorKey (..)
    , modeReplayBytes
    , restoreBytes
    , cellSgr
    , paintRow
    , rtrimBlank
    , screenRowText
    , screenCell
    , blankGrid
    ) where

import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V

import Hat.Geometry
import Hat.Term.Cell
import Hat.Term.HostProtocol (OscColorTarget (..), OscTerm (..))

data Event
    = TitleChanged Text
    | Bell
    | Output B.ByteString  -- ^ bytes to write back to the pty
    | ColorSchemeQuery   -- ^ app asked the current light/dark scheme (CSI ? 996 n)
    | OscColorQuery OscColorTarget OscTerm
        -- ^ app asked a terminal color (OSC 10/11 @;?@); answer with the
        --   same terminator the query used
    | DesktopNotification B.ByteString
        -- ^ app raised a desktop notification (OSC 9 / OSC 777), captured
        --   verbatim to forward to the outer terminal
    | ScreenChanged
    | UnknownProp PropKind Int
        -- ^ the backend reported a terminal property hat does not handle,
        --   tagged with the value's kind and the backend's prop number
    | UnhandledPassthrough B.ByteString
        -- ^ a DCS tmux passthrough payload hat neither answers nor forwards
        --   (OSC 52 clipboard, OSC 12 cursor color, OSC 4 palette, …),
        --   surfaced so the reader logs it instead of dropping it silently
    deriving (Eq, Show)

-- | The value kind of a terminal property, distinguishing the three
-- @settermprop@ callbacks. See the 'UnknownProp' branches in the backend.
data PropKind = PropBool | PropInt | PropStr
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
    , colorReport :: Bool  -- ^ app enabled color-scheme reporting (?2031)
    }
    deriving (Eq, Show)

-- | Immutable view of the visible grid.
data Screen = Screen
    { size          :: Size
    , cells         :: V.Vector (V.Vector Cell)
    , cursor        :: Pos
    , cursorVisible :: Bool
    }

-- | The DECSET bytes that re-establish an app's mode subscriptions in a fresh
-- emulator — feed them into the one an in-place reload adopts a program into,
-- and its ?2031\/?1004\/mouse subscriptions come back. @altScreen@ is omitted:
-- it is screen-buffer state, not a subscription, and 'restoreBytes' handles it
-- alongside the buffer contents.
modeReplayBytes :: Modes -> B.ByteString
modeReplayBytes m = B.concat $
       mouseSeq m.mouse
    ++ [ "\ESC[?1004h" | m.focusReport ]
    ++ [ "\ESC[?2031h" | m.colorReport ]
  where
    mouseSeq MouseOff   = []
    mouseSeq MouseClick = [ "\ESC[?1000h" ]
    mouseSeq MouseDrag  = [ "\ESC[?1002h" ]
    mouseSeq MouseMove  = [ "\ESC[?1003h" ]

-- | Synthesize the bytes that repaint a captured 'Screen' into a fresh
-- emulator of the same size: enter the alternate screen first when the capture
-- was in it (so a later ?1049l reverts as the app expects, rather than
-- stranding the frame), then, row by row, position the cursor and emit each
-- cell's text under an absolute SGR. Feeding this into a fresh emulator
-- reproduces the visible grid — the replay half of reload's live-screen
-- restore. The cursor lands where the capture left it.
restoreBytes :: Modes -> Style -> Screen -> B.ByteString
restoreBytes m pen scr = BL.toStrict $ BB.toLazyByteString $
       (if m.altScreen then BB.byteString "\ESC[?1049h" else mempty)
    <> BB.byteString "\ESC[0m\ESC[H"
    <> foldMap rowBytes [0 .. V.length scr.cells - 1]
    -- Restore the live pen after the grid, so the program's next output
    -- (an echoed keystroke after restart-server) takes its captured colour,
    -- not the last painted cell's. 'cellSgr' of the default pen is "\ESC[0m".
    <> BB.byteString (cellSgr pen)
    <> moveTo scr.cursor
    <> BB.byteString (if scr.cursorVisible then "\ESC[?25h" else "\ESC[?25l")
  where
    -- A fresh grid is already blank, so trailing default-blank cells need no
    -- repaint; the leading "\ESC[0m" makes each row start from a known pen.
    rowBytes r = case scr.cells V.!? r of
        Nothing  -> mempty
        Just row -> case rtrimBlank (V.toList row) of
            []    -> mempty
            cells -> moveTo (Pos r 0) <> BB.byteString "\ESC[0m" <> paintRow defaultStyle cells

rtrimBlank :: [Cell] -> [Cell]
rtrimBlank = reverse . dropWhile (== blankCell) . reverse

-- Emit each cell's text, prefixing an absolute SGR only when the pen changes,
-- and skipping the continuation columns a wide cell occupies.
paintRow :: Style -> [Cell] -> BB.Builder
paintRow _ [] = mempty
paintRow pen (c : cs) =
    let penB = if c.style == pen then mempty else BB.byteString (cellSgr c.style)
        rest = drop (max 0 (c.width - 1)) cs
    in penB <> BB.byteString (TE.encodeUtf8 c.text) <> paintRow c.style rest

-- | Absolute CUP to a 0-based position (VT rows\/cols are 1-based).
moveTo :: Pos -> BB.Builder
moveTo p = BB.byteString "\ESC["
    <> BB.intDec (p.row + 1) <> BB.char8 ';'
    <> BB.intDec (p.col + 1) <> BB.char8 'H'

-- | Absolute SGR for a style: reset then set, so each run stands alone.
-- Mirrors 'Hat.Client.Draw.sgr' (client-out); kept separate to avoid the
-- emulator depending on the client layer.
cellSgr :: Style -> B.ByteString
cellSgr st = BL.toStrict $ BB.toLazyByteString $
    BB.byteString "\ESC[0"
    <> flag st.bold 1
    <> flag st.faint 2
    <> flag st.italic 3
    <> flag st.underline 4
    <> flag st.blink 5
    <> flag st.reverse 7
    <> flag st.strike 9
    <> colorCode 30 38 st.fg
    <> colorCode 40 48 st.bg
    <> BB.char8 'm'
  where
    flag b n = if b then BB.char8 ';' <> BB.intDec n else mempty

-- | An SGR color parameter for one channel: @base@ is the 8-color offset
-- (30 fg, 40 bg), @ext@ the 256\/truecolor selector (38 fg, 48 bg).
colorCode :: Int -> Int -> Color -> BB.Builder
colorCode base ext = \case
    DefaultColor -> mempty
    Indexed n
        | n < 8     -> BB.char8 ';' <> BB.intDec (base + fromIntegral n)
        | otherwise -> sep <> BB.intDec ext <> BB.byteString ";5;"
                           <> BB.intDec (fromIntegral n)
    RGB r g b -> sep <> BB.intDec ext <> BB.byteString ";2;"
        <> BB.intDec (fromIntegral r) <> BB.char8 ';'
        <> BB.intDec (fromIntegral g) <> BB.char8 ';'
        <> BB.intDec (fromIntegral b)
  where
    sep = BB.char8 ';'

-- | Concatenate the text of every cell in a screen row; @\"\"@ for a row
-- index past the bottom of the grid.
screenRowText :: Screen -> Int -> Text
screenRowText scr r = case scr.cells V.!? r of
    Nothing -> ""
    Just row -> T.concat [c.text | c <- V.toList row]

-- | The cell at a position, or 'blankCell' when the position is off-screen.
screenCell :: Screen -> Pos -> Cell
screenCell scr p = fromMaybe blankCell $ do
    row <- scr.cells V.!? p.row
    row V.!? p.col

-- | A grid of the given size filled entirely with 'blankCell'.
blankGrid :: Size -> V.Vector (V.Vector Cell)
blankGrid sz = V.replicate (fromIntegral sz.rows)
    (V.replicate (fromIntegral sz.cols) blankCell)
