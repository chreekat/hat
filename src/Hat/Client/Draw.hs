-- | Turn 'DrawOp's into the escape-sequence bytes the client blits to
-- its tty. Pure; see 'opsToAnsi'.
module Hat.Client.Draw
    ( opsToAnsi
    , sgr
    , enterAltScreen
    , leaveAltScreen
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE

import Hat.Geometry
import Hat.Term.Cell
import Hat.Transport.Wire (DrawOp (..))

-- | Each batch is bracketed in DEC 2026 synchronized output (BSU/ESU),
-- so the terminal presents the whole frame at once — a full redraw's
-- clear-then-repaint would otherwise flicker — and in hide-cursor /
-- restore-cursor so the cursor never trails the paint.
opsToAnsi :: [DrawOp] -> ByteString
opsToAnsi ops = BL.toStrict . BB.toLazyByteString $
    BB.byteString "\ESC[?2026h\ESC[?25l"
    <> foldMap opBuilder ops
    <> restoreCursor
    <> BB.byteString "\ESC[?2026l"
  where
    -- The renderer puts CursorAt last; if a batch lacks one (pure
    -- content update), leave the cursor hidden state to the final op
    -- of the previous batch by showing it again unconditionally only
    -- when some CursorAt asked for it.
    restoreCursor = case [v | CursorAt _ v <- ops] of
        [] -> mempty
        vs | last vs -> BB.byteString "\ESC[?25h"
           | otherwise -> mempty

opBuilder :: DrawOp -> BB.Builder
opBuilder = \case
    ClearAll -> BB.byteString "\ESC[0m\ESC[2J\ESC[H"
    Put pos st txt ->
        moveTo pos <> BB.byteString (sgr st) <> BB.byteString (TE.encodeUtf8 txt)
    CursorAt pos _visible -> moveTo pos

moveTo :: Pos -> BB.Builder
moveTo p = BB.byteString "\ESC["
    <> BB.intDec (p.row + 1) <> BB.char8 ';'
    <> BB.intDec (p.col + 1) <> BB.char8 'H'

-- | Full SGR for a style, starting from reset so runs are independent.
sgr :: Style -> ByteString
sgr st = BL.toStrict . BB.toLazyByteString $
    BB.byteString "\ESC[0"
    <> flag st.bold 1
    <> flag st.faint 2
    <> flag st.italic 3
    <> flag st.underline 4
    <> flag st.blink 5
    <> flag st.reverse 7
    <> flag st.strike 9
    <> fgCode st.fg
    <> bgCode st.bg
    <> BB.char8 'm'
  where
    flag b n = if b then BB.char8 ';' <> BB.intDec n else mempty

fgCode :: Color -> BB.Builder
fgCode = \case
    DefaultColor -> mempty
    Indexed n
        | n < 8 -> BB.char8 ';' <> BB.intDec (30 + fromIntegral n)
        | otherwise -> BB.byteString ";38;5;" <> BB.intDec (fromIntegral n)
    RGB r g b -> BB.byteString ";38;2;"
        <> BB.intDec (fromIntegral r) <> BB.char8 ';'
        <> BB.intDec (fromIntegral g) <> BB.char8 ';'
        <> BB.intDec (fromIntegral b)

bgCode :: Color -> BB.Builder
bgCode = \case
    DefaultColor -> mempty
    Indexed n
        | n < 8 -> BB.char8 ';' <> BB.intDec (40 + fromIntegral n)
        | otherwise -> BB.byteString ";48;5;" <> BB.intDec (fromIntegral n)
    RGB r g b -> BB.byteString ";48;2;"
        <> BB.intDec (fromIntegral r) <> BB.char8 ';'
        <> BB.intDec (fromIntegral g) <> BB.char8 ';'
        <> BB.intDec (fromIntegral b)

-- Also enables focus reporting (@?1004@) so the outer terminal sends
-- @\ESC[I@/@\ESC[O@; the server forwards them to panes when
-- @focus-events@ is on.
enterAltScreen :: ByteString
enterAltScreen = "\ESC[?1049h\ESC[?1004h\ESC[2J\ESC[H"

-- Also resets the cursor colour (OSC 112) in case a pane's
-- @cursor-colour@ was mirrored onto the outer terminal.
leaveAltScreen :: ByteString
leaveAltScreen = "\ESC[?1004l\ESC[?1049l\ESC[0m\ESC[?25h\ESC]112\a"
