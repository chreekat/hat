-- | The message line: a @display-message@ (or error) message and the
-- command prompt drawn over the status line's @message-line@, as tmux's
-- @status_message_redraw@ and @prompt_draw@ lay them out through
-- @message-format@ and @message-style@ (@message-command-style@ while a
-- vi-mode prompt is in command mode).
module Hat.Server.Message
    ( messageArea
    , drawMessage
    , PromptView (..)
    , drawPrompt
    , inputCells
    , quotedInput
    ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

import Hat.Model (Markup (..))
import Hat.Model.Options
import Hat.Server.Format (FormatCtx (..), evaluateCtx, renderFormatCtx)
import Hat.Server.FormatDraw
import Hat.Server.Status (styleOption)
import Hat.TextWidth (charWidth, textWidth)
import Hat.Term.Cell qualified as Cell

-- | tmux's @status_message_area@: the column and width the message or
-- prompt takes on a @width@-cell line, from @message-style@'s @width=@ and
-- @align=@ as written (a style needing format expansion takes the line).
messageArea :: Options -> Int -> (Int, Int)
messageArea opts width = case styleParse Cell.defaultStyle defaultDrawStyle raw of
    Nothing -> (0, width)
    Just sy ->
        let w0 = case sy.width of
                Just (WidthPercent p) -> width * p `div` 100
                Just (WidthCells n) -> n
                Nothing -> width
            w = if w0 == 0 || w0 > width then width else w0
            x = case sy.align of
                AlignCentre -> (width - w) `div` 2
                AlignAbsCentre -> (width - w) `div` 2
                AlignRight -> width - w
                _ -> 0
        in (x, w)
  where
    raw = optionText "message-style" opts

-- | tmux's @status_message_redraw@: @message-format@ (with @#{message}@)
-- drawn in @message-style@ over the message area of @row@.
drawMessage
    :: FormatCtx -> Options -> Markup -> Text -> V.Vector Cell.Cell
    -> V.Vector Cell.Cell
drawMessage ctx0 opts markup msg row = overArea ax aw row (formatDraw ctx.theme gc expanded)
  where
    text = case markup of
        MarkupStyled -> msg
        MarkupLiteral -> T.replace "#" "##" msg
    ctx = withVars [("message", text), ("command_prompt", "0")] ctx0
    gc = styleOption ctx "message-style" opts
    expanded = renderFormatCtx ctx (optionText "message-format" opts)
    (ax, aw) = messageArea opts (V.length row)

-- | What the prompt shows: its label (a format), the input being edited,
-- the cursor's index into it, and whether a vi-mode prompt is in command
-- mode.
data PromptView = PromptView
    { label :: Text
    , input :: Text
    , cursor :: Int
    , commandMode :: Bool
    , typeName :: Text  -- ^ @#{prompt_type}@
    , flags :: Text     -- ^ @#{prompt_flags}@
    , quoted :: Bool    -- ^ a @C-v@ waits for its key, shown as @^@ at the cursor
    , completions :: [Text]  -- ^ what a @Tab@ found the word could be
    }

-- | tmux's @prompt_draw@ (with @prompt_layout@): the label through
-- @message-format@, then the input scrolled to keep the cursor in the
-- area; also the cursor's column.
drawPrompt
    :: FormatCtx -> Options -> PromptView -> V.Vector Cell.Cell
    -> (V.Vector Cell.Cell, Int)
drawPrompt ctx0 opts pv row = (drawn, cursorX)
  where
    (ax, aw) = messageArea opts (V.length row)
    styleName
        | pv.commandMode = "message-command-style"
        | otherwise = "message-style"
    ctx1 = withVars
        [ ("prompt_input", pv.input)
        , ("prompt_flags", pv.flags)
        , ("prompt_type", pv.typeName)
        , ("command_prompt", if pv.commandMode then "1" else "0") ] ctx0
    staticCell = case styleParse Cell.defaultStyle (themedDrawStyle ctx0.theme)
            (optionText styleName opts) of
        Just s -> s.cell
        Nothing -> Cell.defaultStyle
    staticStyle = (themedDrawStyle ctx0.theme) { cell = staticCell }
    sy = maybe staticStyle id $ styleParse Cell.defaultStyle staticStyle
        (evaluateCtx ctx1 (optionText styleName opts))
    gc = sy.cell
    ctx = withVars [("message", renderFormatCtx ctx1 pv.label)] ctx1
    expanded = renderFormatCtx ctx (optionText "message-format" opts)

    labelW = min aw (formatWidth expanded)
    before = T.take pv.cursor pv.input
    shown = quotedInput pv.quoted pv.cursor pv.input
    pcursor = textWidth before
    pwidth = textWidth shown
    avail = aw - labelW
    (offset, inputW, cursorRel)
        | avail == 0 = (0, 0, labelW)
        | pcursor >= avail =
            let o = pcursor - avail + 1
            in (o, avail, labelW + pcursor - o)
        | otherwise = (0, min pwidth avail, labelW + pcursor)
    -- tmux's prompt_draw_complete: the matches after the cursor, when it
    -- ends the input.
    matches = T.concat [ " " <> m | m <- pv.completions ]
    matchesW
        | T.null matches || pv.cursor /= T.length pv.input || cursorRel >= aw = 0
        | otherwise = min (textWidth matches) (aw - cursorRel)
    contentW = min aw (max (labelW + inputW) (cursorRel + matchesW))
    contentX = case sy.align of
        AlignCentre -> ax + (aw - contentW) `div` 2
        AlignAbsCentre -> ax + (aw - contentW) `div` 2
        AlignRight -> ax + aw - contentW
        _ -> ax
    inputX = contentX + labelW
    cursorX = contentX + cursorRel

    filled = case sy.fill of
        Just c -> overArea ax aw row
            (V.map (const (Cell.glyphCell ' ' Cell.defaultStyle { Cell.bg = c })))
        Nothing -> row
    drawn
        | contentW == 0 = filled
        | otherwise =
            overArea cursorX matchesW
                (overArea inputX inputW
                    (overArea contentX labelW filled (formatDraw ctx.theme gc expanded))
                    (const (V.fromList (inputCells gc offset inputW shown))))
                (const (V.fromList (inputCells gc { Cell.underline = True } 0 matchesW matches)))

-- | The input as drawn: a @C-v@ waiting for its key shows as @^@ at the
-- cursor (tmux's @prompt_redraw_quote@).
quotedInput :: Bool -> Int -> Text -> Text
quotedInput quoted cursor input
    | quoted = T.take cursor input <> "^" <> T.drop cursor input
    | otherwise = input

-- | The input's cells from column @offset@ for @width@ columns, control
-- characters shown as @^X@ (tmux's @prompt_redraw_character@).
inputCells :: Cell.Style -> Int -> Int -> Text -> [Cell.Cell]
inputCells gc offset width = go 0 . T.unpack
  where
    go _ [] = []
    go w (c : rest)
        | w < offset = go (w + cw) rest
        | w + cw > offset + width = []
        | otherwise = cells <> go (w + cw) rest
      where
        ctl = c <= '\x1f' || c == '\DEL'
        cw | ctl = 2
           | otherwise = max 1 (charWidth c)
        cells
            | ctl = [ Cell.glyphCell '^' gc
                    , Cell.glyphCell (if c == '\DEL' then '?' else toEnum (fromEnum c + 0x40)) gc ]
            | cw == 2 = [ Cell.Cell (Cell.Glyph c [] Cell.Wide) gc
                        , Cell.Cell Cell.Continuation gc ]
            | otherwise = [Cell.glyphCell c gc]

-- | Replace the @w@ cells of @row@ from column @x@ with @f@ of them
-- (clipped to the row; @f@'s result is padded or cut to the slice).
overArea :: Int -> Int -> V.Vector Cell.Cell -> (V.Vector Cell.Cell -> V.Vector Cell.Cell)
    -> V.Vector Cell.Cell
overArea x w row f
    | w' <= 0 = row
    | otherwise = row V.// zip [x' ..] (V.toList (V.take w' (f (V.slice x' w' row))))
  where
    x' = max 0 x
    w' = min (V.length row - x') (w - (x' - x))

withVars :: [(Text, Text)] -> FormatCtx -> FormatCtx
withVars kvs ctx = ctx { vars = Map.union (Map.fromList kvs) ctx.vars }
