-- | Where @display-menu@ and @display-popup@ put their box (tmux's
-- @cmd_display_menu_get_menu_pos@ and @cmd_display_menu_get_popup_pos@):
-- the @popup_*@ format variables a @-x@\/@-y@ expression may use, the
-- letters that name them, and how the expanded result is pulled back onto
-- the screen. A menu is placed in its window's cells, a popup in its
-- client's.
module Hat.Server.DisplayPos
    ( Axis (..)
    , positionFormat
    , MouseAt (..)
    , StatusAt (..)
    , PaneAt (..)
    , menuPosVars
    , popupPosVars
    , menuPlaceX
    , menuPlaceY
    , popupPlaceX
    , popupPlaceY
    , strtol
    ) where

import Data.Char (isDigit, isSpace)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T

data Axis = AxisX | AxisY
    deriving (Eq, Show)

-- | The format a @-x@ or @-y@ value expands: one of the letters naming a
-- @popup_*@ variable, else the value itself.
positionFormat :: Axis -> Maybe Text -> Text
positionFormat axis mv = case (axis, mv) of
    (AxisX, Nothing) -> "#{popup_centre_x}"
    (AxisX, Just "C") -> "#{popup_centre_x}"
    (AxisX, Just "R") -> "#{popup_pane_right}"
    (AxisX, Just "P") -> "#{popup_pane_left}"
    (AxisX, Just "M") -> "#{popup_mouse_centre_x}"
    (AxisX, Just "L") -> "#{popup_last_x}"
    (AxisX, Just "W") -> "#{popup_window_status_line_x}"
    (AxisY, Nothing) -> "#{popup_centre_y}"
    (AxisY, Just "C") -> "#{popup_centre_y}"
    (AxisY, Just "P") -> "#{popup_pane_bottom}"
    (AxisY, Just "M") -> "#{popup_mouse_top}"
    (AxisY, Just "L") -> "#{popup_last_y}"
    (AxisY, Just "S") -> "#{popup_status_line_y}"
    (AxisY, Just "W") -> "#{popup_window_status_line_y}"
    (_, Just v) -> v

-- | The mouse event behind the command: its client cell, and the status
-- line the client showed (its first row and height) when it happened.
data MouseAt = MouseAt
    { x :: Int
    , y :: Int
    , statusAt :: Maybe Int
    , statusLines :: Int
    }
    deriving (Eq, Show)

-- | The client's status line: whether it is at the top, its height, and
-- the line and column where the target window's entry starts, if it has
-- one.
data StatusAt = StatusAt
    { atTop :: Bool
    , lines :: Int
    , windowEntry :: Maybe (Int, Int)
    }
    deriving (Eq, Show)

-- | The target pane's place in its window.
data PaneAt = PaneAt
    { xoff :: Int
    , yoff :: Int
    , sx :: Int
    , sy :: Int
    }
    deriving (Eq, Show)

-- | The variables placing a @w@ by @h@ menu in a window @wsx@ by @wsy@,
-- @visible@ rows of it on screen; @lastAt@ is where the window's last menu
-- went.
menuPosVars
    :: Int -> Int -> Int -> Int -> Int -> Maybe MouseAt -> (Int, Int) -> Maybe StatusAt
    -> PaneAt -> Map Text Text
menuPosVars w h wsx wsy visible mmouse (lastX, lastY) mstatus pane = Map.fromList $ catMaybes
    [ mouseVar "popup_mouse_x" (\(mx, _) -> mx)
    , mouseVar "popup_mouse_y" (\(_, my) -> my)
    , Just ("popup_last_x", num lastX)
    , Just ("popup_last_y", num (lastY + h))
    , (\(_, start) -> ("popup_window_status_line_x", num start)) <$> (mstatus >>= (.windowEntry))
    , (\st -> ("popup_window_status_line_y", num (if st.atTop then h else wsy)))
        <$> (mstatus >>= \st -> st <$ st.windowEntry)
    , (\st -> ("popup_status_line_y", num (if st.atTop then h else wsy))) <$> mstatus
    , Just ("popup_width", num w)
    , Just ("popup_height", num h)
    , Just ("popup_centre_x", num (max 0 ((wsx - 1) `quot` 2 - w `quot` 2)))
    , Just ("popup_centre_y", num (let n = (wsy - 1) `quot` 2 + h `quot` 2
                                   in if n >= wsy then maxY else n))
    , mouseVar "popup_mouse_centre_x" (\(mx, _) -> max 0 (mx - w `quot` 2))
    , mouseVar "popup_mouse_centre_y" (\(_, my) -> let n = my - h `quot` 2
                                        in if n + h >= wsy then maxY else n)
    , mouseVar "popup_mouse_top" (\(_, my) -> if my + h >= wsy then wsy - 1 else my + h)
    , mouseVar "popup_mouse_bottom" (\(_, my) -> max 0 (my - h))
    , Just ("popup_pane_top", num (let n = pane.yoff + h in if n >= wsy then maxY else n))
    , Just ("popup_pane_bottom", num (pane.yoff + pane.sy))
    , Just ("popup_pane_left", num pane.xoff)
    , Just ("popup_pane_right", num (max 0 (pane.xoff + pane.sx - w)))
    ]
  where
    maxY = if wsy > h then wsy - h else 0
    -- The event in window cells: a click on a top status line is at the
    -- window's top, one on a lower status line at its last visible row.
    windowMouse m = case m.statusAt of
        Just 0 -> (m.x, if m.y >= m.statusLines then m.y - m.statusLines else 0)
        Just at | at > 0, m.y >= at -> (m.x, visible - 1)
        _ -> (m.x, m.y)
    mouseVar name f = (\m -> (name, num (f (windowMouse m)))) <$> mmouse

-- | The variables placing a @w@ by @h@ popup on a client @tsx@ by @tsy@;
-- @lastAt@ is where the target window's last menu went.
popupPosVars
    :: Int -> Int -> Int -> Int -> Maybe MouseAt -> (Int, Int) -> Maybe StatusAt
    -> PaneAt -> Map Text Text
popupPosVars w h tsx tsy mmouse (lastX, lastY) mstatus pane = Map.fromList $ catMaybes
    [ mouseVar "popup_mouse_x" (.x)
    , mouseVar "popup_mouse_y" (.y)
    , Just ("popup_last_x", num lastX)
    , Just ("popup_last_y", num (lastY + h))
    , (\(_, start) -> ("popup_window_status_line_x", num start)) <$> (mstatus >>= (.windowEntry))
    , (\(st, (line, _)) -> ("popup_window_status_line_y", num
            (if st.atTop then line + 1 + h else tsy - st.lines + line)))
        <$> (mstatus >>= \st -> (,) st <$> st.windowEntry)
    , (\st -> ("popup_status_line_y", num (if st.atTop then st.lines + h else tsy - st.lines)))
        <$> mstatus
    , Just ("popup_width", num w)
    , Just ("popup_height", num h)
    , Just ("popup_centre_x", num (max 0 ((tsx - 1) `quot` 2 - w `quot` 2)))
    , Just ("popup_centre_y", num (let n = (tsy - 1) `quot` 2 + h `quot` 2
                                   in if n >= tsy then tsy - h else n))
    , mouseVar "popup_mouse_centre_x" (\m -> max 0 (m.x - w `quot` 2))
    , mouseVar "popup_mouse_centre_y" (\m ->
        let n = unsigned (m.y - h `quot` 2) in if n + h >= tsy then tsy - h else n)
    , mouseVar "popup_mouse_top" (\m -> if m.y + h >= tsy then tsy - 1 else m.y + h)
    , mouseVar "popup_mouse_bottom" (\m -> unsigned (m.y - h))
    , Just ("popup_pane_top", num (let n = top + pane.yoff + h in if n >= tsy then tsy - h else n))
    , Just ("popup_pane_bottom", num (top + pane.yoff + pane.sy))
    , Just ("popup_pane_left", num pane.xoff)
    , Just ("popup_pane_right", num (max 0 (pane.xoff + pane.sx - w)))
    ]
  where
    -- The rows a status line at the top pushes the window down.
    top = case mstatus of
        Just st | st.atTop -> st.lines
        _ -> 0
    mouseVar name f = (\m -> (name, num (f m))) <$> mmouse
    -- tmux computes these in unsigned arithmetic, wrapping below zero.
    unsigned n = if n < 0 then n + 4294967296 else n

-- | A menu's column from its expanded @-x@: kept inside the window.
menuPlaceX :: Int -> Int -> Text -> Int
menuPlaceX wsx w expanded = max 0 (min maxX (strtol expanded))
  where
    maxX = if wsx > w then wsx - w else 0

-- | A menu's row from its expanded @-y@, which names its bottom: kept
-- inside the window.
menuPlaceY :: Int -> Int -> Text -> Int
menuPlaceY wsy h expanded = max 0 (min maxY (fromBottom h (strtol expanded)))
  where
    maxY = if wsy > h then wsy - h else 0

-- | A popup's column from its expanded @-x@: kept on the client.
popupPlaceX :: Int -> Int -> Text -> Int
popupPlaceX tsx w expanded = max 0 (if n + w >= tsx then tsx - w else n)
  where
    n = strtol expanded

-- | A popup's row from its expanded @-y@, which names its bottom: kept on
-- the client.
popupPlaceY :: Int -> Int -> Text -> Int
popupPlaceY tsy h expanded = max 0 (if n + h >= tsy then tsy - h else n)
  where
    n = fromBottom h (strtol expanded)

fromBottom :: Int -> Int -> Int
fromBottom h n = if n < h then 0 else n - h

-- | C's @strtol(s, NULL, 10)@: leading space, a sign, then digits; 0 when
-- there are none.
strtol :: Text -> Int
strtol t0 = case T.uncons t1 of
    Just ('-', rest) -> negate (digits rest)
    Just ('+', rest) -> digits rest
    _ -> digits t1
  where
    t1 = T.dropWhile isSpace t0
    digits s = T.foldl' (\acc c -> acc * 10 + (fromEnum c - fromEnum '0')) 0
        (T.takeWhile isDigit s)

num :: Int -> Text
num = T.pack . show
