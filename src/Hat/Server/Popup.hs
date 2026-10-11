-- | A client's popup (tmux's @popup.c@): a box over everything the client
-- shows, running a command in a pty of its own. Pure: where the box is and
-- how it follows the client and the mouse, what a key or mouse report does
-- to it, and how it draws. "Hat.Server.PopupIO" runs the command.
module Hat.Server.Popup
    ( PopupState (..)
    , PopupClose (..)
    , PopupKeys (..)
    , Dragging (..)
    , newPopupState
    , innerSize
    , refitPopup
    , modifyLines
      -- * Input
    , PopupInput (..)
    , PopupAction (..)
    , JobState (..)
    , popupInput
      -- * Drawing
    , popupGrid
    , popupCursor
    ) where

import Data.Bits ((.&.))
import Data.Text (Text)
import Data.Vector qualified as V

import Hat.Server.Box
import Hat.Server.Theme (Theme)
import Hat.Term.Cell qualified as Cell

-- | When the popup closes by itself: when its command exits (@-E@), only
-- when it exits successfully (@-EE@), or never.
data PopupClose = StaysOpen | CloseOnExit | CloseOnSuccess
    deriving (Eq, Show)

-- | Whether any key closes the popup once its command has exited (@-k@).
data PopupKeys = KeysToJob | AnyKeyCloses
    deriving (Eq, Show)

-- | What a mouse drag on the popup is doing.
data Dragging = DragOff | DragMove | DragSize
    deriving (Eq, Show)

-- | tmux's @struct popup_data@, less the job: the box's place and size
-- now, the place and size it was asked for (which it returns to as the
-- client grows), and how it looks and closes.
data PopupState = PopupState
    { px :: Int
    , py :: Int
    , sx :: Int
    , sy :: Int
    , ppx :: Int
    , ppy :: Int
    , psx :: Int
    , psy :: Int
    , lines :: BoxLines
    , title :: Text
    , style :: Maybe Text  -- ^ @-s@, over @popup-style@'s colours
    , borderStyle :: Maybe Text  -- ^ @-S@, over @popup-border-style@'s
    , onExit :: PopupClose
    , keys :: PopupKeys
    , dragging :: Dragging
    , dx :: Int  -- ^ where a move drag holds the box, from its corner
    , dy :: Int
    }
    deriving (Eq, Show)

-- | A popup at @(x, y)@, @w@ by @h@.
newPopupState :: Int -> Int -> Int -> Int -> BoxLines -> Text -> PopupState
newPopupState x y w h lns t = PopupState
    { px = x, py = y, sx = w, sy = h, ppx = x, ppy = y, psx = w, psy = h
    , lines = lns, title = t, style = Nothing, borderStyle = Nothing
    , onExit = StaysOpen, keys = KeysToJob, dragging = DragOff, dx = 0, dy = 0 }

-- | The size of the screen inside the box, which the command's pty takes;
-- 'Nothing' when a bordered box is too small to hold one.
innerSize :: PopupState -> Maybe (Int, Int)
innerSize ps
    | ps.lines == BoxNone = Just (ps.sx, ps.sy)
    | ps.sx > 2 && ps.sy > 2 = Just (ps.sx - 2, ps.sy - 2)
    | otherwise = Nothing

-- | tmux's @popup_resize_cb@: the popup on a client resized to @tsx@ by
-- @tsy@, kept on it, back to the place and size it wants where they fit.
refitPopup :: Int -> Int -> PopupState -> PopupState
refitPopup tsx tsy ps = ps { sx, sy, px = place ps.ppx sx tsx, py = place ps.ppy sy tsy }
  where
    sx = min ps.psx tsx
    sy = min ps.psy tsy
    place p size limit = if p + size > limit then limit - size else p

-- | The popup's box lines changed (@display-popup@ on an open popup, as
-- tmux's @popup_modify@).
modifyLines :: BoxLines -> PopupState -> PopupState
modifyLines lns ps = ps { lines = lns }

-- | Whether the popup's command still runs.
data JobState = JobRunning | JobDone
    deriving (Eq, Show)

-- | A key (by canonical name, and whether it is part of a paste) or a mouse
-- report (its button byte, client cell, and the report before it) for the
-- popup.
data PopupInput
    = PopupKey Text Bool
    | PopupMouse Int (Int, Int) Int (Int, Int)
    deriving (Eq, Show)

-- | What the popup does with an input.
data PopupAction
    = PopupTook  -- ^ nothing more: it was a drag, or outside the box
    | PopupCloses
    | PopupKeyToJob
    | PopupMouseToJob (Int, Int)  -- ^ the report, at this cell of the inner screen
    deriving (Eq, Show)

-- | tmux's @popup_key_cb@ on a client @tsx@ by @tsy@: a drag moves the box
-- by its top or left edge (or with Meta held anywhere) and resizes it by
-- its others; Escape or @C-c@ closes a popup its command's exit would not;
-- once the command has exited, @-k@ closes it on any key; while it runs,
-- keys and the mouse inside the box go to it.
popupInput :: Int -> Int -> JobState -> PopupInput -> PopupState -> (PopupState, PopupAction)
popupInput tsx tsy job input ps = case input of
    PopupMouse b (x, y) lb (lx, ly)
        | ps.dragging /= DragOff -> (drag b (x, y), PopupTook)
        | x < ps.px || x > ps.px + ps.sx - 1 || y < ps.py || y > ps.py + ps.sy - 1 ->
            (ps, PopupTook)
        | b .&. modifiers == 0 && b .&. buttons == 2 && border x y `elem` [OnLeft, OnTop] ->
            (ps, PopupTook)
        | b .&. modifiers == meta || (border x y /= OffBorder && lb .&. dragBit == 0) ->
            if b .&. dragBit == 0 then (ps, PopupTook)
            else
                let how = case lb .&. buttons of
                        0 -> DragMove
                        2 -> DragSize
                        _ -> ps.dragging
                in (ps { dragging = how, dx = lx - ps.px, dy = ly - ps.py }, PopupTook)
        | job == JobRunning -> (ps, PopupMouseToJob (x - ps.px - inset, y - ps.py - inset))
        | otherwise -> (ps, PopupTook)
    PopupKey k paste
        | (not closesOnExit || job == JobDone) && k `elem` ["Escape", "C-c"] -> (ps, PopupCloses)
        | job == JobDone && ps.keys == AnyKeyCloses && not paste -> (ps, PopupCloses)
        | job == JobRunning -> (ps, PopupKeyToJob)
        | otherwise -> (ps, PopupTook)
  where
    inset = if ps.lines == BoxNone then 0 else 1
    closesOnExit = ps.onExit /= StaysOpen
    border x y
        | ps.lines == BoxNone = OffBorder
        | x == ps.px = OnLeft
        | x == ps.px + ps.sx - 1 = OnRight
        | y == ps.py = OnTop
        | y == ps.py + ps.sy - 1 = OnBottom
        | otherwise = OffBorder
    -- tmux's popup_handle_drag.
    drag b (x, y)
        | b .&. dragBit == 0 = ps { dragging = DragOff }
        | ps.dragging == DragMove =
            let nx | x < ps.dx = 0
                   | x - ps.dx + ps.sx > tsx = tsx - ps.sx
                   | otherwise = x - ps.dx
                ny | y < ps.dy = 0
                   | y - ps.dy + ps.sy > tsy = tsy - ps.sy
                   | otherwise = y - ps.dy
            in ps { px = nx, py = ny, dx = x - nx, dy = y - ny, ppx = nx, ppy = ny }
        | ps.dragging == DragSize =
            let least = if ps.lines == BoxNone then 1 else 3
            in if x < ps.px + least || y < ps.py + least then ps
               else ps { sx = x - ps.px, sy = y - ps.py, psx = x - ps.px, psy = y - ps.py }
        | otherwise = ps
    buttons = 195
    dragBit = 32
    modifiers = 28
    meta = 8

-- | Which edge of the box a report is on.
data Edge = OffBorder | OnLeft | OnRight | OnTop | OnBottom
    deriving (Eq)

-- | tmux's @popup_draw_cb@: the box, its title, and the command's screen
-- inside it, on a cleared screen the size of the popup.
popupGrid :: Theme -> Cell.Style -> PopupState -> V.Vector (V.Vector Cell.Cell) -> Grid
popupGrid th border ps content
    | ps.lines == BoxNone = copyAt 0 0 ps.sx ps.sy blank
    | ps.sx > 2 && ps.sy > 2 =
        copyAt 1 1 (ps.sx - 2) (ps.sy - 2) (drawBox th ps.lines border ps.title ps.sx ps.sy blank)
    | otherwise = blank
  where
    blank = blankGrid ps.sx ps.sy
    copyAt x0 y0 w h g = V.imap (\y row ->
        if y < y0 || y >= y0 + h then row
        else case content V.!? (y - y0) of
            Nothing -> row
            Just src -> V.imap (\x c ->
                if x < x0 || x >= x0 + w then c else maybe c id (src V.!? (x - x0))) row) g

-- | Where the command's cursor is on the client (tmux's @popup_mode_cb@).
popupCursor :: PopupState -> (Int, Int) -> (Int, Int)
popupCursor ps (cx, cy) = (ps.px + inset + cx, ps.py + inset + cy)
  where
    inset = if ps.lines == BoxNone then 0 else 1
