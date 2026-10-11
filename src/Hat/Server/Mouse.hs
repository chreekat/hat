-- | Mouse input, as tmux's @tty_keys_mouse@ and @server_client_check_mouse@
-- read it: a report from the outer terminal becomes a mouse key
-- (@MouseDown1Pane@, @WheelUpStatus@, @MouseDragEnd1Border@, …) carrying the
-- event, which bindings act on and panes may be sent. Pure: the caller
-- supplies what the client sees ('MouseView') and keeps the 'MouseState'.
module Hat.Server.Mouse
    ( -- * Reports
      MouseInput (..)
    , parseMouseInput
      -- * Per-client state
    , MouseState (..)
    , initialMouseState
    , Click (..)
    , ClickStart (..)
    , Drag (..)
    , clickTimeout
      -- * Classifying
    , MouseView (..)
    , PaneBox (..)
    , BoxKind (..)
    , ModalView (..)
    , ClickOutside (..)
    , Placement (..)
    , windowPoint
    , StatusRange (..)
    , Location (..)
    , MouseType (..)
    , MouseEvent (..)
    , MouseKey (..)
    , checkMouse
    , clickExpired
    , mouseKeyName
      -- * Panes
    , mouseAt
    , mouseBytes
    , PaneMouseMode (..)
    , isMouseKey
    , isMoveKey
    , stripMods
    , mouseKeyByName
    , wheel
      -- * The outer terminal
    , outerMouseMode
    , overlayMouseMode
    , mouseModeBytes
    ) where

import Data.Bits (testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Model.Ids (PaneId, SessionId, WindowId)

-- | One report from the outer terminal (tmux's @struct mouse_event@ as
-- @tty_keys_mouse@ fills it): the button byte, the 0-based position, and for
-- an SGR report its press\/release letter and raw button.
data MouseInput = MouseInput
    { b :: Int
    , x :: Int
    , y :: Int
    , sgrType :: Char  -- ^ @'M'@ press or @'m'@ release (SGR), @' '@ legacy
    , sgrB :: Int
    }
    deriving (Eq, Show)

-- | tmux's @tty_keys_mouse@ over a whole report (legacy @ESC [ M bxy@ or SGR
-- @ESC [ < b ; x ; y M/m@); 'Nothing' for one tmux discards.
parseMouseInput :: ByteString -> Maybe MouseInput
parseMouseInput bs
    | Just rest <- B.stripPrefix "\ESC[M" bs
    , [cb, cx, cy] <- map fromIntegral (B.unpack rest) =
        if cb < 0x20 || cx < 0x21 || cy < 0x21 then Nothing
            else Just MouseInput
                { b = cb - 0x20, x = cx - 0x21, y = cy - 0x21
                , sgrType = ' ', sgrB = 0 }
    | Just rest <- B.stripPrefix "\ESC[<" bs, Just (body, final) <- B.unsnoc rest
    , final == 0x4d || final == 0x6d
    , [Just sb, Just sx, Just sy] <- map number (B8.split ';' body)
    , sx >= 1, sy >= 1
    , not (final == 0x6d && wheel sb) =
        Just MouseInput
            { b = if final == 0x6d then 3 else sb, x = sx - 1, y = sy - 1
            , sgrType = if final == 0x6d then 'm' else 'M', sgrB = sb }
    | otherwise = Nothing
  where
    number f = case B8.readInt f of
        Just (n, "") | n >= 0 -> Just n
        _ -> Nothing

-- tmux's MOUSE_* button masks; the modifier bits are 4 shift, 8 meta and
-- 16 ctrl.
buttons, maskDrag :: Int
buttons = 195
maskDrag = 32

isDrag, isRelease, wheel :: Int -> Bool
isDrag v = v .&. maskDrag /= 0
isRelease v = v .&. buttons == 3
wheel v = v .&. 0xc0 == 64

-- | tmux's @KEYC_CLICK_TIMEOUT@, ms: how long a click waits for the next.
clickTimeout :: Int
clickTimeout = 300

-- | What a client's mouse has been doing (tmux's @tty->mouse_*@ and the
-- client's click flags).
data MouseState = MouseState
    { prevX, prevY, prevB :: Int  -- ^ the report before this one
    , dragFlag :: Int  -- ^ 0, or the dragging button + 1
    , dragPane :: Maybe PaneId  -- ^ the pane a drag started over
    , dragStart :: (Int, Int)  -- ^ where the last drag started, in the window
    , click :: Click
    }
    deriving (Eq, Show)

-- | Where a run of clicks stands: a down waiting to become a double click,
-- a second click waiting for a third, with the event and when it lapses.
data Click
    = NoClick
    | AwaitSecond ClickStart
    | AwaitThird ClickStart
    deriving (Eq, Show)

data ClickStart = ClickStart
    { button :: Int
    , loc :: Location
    , pane :: Maybe PaneId
    , event :: MouseEvent
    , expires :: Int
    }
    deriving (Eq, Show)

initialMouseState :: MouseState
initialMouseState = MouseState
    { prevX = 0, prevY = 0, prevB = 0, dragFlag = 0, dragPane = Nothing
    , dragStart = (0, 0), click = NoClick }

-- | What a mouse drag in progress moves (tmux's @mouse_drag_update@).
data Drag
    = DragSelection PaneId  -- ^ the copy-mode selection of a pane
    | DragBorder            -- ^ the pane border it started on
    | DragNewPane PaneId (Int, Int)
        -- ^ the box of a floating pane @new-pane -M@ made, from the window
        -- point the drag started at
    | DragFloat PaneId      -- ^ a floating pane's border: resize, or move by its top
    | DragMove PaneId       -- ^ a floating pane, moved whole (@move-pane -M@)
    deriving (Eq, Show)

-- | What a client shows, for placing a click (status line, panes, borders).
data MouseView = MouseView
    { session :: SessionId
    , window :: WindowId
    , statusAt :: Maybe Int  -- ^ the status line's first row, if shown
    , statusLines :: Int
    , statusRange :: Int -> Int -> Maybe StatusRange
        -- ^ the range under a column of a status line
    , width :: Int  -- ^ the window area
    , height :: Int
    , boxes :: [PaneBox]
        -- ^ the visible panes, the floating ones first, front to back
    , modal :: Maybe ModalView
    }

-- | The window's modal pane, as clicks see it.
data ModalView = ModalView
    { pane :: PaneId
    , outside :: ClickOutside
    }

-- | What a click outside a modal pane does to it (@new-pane -C@).
data ClickOutside = KeepModal | CloseModal
    deriving (Eq, Show)

-- | A visible pane's place in the window area (tmux's @xoff@, @yoff@,
-- @sx@, @sy@).
data PaneBox = PaneBox
    { pane :: PaneId
    , xoff, yoff, sx, sy :: Int
    , kind :: BoxKind
    }
    deriving (Eq, Show)

-- | A tiled pane, or a floating one with or without a border of its own.
data BoxKind = TiledBox | FramedFloat | BareFloat
    deriving (Eq, Show)

-- | A status line range a click can land in (tmux's @style_range@).
data StatusRange
    = RangeNone
    | RangeLeft
    | RangeRight
    | RangePane PaneId
    | RangeWindow WindowId
    | RangeSession SessionId
    | RangeUser
    | RangeControl Int
    deriving (Eq, Show)

-- | Where a mouse key happened (tmux's @KEYC_MOUSE_LOCATION_*@).
data Location
    = LocPane | LocStatus | LocStatusLeft | LocStatusRight | LocStatusDefault
    | LocBorder | LocEmpty | LocControl Int
    deriving (Eq, Show)

-- | What a mouse key is (tmux's @KEYC_TYPE_*@).
data MouseType
    = MouseMove | MouseDown | MouseUp | MouseDrag | MouseDragEnd
    | WheelUp | WheelDown | SecondClick | DoubleClick | TripleClick
    deriving (Eq, Show)

-- | A placed mouse report: the report, the one before it, and what it
-- targets (tmux's @m->s@, @m->w@, @m->wp@).
data MouseEvent = MouseEvent
    { input :: MouseInput
    , lastX, lastY, lastB :: Int
    , statusAt :: Maybe Int
    , statusLines :: Int
    , onSession :: SessionId
    , onWindow :: Maybe WindowId
    , onPane :: Maybe PaneId
    , ignore :: Bool  -- ^ a timed double click, never sent to a pane
    }
    deriving (Eq, Show)

-- | What a report comes to.
data Placement
    = Keyed MouseKey
    | Unplaced  -- ^ no key
    | Swallowed (Maybe PaneId)
        -- ^ outside the modal pane: no key, any drag ends, and a click
        -- closes the modal pane named
    deriving (Eq, Show)

-- | A mouse report's key: its name (with modifiers) and the event.
data MouseKey = MouseKey
    { name :: Text
    , event :: MouseEvent
    , dragging :: Bool  -- ^ a drag update, for the drag in progress
    }
    deriving (Eq, Show)

-- | tmux's @server_client_check_mouse@: what a report means, at time @now@
-- (ms). 'Nothing' for a report that makes no key.
checkMouse :: Int -> MouseView -> MouseState -> MouseInput -> (MouseState, Placement)
checkMouse now view st0 m = case classified of
    Nothing -> (stRecorded, Unplaced)
    Just (ty, ex, ey, eb) -> placed ty ex ey eb
  where
    stRecorded = st0 { prevX = m.x, prevY = m.y, prevB = m.b }
    lx = st0.prevX
    ly = st0.prevY
    lb = st0.prevB
    -- The event type, and the position and button it is about.
    classified
        | (m.sgrType /= ' ' && isDrag m.sgrB && isRelease m.sgrB)
          || (m.sgrType == ' ' && isDrag m.b && isRelease m.b && isRelease lb) =
            Just (MouseMove, m.x, m.y, 0)
        | isDrag m.b = if st0.dragFlag /= 0
            then if m.x == lx && m.y == ly then Nothing
                 else Just (MouseDrag, m.x, m.y, m.b)
            else Just (MouseDrag, lx, ly, lb)
        | wheel m.b = Just (if m.b .&. buttons == 64 then WheelUp else WheelDown, m.x, m.y, m.b)
        | isRelease m.b = Just (MouseUp, m.x, m.y, if m.sgrType == 'm' then m.sgrB else lb)
        | otherwise = case st0.click of
            AwaitSecond _ -> Just (SecondClick, m.x, m.y, m.b)
            AwaitThird _ -> Just (TripleClick, m.x, m.y, m.b)
            NoClick -> Just (MouseDown, m.x, m.y, m.b)
    placed ty ex ey eb
        | Just md <- view.modal, outsideModal md ex ey, not (modalDrag md ty) =
            ( stRecorded { dragFlag = 0, dragPane = Nothing }
            , Swallowed (if md.outside == CloseModal
                            && ty `elem` [MouseDown, SecondClick, TripleClick]
                        then Just md.pane else Nothing) )
        | otherwise =
        let (loc, mwin, mpane, mloc) = locate ty ex ey
            ev = MouseEvent
                { input = m, lastX = lx, lastY = ly, lastB = lb
                , statusAt = view.statusAt, statusLines = view.statusLines
                , onSession = view.session, onWindow = mwin, onPane = mpane
                , ignore = False }
        in case mloc of
            Nothing -> (stRecorded, Unplaced)
            Just () -> finish ty loc ev eb (windowXY ex ey)
    -- A report on the status line is never outside the modal pane.
    outsideModal md ex ey = case view.statusAt of
        Just at | ey >= at && ey < at + view.statusLines -> False
        _ -> let (px, py) = windowXY ex ey
             in px <= view.width && py <= view.height
                && maybe True (\bx -> not (contains bx px py))
                    (find (\bx -> bx.pane == md.pane) view.boxes)
    -- A drag begun on the modal pane follows the mouse out of it.
    modalDrag md ty = st0.dragPane == Just md.pane && st0.dragFlag /= 0
        && ty `elem` [MouseDrag, MouseUp]
    windowXY ex ey = (ex, windowRow view.statusAt view.statusLines ey)
    -- The status line range, or the pane, border or empty area.
    locate ty ex ey = case view.statusAt of
        Just at | ey >= at && ey < at + view.statusLines ->
            case view.statusRange ex (ey - at) of
                Nothing -> (LocStatusDefault, Nothing, Nothing, Just ())
                Just r -> case r of
                    RangeNone -> (LocStatusDefault, Nothing, Nothing, Nothing)
                    RangeLeft -> (LocStatusLeft, Nothing, Nothing, Just ())
                    RangeRight -> (LocStatusRight, Nothing, Nothing, Just ())
                    RangePane p -> (LocStatus, Nothing, Just p, Just ())
                    RangeWindow w -> (LocStatus, Just w, Nothing, Just ())
                    RangeSession _ -> (LocStatus, Nothing, Nothing, Just ())
                    RangeUser -> (LocStatus, Nothing, Nothing, Just ())
                    RangeControl n -> (LocControl n, Nothing, Nothing, Just ())
        _ ->
            let (px, py) = windowXY ex ey
                isModalDrag = maybe False (\md -> modalDrag md ty) view.modal
                byDrag = if ty == MouseDrag || isModalDrag then st0.dragPane else Nothing
                wp = case byDrag of
                    Just p -> find (\bx -> bx.pane == p) view.boxes
                    Nothing -> boxAt px py
            in if px > view.width || py > view.height
                then (LocEmpty, Nothing, Nothing, Nothing)
                else case wp of
                    Nothing -> (LocEmpty, Just view.window, Nothing, Just ())
                    Just bx ->
                        ( if isModalDrag then LocPane else inPane bx px py
                        , Just view.window, Just bx.pane, Just () )
    -- tmux's @window_get_active_at@: only the modal pane while there is
    -- one; else the floating panes front to back, then the tiled ones.
    boxAt px py = case view.modal of
        Just md -> find (\bx -> bx.pane == md.pane && contains bx px py) view.boxes
        Nothing -> find (\bx -> bx.kind /= TiledBox && contains bx px py) view.boxes
            `orElse` find (\bx -> bx.kind == TiledBox && px >= bx.xoff && px < bx.xoff + bx.sx
                             && py >= bx.yoff && py < bx.yoff + bx.sy) view.boxes
            `orElse` find (\bx -> bx.kind == TiledBox && onBorderOf bx px py) view.boxes
    orElse (Just v) _ = Just v
    orElse Nothing v = v
    -- tmux's @window_get_active_at@ counts a pane's right and bottom border
    -- as its own.
    onBorderOf bx px py =
        px >= bx.xoff && px <= bx.xoff + bx.sx && py >= bx.yoff && py <= bx.yoff + bx.sy
    inPane bx px py
        | bx.kind /= TiledBox = if insideContent bx px py then LocPane else LocBorder
        | (bx.yoff == 0 && py < bx.sy || py >= bx.yoff && py < bx.yoff + bx.sy)
          && px < bx.xoff + bx.sx = LocPane
        | any (\f -> f.kind /= BareFloat && borderOf f px py) view.boxes = LocBorder
        | otherwise = LocPane
    borderOf f px py =
        (py >= f.yoff - 1 && py <= f.yoff + f.sy && px == f.xoff + f.sx)
        || (px >= f.xoff - 1 && px <= f.xoff + f.sx
            && (py == f.yoff + f.sy || py == f.yoff - 1))
    finish ty0 loc ev eb start =
        let -- A click run restarts on another button, place or pane.
            (ty, click1) = clickStep ty0 loc ev
            stClick = stRecorded { click = click1 }
            (ty', stDrag) = endDrag ty stClick
            stMoved = case ty' of
                MouseDrag -> startDrag ev eb start stDrag
                _ -> stDrag
            dragUpdate = ty' == MouseDrag && st0.dragFlag /= 0
            key = MouseKey
                { name = mouseKeyName ty' (buttonNumber eb) loc (modifiers eb)
                , event = ev, dragging = dragUpdate }
        in (stMoved, Keyed key)
    clickStep ty loc ev
        | ty `notElem` [MouseDown, SecondClick, TripleClick] = (ty, st0.click)
        | otherwise =
            let start = ClickStart
                    { button = m.b, loc = loc, pane = ev.onPane, event = ev
                    , expires = now + clickTimeout }
                same c = c.button == m.b && c.loc == loc && c.pane == ev.onPane
                ty' = case (ty, st0.click) of
                    (SecondClick, AwaitSecond c) | not (same c) -> MouseDown
                    (TripleClick, AwaitThird c) | not (same c) -> MouseDown
                    _ -> ty
            in case ty' of
                MouseDown -> (MouseDown, AwaitSecond start)
                SecondClick -> (SecondClick, AwaitThird start)
                _ -> (ty', NoClick)
    endDrag ty st
        | ty `notElem` [MouseDrag, WheelUp, WheelDown, DoubleClick, TripleClick]
        , st.dragFlag /= 0 = (MouseDragEnd, st { dragFlag = 0, dragPane = Nothing })
        | otherwise = (ty, st)
    startDrag ev eb start st = st
        { dragFlag = (eb .&. buttons) + 1
        , dragStart = if st.dragFlag == 0 then start else st.dragStart
        , dragPane = case st.dragPane of
            Just p -> Just p
            Nothing -> ev.onPane }
    modifiers v = (testBit v 4, testBit v 3, testBit v 2)
    buttonNumber v = case v .&. buttons of
        0 -> 1
        1 -> 2
        2 -> 3
        66 -> 6
        67 -> 7
        128 -> 8
        129 -> 9
        130 -> 10
        131 -> 11
        _ -> 0

-- | Whether a window point is on a pane's box (tmux's
-- @window_pane_contains@): a tiled pane's to its right and bottom border, a
-- floating pane's with its border all round, if it has one.
contains :: PaneBox -> Int -> Int -> Bool
contains bx px py = case bx.kind of
    TiledBox -> px >= bx.xoff && px <= bx.xoff + bx.sx && py >= bx.yoff && py <= bx.yoff + bx.sy
    FramedFloat -> px >= bx.xoff - 1 && px <= bx.xoff + bx.sx
        && py >= bx.yoff - 1 && py <= bx.yoff + bx.sy
    BareFloat -> insideContent bx px py

insideContent :: PaneBox -> Int -> Int -> Bool
insideContent bx px py =
    px >= bx.xoff && px < bx.xoff + bx.sx && py >= bx.yoff && py < bx.yoff + bx.sy

-- | A client row in the window: below a top status line, or clamped above
-- a lower one.
windowRow :: Maybe Int -> Int -> Int -> Int
windowRow statusAt statusLines y
    | statusAt == Just 0 && y >= statusLines = y - statusLines
    | Just at <- statusAt, at > 0, y >= at = at - 1
    | otherwise = y

-- | Where an event (or, with @useLast@, the report before it) is in the
-- window.
windowPoint :: Bool -> MouseEvent -> (Int, Int)
windowPoint useLast ev =
    let (x, y) = if useLast then (ev.lastX, ev.lastY) else (ev.input.x, ev.input.y)
    in (x, windowRow ev.statusAt ev.statusLines y)

-- | tmux's click timer: once it lapses, a second click left waiting for a
-- third was a double click.
clickExpired :: Int -> MouseState -> (MouseState, Maybe MouseKey)
clickExpired now st = case st.click of
    AwaitThird c | now >= c.expires ->
        ( st { click = NoClick }
        , Just MouseKey
            { name = mouseKeyName DoubleClick (buttonOf c.button) c.loc
                (testBit c.button 4, testBit c.button 3, testBit c.button 2)
            , event = c.event { ignore = True }, dragging = False } )
    AwaitSecond c | now >= c.expires -> (st { click = NoClick }, Nothing)
    _ -> (st, Nothing)
  where
    buttonOf v = case v .&. buttons of
        0 -> 1
        1 -> 2
        2 -> 3
        _ -> 0

-- | tmux's name for a mouse key: modifiers, type, button, location.
mouseKeyName :: MouseType -> Int -> Location -> (Bool, Bool, Bool) -> Text
mouseKeyName ty bn loc (ctrl, meta, shift) =
    T.concat ([ "C-" | ctrl ] ++ [ "M-" | meta ] ++ [ "S-" | shift ]) <> base
  where
    base = case ty of
        MouseMove -> "MouseMove" <> place
        WheelUp -> "WheelUp" <> place
        WheelDown -> "WheelDown" <> place
        MouseDown -> "MouseDown" <> num <> place
        MouseUp -> "MouseUp" <> num <> place
        MouseDrag -> "MouseDrag" <> num <> place
        MouseDragEnd -> "MouseDragEnd" <> num <> place
        SecondClick -> "SecondClick" <> num <> place
        DoubleClick -> "DoubleClick" <> num <> place
        TripleClick -> "TripleClick" <> num <> place
    num = T.pack (show bn)
    place = case loc of
        LocPane -> "Pane"
        LocStatus -> "Status"
        LocStatusLeft -> "StatusLeft"
        LocStatusRight -> "StatusRight"
        LocStatusDefault -> "StatusDefault"
        LocBorder -> "Border"
        LocEmpty -> "Empty"
        LocControl n -> "Control" <> T.pack (show n)

-- | A mouse key's canonical name, from a spelling a binding may use:
-- modifiers in any order, any case.
mouseKeyByName :: Text -> Maybe Text
mouseKeyByName t = go (False, False, False) t
  where
    go (c, m, s) rest = case T.toUpper (T.take 2 rest) of
        "C-" -> go (True, m, s) (T.drop 2 rest)
        "M-" -> go (c, True, s) (T.drop 2 rest)
        "S-" -> go (c, m, True) (T.drop 2 rest)
        _ -> do
            base <- find ((== T.toLower rest) . T.toLower) allMouseKeys
            pure (T.concat ([ "C-" | c ] ++ [ "M-" | m ] ++ [ "S-" | s ]) <> base)
    allMouseKeys =
        [ mouseKeyName ty bn loc (False, False, False)
        | ty <- [MouseMove, WheelUp, WheelDown, MouseDown, MouseUp, MouseDrag
                , MouseDragEnd, SecondClick, DoubleClick, TripleClick]
        , bn <- if ty `elem` [MouseMove, WheelUp, WheelDown] then [0] else [1 .. 11]
        , loc <- [LocPane, LocStatus, LocStatusLeft, LocStatusRight, LocStatusDefault
                 , LocBorder, LocEmpty] ++ map LocControl [0 .. 9] ]

-- | Whether a key name is a mouse key (any type, location or modifier).
isMouseKey :: Text -> Bool
isMouseKey n = any (`T.isPrefixOf` stripMods n)
    [ "MouseMove", "MouseDown", "MouseUp", "MouseDrag", "WheelUp", "WheelDown"
    , "SecondClick", "DoubleClick", "TripleClick" ]

-- | Whether a key is a motion report, which never leaves a key table.
isMoveKey :: Text -> Bool
isMoveKey n = "MouseMove" `T.isPrefixOf` stripMods n

stripMods :: Text -> Text
stripMods t = case T.splitAt 2 t of
    (p, rest) | p `elem` ["C-", "M-", "S-"] -> stripMods rest
    _ -> t

-- | tmux's @cmd_mouse_at@: the event's position inside a pane's box, if it is
-- inside.
mouseAt :: Bool -> MouseEvent -> PaneBox -> Maybe (Int, Int)
mouseAt useLast ev bx =
    let (x0, y0) = if useLast then (ev.lastX, ev.lastY) else (ev.input.x, ev.input.y)
        y1 = if ev.statusAt == Just 0 && y0 >= ev.statusLines
                then y0 - ev.statusLines else y0
    in if x0 < bx.xoff || x0 >= bx.xoff + bx.sx || y1 < bx.yoff || y1 >= bx.yoff + bx.sy
        then Nothing
        else Just (x0 - bx.xoff, y1 - bx.yoff)

-- | The mouse tracking a pane's app turned on.
data PaneMouseMode = PaneMouseMode
    { tracking :: Int  -- ^ 0 off, 1 clicks (1000), 2 drags (1002), 3 all motion (1003)
    , sgr :: Bool      -- ^ ?1006
    , utf8 :: Bool     -- ^ ?1005
    }
    deriving (Eq, Show)

-- | tmux's @input_key_get_mouse@: the bytes a pane's app expects for an
-- event at a pane-relative position, or 'Nothing' when it asked for none of
-- this kind.
mouseBytes :: PaneMouseMode -> MouseEvent -> (Int, Int) -> Maybe ByteString
mouseBytes mode ev (px, py)
    | mode.tracking == 0 = Nothing
    | isDrag ev.input.b && mode.tracking < 2 = Nothing
    | ev.input.sgrType /= ' ', isDrag ev.input.sgrB, isRelease ev.input.sgrB
    , mode.tracking < 3 = Nothing
    | ev.input.sgrType == ' ', isDrag ev.input.b, isRelease ev.input.b
    , isRelease ev.lastB, mode.tracking < 3 = Nothing
    | ev.input.sgrType /= ' ' && mode.sgr = Just $ B8.pack $
        "\ESC[<" <> show ev.input.sgrB <> ";" <> show (px + 1) <> ";"
            <> show (py + 1) <> [ev.input.sgrType]
    | mode.utf8 =
        if ev.input.b > 0x7ff - 0x20 || px > 0x7ff - 0x21 || py > 0x7ff - 0x21
            then Nothing
            else Just ("\ESC[M" <> split2 (ev.input.b + 0x20) <> split2 (px + 0x21)
                        <> split2 (py + 0x21))
    | ev.input.b + 0x20 > 0xff = Nothing
    | otherwise = Just ("\ESC[M" <> B.pack (map fromIntegral
        [ev.input.b + 0x20, min 0xff (px + 0x21), min 0xff (py + 0x21)]))
  where
    -- tmux's @input_key_split2@: a value above 0x7f as two UTF-8 bytes.
    split2 c
        | c > 0x7f = B.pack (map fromIntegral [(c `div` 64) .|. 0xc0, (c .&. 0x3f) .|. 0x80])
        | otherwise = B.singleton (fromIntegral c)

-- | The mouse tracking a client asks of its terminal (tmux's
-- @server_client_reset_state@): the active pane's own, or with @mouse@ on
-- drags always and all motion when a pane or @focus-follows-mouse@ wants it.
-- Tracking levels are 'PaneMouseMode'\'s.
outerMouseMode :: Bool -> Bool -> Int -> [Int] -> Int
outerMouseMode mouse ffm active panes
    | not mouse = active
    | ffm || 3 `elem` panes = 3
    | otherwise = 2

-- | The mouse tracking a client asks of its terminal over a menu or popup
-- (tmux's @server_client_reset_state@ with an overlay): the overlay's own,
-- with @mouse@ on raised to drags, or to all motion for
-- @focus-follows-mouse@.
overlayMouseMode :: Bool -> Bool -> Int -> Int
overlayMouseMode mouse ffm own
    | not mouse = own
    | ffm || own == 3 = 3
    | otherwise = 2

-- | tmux's @tty_update_mode@: clear every mouse mode, then ask for the new
-- one, SGR-encoded.
mouseModeBytes :: Int -> ByteString
mouseModeBytes m = "\ESC[?1006l\ESC[?1000l\ESC[?1002l\ESC[?1003l" <> case m of
    3 -> "\ESC[?1006h\ESC[?1000h\ESC[?1002h\ESC[?1003h"
    2 -> "\ESC[?1006h\ESC[?1000h\ESC[?1002h"
    1 -> "\ESC[?1006h\ESC[?1000h"
    _ -> ""
