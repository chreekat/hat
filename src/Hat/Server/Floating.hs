-- | A window's floating panes beyond their cells in the layout tree: their
-- stacking (tmux's @z_index@), the modal pane that holds a window's focus
-- (@w->modal@), and the arithmetic of the commands that place them
-- (@layout_floating_args_parse@, @move-pane -P@\/@-z@).
module Hat.Server.Floating
    ( FloatState (..)
    , Modal (..)
    , noFloats
    , stacking
    , paneZ
    , stackNew
    , Restack (..)
    , parseRestack
    , restack
    , restackTo
    , swapStacked
    , ModalReturn (..)
    , loseModal
    , modalPane
    , Border (..)
    , borderOf
    , GeomArgs (..)
    , floatingGeom
    , FloatSplit (..)
    , splitFloatingGeom
    , Spot (..)
    , parseSpot
    , placeAt
    ) where

import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Hat.Geometry (Size (..))
import Hat.Model.Options (BorderLines (..))
import Hat.Model.Ids (PaneId)
import Hat.Server.ArgNumber (stringPercentage)
import Hat.Server.Layout (FloatGeom (..), Layout, floatingCells)

-- | What a window keeps about its floating panes outside the layout tree.
data FloatState = FloatState
    { zorder  :: [PaneId]
        -- ^ floating panes, frontmost first; it may name panes no longer
        -- floating, and omit new ones (see 'stacking')
    , modal   :: Maybe Modal
    , lastNew :: (Int, Int)
        -- ^ where the last floating pane placed without @-X@\/@-Y@ went
        -- (@last_new_pane_x@\/@_y@); @(0, 0)@ before any
    }
    deriving (Eq, Show)

-- | The window's modal pane: the only pane that may be active while it
-- lives.
data Modal = Modal
    { pane     :: PaneId
    , returnTo :: Maybe PaneId
        -- ^ the pane active when it opened, active again when it closes
        -- (@modal_last@)
    , rezoom   :: Bool
        -- ^ the window was zoomed when it opened, and zooms again when it
        -- closes (@WINDOW_WASMODALZOOMED@)
    }
    deriving (Eq, Show)

noFloats :: FloatState
noFloats = FloatState { zorder = [], modal = Nothing, lastNew = (0, 0) }

-- | The modal pane, if any.
modalPane :: FloatState -> Maybe PaneId
modalPane fs = (.pane) <$> fs.modal

-- | The floating panes front to back: any 'zorder' does not name come
-- first, in layout order, then 'zorder''s still floating.
stacking :: FloatState -> Layout -> [PaneId]
stacking fs lay = unnamed <> [ p | p <- fs.zorder, p `elem` floats ]
  where
    floats = map fst (floatingCells lay)
    unnamed = filter (`notElem` fs.zorder) floats

-- | A pane's @pane_z@ (@window_pane_zindex@): a floating pane's place in
-- 'stacking', front at 0; every tiled pane sits behind them all, at one
-- more than their count.
paneZ :: FloatState -> Layout -> PaneId -> Int
paneZ fs lay pid = fromMaybe (length stack + 1) (List.elemIndex pid stack)
  where
    stack = stacking fs lay

-- | Stack a new floating pane: at the front, or just behind the modal
-- pane while there is one (@window_add_pane@).
stackNew :: PaneId -> FloatState -> Layout -> FloatState
stackNew pid fs lay = fs { zorder = case modalPane fs of
    Just m | (pre, m' : post) <- break (== m) stack -> pre <> [m', pid] <> post
    _ -> pid : stack }
  where
    stack = filter (/= pid) (stacking fs lay)

-- | A @move-pane -P@ stacking move.
data Restack
    = ToFront
    | ToBack
    | Forward
    | Backward
    | ForwardLoop
    | BackwardLoop
    deriving (Eq, Show)

parseRestack :: Text -> Maybe Restack
parseRestack = \case
    "front" -> Just ToFront
    "back" -> Just ToBack
    "forward" -> Just Forward
    "backward" -> Just Backward
    "forward-loop" -> Just ForwardLoop
    "backward-loop" -> Just BackwardLoop
    _ -> Nothing

-- | Move a floating pane in the stacking (@cmd_join_pane_place@): one step
-- toward the front or back, to either end, or a step that wraps around at
-- the end it reaches.
restack :: Restack -> PaneId -> FloatState -> Layout -> FloatState
restack how pid fs lay = fs { zorder = case how of
    ToFront -> pid : rest
    ToBack -> rest <> [pid]
    Forward -> case before of
        [] -> stack
        _ -> init before <> [pid, last before] <> after
    Backward -> case after of
        [] -> stack
        next : more -> before <> [next, pid] <> more
    ForwardLoop -> case before of
        [] -> rest <> [pid]
        _ -> init before <> [pid, last before] <> after
    BackwardLoop -> case after of
        [] -> pid : rest
        next : more -> before <> [next, pid] <> more }
  where
    stack = stacking fs lay
    (before, after) = fmap (drop 1) (break (== pid) stack)
    rest = filter (/= pid) stack

-- | Put a floating pane at a stacking depth, 0 the front, past the back
-- clamped to it (@move-pane -z@).
restackTo :: Int -> PaneId -> FloatState -> Layout -> FloatState
restackTo z pid fs lay = fs { zorder = take z rest <> [pid] <> drop z rest }
  where
    rest = filter (/= pid) (stacking fs lay)

-- | Two panes trade places in the stacking as they trade cells
-- (@swap-pane@).
swapStacked :: PaneId -> PaneId -> FloatState -> FloatState
swapStacked a b fs = fs { zorder = map swap fs.zorder }
  where
    swap p
        | p == a = b
        | p == b = a
        | otherwise = p

-- | What a window must do when its modal pane goes: activate the pane it
-- returns to, if it is still there, and zoom it again if the window was
-- zoomed when the modal opened.
data ModalReturn = ModalReturn
    { returnTo :: Maybe PaneId
    , rezoom   :: Bool
    }
    deriving (Eq, Show)

-- | A pane leaves the window (@window_lost_pane@): the modal pane's going
-- yields its 'ModalReturn'; the pane it would return to going leaves it
-- nothing to return to, nor to rezoom.
loseModal :: PaneId -> FloatState -> (FloatState, Maybe ModalReturn)
loseModal pid fs = case fs.modal of
    Just m
        | m.pane == pid ->
            ( fs { modal = Nothing }
            , Just ModalReturn { returnTo = m.returnTo, rezoom = m.rezoom } )
        | m.returnTo == Just pid ->
            (fs { modal = Just (Modal m.pane Nothing False) }, Nothing)
    _ -> (fs, Nothing)

-- | Whether a floating pane draws a border around its content box
-- (@pane-border-lines@ other than @none@).
data Border = Bordered | Borderless
    deriving (Eq, Show)

-- | Whether @pane-border-lines@ draws a border.
borderOf :: BorderLines -> Border
borderOf = \case
    BorderSingle -> Bordered
    BorderHeavy -> Bordered
    BorderDouble -> Bordered
    BorderSimple -> Bordered

-- | The geometry flags of a floating pane's spawn: @-x@\/@-y@ (size) and
-- @-X@\/@-Y@ (position), cells or percentages of the window, already
-- format-expanded.
data GeomArgs = GeomArgs
    { sizeX :: Maybe Text
    , sizeY :: Maybe Text
    , posX  :: Maybe Text
    , posY  :: Maybe Text
    }
    deriving (Eq, Show)

-- | A new floating pane's geometry from its flags, in a window of @area@
-- (@layout_floating_args_parse@). The sizes and positions name the
-- on-screen box, border included: a bordered pane's content is two cells
-- smaller and one cell further in. Without a size, a quarter of the
-- window's height and half its width (or the @start@ geometry, for a pane
-- that floated before); without a position, a cascade four columns and two
-- rows on from the last such pane. Yields the new cascade position too.
floatingGeom
    :: Size -> Border -> Maybe FloatGeom -> (Int, Int) -> GeomArgs
    -> Either Text (FloatGeom, (Int, Int))
floatingGeom area border start (lastX, lastY) args = do
    let wsx = fromIntegral area.cols
        wsy = fromIntegral area.rows
        inset = if border == Bordered then 2 else 0
        shift = if border == Bordered then 1 else 0
        position e = either (Left . ("position " <>)) Right e
    sx <- case args.sizeX of
        Just v -> subtract inset <$> position (stringPercentage 0 paneMaximum wsx v)
        Nothing -> Right (maybe (wsx `div` 2) (.width) start)
    sy <- case args.sizeY of
        Just v -> subtract inset <$> position (stringPercentage 0 paneMaximum wsy v)
        Nothing -> Right (maybe (wsy `div` 4) (.height) start)
    mox <- traverse (\v -> (+ shift) <$> position (stringPercentage (negate sx) wsx wsx v))
        args.posX
    moy <- traverse (\v -> (+ shift) <$> position (stringPercentage (negate sy) wsy wsy v))
        args.posY
    let (ox, lastX') = case (mox, start) of
            (Just x, _) -> (x, lastX)
            (Nothing, Just g) -> (g.left, lastX)
            (Nothing, Nothing) ->
                let x = if lastX == 0 || lastX > wsx then 4 else lastX + 4
                in (x, x)
        (oy, lastY') = case (moy, start) of
            (Just y, _) -> (y, lastY)
            (Nothing, Just g) -> (g.top, lastY)
            (Nothing, Nothing) ->
                let y = if lastY == 0 || lastY > wsy then 2 else lastY + 2
                in (y, y)
    if sx < 1 || sx > paneMaximum then Left "invalid width"
    else if sy < 1 || sy > paneMaximum then Left "invalid height"
    else Right (FloatGeom { width = sx, height = sy, left = ox, top = oy }, (lastX', lastY'))

-- | How @split-window@ divides a floating pane: across (@-h@) or down,
-- the new pane before (@-b@) or after it, and @-f@ to take the whole
-- window's span along the other axis.
data FloatSplit = FloatSplit
    { across   :: Bool
    , before   :: Bool
    , fullSpan :: Bool
    }
    deriving (Eq, Show)

-- | Split a floating pane's box into the box it keeps and a new floating
-- pane's (@layout_split_floating_cell@): the pane is first pulled inside
-- the window, the new box goes beside it, and when that would leave the
-- window the space there is shared between the two.
splitFloatingGeom
    :: Size -> Border -> FloatSplit -> FloatGeom
    -> Either Text (FloatGeom, FloatGeom)
splitFloatingGeom area border how g0
    | minimum [new.width, new.height, old.width, old.height] < 1 =
        Left "no space for a new pane"
    | otherwise = Right (old, new)
  where
    wsx = fromIntegral area.cols
    wsy = fromIntegral area.rows
    (tb, bb, lb, rb) = (1, wsy - 1, 3, wsx - 3) :: (Int, Int, Int, Int)
    b = if border == Bordered then 1 else 0
    inBounds g =
        let x1 = if lb > g.left - b then lb + b else g.left
            x2 = if rb < x1 + g.width + b then rb - g.width - b else x1
            y1 = if tb > g.top - b then tb + b else g.top
            y2 = if bb < y1 + g.height + b then bb - g.height - b else y1
        in g { left = x2, top = y2 }
    old1 = inBounds g0
    new1
        | how.across && how.before = old1 { left = old1.left - (old1.width + 2 * b) }
        | how.across = old1 { left = old1.left + old1.width + 2 * b }
        | how.before = old1 { top = old1.top - (old1.height + 2 * b) }
        | otherwise = old1 { top = old1.top + old1.height + 2 * b }
    (old2, new2)
        | lb > new1.left - b =
            let space = old1.left + old1.width - lb - 3 * b + 1
                size = space `quot` 2
                n = new1 { width = size, left = lb + b }
                o = old1 { width = size - (if even space then 1 else 0)
                         , left = lb + b + size + 2 * b }
            in (o, n)
        | rb < new1.left + new1.width + b =
            let space = rb - old1.left - 3 * b + 1
                size = space `quot` 2
                o = old1 { width = size }
                n = new1 { width = size - (if even space then 1 else 0)
                         , left = old1.left + size + 2 * b }
            in (o, n)
        | tb > new1.top - b =
            let space = old1.height + old1.top - tb - 3 * b + 1
                size = space `quot` 2
                n = new1 { height = size, top = tb + b }
                o = old1 { height = size - (if even space then 1 else 0)
                         , top = tb + b + size + 2 * b }
            in (o, n)
        | bb < new1.top + new1.height + b =
            let space = bb - old1.top - 3 * b + 1
                size = space `quot` 2
                o = old1 { height = size }
                n = new1 { height = size - (if even space then 1 else 0)
                         , top = old1.top + size + 2 * b }
            in (o, n)
        | otherwise = (old1, new1)
    old = old2
    new
        | not how.fullSpan = new2
        | how.across =
            let n = new2 { top = tb + b, height = bb - tb - 2 * b }
            in if how.before
                then n { left = lb + b, width = old2.left - (lb + b) - 2 * b }
                else n { width = rb - new2.left - b }
        | otherwise =
            let n = new2 { left = lb + b, width = rb - lb - 2 * b }
            in if how.before
                then n { top = tb + b, height = old2.top - (tb + b) - 2 * b }
                else n { height = bb - new2.top - b }

-- | tmux's @PANE_MAXIMUM@.
paneMaximum :: Int
paneMaximum = 10000

-- | A @move-pane -P@ position name.
data Spot
    = TopLeft | TopCentre | TopRight
    | CentreLeft | Centre | CentreRight
    | BottomLeft | BottomCentre | BottomRight
    | TopLeftCentre | TopRightCentre | BottomLeftCentre | BottomRightCentre
    deriving (Eq, Show)

parseSpot :: Text -> Maybe Spot
parseSpot = \case
    "top-left" -> Just TopLeft
    "top-centre" -> Just TopCentre
    "top-center" -> Just TopCentre
    "top-right" -> Just TopRight
    "centre-left" -> Just CentreLeft
    "center-left" -> Just CentreLeft
    "centre" -> Just Centre
    "center" -> Just Centre
    "centre-right" -> Just CentreRight
    "center-right" -> Just CentreRight
    "bottom-left" -> Just BottomLeft
    "bottom-centre" -> Just BottomCentre
    "bottom-center" -> Just BottomCentre
    "bottom-right" -> Just BottomRight
    "top-left-centre" -> Just TopLeftCentre
    "top-left-center" -> Just TopLeftCentre
    "top-right-centre" -> Just TopRightCentre
    "top-right-center" -> Just TopRightCentre
    "bottom-left-centre" -> Just BottomLeftCentre
    "bottom-left-center" -> Just BottomLeftCentre
    "bottom-right-centre" -> Just BottomRightCentre
    "bottom-right-center" -> Just BottomRightCentre
    _ -> Nothing

-- | A floating pane moved to a named spot in a window of @area@, its size
-- kept (@cmd_join_pane_place@): the edges hold its border, if it has one,
-- just inside the window.
placeAt :: Size -> Border -> Spot -> FloatGeom -> FloatGeom
placeAt area border spot g = g { left = x, top = y }
  where
    wx = fromIntegral area.cols
    wy = fromIntegral area.rows
    px = g.width
    py = g.height
    b = if border == Bordered then 1 else 0
    midX = (wx - px) `div` 2
    midY = (wy - py) `div` 2
    (x, y) = case spot of
        TopLeft -> (b, b)
        TopCentre -> (midX, b)
        TopRight -> (wx - px - b, b)
        CentreLeft -> (b, midY)
        Centre -> (midX, midY)
        CentreRight -> (wx - px - b, midY)
        BottomLeft -> (b, wy - py - b)
        BottomCentre -> (midX, wy - py - b)
        BottomRight -> (wx - px - b, wy - py - b)
        TopLeftCentre -> (wx `div` 4 - px `div` 2, wy `div` 4 - py `div` 2)
        TopRightCentre -> ((3 * wx) `div` 4 - px `div` 2, wy `div` 4 - py `div` 2)
        BottomLeftCentre -> (wx `div` 4 - px `div` 2, (3 * wy) `div` 4 - py `div` 2)
        BottomRightCentre -> ((3 * wx) `div` 4 - px `div` 2, (3 * wy) `div` 4 - py `div` 2)
