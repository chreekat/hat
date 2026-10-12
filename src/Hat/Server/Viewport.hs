{-# LANGUAGE MultiWayIf #-}

-- | Which part of a window larger than its client the client shows: tmux's
-- @tty_window_offset1@, and the panning @refresh-client@ applies.
module Hat.Server.Viewport
    ( viewportFor
    , Pan (..)
    , panViewport
    ) where

import Hat.Geometry (Pos (..), Size (..))
import Hat.Model (Viewport (..), WindowId)

-- | The view of window @wid@ (of size @area@) a scene of @scene@ draws: the
-- whole window when it fits; else a pan made on this window, clamped to it;
-- else a view that keeps the active pane's shown cursor in sight.
viewportFor :: WindowId -> Size -> Size -> Maybe Pos -> Viewport -> Viewport
viewportFor wid area scene mcursor prev
    | sx >= wsx && sy >= wsy = Viewport (Pos 0 0) area Nothing
    | prev.pinned == Just wid = Viewport
        { origin = Pos { row = clampTo sy wsy prev.origin.row
                       , col = clampTo sx wsx prev.origin.col }
        , extent = scene, pinned = Just wid }
    | otherwise = Viewport
        { origin = maybe (Pos 0 0) follow mcursor, extent = scene, pinned = Nothing }
  where
    wsx = fromIntegral area.cols :: Int
    wsy = fromIntegral area.rows :: Int
    sx = fromIntegral scene.cols :: Int
    sy = fromIntegral scene.rows :: Int
    clampTo s ws o
        | s >= ws = 0
        | otherwise = min o (ws - s)
    follow c = Pos
        { row = if | c.row < sy -> 0
                   | c.row > wsy - sy -> wsy - sy
                   | otherwise -> c.row - sy + 1
        , col = if | c.col < sx -> 0
                   | c.col > wsx - sx -> wsx - sx
                   | otherwise -> c.col - sx `div` 2 }

-- | A @refresh-client@ move of the view, by a number of cells.
data Pan = PanUp Int | PanDown Int | PanLeft Int | PanRight Int | Unpan
    deriving (Eq, Show)

-- | Move the view of window @wid@ (of size @area@): a pan starts from the
-- view last drawn and pins it to the window until 'Unpan'.
panViewport :: Pan -> WindowId -> Size -> Viewport -> Viewport
panViewport pan wid area v = case pan of
    Unpan -> v { pinned = Nothing }
    PanUp n -> pinTo o { row = max 0 (o.row - n) }
    PanDown n -> pinTo o { row = min (room v.extent.rows area.rows) (o.row + n) }
    PanLeft n -> pinTo o { col = max 0 (o.col - n) }
    PanRight n -> pinTo o { col = min (room v.extent.cols area.cols) (o.col + n) }
  where
    o = v.origin
    pinTo p = v { origin = p, pinned = Just wid }
    room s ws = max 0 (fromIntegral ws - fromIntegral s :: Int)
