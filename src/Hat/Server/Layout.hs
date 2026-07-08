-- | Pane geometry: the layout tree and its projection onto a window's
-- rectangle. Splits arrive in M4; today a window holds one pane.
module Hat.Server.Layout
    ( Layout (..)
    , paneRects
    , layoutPanes
    ) where

import Hat.Geometry
import Hat.Model.Ids (PaneId)

data Layout
    = Leaf PaneId
    deriving (Eq, Show)

-- | Where each pane lands inside a window of the given size.
paneRects :: Size -> Layout -> [(PaneId, Rect)]
paneRects sz (Leaf pid) =
    [ ( pid
      , Rect
          { startRow = 0
          , endRow = fromIntegral sz.rows
          , startCol = 0
          , endCol = fromIntegral sz.cols
          }
      )
    ]

layoutPanes :: Layout -> [PaneId]
layoutPanes (Leaf pid) = [pid]
