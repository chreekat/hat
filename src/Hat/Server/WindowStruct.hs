-- | One window's structure, read out of the live tree as a single
-- consistent unit — the shared first step of both things that record a
-- window: the persistence mirror and the reload handover.
module Hat.Server.WindowStruct
    ( WindowStruct (..)
    , windowStruct
    ) where

import Control.Concurrent.STM
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)

import Hat.Geometry
import Hat.Model
import Hat.Server.Floating (FloatState (..), Modal (..), stacking)
import Hat.Server.Layout (fitLayout, layoutPanes)
import Hat.Server.LayoutString (emitLayoutZ)
import Hat.Server.Persist (ModalSnap (..))

-- | One window's structure read as a single consistent unit. Its layout,
-- active\/last-active ordinals and live panes are read together in one STM
-- transaction, so a concurrent split or close cannot leave the saved layout
-- referring to panes the snapshot dropped. The per-pane cwd and argv are
-- gathered afterwards in IO ('captureWindow').
data WindowStruct = WindowStruct
    { wsIx         :: Int
    , wsName       :: Text
    , wsLayout     :: Text
    , wsActive     :: Int
    , wsLastActive :: [Int]  -- ^ MRU pane ordinals, head first
    , wsAutoRename :: Bool
    , wsModal      :: Maybe ModalSnap
    , wsPanes      :: [Pane]
    }

windowStruct :: Size -> (Int, Window) -> STM WindowStruct
windowStruct eff (wix, w) = do
    nm       <- readTVar w.name
    lay      <- readTVar w.layout
    activeId <- readTVar w.activeId
    hist     <- readTVar w.paneHist
    auto     <- readTVar w.autoRename
    paneMap  <- readTVar w.panes
    fs       <- readTVar w.floats
    let order = layoutPanes lay
        ordinal p = List.elemIndex p order
        modal = do
            m <- fs.modal
            p <- ordinal m.pane
            pure ModalSnap
                { pane = p, returnTo = m.returnTo >>= ordinal, rezoom = m.rezoom }
        activeOrd = fromMaybe 0 (List.elemIndex activeId order)
        lastOrds = mapMaybe (`List.elemIndex` order) hist
    pure WindowStruct
        { wsIx = wix, wsName = nm
        , wsLayout = emitLayoutZ (stacking fs lay) (fitLayout eff lay)
        , wsActive = activeOrd, wsLastActive = lastOrds
        , wsAutoRename = auto
        , wsModal = modal
        , wsPanes = mapMaybe (`Map.lookup` paneMap) order }