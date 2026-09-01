-- | The prefix flash: arming the prefix briefly blanks the active pane to
-- the terminal's background, and only when more than one pane is visible.
module Hat.Server.FlashSpec (spec) where

import Test.Hspec

import Control.Concurrent.STM
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V

import Hat.Geometry (Rect (..), Size (..))
import Hat.Model
import Hat.Model.Options (emptyDelta)
import Hat.Server.Flash (flashDeadline, flashExpired)
import Hat.Server.Layout (Layout (..), Orientation (..))
import Hat.Server.Render (blankFrame, blankRect)
import Hat.Server.Resize (windowArrange)
import Hat.Server.View (flashTarget)
import Hat.Term.Cell qualified as Cell

spec :: Spec
spec = do
    describe "flashTarget picks what the prefix flash highlights" $ do
        let ra = Rect { startRow = 0, endRow = 10, startCol = 0, endCol = 40 }
            rb = Rect { startRow = 0, endRow = 10, startCol = 41, endCol = 80 }
            both = [(PaneId 1, ra), (PaneId 2, rb)]

        it "a split window: the active pane's rect" $ do
            flashTarget both (PaneId 1) `shouldBe` Just ra
            flashTarget both (PaneId 2) `shouldBe` Just rb

        it "a window with a single visible pane: no flash" $
            flashTarget [(PaneId 1, ra)] (PaneId 1) `shouldBe` Nothing

        it "a zoomed pane arranges alone, so no flash" $ do
            win <- splitWindow
            atomically $ writeTVar win.zoomed (Just (PaneId 1))
            (rects, _) <- atomically $
                windowArrange (Size { rows = 24, cols = 80 }) win
            flashTarget rects (PaneId 1) `shouldBe` Nothing

        it "an active pane missing from the layout: no flash" $
            flashTarget both (PaneId 9) `shouldBe` Nothing

    describe "blankRect" $ do
        let sz = Size { rows = 4, cols = 6 }
            rect = Rect { startRow = 1, endRow = 3, startCol = 2, endCol = 5 }
            busy = V.map (V.map (const x)) (blankFrame sz)
            x = Cell.glyphCell 'x' Cell.defaultStyle

        it "blanks to the default background exactly inside the rect" $ do
            let frame = blankRect busy rect
            [ (r, c)
                | (r, row) <- zip [0 :: Int ..] (V.toList frame)
                , (c, cell) <- zip [0 :: Int ..] (V.toList row)
                , cell == Cell.blankCell ]
                `shouldBe` [ (r, c) | r <- [1, 2], c <- [2, 3, 4] ]

    describe "flash lifetime" $ do
        let shownAt = 41000000000
            flash = Flash { deadline = flashDeadline shownAt }

        it "alive when shown, gone within a second" $ do
            flashExpired shownAt flash `shouldBe` False
            flashExpired (shownAt + 1000000000) flash `shouldBe` True

-- A two-pane split window, bare TVars around the fields 'windowArrange'
-- reads.
splitWindow :: IO Window
splitWindow = do
    win <- Window (WindowId 0)
        <$> newTVarIO "w"
        <*> newTVarIO (Split LeftRight (1 / 2) (Leaf (PaneId 1)) (Leaf (PaneId 2)))
        <*> newTVarIO Nothing
        <*> newTVarIO Map.empty
        <*> newTVarIO (PaneId 1)
        <*> newTVarIO []
        <*> newTVarIO False
        <*> newTVarIO False
        <*> newTVarIO False
        <*> newTVarIO 0
        <*> newTVarIO Nothing
        <*> newTVarIO True
        <*> newTVarIO emptyDelta
    mapM_
        (\n -> do
            p <- flashStubPane (PaneId n)
            atomically $ modifyTVar' win.panes (Map.insert p.id p))
        [1, 2]
    pure win

flashStubPane :: PaneId -> IO Pane
flashStubPane pid = do
    sizeV <- newTVarIO (Size { rows = 24, cols = 80 })
    deadV <- newTVarIO False
    modeV <- newTVarIO Nothing
    pipeV <- newTVarIO Nothing
    tidV <- newTVarIO Nothing
    optsV <- newTVarIO emptyDelta
    pure Pane
        { id = pid
        , pty = error "flashStubPane: pty never touched"
        , emulator = error "flashStubPane: emulator never touched"
        , size = sizeV
        , dead = deadV
        , startCwd = "/"
        , mode = modeV
        , options = optsV
        , pipe = pipeV
        , readerTid = tidV
        , pendingInput = Nothing
        }
