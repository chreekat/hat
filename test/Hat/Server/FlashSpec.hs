-- | The prefix flash: arming the prefix briefly tints the active pane's
-- border, and only when more than one pane is visible.
module Hat.Server.FlashSpec (spec) where

import Test.Hspec

import Control.Concurrent.STM
import Data.Map.Strict qualified as Map

import Hat.Geometry (Pos (..), Rect (..), Size (..))
import Hat.Model
import Hat.Model.Options
    (BorderIndicators (..), Options (..), defaultOptions, emptyDelta)
import Hat.Server.Flash (flashDeadline, flashExpired)
import Hat.Server.Layout (Layout (..), Orientation (..))
import Hat.Server.Resize (windowArrange)
import Hat.Server.View (borderCells, flashTarget)
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

    describe "the flash tints the active pane's border" $ do
        let activeSty = Cell.defaultStyle { Cell.fg = Cell.Indexed 9 }
            borderSty = Cell.defaultStyle { Cell.fg = Cell.Indexed 5 }
            tint = Cell.defaultStyle { Cell.fg = Cell.Indexed 12 }
            rect = Rect { startRow = 0, endRow = 2, startCol = 1, endCol = 3 }
            onEdge = Pos { row = 0, col = 0 }
            offEdge = Pos { row = 5, col = 5 }
            styleAt ind p = fmap (.style) . lookup p $ borderCells
                defaultOptions
                    { paneBorderStyle = borderSty
                    , paneActiveBorderStyle = activeSty
                    , paneBorderIndicators = ind }
                (Just rect) (Just tint) 0
                [ (onEdge, '\x2502'), (offEdge, '\x2502') ]

        it "an active-perimeter cell takes the tint over the active style" $
            styleAt IndicatorsColour onEdge `shouldBe` Just tint

        it "a cell off the perimeter keeps pane-border-style" $
            styleAt IndicatorsColour offEdge `shouldBe` Just borderSty

        it "the tint shows even with pane-border-indicators off" $
            styleAt IndicatorsOff onEdge `shouldBe` Just tint

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
