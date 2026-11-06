-- | The client render seam: the server's messages stepped through
-- 'renderStep', the bytes shown on a terminal emulator as a real terminal
-- would show them.
module Hat.Client.RenderSpec (spec) where

import Data.ByteString qualified as B
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Hspec

import Hat.Client (ClientRender (..), ExitReason (..), Step (..), renderStep)
import Hat.Client.Draw (Dialect, dialectOf)
import Hat.Geometry (Pos (..), Size (..))
import Hat.Server.Render (Frame, composeFrame, diffFrame, fullRedraw)
import Hat.Term.Caps (createTerm)
import Hat.Term.Cell
    (Cell (..), Color (..), Content (..), Style (..), blankCell, defaultStyle, glyphCell)
import Hat.Term.Emulator qualified as Emu
import Hat.Term.HostProtocol
    (CursorChange (..), CursorModel (..), CursorStyle (..), applyCursorChange
    , defaultCursorModel, terminalCursorBytes)
import Hat.Transport.Wire (DrawOp (..), ServerToClient (..))

-- | An attached client and its terminal: the bytes each message makes the
-- client write are what the terminal shows.
data Terminal = Terminal
    { client :: IORef ClientRender
    , screen :: Emu.Emulator
    , reported :: IORef [Emu.Event]  -- ^ what the terminal reported, in order
    }

-- | The capabilities every test terminal has besides the ones given.
baseCaps :: [Text]
baseCaps = ["clear=\ESC[H\ESC[2J", "cup=x", "am=1", "colors=256"]

-- | The dialect of a terminal with these capabilities besides 'baseCaps'.
dialect :: [Text] -> Dialect
dialect extra = either (error . show) dialectOf $
    createTerm "vt100" (baseCaps <> extra) [] [] Nothing

-- | A terminal that takes titles.
titled :: Dialect
titled = dialect ["tsl=\ESC]0;", "fsl=\a"]

newTerminal :: Dialect -> Size -> IO Terminal
newTerminal d sz = Terminal
    <$> newIORef ClientRender { dialect = d, size = sz }
    <*> Emu.newEmulator sz 0
    <*> newIORef []

-- | One message from the server, its bytes shown on the terminal.
deliver :: Terminal -> ServerToClient -> IO Step
deliver t msg = do
    step <- renderStep <$> readIORef t.client <*> pure msg
    writeIORef t.client step.render
    evs <- Emu.feed t.screen step.output
    modifyIORef' t.reported (<> evs)
    pure step

-- | The frame, with its cursor at the origin: how the server sends one.
frame :: [DrawOp] -> ServerToClient
frame ops = Draw (ops <> [CursorAt (Pos 0 0) True])

screenRows :: Terminal -> IO [Text]
screenRows t = do
    scr <- Emu.snapshot t.screen
    pure [ T.stripEnd (Emu.screenRowText scr r) | r <- [0 .. V.length scr.cells - 1] ]

-- | A frame of these lines of plain text at this size.
frameOf :: Size -> [Text] -> Frame
frameOf sz ls = composeFrame sz $ V.fromList
    [ V.fromList (map (`glyphCell` defaultStyle) (T.unpack l)) | l <- ls ]

-- | A frame's text, as 'screenRows' reads a terminal's.
frameRows :: Frame -> [Text]
frameRows = map (T.stripEnd . T.pack . map glyph . V.toList) . V.toList
  where
    glyph c = case c.content of
        Glyph ch _ _ -> ch
        _ -> ' '

-- | A frame of this size whose every cell is this glyph.
filled :: Size -> Char -> Frame
filled sz ch = frameOf sz $
    replicate (fromIntegral sz.rows) (T.replicate (fromIntegral sz.cols) (T.singleton ch))

spec :: Spec
spec = do
    -- The painter half of "renders vim to two simultaneous clients": a
    -- client's frame is shown at that client's size, whatever another's is.
    it "shows each client its own frame at its own size" $ do
        let wide = Size 6 20
            narrow = Size 3 8
            wideFrame = frameOf wide ["alpha", "", "wide client"]
            narrowFrame = frameOf narrow ["beta", "narrow"]
        a <- newTerminal titled wide
        b <- newTerminal titled narrow
        _ <- deliver a (frame (fullRedraw wideFrame))
        _ <- deliver b (frame (fullRedraw narrowFrame))
        screenRows a `shouldReturn` frameRows wideFrame
        screenRows b `shouldReturn` frameRows narrowFrame

    -- The painter half of a shrunk client's repaint (bug 97) and of a
    -- program leaving the alternate screen (bug 6): the server's full
    -- redraw clears what was there, not only the cells it writes.
    it "a full redraw replaces the whole screen" $ do
        let sz = Size 4 10
            sparse = frameOf sz ["", "back"]
        t <- newTerminal titled sz
        _ <- deliver t (frame (fullRedraw (filled sz 'X')))
        _ <- deliver t (frame (fullRedraw sparse))
        screenRows t `shouldReturn` frameRows sparse

    -- Keystroke echo and a joined pane's output ride frame diffs: the
    -- server's diff from the frame on screen must land as the new frame.
    it "a frame diff takes the terminal from the old frame to the new" $ do
        let sz = Size 4 12
            old = frameOf sz ["$ vim", "~", "~"]
            new = frameOf sz ["$ vim", "hello from", "~", "-- INSERT --"]
        t <- newTerminal titled sz
        _ <- deliver t (frame (fullRedraw old))
        _ <- deliver t (frame (diffFrame old new))
        screenRows t `shouldReturn` frameRows new

    -- An overlay scrollbar is a run of background colour; the colour must
    -- survive the trip to the terminal.
    it "a styled cell's background reaches the terminal" $ do
        let sz = Size 3 6
            bar = defaultStyle { bg = Indexed 4 }
            f = frameOf sz [] V.// [(1, V.replicate 5 blankCell <> V.singleton (glyphCell ' ' bar))]
        t <- newTerminal titled sz
        _ <- deliver t (frame (fullRedraw f))
        scr <- Emu.snapshot t.screen
        ((scr.cells V.! 1) V.! 5).style.bg `shouldBe` Indexed 4

    -- set-titles: the server's title reaches the outer terminal through
    -- the terminal's own tsl/fsl.
    it "titles the terminal with the server's title" $ do
        t <- newTerminal titled (Size 2 10)
        _ <- deliver t (SetTitle "T:0")
        readIORef t.reported >>= (`shouldSatisfy` elem (Emu.TitleRequest "T:0"))

    -- cursor-style: the server's DECSCUSR notify reaches the terminal verbatim.
    it "shapes the terminal's cursor as the server's notify says" $ do
        let bar = applyCursorChange (CursorStyleSet 6) defaultCursorModel
        t <- newTerminal titled (Size 2 10)
        _ <- deliver t (Notify (terminalCursorBytes bar))
        (.style) <$> Emu.appCursor t.screen `shouldReturn` StyleBar

    -- A terminal the server opens after attach is drawn to as it can be:
    -- its features turned on, and no title for one without tsl/fsl.
    it "draws in the dialect of a terminal the server opens after attach" $ do
        t <- newTerminal titled (Size 2 10)
        opened <- deliver t
            (OpenedTerminal (baseCaps <> ["Enfcs=\ESC[?1004h", "Enbp=\ESC[?2004h"]) [])
        opened.output `shouldBe` "\ESC[?1004h\ESC[?2004h"
        untitled <- deliver t (SetTitle "T:0")
        untitled.output `shouldBe` ""

    it "maps the server's farewells to exit reasons" $ do
        let r = ClientRender { dialect = titled, size = Size 2 10 }
            outcomeOf = (.outcome) . renderStep r
        outcomeOf Exited `shouldBe` Just SessionEnded
        outcomeOf DetachOk `shouldBe` Just Detached
        outcomeOf (RestartClientTo "s" ["a"]) `shouldBe` Just (RestartRequested (Just "s") ["a"])
        outcomeOf (ServerError "no") `shouldBe` Just (Rejected "no")
        map outcomeOf [Draw [], Message "m", CommandDone, ServerVersion 0]
            `shouldBe` replicate 4 Nothing
        B.null (renderStep r (Message "m")).output `shouldBe` True
