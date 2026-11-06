-- | The client render seam, driven over a real socketpair: what 'renderOnce'
-- sends must take the client's screen to the server's view of its session —
-- after a shrink (regression guard, bug 97), for several clients on one
-- pane, and across a program's alternate-screen exit (bug 6).
module Hat.Server.ResizeRenderSpec (spec) where

import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO, writeTVar)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.IORef (newIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Vector qualified as V
import Network.Socket
    (Family (AF_UNIX), SocketType (Stream), close, socketPair)
import System.Timeout (timeout)
import Test.Hspec

import Hat.Geometry (Pos (..), Size (..))
import Hat.Log (newLogger)
import Hat.Model hiding (Toast (..))
import Hat.Server.ClientIO (withClientWriter)
import Hat.Server.Dispatch (runArgv)
import Hat.Server.Outbound (newOutbound)
import Hat.Server.Keys (EscPending (NoEscPending), initialKeyState)
import Hat.Server.Render (Frame, blankFrame)
import Hat.Server.View (renderOnce)
import Hat.Term.Cell
import Hat.Term.Emulator qualified as Emu
import Hat.Term.Pty qualified as Pty
import Hat.TestSupport (awaitTrue, realPanes, withServer)
import Hat.Transport.Wire
    (Autostart (..), DrawOp (..), Inbound (..), ServerToClient (..)
    , ReadEnd (..), newReadEnd, protocolVersion, recvMessage)
import Hat.Term.HostProtocol (defaultCursorModel)

-- A ready Client at @sz@, its last-sent frame set to @lastFrame@ and its
-- full-redraw flag cleared — the post-race state bug 97 leaves behind.
mkClient :: Size -> Frame -> IO (Client, ReadEnd)
mkClient sz lastFrame = do
    (a, b) <- socketPair AF_UNIX Stream 0
    outboundQ <- newOutbound
    sizeV   <- newTVarIO sz
    activeV <- newTVarIO 0
    sessV   <- newTVarIO (SessionId 0)
    lastV   <- newTVarIO []
    readyV  <- newTVarIO True
    keyV    <- newIORef initialKeyState
    escV    <- newIORef NoEscPending
    frameV  <- newIORef lastFrame
    originsV <- newIORef V.empty
    chromeV <- newIORef Nothing
    statusV <- newIORef Nothing
    expansionsV <- newIORef Map.empty
    stylesV <- newIORef Map.empty
    mouseModeVar <- newIORef 0
    curV    <- newIORef (Pos 0 0, True)
    colourV <- newIORef Nothing
    shapeVar <- newIORef defaultCursorModel
    fullV   <- newTVarIO False
    toastV  <- newTVarIO Nothing
    flashV  <- newTVarIO Nothing
    promptV <- newTVarIO Nothing
    popupV  <- newTVarIO Nothing
    focusV  <- newTVarIO True
    envImpV <- newTVarIO ImportEnv
    viewportV <- newTVarIO unpannedViewport
    viewOriginV <- newIORef (Pos 0 0)
    termV <- newTVarIO Nothing
    let client = Client
            { id = ClientId 0, role = Attached, autostart = Joined, sock = a
            , wireLevel = protocolVersion, outbound = outboundQ, size = sizeV
            , lastActive = activeV, session = sessV, sessionHist = lastV
            , ready = readyV, keyState = keyV, escState = escV
            , lastFrame = frameV, lastOrigins = originsV
            , lastChrome = chromeV
            , lastStatus = statusV
            , lastExpansions = expansionsV
            , lastStyles = stylesV
            , lastMouseMode = mouseModeVar
            , lastCursor = curV, lastCursorColour = colourV, lastCursorShape = shapeVar
            , needsFull = fullV, toast = toastV, flash = flashV, prompt = promptV
            , popup = popupV
            , outerFocused = focusV, envImport = envImpV
            , env = [], cwd = "", term = "", ttyName = Nothing, ttyTerm = termV, createdAt = 0
            , stdinSource = NoStdin, control = Nothing
            , viewport = viewportV, lastViewOrigin = viewOriginV
             }
    re <- newReadEnd b
    pure (client, re)

-- A frame every cell of which is a non-blank glyph — stand-in for the busy
-- pane content that was on screen at the old (wider) size.
filledFrame :: Size -> Frame
filledFrame sz = V.replicate (fromIntegral sz.rows)
    (V.replicate (fromIntegral sz.cols)
        (glyphCell 'X' defaultStyle))

-- What a single-width terminal shows after executing the ops.
applyOps :: Frame -> [DrawOp] -> Frame
applyOps = foldl apply
  where
    apply frame = \case
        ClearAll -> V.map (V.map (const blankCell)) frame
        CursorAt _ _ -> frame
        Put pos st txt -> case frame V.!? pos.row of
            Nothing -> frame
            Just row ->
                let updates =
                        [ (c, glyphCell ch st)
                        | (i, ch) <- zip [0 ..] (T.unpack txt)
                        , let c = pos.col + i, c < V.length row ]
                in frame V.// [(pos.row, row V.// updates)]

-- The next frame's ops; none within the deadline means no frame was sent.
drawOps :: ReadEnd -> IO [DrawOp]
drawOps peer = fromMaybe [] <$> timeout 1_000_000 go
  where
    go = recvMessage peer >>= \case
        Just (Known (Draw ops)) -> pure ops
        Just _ -> go
        Nothing -> pure []

-- Each row's text, as a terminal shows the frame.
frameText :: Frame -> [T.Text]
frameText = map (T.pack . map baseChar . V.toList) . V.toList

-- Render to the client once and show the result on a screen that held
-- @onScreen@.
paint :: ServerState -> Client -> ReadEnd -> Frame -> IO Frame
paint st client peer onScreen = do
    withClientWriter client (renderOnce st client)
    applyOps onScreen <$> drawOps peer

-- A client of the test server's one session, viewing it at @sz@ with
-- nothing yet on its screen.
viewer :: ServerState -> Size -> IO (Client, ReadEnd)
viewer st sz = do
    [sid] <- Map.keys <$> readTVarIO st.sessions
    (client, peer) <- mkClient sz V.empty
    atomically (writeTVar client.session sid)
    pure (client, peer)

-- The test server's one pane.
onlyPane :: ServerState -> IO Pane
onlyPane st = realPanes st >>= \case
    [pane] -> pure pane
    panes -> fail ("expected one pane, found " <> show (length panes))

screenHas :: ByteString -> Emu.Emulator -> IO Bool
screenHas needle emu = any (needle `B.isInfixOf`) . snd <$> Emu.screenPainted emu

-- A session whose one window, sized to a 7-row client, runs @cmd@.
newSessionRunning :: ServerState -> T.Text -> IO ()
newSessionRunning st cmd =
    runArgv st Nothing ["new-session", "-d", "-s", "S", "-x", "40", "-y", "6", cmd]
        `shouldReturn` []

spec :: Spec
spec = do
    describe "renderOnce after a shrink" $
        it "fully repaints so no stale wide content survives" $ do
        let wide = Size { rows = 6, cols = 40 }
            narrow = Size { rows = 6, cols = 12 }
        lg <- newLogger "/dev/null"
        st <- newServerState Map.empty lg "/tmp/hat-resizerenderspec.sock" Nothing
        -- On screen and in the server's model: the busy pane painted at the
        -- old wide size. The client is now narrow with no pending full flag.
        (client, peer) <- mkClient narrow (filledFrame wide)
        withClientWriter client (renderOnce st client)
        ops <- drawOps peer
        -- The narrow client shows no session, so its frame is all blank.
        -- Executing the ops against the wide on-screen frame must erase
        -- every cell — a diff would leave columns 12..39 filled.
        let onScreen = applyOps (filledFrame wide) ops
            allBlank = all (V.all (== blankCell)) (V.toList onScreen)
        allBlank `shouldBe` True
        close peer.sock
        close client.sock

    describe "renderOnce over a live pane" $ do
        it "paints one pane to two clients, each at its own size" $
            withServer $ \st -> do
            newSessionRunning st "printf SHARED-9; exec sleep 600"
            pane <- onlyPane st
            awaitTrue "the pane's output" (screenHas "SHARED-9" pane.emulator)
            let small = Size { rows = 7, cols = 40 }
                large = Size { rows = 12, cols = 60 }
            (c1, p1) <- viewer st small
            (c2, p2) <- viewer st large
            f1 <- frameText <$> paint st c1 p1 (blankFrame small)
            f2 <- frameText <$> paint st c2 p2 (blankFrame large)
            -- The pane's row tops both screens; the status line sits on each
            -- client's own bottom row.
            let edges f = (T.concat (take 1 f), T.concat (drop (length f - 1) f))
                (top1, bottom1) = edges f1
                (top2, bottom2) = edges f2
            top1 `shouldSatisfy` T.isInfixOf "SHARED-9"
            top2 `shouldSatisfy` T.isInfixOf "SHARED-9"
            bottom1 `shouldSatisfy` T.isInfixOf "[S]"
            bottom2 `shouldSatisfy` T.isInfixOf "[S]"
            (length f1, length f2) `shouldBe` (7, 12)
            mapM_ close [p1.sock, c1.sock, p2.sock, c2.sock]

        it "carries later output to both clients through each one's own diff" $
            withServer $ \st -> do
            newSessionRunning st "cat"
            pane <- onlyPane st
            let sz = Size { rows = 7, cols = 40 }
            (c1, p1) <- viewer st sz
            (c2, p2) <- viewer st sz
            Pty.writePty pane.pty "first-7\n"  -- echoed back as pane output
            awaitTrue "the first line" (screenHas "first-7" pane.emulator)
            s1 <- paint st c1 p1 (blankFrame sz)
            s2 <- paint st c2 p2 (blankFrame sz)
            Pty.writePty pane.pty "second-8\n"
            awaitTrue "the second line" (screenHas "second-8" pane.emulator)
            s1' <- frameText <$> paint st c1 p1 s1
            s2' <- frameText <$> paint st c2 p2 s2
            s1' `shouldSatisfy` any (T.isInfixOf "second-8")
            s2' `shouldSatisfy` any (T.isInfixOf "second-8")
            mapM_ close [p1.sock, c1.sock, p2.sock, c2.sock]

        -- bug 6: a program leaving the alternate screen must take the client
        -- back to the primary screen, not only the emulator.
        it "repaints the primary screen when a program leaves the alternate screen (bug 6)" $
            withServer $ \st -> do
            -- `read` parks the program on the alternate screen until a line
            -- arrives; the markers are printf output, never an echoed command.
            newSessionRunning st
                "printf PRIMARY-7; printf '\\033[?1049hALT-42'; read x; \
                \printf '\\033[?1049l'; exec sleep 600"
            pane <- onlyPane st
            awaitTrue "the alternate screen" (screenHas "ALT-42" pane.emulator)
            let sz = Size { rows = 7, cols = 40 }
            (client, peer) <- viewer st sz
            onAlt <- paint st client peer (blankFrame sz)
            frameText onAlt `shouldSatisfy` any (T.isInfixOf "ALT-42")
            Pty.writePty pane.pty "\n"
            awaitTrue "the primary screen back" (screenHas "PRIMARY-7" pane.emulator)
            back <- frameText <$> paint st client peer onAlt
            back `shouldSatisfy` any (T.isInfixOf "PRIMARY-7")
            back `shouldNotSatisfy` any (T.isInfixOf "ALT-42")
            close peer.sock
            close client.sock
