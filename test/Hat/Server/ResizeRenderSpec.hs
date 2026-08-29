-- | A client whose frame shrank must be fully repainted, never diffed
-- (regression guard, bug 97).
module Hat.Server.ResizeRenderSpec (spec) where

import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM (newTVarIO)
import Data.IORef (newIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Network.Socket
    (Family (AF_UNIX), SocketType (Stream), Socket, close, socketPair)
import System.Timeout (timeout)
import Test.Hspec

import Hat.Geometry (Pos (..), Size (..))
import Hat.Log (newLogger)
import Hat.Model hiding (Toast (..))
import Hat.Server.Keys (EscPending (NoEscPending), PrefixState (NoPrefix))
import Hat.Server.Render (Frame)
import Hat.Server.View (renderOnce)
import Hat.Term.Cell
import Hat.Transport.Wire
    (Autostart (..), DrawOp (..), Inbound (..), ServerToClient (..)
    , protocolVersion, recvMessage)

-- A ready Client at @sz@, its last-sent frame set to @lastFrame@ and its
-- full-redraw flag cleared — the post-race state bug 97 leaves behind.
mkClient :: Size -> Frame -> IO (Client, Socket)
mkClient sz lastFrame = do
    (a, b) <- socketPair AF_UNIX Stream 0
    lock    <- newMVar ()
    sizeV   <- newTVarIO sz
    activeV <- newTVarIO 0
    sessV   <- newTVarIO (SessionId 0)
    lastV   <- newTVarIO []
    readyV  <- newTVarIO True
    keyV    <- newIORef NoPrefix
    escV    <- newIORef NoEscPending
    frameV  <- newIORef lastFrame
    curV    <- newIORef (Pos 0 0, True)
    colourV <- newIORef ""
    fullV   <- newTVarIO False
    toastV  <- newTVarIO Nothing
    promptV <- newTVarIO Nothing
    pickV   <- newTVarIO Nothing
    focusV  <- newTVarIO True
    envImpV <- newTVarIO ImportEnv
    let client = Client
            { id = ClientId 0, role = Attached, autostart = Joined, sock = a
            , wireLevel = protocolVersion, sendLock = lock, size = sizeV
            , lastActive = activeV, session = sessV, sessionHist = lastV
            , ready = readyV, keyState = keyV, escState = escV
            , lastFrame = frameV, lastCursor = curV, lastCursorColour = colourV
            , needsFull = fullV, toast = toastV, prompt = promptV
            , picker = pickV, outerFocused = focusV, envImport = envImpV
            , env = [], cwd = "" }
    pure (client, b)

-- A frame every cell of which is a non-blank glyph — stand-in for the busy
-- pane content that was on screen at the old (wider) size.
filledFrame :: Size -> Frame
filledFrame sz = V.replicate (fromIntegral sz.rows)
    (V.replicate (fromIntegral sz.cols)
        blankCell { char = 'X' })

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
                        [ (c, blankCell { char = ch, style = st })
                        | (i, ch) <- zip [0 ..] (T.unpack txt)
                        , let c = pos.col + i, c < V.length row ]
                in frame V.// [(pos.row, row V.// updates)]

drawOps :: Socket -> IO [DrawOp]
drawOps peer = do
    r <- timeout 250_000 (recvMessage peer)
    pure $ case r of
        Just (Just (Known (Draw ops))) -> ops
        _ -> []

spec :: Spec
spec = describe "renderOnce after a shrink" $
    it "fully repaints so no stale wide content survives" $ do
        let wide = Size { rows = 6, cols = 40 }
            narrow = Size { rows = 6, cols = 12 }
        lg <- newLogger "/dev/null"
        st <- newServerState Map.empty lg "/tmp/hat-resizerenderspec.sock" Nothing
        -- On screen and in the server's model: the busy pane painted at the
        -- old wide size. The client is now narrow with no pending full flag.
        (client, peer) <- mkClient narrow (filledFrame wide)
        renderOnce st client
        ops <- drawOps peer
        -- The narrow client shows no session, so its frame is all blank.
        -- Executing the ops against the wide on-screen frame must erase
        -- every cell — a diff would leave columns 12..39 filled.
        let onScreen = applyOps (filledFrame wide) ops
            allBlank = all (V.all (== blankCell)) (V.toList onScreen)
        allBlank `shouldBe` True
        close peer
        close client.sock
