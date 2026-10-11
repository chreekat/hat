module Hat.Server.PaneReaderSpec (spec) where

import Control.Concurrent (threadDelay, threadWaitRead)
import Control.Concurrent.Async (async)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.Maybe (isJust, isNothing)
import GHC.Conc (BlockReason (BlockedOnSTM), ThreadStatus (ThreadBlocked), threadStatus)
import System.Posix.IO.ByteString (fdRead)
import System.Posix.Types (Fd)
import Data.Map.Strict qualified as Map
import Foreign.Ptr (castPtr)
import GHC.IO.Buffer (newByteBuffer)
import GHC.IO.BufferedIO (BufferedIO (..), writeBuf, writeBufNonBlocking)
import GHC.IO.Device (IODevice (..), IODeviceType (Stream), RawIO (..))
import GHC.IO.Handle (mkFileHandle, noNewlineTranslation)
import System.IO (Handle, IOMode (WriteMode))
import System.Timeout (timeout)
import Test.Hspec

import Hat.Geometry
import Hat.Log (newLogger)
import Hat.Model
import Hat.Server.Pane (hangupPane, newWindowWithPane, pauseReaders, startPaneReader)
import Hat.Term.Emulator qualified as Emu
import Hat.Term.Pty qualified as Pty

-- | A pipe-pane sink whose writes report their bytes to 'seen', then block
-- until 'hold' is filled.
data StallSink = StallSink
    { seen :: MVar B.ByteString
    , hold :: MVar ()
    }

instance RawIO StallSink where
    read _ _ _ _ = pure 0
    readNonBlocking _ _ _ _ = pure Nothing
    write s p _ n = do
        bs <- B.packCStringLen (castPtr p, n)
        _ <- tryPutMVar s.seen bs
        readMVar s.hold
    writeNonBlocking s p off n = n <$ write s p off n

instance IODevice StallSink where
    ready _ _ _ = pure True
    close _ = pure ()
    devType _ = pure Stream

instance BufferedIO StallSink where
    newBuffer _ = newByteBuffer 8192
    fillReadBuffer _ b = pure (0, b)
    fillReadBuffer0 _ b = pure (Nothing, b)
    flushWriteBuffer = writeBuf
    flushWriteBuffer0 = writeBufNonBlocking

stallingSink :: IO (Handle, StallSink)
stallingSink = do
    s <- StallSink <$> newEmptyMVar <*> newEmptyMVar
    h <- mkFileHandle s "<stalling pipe-pane>" WriteMode Nothing noNewlineTranslation
    pure (h, s)

screenHas :: B.ByteString -> Emu.Emulator -> IO Bool
screenHas needle emu = any (needle `B.isInfixOf`) . snd <$> Emu.screenPainted emu

-- | Run @body@ on a live pane whose reader has consumed its first output
-- chunk (passed in) and is stalled forwarding it to a 'StallSink' pipe-pane;
-- the pane is hung up however @body@ ends.
withStalledReader
    :: (ServerState -> Pane -> StallSink -> B.ByteString -> IO a) -> IO a
withStalledReader body = do
    lg <- newLogger "/dev/null"
    st <- newServerState Map.empty lg "/tmp/hat-panereaderspec.sock" Nothing
    let sid = SessionId 0
    (sink, stall) <- stallingSink
    bracket
        (newWindowWithPane st sid "/bin/sh"
            (Just "printf b89-fed; exec sleep 600") "/" []
            Size { rows = 24, cols = 80 })
        (\(_, pane) -> tryPutMVar stall.hold () >> hangupPane pane)
        (\(win, pane) -> do
            sup <- async (pure ())
            atomically $ writeTVar pane.pipe
                (Just PipeHandle { super = sup, toStdin = Just sink })
            startPaneReader st sid win pane
            mchunk <- timeout 10_000_000 (takeMVar stall.seen)
            chunk <- maybe (expectationFailure "no output reached the pipe"
                                >> pure B.empty) pure mchunk
            body st pane stall chunk)

-- | Poll until @act@ holds, failing the test past a 10s deadline.
awaitTrue :: String -> IO Bool -> IO ()
awaitTrue what act = do
    r <- timeout 10_000_000 loop
    when (isNothing r) $ expectationFailure ("timed out awaiting " <> what)
  where
    loop = do
        ok <- act
        unless ok (threadDelay 1000 >> loop)

-- | Read the pty master directly, as the next image would, until @needle@
-- has arrived; False past a 10s deadline.
rawReadsUntil :: Fd -> B.ByteString -> IO Bool
rawReadsUntil fd needle = isJust <$> timeout 10_000_000 (go B.empty)
  where
    go acc
        | needle `B.isInfixOf` acc = pure ()
        | otherwise = threadWaitRead fd >> fdRead fd 4096 >>= go . (acc <>)

spec :: Spec
spec = describe "pane reader" $ do
    -- b89: the capture after a pause must hold every byte a reader consumed.
    it "pauseReaders returns only once every consumed chunk is fed (b89)" $
        withStalledReader $ \st pane _ chunk -> do
        pauseReaders st
        fed <- screenHas chunk pane.emulator
        unless fed $ expectationFailure
            ("consumed but never fed: " <> show chunk)

    -- b89: what a paused reader has not fed must survive the exec.
    it "a paused reader leaves later output unconsumed (b89)" $
        withStalledReader $ \st pane stall _ -> do
        pauseReaders st
        let fd = Pty.masterFd pane.pty
            kept = B8.pack "b89-kept"
        Pty.writePty pane.pty kept  -- echoed back as pane output
        _ <- timeout 10_000_000 (threadWaitRead fd)
        putMVar stall.hold ()
        tid <- readTVarIO pane.readerTid
        awaitTrue "the reader to park" $
            maybe (pure True) (fmap (== ThreadBlocked BlockedOnSTM) . threadStatus) tid
        rawReadsUntil fd kept `shouldReturn` True
