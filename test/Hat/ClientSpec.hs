module Hat.ClientSpec (spec) where

import Control.Concurrent.Async (waitCatchSTM, withAsync)
import Control.Concurrent.STM
import Data.IORef (newIORef)
import Network.Socket (Family (AF_UNIX), SocketType (Stream), close, socketPair)
import Test.Hspec

import Hat.Client (ExitReason (..), declinedLock, forwardToServer, receiveServer)
import Hat.Client.Draw (legacyDialect)
import Hat.Geometry (Size (..))
import Hat.Transport.Wire

spec :: Spec
spec = do
    describe "forwardToServer" $
        -- Regression guard: a send to a server already gone must not end the
        -- client before the server's last word is read.
        it "keeps taking the client's messages once the server's end is gone" $ do
            (ours, theirs) <- socketPair AF_UNIX Stream 0
            close theirs
            outbox <- newTQueueIO
            atomically $ mapM_ (writeTQueue outbox) [Detach, Detach]
            withAsync (forwardToServer ours outbox) $ \fwd -> do
                ended <- atomically $
                    (Just <$> waitCatchSTM fwd)
                        `orElse` (Nothing <$ (isEmptyTQueue outbox >>= check))
                fmap (either show (const "returned")) ended `shouldBe` Nothing

    describe "receiveServer" $
        it "hands a terminal an older server locks straight back, saying why" $ do
            (ours, theirs) <- socketPair AF_UNIX Stream 0
            mapM_ (sendMessage theirs) [LockClient "vlock", Exited]
            re <- newReadEnd ours
            outbox <- newTQueueIO
            dialect <- newIORef legacyDialect
            size <- newIORef Size { rows = 24, cols = 80 }
            receiveServer (\_ -> pure ()) dialect size outbox re
                `shouldReturn` SessionEnded
            atomically (flushTQueue outbox) `shouldReturn` declinedLock
            declinedLock `shouldSatisfy` \case
                LockDone : _ -> True
                _ -> False
