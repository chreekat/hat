module Hat.ClientSpec (spec) where

import Control.Concurrent.Async (waitCatchSTM, withAsync)
import Control.Concurrent.STM
import Network.Socket (Family (AF_UNIX), SocketType (Stream), close, socketPair)
import Test.Hspec

import Hat.Client (forwardToServer)
import Hat.Transport.Wire (ClientToServer (..))

spec :: Spec
spec = describe "forwardToServer" $
    -- Regression guard: a send to a server already gone must not end the
    -- client before the server's last word is read.
    it "keeps taking the client's messages once the server's end is gone" $ do
        (ours, theirs) <- socketPair AF_UNIX Stream 0
        close theirs
        outbox <- newTQueueIO
        atomically $ mapM_ (writeTQueue outbox) [LockDone, LockDone]
        withAsync (forwardToServer ours outbox) $ \fwd -> do
            ended <- atomically $
                (Just <$> waitCatchSTM fwd)
                    `orElse` (Nothing <$ (isEmptyTQueue outbox >>= check))
            fmap (either show (const "returned")) ended `shouldBe` Nothing
