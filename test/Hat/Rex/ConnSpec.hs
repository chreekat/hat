module Hat.Rex.ConnSpec (spec) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Network.Socket qualified as N
import Network.WebSockets qualified as WS
import Test.Hspec

import Hat.Rex.Conn

-- | Open a connection to a server that accepts the upgrade, does what it is
-- given to, and hangs up; then, with the server gone, run the client's part.
afterServer :: (WS.Connection -> IO ()) -> (WsConn -> IO a) -> IO a
afterServer server client =
    bracket (N.socketPair N.AF_UNIX N.Stream 0) (\(a, b) -> N.close a >> N.close b) $ \(ours, theirs) -> do
        let serve = do
                request <- WS.makePendingConnection theirs WS.defaultConnectionOptions
                conn <- WS.acceptRequestWith request (WS.AcceptRequest (Just proto) [])
                server conn
                N.close theirs
        withAsync serve $ \served -> do
            Right conn <- openWs ours "/api/data" proto
            wait served
            client conn
  where
    proto = "rex.data.terminal.v1" :: ByteString

spec :: Spec
spec = do
    describe "a refused WebSocket upgrade" $ do
        it "is worded from the JSON body's error" $
            refusalText 403 "{\"error\": \"user not allowed\"}\n"
                `shouldBe` "HTTP 403: user not allowed"

        it "quotes a body that is not that JSON" $
            refusalText 404 "404 page not found\n" `shouldBe` "HTTP 404: 404 page not found"

        it "is the bare status when there is no body" $
            refusalText 502 "" `shouldBe` "HTTP 502"

    describe "a connection's end" $ do
        it "keeps the peer's close code when the peer has hung up before the answer" $
            afterServer (\c -> WS.sendBinaryData c ("tail" :: ByteString)
                            >> WS.sendCloseCode c 4000 ("block closed" :: ByteString)) $ \conn -> do
                recvMessage conn `shouldReturn` GotBinary "tail"
                recvMessage conn
                    `shouldReturn` PeerClosed Close { code = 4000, reason = "block closed" }

        it "shows a send to a vanished peer as a lost connection on the next receive" $
            afterServer (\_ -> pure ()) $ \conn -> do
                sendText conn "{\"type\":\"hello\"}"
                sendBinary conn "\x01"
                recvMessage conn `shouldReturn` ConnectionLost
