-- | A WebSocket client connection over a connected socket: what the Rex
-- client needs of the @websockets@ library, with its exceptions turned into
-- values.
module Hat.Rex.Conn
    ( WsConn
    , openWs
    , refusalText
    , sendText
    , sendBinary
    , Received (..)
    , Close (..)
    , recvMessage
    , closeWs
    ) where

import Control.Exception (IOException, handle, try)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word16)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy qualified as NL
import Network.WebSockets qualified as WS
import Network.WebSockets.Stream qualified as WS.Stream

newtype WsConn = WsConn WS.Connection

-- | Why a connection was closed, as the peer's close frame says.
data Close = Close
    { code :: Word16
    , reason :: Text
    }
    deriving (Eq, Show)

-- | What 'recvMessage' has to say next.
data Received
    = GotText ByteString
    | GotBinary ByteString
    | PeerClosed Close
    | ConnectionLost       -- ^ the transport ended with no close frame
    | FramingBroken Text   -- ^ the peer sent what is not WebSocket; how, for the user
    deriving (Eq, Show)

-- | Upgrade a connected socket to a WebSocket at a path, speaking one
-- subprotocol; 'Left' says why the server would not. Closing the socket
-- stays with the caller.
openWs :: Socket -> String -> ByteString -> IO (Either Text WsConn)
openWs sock path subprotocol = do
    -- The response as it arrives, kept until the upgrade is settled: the
    -- library reports a refusal's status but not its body.
    response <- newIORef (Just B.empty)
    let receive = do
            chunk <- recv sock 65536
            if B.null chunk then pure Nothing else do
                modifyIORef' response (fmap (<> chunk))
                pure (Just chunk)
    stream <- WS.Stream.makeStream receive (maybe (pure ()) (NL.sendAll sock))
    r <- handle (\(_ :: IOException) -> pure (Right Nothing)) $
        handle (\(_ :: WS.ConnectionException) -> pure (Right Nothing)) $
        try $ Just <$> WS.newClientConnection stream "localhost" path
            WS.defaultConnectionOptions [("Sec-WebSocket-Protocol", subprotocol)]
    case r of
        Right (Just conn) -> Right (WsConn conn) <$ writeIORef response Nothing
        Right Nothing -> pure (Left "the connection was lost during the WebSocket handshake")
        Left (WS.MalformedResponse hd _) | WS.responseCode hd /= 101 -> do
            seen <- maybe B.empty id <$> readIORef response
            let body0 = B.drop 4 (snd (B.breakSubstring "\r\n\r\n" seen))
                wanted = maybe 0 fst (lookup "Content-Length" (WS.responseHeaders hd) >>= B8.readInt)
                rest body
                    | B.length body >= wanted = pure body
                    | otherwise = do
                        chunk <- recv sock 65536
                        if B.null chunk then pure body else rest (body <> chunk)
            body <- handle (\(_ :: IOException) -> pure body0) (rest body0)
            pure (Left (refusalText (WS.responseCode hd) body))
        Left e -> pure (Left ("WebSocket handshake failed: " <> T.pack (show e)))

-- | How to word a refused upgrade from its HTTP status and body: the
-- @error@ of a JSON body when there is one, else the body as it is.
refusalText :: Int -> ByteString -> Text
refusalText status body = "HTTP " <> T.pack (show status) <> case errorOf of
    Just e -> ": " <> e
    Nothing | T.null plain -> ""
            | otherwise -> ": " <> plain
  where
    plain = T.strip (TE.decodeUtf8Lenient body)
    errorOf = Aeson.decodeStrict body
        >>= Aeson.parseMaybe (Aeson.withObject "refusal" (Aeson..: "error"))

sendText :: WsConn -> ByteString -> IO ()
sendText (WsConn c) = WS.sendTextData c

sendBinary :: WsConn -> ByteString -> IO ()
sendBinary (WsConn c) = WS.sendBinaryData c

-- | The next data message, or how the connection ended. Pings are answered
-- on the way.
recvMessage :: WsConn -> IO Received
recvMessage (WsConn c) = handle lost (handle ended (got <$> WS.receiveDataMessage c))
  where
    got = \case
        WS.Text bs _ -> GotText (BL.toStrict bs)
        WS.Binary bs -> GotBinary (BL.toStrict bs)
    ended = pure . \case
        WS.CloseRequest code why ->
            PeerClosed Close { code, reason = TE.decodeUtf8Lenient (BL.toStrict why) }
        WS.ConnectionClosed -> ConnectionLost
        WS.ParseException e -> FramingBroken ("WebSocket framing error: " <> T.pack e)
        WS.UnicodeException e -> FramingBroken ("WebSocket text is not UTF-8: " <> T.pack e)
    lost (_ :: IOException) = pure ConnectionLost

-- | Say goodbye with a status code. A peer already gone is no error.
closeWs :: WsConn -> Word16 -> IO ()
closeWs (WsConn c) code =
    handle (\(_ :: IOException) -> pure ()) $
    handle (\(_ :: WS.ConnectionException) -> pure ()) $
        WS.sendCloseCode c code B.empty
