{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# OPTIONS_GHC -Wno-orphans #-}  -- Serialise instances for geometry/cell types live here: the wire encoding is this module's concern

-- | The wire protocol: message types, CBOR encoding, and framing.
--
-- Every frame on the socket is a 4-byte big-endian length followed by
-- that many bytes of CBOR. The first client message must be 'Hello';
-- the server answers 'Welcome' (or 'ServerError' and closes). During
-- the fleshing-out phase the protocol version is bumped freely and a
-- mismatch is fatal.
module Hat.Transport.Wire
    ( protocolVersion
    , Intent (..)
    , Hello (..)
    , ClientToServer (..)
    , ServerToClient (..)
    , DrawOp (..)
    , encodeMessage
    , decodeMessage
    , sendMessage
    , recvMessage
    ) where

import Codec.Serialise
    (DeserialiseFailure, Serialise, deserialiseOrFail, serialise)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as SB
import qualified Network.Socket.ByteString.Lazy as SBL

import Hat.Geometry
import Hat.Term.Cell

protocolVersion :: Word16
protocolVersion = 3

deriving instance Generic Size
deriving anyclass instance Serialise Size
deriving instance Generic Pos
deriving anyclass instance Serialise Pos
deriving instance Generic Color
deriving anyclass instance Serialise Color
deriving instance Generic Style
deriving anyclass instance Serialise Style

-- | Why this client connected: to attach and render, or only to issue
-- commands (e.g. @hat kill-server@ from a shell).
--
-- 'AttachIntent' carries setup commands the server runs to establish the
-- client's session before rendering — e.g. @new-session@ or
-- @attach-session -t@ typed at a shell. An empty list attaches to the
-- existing (or a freshly created) session, the plain @hat@/@hat attach@
-- behaviour.
data Intent = AttachIntent [[Text]] | ControlIntent
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialise)

-- | The client's opening handshake. A standalone single-constructor
-- record so its field selectors are total.
data Hello = Hello
    { protoVersion :: Word16
    , term         :: Text
    , env          :: [(Text, Text)]
    , size         :: Size
    , cwd          :: Text
    , intent       :: Intent
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialise)

data ClientToServer
    = ClientHello Hello
    | Input ByteString      -- ^ raw bytes from the client's tty
    | Resize Size
    | Command [[Text]]      -- ^ pre-tokenised commands, one inner list per @;@-separated command
    | Detach
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialise)

data ServerToClient
    = Welcome Text          -- ^ carries the attached session's name
    | Draw [DrawOp]
    | SetTitle Text
    | RingBell
    | Message Text          -- ^ toast (display-message)
    | DetachOk
    | CommandDone           -- ^ all replies for one Command were sent
    | ServerError Text
    | Exited                -- ^ session is gone; client should quit
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialise)

data DrawOp
    = Put Pos Style Text    -- ^ draw a styled text run starting at pos
    | ClearAll
    | CursorAt Pos Bool     -- ^ final cursor position and visibility
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialise)

encodeMessage :: Serialise a => a -> ByteString
encodeMessage = BL.toStrict . serialise

decodeMessage :: Serialise a => ByteString -> Either String a
decodeMessage bs = case deserialiseOrFail (BL.fromStrict bs) of
    Left (err :: DeserialiseFailure) -> Left (show err)
    Right v -> Right v

maxFrame :: Word32
maxFrame = 64 * 1024 * 1024

sendMessage :: Serialise a => Socket -> a -> IO ()
sendMessage sock msg = do
    let payload = encodeMessage msg
        n = fromIntegral (B.length payload) :: Word32
        header = B.pack
            [ fromIntegral (n `shiftR` 24 .&. 0xff)
            , fromIntegral (n `shiftR` 16 .&. 0xff)
            , fromIntegral (n `shiftR` 8 .&. 0xff)
            , fromIntegral (n .&. 0xff)
            ]
    SBL.sendAll sock (BL.fromChunks [header, payload])

-- | Nothing means the peer closed the connection.
recvMessage :: Serialise a => Socket -> IO (Maybe (Either String a))
recvMessage sock = do
    mheader <- recvExactly sock 4
    case mheader of
        Nothing -> pure Nothing
        Just header -> case fromIntegral <$> B.unpack header :: [Word32] of
            [a, b, c, d] -> do
                let n = a `shiftL` 24 .|. b `shiftL` 16 .|. c `shiftL` 8 .|. d
                if n > maxFrame
                    then pure (Just (Left "frame too large"))
                    else do
                        mbody <- recvExactly sock (fromIntegral n)
                        pure $ decodeMessage <$> mbody
            _ -> pure (Just (Left "short frame header"))

recvExactly :: Socket -> Int -> IO (Maybe ByteString)
recvExactly sock n = go n []
  where
    go 0 acc = pure (Just (B.concat (Prelude.reverse acc)))
    go remaining acc = do
        chunk <- SB.recv sock remaining
        if B.null chunk
            then pure Nothing
            else go (remaining - B.length chunk) (chunk : acc)
