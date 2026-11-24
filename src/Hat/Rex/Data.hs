-- | The handshake that opens a Rex data connection: fixed-layout binary
-- messages, integers little-endian.
module Hat.Rex.Data
    ( Ticket
    , ticketFromText
    , encodeHello
    , Nonce (..)
    , HandshakeReply (..)
    , RejectReason (..)
    , decodeHandshakeReply
    , rejectText
    ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64, Word8)

import Hat.Base64 (base64Decode)

-- | A single-use ticket from @data.connect@: 16 bytes.
newtype Ticket = Ticket ByteString
    deriving (Eq)

-- | Never shows the secret.
instance Show Ticket where
    show _ = "<ticket>"

-- | A ticket from its 22 unpadded base64url characters.
ticketFromText :: Text -> Either Text Ticket
ticketFromText t = case base64Decode (B.map standard (TE.encodeUtf8 t)) of
    Just bs | B.length bs == 16 -> Right (Ticket bs)
    _ -> Left "the server's data ticket is not 16 bytes of base64url"
  where
    standard = \case
        0x2d -> 0x2b  -- - for +
        0x5f -> 0x2f  -- _ for /
        w -> w

-- | HELLO: the first message of a data connection.
encodeHello :: Ticket -> ByteString
encodeHello (Ticket bs) = B.cons 0x01 bs

-- | The per-channel secret marking Rex's own messages in a terminal stream.
newtype Nonce = Nonce Word64
    deriving (Eq, Show)

data HandshakeReply
    = Welcomed Nonce
    | Rejected RejectReason
    deriving (Eq, Show)

data RejectReason
    = TicketInvalid
    | AlreadyConnected
    | StreamUnavailable
    | LimitReached
    | MalformedHandshake
    | ServerFailed
    | WrongSubprotocol
    | UnknownReason Word8
    deriving (Eq, Show)

-- | Read the server's answer to a HELLO: a 32-byte WELCOME or a REJECT.
decodeHandshakeReply :: ByteString -> Either Text HandshakeReply
decodeHandshakeReply bs = case B.unpack (B.take 2 bs) of
    0x81 : _
        | B.length bs /= 32 -> Left "the data WELCOME is not 32 bytes"
        | nonce == 0 -> Left "the data WELCOME has a zero channel nonce"
        | otherwise -> Right (Welcomed (Nonce nonce))
    [0x82, r] -> Right . Rejected $ case r of
        0x01 -> TicketInvalid
        0x02 -> AlreadyConnected
        0x03 -> StreamUnavailable
        0x04 -> LimitReached
        0x05 -> MalformedHandshake
        0x06 -> ServerFailed
        0x07 -> WrongSubprotocol
        _ -> UnknownReason r
    _ -> Left "the data handshake reply is neither a WELCOME nor a REJECT"
  where
    nonce = B.foldr (\b acc -> acc `shiftL` 8 .|. fromIntegral b) 0 (B.drop 24 bs)

-- | Why the server refused a data connection, for the user.
rejectText :: RejectReason -> Text
rejectText = \case
    TicketInvalid -> "the data ticket was unknown, expired or already used"
    AlreadyConnected -> "this client already has a connection to that pane's stream"
    StreamUnavailable -> "the pane's stream is not available"
    LimitReached -> "the server is at a connection limit"
    MalformedHandshake -> "the server found the data handshake malformed or late"
    ServerFailed -> "the server failed to open the stream"
    WrongSubprotocol -> "the data ticket was issued for another subprotocol"
    UnknownReason r -> "the server refused the stream (reason " <> T.pack (show r) <> ")"
