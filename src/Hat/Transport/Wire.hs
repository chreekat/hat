{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RoleAnnotations   #-}
{-# OPTIONS_GHC -Wno-orphans #-}  -- Serialise instances for geometry/cell types live here: the wire encoding is this module's concern

-- | The wire protocol: message types, CBOR encoding, and framing.
--
-- Every frame on the socket is a 4-byte big-endian length followed by
-- that many bytes of CBOR. The CBOR payload of a message is a
-- definite-length list @[tag, field1, field2, ...]@ where @tag@ is a
-- 'Word' that names the constructor. The first client message must be a
-- 'ClientHello'; the server answers 'Welcome' (or 'ServerError' and
-- closes).
--
-- == Evolution charter
--
-- 'protocolVersion' is a wire /era/: it is bumped ONLY for an
-- incompatible reframing (ideally never again). A mismatched era is
-- fatal. Within an era the protocol evolves by APPENDING new tags:
--
--   * Tags are append-only forever. Never renumber a tag, never reuse a
--     retired one, never insert one mid-list. The tag registries below
--     are frozen; new constructors get the next unused number.
--   * A tag's payload shape is frozen forever. To change a message,
--     add a NEW tag; do not alter an existing one.
--   * Receivers SKIP unknown tags ('UnknownTag') so a new sender can
--     talk to an old receiver.
--   * A malformed payload ('Malformed') is FATAL to the connection: the
--     bytes are unintelligible, so draining further would loop.
--   * The golden-byte tests in "Hat.Transport.WireSpec" pin every tag
--     number and every payload shape — including the Generic-derived
--     leaf types (Size, Pos, Color, Style, DrawOp, Intent). Changing any
--     leaf encoding therefore breaks the wire contract: add a new
--     message tag instead, or bump the era knowing all peers must
--     restart together.
--
-- 'Hello' is encoded as a definite list of its fields and its decoder
-- TOLERATES extra trailing elements, so new hello fields can be appended
-- compatibly without a tag or era bump.
module Hat.Transport.Wire
    ( protocolVersion
    , Intent (..)
    , Hello (..)
    , ClientToServer (..)
    , ServerToClient (..)
    , DrawOp (..)
    , Inbound (..)
    , WireMessage (..)
    , encodeMessage
    , decodeMessage
    , sendMessage
    , recvMessage
    ) where

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (decodeTerm)
import Codec.CBOR.Write (toStrictByteString)
import Codec.Serialise (Serialise, decode, encode)
import Codec.Serialise.Decoding
    (Decoder, decodeListLen, decodeWord, decodeWord16)
import Codec.Serialise.Encoding
    (Encoding, encodeListLen, encodeWord, encodeWord16)
import Control.Monad (replicateM_)
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

-- | The wire era. Bumped only for an incompatible reframing of the
-- envelope; a mismatch between peers is fatal. See the evolution charter
-- in the module haddock.
protocolVersion :: Word16
protocolVersion = 5

deriving instance Generic Size
deriving anyclass instance Serialise Size
deriving instance Generic Pos
deriving anyclass instance Serialise Pos
-- 'Color' and 'Style' get their 'Serialise' instances at their home module
-- ('Hat.Term.Cell'); the wire reuses them as leaf field types. 'Style' is
-- hand-written and append-tolerant, but a leaf change still shifts these golden
-- bytes, so it rides a 'protocolVersion' bump (peers must match).

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

data ServerToClient
    = Welcome Text          -- ^ carries the attached session's name
    | Draw [DrawOp]
    | SetTitle Text
    | RingBell
    | Notify ByteString     -- ^ a pane's OSC 9/777 desktop notification, to
                            --   write verbatim to the outer terminal
    | Message Text          -- ^ toast (display-message)
    | DetachOk
    | CommandDone           -- ^ all replies for one Command were sent
    | ServerError Text
    | Exited                -- ^ the client's session is gone
    deriving (Eq, Show, Generic)

data DrawOp
    = Put Pos Style Text    -- ^ draw a styled text run starting at pos
    | ClearAll
    | CursorAt Pos Bool     -- ^ final cursor position and visibility
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialise)

-- | The outcome of decoding a message frame. Receiver policy per case
-- is set by the evolution charter above.
type role Inbound representational
data Inbound a
    = Known a           -- ^ a message this build knows
    | UnknownTag Word   -- ^ a newer peer's message this build doesn't know
    | Malformed String  -- ^ a frame that didn't decode
    deriving (Eq, Show)

-- | Explicit, tag-stable CBOR for the two top-level message types.
--
-- Encoding is per-constructor (never Generic-derived) so constructor
-- order in the Haskell data type is decoupled from the wire tags: the
-- registries below are the source of truth. Leaf/field types reuse their
-- 'Serialise' instances.
class WireMessage a where
    -- | @[tag, field...]@ as a definite-length list.
    encodeWire :: a -> Encoding
    -- | Given the already-read list length and tag, decode the fields.
    -- The list length lets 'Hello'-style decoders tolerate extras.
    decodeWirePayload :: Word -> Int -> Decoder s (Inbound a)

-- ClientToServer tag registry (append-only; never renumber or reuse):
--   ClientHello = 0
--   Input       = 1
--   Resize      = 2
--   Command     = 3
--   Detach      = 4
instance WireMessage ClientToServer where
    encodeWire = \case
        ClientHello h -> encodeListLen 2 <> encodeWord 0 <> encodeHello h
        Input bs      -> encodeListLen 2 <> encodeWord 1 <> encode bs
        Resize sz     -> encodeListLen 2 <> encodeWord 2 <> encode sz
        Command cmds  -> encodeListLen 2 <> encodeWord 3 <> encode cmds
        Detach        -> encodeListLen 1 <> encodeWord 4
    decodeWirePayload tag len = case tag of
        0 -> field len (Known . ClientHello <$> decodeHello)
        1 -> field len (Known . Input <$> decode)
        2 -> field len (Known . Resize <$> decode)
        3 -> field len (Known . Command <$> decode)
        4 -> nullary len (Known Detach)
        _ -> pure (UnknownTag tag)

-- ServerToClient tag registry (append-only; never renumber or reuse):
--   Welcome     = 0
--   Draw        = 1
--   SetTitle    = 2
--   RingBell    = 3
--   Notify      = 4
--   Message     = 5
--   DetachOk    = 6
--   CommandDone = 7
--   ServerError = 8
--   Exited      = 9
instance WireMessage ServerToClient where
    encodeWire = \case
        Welcome n     -> encodeListLen 2 <> encodeWord 0 <> encode n
        Draw ops      -> encodeListLen 2 <> encodeWord 1 <> encode ops
        SetTitle t    -> encodeListLen 2 <> encodeWord 2 <> encode t
        RingBell      -> encodeListLen 1 <> encodeWord 3
        Notify raw    -> encodeListLen 2 <> encodeWord 4 <> encode raw
        Message t     -> encodeListLen 2 <> encodeWord 5 <> encode t
        DetachOk      -> encodeListLen 1 <> encodeWord 6
        CommandDone   -> encodeListLen 1 <> encodeWord 7
        ServerError e -> encodeListLen 2 <> encodeWord 8 <> encode e
        Exited        -> encodeListLen 1 <> encodeWord 9
    decodeWirePayload tag len = case tag of
        0 -> field len (Known . Welcome <$> decode)
        1 -> field len (Known . Draw <$> decode)
        2 -> field len (Known . SetTitle <$> decode)
        3 -> nullary len (Known RingBell)
        4 -> field len (Known . Notify <$> decode)
        5 -> field len (Known . Message <$> decode)
        6 -> nullary len (Known DetachOk)
        7 -> nullary len (Known CommandDone)
        8 -> field len (Known . ServerError <$> decode)
        9 -> nullary len (Known Exited)
        _ -> pure (UnknownTag tag)

-- | A single-field constructor: the payload list must be @[tag, field]@.
field :: Int -> Decoder s (Inbound a) -> Decoder s (Inbound a)
field 2 d = d
field n _ = pure (Malformed ("expected 1 field, got " <> show (n - 1)))

-- | A nullary constructor: the payload list must be @[tag]@.
nullary :: Int -> Inbound a -> Decoder s (Inbound a)
nullary 1 v = pure v
nullary n _ = pure (Malformed ("expected 0 fields, got " <> show (n - 1)))

-- | 'Hello' as a definite list of its 6 fields. Decoding accepts any
-- length >= 6 and ignores extras, so fields can be appended compatibly.
encodeHello :: Hello -> Encoding
encodeHello h =
       encodeListLen 6
    <> encodeWord16 h.protoVersion
    <> encode h.term
    <> encode h.env
    <> encode h.size
    <> encode h.cwd
    <> encode h.intent

decodeHello :: Decoder s Hello
decodeHello = do
    n <- decodeListLen
    if n < 6
        then fail ("hello too short: " <> show n)
        else do
            h <- Hello
                <$> decodeWord16
                <*> decode
                <*> decode
                <*> decode
                <*> decode
                <*> decode
            replicateM_ (n - 6) skipField
            pure h

-- | Skip one CBOR term (used to drop trailing 'Hello' fields).
skipField :: Decoder s ()
skipField = () <$ decodeTerm

encodeMessage :: WireMessage a => a -> ByteString
encodeMessage = toStrictByteString . encodeWire

-- | Decode a framed message payload into an 'Inbound' result.
decodeMessage :: WireMessage a => ByteString -> Inbound a
decodeMessage bs =
    case deserialiseFromBytes decodeInbound (BL.fromStrict bs) of
        Left err       -> Malformed (show err)
        Right (_, ind) -> ind

decodeInbound :: WireMessage a => Decoder s (Inbound a)
decodeInbound = do
    len <- decodeListLen
    if len < 1
        then pure (Malformed "empty message list")
        else do
            tag <- decodeWord
            decodeWirePayload tag len

maxFrame :: Word32
maxFrame = 64 * 1024 * 1024

sendMessage :: WireMessage a => Socket -> a -> IO ()
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

-- | 'Nothing' means the peer closed the connection.
recvMessage :: WireMessage a => Socket -> IO (Maybe (Inbound a))
recvMessage sock = do
    mheader <- recvExactly sock 4
    case mheader of
        Nothing -> pure Nothing
        Just header -> case fromIntegral <$> B.unpack header :: [Word32] of
            [a, b, c, d] -> do
                let n = a `shiftL` 24 .|. b `shiftL` 16 .|. c `shiftL` 8 .|. d
                if n > maxFrame
                    then pure (Just (Malformed "frame too large"))
                    else do
                        mbody <- recvExactly sock (fromIntegral n)
                        pure $ decodeMessage <$> mbody
            _ -> pure (Just (Malformed "short frame header"))

recvExactly :: Socket -> Int -> IO (Maybe ByteString)
recvExactly sock n = go n []
  where
    go 0 acc = pure (Just (B.concat (Prelude.reverse acc)))
    go remaining acc = do
        chunk <- SB.recv sock remaining
        if B.null chunk
            then pure Nothing
            else go (remaining - B.length chunk) (chunk : acc)
