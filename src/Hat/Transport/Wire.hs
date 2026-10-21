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
-- 'protocolVersion' names this build's dialect. Peers exchange versions
-- ('Hello' up, 'ServerVersion' down) and both speak the minimum
-- ('negotiate'); every version ≥ 'dialectFloor' interoperates, forever. A
-- build therefore READS every dialect ≤ its own (tolerant leaf decoders)
-- and WRITES any dialect ≤ its own ('encodeServerMessageAt'); each
-- level's bytes are pinned by the dialect corpus in WireSpec.
--
--   * Tags are append-only forever. Never renumber a tag, never reuse a
--     retired one, never insert one mid-list. The tag registries below
--     are frozen; new constructors get the next unused number.
--   * A tag's payload shape is frozen forever — the control core
--     (hello, Command, its replies) is what lets any client drive
--     restart-server on any older server. To change a message, add a
--     NEW tag; to grow a leaf (like 'Style'), append the field under a
--     new dialect level and teach 'encodeServerMessageAt' the old form.
--   * Receivers SKIP unknown tags ('UnknownTag') so a new sender can
--     talk to an old receiver.
--   * A malformed payload ('Malformed') is FATAL to the connection: the
--     bytes are unintelligible, so draining further would loop.
--
-- 'Hello' is encoded as a definite list of its fields and its decoder
-- TOLERATES extra trailing elements, so new hello fields can be appended
-- compatibly without a tag or level bump.
module Hat.Transport.Wire
    ( protocolVersion
    , dialectFloor
    , negotiate
    , Intent (..)
    , ExitWait (..)
    , Autostart (..)
    , Hello (..)
    , ClientToServer (..)
    , ServerToClient (..)
    , DrawOp (..)
    , Inbound (..)
    , WireMessage (..)
    , encodeMessage
    , encodeServerMessageAt
    , decodeMessage
    , sendMessage
    , frameServerMessageAt
    , sendFrame
    , ReadEnd (..)
    , newReadEnd
    , recvMessage
    ) where

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (decodeTerm)
import Codec.CBOR.Write (toStrictByteString)
import Codec.Serialise (Serialise, decode, encode)
import Codec.Serialise.Decoding
    (Decoder, decodeListLen, decodeWord, decodeWord16)
import Codec.Serialise.Encoding
    ( Encoding, encodeBreak, encodeListLen, encodeListLenIndef, encodeWord
    , encodeWord16 )
import Control.Exception (mask_)
import Control.Monad (replicateM_, unless)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Network.Socket (Socket)
import Network.Socket.ByteString qualified as SB

import Hat.Geometry
import Hat.Term.Cell

-- | This build's wire version. See the evolution charter in the module
-- haddock and 'negotiate'.
protocolVersion :: Word16
protocolVersion = 12

-- | The oldest dialect any build ever froze; nothing older exists to speak.
dialectFloor :: Word16
dialectFloor = 4

-- | Accept any client at or above 'dialectFloor'; both sides then speak
-- @min client server@. The server answers with 'ServerVersion' so the
-- client computes the same minimum.
negotiate :: Word16 -> Either Text Word16
negotiate clientV
    | clientV < dialectFloor =
        Left ("protocol too old: client " <> T.pack (show clientV)
              <> ", server floor " <> T.pack (show dialectFloor))
    | otherwise = Right (min protocolVersion clientV)

deriving instance Generic Size
deriving anyclass instance Serialise Size
deriving instance Generic Pos
deriving anyclass instance Serialise Pos
-- 'Color' and 'Style' live in 'Hat.Term.Cell'; the wire reuses them as leaf
-- field types. 'Style' is the one dialect-sensitive leaf — see
-- 'Hat.Term.Cell.encodeStyleAt'.

-- | Why this client connected: to attach and render, or only to issue
-- commands (e.g. @hat kill-server@ from a shell).
--
-- 'AttachIntent' carries setup commands the server runs to establish the
-- client's session before rendering — e.g. @new-session@ or
-- @attach-session -t@ typed at a shell. An empty list attaches to the
-- existing (or a freshly created) session, the plain @hat@/@hat attach@
-- behaviour.
--
-- 'ControlModeIntent' is tmux's control mode (@hat -C@): a client that
-- attaches without a terminal, sends command lines as 'Input', and reads
-- the textual protocol back as 'WriteStdout' until a 'ControlExit'. Its
-- commands are the session-establishing command line (e.g. @attach@).
data Intent = AttachIntent [[Text]] | ControlIntent | ControlModeIntent [[Text]]
    deriving (Eq, Show)

-- Intent tag registry (append-only; never renumber or reuse):
--   AttachIntent      = 0
--   ControlIntent     = 1
--   ControlModeIntent = 2
-- The first two mirror the Generic encoding they shipped with, byte for
-- byte; an older server fails to decode a newer tag, so it rejects the
-- hello rather than misreading it.
instance Serialise Intent where
    encode = \case
        AttachIntent cmds -> encodeListLen 2 <> encodeWord 0 <> encode cmds
        ControlIntent -> encodeListLen 1 <> encodeWord 1
        ControlModeIntent cmds -> encodeListLen 2 <> encodeWord 2 <> encode cmds
    decode = do
        len <- decodeListLen
        tag <- decodeWord
        case (tag, len) of
            (0, 2) -> AttachIntent <$> decode
            (1, 1) -> pure ControlIntent
            (2, 2) -> ControlModeIntent <$> decode
            _ -> fail ("unknown intent " <> show tag <> "/" <> show len)

-- | Whether a control-mode client, having printed @%exit@, lingers until its
-- input sends an empty line or closes (@refresh-client -f wait-exit@).
data ExitWait = ExitNow | AwaitEmptyLine
    deriving (Eq, Show)

-- | Whether this invocation spawned the server it is talking to, or found
-- one already running. See 'Hat.Server.startupGate'.
data Autostart = Autostarted | Joined
    deriving (Eq, Show)

-- | The client's opening handshake. A standalone single-constructor
-- record so its field selectors are total.
data Hello = Hello
    { protoVersion :: Word16
    , term         :: Text
    , env          :: [(Text, Text)]
    , size         :: Size
    , cwd          :: Text
    , intent       :: Intent
    , autostarted  :: Bool  -- ^ appended 2026-08; absent in a six-field
                            --   hello -> 'False'. See 'Hat.Server.startupGate'.
    , sessionHist  :: [Text]  -- ^ the MRU session names a reattaching client
                              --   carries across a restart; absent in a
                              --   seven-field hello -> @[]@. See
                              --   'Hat.Server.Conn.newClient'.
    , termCaps     :: Maybe [Text]
        -- ^ the terminfo capabilities read for @term@, as
        --   'Hat.Client.Terminfo.readTermCaps' lists them (@[]@ when the
        --   client has no terminal or no @TERM@); 'Nothing' from an
        --   eight-field hello, whose client predates them.
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (Serialise)

data ClientToServer
    = ClientHello Hello
    | Input ByteString      -- ^ raw bytes from the client's tty
    | Resize Size
    | Command [[Text]]      -- ^ pre-tokenised commands, one inner list per @;@-separated command
    | Detach
    | StdinData ByteString  -- ^ a chunk of the client's standard input,
                            --   answering 'ReadStdin'
    | StdinEnd              -- ^ the client's standard input is exhausted
    | LockDone              -- ^ retired: the terminal a 'LockClient'
                            --   asked for is the server's again
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
    | ServerVersion Word16  -- ^ the server's own wire version; see 'negotiate'
    | RestartClient         -- ^ re-exec yourself in place, keeping the attachment
    | RestartClientTo Text [Text]
        -- ^ 'RestartClient' naming the session to reattach to and the MRU
        --   session history to carry back (most-recent first; @[]@ from a
        --   pre-level-7 server)
    | ReadStdin             -- ^ send your standard input as 'StdinData'
                            --   chunks and a 'StdinEnd' (level 8 and up)
    | WriteStdout ByteString  -- ^ bytes for the client's standard output,
                              --   verbatim
    | ControlExit (Maybe Text) ExitWait Int
        -- ^ a control-mode client is done: print @%exit@ with the reason
        --   (if any), linger per 'ExitWait', and exit with the status
        --   (level 9 and up)
    | LockClient Text
        -- ^ retired: hand the terminal to this shell command
        --   (@lock-command@), then answer 'LockDone' (sent only by a
        --   level-10-to-12 server, which locked clients)
    | OpenedTerminal [Text] [Text]
        -- ^ the terminal the server opened for an attaching client: its
        --   capabilities and applied features, as 'Hat.Term.Caps.capList'
        --   and 'Hat.Term.Caps.featureName' give them. Sent ahead of
        --   'Welcome', so only to a level-11-and-up peer.
    | ExitStatus Int
        -- ^ the status a command client exits with once its commands are
        --   done (level 12 and up; an older peer skips the tag and exits 0)
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
--   StdinData   = 5
--   StdinEnd    = 6
--   LockDone    = 7  (retired; hat no longer locks clients)
instance WireMessage ClientToServer where
    encodeWire = \case
        ClientHello h -> encodeListLen 2 <> encodeWord 0 <> encodeHello h
        Input bs      -> encodeListLen 2 <> encodeWord 1 <> encode bs
        Resize sz     -> encodeListLen 2 <> encodeWord 2 <> encode sz
        Command cmds  -> encodeListLen 2 <> encodeWord 3 <> encode cmds
        Detach        -> encodeListLen 1 <> encodeWord 4
        StdinData bs  -> encodeListLen 2 <> encodeWord 5 <> encode bs
        StdinEnd      -> encodeListLen 1 <> encodeWord 6
        LockDone      -> encodeListLen 1 <> encodeWord 7
    decodeWirePayload tag len = case tag of
        0 -> field len (Known . ClientHello <$> decodeHello)
        1 -> field len (Known . Input <$> decode)
        2 -> field len (Known . Resize <$> decode)
        3 -> field len (Known . Command <$> decode)
        4 -> nullary len (Known Detach)
        5 -> field len (Known . StdinData <$> decode)
        6 -> nullary len (Known StdinEnd)
        7 -> nullary len (Known LockDone)
        _ -> pure (UnknownTag tag)

-- ServerToClient tag registry (append-only; never renumber or reuse):
--   Welcome       = 0
--   Draw          = 1
--   SetTitle      = 2
--   RingBell      = 3
--   Notify        = 4
--   Message       = 5
--   DetachOk      = 6
--   CommandDone   = 7
--   ServerError   = 8
--   Exited        = 9
--   ServerVersion = 10
--   RestartClient = 11
--   RestartClientTo = 12
--   ReadStdin     = 13
--   WriteStdout   = 14
--   ControlExit   = 15
--   LockClient    = 16  (retired; hat no longer locks clients)
--   OpenedTerminal = 17
--   ExitStatus    = 18
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
        ServerVersion v -> encodeListLen 2 <> encodeWord 10 <> encode v
        RestartClient -> encodeListLen 1 <> encodeWord 11
        RestartClientTo t hist ->
            encodeListLen 3 <> encodeWord 12 <> encode t <> encode hist
        ReadStdin     -> encodeListLen 1 <> encodeWord 13
        WriteStdout bs -> encodeListLen 2 <> encodeWord 14 <> encode bs
        ControlExit reason wait status ->
            encodeListLen 4 <> encodeWord 15 <> encode reason
                <> encodeWord (case wait of
                    ExitNow -> 0
                    AwaitEmptyLine -> 1)
                <> encode status
        LockClient cmd -> encodeListLen 2 <> encodeWord 16 <> encode cmd
        OpenedTerminal caps feats ->
            encodeListLen 3 <> encodeWord 17 <> encode caps <> encode feats
        ExitStatus n  -> encodeListLen 2 <> encodeWord 18 <> encode n
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
        10 -> field len (Known . ServerVersion <$> decode)
        11 -> nullary len (Known RestartClient)
        -- A level-6 server omits the history field: default it to @[]@.
        12 -> case len of
            2 -> Known . (`RestartClientTo` []) <$> decode
            3 -> Known <$> (RestartClientTo <$> decode <*> decode)
            _ -> pure (Malformed
                ("RestartClientTo: expected 1-2 fields, got " <> show (len - 1)))
        13 -> nullary len (Known ReadStdin)
        14 -> field len (Known . WriteStdout <$> decode)
        15 -> case len of
            4 -> do
                reason <- decode
                waitTag <- decodeWord
                status <- decode
                pure $ case waitTag of
                    0 -> Known (ControlExit reason ExitNow status)
                    1 -> Known (ControlExit reason AwaitEmptyLine status)
                    w -> Malformed ("ControlExit: unknown wait " <> show w)
            _ -> pure (Malformed
                ("ControlExit: expected 3 fields, got " <> show (len - 1)))
        16 -> field len (Known . LockClient <$> decode)
        17 -> case len of
            3 -> Known <$> (OpenedTerminal <$> decode <*> decode)
            _ -> pure (Malformed
                ("OpenedTerminal: expected 2 fields, got " <> show (len - 1)))
        18 -> field len (Known . ExitStatus <$> decode)
        _ -> pure (UnknownTag tag)

-- | A single-field constructor: the payload list must be @[tag, field]@.
field :: Int -> Decoder s (Inbound a) -> Decoder s (Inbound a)
field 2 d = d
field n _ = pure (Malformed ("expected 1 field, got " <> show (n - 1)))

-- | A nullary constructor: the payload list must be @[tag]@.
nullary :: Int -> Inbound a -> Decoder s (Inbound a)
nullary 1 v = pure v
nullary n _ = pure (Malformed ("expected 0 fields, got " <> show (n - 1)))

-- | 'Hello' as a definite list of its fields. Decoding accepts any length
-- >= 6 and ignores extras, so fields can be appended compatibly.
encodeHello :: Hello -> Encoding
encodeHello h =
       encodeListLen (maybe 8 (const 9) h.termCaps)
    <> encodeWord16 h.protoVersion
    <> encode h.term
    <> encode h.env
    <> encode h.size
    <> encode h.cwd
    <> encode h.intent
    <> encode h.autostarted
    <> encode h.sessionHist
    <> foldMap encode h.termCaps

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
                -- Fields appended since the six-field original default when
                -- absent; any the decoder doesn't know yet are skipped.
                <*> (if n >= 7 then decode else pure False)
                <*> (if n >= 8 then decode else pure [])
                <*> (if n >= 9 then Just <$> decode else pure Nothing)
            replicateM_ (max 0 (n - 9)) skipField
            pure h

-- | Skip one CBOR term (used to drop trailing 'Hello' fields).
skipField :: Decoder s ()
skipField = () <$ decodeTerm

encodeMessage :: WireMessage a => a -> ByteString
encodeMessage = toStrictByteString . encodeWire

-- | Encode for a peer at the negotiated dialect @lvl@: 'Draw' carries the
-- level's 'Style' leaf, and a tag the level predates is rewritten to the
-- form that peer knows; every other message is shape-frozen.
encodeServerMessageAt :: Word16 -> ServerToClient -> ByteString
encodeServerMessageAt lvl = \case
    Draw ops -> toStrictByteString $
        encodeListLen 2 <> encodeWord 1 <> encodeDrawOpsAt lvl ops
    RestartClientTo _ _ | lvl < 6 -> encodeMessage RestartClient
    RestartClientTo t _ | lvl < 7 ->
        -- A level-6 peer knows the name-only form; drop the history.
        toStrictByteString $ encodeListLen 2 <> encodeWord 12 <> encode t
    -- A pre-8 peer only prints text lines.
    WriteStdout bs | lvl < 8 -> encodeMessage (Message (TE.decodeUtf8Lenient bs))
    -- Only a control-mode client (level 9 and up) is ever sent this; an
    -- older peer would only know its session ended.
    ControlExit {} | lvl < 9 -> encodeMessage Exited
    -- Retired, and never sent; a pre-10 peer would be told it cannot lock.
    LockClient _ | lvl < 10 ->
        encodeMessage (Message "locking: not supported by this client")
    msg -> encodeMessage msg

-- Mirrors the Generic @[DrawOp]@ encoding byte-for-byte (pinned in WireSpec),
-- with 'Style' at @lvl@.
encodeDrawOpsAt :: Word16 -> [DrawOp] -> Encoding
encodeDrawOpsAt lvl ops = case ops of
    [] -> encodeListLen 0  -- cborg encodes only the empty list definite
    _  -> encodeListLenIndef <> foldMap opAt ops <> encodeBreak
  where
    opAt = \case
        Put p s t -> encodeListLen 4 <> encodeWord 0
            <> encode p <> encodeStyleAt lvl s <> encode t
        ClearAll -> encodeListLen 1 <> encodeWord 1
        CursorAt p v -> encodeListLen 3 <> encodeWord 2
            <> encode p <> encode v

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
sendMessage sock = sendFrame sock . framePayload . encodeMessage

-- | A message as 'sendFrame' writes it, at a peer's negotiated dialect; see
-- 'encodeServerMessageAt'.
frameServerMessageAt :: Word16 -> ServerToClient -> ByteString
frameServerMessageAt lvl = framePayload . encodeServerMessageAt lvl

framePayload :: ByteString -> ByteString
framePayload payload = header <> payload
  where
    n = fromIntegral (B.length payload) :: Word32
    header = B.pack
        [ fromIntegral (n `shiftR` 24 .&. 0xff)
        , fromIntegral (n `shiftR` 16 .&. 0xff)
        , fromIntegral (n `shiftR` 8 .&. 0xff)
        , fromIntegral (n .&. 0xff)
        ]

-- | Write one framed message. One contiguous buffer, one write: a frame is
-- small and per-frame syscalls outweigh the copy.
sendFrame :: Socket -> ByteString -> IO ()
sendFrame = SB.sendAll

-- | A connection's receive side: the socket with its pull buffer, so a
-- framed message is normally served by a single recv. Owned by the one
-- reader thread of its connection; bytes buffered past one message are
-- served to the next, so a socket must keep the same 'ReadEnd' for life.
data ReadEnd = ReadEnd
    { sock :: Socket
    , pending :: IORef ByteString
    }

newReadEnd :: Socket -> IO ReadEnd
newReadEnd s = ReadEnd s <$> newIORef B.empty

-- | 'Nothing' means the peer closed the connection. The buffer is
-- consumed only once the whole frame is present, so a read cancelled at
-- any point (the escape-time race, a test timeout) leaves the stream
-- intact for the next call.
recvMessage :: WireMessage a => ReadEnd -> IO (Maybe (Inbound a))
recvMessage re = do
    mbuf <- fillTo re 4
    case mbuf of
        Nothing -> pure Nothing
        Just buf -> case fromIntegral <$> B.unpack (B.take 4 buf) :: [Word32] of
            [a, b, c, d] -> do
                let n = a `shiftL` 24 .|. b `shiftL` 16 .|. c `shiftL` 8 .|. d
                if n > maxFrame
                    then do
                        writeIORef re.pending (B.drop 4 buf)
                        pure (Just (Malformed "frame too large"))
                    else do
                        let end = 4 + fromIntegral n
                        mfull <- fillTo re end
                        case mfull of
                            Nothing -> pure Nothing
                            Just full -> do
                                writeIORef re.pending (B.drop end full)
                                pure $ Just $ decodeMessage
                                    (B.take (fromIntegral n) (B.drop 4 full))
            _ -> pure (Just (Malformed "short frame header"))

-- | Grow the pull buffer to at least @n@ bytes and hand it back without
-- consuming any of it. The refill appends under mask, so bytes that left
-- the kernel are always retained. 'Nothing' means the peer closed first.
fillTo :: ReadEnd -> Int -> IO (Maybe ByteString)
fillTo re n = do
    buf <- readIORef re.pending
    if B.length buf >= n
        then pure (Just buf)
        else do
            chunk <- mask_ $ do
                chunk <- SB.recv re.sock 65536
                unless (B.null chunk) $
                    modifyIORef' re.pending (<> chunk)
                pure chunk
            if B.null chunk
                then pure Nothing
                else fillTo re n
