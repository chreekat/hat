{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}  -- Arbitrary instances for wire types live with the wire tests: the library must not depend on QuickCheck

module Hat.Transport.WireSpec (spec) where

import Codec.Serialise (DeserialiseFailure, deserialiseOrFail)
import Control.Concurrent.Async (concurrently)
import Data.Bits (shiftR)
import qualified Data.Bits as Bits
import qualified Data.ByteString as B
import Data.Word (Word32, Word8)
import qualified Data.Text as T
import Network.Socket
    (Family (AF_UNIX), SocketType (Stream), close, socketPair)
import qualified Network.Socket.ByteString.Lazy as SBL
import qualified Data.ByteString.Lazy as BL
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Client (ExitReason (..), runControl)
import Hat.Geometry
import Hat.Term.Cell
import Hat.Transport.Wire

instance Arbitrary Size where
    arbitrary = Size <$> arbitrary <*> arbitrary
    shrink sz = [Size r c | (r, c) <- shrink (sz.rows, sz.cols)]

instance Arbitrary Pos where
    arbitrary = Pos <$> arbitrary <*> arbitrary
    shrink p = [Pos r c | (r, c) <- shrink (p.row, p.col)]

instance Arbitrary Color where
    arbitrary = oneof
        [ pure DefaultColor
        , Indexed <$> arbitrary
        , RGB <$> arbitrary <*> arbitrary <*> arbitrary
        ]
    shrink = \case
        DefaultColor -> []
        Indexed n -> DefaultColor : (Indexed <$> shrink n)
        RGB r g b -> DefaultColor : [RGB r' g' b' | (r', g', b') <- shrink (r, g, b)]

instance Arbitrary Style where
    arbitrary = Style <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrink st =
        [ Style fg' bg' bo un ita rv sk bl ft
        | ((fg', bg', bo, un, ita, rv, sk, bl), ft) <-
            shrink ((st.fg, st.bg, st.bold, st.underline, st.italic,
                     st.reverse, st.strike, st.blink), st.faint)
        ]

genText :: Gen T.Text
genText = T.pack <$> listOf (chooseEnum (' ', '~'))

-- Shrink helpers for nested [[Text]] via String (no Arbitrary Text).
strs :: [[T.Text]] -> [[String]]
strs = map (map T.unpack)

unstrs :: [[String]] -> [[T.Text]]
unstrs = map (map T.pack)

instance Arbitrary DrawOp where
    arbitrary = oneof
        [ Put <$> arbitrary <*> arbitrary <*> genText
        , pure ClearAll
        , CursorAt <$> arbitrary <*> arbitrary
        ]
    shrink = \case
        Put p st t -> ClearAll : [Put p' st' (T.pack t') | (p', st', t') <- shrink (p, st, T.unpack t)]
        ClearAll -> []
        CursorAt p v -> ClearAll : [CursorAt p' v' | (p', v') <- shrink (p, v)]

genIntent :: Gen Intent
genIntent = oneof
    [ AttachIntent <$> listOf (listOf genText)
    , pure ControlIntent
    ]

genHello :: Gen Hello
genHello = Hello
    <$> arbitrary
    <*> genText
    <*> listOf ((,) <$> genText <*> genText)
    <*> arbitrary
    <*> genText
    <*> genIntent

instance Arbitrary ClientToServer where
    arbitrary = oneof
        [ ClientHello <$> genHello
        , Input . B.pack <$> arbitrary
        , Resize <$> arbitrary
        , Command <$> listOf (listOf genText)
        , pure Detach
        ]
    shrink = \case
        ClientHello h ->
            [ ClientHello h { term = T.pack t' } | t' <- shrink (T.unpack h.term) ]
            ++ [ ClientHello h { cwd = T.pack c' } | c' <- shrink (T.unpack h.cwd) ]
        Input bs -> Input . B.pack <$> shrink (B.unpack bs)
        Resize sz -> Resize <$> shrink sz
        Command cmds -> Command . unstrs <$> shrink (strs cmds)
        Detach -> []

instance Arbitrary ServerToClient where
    arbitrary = oneof
        [ Welcome <$> genText
        , Draw <$> listOf arbitrary
        , SetTitle <$> genText
        , pure RingBell
        , Notify . B.pack <$> arbitrary
        , Message <$> genText
        , pure DetachOk
        , pure CommandDone
        , ServerError <$> genText
        , pure Exited
        , ServerVersion <$> arbitrary
        ]
    shrink = \case
        Welcome n -> Welcome . T.pack <$> shrink (T.unpack n)
        Draw ops -> Draw <$> shrink ops
        SetTitle t -> SetTitle . T.pack <$> shrink (T.unpack t)
        RingBell -> []
        Notify raw -> Notify . B.pack <$> shrink (B.unpack raw)
        Message t -> Message . T.pack <$> shrink (T.unpack t)
        DetachOk -> []
        CommandDone -> []
        ServerError e -> ServerError . T.pack <$> shrink (T.unpack e)
        Exited -> []
        ServerVersion v -> ServerVersion <$> shrink v

-- | Render bytes as lowercase hex for golden comparison.
hex :: B.ByteString -> String
hex = concatMap byte . B.unpack
  where
    byte w = [digit (w `shiftR` 4), digit (w Bits..&. 0xf)]
    digit n = "0123456789abcdef" !! fromIntegral n

-- | Prefix a raw CBOR payload with the 4-byte big-endian length header,
-- exactly as 'sendMessage' frames a message.
frameOf :: B.ByteString -> B.ByteString
frameOf payload = B.pack header <> payload
  where
    n = fromIntegral (B.length payload) :: Word32
    header =
        [ fromIntegral (n `shiftR` 24 Bits..&. 0xff)
        , fromIntegral (n `shiftR` 16 Bits..&. 0xff)
        , fromIntegral (n `shiftR` 8 Bits..&. 0xff)
        , fromIntegral (n Bits..&. 0xff)
        ] :: [Word8]

spec :: Spec
spec = do
    -- WARNING: these golden bytes ARE the wire contract. A failure here
    -- means the on-wire encoding changed and a deployed peer may misread it.
    -- NEVER "fix" a golden by blindly pasting the new bytes. Valid ways to
    -- change one: append a NEW message tag (leaving old ones untouched); grow
    -- an append-tolerant leaf (like 'Style' — a longer list an old reader
    -- rejects cleanly and a new reader defaults, pinned by the compat test
    -- below); or, for an envelope reframing, bump 'protocolVersion' knowing all
    -- peers must restart TOGETHER — a bump can't ride 'restart-server', since
    -- the running old server would reject the new client's handshake. These
    -- goldens pin the tag numbers AND the payload shapes (Size, Pos, Color,
    -- Style, DrawOp, Intent).
    describe "golden bytes (client -> server)" $ do
        it "ClientHello" $
            hex (encodeMessage (ClientHello
                (Hello 4 "xterm" [("A", "B")] (Size 24 80) "/tmp" ControlIntent)))
                `shouldBe` "8200860465787465726d9f8261416142ff830018181850642f746d708101"
        it "Input" $
            hex (encodeMessage (Input "hi")) `shouldBe` "8201426869"
        it "Resize" $
            hex (encodeMessage (Resize (Size 24 80))) `shouldBe` "8202830018181850"
        it "Command" $
            hex (encodeMessage (Command [["kill-server"]]))
                `shouldBe` "82039f9f6b6b696c6c2d736572766572ffff"
        it "Detach" $
            hex (encodeMessage Detach) `shouldBe` "8104"

    describe "golden bytes (server -> client)" $ do
        it "Welcome" $
            hex (encodeMessage (Welcome "main")) `shouldBe` "8200646d61696e"
        it "Draw" $
            hex (encodeMessage
                (Draw [Put (Pos 1 2) defaultStyle "x", ClearAll, CursorAt (Pos 3 4) True]))
                `shouldBe` "82019f8400830001028a0081008100f4f4f4f4f4f4f461788101830283000304f5ff"
        it "SetTitle" $
            hex (encodeMessage (SetTitle "t")) `shouldBe` "82026174"
        it "RingBell" $
            hex (encodeMessage RingBell) `shouldBe` "8103"
        it "Notify" $
            hex (encodeMessage (Notify "n")) `shouldBe` "8204416e"
        it "Message" $
            hex (encodeMessage (Message "m")) `shouldBe` "8205616d"
        it "DetachOk" $
            hex (encodeMessage DetachOk) `shouldBe` "8106"
        it "CommandDone" $
            hex (encodeMessage CommandDone) `shouldBe` "8107"
        it "ServerError" $
            hex (encodeMessage (ServerError "boom")) `shouldBe` "820864626f6f6d"
        it "Exited" $
            hex (encodeMessage Exited) `shouldBe` "8109"
        it "ServerVersion" $
            hex (encodeMessage (ServerVersion 5)) `shouldBe` "820a05"

    -- The reason 'faint' ships without a 'protocolVersion' bump: a new build
    -- must read an OLD peer's nine-element (pre-faint) Style as faint-off, so a
    -- new client drives an old server through 'restart-server' and a reload
    -- reads an old handover. A bump would instead make the old server reject the
    -- new client at the handshake, bricking the upgrade.
    describe "append-tolerant leaf compatibility" $ do
        let decodeStyle ws = either (const Nothing) Just
                (deserialiseOrFail (BL.pack ws) :: Either DeserialiseFailure Style)
        it "reads a legacy nine-element Style with faint defaulted off" $
            decodeStyle [0x89,0,0x81,0,0x81,0,0xf4,0xf4,0xf4,0xf4,0xf4,0xf4]
                `shouldBe` Just defaultStyle
        it "keeps the legacy fields when the pre-faint list is short" $
            decodeStyle [0x89,0,0x81,0,0x81,0,0xf5,0xf4,0xf4,0xf4,0xf4,0xf4]
                `shouldBe` Just defaultStyle { bold = True }

    describe "roundtrip" $ do
        prop "client messages" $ \(msg :: ClientToServer) ->
            decodeMessage (encodeMessage msg) === Known msg
        prop "server messages" $ \(msg :: ServerToClient) ->
            decodeMessage (encodeMessage msg) === Known msg

    describe "unknown tags are forward-compatible" $ do
        it "an unused tag decodes to UnknownTag" $
            -- CBOR [999]: array(1) then unsigned 999 (0x19 0x03e7).
            (decodeMessage (B.pack [0x81, 0x19, 0x03, 0xe7]) :: Inbound ServerToClient)
                `shouldBe` UnknownTag 999

    describe "malformed payloads are fatal" $ do
        it "a known tag with wrong arity is Malformed" $
            -- CBOR [8]: ServerError's tag with no Text field.
            (decodeMessage (B.pack [0x81, 0x08]) :: Inbound ServerToClient)
                `shouldSatisfy` isMalformed
        it "garbage bytes are Malformed" $
            (decodeMessage "not cbor at all" :: Inbound ServerToClient)
                `shouldSatisfy` isMalformed

    describe "runControl drain policy over a socketpair" $ do
        it "skips an unknown-tag frame then finishes on CommandDone" $ do
            reason <- withFakeServer
                [ frameOf (encodeMessage (Welcome ""))
                , frameOf (B.pack [0x81, 0x19, 0x03, 0xe7])  -- [999]
                , frameOf (encodeMessage CommandDone)
                ]
            reason `shouldBe` SessionEnded
        it "returns the fatal error on a malformed frame" $ do
            reason <- withFakeServer
                [ frameOf (encodeMessage (Welcome ""))
                , frameOf (B.pack [0x81, 0x08])  -- ServerError tag, no field
                ]
            reason `shouldSatisfy` isRejected

isMalformed :: Inbound a -> Bool
isMalformed (Malformed _) = True
isMalformed _ = False

isRejected :: ExitReason -> Bool
isRejected (Rejected _) = True
isRejected _ = False

-- | Run 'runControl' against a scripted peer that sends the given
-- pre-framed responses, then drains the client's writes until the client
-- closes (which happens when 'runControl' returns).
withFakeServer :: [B.ByteString] -> IO ExitReason
withFakeServer frames = do
    (clientSock, serverSock) <- socketPair AF_UNIX Stream 0
    (reason, _) <- concurrently
        (client clientSock)
        (server serverSock)
    pure reason
  where
    client sock = do
        r <- runControl sock [["kill-server"]]
        close sock
        pure r
    server sock = do
        SBL.sendAll sock (BL.fromChunks frames)
        drainPeer sock
        close sock
    drainPeer sock = do
        chunk <- SBL.recv sock 4096
        if BL.null chunk then pure () else drainPeer sock
