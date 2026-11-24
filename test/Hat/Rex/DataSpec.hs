module Hat.Rex.DataSpec (spec) where

import Data.ByteString qualified as B
import Data.Either (isLeft)
import Test.Hspec

import Hat.Rex.Data

spec :: Spec
spec = describe "Rex data handshake" $ do
    it "sends 0x01 and the ticket's 16 bytes as HELLO" $
        -- The spec's example ticket.
        (encodeHello <$> ticketFromText "AAECAwQFBgcICQoLDA0ODw")
            `shouldBe` Right (B.pack (0x01 : [0 .. 15]))

    it "reads a ticket in the URL alphabet" $
        (encodeHello <$> ticketFromText "-_-_-_-_-_-_-_-_-_-_-w")
            `shouldBe` Right (B.pack (0x01 : take 16 (cycle [0xfb, 0xff, 0xbf])))

    it "refuses a ticket that is not 16 bytes" $ do
        ticketFromText "AAECAwQFBgcICQoLDA0O" `shouldSatisfy` isLeft
        ticketFromText "<redacted>" `shouldSatisfy` isLeft

    it "does not show a ticket" $
        (show <$> ticketFromText "AAECAwQFBgcICQoLDA0ODw") `shouldBe` Right "<ticket>"

    -- 0x81, a 16-byte reconnect token, 3 zero bytes, the retention as 32
    -- bits, then the nonce as 64 bits, little-endian.
    let welcome nonce = B.pack ([0x81] <> replicate 16 0xaa <> [0, 0, 0] <> [0x60, 0xea, 0, 0] <> nonce)

    it "reads the channel nonce of a WELCOME" $
        -- 4554578536633190305, the r19 capture's nonce.
        decodeHandshakeReply (welcome [0xa1, 0xc7, 0xa3, 0x41, 0x0c, 0x1d, 0x35, 0x3f])
            `shouldBe` Right (DataWelcome (Nonce 4554578536633190305))

    it "refuses a WELCOME of another length or with a zero nonce" $ do
        decodeHandshakeReply (B.take 31 (welcome [1, 0, 0, 0, 0, 0, 0, 0])) `shouldSatisfy` isLeft
        decodeHandshakeReply (welcome [0, 0, 0, 0, 0, 0, 0, 0]) `shouldSatisfy` isLeft

    it "reads every REJECT reason" $
        map (\r -> decodeHandshakeReply (B.pack [0x82, r])) [1 .. 8] `shouldBe` map (Right . DataReject)
            [ TicketInvalid, AlreadyConnected, StreamUnavailable, LimitReached
            , MalformedHandshake, ServerFailed, WrongSubprotocol, UnknownReason 8 ]

    it "refuses any other reply" $ do
        decodeHandshakeReply "" `shouldSatisfy` isLeft
        decodeHandshakeReply (B.pack [0x83, 0x01]) `shouldSatisfy` isLeft
