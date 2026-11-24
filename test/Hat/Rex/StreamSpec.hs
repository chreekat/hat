module Hat.Rex.StreamSpec (spec) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.List (mapAccumL)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Hat.Geometry (Size (..))
import Hat.Rex.Data (Nonce (..))
import Hat.Rex.Stream

-- | The r19 capture's channel nonce and where its snapshot ends
-- (@stream_meta.json@).
captureNonce :: Nonce
captureNonce = Nonce 4554578536633190305

snapshotLen :: Int
snapshotLen = 1159

-- | The pieces of a stream delivered in the given chunks, with neighbouring
-- terminal bytes joined: where a chunk ends has no meaning.
pieces :: Nonce -> [ByteString] -> [Piece]
pieces nonce = joined . concat . snd . mapAccumL (splitRex nonce) noCarry
  where
    joined = \case
        TermBytes a : TermBytes b : rest -> joined (TermBytes (a <> b) : rest)
        p : rest -> p : joined rest
        [] -> []

-- | Cut points into a byte string, as offsets.
newtype Cuts = Cuts [Int]
    deriving (Show)

instance Arbitrary Cuts where
    arbitrary = Cuts <$> listOf (chooseInt (0, 400))
    shrink (Cuts cs) = Cuts <$> shrinkList shrink cs

cutAt :: Cuts -> ByteString -> [ByteString]
cutAt (Cuts cs) = go 0 (foldr insertSorted [] cs)
  where
    insertSorted c acc = let (lo, hi) = span (< c) acc in lo <> (c : hi)
    go _ [] bs = [bs]
    go at (c : rest) bs =
        let (a, b) = B.splitAt (c - at) bs in a : go (max at c) rest b

rex :: Nonce -> ByteString -> ByteString
rex (Nonce n) json = "\ESC_rex1;" <> B8.pack (show n) <> ";" <> json <> "\ESC\\"

spec :: Spec
spec = describe "Rex terminal stream" $ do
    let nonce = Nonce 42
        resizeTo100 = rex nonce "{\"type\":\"resize\",\"cols\":100,\"rows\":30,\"owner\":null}"

    describe "revision 19 capture" $ do
        it "holds one resize to 80x24 right after the snapshot, then the program's bytes" $ do
            stream <- B.drop snapshotLen <$> B.readFile "test/fixtures/rex/r19/stream.bin"
            -- The 117-byte message, then everything to the end.
            pieces captureNonce [stream] `shouldBe`
                [ RexMessage (Resize Size { rows = 24, cols = 80 })
                , TermBytes (B.drop 117 stream) ]

        prop "splits the same however it is chunked" $ \cuts -> ioProperty $ do
            stream <- B.drop snapshotLen <$> B.readFile "test/fixtures/rex/r19/stream.bin"
            pure (pieces captureNonce (cutAt cuts stream) === pieces captureNonce [stream])

    it "applies a message between the bytes around it" $
        pieces nonce ["ab" <> resizeTo100 <> "cd"] `shouldBe`
            [TermBytes "ab", RexMessage (Resize Size { rows = 30, cols = 100 }), TermBytes "cd"]

    prop "finds a message across any chunking" $ \cuts ->
        let stream = "ab\ESC" <> resizeTo100 <> "\ESC_rex" <> resizeTo100 <> "cd\ESC_"
        in pieces nonce (cutAt cuts stream) === pieces nonce [stream]

    it "leaves a look-alike with another nonce among the terminal bytes" $ do
        let lookalike = rex (Nonce 43) "{\"type\":\"resize\",\"cols\":1,\"rows\":1}"
        pieces nonce ["a" <> lookalike <> "b"] `shouldBe` [TermBytes ("a" <> lookalike <> "b")]

    it "leaves any other APC among the terminal bytes" $
        pieces nonce ["\ESC_Gf=24;AAAA\ESC\\"] `shouldBe` [TermBytes "\ESC_Gf=24;AAAA\ESC\\"]

    it "carries a message type it does not know" $
        pieces nonce [rex nonce "{\"type\":\"sparkle\",\"n\":1}"]
            `shouldBe` [RexMessage (UnknownStreamMessage "sparkle")]

    it "fails the stream on a resize it cannot read" $
        pieces nonce [rex nonce "{\"type\":\"resize\",\"cols\":\"wide\"}"] `shouldSatisfy` \case
            [StreamError _] -> True
            _ -> False

    it "fails the stream on a message past the size cap" $
        pieces nonce ["\ESC_rex1;42;", B.replicate (maxMessageBytes + 1) 0x20] `shouldSatisfy` \case
            [StreamError _] -> True
            _ -> False
