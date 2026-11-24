module Hat.Rex.ReplaySpec (spec) where

import Control.Monad (forM_)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither, withObject, (.:))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Either (isLeft)
import Data.IORef
import Data.List (dropWhileEnd)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import Hat.Geometry (Size (..))
import Hat.Rex.Data (Nonce (..))
import Hat.Rex.Stream
import Hat.Term.Emulator

-- | An action yielding these chunks in turn, then the end of the stream.
chunked :: [ByteString] -> IO (IO ByteString)
chunked chunks = do
    left <- newIORef chunks
    pure $ atomicModifyIORef' left $ \case
        c : rest -> (rest, c)
        [] -> ([], B.empty)

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf n bs
    | B.null bs = []
    | otherwise = let (a, b) = B.splitAt n bs in a : chunksOf n b

-- | The screen as lines without trailing blanks, like Rex's
-- @terminal.format@.
screenLines :: Emulator -> IO [Text]
screenLines e = do
    scr <- snapshot e
    pure (dropWhileEnd T.null [ T.stripEnd (screenRowText scr r) | r <- [0 .. fromIntegral scr.size.rows - 1] ])

-- | Mirror a whole stream: the snapshot, then every piece in order.
mirror :: Nonce -> [ByteString] -> IO (Either Text Emulator)
mirror nonce chunks = do
    next <- chunked chunks
    fromSnapshot 2000 next >>= \case
        Left e -> pure (Left e)
        Right (emu, leftover) -> do
            let step carry bs = do
                    let (carry', ps) = splitRex nonce carry bs
                    forM_ ps $ \case
                        TermBytes t -> feedMirror emu t
                        RexMessage (Resize sz) -> resize emu sz
                        RexMessage (UnknownStreamMessage _) -> pure ()
                        StreamError err -> expectationFailure (T.unpack err)
                    pure carry'
                loop carry = next >>= \c ->
                    if B.null c then pure () else step carry c >>= loop
            Right emu <$ (step noCarry leftover >>= loop)

spec :: Spec
spec = describe "Rex stream replay (revision 19 capture)" $ do
    let fixture name = B.readFile ("test/fixtures/rex/r19/" <> name)
        nonce = Nonce 4554578536633190305
        -- Where the snapshot ends and where the server cut its messages
        -- (stream_meta.json).
        snapshotLen = 1159
        frames bs = [B.take 1276 bs, B.drop 1276 bs]

    it "shows what Rex itself says the terminal shows" $ do
        stream <- fixture "stream.bin"
        content <- either fail pure . (>>= parseEither (withObject "format" (.: "content")))
            . Aeson.eitherDecodeStrict =<< fixture "format.json"
        Right emu <- mirror nonce (frames stream)
        screenLines emu `shouldReturn` map T.stripEnd (T.lines content)
        (.size) <$> snapshot emu `shouldReturn` Size { rows = 24, cols = 80 }

    it "hands back exactly the bytes after the snapshot" $ do
        stream <- fixture "stream.bin"
        forM_ [frames stream, [stream], chunksOf 97 stream] $ \chunks -> do
            next <- chunked chunks
            Right (_, leftover) <- fromSnapshot 2000 next
            rest <- drain next
            (leftover <> rest) `shouldBe` B.drop snapshotLen stream

    it "refuses a stream that ends inside the snapshot" $ do
        stream <- fixture "stream.bin"
        next <- chunked [B.take 500 stream]
        (fmap (const ()) <$> fromSnapshot 2000 next)
            `shouldReturn` Left "the terminal stream ended inside its snapshot"

    it "refuses bytes that are no snapshot" $ do
        next <- chunked [B.replicate 4096 0x41]
        r <- fromSnapshot 2000 next
        fmap (const ()) r `shouldSatisfy` isLeft

    it "starts a snapshot-born terminal with no title and nothing to answer" $ do
        stream <- fixture "stream.bin"
        Right emu <- mirror nonce (frames stream)
        -- A cursor report request and an APC, which feed would act on.
        feedMirror emu "\ESC[6n\ESC_rex1;1;{}\ESC\\"
        title emu `shouldReturn` ""
        feed emu "" `shouldReturn` []
  where
    drain next = do
        c <- next
        if B.null c then pure B.empty else (c <>) <$> drain next
