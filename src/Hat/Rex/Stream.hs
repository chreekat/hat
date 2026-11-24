-- | A Rex terminal stream after its snapshot: the program's output, with
-- Rex's own messages among it as @ESC _ rex1;\<nonce\>;\<json\> ESC \\@.
module Hat.Rex.Stream
    ( Piece (..)
    , StreamMessage (..)
    , Carry
    , noCarry
    , splitRex
    , maxMessageBytes
    ) where

import Data.Aeson (eitherDecodeStrict, withObject, (.:))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)

import Hat.Geometry (Size (..))
import Hat.Rex.Data (Nonce (..))

-- | One stretch of a stream, in stream order.
data Piece
    = TermBytes ByteString   -- ^ for the terminal, as they are
    | RexMessage StreamMessage
    | StreamError Text       -- ^ the stream cannot be followed past here
    deriving (Eq, Show)

data StreamMessage
    = Resize Size            -- ^ the terminal's size from here on
    | UnknownStreamMessage Text  -- ^ a @type@ hat does not act on
    deriving (Eq, Show)

-- | The end of a chunk that may be the start of a Rex message.
newtype Carry = Carry ByteString
    deriving (Eq, Show)

noCarry :: Carry
noCarry = Carry B.empty

-- | The most a Rex message may hold between its marker and its end.
maxMessageBytes :: Int
maxMessageBytes = 1024 * 1024

-- | Split what was carried plus a new chunk into terminal bytes and Rex
-- messages. Only the exact marker carrying the channel's nonce opens a
-- message: the nonce is a secret no program can print, and JSON holds no raw
-- ESC, so the first @ESC \\@ after the marker ends it. Any other APC,
-- @rex1@ with another nonce included, stays among the terminal bytes.
splitRex :: Nonce -> Carry -> ByteString -> (Carry, [Piece])
splitRex (Nonce nonce) (Carry carried) chunk = go (carried <> chunk)
  where
    marker = BL.toStrict . BB.toLazyByteString $
        "\ESC_rex1;" <> BB.word64Dec nonce <> ";"
    go buf = case B.breakSubstring marker buf of
        (before, rest)
            | B.null rest ->
                let held = partialMarker buf
                    out = B.dropEnd (B.length held) buf
                in (Carry held, term out)
            | otherwise ->
                let body = B.drop (B.length marker) rest
                in case B.breakSubstring "\ESC\\" body of
                    (json, end)
                        | B.length json > maxMessageBytes ->
                            (noCarry, term before <> [StreamError "a Rex stream message exceeds 1 MiB"])
                        | B.null end -> (Carry rest, term before)
                        | otherwise ->
                            (term before <>) . (message json :) <$> go (B.drop 2 end)
    term bs = [TermBytes bs | not (B.null bs)]
    -- The longest end of the buffer that begins the marker.
    partialMarker buf =
        case [ t | n <- [min (B.length marker - 1) (B.length buf), min (B.length marker - 1) (B.length buf) - 1 .. 1]
                 , let t = B.takeEnd n buf, t `B.isPrefixOf` marker ] of
            t : _ -> t
            [] -> B.empty

-- | Read a Rex message's JSON.
message :: ByteString -> Piece
message json = either (StreamError . ("unreadable Rex stream message: " <>) . T.pack) RexMessage $
    eitherDecodeStrict json >>= parseEither (withObject "stream message" $ \o ->
        (o .: "type" :: Parser Text) >>= \case
            "resize" -> do
                cols <- o .: "cols" :: Parser Word16
                rows <- o .: "rows"
                pure (Resize Size { rows, cols })
            other -> pure (UnknownStreamMessage other))
