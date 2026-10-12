-- | Base64 as tmux's OSC 52 handling encodes and decodes it.
module Hat.Base64
    ( base64Encode
    , base64Decode
    ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Word (Word8)

base64Alphabet :: ByteString
base64Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

base64Encode :: ByteString -> ByteString
base64Encode = B.concat . go
  where
    go bs
        | B.null bs = []
        | otherwise =
            let (chunk, rest) = B.splitAt 3 bs
                bytes = B.unpack chunk ++ replicate (3 - B.length chunk) 0
                n = foldl (\acc b -> acc `shiftL` 8 .|. fromIntegral b) (0 :: Int) bytes
                sextet i = B.index base64Alphabet ((n `shiftR` (18 - 6 * i)) .&. 63)
                kept = B.length chunk + 1
            in B.pack (map sextet [0 .. kept - 1] ++ replicate (4 - kept) 0x3d) : go rest

-- | Decode base64, skipping whitespace as tmux's @b64_pton@ does; 'Nothing'
-- for anything that is not base64.
base64Decode :: ByteString -> Maybe ByteString
base64Decode raw = do
    let body = B.filter (`B.notElem` " \t\r\n") raw
        unpadded = B.takeWhile (/= 0x3d) body
    if B.any (/= 0x3d) (B.drop (B.length unpadded) body)
        || B.length unpadded `mod` 4 == 1
        then Nothing
        else B.pack . concat <$> mapM quad (chunks unpadded)
  where
    chunks bs
        | B.null bs = []
        | otherwise = B.take 4 bs : chunks (B.drop 4 bs)
    quad q = do
        vs <- mapM value (B.unpack q)
        let n = foldl (\acc v -> acc `shiftL` 6 .|. v) (0 :: Int) vs
                    `shiftL` (6 * (4 - length vs))
            out = length vs - 1
        pure [ fromIntegral ((n `shiftR` (16 - 8 * i)) .&. 255) | i <- [0 .. out - 1] ]
    value :: Word8 -> Maybe Int
    value b = B.elemIndex b base64Alphabet
