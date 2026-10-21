-- | A command argument as it arrived from a client's argv: text, or bytes
-- that are not UTF-8 and so cannot become 'Text' without loss.
module Hat.Command.Arg
    ( Arg (..)
    , argFromBytes
    , argBytes
    , argLossy
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE

data Arg
    = Utf8Arg Text
    | RawArg ByteString  -- ^ never valid UTF-8
    deriving (Eq, Show)

-- | Strict UTF-8 decoding, whatever the locale: overlong forms, surrogates
-- and code points past U+10FFFF are raw too.
argFromBytes :: ByteString -> Arg
argFromBytes bytes = either (const (RawArg bytes)) Utf8Arg (TE.decodeUtf8' bytes)

argBytes :: Arg -> ByteString
argBytes = \case
    Utf8Arg t -> TE.encodeUtf8 t
    RawArg bytes -> bytes

-- | U+FFFD for each invalid sequence: for logs, hooks, toasts, and peers
-- that only carry text.
argLossy :: Arg -> Text
argLossy = \case
    Utf8Arg t -> t
    RawArg bytes -> TE.decodeUtf8Lenient bytes
