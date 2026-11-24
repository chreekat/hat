-- | A record of a Rex attach for debugging and for capturing test corpora:
-- the control messages as one JSON line each, in the layout of
-- @test\/fixtures\/rex\/*\/control.jsonl@.
module Hat.Rex.Log
    ( Direction (..)
    , controlLine
    , redact
    ) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Encoding qualified as TE

-- | Which way a control message went.
data Direction = ClientToServer | ServerToClient
    deriving (Eq, Show)

-- | The line recording a control message, secrets redacted. A message that
-- is not JSON is recorded as the text it was.
controlLine :: Direction -> ByteString -> ByteString
controlLine dir raw = BL.toStrict . (<> "\n") . Aeson.encode $ object
    [ "dir" .= (case dir of
        ClientToServer -> "c2s"
        ServerToClient -> "s2c" :: Text)
    , "msg" .= maybe (String (TE.decodeUtf8Lenient raw)) redact (Aeson.decodeStrict raw)
    ]

-- | A message without the values that would let a reader act as this
-- client: reconnect tokens and data tickets, wherever they sit.
redact :: Value -> Value
redact = \case
    Object o -> Object (KeyMap.mapWithKey field o)
    Array a -> Array (fmap redact a)
    v -> v
  where
    field k v
        | Key.toText k `elem` ["reconnect_token", "ticket"] = String "<redacted>"
        | otherwise = redact v
