-- | Rex's control protocol (@rex.control.v1@): the JSON messages of the
-- control connection, as a client writes and reads them.
--
-- Rex owns this format. Fields hat does not act on are ignored, and a
-- message or event hat does not know is carried as unknown rather than
-- refused; a missing or mistyped field hat does act on is a decoding
-- error.
module Hat.Rex.Control
    ( -- * Revisions
      apiRevision
    , apiMinRevision
      -- * Client to server
    , hello
    , Call (..)
    , request
    , requestId
    , sessionList
    , sessionAttach
    , dataConnect
      -- * Server to client
    , ServerMessage (..)
    , Welcome (..)
    , checkWelcome
    , Response (..)
    , Event (..)
    , SessionEvent (..)
    , decodeServerMessage
      -- * Results
    , SessionEntry (..)
    , decodeSessionList
    , decodeAttach
    , decodeTicket
    ) where

import Data.Aeson (Value, eitherDecodeStrict, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types (Object, Parser, parseEither)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TB

import Hat.Rex.View

-- | The control API revision hat was written against.
apiRevision :: Int
apiRevision = 19

-- | The oldest revision hat can use: the oldest with a corpus under
-- @test\/fixtures\/rex@.
apiMinRevision :: Int
apiMinRevision = 19

-- | The first message of a connection, from a client on the given platform
-- (one of the protocol's names, such as @linux@).
hello :: Text -> Value
hello platform = object
    [ "type" .= ("hello" :: Text)
    , "api_revision" .= apiRevision
    , "api_min_revision" .= apiMinRevision
    , "client" .= object
        [ "kind" .= ("cli" :: Text)
        , "platform" .= platform
        , "name" .= ("hat" :: Text)
        ]
    ]

-- | A method and its arguments.
data Call = Call
    { method :: Text
    , payload :: Maybe Value  -- ^ 'Nothing' for a method that takes none
    }
    deriving (Eq, Show)

-- | The id of the request with the given number: the number in decimal.
requestId :: Int -> Text
requestId = TL.toStrict . TB.toLazyText . TB.decimal

-- | A call as the request with the given number.
request :: Int -> Call -> Value
request n call = object $
    [ "type" .= ("request" :: Text)
    , "id" .= requestId n
    , "method" .= call.method
    ] <> maybe [] (\p -> ["payload" .= p]) call.payload

sessionList :: Call
sessionList = Call { method = "session.list", payload = Nothing }

sessionAttach :: SessionId -> Call
sessionAttach sid = Call
    { method = "session.attach"
    , payload = Just (object ["session_id" .= sid])
    }

-- | Ask for a ticket to a block's terminal stream.
dataConnect :: SessionId -> BlockId -> Call
dataConnect sid bid = Call
    { method = "data.connect"
    , payload = Just $ object
        [ "session_id" .= sid
        , "block_id" .= bid
        , "data_ref_id" .= ("terminal" :: Text)
        , "subprotocol" .= ("rex.data.terminal.v1" :: Text)
        ]
    }

data ServerMessage
    = WelcomeMessage Welcome
    | ResponseMessage Response
    | EventMessage Event
    | UnknownMessage Text     -- ^ a @type@ from a newer server
    deriving (Eq, Show)

data Welcome = Welcome
    { protocol :: Text
    , revision :: Int  -- ^ the revision every later message uses
    }
    deriving (Eq, Show)

data Response = Response
    { id :: Text
    , result :: Either Text Value  -- ^ the result, or the server's words for why there is none
    }
    deriving (Eq, Show)

data Event
    = SessionDestroyed SessionId
    | SessionEvent SessionId SessionEvent
    | UnknownEvent Text
    deriving (Eq, Show)

data SessionEvent
    = BlockClosed BlockId
    | UnknownSessionEvent Text
    deriving (Eq, Show)

-- | Whether a welcome is one hat can go on from; if not, why, for the user.
checkWelcome :: Welcome -> Either Text ()
checkWelcome w
    | w.protocol /= "rex.control.v1" =
        Left ("the server speaks " <> w.protocol <> ", not rex.control.v1")
    | w.revision < apiMinRevision || w.revision > apiRevision =
        Left ("the server chose Rex API revision " <> T.pack (show w.revision)
            <> "; this hat speaks " <> range)
    | otherwise = Right ()
  where
    range | apiMinRevision == apiRevision = "only " <> T.pack (show apiRevision)
          | otherwise = T.pack (show apiMinRevision) <> " to " <> T.pack (show apiRevision)

-- | Read one text message of the control connection.
decodeServerMessage :: ByteString -> Either Text ServerMessage
decodeServerMessage bs = first T.pack $
    eitherDecodeStrict bs >>= parseEither (withObject "message" message)
  where
    message o = (o .: "type" :: Parser Text) >>= \case
        "welcome" -> do
            protocol <- o .: "protocol"
            revision <- o .:? "api_revision" .!= 0
            pure (WelcomeMessage Welcome { protocol, revision })
        "response" -> do
            rid <- o .: "id"
            merr <- o .:? "error"
            result <- case merr of
                Just e -> Left <$> withObject "error" (.: "message") e
                Nothing -> Right . fromMaybe (object []) <$> o .:? "payload"
            pure (ResponseMessage Response { id = rid, result })
        "event" -> EventMessage <$> (o .: "payload" >>= named event)
        other -> pure (UnknownMessage other)
    event name o = case name of
        "session_destroyed" -> SessionDestroyed <$> o .: "session_id"
        "session_event" ->
            SessionEvent <$> o .: "session_id" <*> (o .: "event" >>= named sessionEvent)
        _ -> pure (UnknownEvent name)
    sessionEvent name o = case name of
        "block_closed" -> BlockClosed <$> o .: "block_id"
        _ -> pure (UnknownSessionEvent name)

-- | Read an event's envelope: its name in @type@, its fields in @payload@.
named :: (Text -> Object -> Parser a) -> Value -> Parser a
named k = withObject "event" $ \o -> do
    name <- o .: "type"
    fields <- o .:? "payload" .!= mempty
    k name fields

-- | A session as @session.list@ lists it.
data SessionEntry = SessionEntry
    { sessionId :: SessionId
    , label :: Text
    }
    deriving (Eq, Show)

-- | The result of 'sessionList', in the server's order.
decodeSessionList :: Value -> Either Text [SessionEntry]
decodeSessionList = resultOf "session.list" $ \o -> do
    sessions <- o .:? "sessions" .!= []
    mapM (withObject "session" entry) sessions
  where
    entry s = do
        sessionId <- s .: "session_id"
        label <- s .:? "label" .!= ""
        pure SessionEntry { sessionId, label }

-- | The result of 'sessionAttach': the session's view.
decodeAttach :: Value -> Either Text View
decodeAttach = resultOf "session.attach" $ \o -> o .: "view" >>= parseView

-- | The result of 'dataConnect': the ticket, still in base64url.
decodeTicket :: Value -> Either Text Text
decodeTicket = resultOf "data.connect" (.: "ticket")

resultOf :: String -> (Object -> Parser a) -> Value -> Either Text a
resultOf name p = first T.pack . parseEither (withObject (name <> " result") p)
