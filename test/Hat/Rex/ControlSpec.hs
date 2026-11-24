module Hat.Rex.ControlSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither, withObject, (.:))
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Hspec

import Hat.Rex.Control
import Hat.Rex.View

-- | Which way a transcript line went.
data Direction = ClientToServer | ServerToClient
    deriving (Eq, Show)

-- | A revision's recorded control transcript: one message per line, never
-- edited. A new revision appends a directory.
transcript :: FilePath -> IO [(Direction, Value)]
transcript revision = do
    raw <- B.readFile ("test/fixtures/rex/" <> revision <> "/control.jsonl")
    either fail pure (mapM line (B8.lines raw))
  where
    line l = Aeson.eitherDecodeStrict l >>= parseEither (withObject "line" $ \o -> do
        dir <- o .: "dir" >>= \case
            ("c2s" :: Text) -> pure ClientToServer
            "s2c" -> pure ServerToClient
            other -> fail ("direction " <> show other)
        msg <- o .: "msg"
        pure (dir, msg))

decodeValue :: Value -> Either Text ServerMessage
decodeValue = decodeServerMessage . BL.toStrict . Aeson.encode

sid :: SessionId
sid = SessionId "session:01a20ce8-6af3-7b3f-a1bf-df8bdbcacd8a"

bid :: BlockId
bid = BlockId "block:01a20ce8-6af4-76be-aecf-6211a916db8d"

wid :: WindowId
wid = WindowId "window:01a20ce8-6b00-78ef-aad9-c75ce0d97b15"

spec :: Spec
spec = describe "Rex control protocol" $ do
    describe "revision 19 transcript" $ do
        it "reads every message the server sent" $ do
            msgs <- transcript "r19"
            let unread = [ (v, e) | (ServerToClient, v) <- msgs, Left e <- [decodeValue v] ]
            unread `shouldBe` []

        it "writes the hello and the attach sequence as recorded" $ do
            sent <- map snd . filter ((== ClientToServer) . fst) <$> transcript "r19"
            [ hello "linux"
              , request 1 sessionList
              , request 2 (sessionAttach sid)
              , request 3 (dataConnect sid bid)
              ] `shouldBe` take 4 sent

        it "reads the recorded results" $ do
            msgs <- transcript "r19"
            let results = [ r | (ServerToClient, v) <- msgs
                          , Right (ResponseMessage r) <- [decodeValue v] ]
                payload n = case [ p | r <- results, r.id == n, Right p <- [r.result] ] of
                    p : _ -> pure p
                    [] -> fail ("no result for request " <> show n)
            (decodeSessionList <$> payload "1")
                `shouldReturn` Right [SessionEntry { sessionId = sid, label = "main" }]
            (decodeAttach <$> payload "2") `shouldReturn` Right View
                { revision = 1
                , sessionId = sid
                , label = "main"
                , activeWindow = Just wid
                , windows =
                    [ Window
                        { windowId = wid
                        , label = "main"
                        , focusedBlock = Just bid
                        , blocks = [Block { blockId = bid, creatorName = Just "com.superlogical.terminal" }]
                        } ]
                }
            (decodeTicket <$> payload "3") `shouldReturn` Right "<redacted>"

        it "reads the events that end a session" $ do
            msgs <- transcript "r19"
            let events = [ e | (ServerToClient, v) <- msgs
                         , Right (EventMessage _ e) <- [decodeValue v] ]
            filter known events `shouldBe`
                [ SessionEvent sid (BlockClosed bid), SessionDestroyed sid ]

    describe "welcome" $ do
        let welcome rev = Welcome { protocol = "rex.control.v1", revision = rev }

        it "accepts the revision hat asked for" $
            checkWelcome (welcome 19) `shouldBe` Right ()

        it "refuses a revision outside hat's range" $ do
            checkWelcome (welcome 18) `shouldSatisfy` isLeft
            checkWelcome (welcome 20) `shouldSatisfy` isLeft

        it "takes a welcome naming no revision as revision 0" $
            decodeServerMessage "{\"type\":\"welcome\",\"protocol\":\"rex.control.v1\",\"limits\":{}}"
                `shouldBe` Right (WelcomeMessage (welcome 0))

        it "refuses another protocol" $
            checkWelcome Welcome { protocol = "rex.control.v2", revision = 19 }
                `shouldSatisfy` isLeft

    describe "tolerance" $ do
        it "carries a message type it does not know" $
            decodeServerMessage "{\"type\":\"notice\",\"text\":\"hi\"}"
                `shouldBe` Right (UnknownMessage "notice")

        it "carries events it does not know, in or out of a session" $ do
            decodeValue (event "hosts_changed" (object []))
                `shouldBe` Right (EventMessage 7 (UnknownEvent "hosts_changed"))
            decodeValue (event "session_event" (object
                    [ "session_id" .= sid
                    , "event" .= object ["type" .= ("window_created" :: Text), "payload" .= object []] ]))
                `shouldBe` Right (EventMessage 7 (SessionEvent sid (UnknownSessionEvent "window_created")))

        it "ignores fields it does not know" $
            decodeValue (object
                    [ "type" .= ("event" :: Text), "seq" .= (7 :: Int), "later" .= True
                    , "payload" .= object
                        [ "type" .= ("session_destroyed" :: Text)
                        , "payload" .= object ["session_id" .= sid, "why" .= ("x" :: Text)] ] ])
                `shouldBe` Right (EventMessage 7 (SessionDestroyed sid))

        it "reads a failed request's error, whatever its code" $
            decodeServerMessage
                "{\"type\":\"response\",\"id\":\"2\",\"seq\":0,\
                \\"error\":{\"code\":\"from_the_future\",\"message\":\"no\",\"status\":404}}"
                `shouldBe` Right (ResponseMessage Response
                    { id = "2", seq = 0
                    , result = Left RexError { code = "from_the_future", message = "no", status = 404 } })

        it "refuses a known event missing the field hat acts on" $
            decodeValue (event "session_destroyed" (object [])) `shouldSatisfy` isLeft

        it "refuses a message with no type" $
            decodeServerMessage "{\"seq\":1}" `shouldSatisfy` isLeft
  where
    event :: Text -> Value -> Value
    event name fields = object
        [ "type" .= ("event" :: Text), "seq" .= (7 :: Int)
        , "payload" .= object ["type" .= name, "payload" .= fields] ]
    known = \case
        UnknownEvent _ -> False
        SessionEvent _ (UnknownSessionEvent _) -> False
        _ -> True
