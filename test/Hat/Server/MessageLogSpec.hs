module Hat.Server.MessageLogSpec (spec) where

import Control.Concurrent.STM (atomically, writeTVar)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import Hat.Log (newLogger)
import Hat.Model (ServerState (..), StartupPhase (..), newServerState)
import Hat.Model.MessageLog (MessageEntry (..), MessageLog (..), addMessage, emptyMessageLog)
import Hat.Server (Reply (..))
import Hat.Server.Dispatch (runArgv)
import Hat.Server.Keymap (defaultKeymap)
import Hat.Server.MessageLog (commandMessage)

server :: IO ServerState
server = do
    lg <- newLogger "/dev/null"
    newServerState defaultKeymap lg "/tmp/hat-messagelogspec.sock" Nothing

run :: ServerState -> [Text] -> IO [Reply]
run st = runArgv st Nothing

-- | @show-messages@'s lines without their time.
shown :: ServerState -> IO [Text]
shown st = do
    replies <- run st ["show-messages"]
    pure [ T.drop 2 (snd (T.breakOn ": " l)) | ROutput l <- replies ]

texts :: MessageLog -> [Text]
texts l = map (.text) (toList l.entries)

spec :: Spec
spec = do
    describe "addMessage" $ do
        it "keeps the newest message-limit entries" $
            texts (foldl (\l t -> addMessage 2 0 t l) emptyMessageLog ["a", "b", "c"])
                `shouldBe` ["b", "c"]
        it "keeps nothing at a limit of 0, not even the new entry" $
            texts (addMessage 0 0 "a" (addMessage 5 0 "b" emptyMessageLog))
                `shouldBe` []
        it "numbers entries on, past the ones trimmed" $
            map (.number) (toList (foldl (\l t -> addMessage 1 0 t l) emptyMessageLog ["a", "b"]).entries)
                `shouldBe` [1]

    describe "commandMessage" $ do
        it "names the client and the key that ran the command" $ do
            commandMessage (Just "client1") (Just "c") "new-window"
                `shouldBe` "client1 key c: new-window"
            commandMessage (Just "client1") Nothing "new-window"
                `shouldBe` "client1 command: new-window"
            commandMessage Nothing Nothing "new-window" `shouldBe` "command: new-window"

    describe "show-messages" $ do
        it "lists every command run, newest first, as tmux prints it" $ do
            st <- server
            [] <- run st ["set", "-g", "@x", "a b"]
            _ <- run st ["lsk", "-T", "prefix", "c"]
            shown st `shouldReturn`
                [ "command: show-messages"
                , "command: list-keys -T prefix c"
                , "command: set-option -g @x \"a b\"" ]
        it "keeps only the last message-limit messages" $ do
            st <- server
            [] <- run st ["set", "-g", "message-limit", "2"]
            [] <- run st ["set", "-g", "@x", "1"]
            shown st `shouldReturn`
                [ "command: show-messages", "command: set-option -g @x 1" ]
        it "does not log the commands of the server's own configuration" $ do
            st <- server
            atomically (writeTVar st.startupPhase LoadingConfig)
            [] <- run st ["set", "-g", "@x", "1"]
            atomically (writeTVar st.startupPhase Ready)
            shown st `shouldReturn` ["command: show-messages"]
        it "stamps each message with its time" $ do
            st <- server
            replies <- run st ["show-messages"]
            [ T.length (fst (T.breakOn ": " l)) | ROutput l <- replies ]
                `shouldSatisfy` all (> 0)
        it "fails loud on the job and terminal lists hat does not keep" $ do
            st <- server
            run st ["show-messages", "-J"]
                `shouldReturn` [RErr "show-messages -J: not supported"]
            run st ["show-messages", "-T"]
                `shouldReturn` [RErr "show-messages -T: not supported"]
