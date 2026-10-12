module Hat.Server.PromptHistorySpec (spec) where

import Control.Concurrent.STM
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import Test.Hspec

import Hat.Log (newLogger)
import Hat.Model
import Hat.Model.Options (OptionValue (..), singletonDelta)
import Hat.Server.Command.Option (refreshGlobalOptions)
import Hat.Server.PromptHistory

spec :: Spec
spec = do
    -- The history file's version corpus: each case is a file as some tmux
    -- (or hat) wrote it, pinned to what the current reader makes of it.
    -- Append a case for every new shape; never edit one.
    describe "reading history files" $ do
        it "reads typed lines into their own histories" $
            parseHistory "command:new-window\ncommand:kill-pane\nsearch:foo\n"
                `shouldBe` PromptHistory
                    { commands = ["kill-pane", "new-window"], searches = ["foo"] }
        it "reads a pre-typed file's lines as commands" $
            parseHistory "split-window -h\nselect-layout tiled\n"
                `shouldBe` PromptHistory
                    { commands = ["select-layout tiled", "split-window -h"]
                    , searches = [] }
        it "keeps an unknown type's line whole, as a command" $
            parseHistory "target:%1\ncommand:a:b\n"
                `shouldBe` PromptHistory
                    { commands = ["a:b", "target:%1"], searches = [] }
        it "drops a repeat of its history's newest entry" $
            parseHistory "command:ls\ncommand:ls\nsearch:x\ncommand:ls\n"
                `shouldBe` PromptHistory { commands = ["ls"], searches = ["x"] }
        it "reads a last line without a newline, and empty lines as entries" $
            parseHistory "command:a\n\ncommand:b"
                `shouldBe` PromptHistory { commands = ["b", "", "a"], searches = [] }

    describe "writing history files" $
        it "writes commands then searches, oldest first, in tmux's form" $
            renderHistory PromptHistory
                { commands = ["kill-pane", "new-window"], searches = ["foo"] }
                `shouldBe` "command:new-window\ncommand:kill-pane\nsearch:foo\n"

    describe "restoring a loaded history (prompt-history-limit)" $ do
        let loaded = PromptHistory { commands = ["c", "b", "a"], searches = ["s"] }
            none = PromptHistory [] []
        it "keeps the newest entries up to the limit" $
            restoreHistory 2 loaded none
                `shouldBe` PromptHistory { commands = ["c", "b"], searches = ["s"] }
        it "loads over what the server already has, as newer entries" $
            restoreHistory 100 loaded PromptHistory { commands = ["z"], searches = [] }
                `shouldBe` PromptHistory { commands = ["c", "b", "a", "z"], searches = ["s"] }
        it "keeps nothing under a limit of 0" $
            restoreHistory 0 loaded none `shouldBe` none

    describe "historyFilePath" $
        it "takes an absolute path, ~/ under the home, and nothing relative or empty" $ do
            historyFilePath (Just "/home/b") "/var/h" `shouldBe` Just "/var/h"
            historyFilePath (Just "/home/b") "~/h" `shouldBe` Just "/home/b/h"
            historyFilePath Nothing "~/h" `shouldBe` Nothing
            historyFilePath (Just "/home/b") "rel/h" `shouldBe` Nothing
            historyFilePath (Just "/home/b") "" `shouldBe` Nothing

    describe "history-file round trip" $
        it "saves the prompt histories and loads them back" $ do
            tmp <- getTemporaryDirectory
            (path, h) <- openTempFile tmp "hat-history"
            hClose h
            let withHistoryAt body = do
                    lg <- newLogger "/dev/null"
                    st <- newServerState Map.empty lg "/tmp/hat-history.sock" Nothing
                    atomically $ do
                        writeTVar st.serverOptions (singletonDelta "history-file"
                            (OVScalar (T.pack path)))
                        refreshGlobalOptions st
                    body st
            withHistoryAt $ \st -> do
                atomically $ do
                    writeTVar st.cmdHistory ["b", "a"]
                    writeTVar st.searchHistory ["s"]
                saveHistory st
            withHistoryAt $ \st -> do
                loadHistory st
                readTVarIO st.cmdHistory `shouldReturn` ["b", "a"]
                readTVarIO st.searchHistory `shouldReturn` ["s"]
            removeFile path
