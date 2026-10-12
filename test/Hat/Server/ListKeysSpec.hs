module Hat.Server.ListKeysSpec (spec) where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import Hat.Log (newLogger)
import Hat.Model (ServerState, newServerState)
import Hat.Server (Reply (..))
import Hat.Server.Command.ListKeys (keyCode, printCommands)
import Hat.Server.Dispatch (runArgv)
import Hat.Server.Keymap (defaultKeymap)

server :: IO ServerState
server = do
    lg <- newLogger "/dev/null"
    newServerState defaultKeymap lg "/tmp/hat-listkeysspec.sock" Nothing

run :: ServerState -> [Text] -> IO [Reply]
run st = runArgv st Nothing

spec :: Spec
spec = do
    describe "printCommands" $ do
        it "prints each command as tmux's cmd_print: full name, flags, then values" $ do
            printCommands [["splitw", "-c", "#{pane_current_path}", "-h"]]
                `shouldBe` "split-window -h -c \"#{pane_current_path}\""
            printCommands [["command-prompt", "-I", "#W", "rename-window '%%'"]]
                `shouldBe` "command-prompt -I \"#W\" \"rename-window '%%'\""
            printCommands [["resize-pane", "-U", "5"], ["select-pane", "-dd"]]
                `shouldBe` "resize-pane -U 5 \\; select-pane -dd"
        it "keeps a command tmux does not know as written" $
            printCommands [["activity-window", "x y"]] `shouldBe` "activity-window \"x y\""

    describe "keyCode" $
        it "ranks keys as tmux's key codes do" $
            List.sortOn keyCode
                [ "C-a", "M-MouseDown3Empty", "WheelUpStatus", "MouseDown3Empty"
                , "MouseDown1Pane", "F1", "Enter", "BSpace", "~" ]
                `shouldBe`
                [ "Enter", "~", "BSpace", "F1", "MouseDown1Pane", "MouseDown3Empty"
                , "WheelUpStatus", "M-MouseDown3Empty", "C-a" ]

    describe "list-keys" $ do
        it "lists a binding in tmux's default format" $ do
            st <- server
            run st ["list-keys", "-T", "prefix", "c"]
                `shouldReturn` [ROutput "bind-key  -T prefix c new-window"]
        it "pads the table and key columns across the listed bindings" $ do
            st <- server
            [] <- run st ["bind-key", "-T", "t", "Up", "kill-pane"]
            [] <- run st ["bind-key", "-T", "t", "a", "kill-window"]
            run st ["list-keys", "-T", "t"] `shouldReturn`
                [ ROutput "bind-key  -T t a  kill-window"
                , ROutput "bind-key  -T t Up kill-pane" ]
        it "orders keys by tmux's key code within a table" $ do
            st <- server
            mapM_ (\k -> run st ["bind-key", "-T", "t", k, "kill-pane"])
                ["C-a", "M-a", "Up", "z", "a"]
            run st ["list-keys", "-T", "t", "-F", "#{key_string}"] `shouldReturn`
                map ROutput ["a", "z", "Up", "M-a", "C-a"]
        it "-1 lists only the first binding" $ do
            st <- server
            [] <- run st ["bind-key", "-T", "t", "a", "kill-window"]
            [] <- run st ["bind-key", "-T", "t", "b", "kill-pane"]
            run st ["list-keys", "-1", "-T", "t", "-F", "#{key_command}"]
                `shouldReturn` [ROutput "kill-window"]
        it "-P sets #{key_prefix}" $ do
            st <- server
            run st ["list-keys", "-T", "prefix", "-P", "PFX", "-F", "#{key_prefix}", "c"]
                `shouldReturn` [ROutput "PFX"]
            run st ["list-keys", "-T", "prefix", "-F", "#{key_prefix}", "c"]
                `shouldReturn` [ROutput "C-b"]
        it "fails like tmux on a bad table, key, or order" $ do
            st <- server
            run st ["list-keys", "-T", "nope"]
                `shouldReturn` [RErr "table nope doesn't exist"]
            run st ["list-keys", "-T", "prefix", "F9"]
                `shouldReturn` [RErr "unknown key: F9"]
            run st ["list-keys", "NotAKey"]
                `shouldReturn` [RErr "invalid key: NotAKey"]
            run st ["list-keys", "-O", "sideways"]
                `shouldReturn` [RErr "invalid sort order"]
        it "#{key_note} is the note bind -N gave the key" $ do
            st <- server
            [] <- run st ["bind-key", "-N", "Kill it", "-T", "t", "a", "kill-pane"]
            [] <- run st ["bind-key", "-T", "t", "b", "kill-pane"]
            run st ["list-keys", "-T", "t", "-F", "#{key_string}=#{key_note}"]
                `shouldReturn` map ROutput ["a=Kill it", "b="]
        it "bind -N without a command re-notes an existing binding, binds nothing new" $ do
            st <- server
            [] <- run st ["bind-key", "-T", "t", "a", "kill-pane"]
            [] <- run st ["bind-key", "-N", "Later note", "-T", "t", "a"]
            [] <- run st ["bind-key", "-N", "Unbound", "-T", "t", "b"]
            run st ["list-keys", "-T", "t", "-F", "#{key_string}=#{key_note}=#{key_command}"]
                `shouldReturn` [ROutput "a=Later note=kill-pane"]
        it "-N lists only noted bindings, the prefix table's then the root's" $ do
            st <- server
            [] <- run st ["bind-key", "-n", "-N", "Root note", "F1", "kill-pane"]
            [] <- run st ["bind-key", "-N", "Prefix note", "F2", "kill-pane"]
            [] <- run st ["bind-key", "-T", "t", "-N", "Other table", "F3", "kill-pane"]
            [] <- run st ["bind-key", "F4", "kill-pane"]
            out <- run st ["list-keys", "-N", "-F", "#{key_table} #{key_string} #{key_note}"]
            filter (`elem` map ROutput ["prefix F2 Prefix note", "root F1 Root note"]) out
                `shouldBe` map ROutput ["prefix F2 Prefix note", "root F1 Root note"]
            [ l | ROutput l <- out, any (`T.isInfixOf` l) ["F3", "F4"] ] `shouldBe` []
            [ l | ROutput l <- out, T.null (T.takeWhileEnd (/= ' ') l) ] `shouldBe` []
        it "-N -a lists un-noted bindings by their command; -P replaces the prefix" $ do
            st <- server
            [] <- run st ["bind-key", "-N", "Kill it", "-T", "t", "a", "kill-pane"]
            [] <- run st ["bind-key", "-T", "t", "b", "kill-window"]
            run st ["list-keys", "-N", "-T", "t"]
                `shouldReturn` [ROutput "C-b a Kill it"]
            run st ["list-keys", "-aN", "-P", ">", "-T", "t"]
                `shouldReturn` [ROutput "> a Kill it", ROutput "> b kill-window"]
        it "the default prefix bindings carry tmux's notes" $ do
            st <- server
            run st ["list-keys", "-1N", "c"]
                `shouldReturn` [ROutput "C-b c Create a new window"]
