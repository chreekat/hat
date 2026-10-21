module Hat.Server.ConfigSpec (spec) where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Exception (bracket)
import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import System.Directory (removeDirectoryRecursive)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

import Hat.Log (newLogger)
import Hat.Geometry (Size (..))
import Hat.Model (Client, ServerState (..), newServerState)
import Hat.Model.Options (Options (..), optionText)
import Hat.Server (cmdSourceFile, readConfigUtf8)
import Hat.Server.Command.Types (Reply (..))
import Hat.Server.Conn (newClient)
import Hat.Server.Dispatch (FileLoad (..), loadCommandFile, runCommandText)
import Hat.Server.Keys (Binding (..))
import Hat.Transport.Wire (ArgvBytes (..), Hello (..), Intent (..), protocolVersion)
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), defaultProtocol, socketPair)
import Hat.Server.Environ
    (EnvEntry (..), EnvVisibility (..), environFind)

-- | A fresh server whose files live in a fresh directory.
withDir :: String -> (FilePath -> ServerState -> IO a) -> IO a
withDir prefix act = bracket (mkdtemp prefix) removeDirectoryRecursive $ \dir -> do
    lg <- newLogger "/dev/null"
    st <- newServerState Map.empty lg (dir <> "/s.sock") Nothing
    act dir st

-- | A command client (as @hat <command>@ connects), never driven.
commandClient :: ServerState -> IO (Client, Socket)
commandClient st = do
    (server, peer) <- socketPair AF_UNIX Stream defaultProtocol
    c <- newClient st server Hello
        { protoVersion = protocolVersion, term = "", env = [], size = Size 24 80
        , cwd = "/", intent = ControlIntent, autostarted = False, sessionHist = [], termCaps = Nothing
        , argvBytes = TextIsExact }
    pure (c, peer)

spec :: Spec
spec = do
    -- The user's config threw mid-read under a non-UTF-8 locale because
    -- TIO.readFile decodes with the locale; a config with a `·` (U+00B7,
    -- bytes C2 B7) aborted startup. readConfigUtf8 must decode UTF-8
    -- regardless of locale and never throw on a malformed byte.
    describe "readConfigUtf8" $
        it "decodes UTF-8 locale-independently and survives bad bytes" $ do
            bracket (mkdtemp "/tmp/hat-config-")
                    removeDirectoryRecursive $ \dir -> do
                let p = dir <> "/c"
                -- ".·" then a lone 0xFF (invalid UTF-8) then newline.
                B.writeFile p (B.pack [0x2e, 0xc2, 0xb7, 0xff, 0x0a])
                t <- readConfigUtf8 p
                -- the middle dot round-trips …
                t `shouldSatisfy` T.isInfixOf (T.singleton '\x00b7')
                -- … and the invalid byte is replaced, not thrown on.
                t `shouldSatisfy` T.isInfixOf (T.singleton '\xfffd')

    -- The reload binding `bind r source-file ~/.tmux.conf` looked like it
    -- worked (the paired display-message toast fired) but the status-bar
    -- change never took effect: source-file passed the literal `~/…` to
    -- doesFileExist, which is always False, so the file was never read.
    describe "source-file" $
        it "expands a ~/ path so a reload actually re-reads the file" $ do
            bracket (mkdtemp "/tmp/hat-source-")
                    removeDirectoryRecursive $ \dir -> do
                writeFile (dir <> "/reload.conf")
                    "set -g status-left RELOADED\n"
                lg <- newLogger "/dev/null"
                st0 <- newServerState Map.empty lg (dir <> "/s.sock") Nothing
                let st = st0 { home = Just dir }
                _ <- cmdSourceFile st Nothing ["~/reload.conf"]
                opts <- readTVarIO st.options
                optionText "status-left" opts `shouldBe` "RELOADED"

    describe "files of commands" $ do
        it "run only whole: a command no name resolves stops the file, as a cause" $
            withDir "/tmp/hat-cause-" $ \dir st -> do
                let p = dir <> "/c.conf"
                loadCommandFile st Nothing RunFile p "set -g @a 1\nwibble wobble\n"
                    `shouldReturn` [RExitStatus 1]
                opts <- readTVarIO st.options
                Map.lookup "@a" opts.user `shouldBe` Nothing
                readTVarIO st.configCauses `shouldReturn` [T.pack p <> ":2: unknown command: wibble"]
        it "print their causes to the client that sourced them, and fail it" $
            withDir "/tmp/hat-cause-" $ \dir st -> do
                let p = dir <> "/c.conf"
                writeFile p "wibble\n"
                (c, _) <- commandClient st
                cmdSourceFile st (Just c) [T.pack p]
                    `shouldReturn`
                        [RExitStatus 1, ROutput (T.pack p <> ":1: unknown command: wibble")]
                readTVarIO st.configCauses `shouldReturn` []
        it "make a clientless command's errors causes, a nested command's naming its line" $
            withDir "/tmp/hat-cause-" $ \dir st -> do
                let p = dir <> "/c.conf"
                _ <- loadCommandFile st Nothing RunFile p "set -g @a 1\nif -F 1 'wibble x'\n"
                opts <- readTVarIO st.options
                Map.lookup "@a" opts.user `shouldBe` Just "1"
                readTVarIO st.configCauses
                    `shouldReturn` [T.pack p <> ":2: " <> T.pack p <> ":2: unknown command: wibble"]
        it "report a missing file as tmux does, -q quietly" $
            withDir "/tmp/hat-cause-" $ \dir st -> do
                cmdSourceFile st Nothing [T.pack (dir <> "/none")]
                    `shouldReturn` [RErr (T.pack ("No such file or directory: " <> dir <> "/none"))]
                cmdSourceFile st Nothing ["-q", T.pack (dir <> "/none")] `shouldReturn` []
        it "glob their paths" $
            withDir "/tmp/hat-cause-" $ \dir st -> do
                writeFile (dir <> "/a.conf") "set -g @a 1\n"
                writeFile (dir <> "/b.conf") "set -g @b 2\n"
                cmdSourceFile st Nothing [T.pack (dir <> "/*.conf")] `shouldReturn` []
                opts <- readTVarIO st.options
                map (`Map.lookup` opts.user) ["@a", "@b"] `shouldBe` [Just "1", Just "2"]

    describe "conditionals" $ do
        it "run only the branch taken; a branch not taken never fails the file" $
            withDir "/tmp/hat-cond-" $ \dir st -> do
                let p = dir <> "/c.conf"
                loadCommandFile st Nothing RunFile p
                    "%if #{@unset}\nwibble\n%else\nset -g @a 1\n%endif\n%if 1 set -g @b 2 %endif\n"
                    `shouldReturn` []
                opts <- readTVarIO st.options
                map (`Map.lookup` opts.user) ["@a", "@b"] `shouldBe` [Just "1", Just "2"]
                readTVarIO st.configCauses `shouldReturn` []
        it "expand in the global context before any of the file runs" $
            withDir "/tmp/hat-cond-" $ \dir st -> do
                let p = dir <> "/c.conf"
                _ <- loadCommandFile st Nothing RunFile p "set -g @c 1\n"
                loadCommandFile st Nothing RunFile p (T.unlines
                    [ "set -g @d 1", "%if #{@c}", "set -g @a 1", "%endif"
                    , "%if #{@d}", "set -g @b 1", "%endif" ])
                    `shouldReturn` []
                opts <- readTVarIO st.options
                map (`Map.lookup` opts.user) ["@a", "@b"] `shouldBe` [Just "1", Nothing]
        it "expand without jobs: none starts, and a cached one's output is unseen" $
            withDir "/tmp/hat-cond-" $ \dir st -> do
                let p = dir <> "/c.conf"
                now <- getCurrentTime
                atomically (writeTVar st.shellCache (Map.singleton "echo 1" (now, "1")))
                loadCommandFile st Nothing RunFile p (T.unlines
                    [ "%if \"#(echo 1)\"", "set -g @e 1", "%endif"
                    , "%if \"#(echo 2)\"", "set -g @f 1", "%endif" ])
                    `shouldReturn` []
                Map.lookup "@e" . (.user) <$> readTVarIO st.options `shouldReturn` Nothing
                Map.keys <$> readTVarIO st.shellCache `shouldReturn` ["echo 1"]
        it "see the assignments made above them, which a failed file keeps" $
            withDir "/tmp/hat-cond-" $ \dir st -> do
                let p = dir <> "/c.conf"
                loadCommandFile st Nothing RunFile p
                    "CONDX=yes\n%if #{CONDX}\nset -g @a 1\n%endif\n%if 1\n%endif\n"
                    `shouldReturn` [RExitStatus 1]
                readTVarIO st.configCauses `shouldReturn` [T.pack p <> ":6: syntax error"]
                env <- readTVarIO st.globalEnviron
                environFind "CONDX" env `shouldBe` Just (EnvEntry (Just "yes") EnvVisible)
                opts <- readTVarIO st.options
                Map.lookup "@a" opts.user `shouldBe` Nothing
        it "only check a file under source-file -n, assigning nothing" $
            withDir "/tmp/hat-cond-" $ \dir st -> do
                let p = dir <> "/c.conf"
                loadCommandFile st Nothing ParseOnly p "CONDY=1\n%if #{CONDY}\nwibble\n%endif\n"
                    `shouldReturn` []
                environFind "CONDY" <$> readTVarIO st.globalEnviron `shouldReturn` Nothing
        it "choose a key binding's commands when it is bound" $
            withDir "/tmp/hat-cond-" $ \dir st -> do
                let p = dir <> "/c.conf"
                loadCommandFile st Nothing RunFile p
                    "bind x {\n%if #{@unset}\nset -g @a 1\n%else\nset -g @a 2\n%endif\n}\n"
                    `shouldReturn` []
                km <- readTVarIO st.keymap
                ((.commands) <$> (Map.lookup "prefix" km >>= Map.lookup "x"))
                    `shouldBe` Just [["set", "-g", "@a", "2"]]

    describe "command strings" $ do
        it "parse as files do: conditionals, assignments, a line per command" $
            withDir "/tmp/hat-str-" $ \_ st -> do
                runCommandText st Nothing (T.unlines
                    [ "STRX=1", "%if #{@unset}", "set -g @a 1", "%elif #{STRX}", "set -g @a 2"
                    , "%endif", "set -g @b 1 ; %if 1 set -g @c 1 %endif", "set -g @d 1" ])
                    `shouldReturn` []
                opts <- readTVarIO st.options
                map (`Map.lookup` opts.user) ["@a", "@b", "@c", "@d"]
                    `shouldBe` map Just ["2", "1", "1", "1"]
                environFind "STRX" <$> readTVarIO st.globalEnviron
                    `shouldReturn` Just (EnvEntry (Just "1") EnvVisible)
        it "take a # that starts a word as a comment, to the end of its line" $
            withDir "/tmp/hat-str-" $ \_ st -> do
                runCommandText st Nothing
                    "set -g @a x#y ; set -g @b '#y' ; set -g @c \"#{l:z}\" ;#; set -g @d w\nset -g @e v"
                    `shouldReturn` []
                opts <- readTVarIO st.options
                map (`Map.lookup` opts.user) ["@a", "@b", "@c", "@d", "@e"]
                    `shouldBe` [Just "x#y", Just "#y", Just "#{l:z}", Nothing, Just "v"]
        it "fail at their own line, counted from their command's" $
            withDir "/tmp/hat-str-" $ \dir st -> do
                let p = dir <> "/c.conf"
                _ <- loadCommandFile st Nothing RunFile p
                    "set -g @a 1\nif -F 1 'set -g @b 1\n%foo'\nif -F 1 'set -g @c 1\nwibble'\n"
                readTVarIO st.configCauses `shouldReturn`
                    [ T.pack p <> ":2: " <> T.pack p <> ":3: syntax error"
                    , T.pack p <> ":4: " <> T.pack p <> ":5: unknown command: wibble" ]
                opts <- readTVarIO st.options
                map (`Map.lookup` opts.user) ["@a", "@b", "@c"]
                    `shouldBe` [Just "1", Nothing, Nothing]

    -- tmux's config assignment forms (environ_put): a bare NAME=value line
    -- sets a global environment variable, %hidden NAME=value a hidden one.
    describe "config assignment forms" $
        it "NAME=value and %hidden NAME=value set global variables" $ do
            bracket (mkdtemp "/tmp/hat-envconf-")
                    removeDirectoryRecursive $ \dir -> do
                writeFile (dir <> "/env.conf")
                    "CFGVAR=fromconfig\n%hidden CFGHID=hiddencfg\n"
                lg <- newLogger "/dev/null"
                st <- newServerState Map.empty lg (dir <> "/s.sock") Nothing
                _ <- cmdSourceFile st Nothing [T.pack (dir <> "/env.conf")]
                env <- readTVarIO st.globalEnviron
                environFind "CFGVAR" env
                    `shouldBe` Just (EnvEntry (Just "fromconfig") EnvVisible)
                environFind "CFGHID" env
                    `shouldBe` Just (EnvEntry (Just "hiddencfg") EnvHidden)
