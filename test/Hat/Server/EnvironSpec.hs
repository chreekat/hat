-- | The environment engine: entry states (valued, cleared, hidden), the
-- show-environment output forms, glob matching, scope merging, and the
-- set-environment/show-environment commands over both scopes.
module Hat.Server.EnvironSpec (spec) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

import Hat.Geometry (Size (..))
import Hat.Log (newLogger)
import Hat.Model
import Hat.Model.Options (emptyDelta)
import Hat.Server (Reply (..), cmdSetEnvironment, cmdShowEnvironment)
import Hat.Server.Environ

-- A server with one session named @s@, enough for both environment scopes.
seedServer :: IO (ServerState, Session)
seedServer = do
    lg <- newLogger "/dev/null"
    st <- newServerState Map.empty lg "/tmp/hat-environspec.sock" Nothing
    sess <- Session (SessionId 0)
        <$> newTVarIO "s"
        <*> newTVarIO Map.empty
        <*> newTVarIO 0
        <*> newTVarIO []
        <*> newTVarIO (Size { rows = 24, cols = 80 })
        <*> newTVarIO emptyEnviron
        <*> newTVarIO "/"
        <*> newTVarIO emptyDelta
    atomically $ modifyTVar' st.sessions (Map.insert (SessionId 0) sess)
    pure (st, sess)

setEnv :: ServerState -> [Text] -> IO [Reply]
setEnv st = cmdSetEnvironment st Nothing

showEnv :: ServerState -> [Text] -> IO [Reply]
showEnv st = cmdShowEnvironment st Nothing

spec :: Spec
spec = do
    describe "set-environment / show-environment" $ do
        it "sets and shows a variable at session scope" $ do
            (st, _) <- seedServer
            setEnv st ["FOO", "bar"] `shouldReturn` []
            showEnv st ["FOO"] `shouldReturn` [ROutput "FOO=bar"]

        it "overwrites on a second set" $ do
            (st, _) <- seedServer
            _ <- setEnv st ["FOO", "bar"]
            _ <- setEnv st ["FOO", "baz"]
            showEnv st ["FOO"] `shouldReturn` [ROutput "FOO=baz"]

        it "keeps the global scope separate from the session's" $ do
            (st, _) <- seedServer
            _ <- setEnv st ["FOO", "bar"]
            setEnv st ["-g", "GVAR", "gval"] `shouldReturn` []
            showEnv st ["-g", "GVAR"] `shouldReturn` [ROutput "GVAR=gval"]
            showEnv st ["-g", "FOO"]
                `shouldReturn` [RErr "unknown variable: FOO"]
            showEnv st ["GVAR"]
                `shouldReturn` [RErr "unknown variable: GVAR"]

        it "-F expands the value against the target session" $ do
            (st, _) <- seedServer
            setEnv st ["-t", "s", "-F", "EXP", "#{session_name}"]
                `shouldReturn` []
            showEnv st ["EXP"] `shouldReturn` [ROutput "EXP=s"]

        it "-h hides a variable from plain show; -h shows only hidden" $ do
            (st, _) <- seedServer
            _ <- setEnv st ["-h", "SECRET", "s3cr"]
            _ <- setEnv st ["FOO", "bar"]
            showEnv st ["SECRET"] `shouldReturn` []
            showEnv st ["-h", "SECRET"] `shouldReturn` [ROutput "SECRET=s3cr"]
            showEnv st ["-h", "FOO"] `shouldReturn` []

        it "-r clears: plain shows -NAME, shell form unset NAME;" $ do
            (st, _) <- seedServer
            _ <- setEnv st ["FOO", "bar"]
            setEnv st ["-r", "FOO"] `shouldReturn` []
            showEnv st ["FOO"] `shouldReturn` [ROutput "-FOO"]
            showEnv st ["-s", "FOO"] `shouldReturn` [ROutput "unset FOO;"]

        it "-u removes the variable entirely" $ do
            (st, _) <- seedServer
            _ <- setEnv st ["FOO", "bar"]
            setEnv st ["-u", "FOO"] `shouldReturn` []
            showEnv st ["FOO"]
                `shouldReturn` [RErr "unknown variable: FOO"]

        it "a bare show lists every non-hidden variable, sorted" $ do
            (st, _) <- seedServer
            _ <- setEnv st ["-g", "LISTB", "2"]
            _ <- setEnv st ["-g", "LISTA", "1"]
            _ <- setEnv st ["-gh", "LISTHID", "3"]
            showEnv st ["-g"]
                `shouldReturn` [ROutput "LISTA=1", ROutput "LISTB=2"]
            showEnv st ["-gh"] `shouldReturn` [ROutput "LISTHID=3"]

        it "rejects bad arguments with tmux's errors" $ do
            (st, _) <- seedServer
            setEnv st ["", "x"] `shouldReturn` [RErr "empty variable name"]
            setEnv st ["A=B", "x"]
                `shouldReturn` [RErr "variable name contains ="]
            setEnv st ["-u", "FOO", "val"]
                `shouldReturn` [RErr "can't specify a value with -u"]
            setEnv st ["-r", "FOO", "val"]
                `shouldReturn` [RErr "can't specify a value with -r"]
            setEnv st ["NOVAL"] `shouldReturn` [RErr "no value specified"]
            showEnv st ["MISSING"]
                `shouldReturn` [RErr "unknown variable: MISSING"]

        it "an unresolvable target names the session in the error" $ do
            (st, _) <- seedServer
            showEnv st ["-t", "nosuch", "FOO"]
                `shouldReturn` [RErr "no such session: nosuch"]
            setEnv st ["-t", "nosuch", "FOO", "bar"]
                `shouldReturn` [RErr "no such session: nosuch"]
    describe "entry operations" $ do
        it "set then find returns the value" $ do
            let env = environSet EnvVisible "FOO" "bar" emptyEnviron
            environFind "FOO" env
                `shouldBe` Just (EnvEntry (Just "bar") EnvVisible)

        it "set overwrites value and visibility" $ do
            let env = environSet EnvVisible "FOO" "baz"
                    (environSet EnvHidden "FOO" "bar" emptyEnviron)
            environFind "FOO" env
                `shouldBe` Just (EnvEntry (Just "baz") EnvVisible)

        it "clear keeps the entry but drops the value" $ do
            let env = environClear "FOO"
                    (environSet EnvVisible "FOO" "bar" emptyEnviron)
            environFind "FOO" env
                `shouldBe` Just (EnvEntry Nothing EnvVisible)

        it "clear preserves a hidden entry's visibility" $ do
            let env = environClear "SEC"
                    (environSet EnvHidden "SEC" "s" emptyEnviron)
            environFind "SEC" env
                `shouldBe` Just (EnvEntry Nothing EnvHidden)

        it "clear of an absent name creates a cleared entry" $
            environFind "GONE" (environClear "GONE" emptyEnviron)
                `shouldBe` Just (EnvEntry Nothing EnvVisible)

        it "unset removes the entry entirely" $ do
            let env = environUnset "FOO"
                    (environSet EnvVisible "FOO" "bar" emptyEnviron)
            environFind "FOO" env `shouldBe` Nothing

    describe "spawn-facing pairs" $ do
        it "drops hidden and cleared entries" $ do
            let env = environSet EnvVisible "A" "1"
                    . environSet EnvHidden "SEC" "s"
                    . environClear "GONE"
                    $ environFromPairs [("B", "2")]
            environPairs env `shouldBe` [("A", "1"), ("B", "2")]

        it "a cleared session entry masks the global variable" $ do
            let global = environFromPairs [("KEEP", "g"), ("MASKED", "g")]
                sess = environClear "MASKED" emptyEnviron
            environPairs (environMerge global sess)
                `shouldBe` [("KEEP", "g")]

        it "a session value shadows the global one" $ do
            let global = environFromPairs [("X", "old")]
                sess = environFromPairs [("X", "new")]
            environPairs (environMerge global sess)
                `shouldBe` [("X", "new")]

    describe "renderEnvLine" $ do
        it "plain: NAME=value" $
            renderEnvLine EnvPlain "FOO" (EnvEntry (Just "bar") EnvVisible)
                `shouldBe` "FOO=bar"
        it "plain: a cleared entry prints -NAME" $
            renderEnvLine EnvPlain "FOO" (EnvEntry Nothing EnvVisible)
                `shouldBe` "-FOO"
        it "shell: quoted assignment plus export" $
            renderEnvLine EnvShellExport "FOO" (EnvEntry (Just "bar") EnvVisible)
                `shouldBe` "FOO=\"bar\"; export FOO;"
        it "shell: escapes $ ` \" and backslash" $
            renderEnvLine EnvShellExport "ESC"
                (EnvEntry (Just "a$b`c\"d\\e") EnvVisible)
                `shouldBe` "ESC=\"a\\$b\\`c\\\"d\\\\e\"; export ESC;"
        it "shell: a cleared entry prints unset NAME;" $
            renderEnvLine EnvShellExport "FOO" (EnvEntry Nothing EnvVisible)
                `shouldBe` "unset FOO;"

    describe "globMatch" $ do
        it "a literal pattern matches only itself" $ do
            globMatch "MYVAR" "MYVAR" `shouldBe` True
            globMatch "MYVAR" "MYVAR2" `shouldBe` False
        it "* matches any run including empty" $ do
            globMatch "TEST_*" "TEST_GLOB" `shouldBe` True
            globMatch "TEST_*" "TEST_" `shouldBe` True
            globMatch "TEST_*" "OTHER" `shouldBe` False
        it "? matches exactly one character" $ do
            globMatch "A?C" "ABC" `shouldBe` True
            globMatch "A?C" "AC" `shouldBe` False
