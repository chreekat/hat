module Hat.Server.ConfigSpec (spec) where

import Control.Concurrent.STM (readTVarIO)
import Control.Exception (bracket)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (removeDirectoryRecursive)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

import Hat.Log (newLogger)
import Hat.Model (ServerState (..), newServerState)
import Hat.Model.Options (Options (..))
import Hat.Server (cmdSourceFile, readConfigUtf8)
import Hat.Server.Environ
    (EnvEntry (..), EnvVisibility (..), environFind)

-- Run an action with @HOME@ pointed at @dir@, restoring the prior value.
withHome :: FilePath -> IO a -> IO a
withHome dir act = do
    old <- lookupEnv "HOME"
    bracket (setEnv "HOME" dir) (const (restore old)) (const act)
  where
    restore = maybe (unsetEnv "HOME") (setEnv "HOME")

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
                    removeDirectoryRecursive $ \dir -> withHome dir $ do
                writeFile (dir <> "/reload.conf")
                    "set -g status-left RELOADED\n"
                lg <- newLogger "/dev/null"
                st <- newServerState Map.empty lg (dir <> "/s.sock") Nothing
                _ <- cmdSourceFile st Nothing ["~/reload.conf"]
                opts <- readTVarIO st.options
                opts.statusLeft `shouldBe` "RELOADED"

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
