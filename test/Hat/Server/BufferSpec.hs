-- | The paste-buffer commands ('Hat.Server.Command.Buffer') against a real
-- server state and real temp files.
module Hat.Server.BufferSpec (spec) where

import Control.Exception (bracket)
import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (removeDirectoryRecursive)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

import Hat.Log (newLogger)
import Hat.Model (ServerState, newServerState)
import Hat.Server.Command.Buffer
import Hat.Server.Command.Types (Reply (..))

spec :: Spec
spec = do
    describe "save-buffer" $
        it "writes the top buffer to the given path, and -a appends" $
            withState $ \dir st -> do
                let path = dir <> "/out.txt"
                [] <- cmdSetBuffer st Nothing ["alpha"]
                [] <- cmdSaveBuffer st Nothing [T.pack path]
                readFile path `shouldReturn` "alpha"
                -- a fresh set-buffer pushes a new top buffer; -a appends it.
                [] <- cmdSetBuffer st Nothing ["beta"]
                [] <- cmdSaveBuffer st Nothing ["-a", T.pack path]
                readFile path `shouldReturn` "alphabeta"

    describe "load-buffer" $ do
        it "round-trips arbitrary bytes through save-buffer" $
            withState $ \dir st -> do
                let bytes = B.pack [0x6c, 0x09, 0x1b, 0x01, 0xc3, 0xa9, 0xff, 0x0a]
                    src = dir <> "/in"
                    dst = dir <> "/out"
                B.writeFile src bytes
                [] <- cmdLoadBuffer st Nothing ["-b", "file", T.pack src]
                [] <- cmdSaveBuffer st Nothing ["-b", "file", T.pack dst]
                B.readFile dst `shouldReturn` bytes
        it "reports a missing file with its errno text" $
            withState $ \dir st ->
                cmdLoadBuffer st Nothing [T.pack (dir <> "/nosuch")]
                    `shouldReturn`
                        [RErr ("No such file or directory: " <> T.pack dir <> "/nosuch")]

    describe "set-buffer" $ do
        it "reports an unknown rename source and missing data" $
            withState $ \_ st -> do
                cmdSetBuffer st Nothing ["-b", "nosuch", "-n", "foo"]
                    `shouldReturn` [RErr "unknown buffer: nosuch"]
                cmdSetBuffer st Nothing ["-b", "other"]
                    `shouldReturn` [RErr "no data specified"]
        it "rejects -w loudly" $
            withState $ \_ st ->
                cmdSetBuffer st Nothing ["-w", "x"] `shouldNotReturn` []

    describe "list-buffers" $ do
        it "lists most recent first in the default format" $
            withState $ \_ st -> do
                [] <- cmdSetBuffer st Nothing ["one"]
                [] <- cmdSetBuffer st Nothing ["a\tb"]
                cmdListBuffers st Nothing [] `shouldReturn`
                    [ ROutput "buffer1: 3 bytes: \"a\\tb\""
                    , ROutput "buffer0: 3 bytes: \"one\"" ]
        it "filters with -f and shapes with -F" $
            withState $ \_ st -> do
                [] <- cmdSetBuffer st Nothing ["-b", "other", "x"]
                [] <- cmdSetBuffer st Nothing ["y"]
                cmdListBuffers st Nothing
                    ["-f", "#{==:#{buffer_name},other}", "-F", "#{buffer_name}=#{buffer_size}"]
                    `shouldReturn` [ROutput "other=1"]
        it "sorts by name with -O and reverses with -r" $
            withState $ \_ st -> do
                [] <- cmdSetBuffer st Nothing ["-b", "b", "x"]
                [] <- cmdSetBuffer st Nothing ["-b", "a", "x"]
                [] <- cmdSetBuffer st Nothing ["-b", "c", "x"]
                cmdListBuffers st Nothing ["-O", "name", "-r", "-F", "#{buffer_name}"]
                    `shouldReturn` map ROutput ["c", "b", "a"]
                cmdListBuffers st Nothing ["-O", "bogus"]
                    `shouldReturn` [RErr "invalid sort order"]
  where
    withState :: (FilePath -> ServerState -> IO a) -> IO a
    withState act =
        bracket (mkdtemp "/tmp/hat-buffer-") removeDirectoryRecursive $ \dir -> do
            lg <- newLogger "/dev/null"
            st <- newServerState Map.empty lg "/tmp/hat-bufferspec.sock" Nothing
            act dir st
