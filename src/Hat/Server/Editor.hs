-- | tmux's @spawn_editor@: some text opened in the @editor@ option's program,
-- in a modal floating pane over the client's current window, and handed back
-- once the editor ends.
module Hat.Server.Editor
    ( spawnEditor
    ) where

import Control.Concurrent (forkIOWithUnmask)
import Control.Concurrent.STM (readTVarIO)
import Control.Exception (IOException, finally, mask_, try)
import Control.Monad (unless, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Directory (removeFile)
import System.Exit (ExitCode (..))
import System.IO (hClose, openBinaryTempFile)
import System.Posix.Process (ProcessStatus (..))

import Hat.Model
import Hat.Model.Options (optionText)
import Hat.Server.Command.Types (Dispatch (..))
import Hat.Term.Pty qualified as Pty

-- | Open @text@ in an editor for @client@: @begin@ runs once it is open, and
-- @finish@ once it ends, with what it saved when it exits successfully, or
-- 'Nothing' when it fails, is killed or saves nothing. Neither runs when no
-- editor could open (the window already has a modal pane).
spawnEditor
    :: Dispatch -> ServerState -> Client -> ByteString -> (EditorWait -> IO ())
    -> (EditorWait -> Maybe ByteString -> IO ()) -> IO ()
spawnEditor d st client text begin finish = do
    editor <- optionText "editor" <$> readTVarIO st.options
    (path, h) <- openBinaryTempFile "/tmp" "hat.edit"
    B.hPut h text `finally` hClose h
    opened <- newIORef False
    let watch pane = do
            writeIORef opened True
            let wait = EditorWait
                    { editorPane = pane.id, editorPid = fromIntegral (Pty.pid pane.pty) }
            begin wait
            void $ mask_ $ forkIOWithUnmask $ \unmask ->
                unmask (awaitEdit wait pane path) `finally` removeQuietly path
    _ <- d.newEditorPane watch st (Just client)
        ["-c", "/tmp", editor <> " " <> T.pack path]
    watched <- readIORef opened
    unless watched (removeQuietly path)
  where
    awaitEdit wait pane path = do
        status <- Pty.waitExit pane.pty
        saved <- case status of
            Just (Exited ExitSuccess) -> do
                r <- try (B.readFile path)
                pure $ case r of
                    Right bytes | not (B.null bytes) -> Just bytes
                    Right _ -> Nothing
                    Left (_ :: IOException) -> Nothing
            _ -> pure Nothing
        finish wait saved
    removeQuietly path = void (try (removeFile path) :: IO (Either IOException ()))
