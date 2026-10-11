-- | The @history-file@: prompt history kept across server lifetimes in
-- tmux's own on-disk format, so tmux and hat can share one file. Each line
-- is @type:entry@ (@command@ or @search@); a line without a known type is a
-- command entry whole, which is how tmux reads files from before typed
-- lines. See 'parseHistory' and 'renderHistory'.
module Hat.Server.PromptHistory
    ( PromptHistory (..)
    , parseHistory
    , renderHistory
    , restoreHistory
    , historyFilePath
    , loadHistory
    , saveHistory
    ) where

import Control.Concurrent.STM
import Control.Exception (IOException, try)
import Data.ByteString qualified as B
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import System.Environment (lookupEnv)
import System.IO.Error (isDoesNotExistError)
import System.Posix.User (getRealUserID, getUserEntryForID, homeDirectory)

import Hat.Log
import Hat.Model
import Hat.Model.Options (optionNumber, optionText)
import Hat.Server.Prompt (addHistory)

-- | Each prompt type's history, most recent first.
data PromptHistory = PromptHistory
    { commands :: [Text]
    , searches :: [Text]
    }
    deriving (Eq, Show)

-- | tmux's @prompt_load_history@ over a file's text: each line joins its
-- type's history unless it repeats that history's newest entry.
parseHistory :: Text -> PromptHistory
parseHistory = List.foldl' add (PromptHistory [] []) . fileLines
  where
    fileLines t = case T.splitOn "\n" t of
        ls | Just (body, "") <- unsnoc ls -> body  -- the final newline
        ls -> ls
    unsnoc xs = case reverse xs of
        [] -> Nothing
        l : rest -> Just (reverse rest, l)
    add h line = case T.breakOn ":" line of
        ("command", rest) | not (T.null rest) ->
            h { commands = push (T.drop 1 rest) h.commands }
        ("search", rest) | not (T.null rest) ->
            h { searches = push (T.drop 1 rest) h.searches }
        _ -> h { commands = push line h.commands }
    push entry = \case
        newest : older | newest == entry -> newest : older
        hist -> entry : hist

-- | A history read from the file added to the server's, oldest entry first,
-- as tmux's @prompt_load_history@ does, under @prompt-history-limit@.
restoreHistory :: Int -> PromptHistory -> PromptHistory -> PromptHistory
restoreHistory limit loaded existing = PromptHistory
    { commands = restore loaded.commands existing.commands
    , searches = restore loaded.searches existing.searches }
  where
    restore new old = List.foldl' (flip (addHistory limit)) old (reverse new)

-- | tmux's @prompt_save_history@: every command entry, then every search
-- entry, oldest first, one @type:entry@ line each.
renderHistory :: PromptHistory -> Text
renderHistory h = T.concat
    (typed "command" h.commands <> typed "search" h.searches)
  where
    typed ty hist = [ ty <> ":" <> entry <> "\n" | entry <- reverse hist ]

-- | tmux's @prompt_find_history_file@: an absolute path as given, @~/@
-- under the home directory, anything else (empty included) no file.
historyFilePath :: Text -> IO (Maybe FilePath)
historyFilePath setting = case T.unpack setting of
    path@('/' : _) -> pure (Just path)
    '~' : '/' : rest -> fmap (<> ('/' : rest)) <$> homeDir
    _ -> pure Nothing
  where
    homeDir = lookupEnv "HOME" >>= \case
        Just h | not (null h) -> pure (Just h)
        _ -> either (const Nothing :: IOException -> Maybe FilePath)
                (Just . homeDirectory)
            <$> try (getUserEntryForID =<< getRealUserID)

-- | Read the @history-file@ into the prompt histories, as their newest
-- entries. A missing file is a fresh start; any other failure is logged and
-- leaves the histories alone.
loadHistory :: ServerState -> IO ()
loadHistory st = withHistoryFile st $ \path -> do
    r <- try (B.readFile path)
    case r of
        Left e | isDoesNotExistError e -> pure ()
               | otherwise -> fault path e
        Right bytes -> do
            let loaded = parseHistory (TE.decodeUtf8With lenientDecode bytes)
            atomically $ do
                limit <- optionNumber "prompt-history-limit" <$> readTVar st.options
                had <- PromptHistory <$> readTVar st.cmdHistory <*> readTVar st.searchHistory
                let now = restoreHistory limit loaded had
                writeTVar st.cmdHistory now.commands
                writeTVar st.searchHistory now.searches
  where
    fault path e = logEvent st.logger HistoryFileFault
        { file = path, err = T.pack (show e) }

-- | Write the prompt histories to the @history-file@, replacing it.
saveHistory :: ServerState -> IO ()
saveHistory st = withHistoryFile st $ \path -> do
    h <- PromptHistory <$> readTVarIO st.cmdHistory <*> readTVarIO st.searchHistory
    r <- try (B.writeFile path (TE.encodeUtf8 (renderHistory h)))
    case r of
        Left (e :: IOException) -> logEvent st.logger HistoryFileFault
            { file = path, err = T.pack (show e) }
        Right () -> pure ()

withHistoryFile :: ServerState -> (FilePath -> IO ()) -> IO ()
withHistoryFile st act = do
    setting <- optionText "history-file" <$> readTVarIO st.options
    mapM_ act =<< historyFilePath setting
