-- | The paste-buffer commands (@set@\/@delete@\/@show@\/@list@\/@load@\/
-- @save@\/@paste-buffer@), tmux's @cmd-*-buffer.c@ over the store in
-- 'Hat.Model.PasteBuffers'; and @pipe-pane@, forwarding a pane's output to
-- an external process.
module Hat.Server.Command.Buffer
    ( cmdShowBuffer
    , cmdSetBuffer
    , cmdDeleteBuffer
    , cmdListBuffers
    , cmdLoadBuffer
    , cmdSaveBuffer
    , cmdPasteBuffer
    , cmdPipePane
    , storeBuffer
    , readClientStdin
    , bufferEnv
    , formatTrue
    ) where

import Control.Concurrent.STM
import Control.Exception (IOException, try)
import Control.Monad (filterM, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.IO.Exception (IOException (..))
import System.FilePath (isAbsolute, (</>))

import Hat.Path (expandTilde)
import Hat.Model
import Hat.Model.Options (optionNumber)
import Hat.Model.PasteBuffers
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import Hat.Server.Format (FormatEnv)
import Hat.Server.Hooks (notifyPasteChanges)
import Hat.Server.FormatEnv (expandFormat, paneFormatEnv)
import Hat.Server.Locate (findTarget, paneIndexOf, targetPane)
import Hat.Server.Pane (OutputTap (..), StdinFeed (..), startPipe, stopPipe, typeIntoPane)
import Hat.Server.Target qualified as Target
import Hat.Term.Emulator qualified as Emu

-- | @set-buffer [-aw] [-b name] [-n new-name] [-t client] [data]@ and
-- @delete-buffer [-b name]@ (tmux's @cmd-set-buffer.c@): without @-b@,
-- @-n@ and @delete-buffer@ act on the most recent automatic buffer.
cmdSetBuffer :: CommandImpl
cmdSetBuffer st _ args = withFlags "set-buffer" "aw" flags $
    if "-w" `elem` flags
        then pure [RErr "set-buffer -w: setting the clipboard is not implemented"]
        else case (lookup "-n" opts, pos) of
            (Just new, _) -> modifyStore st $ \s -> do
                old <- named s
                pasteRename old new s
            (Nothing, [bytes]) -> do
                now <- currentTime
                modifyStoreLimited st $ \limit s ->
                    let prev = case (appending, mname >>= (`pasteGet` s)) of
                            (True, Just pb) -> pb.body
                            _ -> B.empty
                        new = TE.encodeUtf8 bytes
                    in if B.null new
                        then Right s
                        else pasteSet limit mname now (prev <> new) s
            (Nothing, []) -> pure [RErr "no data specified"]
            (Nothing, _) -> pure [RErr usage]
  where
    (opts, flags, pos) = parseArgs "btn" args
    mname = lookup "-b" opts
    appending = "-a" `elem` flags
    named s = case mname of
        Just n
            | isJust (pasteGet n s) -> Right n
            | otherwise -> Left ("unknown buffer: " <> n)
        Nothing -> maybe (Left "no buffer") (Right . (.name)) (pasteTop s)
    usage = "usage: set-buffer [-aw] [-b buffer-name] [-n new-buffer-name] \
            \[-t target-client] [data]"

cmdDeleteBuffer :: CommandImpl
cmdDeleteBuffer st _ args = withFlags "delete-buffer" "" flags $
    if not (null pos)
        then pure [RErr "usage: delete-buffer [-b buffer-name]"]
        else modifyStore st $ \s -> case lookup "-b" opts of
            Just n
                | isJust (pasteGet n s) -> Right (pasteFree n s)
                | otherwise -> Left ("unknown buffer: " <> n)
            Nothing -> case pasteTop s of
                Just pb -> Right (pasteFree pb.name s)
                Nothing -> Left "no buffer"
  where
    (opts, flags, pos) = parseArgs "b" args

-- | @show-buffer [-b name]@: the buffer's bytes, verbatim, to the issuing
-- command client's standard output.
cmdShowBuffer :: CommandImpl
cmdShowBuffer st mclient args = withFlags "show-buffer" "" flags $ do
    s <- readTVarIO st.buffers
    pure $ case pickBuffer (lookup "-b" opts) s of
        Left e -> [RErr e]
        Right pb -> case (.role) <$> mclient of
            Just Control -> [RData pb.body]
            _ -> map (ROutput . TE.decodeUtf8Lenient) (B8.lines pb.body)
  where
    (opts, flags, _) = parseArgs "b" args

-- | @list-buffers [-F format] [-f filter] [-O order] [-r]@, most recent
-- first unless an order is given.
cmdListBuffers :: CommandImpl
cmdListBuffers st _ args = withFlags "list-buffers" "r" flags $
    case orderBy of
        Nothing -> pure [RErr "invalid sort order"]
        Just sorter -> do
            bufs <- sorter . pasteWalk <$> readTVarIO st.buffers
            shown <- filterM keep bufs
            mapM (\pb -> ROutput <$> expandFormat st (bufferEnv pb) template) shown
  where
    (opts, flags, _) = parseArgs "FfO" args
    template = fromMaybe defaultTemplate (lookup "-F" opts)
    defaultTemplate = "#{buffer_name}: #{buffer_size} bytes: \"#{buffer_sample}\""
    keep pb = case lookup "-f" opts of
        Nothing -> pure True
        Just f -> formatTrue <$> expandFormat st (bufferEnv pb) f
    reversed = "-r" `elem` flags
    orderBy = case T.toLower <$> lookup "-O" opts of
        Nothing -> Just id
        Just o
            | o == "order" -> Just (if reversed then reverse else id)
            | o == "name" || o == "title" -> sorted (\_ _ -> EQ)
            | o == "creation" -> sorted (\a b -> compare b.order a.order)
            | o == "size" -> sorted (\a b -> compare (B.length a.body) (B.length b.body))
            | o `elem` ["activity", "index", "key", "modifier", "z"] ->
                sorted (\_ _ -> EQ)
            | otherwise -> Nothing
    sorted primary = Just . sortBy $ \a b ->
        let c = primary a b <> compare a.name b.name
        in if reversed then compare EQ c else c

-- | A buffer's @#{buffer_*}@ variables.
bufferEnv :: PasteBuffer -> FormatEnv
bufferEnv pb = Map.fromList
    [ ("buffer_name", pb.name)
    , ("buffer_size", T.pack (show (B.length pb.body)))
    , ("buffer_sample", pasteSample pb)
    , ("buffer_full", TE.decodeUtf8Lenient pb.body)
    , ("buffer_created", T.pack (show pb.created))
    ]

-- | tmux's @format_true@.
formatTrue :: Text -> Bool
formatTrue t = not (T.null t) && t /= "0"

-- | @load-buffer [-w] [-b name] [-t client] path@: a file's bytes (or the
-- command client's standard input, for @-@) into a buffer.
cmdLoadBuffer :: CommandImpl
cmdLoadBuffer st mclient args = withFlags "load-buffer" "w" flags $
    case pos of
        _ | "-w" `elem` flags ->
            pure [RErr "load-buffer -w: setting the clipboard is not implemented"]
        [raw] -> do
            path <- expandPath st mclient raw
            r <- if path == "-"
                then readClientStdin mclient
                else either (Left . ioMessage path) Right <$> try (B.readFile path)
            case r of
                Left e -> pure [RErr e]
                Right bytes -> do
                    now <- currentTime
                    modifyStoreLimited st $ \limit ->
                        pasteSet limit (lookup "-b" opts) now bytes
        _ -> pure [RErr "usage: load-buffer [-b buffer-name] [-t target-client] path"]
  where
    (opts, flags, pos) = parseArgs "bt" args

-- | The issuing client's standard input, which only a command client has.
readClientStdin :: Maybe Client -> IO (Either Text ByteString)
readClientStdin = \case
    Just Client { stdinSource = StdinFrom fetch } -> fetch
    _ -> pure (Left "Bad file descriptor: -")

-- | @save-buffer [-a] [-b name] path@: write (or with @-a@ append) a
-- buffer's bytes to a file, or for @-@ to the command client's standard
-- output.
cmdSaveBuffer :: CommandImpl
cmdSaveBuffer st mclient args = withFlags "save-buffer" "a" flags $
    case pos of
        [raw] -> do
            s <- readTVarIO st.buffers
            case pickBuffer (lookup "-b" opts) s of
                Left e -> pure [RErr e]
                Right pb -> do
                    path <- expandPath st mclient raw
                    if path == "-"
                        then pure $ case (.role) <$> mclient of
                            Just Control -> [RData pb.body]
                            _ -> [RErr "Bad file descriptor: -"]
                        else do
                            let write = if "-a" `elem` flags then B.appendFile else B.writeFile
                            r <- try (write path pb.body)
                            pure $ either (\e -> [RErr (ioMessage path e)]) (const []) r
        _ -> pure [RErr "usage: save-buffer [-a] [-b buffer-name] path"]
  where
    (opts, flags, pos) = parseArgs "b" args

-- | tmux's @"%s: %s", strerror(errno), path@.
ioMessage :: FilePath -> IOException -> Text
ioMessage path e = T.pack (ioe_description e) <> ": " <> T.pack path

-- | A command's path argument: format-expanded, @~@-expanded, and relative
-- to the client's directory — a command client's own working directory, an
-- attached client's session's, otherwise @HOME@. @-@ stays @-@.
expandPath :: ServerState -> Maybe Client -> Text -> IO FilePath
expandPath st mclient raw = do
    env <- targetEnv st mclient
    expanded <- T.unpack <$> expandFormat st env raw
    p <- expandTilde st.home expanded
    if p == "-" || isAbsolute p
        then pure p
        else (</> p) <$> clientDir st mclient

clientDir :: ServerState -> Maybe Client -> IO FilePath
clientDir st = \case
    Just c | c.role == Control -> pure (T.unpack c.cwd)
    Just c -> do
        sid <- readTVarIO c.session
        msess <- Map.lookup sid <$> readTVarIO st.sessions
        case msess of
            Just sess -> readTVarIO sess.startCwd
            Nothing -> home
    Nothing -> home
  where
    home = pure (fromMaybe "/" st.home)

-- | The default target pane's format variables, or none without one.
targetEnv :: ServerState -> Maybe Client -> IO FormatEnv
targetEnv st mclient = do
    res <- findTarget st mclient Target.FindPane Nothing
    case res of
        Left _ -> pure Map.empty
        Right (sess, wix, win, pane) -> do
            pix <- paneIndexOf st win pane
            paneFormatEnv st sess wix win pix pane

-- | @paste-buffer [-dprS] [-s separator] [-b name] [-t pane]@: write a
-- buffer into a pane, each newline replaced by the separator (default CR,
-- LF with @-r@), its text made safe unless @-S@; @-p@ brackets it when the
-- pane's app asked for bracketed paste; @-d@ deletes it after.
cmdPasteBuffer :: CommandImpl
cmdPasteBuffer st mclient args = withFlags "paste-buffer" "dprS" flags $ do
    res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
    case res of
        Left e -> pure [RErr e]
        Right (_, _, _, pane) -> do
            dead <- readTVarIO pane.dead
            s <- readTVarIO st.buffers
            let mpb = case lookup "-b" opts of
                    Just n -> maybe (Left ("no buffer " <> n)) (Right . Just) (pasteGet n s)
                    Nothing -> Right (pasteTop s)
            case mpb of
                _ | dead -> pure [RErr "target pane has exited"]
                Left e -> pure [RErr e]
                Right Nothing -> pure []
                Right (Just pb) -> do
                    bracket <- if has "-p"
                        then Emu.bracketedPaste pane.emulator
                        else pure False
                    let wrap b
                            | bracket = "\ESC[200~" <> b <> "\ESC[201~"
                            | otherwise = b
                    typeIntoPane pane (wrap (pasteBytes pb.body))
                    when (has "-d") $ modifyStore_ st (pasteFree pb.name)
                    pure []
  where
    (opts, flags, _) = parseArgs "bst" args
    has f = f `elem` flags
    separator = case lookup "-s" opts of
        Just sep -> TE.encodeUtf8 sep
        Nothing | has "-r" -> "\n"
                | otherwise -> "\r"
    pasteBytes = B.intercalate separator
        . map (if has "-S" then id else pasteVisSafe) . B8.split '\n'

-- | Store bytes as a named buffer, or a fresh automatic one, reporting a bad
-- name.
storeBuffer :: ServerState -> Maybe Text -> ByteString -> IO (Either Text ())
storeBuffer st mname bytes = do
    now <- currentTime
    replies <- modifyStoreLimited st (\limit -> pasteSet limit mname now bytes)
    pure $ case replies of
        RErr e : _ -> Left e
        _ -> Right ()

-- | @pipe-pane [-IOo] [-t target] [command]@. With no command (or @-o@
-- while already piping) it stops the pane's pipe. Otherwise it spawns
-- @sh -c command@: @-O@ (the default) feeds pane output to the process's
-- stdin, @-I@ feeds the process's stdout back into the pane.
cmdPipePane :: CommandImpl
cmdPipePane st mclient args = do
    let (opts, flags, pos) = parseArgs "t" args
        hasI = "-I" `elem` flags
        hasO = "-O" `elem` flags
        outputTap = if hasO || not hasI  -- default direction is -O
            then OutputTapped else OutputUntapped
        stdinFeed = if hasI then StdinFed else StdinUnfed
        toggle = "-o" `elem` flags
        cmd = T.strip (T.unwords pos)
    mpane <- targetPane st mclient (lookup "-t" opts)
    case mpane of
        Nothing -> pure []
        Just pane -> do
            wasPiping <- isJust <$> readTVarIO pane.pipe
            stopPipe pane
            if T.null cmd || (toggle && wasPiping)
                then pure []
                else startPipe pane (T.unpack cmd) outputTap stdinFeed >> pure []

-- | The named buffer, or the most recent automatic one.
pickBuffer :: Maybe Text -> PasteStore -> Either Text PasteBuffer
pickBuffer mname s = case mname of
    Just n -> maybe (Left ("no buffer " <> n)) Right (pasteGet n s)
    Nothing -> maybe (Left "no buffers") Right (pasteTop s)

-- | Apply a fallible store update, replying with its error.
modifyStore :: ServerState -> (PasteStore -> Either Text PasteStore) -> IO [Reply]
modifyStore st f = modifyStoreLimited st (const f)

-- | 'modifyStore' for an update that may evict, given @buffer-limit@.
modifyStoreLimited
    :: ServerState -> (Int -> PasteStore -> Either Text PasteStore) -> IO [Reply]
modifyStoreLimited st f = do
    r <- atomically $ do
        limit <- optionNumber "buffer-limit" <$> readTVar st.options
        s <- readTVar st.buffers
        case f limit s of
            Left e -> pure (Left e)
            Right s' -> do
                writeTVar st.buffers s'
                bumpDirty st
                pure (Right (s, s'))
    case r of
        Left e -> pure [RErr e]
        Right (old, new) -> [] <$ notifyPasteChanges st old new

modifyStore_ :: ServerState -> (PasteStore -> PasteStore) -> IO ()
modifyStore_ st f = () <$ modifyStore st (Right . f)

currentTime :: IO Integer
currentTime = floor <$> getPOSIXTime

-- | Run a command whose boolean flags must all be among @known@; any other
-- is an error rather than silently ignored.
withFlags :: Text -> [Char] -> [Text] -> IO [Reply] -> IO [Reply]
withFlags name known flags act =
    case filter (`notElem` [T.pack ['-', c] | c <- known]) flags of
        f : _ -> pure [RErr (name <> ": unknown flag " <> f)]
        [] -> act
