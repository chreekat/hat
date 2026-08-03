-- | The paste-buffer commands (@show@\/@set@\/@list@\/@delete@\/@save@\/
-- @paste-buffer@) and @pipe-pane@: the named-buffer stack and forwarding a
-- pane's output to an external process.
module Hat.Server.Command.Buffer
    ( cmdShowBuffer
    , cmdSetBuffer
    , cmdListBuffers
    , cmdDeleteBuffer
    , cmdSaveBuffer
    , cmdPasteBuffer
    , cmdPipePane
    ) where

import Control.Concurrent.STM
import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Hat.Path (expandTilde)
import Hat.Model
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import Hat.Server.Locate (targetPane)
import Hat.Server.Pane (OutputTap (..), StdinFeed (..), startPipe, stopPipe)
import qualified Hat.Term.Pty

cmdShowBuffer :: CommandImpl
cmdShowBuffer st _ args = do
    let (opts, _, _) = parseArgs "b" args
    bufs <- readTVarIO st.buffers
    pure $ case bufferBody (lookup "-b" opts) bufs of
        Nothing -> [RErr "no buffers"]
        Just body -> [ROutput body]

cmdSetBuffer :: CommandImpl
cmdSetBuffer st _ args = do
    let (opts, flags, pos) = parseArgs "bn" args
        appendMode = "-a" `elem` flags
        mname = lookup "-b" opts
        body = T.unwords pos
    if null pos
        then pure [RErr "usage: set-buffer [-a] [-b name] data"]
        else atomically $ do
            bufs <- readTVar st.buffers
            case mname of
                Just name -> do
                    let existing = lookupBuffer name bufs
                        newBody = case (appendMode, existing) of
                            (True, Just prev) -> prev <> body
                            _ -> body
                        others = Seq.filter ((/= name) . fst) bufs
                    writeTVar st.buffers ((name, newBody) Seq.<| others)
                Nothing -> do
                    n <- readTVar st.nextBuffer
                    writeTVar st.nextBuffer (n + 1)
                    let name = "buffer" <> T.pack (show n)
                    writeTVar st.buffers ((name, body) Seq.<| bufs)
            bumpDirty st
            pure []

cmdListBuffers :: CommandImpl
cmdListBuffers st _ _ = do
    bufs <- readTVarIO st.buffers
    pure . map row $ toList' bufs
  where
    row (name, body) =
        ROutput (name <> ": " <> tshow (T.length body) <> " bytes")
    toList' s = case Seq.viewl s of
        Seq.EmptyL -> []
        x Seq.:< xs -> x : toList' xs

cmdDeleteBuffer :: CommandImpl
cmdDeleteBuffer st _ args = do
    let (opts, _, _) = parseArgs "b" args
    atomically $ do
        bufs <- readTVar st.buffers
        writeTVar st.buffers (dropBuffer (lookup "-b" opts) bufs)
        pure []

-- | Write the top (or named) buffer to a file. @-a@ appends; the path
-- may start with @~/@.
cmdSaveBuffer :: CommandImpl
cmdSaveBuffer st _ args = do
    let (opts, flags, pos) = parseArgs "b" args
        appendMode = "-a" `elem` flags
    case pos of
        [] -> pure [RErr "usage: save-buffer [-a] [-b name] path"]
        (rawPath : _) -> do
            bufs <- readTVarIO st.buffers
            case bufferBody (lookup "-b" opts) bufs of
                Nothing -> pure [RErr "no buffers"]
                Just body -> do
                    path <- expandTilde (T.unpack rawPath)
                    let write = if appendMode then TIO.appendFile else TIO.writeFile
                    r <- try (write path body)
                    pure $ case r of
                        Left (e :: IOException) -> [RErr (T.pack (show e))]
                        Right () -> []

-- | Paste the top (or named) buffer into a pane's pty. @-d@ deletes the
-- buffer afterwards, @-p@ wraps it in bracketed-paste markers, @-r@
-- turns carriage returns into newlines.
cmdPasteBuffer :: CommandImpl
cmdPasteBuffer st mclient args = do
    let (opts, flags, _) = parseArgs "bt" args
        del = "-d" `elem` flags
        bracketed = "-p" `elem` flags
        crToNl = "-r" `elem` flags
        mname = lookup "-b" opts
    bufs <- readTVarIO st.buffers
    case bufferBody mname bufs of
        Nothing -> pure [RErr "no buffers"]
        Just body0 -> do
            mpane <- targetPane st mclient (lookup "-t" opts)
            case mpane of
                Nothing -> pure [RErr "no target pane"]
                Just pane -> do
                    let body = if crToNl
                            then T.map (\c -> if c == '\r' then '\n' else c) body0
                            else body0
                        payload
                            | bracketed = "\ESC[200~" <> body <> "\ESC[201~"
                            | otherwise = body
                    Hat.Term.Pty.writePty pane.pty (TE.encodeUtf8 payload)
                    when del $ atomically $ do
                        cur <- readTVar st.buffers
                        writeTVar st.buffers (dropBuffer mname cur)
                    pure []

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

-- | The top buffer, or a named one.
bufferBody :: Maybe Text -> Seq (Text, Text) -> Maybe Text
bufferBody mname bufs = case mname of
    Just name -> lookupBuffer name bufs
    Nothing -> case bufs of
        Seq.Empty -> Nothing
        (_, body) Seq.:<| _ -> Just body

-- | Drop the top buffer, or a named one.
dropBuffer :: Maybe Text -> Seq (Text, Text) -> Seq (Text, Text)
dropBuffer mname bufs = case mname of
    Just name -> Seq.filter ((/= name) . fst) bufs
    Nothing -> case bufs of
        Seq.Empty -> bufs
        _ Seq.:<| rest -> rest

lookupBuffer :: Text -> Seq (Text, Text) -> Maybe Text
lookupBuffer name = go
  where
    go s = case Seq.viewl s of
        Seq.EmptyL -> Nothing
        (n, b) Seq.:< rest
            | n == name -> Just b
            | otherwise -> go rest
