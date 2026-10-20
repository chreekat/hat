-- | tmux's control mode (@hat -C@): a client that drives the server with
-- command lines and reads back a line protocol — each command's reply
-- framed in @%begin@\/@%end@ (or @%error@), notifications such as
-- @%window-renamed@, and pane output as @%output@ — until @%exit@.
--
-- The connection's reader runs command lines ('feedControlInput'); a
-- writer thread ('controlWriter') alone renders the queued stream (see
-- "Hat.Server.ControlQueue") onto the client's socket, so a client that
-- reads slowly holds up only itself.
module Hat.Server.Control
    ( BlockOrigin (..)
    , runControlCommands
    , feedControlInput
    , splitInputLines
    , controlWriter
    , controlSink
    , escapeOutput
    , guardLine
    , parseControlFlags
    , ControlFlagChange (..)
    , applyControlFlags
    , ControlSize (..)
    , parseControlSize
    , cmdRefreshClient
    , subscriptionLoop
    ) where

import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, forever, unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

import Hat.Command.Parser (parseCommandLine)
import Hat.Geometry
import Hat.Model
import Hat.Server.ClientIO (hasRoom, sendDirect, sendFinal)
import Hat.Server.Command.Types
import Hat.Server.ControlQueue
import Hat.Server.ControlTypes
import Hat.Server.FormatEnv
    (expandFormat, paneFormatEnv, sessionFormatEnv, windowFormatEnv)
import Hat.Server.HookMonitor (parseMonitorSpec)
import Hat.Server.HookTypes (MonitorTarget (..))
import Hat.Server.Locate (findTargetClient, paneIndexOf)
import Hat.Server.HookTypes (EventSink, PayloadItem (..))
import Hat.Server.MessageLog (logShownMessage)
import Hat.Server.Pane (syncAllPanes)
import Hat.Server.Hooks (clientNameOf)
import Hat.Server.Resize (applySessionSize, awaitReconciled)
import Hat.Server.Viewport (Pan (..), panViewport)
import Hat.Server.Winlink (windowLinks)
import Hat.Transport.Wire

-- Commands ------------------------------------------------------------------

-- | Who issued a command block: the command line the client was started
-- with, or a line it sent. The @%begin@ flags field tells them apart.
data BlockOrigin = SetupCommand | ClientLine
    deriving (Eq, Show)

-- | A @%begin@\/@%end@\/@%error@ guard line: the guard word, the command's
-- start time (epoch seconds), its number, and its origin's flags.
guardLine :: ByteString -> Int -> Int -> BlockOrigin -> ByteString
guardLine guard t n origin = B8.pack $ concat
    [ "%", B8.unpack guard, " ", show t, " ", show n, " "
    , case origin of
        SetupCommand -> "0"
        ClientLine -> "1" ]

-- | Run a parsed command list, each command in its own reply block,
-- stopping at the first that fails (tmux drops the rest of a failed
-- command's group). A list that did not parse, or names any unknown
-- command, runs nothing: it fails as one parse-error block.
runControlCommands
    :: Dispatch -> ServerState -> Client -> ControlState -> BlockOrigin
    -> Either Text [[Text]] -> IO ()
runControlCommands d st client cs origin = \case
    Left e -> parseError e
    Right cmds -> do
        unknown <- mapM (d.unknownCommand st) cmds
        case [ e | Just e <- unknown ] of
            e : _ -> parseError e
            [] -> go cmds
  where
    go [] = pure ()
    go (argv : rest) = do
        (t, n) <- begin
        syncAllPanes st
        replies <- withCommandBatch st (d.runArgv st (Just client) argv)
        -- Report only once any layout change the command made reached the
        -- panes, so the next command observes their new sizes.
        awaitReconciled st
        let errors = [ e | RErr e <- replies ]
            failed = not (null errors)
        mapM_ (logShownMessage st client) errors
        atomically $ do
            forM_ replies (mapM_ (controlEmit cs) . replyLines)
            end t n (if failed then "error" else "end")
            when failed (writeTVar cs.retval 1)
        unless failed (go rest)
    parseError e = do
        (t, n) <- begin
        atomically $ do
            controlEmit cs ("parse error: "
                <> TE.encodeUtf8 (T.takeWhile (/= '\n') e))
            end t n "error"
            writeTVar cs.retval 1
    begin = do
        t <- floor <$> getPOSIXTime
        atomically $ do
            n <- stateTVar cs.commandNumber (\k -> (k + 1, k + 1))
            openBlock cs
            controlEmit cs (guardLine "begin" t n origin)
            pure (t, n)
    end t n guard = do
        controlEmit cs (guardLine guard t n origin)
        closeBlock cs

-- | A reply as protocol lines.
replyLines :: Reply -> [ByteString]
replyLines = \case
    ROutput t -> [TE.encodeUtf8 t]
    RErr e -> [TE.encodeUtf8 e]
    RData bytes -> B8.lines bytes
    RExitStatus _ -> []

-- | Split input into its complete lines and the unterminated rest.
splitInputLines :: ByteString -> ([ByteString], ByteString)
splitInputLines = go []
  where
    go acc bs = case B.elemIndex 10 bs of
        Nothing -> (reverse acc, bs)
        Just i -> go (B.take i bs : acc) (B.drop (i + 1) bs)

-- | Feed a chunk of the client's input: run each complete line as a
-- command line, until an empty line asks the client to exit; once it is
-- exiting, input is ignored.
feedControlInput
    :: Dispatch -> ServerState -> Client -> ControlState -> IORef ByteString
    -> ByteString -> IO ()
feedControlInput d st client cs pendingRef chunk = do
    pending <- readIORef pendingRef
    let (lns, rest) = splitInputLines (pending <> chunk)
    writeIORef pendingRef rest
    run lns
  where
    run [] = pure ()
    run (l : more)
        | B.null l = atomically (requestExit cs Nothing)
        | otherwise = do
            live <- readTVarIO cs.exiting
            case live of
                Exiting _ -> pure ()
                Running -> do
                    runControlCommands d st client cs ClientLine
                        (parseCommandLine (TE.decodeUtf8Lenient l))
                    run more

-- Output ----------------------------------------------------------------

-- | Pane output as @%output@ text: bytes below space and backslash become
-- @\\ooo@ octal escapes; everything else passes through.
escapeOutput :: ByteString -> BB.Builder
escapeOutput = B.foldr (\w acc -> byte w <> acc) mempty
  where
    byte w
        | w < 32 || w == 92 = BB.char7 '\\' <> octal w
        | otherwise = BB.word8 w
    octal w = foldMap (\k -> BB.word8 (48 + (w `div` k) `mod` 8)) [64, 8, 1]

-- | Send the client's queued stream until it has exited: lines as they
-- are, pane output escaped (or, past its age limit, paused or the client
-- evicted), then the farewell once everything queued is out.
controlWriter :: Client -> ControlState -> IO ()
controlWriter client cs = loop
  where
    loop = do
        now <- monotonicMs
        step <- atomically (nextBatch now)
        case step of
            Left reason -> do
                (status, fl) <- atomically $
                    (,) <$> readTVar cs.retval <*> readTVar cs.flags
                sendFinal client (ControlExit reason fl.exitWait status)
            Right out -> do
                unless (BL.null out) $
                    sendDirect client (WriteStdout (BL.toStrict out))
                loop
    -- Everything queued, rendered once the client's socket has room for
    -- it; or, with nothing left on an exiting client, its exit reason.
    nextBatch now = do
        check =<< hasRoom client  -- See Note [Falling behind]
        q <- swapTVar cs.queue Seq.empty
        writeTVar cs.outputBytes 0
        case q of
            Empty -> readTVar cs.exiting >>= \case
                Exiting reason -> pure (Left reason)
                Running -> retry
            _ -> Right . BB.toLazyByteString <$> render now q
    render now = \case
        Empty -> pure mempty
        ControlLine l :<| rest -> ((BB.byteString l <> BB.char7 '\n') <>)
            <$> render now rest
        ControlOutput pid t bytes :<| rest -> do
            let age = ageAt now t
            fl <- readTVar cs.flags
            case ageVerdict fl age of
                PausePane -> do
                    modifyTVar' cs.panes (Map.insert pid PanePaused)
                    let rest' = Seq.filter (not . ofPane pid) rest
                    ((pauseLine pid <> BB.char7 '\n') <>) <$> render now rest'
                TooFarBehind -> do
                    requestExit cs (Just "too far behind")
                    render now (Seq.filter isLine rest)
                InTime -> ((line <> BB.char7 '\n') <>) <$> render now rest
                  where
                    line = case fl.pauseAfter of
                        Just _ -> extended pid age bytes
                        Nothing -> plain pid bytes
    plain pid bytes =
        "%output " <> paneRef pid <> BB.char7 ' ' <> escapeOutput bytes
    extended pid age bytes =
        "%extended-output " <> paneRef pid <> BB.char7 ' ' <> BB.word64Dec age
            <> " : " <> escapeOutput bytes
    pauseLine pid = "%pause " <> paneRef pid
    ofPane pid = \case
        ControlOutput p _ _ -> p == pid
        ControlLine _ -> False
    isLine = \case
        ControlLine _ -> True
        ControlOutput {} -> False

paneRef :: PaneId -> BB.Builder
paneRef pid = BB.char7 '%' <> BB.intDec (rawPane pid)

monotonicMs :: IO Word64
monotonicMs = (`div` 1000000) <$> getMonotonicTimeNSec

-- Notifications -----------------------------------------------------------

-- | The control-mode notifications for a built-in event, written to every
-- control client it concerns (tmux's control event sinks).
controlSink :: ServerState -> EventSink
controlSink st name _ payload = do
    clients <- controlClients st
    unless (null clients) $ case name of
        "pane-mode-changed" | Just pid <- paneOf "pane" ->
            toAll clients ("%pane-mode-changed " <> ref '%' (rawPane pid))
        "window-layout-changed" | Just wid <- windowOf -> do
            mline <- layoutLine wid
            forM_ mline $ \line -> forM_ clients $ \(_, cs, msid) -> do
                holds <- maybe (pure False) (sessionHolds wid) msid
                when holds (atomically (controlNotify cs line))
        "window-pane-changed" | Just wid <- windowOf -> do
            mwin <- atomically (findWindowById st wid)
            forM_ mwin $ \win -> do
                active <- readTVarIO win.activeId
                toAll clients ("%window-pane-changed " <> ref '@' (rawWindow wid)
                    <> " " <> ref '%' (rawPane active))
        "window-unlinked" | Just wid <- windowOf ->
            perWindow clients wid "%window-close " "%unlinked-window-close " ""
        "window-linked" | Just wid <- windowOf ->
            perWindow clients wid "%window-add " "%unlinked-window-add " ""
        "window-renamed" | Just (wid, wname) <- windowNamed ->
            perWindow clients wid "%window-renamed " "%unlinked-window-renamed "
                (" " <> TE.encodeUtf8 wname)
        "client-session-changed" | Just cname <- clientOf -> do
            msess <- maybe (pure Nothing) (atomically . findSessionById st)
                (sessionOf "session")
            forM_ msess $ \sess -> do
                sname <- readTVarIO sess.name
                forM_ clients $ \(c, cs, _) ->
                    atomically $ controlNotify cs $ if clientNameOf (Just c) == cname
                        then "%session-changed " <> ref '$' (rawSession sess.id)
                            <> " " <> TE.encodeUtf8 sname
                        else "%client-session-changed " <> TE.encodeUtf8 cname
                            <> " " <> ref '$' (rawSession sess.id) <> " "
                            <> TE.encodeUtf8 sname
        "client-detached" | Just cname <- clientOf ->
            toAll clients ("%client-detached " <> TE.encodeUtf8 cname)
        "session-renamed" | Just (PSessionRef sid sname) <- lookup "session" payload ->
            toAll clients ("%session-renamed " <> ref '$' (rawSession sid)
                <> " " <> TE.encodeUtf8 sname)
        "session-created" -> toAll clients "%sessions-changed"
        "session-closed" -> toAll clients "%sessions-changed"
        "session-window-changed"
            | Just sid <- sessionOf "session", Just wid <- windowOf ->
                toAll clients ("%session-window-changed " <> ref '$' (rawSession sid)
                    <> " " <> ref '@' (rawWindow wid))
        "paste-buffer-changed" | Just (PText b) <- lookup "paste_buffer" payload ->
            toAll clients ("%paste-buffer-changed " <> TE.encodeUtf8 b)
        "paste-buffer-deleted" | Just (PText b) <- lookup "paste_buffer" payload ->
            toAll clients ("%paste-buffer-deleted " <> TE.encodeUtf8 b)
        _ -> pure ()
  where
    ref c n = B8.pack (c : show n)
    toAll clients line =
        forM_ clients $ \(_, cs, _) -> atomically (controlNotify cs line)
    paneOf k = case lookup k payload of
        Just (PPaneRef pid) -> Just pid
        _ -> Nothing
    sessionOf k = case lookup k payload of
        Just (PSessionRef sid _) -> Just sid
        _ -> Nothing
    windowNamed = case lookup "window" payload of
        Just (PWindowRef wid wname) -> Just (wid, wname)
        _ -> Nothing
    windowOf = fst <$> windowNamed
    clientOf = case lookup "client" payload of
        Just (PClientRef n) -> Just n
        _ -> Nothing
    -- The event's own session counts as holding the window even once it
    -- has been unlinked from it.
    perWindow clients wid linked unlinked suffix =
        forM_ clients $ \(_, cs, msid) -> do
            holds <- case msid of
                Nothing -> pure False
                Just sid
                    | sessionOf "session" == Just sid -> pure True
                    | otherwise -> sessionHolds wid sid
            atomically $ controlNotify cs $
                (if holds then linked else unlinked) <> ref '@' (rawWindow wid)
                    <> suffix
    sessionHolds wid sid = atomically $
        findSessionById st sid >>= \case
            Nothing -> pure False
            Just sess -> any (\w -> w.id == wid) <$> readTVar sess.windows
    layoutLine wid = do
        mloc <- atomically $ findWindowById st wid >>= \case
            Nothing -> pure Nothing
            Just win -> do
                ps <- readTVar win.panes
                links <- windowLinks st win
                pure $ if Map.null ps then Nothing
                    else (\(s, ix) -> (s, ix, win)) <$> listToMaybe links
        case mloc of
            Nothing -> pure Nothing
            Just (sess, ix, win) -> do
                env <- windowFormatEnv st sess ix win
                Just . ("%layout-change " <>) . TE.encodeUtf8
                    <$> expandFormat st env
                        "#{window_id} #{window_layout} \
                        \#{window_visible_layout} #{window_raw_flags}"

-- | Every control client, with the session it is attached to (if any).
controlClients :: ServerState -> IO [(Client, ControlState, Maybe SessionId)]
controlClients st = atomically $ do
    cs <- Map.elems <$> readTVar st.clients
    sessions <- readTVar st.sessions
    fmap concat . mapM (\c -> case c.control of
        Nothing -> pure []
        Just ctl -> do
            sid <- readTVar c.session
            pure [(c, ctl, if Map.member sid sessions then Just sid else Nothing)])
        $ cs

-- Subscriptions -----------------------------------------------------------

-- | Sample the client's @refresh-client -B@ subscriptions once a second
-- (tmux's monitor timer), reporting each value that is new or changed.
subscriptionLoop :: ServerState -> Client -> ControlState -> IO ()
subscriptionLoop st client cs = forever $ do
    threadDelay 1000000
    msess <- atomically $ do
        sid <- readTVar client.session
        findSessionById st sid
    subs <- Map.toAscList <$> readTVarIO cs.subscriptions
    forM_ msess $ \sess -> forM_ subs $ \(name, sub) -> do
        samples <- sampleSubscription st sess sub
        atomically $ do
            current <- Map.lookup name <$> readTVar cs.subscriptions
            forM_ current $ \cur -> when (cur.serial == sub.serial) $ do
                let fresh = Map.fromList [ (k, v) | (k, _, v) <- samples ]
                    changed = [ (k, ref, v) | (k, ref, v) <- samples
                              , Map.lookup k cur.lastValues /= Just v ]
                modifyTVar' cs.subscriptions $ Map.insert name cur
                    { lastValues = case cur.target of
                        -- The all-panes/all-windows sets forget what is gone.
                        MonAllPanes -> fresh
                        MonAllWindows -> fresh
                        _ -> Map.union fresh cur.lastValues }
                forM_ changed $ \(_, ref, v) ->
                    controlNotify cs ("%subscription-changed " <> TE.encodeUtf8 name
                        <> " " <> ref <> " : " <> TE.encodeUtf8 v)

-- | One subscription's current values in a session, each with its key and
-- the @$s \@w idx %p@ reference its change is reported under.
sampleSubscription
    :: ServerState -> Session -> Subscription
    -> IO [(SubscriptionKey, ByteString, Text)]
sampleSubscription st sess sub = do
    ws <- Map.toAscList <$> readTVarIO sess.windows
    case sub.target of
        MonSession -> do
            env <- sessionFormatEnv st sess
            v <- expandFormat st env sub.format
            pure [(SubSession, sref <> " - - -", v)]
        MonWindow n -> concat <$> forM [ (ix, w) | (ix, w) <- ws, rawWindow w.id == n ]
            (uncurry windowValue)
        MonAllWindows -> concat <$> forM ws (uncurry windowValue)
        MonPane n -> fmap concat . forM ws $ \(ix, w) -> do
            ps <- atomically (windowPanes w)
            concat <$> forM [ p | p <- ps, rawPane p.id == n ] (paneValue ix w)
        MonAllPanes -> fmap concat . forM ws $ \(ix, w) -> do
            ps <- atomically (windowPanes w)
            concat <$> forM ps (paneValue ix w)
  where
    sref = "$" <> B8.pack (show (rawSession sess.id))
    wref ix w = sref <> " @" <> B8.pack (show (rawWindow w.id))
        <> " " <> B8.pack (show ix)
    windowValue ix w = do
        env <- windowFormatEnv st sess ix w
        v <- expandFormat st env sub.format
        pure [(SubWindow (rawWindow w.id) ix, wref ix w <> " -", v)]
    paneValue ix w p = do
        gone <- readTVarIO p.dead
        if gone then pure [] else do
            pix <- paneIndexOf st w p
            env <- paneFormatEnv st sess ix w pix p
            v <- expandFormat st env sub.format
            pure [ ( SubPane (rawPane p.id) ix
                   , wref ix w <> " %" <> B8.pack (show (rawPane p.id)), v ) ]

-- refresh-client ----------------------------------------------------------

-- | One @refresh-client -f@ item: a flag set or cleared (@!flag@).
data ControlFlagChange
    = SetNoOutput Bool
    | SetWaitExit Bool
    | SetPauseAfter (Maybe Word64)
    deriving (Eq, Show)

-- | The control-only flags in a @-f@ list; tmux silently skips names it
-- does not know (and the general client flags are handled elsewhere).
parseControlFlags :: Text -> [ControlFlagChange]
parseControlFlags = concatMap one . T.splitOn ","
  where
    one item =
        let (off, name) = maybe (False, item) (True,) (T.stripPrefix "!" item)
        in case name of
            "no-output" -> [SetNoOutput (not off)]
            "wait-exit" -> [SetWaitExit (not off)]
            "pause-after"
                | off -> [SetPauseAfter Nothing]
                | otherwise -> [SetPauseAfter (Just 0)]
            _ | Just v <- T.stripPrefix "pause-after=" name
              , Right (secs, "") <- TR.decimal v ->
                  [SetPauseAfter (if off then Nothing else Just (secs * 1000))]
              | otherwise -> []

applyControlFlags :: [ControlFlagChange] -> ControlFlags -> ControlFlags
applyControlFlags = flip (foldl step)
  where
    step fl = \case
        SetNoOutput on -> fl { output = if on then NoOutput else SendOutput }
        SetWaitExit on -> fl { exitWait = if on then AwaitEmptyLine else ExitNow }
        SetPauseAfter age -> fl { pauseAfter = age }

-- | A @refresh-client -C@ argument: the client's size, or one window's
-- (@\@id:WxH@), or clearing one window's (@\@id:@).
data ControlSize
    = ClientSize Size
    | WindowSize WindowId Size
    | ClearWindowSize WindowId
    deriving (Eq, Show)

-- | Parse @-C@ as tmux's sscanf does, with its size bounds.
parseControlSize :: Text -> Either Text ControlSize
parseControlSize arg = case T.stripPrefix "@" arg of
    Just rest | Right (w, afterW) <- TR.decimal rest
              , Just sz <- T.stripPrefix ":" afterW ->
        if T.null sz
            then Right (ClearWindowSize (WindowId w))
            else WindowSize (WindowId w) <$> dims (T.breakOn "x" sz)
    _ -> ClientSize <$> case T.breakOn "," arg of
        (w, rest) | not (T.null rest) -> dims (w, rest)
        _ -> dims (T.breakOn "x" arg)
  where
    dims (w, rest) = case (TR.decimal w, TR.decimal (T.drop 1 rest)) of
        (Right (x, _), Right (y, _)) | not (T.null rest) ->
            if x < 1 || x > 10000 || y < 1 || y > 10000
                then Left "size too small or too big"
                else Right Size { rows = fromInteger y, cols = fromInteger x }
        _ -> Left "bad size argument"

-- | @refresh-client@: @-U@\/@-D@\/@-L@\/@-R@ pan the target client's view
-- of a window larger than it and @-c@ returns the view to the cursor; for
-- a control client, @-C@ sizes it (or one of its windows), @-f@ sets its
-- flags, @-A@ sets panes' output flow and @-B@ manages subscriptions; for
-- any client, a plain refresh redraws it.
cmdRefreshClient :: CommandImpl
cmdRefreshClient st mclient args
    | Just e <- unknownFlag "refresh-client" "cDlLRSU" flags = pure [RErr e]
    | Just pan <- firstOf ["-c", "-L", "-R", "-U", "-D"] =
        case traverse (strtonum 1 2147483647) (listToMaybe pos) of
            Left e -> pure [RErr ("adjustment " <> e)]
            Right madjust -> panClient (panOf pan (fromMaybe 1 madjust))
    | Just f <- firstOf ["-l"] =
        pure [RErr ("refresh-client " <> f <> ": not supported")]
    | isJust' "-r" = pure [RErr "refresh-client -r: not supported"]
    | otherwise = case mclient of
        Nothing -> pure [RErr "no current client"]
        Just client -> do
            forM_ (lookup "-F" opts <> lookup "-f" opts) $ \fs ->
                setFlags client fs
            case client.control of
                Nothing
                    | any isJust' ["-A", "-B", "-C"] ->
                        pure [RErr "not a control client"]
                    | otherwise -> redraw client
                Just cs
                    | isJust' "-A" -> [] <$ mapM_ (paneFlow cs)
                        [ v | ("-A", v) <- opts ]
                    | isJust' "-B" -> [] <$ mapM_ (subscribe cs)
                        [ v | ("-B", v) <- opts ]
                    | Just arg <- lookup "-C" opts -> controlSize client cs arg
                    | otherwise -> redraw client
  where
    (optsRev, flags, pos) = parseArgs "ABCfFrt" args
    opts = reverse optsRev
    isJust' f = any ((== f) . fst) opts
    firstOf fs = listToMaybe [ f | f <- fs, f `elem` flags ]
    panOf f n = case f of
        "-L" -> PanLeft n
        "-R" -> PanRight n
        "-U" -> PanUp n
        "-D" -> PanDown n
        _ -> Unpan
    panClient pan = findTargetClient st mclient Nothing (lookup "-t" opts) >>= \case
        Left e -> pure [RErr e]
        Right tc -> do
            atomically $ do
                sid <- readTVar tc.session
                msess <- Map.lookup sid <$> readTVar st.sessions
                forM_ msess $ \sess -> do
                    mwin <- currentWindow sess
                    forM_ mwin $ \win -> do
                        area <- readTVar win.size
                        modifyTVar' tc.viewport (panViewport pan win.id area)
                writeTVar tc.needsFull True
                bumpDirty st
            pure []
    redraw client = do
        atomically $ do
            writeTVar client.needsFull True
            bumpDirty st
        pure []
    setFlags client fs = forM_ client.control $ \cs -> atomically $ do
        let changes = parseControlFlags fs
        modifyTVar' cs.flags (applyControlFlags changes)
        when (SetNoOutput True `elem` changes) $ do
            modifyTVar' cs.queue (Seq.filter keepLine)
            writeTVar cs.outputBytes 0
    keepLine = \case
        ControlLine _ -> True
        ControlOutput {} -> False
    paneFlow cs spec = case T.breakOn ":" spec of
        (p, rest) | Just n <- T.stripPrefix "%" p
                  , Right (pid, "") <- TR.decimal n
                  , not (T.null rest) -> do
            mpane <- atomically (findPaneById st pid)
            forM_ mpane $ \pane -> atomically $ setFlow cs pane.id (T.drop 1 rest)
        _ -> pure ()
    setFlow cs pid = \case
        "on" -> modifyTVar' cs.panes $ Map.update
            (\case PaneOff -> Nothing; f -> Just f) pid
        "off" -> do
            dropPane cs pid
            modifyTVar' cs.panes (Map.insert pid PaneOff)
        "continue" -> do
            flow <- Map.lookup pid <$> readTVar cs.panes
            when (flow == Just PanePaused) $ do
                modifyTVar' cs.panes (Map.delete pid)
                controlEmit cs ("%continue %" <> B8.pack (show (rawPane pid)))
        "pause" -> do
            flow <- Map.lookup pid <$> readTVar cs.panes
            unless (flow == Just PanePaused) $ do
                dropPane cs pid
                modifyTVar' cs.panes (Map.insert pid PanePaused)
                controlEmit cs ("%pause %" <> B8.pack (show (rawPane pid)))
        _ -> pure ()
    dropPane cs pid = modifyTVar' cs.queue $ Seq.filter $ \case
        ControlOutput p _ _ -> p /= pid
        ControlLine _ -> True
    -- An unparseable spec names a subscription to drop.
    subscribe cs spec = atomically $ case parseMonitorSpec spec of
        Left _ -> modifyTVar' cs.subscriptions (Map.delete spec)
        Right (name, tgt, fmt) -> do
            n <- stateTVar cs.subscriptionSerial (\k -> (k + 1, k + 1))
            modifyTVar' cs.subscriptions $ Map.insert name Subscription
                { serial = n, target = tgt, format = fmt
                , lastValues = Map.empty }
    controlSize client cs arg = case parseControlSize arg of
        Left e -> pure [RErr e]
        Right change -> do
            atomically $ case change of
                ClientSize sz -> writeTVar cs.size (Just sz)
                WindowSize wid sz -> modifyTVar' cs.windowSizes
                    (Just . Map.insert wid sz . maybe Map.empty id)
                ClearWindowSize wid -> modifyTVar' cs.windowSizes
                    (fmap (Map.delete wid))
            applySessionSize st =<< readTVarIO client.session
            pure []
