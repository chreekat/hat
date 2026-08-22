-- | Building the @#{…}@ format environment — session, window, and pane —
-- and expanding formats against it: the shared substrate the status line,
-- listing, choose, capture, and display commands format against.
module Hat.Server.FormatEnv
    ( windowFormatEnv
    , paneFormatEnv
    , refreshAutoNames
    , autoName
    , activeClientCounts
    , sessionFormatEnv
    , paneModeEnv
    , resolveShell
    , expandFormat
    , WindowFlagState (..)
    , windowFlags
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, void, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Time.LocalTime (getZonedTime)
import System.Exit (ExitCode (..))
import System.Posix.Unistd (SystemID (nodeName), getSystemID)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Process
    (CreateProcess (..), readCreateProcessWithExitCode, shell)
import qualified Data.Vector as V

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.ColorScheme (schemeName)
import Hat.Server.Format (FormatCtx (..), FormatEnv, formatCtx, renderFormatCtx)
import Hat.Server.Locate (locatePane, paneIndexOf)
import Hat.Server.Layout
import Hat.Server.LayoutString (emitLayout)
import Hat.Server.Pane (paneCommandName)
import qualified Hat.Term.Emulator as Emu
import qualified Hat.Term.Pty

windowFormatEnv :: ServerState -> Session -> Int -> Window -> IO FormatEnv
windowFormatEnv st sess ix win = do
    base <- sessionFormatEnv st sess
    eff <- readTVarIO sess.lastSize
    (wname, lay, cur, mlast, bell, act, auto, zoom) <- atomically $ (,,,,,,,)
        <$> readTVar win.name <*> readTVar win.layout
        <*> readTVar sess.currentIx <*> (listToMaybe <$> readTVar sess.windowHist)
        <*> readTVar win.bellFlag <*> readTVar win.activity
        <*> readTVar win.autoRename <*> readTVar win.zoomed
    ps <- readTVarIO win.panes
    let flags = windowFlags WindowFlagState
            { flagCurrent = ix == cur
            , flagLast = Just ix == mlast
            , flagBell = bell
            , flagActivity = act
            , flagZoomed = isJust zoom
            }
    winUser <- deltaUserVars <$> readTVarIO win.options
    pure $ Map.union winUser $ Map.union (Map.fromList
        [ ("window_index", tshow ix)
        , ("window_id", "@" <> tshow (rawWindow win.id))
        , ("window_name", wname)
        , ("window_layout", emitLayout (sizeRect (eff)) lay)
        , ("window_active", if ix == cur then "1" else "0")
        , ("window_flags", flags)
        , ("window_panes", tshow (Map.size ps))
        , ("window_zoomed_flag", if isJust zoom then "1" else "0")
        , ("automatic_rename", if auto then "1" else "0")
        ]) base

-- | The full format environment for a specific pane, including the
-- fields tmux-resurrect's @save.sh@ dumps (pid, command, cursor,
-- history, cwd).
paneFormatEnv
    :: ServerState -> Session -> Int -> Window -> Int -> Pane -> IO FormatEnv
paneFormatEnv st sess wix win pix pane = do
    wenv <- windowFormatEnv st sess wix win
    dir <- paneCurrentPath pane
    cmd <- paneCommandName pane
    scr <- Emu.snapshot pane.emulator
    hsize <- Emu.scrollbackLength pane.emulator
    active <- readTVarIO win.activeId
    sz <- readTVarIO pane.size
    paneUser <- deltaUserVars <$> readTVarIO pane.options
    pure $ Map.union paneUser $ Map.union (Map.fromList
        [ ("pane_id", "%" <> tshow (rawPane pane.id))
        , ("pane_index", tshow pix)
        , ("pane_pid", tshow (Hat.Term.Pty.pid pane.pty))
        , ("pane_current_path", T.pack dir)
        , ("pane_current_command", cmd)
        , ("pane_active", if pane.id == active then "1" else "0")
        , ("cursor_x", tshow scr.cursor.col)
        , ("cursor_y", tshow scr.cursor.row)
        , ("history_size", tshow hsize)
        , ("pane_width", tshow sz.cols)
        , ("pane_height", tshow sz.rows)
        , ("session_grouped", "0")  -- hat has no session groups
        ]) wenv

-- | Recompute the names of every @automatic-rename@ window from its
-- active pane's foreground command, bumping the render generation on any
-- change. Driven by a periodic poll so no-output commands (an idle
-- @less@, a waiting @cat@) still get picked up.
refreshAutoNames :: ServerState -> IO ()
refreshAutoNames st = do
    fmt <- (.automaticRenameFormat) <$> readTVarIO st.options
    sessions <- Map.elems <$> readTVarIO st.sessions
    forM_ sessions $ \sess -> do
        ws <- Map.toAscList <$> readTVarIO sess.windows
        forM_ ws $ \(ix, win) -> do
            auto <- readTVarIO win.autoRename
            when auto $ do
                mnew <- autoName st sess ix win fmt
                forM_ mnew $ \newName -> atomically $ do
                    cur <- readTVar win.name
                    when (cur /= newName && not (T.null newName)) $ do
                        writeTVar win.name newName
                        bumpDirty st

-- | The name an @automatic-rename@ window should currently take: the
-- @automatic-rename-format@ expanded against the active pane. The default
-- format is just @#{pane_current_command}@, so it takes a cheap path.
autoName :: ServerState -> Session -> Int -> Window -> Text -> IO (Maybe Text)
autoName st sess ix win fmt = do
    mpane <- atomically (activePane win)
    case mpane of
        Nothing -> pure Nothing
        Just pane
            | fmt == "#{pane_current_command}" -> Just <$> paneCommandName pane
            | otherwise -> do
                pbase <- (.paneBaseIndex) <$> readTVarIO st.options
                env <- paneFormatEnv st sess ix win pbase pane
                Just <$> expandFormat st env fmt

-- | tmux's @window_active_clients@, per window: a client counts for the window
-- its session currently shows, keyed by window identity so a window linked
-- into several sessions counts the viewers of every one.
activeClientCounts :: ServerState -> STM (Map.Map WindowId Int)
activeClientCounts st = do
    cs <- Map.elems <$> readTVar st.clients
    sessions <- readTVar st.sessions
    fmap (Map.fromListWith (+) . catMaybes) . forM cs $ \c -> do
        sid <- readTVar c.session
        case Map.lookup sid sessions of
            Nothing -> pure Nothing
            Just sess -> fmap (\w -> (w.id, 1)) <$> currentWindow sess

-- Session-level format environment for the active window and pane.
sessionFormatEnv :: ServerState -> Session -> IO FormatEnv
sessionFormatEnv st sess = do
    hostname <- nodeName <$> getSystemID
    (sname, wEnv, mactive, nclients, nwindows) <- atomically $ do
        sname <- readTVar sess.name
        mwin <- currentWindow sess
        cur <- readTVar sess.currentIx
        nwindows <- Map.size <$> readTVar sess.windows
        counts <- activeClientCounts st
        wEnv <- case mwin of
            Nothing -> pure []
            Just win -> do
                wname <- readTVar win.name
                pure [ ("window_index", tshow cur)
                     , ("window_name", wname)
                     , ( "window_active_clients"
                       , tshow (Map.findWithDefault 0 win.id counts) )
                     ]
        mactive <- maybe (pure Nothing) activePane mwin
        cs <- sessionClients st sess.id
        pure (sname, wEnv, mactive, length cs, nwindows)
    pEnv <- case mactive of
        Nothing -> pure []
        Just pane -> do
            dir <- paneCurrentPath pane
            title <- Emu.title pane.emulator
            modeEnv <- paneModeEnv pane
            pure $ [ ("pane_current_path", T.pack dir)
                   , ("pane_title", title)
                   , ("pane_id", "%" <> tshow (rawPane pane.id))
                   ] <> modeEnv
    sz <- readTVarIO sess.lastSize
    -- @-options are readable as #{@foo}, so if-shell theme conditionals
    -- (@#{@pane-theme}@) resolve.
    userOpts <- (.user) <$> readTVarIO st.options
    localUser <- deltaUserVars <$> readTVarIO sess.options
    msch <- readTVarIO st.colorScheme
    pure . Map.union localUser . Map.union userOpts . Map.fromList $
        [ ("session_name", sname)
        , ("session_id", "$" <> tshow (rawSession sess.id))
        , ("session_attached", tshow nclients)
        , ("session_windows", tshow nwindows)
        , ("host", T.pack hostname)
        , ("window_active_clients", "0")
        , ("window_width", tshow sz.cols)
        , ("window_height", tshow sz.rows)
        , ("color_scheme", maybe "" schemeName msch)
        ]
        <> wEnv <> pEnv

-- | Copy-mode format variables for a pane: @pane_in_mode@/@pane_mode@,
-- plus @copy_cursor_{x,y,line}@ while in mode.
paneModeEnv :: Pane -> IO [(Text, Text)]
paneModeEnv pane = do
    mmode <- readTVarIO pane.mode
    case mmode of
        Nothing -> pure [("pane_in_mode", "0"), ("pane_mode", "")]
        Just pm -> do
            let s = pm.copyState
                top = pm.frozen.fgHsize - s.viewportOffY
            pure [ ("pane_in_mode", "1")
                 , ("pane_mode", "copy-mode")
                 , ("copy_cursor_x", tshow s.cursorCol)
                 , ("copy_cursor_y", tshow (s.cursorRow - top))
                 , ("copy_cursor_line", tshow s.cursorRow)
                 ]

-- Resolve #(cmd) through a 15-second cache; refreshes happen in the
-- background so the status line never blocks on a slow script.
resolveShell :: ServerState -> Text -> IO Text
resolveShell st cmdText = do
    now <- getCurrentTime
    cache <- readTVarIO st.shellCache
    case Map.lookup cmdText cache of
        Just (at, val)
            | diffUTCTime now at < 15 -> pure val
            | otherwise -> refresh now val
        Nothing -> refresh now ""
  where
    refresh now oldVal = do
        -- Optimistically bump the timestamp so only one refresh runs.
        atomically $ modifyTVar' st.shellCache
            (Map.insert cmdText (now, oldVal))
        void . forkIO $ do
            r <- try (readCreateProcessWithExitCode
                (shell (T.unpack cmdText)) { close_fds = True } "")
            let val = case r of
                    Right (ExitSuccess, out, _) ->
                        T.strip (T.takeWhile (/= '\n') (T.pack out))
                    Right (ExitFailure _, _, _) -> ""
                    Left (_ :: SomeException) -> ""
            done <- getCurrentTime
            atomically $ modifyTVar' st.shellCache
                (Map.insert cmdText (done, val))
            atomically (bumpDirty st)
        pure oldVal

-- | Expand a format string fully: #{...}, cached #(...), then strftime.
expandFormat :: ServerState -> FormatEnv -> Text -> IO Text
expandFormat st env fmt = do
    -- Pre-resolve shell segments so the evaluator stays pure.
    resolved <- newIORef Map.empty
    let collect t = case T.breakOn "#(" t of
            (_, rest) | T.null rest -> pure ()
            (_, rest) -> do
                let inner = fst (breakBalanced (T.drop 2 rest))
                val <- resolveShell st inner
                modifyIORef' resolved (Map.insert inner val)
                collect (T.drop (2 + T.length inner + 1) rest)
    collect fmt
    vals <- readIORef resolved
    now <- getZonedTime
    -- Loop/search context, gathered only when the format demands it; the
    -- strict return keeps every deferred read inside this call.
    sessionItems <- unsafeInterleaveIO (loopSessionEnvs st)
    windowItems <- unsafeInterleaveIO (loopWindowEnvs st env)
    paneItems <- unsafeInterleaveIO (loopPaneEnvs st env)
    clientItems <- unsafeInterleaveIO (loopClientEnvs st)
    visibleLines <- unsafeInterleaveIO (targetPaneLines st env)
    let ctx = (formatCtx env (\c -> Map.findWithDefault "" c vals) now)
            { sessions = sessionItems
            , windows = windowItems
            , panes = paneItems
            , clients = clientItems
            , paneLines = visibleLines
            }
    pure $! renderFormatCtx ctx fmt
  where
    breakBalanced = go (0 :: Int) ""
      where
        go depth acc t = case T.uncons t of
            Nothing -> (acc, "")
            Just (')', rest) | depth == 0 -> (acc, rest)
            Just (c, rest)
                | c == '(' -> go (depth + 1) (acc <> T.singleton c) rest
                | c == ')' -> go (depth - 1) (acc <> T.singleton c) rest
                | otherwise -> go depth (acc <> T.singleton c) rest

-- | The conditions that produce a window's @#{window_flags}@ string.
data WindowFlagState = WindowFlagState
    { flagCurrent  :: Bool
    , flagLast     :: Bool
    , flagBell     :: Bool
    , flagActivity :: Bool
    , flagZoomed   :: Bool
    }

-- | Render the window-status flags in tmux's order: current (@*@) or
-- last (@-@), then bell (@!@) and activity (@#@), and finally zoom
-- (@Z@) when the window has a pane zoomed to fill it.
windowFlags :: WindowFlagState -> Text
windowFlags s = T.concat
    [ if s.flagCurrent then "*"
      else if s.flagLast then "-" else ""
    , if s.flagBell then "!" else ""
    , if s.flagActivity then "#" else ""
    , if s.flagZoomed then "Z" else ""
    ]

-- One env per session, for the S loop and N/s.
loopSessionEnvs :: ServerState -> IO [FormatEnv]
loopSessionEnvs st = do
    ss <- Map.elems <$> readTVarIO st.sessions
    mapM (sessionFormatEnv st) ss

-- The target session's windows, for the W loop and N/w.
loopWindowEnvs :: ServerState -> FormatEnv -> IO [FormatEnv]
loopWindowEnvs st env = do
    msess <- targetSessionOf st env
    case msess of
        Nothing -> pure []
        Just sess -> do
            ws <- Map.toAscList <$> readTVarIO sess.windows
            mapM (\(ix, win) -> windowFormatEnv st sess ix win) ws

-- The target window's panes in creation order, for the P loop.
loopPaneEnvs :: ServerState -> FormatEnv -> IO [FormatEnv]
loopPaneEnvs st env = do
    mwin <- targetWindowOf st env
    case mwin of
        Nothing -> pure []
        Just (sess, wix, win) -> do
            ps <- Map.elems <$> readTVarIO win.panes
            mapM (\pane -> do
                pix <- paneIndexOf st win pane
                paneFormatEnv st sess wix win pix pane) ps

-- One env per attached client, for the L loop.
loopClientEnvs :: ServerState -> IO [FormatEnv]
loopClientEnvs st = do
    cs <- Map.elems <$> readTVarIO st.clients
    pure [ Map.fromList [("client_name", "client" <> tshow (rawClient c.id))]
         | c <- cs, c.role == Attached ]

-- The target pane's visible lines, for the C search modifier.
targetPaneLines :: ServerState -> FormatEnv -> IO [Text]
targetPaneLines st env = case idNum '%' "pane_id" env of
    Nothing -> pure []
    Just n -> do
        mloc <- atomically (locatePane st (PaneId n))
        case mloc of
            Nothing -> pure []
            Just (_, win) -> do
                ps <- readTVarIO win.panes
                case Map.lookup (PaneId n) ps of
                    Nothing -> pure []
                    Just pane -> do
                        scr <- Emu.snapshot pane.emulator
                        pure [ Emu.screenRowText scr r
                             | r <- [0 .. V.length scr.cells - 1] ]

targetSessionOf :: ServerState -> FormatEnv -> IO (Maybe Session)
targetSessionOf st env = case idNum '$' "session_id" env of
    Nothing -> pure Nothing
    Just n -> Map.lookup (SessionId n) <$> readTVarIO st.sessions

targetWindowOf :: ServerState -> FormatEnv -> IO (Maybe (Session, Int, Window))
targetWindowOf st env = do
    msess <- targetSessionOf st env
    case (msess, idNum '@' "window_id" env) of
        (Just sess, Just wid) -> do
            ws <- Map.toAscList <$> readTVarIO sess.windows
            pure (listToMaybe
                [ (sess, ix, win) | (ix, win) <- ws, win.id == WindowId wid ])
        _ -> pure Nothing

-- A "$N"/"@N"/"%N" id variable's number.
idNum :: Char -> Text -> FormatEnv -> Maybe Int
idNum pre key env = do
    v <- Map.lookup key env
    rest <- T.stripPrefix (T.singleton pre) v
    case TR.decimal rest of
        Right (n, "") -> Just n
        _ -> Nothing

-- @-prefixed user options set at one scope, as format variables.
deltaUserVars :: OptionsDelta -> Map.Map Text Text
deltaUserVars d = Map.fromList
    [ (nm, v) | (OptUser nm, OVText v) <- deltaEntries d ]

