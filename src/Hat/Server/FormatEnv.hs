-- | Building the @#{…}@ format environment — session, window, and pane —
-- and expanding formats against it: the shared substrate the status line,
-- listing, choose, capture, and display commands format against.
module Hat.Server.FormatEnv
    ( Wanted (..)
    , WantedNames (..)
    , onlyVars
    , wantedNames
    , windowFormatEnv
    , windowVars
    , windowOwnEnv
    , windowOwnVars
    , paneFormatEnv
    , paneVars
    , paneEnvById
    , refreshAutoNames
    , autoName
    , activeClientCounts
    , sessionFormatEnv
    , sessionVars
    , demanding
    , FormatTarget (..)
    , FormatScope (..)
    , targetVars
    , expandAt
    , ExpansionMemo
    , frameMemo
    , keepMemo
    , memoExpand
    , Expand (..)
    , freshExpand
    , memoExpander
    , clientContextFormatEnv
    , paneModeEnv
    , resolveShell
    , expandFormat
    , formatCtxFor
    , clientTheme
    , WindowFlagState (..)
    , windowFlags
    , clientFormatVars
    , clientFormatEnv
    ) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, try)
import Control.Exception qualified
import Data.IntSet qualified as IntSet
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Control.Monad (filterM, forM, forM_, void, when)
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Time.LocalTime (getZonedTime)
import System.Exit (ExitCode (..))
import Network.Socket (getPeerCredential)
import System.Posix.Process (getProcessID)
import System.Posix.Terminal (getSlaveTerminalName)
import System.Posix.User (getRealUserID, getUserEntryForID, userName)
import System.Posix.Unistd (SystemID (nodeName), getSystemID)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import System.Process
    (CreateProcess (..), readCreateProcessWithExitCode, shell)
import Data.Vector qualified as V

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.PtrEq (samePtr)
import Hat.Server.ColorScheme (schemeName)
import Hat.Server.Environ (EnvEntry (..), emptyEnviron, environFind)
import Hat.Server.Format
    ( ClientQueries (..), FormatCtx (..), FormatEnv, UndemandedVar (..), evaluateCtx, formatCtx
    , formatNames, formatVolatile, quoteArgsT, quoteShellSqT, renderFormatCtx )
import Hat.Server.HookTypes (HookAmbient (..), HooksState (..))
import Hat.Server.Floating (FloatState (..), modalPane, paneZ, stacking)
import Hat.Server.Layout
import Hat.Server.LayoutString (emitLayout, emitLayoutZ)
import Hat.Server.Locate (clientOptions, locatePane, paneIndexOf)
import Hat.Server.ControlTypes (ControlFlags (..), ControlState (..), OutputMode (..))
import Hat.Server.Resize (unzoomedArrange, windowArea)
import Hat.Server.CopyMode qualified as CopyMode
import Hat.Server.FormatDraw (styleCell)
import Hat.Server.Render (paneDefaults)
import Hat.Server.Theme (Theme, clientColours, resolveTheme)
import Hat.Version (versionText)
import Hat.Server.Wanted
import Hat.Transport.Wire (ExitWait (..))
import Hat.Server.Winlink (windowLinks)
import Hat.Term.Caps (TtyTerm (..), clientColourCount, featureNames)
import Hat.Term.Cell qualified as Cell
import Hat.Term.Emulator qualified as Emu
import Hat.Term.Pty qualified

import Hat.Server.Keys (KeyState (..))

-- | Just these variables.
onlyVars :: Set.Set Text -> Wanted
onlyVars ns = OnlyVars (wantedNames ns)

-- | Some names, planned: each part of the plan is worked out on first use,
-- once for every holder of the result.
wantedNames :: Set.Set Text -> WantedNames
wantedNames ns = WantedNames
    { names = ns
    , optionNames = filter optionName (Set.toList ns)
    , sessionGroups = givingGroups sessionLayer ns
    , windowGroups = givingGroups windowLayer ns
    , paneGroups = givingGroups paneLayer ns
    , environNames = filter (not . given) (Set.toList ns)
    }
  where
    optionName n = "@" `T.isPrefixOf` n || isJust (lookupOptionDef n)
    given n = optionName n || Map.member n sessionLayer.givers
        || Map.member n windowLayer.givers || Map.member n paneLayer.givers

-- | Variables read together, by the names they give.
type role VarGroup representational
data VarGroup c = VarGroup [Text] (c -> IO [(Text, Text)])

-- | A layer's variables: its groups, and which give each name.
type role VarLayer representational
data VarLayer c = VarLayer
    { groups :: V.Vector (VarGroup c)
    , givers :: Map.Map Text IntSet.IntSet
    }

varLayer :: [VarGroup c] -> VarLayer c
varLayer gs = VarLayer
    { groups = V.fromList gs
    , givers = Map.fromListWith IntSet.union
        [ (n, IntSet.singleton i) | (i, VarGroup ns _) <- zip [0 ..] gs, n <- ns ]
    }

-- | The groups of a layer that give any of some names, in the layer's
-- order.
givingGroups :: VarLayer c -> Set.Set Text -> [Int]
givingGroups l ns =
    IntSet.toAscList (foldMap (\n -> Map.findWithDefault IntSet.empty n l.givers) ns)

-- | A layer's wanted variables, read by just the groups that give them
-- (@groups@ picks those out of a plan), in the layer's order.
layerVars :: VarLayer c -> (WantedNames -> [Int]) -> Wanted -> c -> IO FormatEnv
layerVars l _ AllVars c = Map.fromList . concat <$> mapM (\(VarGroup _ g) -> g c) (V.toList l.groups)
layerVars l groups (OnlyVars w) c = do
    let run i = case l.groups V.! i of VarGroup _ g -> g c
    kvs <- concat <$> mapM run (groups w)
    pure (Map.fromList [ kv | kv@(k, _) <- kvs, k `Set.member` w.names ])

-- | The wanted ones among some variables.
pick :: Wanted -> FormatEnv -> FormatEnv
pick AllVars m = m
pick (OnlyVars w) m = Map.restrictKeys m w.names

-- | What an outer layer need build under an inner one's variables.
unshadowed :: FormatEnv -> Wanted -> Wanted
unshadowed _ AllVars = AllVars
unshadowed inner (OnlyVars w)
    | Set.size rest == Set.size w.names = OnlyVars w
    | otherwise = OnlyVars (wantedNames rest)
        { optionNames = filter (`Set.member` rest) w.optionNames
        , environNames = filter (`Set.member` rest) w.environNames }
  where
    rest = w.names `Set.difference` Map.keysSet inner

-- | The wanted options of a resolved set, as format variables.
optionVars :: Wanted -> Options -> FormatEnv
optionVars AllVars o = optionFormats o
optionVars (OnlyVars w) o =
    Map.fromList [ (n, v) | n <- w.optionNames, Just v <- [optionFormat n o] ]

-- | The wanted entries of one scope's own options, as format variables.
deltaVars :: Wanted -> OptionsDelta -> FormatEnv
deltaVars w d = pick w (deltaFormats d)

windowFormatEnv :: ServerState -> Session -> Int -> Window -> IO FormatEnv
windowFormatEnv st sess ix win = windowVars st sess ix win AllVars

-- | 'windowFormatEnv', built with just the wanted variables.
windowVars :: ServerState -> Session -> Int -> Window -> Wanted -> IO FormatEnv
windowVars st sess ix win w = do
    own <- windowOwnVars st sess ix win w
    Map.union own <$> sessionVars st sess (unshadowed own w)

-- | A window's own variables (and its window options), without its
-- session's: what a @W:@ loop item adds over the session's environment.
windowOwnEnv :: ServerState -> Session -> Int -> Window -> IO FormatEnv
windowOwnEnv st sess ix win = windowOwnVars st sess ix win AllVars

-- | 'windowOwnEnv', built with just the wanted variables.
windowOwnVars :: ServerState -> Session -> Int -> Window -> Wanted -> IO FormatEnv
windowOwnVars st sess ix win w = do
    winUser <- deltaVars w <$> readOwnIO win.options
    Map.union winUser <$> layerVars windowLayer (.windowGroups) w (st, sess, ix, win)

-- | Where a window's own variables are read: its server, session, index
-- in that session, and the window.
type WindowAt = (ServerState, Session, Int, Window)

windowLayer :: VarLayer WindowAt
windowLayer = varLayer
    [ VarGroup ["window_index", "window_id"] $ \(_, _, ix, win) ->
        pure [ ("window_index", tshow ix)
             , ("window_id", "@" <> tshow (rawWindow win.id)) ]
    , VarGroup
        [ "window_name", "window_raw_flags", "window_active", "window_flags"
        , "window_zoomed_flag", "window_modal_pane", "window_bell_flag"
        , "window_activity_flag", "window_silence_flag", "automatic_rename"
        , "window_last_flag" ] $ \(_, sess, ix, win) -> do
        (wname, cur, mlast, bell, act, auto, zoom) <- atomically $ (,,,,,,)
            <$> readTVar win.name
            <*> readTVar sess.currentIx <*> (listToMaybe <$> readTVar sess.windowHist)
            <*> readTVar win.bellFlag <*> readTVar win.activity
            <*> readTVar win.autoRename <*> readTVar win.zoomed
        sil <- readTVarIO win.silenceFlag
        fs <- readTVarIO win.floats
        let flags = windowFlags WindowFlagState
                { flagCurrent = ix == cur
                , flagLast = Just ix == mlast
                , flagBell = bell
                , flagActivity = act
                , flagSilence = sil
                , flagModal = isJust fs.modal
                , flagZoomed = isJust zoom
                }
        pure [ ("window_name", wname)
             , ("window_raw_flags", flags)
             , ("window_active", if ix == cur then "1" else "0")
             , ("window_flags", flags)
             , ("window_zoomed_flag", if isJust zoom then "1" else "0")
             , ("window_modal_pane", maybe "" (\p -> "%" <> tshow (rawPane p)) (modalPane fs))
             , ("window_bell_flag", if bell then "1" else "0")
             , ("window_activity_flag", if act then "1" else "0")
             , ("window_silence_flag", if sil then "1" else "0")
             , ("automatic_rename", if auto then "1" else "0")
             , ("window_last_flag", if Just ix == mlast then "1" else "0")
             ]
    , VarGroup ["window_width", "window_height"] $ \(st, _, _, win) -> do
        eff <- atomically (windowArea st win)
        pure [ ("window_width", tshow eff.cols), ("window_height", tshow eff.rows) ]
    , VarGroup ["window_layout", "window_visible_layout"] $ \(st, _, _, win) -> do
        eff <- atomically (windowArea st win)
        (lay, fs, zoom) <- atomically $
            (,,) <$> readTVar win.layout <*> readTVar win.floats <*> readTVar win.zoomed
        pure [ ("window_layout", emitLayoutZ (stacking fs lay) (fitLayout eff lay))
             , ("window_visible_layout", emitLayout
                 (fitLayout eff (maybe lay singlePane zoom))) ]
    , VarGroup ["window_panes"] $ \(_, _, _, win) -> do
        ps <- readTVarIO win.panes
        pure [ ("window_panes", tshow (Map.size ps)) ]
    , VarGroup ["window_linked", "window_linked_sessions", "window_linked_sessions_list"] $
        \(st, _, _, win) -> do
        (links, linkNames) <- atomically $ do
            links <- map fst <$> windowLinks st win
            (,) links <$> mapM (\s -> readTVar s.name) links
        pure [ ("window_linked", if length links > 1 then "1" else "0")
             -- A session group shares one window map, so it counts once.
             , ("window_linked_sessions", tshow (length
                 (List.nubBy (\a b -> a.windows == b.windows) links)))
             , ("window_linked_sessions_list", T.intercalate "," linkNames) ]
    , VarGroup ["window_active_clients"] $ \(st, _, _, win) -> do
        viewers <- Map.findWithDefault 0 win.id <$> atomically (activeClientCounts st)
        pure [ ("window_active_clients", tshow viewers) ]
    , VarGroup ["window_active_clients_list"] $ \(st, _, _, win) -> atomically $ do
        ss <- readTVar st.sessions
        cs <- Map.elems <$> readTVar st.clients
        viewing <- filterM (\c -> do
            sid <- readTVar c.session
            case Map.lookup sid ss of
                Just s -> maybe False ((== win.id) . (.id)) <$> currentWindow s
                Nothing -> pure False) cs
        pure [ ("window_active_clients_list", T.intercalate "," (map clientName viewing)) ]
    , VarGroup ["window_active_sessions", "window_active_sessions_list"] $
        \(st, _, _, win) -> atomically $ do
        links <- windowLinks st win
        showing <- filterM (\(s, ix) -> (== ix) <$> readTVar s.currentIx) links
        names <- mapM (\(s, _) -> readTVar s.name) showing
        pure [ ("window_active_sessions", tshow (length showing))
             , ("window_active_sessions_list", T.intercalate "," names) ]
    , VarGroup ["window_start_flag", "window_end_flag", "window_stack_index"] $
        \(_, sess, ix, _) -> atomically $ do
        (ws, hist) <- (,) <$> readTVar sess.windows <*> readTVar sess.windowHist
        pure [ ("window_start_flag", flag ((fst <$> Map.lookupMin ws) == Just ix))
             , ("window_end_flag", flag ((fst <$> Map.lookupMax ws) == Just ix))
             , ("window_stack_index"
               , maybe "0" (tshow . (+ 1)) (List.elemIndex ix hist)) ]
    , VarGroup ["window_marked_flag"] $ \(st, sess, _, win) -> atomically $ do
        mp <- readTVar st.markedPane
        marked <- case mp of
            Nothing -> pure False
            Just p -> maybe False (\(sid, w) -> sid == sess.id && w.id == win.id)
                <$> locatePane st p
        pure [ ("window_marked_flag", flag marked) ]
    , VarGroup formatTypeNames $ \_ -> pure (formatType WindowFormat)
    ]

-- | A pane's @pane_pid@: empty unless its program is still running (tmux:
-- an open fd), so it also shadows the session's active-pane value.
panePid :: Pane -> IO Text
panePid pane = do
    mexit <- readTVarIO pane.exited
    pure $ if pane.kind == ProgramPane && isNothing mexit
        then tshow (Hat.Term.Pty.pid pane.pty)
        else ""

-- | The full format environment for a specific pane, including the
-- fields tmux-resurrect's @save.sh@ dumps (pid, command, cursor,
-- history, cwd).
paneFormatEnv
    :: ServerState -> Session -> Int -> Window -> Int -> Pane -> IO FormatEnv
paneFormatEnv st sess wix win pix pane = paneVars st sess wix win pix pane AllVars

-- | 'paneFormatEnv', built with just the wanted variables.
paneVars
    :: ServerState -> Session -> Int -> Window -> Int -> Pane -> Wanted -> IO FormatEnv
paneVars st sess wix win pix pane w = do
    paneUser <- deltaVars w <$> readOwnIO pane.options
    own <- Map.union paneUser <$> layerVars paneLayer (.paneGroups) w (st, sess, win, pix, pane)
    Map.union own <$> windowVars st sess wix win (unshadowed own w)

-- | Where a pane's own variables are read: its server, session, window,
-- index in that window, and the pane.
type PaneAt = (ServerState, Session, Window, Int, Pane)

paneLayer :: VarLayer PaneAt
paneLayer = varLayer
    -- A signal is reported by number, as tmux does where libc has no
    -- @sys_signame@ (glibc).
    [ VarGroup formatTypeNames $ \_ -> pure (formatType PaneFormat)
    , VarGroup ["pane_last"] $ \(_, _, win, _, pane) -> do
        hist <- readTVarIO win.paneHist
        pure [ ("pane_last", flag (listToMaybe hist == Just pane.id)) ]
    , VarGroup ["pane_pipe"] $ \(_, _, _, _, pane) -> do
        piped <- isJust <$> readTVarIO pane.pipe
        pure [ ("pane_pipe", flag piped) ]
    , VarGroup ["pane_start_command", "pane_start_command_list", "pane_start_path"] $
        \(_, _, _, _, pane) -> do
        let argv = case pane.start of
                FreshShell -> []
                ShellCommand c -> [c]
                ExecArgv a -> a
                ShellInject a -> a
        pure [ ("pane_start_command", T.unwords (map quoteArgsT argv))
             , ("pane_start_command_list", T.unwords (map quoteShellSqT argv))
             , ("pane_start_path", T.pack pane.startCwd) ]
    , VarGroup ["pane_tty"] $ \(_, _, _, _, pane) -> do
        tty <- (Just . T.pack <$> getSlaveTerminalName (Hat.Term.Pty.masterFd pane.pty))
            `Control.Exception.catch` \(_ :: IOException) -> pure Nothing
        pure [ ("pane_tty", t) | Just t <- [tty] ]
    , VarGroup
        [ "cursor_flag", "insert_flag", "keypad_cursor_flag", "keypad_flag"
        , "origin_flag", "wrap_flag", "bracket_paste_flag"
        , "synchronized_output_flag" ] $ \(_, _, _, _, pane) -> do
        m <- Emu.vtModes pane.emulator
        pure [ ("cursor_flag", flag m.cursorVisible)
             , ("insert_flag", flag m.insertMode)
             , ("keypad_cursor_flag", flag m.cursorKeys)
             , ("keypad_flag", flag m.keypadKeys)
             , ("origin_flag", flag m.originMode)
             , ("wrap_flag", flag m.autoWrap)
             , ("bracket_paste_flag", flag m.bracketPaste)
             , ("synchronized_output_flag", flag m.syncOutput) ]
    , VarGroup ["history_limit"] $ \(st, sess, win, _, pane) -> do
        opts <- resolveForPaneIO st sess win pane
        pure [ ("history_limit", tshow (optionNumber "history-limit" opts)) ]
    , VarGroup ["pane_dead", "pane_dead_time", "pane_dead_status", "pane_dead_signal"] $
        \(_, _, _, _, pane) -> readTVarIO pane.exited <&> \case
        Nothing -> [("pane_dead", "0")]
        Just e ->
            [ ("pane_dead", "1")
            , ("pane_dead_time", tshow (floor e.diedAt :: Integer)) ]
            <> [ ("pane_dead_status", tshow n) | Just n <- [e.status] ]
            <> [ ("pane_dead_signal", tshow n) | Just n <- [e.signal] ]
    , VarGroup ["pane_pid"] $ \(_, _, _, _, pane) -> do
        pid <- panePid pane
        pure [ ("pane_pid", pid) ]
    , VarGroup
        [ "mouse_any_flag", "mouse_standard_flag", "mouse_button_flag"
        , "mouse_all_flag", "mouse_sgr_flag", "mouse_utf8_flag", "alternate_on" ] $
        \(_, _, _, _, pane) -> do
        ms <- Emu.modes pane.emulator
        (sgr, utf8) <- Emu.mouseEncoding pane.emulator
        pure [ ("mouse_any_flag", flag (ms.mouse /= Emu.MouseOff))
             , ("mouse_standard_flag", flag (ms.mouse == Emu.MouseClick))
             , ("mouse_button_flag", flag (ms.mouse == Emu.MouseDrag))
             , ("mouse_all_flag", flag (ms.mouse == Emu.MouseMove))
             , ("mouse_sgr_flag", flag sgr)
             , ("mouse_utf8_flag", flag utf8)
             , ("alternate_on", flag ms.altScreen) ]
    , VarGroup ["pane_id", "pane_index"] $ \(_, _, _, pix, pane) ->
        pure [ ("pane_id", "%" <> tshow (rawPane pane.id))
             , ("pane_index", tshow pix) ]
    , VarGroup
        [ "pane_active", "pane_marked", "pane_marked_set", "pane_input_off"
        , "pane_width", "pane_height" ] $ \(st, _, win, _, pane) -> do
        (active, marked, inputOff, sz) <- atomically $ (,,,)
            <$> readTVar win.activeId <*> readTVar st.markedPane
            <*> readTVar pane.inputOff <*> readTVar pane.size
        pure [ ("pane_active", if pane.id == active then "1" else "0")
             , ("pane_marked", if marked == Just pane.id then "1" else "0")
             , ("pane_marked_set", if isJust marked then "1" else "0")
             , ("pane_input_off", if inputOff then "1" else "0")
             , ("pane_width", tshow sz.cols)
             , ("pane_height", tshow sz.rows) ]
    , VarGroup ["pane_modal_flag"] $ \(_, _, win, _, pane) -> do
        fs <- readTVarIO win.floats
        pure [ ("pane_modal_flag", if modalPane fs == Just pane.id then "1" else "0") ]
    , VarGroup paneModeNames $ \(st, _, _, _, pane) -> paneModeEnv st pane
    , VarGroup ["pane_current_path"] $ \(_, _, _, _, pane) -> do
        dir <- paneCurrentPath pane
        pure [ ("pane_current_path", T.pack dir) ]
    , VarGroup ["pane_current_command"] $ \(_, _, _, _, pane) -> do
        cmd <- paneCommandName pane
        pure [ ("pane_current_command", cmd) ]
    , VarGroup ["pane_synchronized"] $ \(st, sess, win, _, pane) -> do
        synced <- optionFlag "synchronize-panes"
            <$> resolveForPaneIO st sess win pane
        pure [ ("pane_synchronized", if synced then "1" else "0") ]
    , VarGroup ["pane_title"] $ \(_, _, _, _, pane) -> do
        title <- Emu.title pane.emulator
        pure [ ("pane_title", title) ]
    , VarGroup ["cursor_x", "cursor_y", "cursor_character"] $ \(_, _, _, _, pane) -> do
        scr <- Emu.snapshot pane.emulator
        pendingWrap <- Emu.cursorPendingWrap pane.emulator
        pure [ ("cursor_x", tshow (scr.cursor.col + fromEnum pendingWrap))
             , ("cursor_y", tshow scr.cursor.row)
             , ("cursor_character", case (Emu.screenCell scr scr.cursor).content of
                   Cell.Glyph c cs _ -> T.pack (c : cs)
                   Cell.Continuation -> "") ]
    , VarGroup ["history_size"] $ \(_, _, _, _, pane) -> do
        hsize <- Emu.scrollbackLength pane.emulator
        pure [ ("history_size", tshow hsize) ]
    , VarGroup ["pane_fg", "pane_bg"] $ \(st, sess, win, _, pane) -> do
        (dfg, dbg) <- paneColourVars st sess win pane
        pure [ ("pane_fg", dfg), ("pane_bg", dbg) ]
    , VarGroup ["pane_unzoomed_width", "pane_unzoomed_height"] $ \(st, _, win, _, pane) -> do
        area <- atomically (windowArea st win)
        unzoomed <- atomically (List.lookup pane.id <$> unzoomedArrange st area win)
        pure $ concat [ [ ("pane_unzoomed_width", tshow (r.endCol - r.startCol))
                        , ("pane_unzoomed_height", tshow (r.endRow - r.startRow)) ]
                      | Just r <- [unzoomed] ]
    , VarGroup
        [ "pane_floating_flag", "pane_z", "pane_flags"
        , "pane_left", "pane_top", "pane_x", "pane_y", "pane_right", "pane_bottom"
        , "pane_at_left", "pane_at_top", "pane_at_right", "pane_at_bottom" ] $
        \(st, _, win, _, pane) -> do
        active <- readTVarIO win.activeId
        area <- atomically (windowArea st win)
        (geom, floatVars) <- atomically $ do
            zoomed <- readTVar win.zoomed
            lay <- fitLayout area <$> readTVar win.layout
            fs <- readTVar win.floats
            hist <- readTVar win.paneHist
            -- tmux's zoom swaps the whole tree for the zoomed pane alone, so
            -- nothing floats while a window is zoomed.
            let floating = isNothing zoomed && isFloating pane.id lay
                modal = modalPane fs == Just pane.id
                letters = T.concat
                    [ if pane.id == active then "*" else ""
                    , if listToMaybe hist == Just pane.id then "-" else ""
                    , if zoomed == Just pane.id then "Z" else ""
                    , if floating then "F" else ""
                    , if modal then "O" else "" ]
            pure ( if zoomed == Just pane.id
                    then Just (0, 0, fromIntegral area.cols, fromIntegral area.rows)
                    else (\c -> (c.xoff, c.yoff, c.sx, c.sy)) <$> paneCell pane.id lay
                 , [ ("pane_floating_flag", if floating then "1" else "0")
                   , ("pane_z", tshow (paneZ fs lay pane.id))
                   , ("pane_flags", letters) ] )
        pure (floatVars <> panePositionVars area geom)
    ]

-- | @pane_fg@ and @pane_bg@: the colours a pane's default-coloured cells
-- draw in (see 'Hat.Server.Render.paneDefaults'), its styles read as
-- written.
paneColourVars :: ServerState -> Session -> Window -> Pane -> IO (Text, Text)
paneColourVars st sess win pane = do
    th <- clientTheme st
    (popts, active) <- atomically $
        (,) <$> resolveForPane st sess win pane <*> readTVar win.activeId
    (pfg, pbg) <- Emu.appDefaultColours pane.emulator
    let rgb = maybe Cell.DefaultColor (\(r, g, b) -> Cell.RGB r g b)
        style n = styleCell th (optionText n popts)
        (fg, bg) = paneDefaults (pane.id == active) (rgb pfg, rgb pbg)
            (style "window-style") (style "window-active-style")
    pure (colourName fg, colourName bg)

-- | tmux's @colour_tostring@ for a cell colour.
colourName :: Cell.Color -> Text
colourName = \case
    Cell.DefaultColor -> "default"
    Cell.Indexed n
        | n < 8 -> names !! fromIntegral n
        | n < 16 -> "bright" <> names !! fromIntegral (n - 8)
        | otherwise -> "colour" <> tshow n
    Cell.RGB r g b -> T.pack ('#' : concatMap hex2 [r, g, b])
  where
    names = ["black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"]
    hex2 w = [digit (w `div` 16), digit (w `mod` 16)]
    digit d = "0123456789abcdef" !! fromIntegral d

-- | A pane's position in its window (tmux's @pane_left@ … @pane_at_bottom@),
-- from its cell: @(xoff, yoff, width, height)@.
panePositionVars :: Size -> Maybe (Int, Int, Int, Int) -> [(Text, Text)]
panePositionVars area = \case
    Nothing -> []
    Just (x, y, w, h) ->
        [ ("pane_left", tshow x)
        , ("pane_top", tshow y)
        , ("pane_x", tshow x)
        , ("pane_y", tshow y)
        , ("pane_right", tshow (x + w - 1))
        , ("pane_bottom", tshow (y + h - 1))
        , ("pane_at_left", flag (x == 0))
        , ("pane_at_top", flag (y == 0))
        , ("pane_at_right", flag (x + w == fromIntegral area.cols))
        , ("pane_at_bottom", flag (y + h == fromIntegral area.rows))
        ]

-- | 'paneFormatEnv' for a pane named only by id: locate its window and
-- session first.
paneEnvById :: ServerState -> PaneId -> IO (Maybe FormatEnv)
paneEnvById st pid = do
    mctx <- atomically $ do
        mloc <- locatePane st pid
        case mloc of
            Nothing -> pure Nothing
            Just (sid, win) -> do
                msess <- Map.lookup sid <$> readTVar st.sessions
                case msess of
                    Nothing -> pure Nothing
                    Just sess -> do
                        ws <- readTVar sess.windows
                        ps <- readTVar win.panes
                        let mwix = listToMaybe
                                [ i | (i, w) <- Map.toList ws, w.id == win.id ]
                        pure $ (,,,) sess <$> mwix <*> Just win
                            <*> Map.lookup pid ps
    case mctx of
        Nothing -> pure Nothing
        Just (sess, wix, win, pane) -> do
            pix <- paneIndexOf st win pane
            Just <$> paneFormatEnv st sess wix win pix pane

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
sessionFormatEnv st sess = sessionVars st sess AllVars

-- | 'sessionFormatEnv', built with just the wanted variables.
sessionVars :: ServerState -> Session -> Wanted -> IO FormatEnv
sessionVars st sess w = do
    -- Options are readable as #{status-left}, #{@foo}, … so if-shell theme
    -- conditionals (@#{@pane-theme}@) resolve.
    sessOpts <- optionVars w <$> resolveForSessionIO st sess
    envVars <- environVars st sess w
    own <- layerVars sessionLayer (.sessionGroups) w (st, sess)
    pure (Map.unions [sessOpts, own, envVars])

-- | The wanted environment variables ('targetEnviron') no other variable
-- gives. See Note [Environment variables in formats].
environVars :: ServerState -> Session -> Wanted -> IO FormatEnv
environVars _ _ AllVars = pure Map.empty
environVars st sess (OnlyVars w)
    | null w.environNames = pure Map.empty
    | otherwise = do
        look <- sessionEnviron st (Just sess)
        pure (Map.fromList [ (n, v) | n <- w.environNames, Just v <- [look n] ])

sessionLayer :: VarLayer (ServerState, Session)
sessionLayer = varLayer
    [ VarGroup ["session_name"] $ \(_, sess) -> do
        sname <- readTVarIO sess.name
        pure [ ("session_name", sname) ]
    , VarGroup ["session_id", "session_created", "session_restores"] $ \(_, sess) ->
        pure [ ("session_id", "$" <> tshow (rawSession sess.id))
             , ("session_created", tshow (floor sess.createdAt :: Integer))
             , ("session_restores", tshow sess.restores) ]
    , VarGroup ["session_windows"] $ \(_, sess) -> do
        nwindows <- Map.size <$> readTVarIO sess.windows
        pure [ ("session_windows", tshow nwindows) ]
    , VarGroup serverNames $ \(st, _) -> serverFormatVars st
    , VarGroup ["session_attached"] $ \(st, sess) -> do
        cs <- atomically (sessionClients st sess.id)
        pure [ ("session_attached", tshow (length cs)) ]
    , VarGroup ["window_width", "window_height"] $ \(_, sess) -> do
        sz <- readTVarIO sess.lastSize
        pure [ ("window_width", tshow sz.cols), ("window_height", tshow sz.rows) ]
    , VarGroup ["color_scheme"] $ \(st, _) -> do
        msch <- readTVarIO st.colorScheme
        pure [ ("color_scheme", maybe "" schemeName msch) ]
    , VarGroup ["window_index", "window_id"] $ \(_, sess) -> do
        (mwin, cur) <- atomically $ (,) <$> currentWindow sess <*> readTVar sess.currentIx
        pure $ case mwin of
            Nothing -> []
            Just win ->
                [ ("window_index", tshow cur)
                , ("window_id", "@" <> tshow (rawWindow win.id)) ]
    , VarGroup ["window_name"] $ \(_, sess) -> atomically $ currentWindow sess >>= \case
        Nothing -> pure []
        Just win -> do
            wname <- readTVar win.name
            pure [ ("window_name", wname) ]
    , VarGroup ["window_active_clients"] $ \(st, sess) -> do
        (counts, mwin) <- atomically $ (,) <$> activeClientCounts st <*> currentWindow sess
        pure $ ("window_active_clients", "0") : case mwin of
            Nothing -> []
            Just win ->
                [ ( "window_active_clients"
                  , tshow (Map.findWithDefault 0 win.id counts) ) ]
    , VarGroup
        [ "session_grouped", "session_group", "session_group_size"
        , "session_group_list", "session_group_attached", "session_group_many_attached" ] $
        \(st, sess) -> atomically $ readTVar sess.group >>= \case
        Nothing -> pure [("session_grouped", "0")]
        Just g -> do
            ss <- Map.elems <$> readTVar st.sessions
            members <- filterM (fmap (== Just g) . readTVar . (.group)) ss
            names <- mapM (\m -> readTVar m.name) members
            attached <- sum <$> mapM (fmap length . sessionClients st . (.id)) members
            pure [ ("session_grouped", "1")
                 , ("session_group", g)
                 , ("session_group_size", tshow (length members))
                 , ("session_group_list", T.intercalate "," names)
                 , ("session_group_attached", tshow attached)
                 , ("session_group_many_attached", if attached > 1 then "1" else "0")
                 ]
    , VarGroup ["session_alert", "session_alerts"] $ \(_, sess) ->
        atomically (sessionAlerts sess)
    , VarGroup
        [ "active_window_index", "last_window_index", "session_stack"
        , "session_path" ] $ \(_, sess) -> atomically $ do
        (cur, ws, hist, dir) <- (,,,) <$> readTVar sess.currentIx
            <*> readTVar sess.windows <*> readTVar sess.windowHist
            <*> readTVar sess.startCwd
        pure $ [ ("active_window_index", tshow cur)
               , ("session_stack", T.intercalate "," (map tshow (cur : hist)))
               , ("session_path", T.pack dir) ]
            <> [ ("last_window_index", tshow ix) | Just (ix, _) <- [Map.lookupMax ws] ]
    , VarGroup ["session_attached_list", "session_many_attached"] $ \(st, sess) -> do
        cs <- atomically (sessionClients st sess.id)
        pure [ ("session_attached_list", T.intercalate "," (map clientName cs))
             , ("session_many_attached", flag (length cs > 1)) ]
    , VarGroup ["session_group_attached_list"] $ \(st, sess) -> atomically $
        readTVar sess.group >>= \case
        Nothing -> pure []
        Just g -> do
            ss <- readTVar st.sessions
            cs <- Map.elems <$> readTVar st.clients
            inGroup <- filterM (\c -> do
                sid <- readTVar c.session
                case Map.lookup sid ss of
                    Just s -> (== Just g) <$> readTVar s.group
                    Nothing -> pure False) cs
            pure [ ("session_group_attached_list", T.intercalate "," (map clientName inGroup)) ]
    , VarGroup ["session_marked"] $ \(st, sess) -> atomically $ do
        mp <- readTVar st.markedPane
        marked <- case mp of
            Nothing -> pure False
            Just p -> maybe False ((== sess.id) . fst) <$> locatePane st p
        pure [ ("session_marked", flag marked) ]
    , VarGroup formatTypeNames $ \_ -> pure (formatType SessionFormat)
    -- The active pane's.
    , VarGroup ["pane_current_path"] $ activePaneVars $ \_ pane -> do
        dir <- paneCurrentPath pane
        pure [ ("pane_current_path", T.pack dir) ]
    , VarGroup ["pane_current_command"] $ activePaneVars $ \_ pane -> do
        cmd <- paneCommandName pane
        pure [ ("pane_current_command", cmd) ]
    , VarGroup ["pane_pid"] $ activePaneVars $ \_ pane -> do
        pid <- panePid pane
        pure [ ("pane_pid", pid) ]
    , VarGroup ["pane_active", "pane_id"] $ activePaneVars $ \_ pane ->
        pure [ ("pane_active", "1"), ("pane_id", "%" <> tshow (rawPane pane.id)) ]
    , VarGroup ["pane_title"] $ activePaneVars $ \_ pane -> do
        title <- Emu.title pane.emulator
        pure [ ("pane_title", title) ]
    , VarGroup paneModeNames $ activePaneVars paneModeEnv
    ]
  where
    activePaneVars f (st, sess) = do
        mactive <- atomically $ currentWindow sess >>= maybe (pure Nothing) activePane
        maybe (pure []) (f st) mactive

-- | The variables 'paneModeEnv' gives.
paneModeNames :: [Text]
paneModeNames = ["pane_in_mode", "pane_mode"] <> CopyMode.copyModeNames

-- | tmux's @session_alert@ (each alert kind once, in @#!~@ order) and
-- @session_alerts@ (each alerted window's index and its alert marks).
sessionAlerts :: Session -> STM [(Text, Text)]
sessionAlerts sess = do
    ws <- Map.toAscList <$> readTVar sess.windows
    marks <- forM ws $ \(ix, win) -> do
        (act, bell, sil) <- (,,) <$> readTVar win.activity
            <*> readTVar win.bellFlag <*> readTVar win.silenceFlag
        pure (ix, [ c | (on, c) <- [(act, '#'), (bell, '!'), (sil, '~')], on ])
    let alerted = filter (not . null . snd) marks
        kinds = List.foldl' (\seen c -> if c `elem` seen then seen else seen <> [c])
            [] (concatMap snd alerted)
    pure [ ("session_alert", T.pack kinds)
         , ("session_alerts", T.intercalate ","
             [ tshow ix <> T.pack cs | (ix, cs) <- alerted ]) ]

-- | Which kind of target a format expands at (tmux's @ft->type@).
data FormatType = SessionFormat | WindowFormat | PaneFormat
    deriving (Eq)

-- | The variables 'formatType' gives.
formatTypeNames :: [Text]
formatTypeNames = ["session_format", "window_format", "pane_format"]

-- | @session_format@, @window_format@ and @pane_format@: which one the
-- target is. The innermost layer's stand over the outer ones'.
formatType :: FormatType -> [(Text, Text)]
formatType t =
    [ ("session_format", flag (t == SessionFormat))
    , ("window_format", flag (t == WindowFormat))
    , ("pane_format", flag (t == PaneFormat)) ]

flag :: Bool -> Text
flag b = if b then "1" else "0"

-- | Copy-mode format variables for a pane: @pane_in_mode@/@pane_mode@,
-- plus the copy-mode variables while in copy mode.
paneModeEnv :: ServerState -> Pane -> IO [(Text, Text)]
paneModeEnv st pane = do
    mmode <- atomically (topMode pane)
    case mmode of
        Nothing -> pure [("pane_in_mode", "0"), ("pane_mode", "")]
        Just (CopyMode pm) -> do
            global <- readTVarIO st.options
            opts <- maybe global id <$> paneOptionsNow st pane
            let numbering = CopyMode.lineNumberModeOf (optionText "copy-mode-line-numbers" opts)
            pure $ [ ("pane_in_mode", "1"), ("pane_mode", modeName (CopyMode pm)) ]
                <> CopyMode.copyModeFormats global.wordSeparators numbering pm
        Just m -> pure [("pane_in_mode", "1"), ("pane_mode", modeName m)]

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

-- | tmux's format context for a command with no target: the issuing
-- client's session when it has one, else only the global user options.
clientContextFormatEnv :: ServerState -> Maybe Client -> IO FormatEnv
clientContextFormatEnv st mclient = do
    msess <- case mclient of
        Nothing -> pure Nothing
        Just c -> atomically $ do
            sid <- readTVar c.session
            Map.lookup sid <$> readTVar st.sessions
    case msess of
        Just sess -> sessionFormatEnv st sess
        Nothing -> Map.union <$> ((.user) <$> readTVarIO st.options)
            <*> (Map.fromList <$> serverFormatVars st)

-- | The variables 'serverFormatVars' gives.
serverNames :: [Text]
serverNames =
    [ "host", "host_short", "pid", "socket_path", "start_time", "uid", "user"
    , "next_session_id", "server_sessions", "config_files", "sixel_support"
    , "buffer_mode_format", "client_mode_format", "tree_mode_format", "version" ]

-- | The formats that describe the server process itself.
serverFormatVars :: ServerState -> IO [(Text, Text)]
serverFormatVars st = do
    hostname <- T.pack . nodeName <$> getSystemID
    pid <- getProcessID
    uid <- getRealUserID
    user <- (Just . T.pack . userName <$> getUserEntryForID uid)
        `Control.Exception.catch` \(_ :: IOException) -> pure Nothing
    (nextId, nsessions, mconfig) <- atomically $ (,,)
        <$> readTVar st.nextSession <*> (Map.size <$> readTVar st.sessions)
        <*> readTVar st.serverConfig
    pure $
        [ ("host", hostname)
        , ("host_short", T.takeWhile (/= '.') hostname)
        , ("pid", tshow pid)
        , ("socket_path", T.pack st.sockPath)
        , ("start_time", tshow (floor st.startTime :: Integer))
        , ("uid", tshow uid)
        , ("next_session_id", "$" <> tshow nextId)
        , ("server_sessions", tshow nsessions)
        , ("config_files", maybe "" T.pack mconfig)
        -- hat's emulator draws no sixel images
        , ("sixel_support", "0")
        , ("buffer_mode_format", "#{t/p:buffer_created}: #{buffer_sample}")
        , ("client_mode_format"
          , "#[fg=themelightgrey]#{t/p:client_activity}: session #[default]#{session_name}")
        , ("tree_mode_format", treeModeFormat)
        , ("version", versionText)
        ]
        <> [ ("user", u) | Just u <- [user] ]

-- | tmux's @WINDOW_TREE_DEFAULT_FORMAT@.
treeModeFormat :: Text
treeModeFormat = T.concat
    [ "#{?pane_format,"
    ,     "#{?pane_marked,#[fg=thememagenta],}#{?pane_floating_flag,#[underscore],}"
    ,     "#{pane_current_command}#[fg=themelightgrey]#{pane_flags}"
    ,     "#{?#{&&:#{pane_title},#{!=:#{pane_title},#{host_short}}},: \"#{pane_title}\",}"
    , ",window_format,"
    ,     "#{?window_marked_flag,#[fg=thememagenta],}"
    ,     "#{window_name}#[fg=themelightgrey]#{window_flags}"
    ,     "#{?#{&&:#{==:#{window_panes},1},#{&&:#{pane_title},#{!=:#{pane_title},#{host_short}}}},: \"#{pane_title}\",}"
    , ","
    ,     "#[fg=themelightgrey]#{session_windows} windows"
    ,     "#{?session_grouped, "
    ,         "(group #{session_group}: "
    ,         "#{session_group_list}),"
    ,     "}"
    ,     "#{?session_attached, (attached),}"
    , "}" ]

-- | Expand a format string fully: #{...}, cached #(...), then strftime.
-- Inside a hook, the event's payload formats (#{hook}, #{hook_pane}, …)
-- overlay the caller's environment.
expandFormat :: ServerState -> FormatEnv -> Text -> IO Text
expandFormat st env fmt = do
    ctx <- formatCtxFor st env
    pure $! renderFormatCtx ctx fmt

-- | The evaluation context for expanding formats against @env0@: the hook
-- overlay, @#(…)@ commands resolved through the cache, and the loop and
-- search data. Each is read only if an expansion reaches it, so a job in
-- an untaken branch never runs (as in tmux).
formatCtxFor :: ServerState -> FormatEnv -> IO FormatCtx
formatCtxFor st env0 = do
    tid <- myThreadId
    mamb <- Map.lookup tid <$> readTVarIO st.hooks.ambient
    let env = maybe env0 (\amb -> Map.union amb.formats env0) mamb
    now <- getZonedTime
    -- A command is itself a format, expanded (as tmux's format_job_get
    -- does) without jobs, animation or strftime. The caller's strict use
    -- of the expansion keeps the deferred reads inside its own call.
    let jobCtx = formatCtx env (const "") now
        job inner = unsafePerformIO (resolveShell st (evaluateCtx jobCtx inner))
    sessionItems <- unsafeInterleaveIO (loopSessionEnvs st)
    windowItems <- unsafeInterleaveIO (loopWindowEnvs st env)
    paneItems <- unsafeInterleaveIO (loopPaneEnvs st env)
    clientItems <- unsafeInterleaveIO (loopClientEnvs st)
    visibleLines <- unsafeInterleaveIO (targetPaneLines st env)
    environment <- unsafeInterleaveIO (targetEnviron st env)
    queries <- unsafeInterleaveIO (clientQueries st env)
    th <- clientTheme st
    pure (formatCtx env job now)
        { theme = th
        , environLookup = environment
        , client = queries
        , sessions = sessionItems
        , windows = windowItems
        , panes = paneItems
        , clients = clientItems
        , paneLines = visibleLines
        }

-- | The variables 'formatCtxFor' reads its loop and search data through.
contextIds :: Set.Set Text
contextIds = Set.fromList ["session_id", "window_id", "pane_id"]

-- | Expand against just the variables some formats read: their names
-- ('formatNames'), those of every value they reach that is itself a
-- format, the context ids, and any further one an expansion turns out to
-- look up ('UndemandedVar'). @expand@ forces everything it reads. Also
-- the names the variables were built for, and the variables.
demanding
    :: ServerState -> (Wanted -> IO FormatEnv) -> [Text] -> (FormatCtx -> IO a)
    -> IO (Set.Set Text, FormatEnv, a)
demanding st build formats expand =
    grow Set.empty Map.empty (contextIds <> foldMap formatNames formats)
  where
    grow asked env pending
        | Set.null pending = attempt asked env
        | otherwise = do
            more <- build (onlyVars pending)
            let asked' = asked <> pending
                reached = foldMap formatNames [ v | v <- Map.elems more, "#" `T.isInfixOf` v ]
            grow asked' (Map.union env more) (reached `Set.difference` asked')
    attempt asked env = do
        ctx <- formatCtxFor st env
        r <- try (expand ctx { demanded = Just asked })
        case r of
            Right a -> pure (asked, env, a)
            Left (UndemandedVar n) -> grow asked env (Set.singleton n)

-- | Where a format expands: a session, window or pane, with variables
-- given over its own.
data FormatTarget = FormatTarget
    { scope   :: FormatScope
    , overlay :: FormatEnv
    }

data FormatScope
    = InSession Session
    | InWindow Session Int Window  -- ^ the window and its index
    | InPane Session Int Window Int Pane  -- ^ … and the pane and its index

-- | A target's variables, built with just the wanted ones.
targetVars :: ServerState -> FormatTarget -> Wanted -> IO FormatEnv
targetVars st t w = Map.union (pick w t.overlay) <$> case t.scope of
    InSession sess -> sessionVars st sess w
    InWindow sess wix win -> windowVars st sess wix win w
    InPane sess wix win pix pane -> paneVars st sess wix win pix pane w

-- | Expand a format at a target, building just the variables it reads.
expandAt :: ServerState -> FormatTarget -> Text -> IO Text
expandAt st t fmt = do
    (_, _, txt) <- demanding st (targetVars st t) [fmt] (expandForced fmt)
    pure txt

expandForced :: Text -> FormatCtx -> IO Text
expandForced fmt ctx = Control.Exception.evaluate (renderFormatCtx ctx fmt)

-- | A frame's expansions: the last frame's, kept, and this one's so far.
data ExpansionMemo = ExpansionMemo
    { kept :: Map.Map ExpansionKey KeptExpansion
    , made :: IORef (Map.Map ExpansionKey KeptExpansion)
    , keptStyles :: Map.Map Text (Theme, Cell.Style)
    , madeStyles :: IORef (Map.Map Text (Theme, Cell.Style))
    }

-- | Start a client's frame with the expansions its last frame kept.
frameMemo :: Client -> IO ExpansionMemo
frameMemo client = ExpansionMemo
    <$> readIORef client.lastExpansions <*> newIORef Map.empty
    <*> readIORef client.lastStyles <*> newIORef Map.empty

-- | End a client's frame, keeping just the expansions it made.
keepMemo :: Client -> ExpansionMemo -> IO ()
keepMemo client memo = do
    readIORef memo.made >>= writeIORef client.lastExpansions
    readIORef memo.madeStyles >>= writeIORef client.lastStyles

-- | How a frame expands formats at a target and parses styles.
data Expand = Expand
    { format :: FormatTarget -> Text -> IO Text
    , style  :: Theme -> Text -> IO Cell.Style
    }

-- | Expand and parse afresh ('expandAt', 'styleCell').
freshExpand :: ServerState -> Expand
freshExpand st = Expand { format = expandAt st, style = \th -> pure . styleCell th }

-- | Expand and parse through a frame's memo ('memoExpand', 'memoStyle').
memoExpander :: ServerState -> ExpansionMemo -> Expand
memoExpander st memo = Expand { format = memoExpand st memo, style = memoStyle memo }

-- | 'styleCell' through a frame's memo: the last frame's parse of the same
-- text under the same theme stands.
memoStyle :: ExpansionMemo -> Theme -> Text -> IO Cell.Style
memoStyle memo th raw = do
    let sty = case Map.lookup raw memo.keptStyles of
            Just (th', s) | samePtr th' th || th' == th -> s
            _ -> styleCell th raw
    modifyIORef' memo.madeStyles (Map.insert raw (th, sty))
    pure sty

-- | 'expandAt' through a frame's memo: an expansion the last frame kept
-- stands while every variable it can read is unchanged. One that can
-- change with no variable changing ('formatVolatile') is never kept, nor
-- one made in a hook's context.
memoExpand :: ServerState -> ExpansionMemo -> FormatTarget -> Text -> IO Text
memoExpand st memo t fmt = do
    tid <- myThreadId
    ambient <- Map.member tid <$> readTVarIO st.hooks.ambient
    let key = ExpansionKey
            { place = (sess.id, (.id) <$> mwin, (.id) <$> mpane)
            , overlay = t.overlay, source = fmt }
    hit <- case Map.lookup key memo.kept of
        Just k | not ambient -> do
            now <- targetVars st t (OnlyVars k.names)
            pure (if now == k.vars then Just k else Nothing)
        _ -> pure Nothing
    case hit of
        Just k -> do
            keep key k
            pure k.text
        Nothing -> do
            (names, vars, text) <- demanding st (targetVars st t) [fmt] (expandForced fmt)
            when (not ambient && not (formatVolatile fmt vars)) $
                keep key KeptExpansion
                    { names = wantedNames (names `Set.difference` placeIds)
                    , vars = Map.withoutKeys vars placeIds, text }
            pure text
  where
    keep key k = modifyIORef' memo.made (Map.insert key k)
    -- The place's own ids are the key's, so they stand while it does.
    placeIds = Set.fromList $
        ["session_id"] <> ["window_id" | isJust mwin] <> ["pane_id" | isJust mpane]
    (sess, mwin, mpane) = case t.scope of
        InSession s -> (s, Nothing, Nothing)
        InWindow s _ w -> (s, Just w, Nothing)
        InPane s _ w _ p -> (s, Just w, Just p)

-- | The theme a hat client draws with (see 'resolveTheme'): every client
-- shares the server's colour scheme and draws RGB colours.
clientTheme :: ServerState -> IO Theme
clientTheme st = do
    opts <- readTVarIO st.options
    scheme <- readTVarIO st.colorScheme
    cached <- readTVarIO st.themeCache
    case cached of
        Just c | samePtr c.opts opts, c.scheme == scheme -> pure c.theme
        _ -> do
            -- The palette formats see only @client_colours@, so the theme
            -- follows from the options and the scheme alone.
            now <- getZonedTime
            let ctx = formatCtx (Map.singleton "client_colours" clientColours) (const "") now
                theme = resolveTheme (evaluateCtx ctx) opts scheme
            atomically (writeTVar st.themeCache (Just ThemeCache { opts, scheme, theme }))
            pure theme

-- | The conditions that produce a window's @#{window_flags}@ string.
data WindowFlagState = WindowFlagState
    { flagCurrent  :: Bool
    , flagLast     :: Bool
    , flagBell     :: Bool
    , flagActivity :: Bool
    , flagSilence  :: Bool
    , flagModal    :: Bool  -- ^ the window has a modal pane
    , flagZoomed   :: Bool
    }

-- | Render the window-status flags in tmux's order: current (@*@) or
-- last (@-@), then bell (@!@), activity (@#@) and silence (@~@), and
-- a modal pane (@O@), and finally zoom (@Z@) when the window has a pane
-- zoomed to fill it.
windowFlags :: WindowFlagState -> Text
windowFlags s = T.concat
    [ if s.flagCurrent then "*"
      else if s.flagLast then "-" else ""
    , if s.flagBell then "!" else ""
    , if s.flagActivity then "#" else ""
    , if s.flagSilence then "~" else ""
    , if s.flagModal then "O" else ""
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

-- Note [Environment variables in formats]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A name no option or format variable gives reads the target session's
-- environment, then the global one (tmux's format_find). A build of just
-- the wanted variables includes those values ('environVars'), so whatever
-- keeps an expansion while its variables stand also sees them change; an
-- environment built with every variable leaves them to the context's
-- lookup ('targetEnviron'), as no build can enumerate them.

-- | The target session's environment as 'sessionEnviron' reads it. See
-- Note [Environment variables in formats].
targetEnviron :: ServerState -> FormatEnv -> IO (Text -> Maybe Text)
targetEnviron st env = targetSessionOf st env >>= sessionEnviron st

-- | A variable of a session's environment, else the global one; a cleared
-- session entry masks the global.
sessionEnviron :: ServerState -> Maybe Session -> IO (Text -> Maybe Text)
sessionEnviron st msess = do
    (global, own) <- atomically $ (,) <$> readTVar st.globalEnviron
        <*> maybe (pure emptyEnviron) (readTVar . (.environ)) msess
    pure $ \n -> (.value) =<< (environFind n own <|> environFind n global)

-- One env per attached client, for the L loop.
loopClientEnvs :: ServerState -> IO [FormatEnv]
loopClientEnvs st = do
    cs <- Map.elems <$> readTVarIO st.clients
    mapM (fmap Map.fromList . clientFormatVars st) [ c | c <- cs, c.role == Attached ]

-- | An attached client's format environment: its own variables over its
-- session's. 'Nothing' when its session is gone.
clientFormatEnv :: ServerState -> Client -> IO (Maybe FormatEnv)
clientFormatEnv st c = do
    msess <- atomically $ do
        sid <- readTVar c.session
        Map.lookup sid <$> readTVar st.sessions
    case msess of
        Nothing -> pure Nothing
        Just sess -> do
            senv <- sessionFormatEnv st sess
            cvars <- clientFormatVars st c
            pure (Just (Map.union (Map.fromList cvars) senv))

-- | A client's own format variables.
clientFormatVars :: ServerState -> Client -> IO [(Text, Text)]
clientFormatVars st c = do
    controlFlags <- case c.control of
        Nothing -> pure []
        Just cs -> do
            fl <- readTVarIO cs.flags
            pure $ ["control-mode"]
                <> [ "no-output" | fl.output == NoOutput ]
                <> [ "wait-exit" | fl.exitWait == AwaitEmptyLine ]
                <> [ "pause-after=" <> tshow (age `div` 1000)
                   | Just age <- [fl.pauseAfter] ]
    (focused, sz, msess, mlast) <- atomically $ do
        ss <- readTVar st.sessions
        sid <- readTVar c.session
        hist <- readTVar c.sessionHist
        (,,,) <$> readTVar c.outerFocused <*> readTVar c.size
            <*> traverse (readTVar . (.name)) (Map.lookup sid ss)
            <*> traverse (readTVar . (.name))
                (listToMaybe (mapMaybe (`Map.lookup` ss) hist))
    ks <- readIORef c.keyState
    mterm <- readTVarIO c.ttyTerm
    opts <- clientOptions st c
    (mpid, muid, _) <- getPeerCredential c.sock
        `Control.Exception.catch` \(_ :: IOException) -> pure (Nothing, Nothing, Nothing)
    muser <- case muid of
        Nothing -> pure Nothing
        Just uid -> (Just . T.pack . userName <$> getUserEntryForID (fromIntegral uid))
            `Control.Exception.catch` \(_ :: IOException) -> pure Nothing
    let rootTable = optionText "key-table" opts
        table = maybe rootTable id ks.table
        -- hat's clients always read and draw UTF-8
        flags = ["attached"] <> [ "focused" | focused ] <> controlFlags <> ["UTF-8"]
    pure $
        [ ("client_name", clientName c)
        , ("client_colours", maybe "" (tshow . clientColourCount) mterm)
        , ("client_control_mode", flag (isJust c.control))
        , ("client_flags", T.intercalate "," flags)
        , ("client_key_table", table)
        , ("client_prefix", flag (table /= rootTable))
        , ("client_readonly", "0")
        , ("client_utf8", "1")
        , ("client_termname", c.term)
        , ("client_termtype", maybe "" (.termType) mterm)
        , ("client_termfeatures", maybe "" featureNames mterm)
        , ("client_width", tshow sz.cols)
        , ("client_created", tshow (floor c.createdAt :: Integer))
        ]
        -- a control client has no terminal of its own
        <> [ ("client_height", tshow sz.rows) | isNothing c.control ]
        <> [ ("client_session", n) | Just n <- [msess] ]
        <> [ ("client_last_session", n) | Just n <- [mlast] ]
        <> [ ("client_pid", tshow p) | Just p <- [mpid] ]
        <> [ ("client_uid", tshow u) | Just u <- [muid] ]
        <> [ ("client_user", u) | Just u <- [muser] ]

-- | The terminal client an environment's @client_name@ names, for @I@.
clientQueries :: ServerState -> FormatEnv -> IO (Maybe ClientQueries)
clientQueries st env = case Map.lookup "client_name" env of
    Nothing -> pure Nothing
    Just n -> do
        cs <- Map.elems <$> readTVarIO st.clients
        terms <- mapM (\c -> (c,) <$> readTVarIO c.ttyTerm) cs
        pure $ listToMaybe
            [ ClientQueries { environ = Map.fromList c.env, term }
            | (c, Just term) <- terms, clientName c == n, c.role == Attached ]

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

