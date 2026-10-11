{-# LANGUAGE MultiWayIf #-}

-- | The pane commands: capturing a pane's grid (@capture-pane@), splitting
-- a window (@split-window@), selecting\/killing\/swapping panes, clearing
-- scrollback, resizing, and zooming.
module Hat.Server.Command.Pane
    ( CaptureOpts (..)
    , CaptureRow (..)
    , captureBounds
    , captureText
    , cmdCapturePane
    , cmdSplitWindow
    , cmdNewPane
    , cmdSelectPane
    , cmdLastPane
    , cmdKillPane
    , cmdRespawnPane
    , cmdRespawnWindow
    , cmdSwapPane
    , cmdClearHistory
    , cmdResizePane
    , nextZoom
    , zoomTarget
    , noteActiveChange
    ) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM
import Control.Exception (bracket_)
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Numeric (showOct)

import Hat.Geometry
import Hat.Model
import Hat.Server.Command.Buffer (storeBuffer)
import Hat.Server.Command.Option (paneOptionText, setPaneOption)
import Hat.Server.Command.CaptureRaw (cellUsed, flagLetters, rawDump)
import Hat.Server.ArgNumber (stringPercentage)
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs, strtonum, unknownFlag)
import Hat.Server.Floating
    ( Border (..), FloatSplit (..), FloatState (..), GeomArgs (..), Modal (..), borderOf
    , floatingGeom, modalPane, splitFloatingGeom, stackNew, swapStacked )
import Hat.Server.Format (truthy)
import Hat.Server.Layout
import Hat.Model.Options (Options (..), optionText, quoteIfNeeded)
import Hat.Server.FormatEnv (paneFormatEnv)
import Hat.Server.Hooks
    ( NotifyTarget (..), PayloadItem (..), notify, notifyPane, notifyWindow )
import Hat.Server.Locate (findTarget, markedTarget, paneIndexOf)
import Hat.Server.Mouse (Drag (..))
import Hat.Server.MouseIO (clientMouseEvent, resizeBorderTo, startDrag)
import Hat.Server.Mru (recordVisit)
import Hat.Server.Pane
    ( LifecycleNotify (..), hangupPane, killPaneLocs, killPaneLocsWith, respawnPane
    , emptyPane, launchOf, paneShell, respawnStart, setScreenTitle, sessionSpawnEnv
    , spawnPane, startPaneReader, switchActive, withDefaultCommand )
import Hat.Server.Resize (applySessionSize)
import Hat.Server.FormatEnv (expandFormat, sessionFormatEnv)
import Hat.Server.Resize (windowArea, windowLayoutAt)
import Hat.Server.Target qualified as Target
import Hat.Term.Cell qualified as Cell
import Hat.Term.Emulator qualified as Emu
import Hat.Term.Emulator.Types (rtrimBlank)
import Hat.Term.RawGrid (RawCell (..), RawGrid (..), RawLine (..))

-- | Modifiers that shape a @capture-pane@ grid dump. See 'captureText'.
data CaptureOpts = CaptureOpts
    { captJoin   :: Bool  -- ^ @-J@: join soft-wrapped rows
    , captNoTrim :: Bool  -- ^ @-N@: keep trailing blanks
    , captUsed   :: Bool  -- ^ @-T@: stop at the last used cell
    , captSeq    :: Bool  -- ^ @-e@: emit SGR escape sequences before styled runs
    , captOctal  :: Bool  -- ^ @-C@: octal-escape controls and backslash
    , captNumber :: Bool  -- ^ @-L@: prefix each line with its screen-relative number
    , captFlags  :: Bool  -- ^ @-F@: prefix each line with its line flags
    }
    deriving stock (Eq, Show)

-- | One captured grid row: its screen-relative line number (history rows
-- negative), whether it soft-wraps onto the next row, its @-F@ flag letters
-- (read only when @-F@ asks), and its cells.
data CaptureRow = CaptureRow
    { number  :: Int
    , wrapped :: Bool
    , flags   :: Text
    , cells   :: V.Vector Cell.Cell
    }
    deriving stock (Eq, Show)

-- | Resolve tmux's @-S@\/@-E@ selectors to an inclusive @(top, bottom)@ over
-- the combined history+screen line space: history lines are @0..hsize-1@ and
-- screen lines @hsize..hsize+sy-1@. @-@ means the very start (@-S@) or the very
-- bottom (@-E@); a bare number counts from the top of the screen, negative into
-- history; an absent or unparseable selector defaults to the visible screen.
captureBounds :: Int -> Int -> Maybe Text -> Maybe Text -> (Int, Int)
captureBounds hsize sy sflag eflag =
    if bottom < top then (bottom, top) else (top, bottom)
  where
    lastLine = hsize + sy - 1
    top    = resolve 0        hsize    sflag
    bottom = resolve lastLine lastLine eflag
    resolve dashVal def = \case
        Just "-" -> dashVal
        Just s -> case parseSigned s of
            Just n
                | n < 0 && negate n > hsize -> 0
                | otherwise                 -> min lastLine (hsize + n)
            Nothing -> def
        Nothing -> def
    parseSigned s = case TR.signed TR.decimal s of
        Right (n, rest) | T.null rest -> Just (n :: Int)
        _ -> Nothing

-- | Render captured rows exactly as tmux's @capture-pane@ does
-- (@grid_string_cells@ over @cmd_capture_pane_history@): every row ends in a
-- newline unless @-J@ joins a wrapped row into its successor; @-e@ diffs each
-- cell's style against a pen carried across the whole capture
-- ('styleDiffSgr'); trailing spaces are trimmed from the rendered bytes
-- unless @-N@ or @-J@; @-T@ and @-J@ stop at the last used cell. A wide
-- character occupies one cell carrying its glyph; its continuation cell is
-- skipped.
captureText :: CaptureOpts -> [CaptureRow] -> Text
captureText opts = T.concat . go Cell.defaultStyle
  where
    emptyCells = not opts.captJoin && not opts.captUsed
    trimSpaces = not opts.captJoin && not opts.captNoTrim
    go _ [] = []
    go pen (r : rs) =
        let (body, pen') = renderCells pen (visible r)
            trimmed = if trimSpaces then T.dropWhileEnd (== ' ') body else body
            nl = if opts.captJoin && r.wrapped then "" else "\n"
        in (numberPrefix r.number <> flagPrefix r.flags <> trimmed <> nl)
            : go pen' rs
    numberPrefix n
        | opts.captNumber = tshow n <> " "
        | otherwise       = ""
    flagPrefix f
        | opts.captFlags = f <> " "
        | otherwise      = ""
    -- tmux's cellused has no libghostty analogue: the used extent is
    -- approximated as everything up to the last non-default cell, and a
    -- wrapped row under @-J@ is fully used by construction.
    visible r
        | emptyCells || (opts.captJoin && r.wrapped) = V.toList r.cells
        | otherwise = rtrimBlank (V.toList r.cells)
    renderCells pen [] = ("", pen)
    renderCells pen (cell : cls)
        | Cell.Continuation <- cell.content = renderCells pen cls
        | otherwise =
            let lead | opts.captSeq = styleDiffSgr opts.captOctal pen cell.style
                     | otherwise    = ""
                (rest, pen') = renderCells cell.style cls
            in (lead <> cellText cell <> rest, pen')
    cellText cell
        | opts.captOctal = foldMap escapeOctal (Cell.cluster cell)
        | otherwise      = T.pack (Cell.cluster cell)

-- | The SGR bytes that move the pen from @old@ to @new@, exactly as tmux's
-- @grid_string_cells_code@: a leading @0@ when any attribute drops, newly set
-- attributes in one CSI, then a separate CSI per changed color channel; after
-- a reset a default color needs no code. With @esc@ the introducer is the
-- literal @\\033[@ (tmux's @-C@).
styleDiffSgr :: Bool -> Cell.Style -> Cell.Style -> Text
styleDiffSgr esc old new =
    attrs <> code (fgParams old.fg) (fgParams new.fg)
          <> code (bgParams old.bg) (bgParams new.bg)
  where
    oldA = attrCodes old
    newA = attrCodes new
    reset = any (`notElem` newA) oldA
    slist = [0 | reset] <> filter (`notElem` (if reset then [] else oldA)) newA
    attrs | null slist = ""
          | otherwise  = csi slist
    code oldP newP
        | not reset && newP == oldP = ""
        | reset && (newP == [39] || newP == [49]) = ""
        | otherwise = csi newP
    csi ps = intro <> T.intercalate ";" (map tshow ps) <> "m"
    intro = if esc then "\\033[" else "\ESC["

-- | The SGR codes for a style's set attributes, in tmux's emission order.
attrCodes :: Cell.Style -> [Int]
attrCodes st = concat
    [ [1 | st.bold], [2 | st.faint], [3 | st.italic], [4 | st.underline]
    , [5 | st.blink], [7 | st.reverse], [9 | st.strike] ]

-- | tmux's @grid_string_cells_fg@\/@_bg@: one color channel's SGR parameters.
fgParams, bgParams :: Cell.Color -> [Int]
fgParams = colorParams 30 90 38 39
bgParams = colorParams 40 100 48 49

colorParams :: Int -> Int -> Int -> Int -> Cell.Color -> [Int]
colorParams base bright ext def = \case
    Cell.DefaultColor -> [def]
    Cell.Indexed n
        | n < 8     -> [base + fromIntegral n]
        | n < 16    -> [bright + fromIntegral n - 8]
        | otherwise -> [ext, 5, fromIntegral n]
    Cell.RGB r g b ->
        [ext, 2, fromIntegral r, fromIntegral g, fromIntegral b]

-- | tmux's @-C@ escaping: backslash doubles, control characters become
-- octal @\\ooo@.
escapeOctal :: Char -> Text
escapeOctal ch
    | ch == '\\' = "\\\\"
    | ch < ' ' || ch == '\DEL' = "\\" <> T.pack (pad (showOct (fromEnum ch) ""))
    | otherwise = T.singleton ch
  where
    pad s = replicate (3 - length s) '0' <> s

cmdCapturePane :: CommandImpl
cmdCapturePane st mclient args = do
    let (opts, flags, _) = parseArgs "bESt" args
        has f = f `elem` flags
        copts = CaptureOpts
            { captJoin   = has "-J"
            , captNoTrim = has "-N"
            , captUsed   = has "-T"
            , captSeq    = has "-e"
            , captOctal  = has "-C"
            , captNumber = has "-L"
            , captFlags  = has "-F"
            }
    case filter has ["-P"] of
        (bad : _) ->
            pure [RErr ("capture-pane: " <> bad <> " is not implemented")]
        [] -> do
            res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
            case res of
                Left e -> pure [RErr e]
                Right (_, _, _, pane) -> do
                    ecap <- if
                        | has "-R" -> Right . rawDump <$> Emu.rawGrid pane.emulator
                        | has "-H" -> captureLinks pane copts (has "-a")
                            (has "-M") (lookup "-S" opts) (lookup "-E" opts)
                        | otherwise -> capturePane pane copts (has "-a") (has "-q")
                            (has "-M") (lookup "-S" opts) (lookup "-E" opts)
                    case ecap of
                        Left err -> pure [RErr err]
                        Right txt
                            | has "-p" -> pure
                                [ROutput (fromMaybe txt (T.stripSuffix "\n" txt))]
                            | otherwise ->
                                either (\e -> [RErr e]) (const [])
                                    <$> storeBuffer st (lookup "-b" opts)
                                            (TE.encodeUtf8 txt)

-- | Capture a pane's rows per tmux's source selection: @-a@ asks for the
-- screen saved behind an active alternate screen (which libghostty keeps
-- unreadable, so it only yields tmux's @no alternate screen@ error, silenced
-- by @-q@); @-M@ reads the copy-mode frozen grid when the pane is in copy
-- mode; otherwise the live grid plus scrollback, with @-S@\/@-E@ resolved by
-- 'captureBounds' and wrap flags read only when @-J@ needs them.
capturePane
    :: Pane -> CaptureOpts -> Bool -> Bool -> Bool -> Maybe Text -> Maybe Text
    -> IO (Either Text Text)
capturePane pane copts altFlag quiet copyFlag sflag eflag
    | altFlag = do
        m <- Emu.modes pane.emulator
        pure $ if m.altScreen
            then Left "capture-pane: -a is not implemented"
            else if quiet then Right "" else Left "no alternate screen"
    | otherwise = do
        mmode <- if copyFlag then readTVarIO pane.mode else pure Nothing
        case mmode of
            Just pm
                | copts.captJoin ->
                    pure (Left "capture-pane: -J with -M is not implemented")
                | copts.captFlags ->
                    pure (Left "capture-pane: -F with -M is not implemented")
                | otherwise -> do
                    let fg = pm.frozen
                        (top, bottom) = captureBounds fg.fgHsize fg.fgSy sflag eflag
                    pure . Right . captureText copts $
                        [ CaptureRow (i - fg.fgHsize) False ""
                            (fromMaybe V.empty (fg.fgRows V.!? i))
                        | i <- [top .. bottom] ]
            Nothing -> do
                scr <- Emu.snapshot pane.emulator
                hsize <- Emu.scrollbackLength pane.emulator
                rawLines <- if copts.captFlags
                    then V.fromList . (.lines) <$> Emu.rawGrid pane.emulator
                    else pure V.empty
                let sy = V.length scr.cells
                    (top, bottom) = captureBounds hsize sy sflag eflag
                    flagsAt i = maybe "-" flagLetters (rawLines V.!? i)
                Right . captureText copts
                    <$> mapM (liveRow hsize scr flagsAt) [top .. bottom]
  where
    liveRow hsize scr flagsAt i
        | i < hsize = CaptureRow (i - hsize)
            <$> wrapAt (Emu.scrollbackLineWrapped pane.emulator i)
            <*> pure (flagsAt i)
            <*> (fromMaybe V.empty <$> Emu.scrollbackLine pane.emulator i)
        | otherwise = CaptureRow (i - hsize)
            <$> wrapAt (Emu.screenRowWrapped pane.emulator (i - hsize))
            <*> pure (flagsAt i)
            <*> pure (fromMaybe V.empty (scr.cells V.!? (i - hsize)))
    wrapAt act = if copts.captJoin then act else pure False

-- | @capture-pane -H@: per captured line, the distinct hyperlink URIs its
-- cells carry, space-separated; a URI already listed on an earlier line is
-- not repeated, and a line left with none is dropped. tmux tells links apart
-- by OSC 8 identity, which libghostty-vt reduces to the URI, so two separate
-- links to one URI list once.
captureLinks
    :: Pane -> CaptureOpts -> Bool -> Bool -> Maybe Text -> Maybe Text
    -> IO (Either Text Text)
captureLinks pane copts altFlag copyFlag sflag eflag
    | altFlag = pure (Left "capture-pane: -H with -a is not implemented")
    | otherwise = do
        mmode <- if copyFlag then readTVarIO pane.mode else pure Nothing
        case mmode of
            Just _ -> pure (Left "capture-pane: -H with -M is not implemented")
            Nothing -> Right . linksText copts <$> Emu.rawGrid pane.emulator
  where
    linksText opts g = T.concat (go [] picked)
      where
        (top, bottom) = captureBounds g.hsize g.rows sflag eflag
        picked = take (bottom - top + 1) (drop top (zip [0 ..] g.lines))
        go _ [] = []
        go seen ((i, l) : rest) =
            let new = List.nub
                    [ u | Just u <- map (.link) (take (cellUsed l) l.cells)
                    , u `notElem` seen ]
            in if null new
                then go seen rest
                else (numberPrefix (i - g.hsize) <> flagPrefix l
                        <> T.intercalate " " new <> "\n")
                    : go (seen <> new) rest
        numberPrefix n
            | opts.captNumber = tshow n <> " "
            | otherwise       = ""
        flagPrefix l
            | opts.captFlags = flagLetters l <> " "
            | otherwise      = ""

-- | @split-window@ (@cmd-split-window.c@): give part of the target pane —
-- or with @-f@ of the whole window — to a new pane. A floating target's
-- box is split into two floating panes instead.
cmdSplitWindow :: CommandImpl
cmdSplitWindow = spawnInWindow SplitEntry

-- | @new-pane@ (@cmd-split-window.c@): a new floating pane, at the @-x@\/
-- @-y@ size and @-X@\/@-Y@ position or cascading from the last; with @-L@
-- a tiled one, as @split-window@ makes. @-O@ makes it the window's modal
-- pane.
cmdNewPane :: CommandImpl
cmdNewPane = spawnInWindow NewPaneEntry

-- | Which command is spawning the pane; they share @cmd_split_window_exec@.
data SpawnEntry = SplitEntry | NewPaneEntry
    deriving (Eq)

spawnInWindow :: SpawnEntry -> CommandImpl
spawnInWindow entry st mclient args
    | Just e <- unknownFlag entryName boolFlags flags = pure [RErr e]
    -- tmux closes a modal pane made with -C when a click lands outside it;
    -- hat has no mouse input to click with.
    | "-C" `elem` flags = pure [RErr "new-pane -C is not implemented"]
    | Left e <- launch = pure [RErr e]
    | otherwise = do
        res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
        case res of
            Left e -> pure [RErr e]
            Right (sess, wix, win, target) -> do
                eff <- atomically (windowArea st win)
                lay <- atomically (windowLayoutAt eff win)
                fs <- readTVarIO win.floats
                let floating = case entry of
                        NewPaneEntry -> "-L" `notElem` flags
                        SplitEntry -> isFloating target.id lay
                if | modalFlag && not floating ->
                        pure [RErr "modal pane must be floating"]
                   | modalFlag && isJust fs.modal ->
                        pure [RErr "window already has a modal pane"]
                   -- -M sizes the pane by the mouse drag that ran it; with
                   -- no drag there is nothing to make.
                   | "-M" `elem` flags && floating -> pure []
                   | floating -> do
                        border <- borderFor sess win
                        mgeom <- floatingCellGeom sess eff border lay fs target
                        case mgeom of
                            Left e -> pure [RErr e]
                            Right (targetGeom, g, cascade) ->
                                spawnFloating sess wix win target eff targetGeom g cascade
                   | otherwise -> do
                        msize <- splitSizeArg st sess orient spanning eff target.id lay opts
                        case msize of
                            _ | isFloating target.id lay ->
                                pure [RErr "can't split a floating pane"]
                            Left e -> pure [RErr ("invalid tiled geometry " <> e)]
                            Right size -> do
                                let spec = SplitSpec
                                        { orient = orient, size = size
                                        , placement = placement, span = spanning }
                                splitInto sess wix win target eff
                                    (\pid lay' fs' -> (\l -> (l, fs')) <$> splitPane spec target.id pid lay')
  where
    (entryName, boolFlags, valueFlags) = case entry of
        SplitEntry -> ("split-window", "bdEfhIkPvWZ", "BceFlmpRsStT")
        NewPaneEntry -> ("new-pane", "bCdEfhIkLMOPvWZ", "BceFlmpRsStTxXyY")
    (opts, flags, pos) = parseArgs valueFlags args
    modalFlag = "-O" `elem` flags
    -- A floating pane's border, which its geometry counts: -B's, else the
    -- window's pane-border-lines.
    borderFor sess win = case lookup "-B" opts of
        Just "none" -> pure Borderless
        Just _ -> pure Bordered
        Nothing -> do
            wopts <- atomically (resolveForWindow st sess win)
            pure (borderOf wopts.paneBorderLines)
    -- The new floating pane's box, and the target's own when a split of a
    -- floating pane reshapes it too (@layout_get_floating_cell@).
    floatingCellGeom sess eff border lay fs target = case entry of
        SplitEntry -> pure $ case floatGeom target.id lay of
            Nothing -> Left "can't split a floating pane"
            Just tg -> do
                (tg', g) <- splitFloatingGeom eff border FloatSplit
                    { across = "-h" `elem` flags
                    , before = "-b" `elem` flags
                    , fullSpan = "-f" `elem` flags } tg
                pure (Just tg', g, fs.lastNew)
        NewPaneEntry -> do
            env <- sessionFormatEnv st sess
            let expand v = case T.stripSuffix "%" v of
                    Just body -> (<> "%") <$> expandFormat st env body
                    Nothing -> expandFormat st env v
            geomArgs <- GeomArgs
                <$> traverse expand (lookup "-x" opts)
                <*> traverse expand (lookup "-y" opts)
                <*> traverse expand (lookup "-X" opts)
                <*> traverse expand (lookup "-Y" opts)
            pure $ (\(g, cascade) -> (Nothing, g, cascade))
                <$> floatingGeom eff border Nothing fs.lastNew geomArgs
    spawnFloating sess wix win target eff targetGeom g cascade =
        splitInto sess wix win target eff $ \pid lay fs -> do
            let resized = maybe lay (\tg -> setFloatGeom target.id tg lay) targetGeom
            let spot = if "-b" `elem` flags then FloatBefore target.id else FloatLast
            lay' <- floatPane spot pid g resized
            pure (lay', (stackNew pid fs lay') { lastNew = cascade })
    -- The pane options the flags set (tmux's order); the first the
    -- registry refuses is the command's error.
    setPaneOptions pane = firstError
        [ setPaneOption st pane name value | (name, value) <- paneOptionValues ]
    paneOptionValues =
        concat [ [("window-style", v), ("window-active-style", v)] | Just v <- [lookup "-s" opts] ]
        <> [ ("pane-active-border-style", v) | Just v <- [lookup "-S" opts] ]
        <> [ ("pane-border-style", v) | Just v <- [lookup "-R" opts] ]
        <> [ ("pane-border-lines", v) | Just v <- [lookup "-B" opts] ]
        <> [ ("remain-on-exit", "key") | "-k" `elem` flags || isJust (lookup "-m" opts) ]
        <> [ ("remain-on-exit-format", v) | Just v <- [lookup "-m" opts] ]
    firstError = \case
        [] -> pure Nothing
        act : rest -> act >>= either (pure . Just) (const (firstError rest))
    -- The command client's standard input, written to the pane as if its
    -- program had printed it (tmux's @window_pane_start_input@); an
    -- attached client has none to give.
    feedStdin pane = case mclient of
        Just Client { stdinSource = StdinFrom fetch } -> fetch >>= \case
            Right bytes -> do
                _ <- Emu.feed pane.emulator bytes
                atomically (bumpDirty st)
            Left _ -> pure ()
        _ -> pure ()
    orient = if "-h" `elem` flags then LeftRight else TopBottom
    placement = if "-b" `elem` flags then Before else After
    spanning = if "-f" `elem` flags then SpanWindow else SpanPane
    -- -I fills the new pane from the command client's standard input, so
    -- it is always empty.
    launch = launchOf ("-E" `elem` flags || "-I" `elem` flags) pos
    (how0, kind) = either (const (FreshShell, ProgramPane)) id launch
    -- Place the new pane's cell with @place@, spawn it at that size, and
    -- make it active as tmux's @spawn_pane@ does: a modal pane always, any
    -- other unless -d or while a modal pane holds the window.
    splitInto sess wix win target eff place = do
        pid <- PaneId <$> atomically (freshId st.nextPane)
        planned <- atomically $ do
            lay <- windowLayoutAt eff win
            fs <- readTVar win.floats
            pure (place pid lay fs)
        case planned >>= paneCell pid . fst of
            Nothing -> pure [RErr "no space for a new pane"]
            Just newCell -> do
                dir <- case lookup "-c" opts of
                    Just d -> do
                        env <- sessionFormatEnv st sess
                        T.unpack <$> expandFormat st env d
                    Nothing -> paneCurrentPath target
                environ0 <- sessionSpawnEnv st sess
                sopts <- atomically (resolveForSession st sess)
                shellCmd <- paneShell sopts
                let environ = environ0 <> envPairs opts
                    how = withDefaultCommand (optionText "default-command" sopts) how0
                    cellSize = Size { rows = fromIntegral newCell.sy
                                    , cols = fromIntegral newCell.sx }
                pane <- case kind of
                    ProgramPane -> spawnPane st pid sess.id shellCmd how dir environ cellSize
                    EmptyPane -> emptyPane st pid how dir cellSize
                placed <- atomically $ do
                    lay <- windowLayoutAt eff win
                    fs <- readTVar win.floats
                    case place pid lay fs of
                        Nothing -> pure False
                        Just (lay', fs') -> do
                            modifyTVar' win.panes (Map.insert pane.id pane)
                            writeTVar win.layout lay'
                            active <- readTVar win.activeId
                            wasZoomed <- isJust <$> readTVar win.zoomed
                            let makeActive = do
                                    modifyTVar' win.paneHist (recordVisit active pane.id)
                                    writeTVar win.activeId pane.id
                            if modalFlag
                                then do
                                    writeTVar win.floats fs'
                                        { modal = Just Modal
                                            { pane = pane.id, returnTo = Just active
                                            , rezoom = wasZoomed } }
                                    makeActive
                                else do
                                    writeTVar win.floats fs'
                                    when ("-d" `notElem` flags && isNothing fs.modal)
                                        makeActive
                            -- tmux's window_push_zoom/window_pop_zoom: the
                            -- new pane unzooms the window, and for a tiled
                            -- one -Z zooms the active pane again.
                            active' <- readTVar win.activeId
                            writeTVar win.zoomed
                                (if "-Z" `elem` flags && not (isFloating pane.id lay')
                                    then Just active' else Nothing)
                            True <$ bumpDirty st
                optErr <- if placed then setPaneOptions pane else pure Nothing
                if | not placed -> [RErr "no space for a new pane"] <$ hangupPane pane
                   | Just e <- optErr ->
                        [RErr e] <$ killPaneLocs st [(sess.id, win, pane)]
                   | otherwise -> do
                        forM_ (lookup "-T" opts) (setPaneTitle st sess win pane)
                        startPaneReader st sess.id win pane
                        when ("-I" `elem` flags) $ feedStdin pane
                        applySessionSize st sess.id
                        notifyWindow st "window-layout-changed" (Just sess.id) win []
                        let spawned = case how of
                                ShellCommand c -> quoteIfNeeded c
                                ExecArgv argv -> T.unwords (map quoteIfNeeded argv)
                                _ -> T.pack shellCmd
                        notifyPane st "pane-created" pane
                            [ ("pane_command", PText spawned)
                            , ("created_empty", PInt (if kind == EmptyPane then 1 else 0))
                            , ("created_respawn", PInt 0) ]
                        printed <- if "-P" `elem` flags
                            then do
                                let fmt = fromMaybe
                                        "#{session_name}:#{window_index}.#{pane_index}"
                                        (lookup "-F" opts)
                                pix <- paneIndexOf st win pane
                                env <- paneFormatEnv st sess wix win pix pane
                                out <- expandFormat st env fmt
                                pure [ROutput out]
                            else pure []
                        when ("-W" `elem` flags) (awaitPaneEnd st pane)
                        pure printed

-- | Block until a pane's program ends (it dies, or the pane goes), as
-- @split-window -W@ waits; outside the command batch, so reconciliation
-- runs meanwhile.
awaitPaneEnd :: ServerState -> Pane -> IO ()
awaitPaneEnd st pane = bracket_
    (atomically (modifyTVar' st.commandDepth (subtract 1)))
    (atomically (modifyTVar' st.commandDepth (+ 1)))
    (atomically $ do
        gone <- readTVar pane.dead
        exited <- readTVar pane.exited
        check (gone || isJust exited))

-- | The @-e VAR=value@ pairs of a spawning command, in order.
envPairs :: [(Text, Text)] -> [(Text, Text)]
envPairs opts = reverse
    [ (n, T.drop 1 v) | ("-e", nv) <- opts, let (n, v) = T.breakOn "=" nv ]

-- | A split's requested size in cells from @-l@ (cells or a percentage) or
-- @-p@ (a percentage), both of the pane's — or with @-f@ the window's —
-- extent along the split, after format expansion
-- (@layout_get_tiled_cell@).
splitSizeArg
    :: ServerState -> Session -> Orientation -> SplitSpan -> Size -> PaneId
    -> Layout -> [(Text, Text)] -> IO (Either Text (Maybe Int))
splitSizeArg st sess orient spanning eff target lay opts = do
    env <- sessionFormatEnv st sess
    let expand = expandFormat st env
        whole = case spanning of
            SpanWindow -> eff
            SpanPane -> maybe eff layoutArea (paneCell target lay)
        cur = case orient of
            LeftRight -> fromIntegral whole.cols
            TopBottom -> fromIntegral whole.rows
    case (lookup "-l" opts, lookup "-p" opts) of
        (Just l, _) -> do
            v <- case T.stripSuffix "%" l of
                Just body -> (<> "%") <$> expand body
                Nothing -> expand l
            pure (Just <$> stringPercentage 0 maxBound cur v)
        (Nothing, Just p) -> do
            v <- expand p
            pure (Just . (\n -> cur * n `div` 100) <$> strtonum 0 100 v)
        _ -> pure (Right Nothing)

-- Fire window-pane-changed when a select actually moved the active pane.
noteActiveChange :: ServerState -> Window -> IO a -> IO a
noteActiveChange st win act = do
    old <- readTVarIO win.activeId
    r <- act
    new <- readTVarIO win.activeId
    when (new /= old) $ do
        mloc <- atomically (locateWindowOf st win)
        wname <- readTVarIO win.name
        notify st "window-pane-changed"
            (NotifyTarget (fst <$> mloc) (Just win.id) (Just new))
            [ ("window", PWindowRef win.id wname)
            , ("pane", PPaneRef new)
            , ("old_pane", PPaneRef old)
            , ("new_pane", PPaneRef new) ]
    pure r

-- The session holding a window (and its index there).
locateWindowOf :: ServerState -> Window -> STM (Maybe (SessionId, Int))
locateWindowOf st win = do
    sessions <- readTVar st.sessions
    hits <- forM (Map.toList sessions) $ \(sid, sess) -> do
        ws <- readTVar sess.windows
        pure [ (sid, ix) | (ix, w) <- Map.toList ws, w.id == win.id ]
    pure (listToMaybe (concat hits))

-- | @select-pane@ (@cmd-select-pane.c@): make the target pane — or its
-- neighbour in a @-L@\/@-R@\/@-U@\/@-D@ direction — active; @-l@ acts on
-- the last pane instead; @-m@\/@-M@ set and clear the marked pane; @-d@\/@-e@
-- turn the pane's input off and on; @-T@ sets its title.
cmdSelectPane :: CommandImpl
cmdSelectPane st mclient args
    | Just e <- unknownFlag "select-pane" "DdegLlMmRUZ" flags = pure [RErr e]
    | otherwise = do
        res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
        case res of
            Left e -> pure [RErr e]
            Right (sess, _, win, pane)
                | "-l" `elem` flags -> lastPaneOf st sess win flags
                | "-m" `elem` flags || "-M" `elem` flags -> do
                    -- A pane a zoom hides cannot be marked; a floating one
                    -- marked comes to the front as the active pane.
                    (zoom, lay) <- atomically $ (,) <$> readTVar win.zoomed <*> readTVar win.layout
                    let hidden = maybe False (/= pane.id) zoom
                    if "-m" `elem` flags && hidden then pure [] else do
                        markPane st pane ("-M" `elem` flags)
                        when (isFloating pane.id lay) $ activate st sess win pane False
                        pure []
                -- The deprecated -P and -g set and show the pane's
                -- window-style.
                | Just style <- lookup "-P" opts -> do
                    r <- setPaneOption st pane "window-style" style
                    case r of
                        Left e -> pure [RErr e]
                        Right () -> either (pure . RErr) (const [])
                            <$> setPaneOption st pane "window-active-style" style
                | "-g" `elem` flags ->
                    either (pure . RErr) (pure . ROutput)
                        <$> paneOptionText st sess win pane "window-style"
                | otherwise -> do
                    mtarget <- case direction of
                        Nothing -> pure (Just pane)
                        Just dir -> do
                            eff <- atomically (windowArea st win)
                            (lay, ps) <- atomically $ (,) <$> windowLayoutAt eff win <*> readTVar win.panes
                            pure (directionalTarget eff lay pane.id dir >>= (`Map.lookup` ps))
                    case mtarget of
                        Nothing -> pure []
                        Just wp
                            | "-e" `elem` flags -> [] <$ setInputOff st wp False
                            | "-d" `elem` flags -> [] <$ setInputOff st wp True
                            | Just t <- lookup "-T" opts -> [] <$ setPaneTitle st sess win wp t
                            | otherwise -> [] <$ activate st sess win wp ("-Z" `elem` flags)
  where
    (opts, flags, _) = parseArgs "PtT" args
    direction
        | "-L" `elem` flags = Just DirLeft
        | "-R" `elem` flags = Just DirRight
        | "-U" `elem` flags = Just DirUp
        | "-D" `elem` flags = Just DirDown
        | otherwise = Nothing

-- | @last-pane@: @select-pane -l@ on the target window.
cmdLastPane :: CommandImpl
cmdLastPane st mclient args
    | Just e <- unknownFlag "last-pane" "deZ" flags = pure [RErr e]
    | otherwise = do
        res <- findTarget st mclient Target.FindWindow (lookup "-t" opts)
        case res of
            Left e -> pure [RErr e]
            Right (sess, _, win, _) -> lastPaneOf st sess win flags
  where
    (opts, flags, _) = parseArgs "t" args

-- | The last pane of a window (the head of its history, or with exactly two
-- panes the other one): switched to, or with @-e@\/@-d@ its input turned on
-- or off.
lastPaneOf :: ServerState -> Session -> Window -> [Text] -> IO [Reply]
lastPaneOf st sess win flags = do
    mlast <- atomically $ do
        hist <- readTVar win.paneHist
        ps <- readTVar win.panes
        active <- readTVar win.activeId
        order <- map (.id) <$> windowPanes win
        let other = case break (== active) order of
                (before, _ : after) | length order == 2 ->
                    listToMaybe (reverse before) <|> listToMaybe after
                _ -> Nothing
            pick = case [ p | p <- hist, Map.member p ps ] of
                p : _ -> Just p
                [] -> other
        pure (pick >>= (`Map.lookup` ps))
    case mlast of
        Nothing -> pure [RErr "no last pane"]
        Just wp
            | "-e" `elem` flags -> [] <$ setInputOff st wp False
            | "-d" `elem` flags -> [] <$ setInputOff st wp True
            | otherwise -> [] <$ activate st sess win wp ("-Z" `elem` flags)

-- | Make a pane its window's active one. The window unzooms, unless
-- @keepZoom@ zooms the new active pane instead (tmux's @window_push_zoom@\/
-- @window_pop_zoom@ around @window_set_active_pane@).
activate :: ServerState -> Session -> Window -> Pane -> Bool -> IO ()
activate st sess win wp keepZoom = noteActiveChange st win $ do
    changed <- atomically $ do
        zoomed <- readTVar win.zoomed
        switched <- switchActive win wp.id
        when switched $ do
            writeTVar win.zoomed (if keepZoom && isJust zoomed then Just wp.id else Nothing)
            bumpDirty st
        pure switched
    when changed (applySessionSize st sess.id)

setInputOff :: ServerState -> Pane -> Bool -> IO ()
setInputOff st wp off = atomically $ writeTVar wp.inputOff off >> bumpDirty st

-- | Set a pane's title as its program would (OSC 2), firing
-- @pane-title-changed@ when that changes it.
setPaneTitle :: ServerState -> Session -> Window -> Pane -> Text -> IO ()
setPaneTitle st sess win wp raw = do
    pix <- paneIndexOf st win wp
    wix <- atomically $ do
        ws <- readTVar sess.windows
        pure (listToMaybe [ i | (i, w) <- Map.toList ws, w.id == win.id ])
    env <- paneFormatEnv st sess (fromMaybe 0 wix) win pix wp
    title <- T.filter (\c -> c >= ' ' && c /= '\DEL') <$> expandFormat st env raw
    old <- Emu.title wp.emulator
    when (title /= old) $ do
        setScreenTitle wp title
        atomically (bumpDirty st)
        notifyPane st "pane-title-changed" wp [ ("new_title", PText title) ]

-- | Set or clear the marked pane: @-M@ (or marking the marked pane again)
-- clears it. Fires @marked-pane-changed@.
markPane :: ServerState -> Pane -> Bool -> IO ()
markPane st pane clearing = do
    old <- atomically $ do
        old <- readTVar st.markedPane
        let unmark = clearing || old == Just pane.id
        writeTVar st.markedPane (if unmark then Nothing else Just pane.id)
        bumpDirty st
        pure old
    new <- readTVarIO st.markedPane
    notifyPane st "marked-pane-changed" pane $
        [ ("marked", PInt (if isJust new then 1 else 0)) ]
        <> [ ("new_pane", PPaneRef p) | Just p <- [new] ]
        <> [ ("old_pane", PPaneRef p) | Just p <- [old] ]

-- | Kill the target pane: detach it from the model first so the reflow is
-- synchronous with the command, then reap the child behind ('killPaneLocs').
cmdKillPane :: CommandImpl
cmdKillPane st mclient args
    | Just e <- unknownFlag "kill-pane" "a" flags = pure [RErr e]
    | Just _ <- lookup "-f" opts, "-a" `notElem` flags =
        pure [RErr "-f only valid with -a"]
    | otherwise = do
        res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
        case res of
            Left e -> pure [RErr e]
            Right (sess, wix, win, pane)
                | "-a" `elem` flags -> do
                    atomically (writeTVar win.zoomed Nothing)
                    others <- filter ((/= pane.id) . (.id))
                        <$> atomically (windowPanes win)
                    doomed <- filterM (passes sess wix win) others
                    [] <$ killPaneLocs st [ (sess.id, win, p) | p <- doomed ]
                | otherwise -> [] <$ killPaneLocs st [(sess.id, win, pane)]
  where
    (opts, flags, _) = parseArgs "ft" args
    passes sess wix win p = case lookup "-f" opts of
        Nothing -> pure True
        Just f -> do
            pix <- paneIndexOf st win p
            env <- paneFormatEnv st sess wix win pix p
            truthy <$> expandFormat st env f

-- | @respawn-pane [-Ek] [-c dir] [-e VAR=value] [-t pane] [command ...]@:
-- run a new process in a dead or empty pane, or with @-k@ in a live one;
-- @-E@ instead leaves it empty, storing the command and directory for the
-- next respawn.
cmdRespawnPane :: CommandImpl
cmdRespawnPane st mclient args = do
    let (opts, flags, pos) = parseArgs "cet" args
    res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
    case (unknownFlag "respawn-pane" "Ek" flags, res) of
        (Just e, _) -> pure [RErr e]
        (_, Left e) -> pure [RErr e]
        (_, Right (sess, wix, win, pane)) -> do
            live <- runsProgram pane
            if live && "-k" `notElem` flags
                then do
                    sname <- readTVarIO sess.name
                    pix <- paneIndexOf st win pane
                    pure [RErr ("respawn pane failed: pane " <> sname <> ":"
                        <> tshow wix <> "." <> tshow pix <> " still active")]
                else respawnReply "respawn pane failed: "
                    <$> respawnIn st mclient sess win pane opts flags pos

-- | @respawn-window [-Ek] [-c dir] [-e VAR=value] [-t window] [command ...]@:
-- reduce the window to its first pane and respawn that.
cmdRespawnWindow :: CommandImpl
cmdRespawnWindow st mclient args = do
    let (opts, flags, pos) = parseArgs "cet" args
    res <- findTarget st mclient Target.FindWindow (lookup "-t" opts)
    case (unknownFlag "respawn-window" "Ek" flags, res) of
        (Just e, _) -> pure [RErr e]
        (_, Left e) -> pure [RErr e]
        (_, Right (sess, wix, win, _)) -> do
            panes <- atomically (windowPanes win)
            live <- or <$> mapM runsProgram panes
            case panes of
                _ | live && "-k" `notElem` flags -> do
                    sname <- readTVarIO sess.name
                    pure [RErr ("respawn window failed: window " <> sname
                        <> ":" <> tshow wix <> " still active")]
                [] -> pure [RErr "respawn window failed: no panes"]
                first : others -> do
                    killPaneLocsWith QuietLifecycle st
                        [ (sess.id, win, p) | p <- others ]
                    atomically $ do
                        writeTVar win.activeId first.id
                        writeTVar win.paneHist []
                    respawnReply "respawn window failed: "
                        <$> respawnIn st mclient sess win first opts flags pos

-- | Whether a pane's program is running (tmux: its fd is open): not a dead
-- pane, nor an empty one.
runsProgram :: Pane -> IO Bool
runsProgram pane = do
    exited <- readTVarIO pane.exited
    pure (pane.kind == ProgramPane && isNothing exited)

respawnReply :: Text -> Either Text () -> [Reply]
respawnReply prefix = either (\e -> [RErr (prefix <> e)]) (const [])

-- | Replace @pane@'s process: the given command (one word for the shell,
-- several exec'd directly), else the pane's own launch; in @-c@ (a format;
-- a relative one resolves as tmux's @server_client_get_cwd@: against a
-- command client's directory, else the session's), else the pane's start
-- directory.
respawnIn
    :: ServerState -> Maybe Client -> Session -> Window -> Pane
    -> [(Text, Text)] -> [Text] -> [Text] -> IO (Either Text ())
respawnIn st mclient sess win pane opts flags pos = do
    dir <- case lookup "-c" opts of
        Nothing -> pure pane.startCwd
        Just d -> do
            env <- sessionFormatEnv st sess
            expanded <- T.unpack <$> expandFormat st env d
            base <- case mclient of
                Just c | c.role == Control -> pure (T.unpack c.cwd)
                _ -> readTVarIO sess.startCwd
            pure $ case expanded of
                '/' : _ -> expanded
                "" -> base
                rel -> base <> "/" <> rel
    environ0 <- sessionSpawnEnv st sess
    let environ = environ0 <> envPairs opts
        how = case pos of
            [] -> respawnStart pane.start
            [c] -> ShellCommand c
            argv -> ExecArgv argv
        kind = if "-E" `elem` flags then EmptyPane else ProgramPane
    shellCmd <- paneShell =<< atomically (resolveForSession st sess)
    mnew <- respawnPane st sess.id win pane shellCmd how kind dir environ
    case mnew of
        Nothing -> pure (Left "pane no longer exists")
        Just new -> do
            applySessionSize st sess.id
            notifyPane st "pane-created" new
                [ ("pane_command", PText (spawnedCommand shellCmd how))
                , ("pane_current_path", PText (T.pack dir))
                , ("created_empty", PInt (if kind == EmptyPane then 1 else 0))
                , ("created_respawn", PInt 1) ]
            pure (Right ())
  where
    spawnedCommand shellCmd = \case
        ShellCommand c -> quoteIfNeeded c
        ExecArgv argv -> T.unwords (map quoteIfNeeded argv)
        _ -> T.pack shellCmd

-- | @swap-pane@ (@cmd-swap-pane.c@): exchange two panes' cells, within a
-- window or across two. The source is @-s@, else the marked pane, else the
-- current one; @-U@\/@-D@ instead take the target's previous\/next pane.
-- Without @-d@ the target becomes active; @-Z@ keeps a zoomed window zoomed.
cmdSwapPane :: CommandImpl
cmdSwapPane st mclient args
    | Just e <- unknownFlag "swap-pane" "dDUZ" flags = pure [RErr e]
    | otherwise = do
        dstRes <- findTarget st mclient Target.FindPane (lookup "-t" opts)
        case dstRes of
            Left e -> pure [RErr e]
            Right (dsess, dwix, dwin, dpane) -> do
                dfloating <- atomically (isFloating dpane.id <$> readTVar dwin.layout)
                srcRes <- if
                    | "-D" `elem` flags, dfloating ->
                        pure (Left "cannot swap down on floating pane")
                    | "-U" `elem` flags, dfloating ->
                        pure (Left "cannot swap up on floating pane")
                    | "-D" `elem` flags -> sibling dsess dwix dwin dpane 1
                    | "-U" `elem` flags -> sibling dsess dwix dwin dpane (-1)
                    | otherwise -> do
                        tok <- maybe (markedTarget st) (pure . Just) (lookup "-s" opts)
                        findTarget st mclient Target.FindPane tok
                case srcRes of
                    Left e -> pure [RErr e]
                    Right src@(_, _, swin, spane) -> do
                        modal <- atomically $ (,)
                            <$> (modalPane <$> readTVar swin.floats)
                            <*> (modalPane <$> readTVar dwin.floats)
                        if fst modal == Just spane.id || snd modal == Just dpane.id
                            then pure [RErr "pane is modal"]
                            else [] <$ swapPanes st (dsess, dwix, dwin, dpane) src
                                ("-d" `elem` flags) ("-Z" `elem` flags)
  where
    (opts, flags, _) = parseArgs "st" args
    -- The next or previous tiled pane, wrapping.
    sibling sess wix win pane step = do
        (order0, lay) <- atomically $
            (,) <$> (map (.id) <$> windowPanes win) <*> readTVar win.layout
        ps <- readTVarIO win.panes
        let order = filter (\p -> p == pane.id || not (isFloating p lay)) order0
            n = length order
            i = fromMaybe 0 (List.elemIndex pane.id order)
            pick = order !! ((i + step) `mod` n)
        pure $ case Map.lookup pick ps of
            Just p -> Right (sess, wix, win, p)
            Nothing -> Left "no such pane"

swapPanes
    :: ServerState -> (Session, Int, Window, Pane) -> (Session, Int, Window, Pane)
    -> Bool -> Bool -> IO ()
swapPanes st (dsess, dwix, dwin, dpane) (ssess, swix, swin, spane) detached keepZoom = do
    let sameWindow = swin.id == dwin.id
        wins = if sameWindow then [dwin] else [dwin, swin]
    rezoom <- atomically $ forM wins $ \w -> do
        z <- readTVar w.zoomed
        writeTVar w.zoomed Nothing
        pure (keepZoom && isJust z)
    unless (spane.id == dpane.id) $ do
        atomically $ do
            forM_ wins $ \w -> modifyTVar' w.floats (swapStacked spane.id dpane.id)
            if sameWindow
                then modifyTVar' dwin.layout (swapLeaves spane.id dpane.id)
                else do
                    modifyTVar' swin.layout (swapLeaves spane.id dpane.id)
                    modifyTVar' dwin.layout (swapLeaves spane.id dpane.id)
                    modifyTVar' swin.panes (Map.insert dpane.id dpane . Map.delete spane.id)
                    modifyTVar' dwin.panes (Map.insert spane.id spane . Map.delete dpane.id)
                    writeTVar spane.home (Just (dsess.id, dwin))  -- See Note [Pane home]
                    writeTVar dpane.home (Just (ssess.id, swin))  -- See Note [Pane home]
                    modifyTVar' swin.paneHist (filter (/= spane.id))
                    modifyTVar' dwin.paneHist (filter (/= dpane.id))
            let setActive w p = do
                    a <- readTVar w.activeId
                    when (a /= p) $ do
                        modifyTVar' w.paneHist (recordVisit a p)
                        writeTVar w.activeId p
            if not detached
                then if sameWindow
                    then setActive dwin dpane.id
                    else setActive swin dpane.id >> setActive dwin spane.id
                else do
                    sa <- readTVar swin.activeId
                    when (sa == spane.id) (setActive swin dpane.id)
                    da <- readTVar dwin.activeId
                    when (da == dpane.id) (setActive dwin spane.id)
            bumpDirty st
        unless sameWindow $ do
            sname <- readTVarIO swin.name
            dname <- readTVarIO dwin.name
            let moved p (ow, own, oix) (nw, nwn, nix) = notifyPane st "pane-moved" p
                    [ ("window", PWindowRef nw.id nwn)
                    , ("old_window", PWindowRef ow.id own)
                    , ("new_window", PWindowRef nw.id nwn)
                    , ("old_window_index", PInt oix)
                    , ("window_index", PInt nix)
                    , ("new_window_index", PInt nix) ]
            moved spane (swin, sname, swix) (dwin, dname, dwix)
            moved dpane (dwin, dname, dwix) (swin, sname, swix)
        forM_ (List.nubBy (\(_, a) (_, b) -> a.id == b.id) [(ssess, swin), (dsess, dwin)]) $
            \(sess, w) -> notifyWindow st "window-layout-changed" (Just sess.id) w []
    -- tmux's window_pop_zoom: re-zoom the active pane of a window -Z kept.
    atomically $ forM_ (zip wins rezoom) $ \(w, again) -> when again $ do
        ps <- readTVar w.panes
        a <- readTVar w.activeId
        when (Map.size ps > 1) (writeTVar w.zoomed (Just a))
    mapM_ (applySessionSize st . (.id)) (List.nubBy (\a b -> a.id == b.id) [ssess, dsess])

-- | @clear-history [-t target]@: drop a pane's scrollback.
cmdClearHistory :: CommandImpl
cmdClearHistory st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
    case res of
        Left e -> pure [RErr e]
        Right (_, _, _, pane) -> do
            Emu.clearScrollback pane.emulator
            [] <$ atomically (bumpDirty st)

-- | @resize-pane@ (@cmd-resize-pane.c@): @-T@ trims history, @-Z@ toggles
-- zoom; otherwise the window unzooms and the pane is resized ('resizeSteps').
cmdResizePane :: CommandImpl
cmdResizePane st mclient args
    | Just e <- unknownFlag "resize-pane" "DLMRTUZ" flags = pure [RErr e]
    -- -M follows the mouse: the border under the drag moves with it.
    -- Without a mouse event to follow, it does nothing.
    | "-M" `elem` flags = do
        mev <- clientMouseEvent mclient
        forM_ ((,) <$> mclient <*> mev) $ \(client, ev) -> do
            startDrag client DragBorder
            resizeBorderTo st ev
        pure []
    -- -T trims the pane's history below the cursor, unless a mode is open.
    | "-T" `elem` flags = do
        res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
        case res of
            Left e -> pure [RErr e]
            Right (_, _, _, pane) -> do
                inMode <- isJust <$> readTVarIO pane.mode
                unless inMode $ do
                    Emu.trimHistoryBelowCursor pane.emulator
                    atomically (bumpDirty st)
                pure []
    | "-Z" `elem` flags = do
        r <- zoomTarget st mclient (lookup "-t" opts)
        pure (either (\e -> [RErr e]) (const []) r)
    | otherwise = do
        res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
        case res of
            Left e -> pure [RErr e]
            Right (sess, _, win, pane) -> do
                eff <- atomically (windowArea st win)
                wasZoomed <- atomically $ do
                    z <- readTVar win.zoomed
                    writeTVar win.zoomed Nothing
                    when (isJust z) (bumpDirty st)
                    pure (isJust z)
                when wasZoomed $
                    notifyWindow st "window-unzoomed" (Just sess.id) win []
                border <- borderOf . (.paneBorderLines)
                    <$> atomically (resolveForPane st sess win pane)
                r <- atomically $ do
                    lay <- windowLayoutAt eff win
                    case resizeSteps pane.id border eff opts flags pos lay of
                        Left e -> pure (Left e)
                        Right lay' -> do
                            writeTVar win.layout lay'
                            Right () <$ bumpDirty st
                case r of
                    Left e -> pure [RErr e]
                    Right () -> do
                        applySessionSize st sess.id
                        notifyWindow st "window-layout-changed" (Just sess.id) win []
                        pure []
  where
    (opts, flags, pos) = parseArgs "txy" args

-- | The layout change a non-zoom @resize-pane@ makes to a pane in a window of
-- @area@: an absolute @-x@\/@-y@ (cells, or a percentage of the window), then
-- each of @-U@\/@-D@\/@-L@\/@-R@ moving it by the positional count (default
-- 1). A floating pane's box changes instead, its @-x@\/@-y@ counting its
-- border (@layout_resize_floating_pane_to@) and @-U@\/@-L@ growing it up
-- and left. Fails with tmux's message on a bad size or count.
resizeSteps
    :: PaneId -> Border -> Size -> [(Text, Text)] -> [Text] -> [Text]
    -> Layout -> Either Text Layout
resizeSteps pid border area opts flags pos lay0 = do
    xs <- absolute "-x" "width" (fromIntegral area.cols) LeftRight
    ys <- absolute "-y" "height" (fromIntegral area.rows) TopBottom
    rs <- traverse relative [ f | f <- ["-U", "-D", "-L", "-R"], f `elem` flags ]
    foldl (>>=) (Right lay0) (xs <> ys <> rs)
  where
    absolute flag what whole o = case lookup flag opts of
        Nothing -> Right []
        Just v -> case stringPercentage 0 10000 whole v of
            Left e -> Left (what <> " " <> e)
            Right n -> Right [\lay -> case floatGeom pid lay of
                Nothing -> Right (resizePaneTo pid o n lay)
                Just g -> do
                    let size | border == Bordered && n >= 3 = n - 2
                             | otherwise = n
                    if size < 1 || size > 10000
                        then Left "size size is too big or too small"
                        else Right (setFloatGeom pid (case o of
                            LeftRight -> FloatGeom size g.height g.left g.top
                            TopBottom -> FloatGeom g.width size g.left g.top) lay)]
    relative flag = do
        n <- either (Left . ("adjustment " <>)) Right
            (strtonum minBound maxBound (fromMaybe "1" (listToMaybe pos)))
        pure $ \lay -> case floatGeom pid lay of
            Nothing -> Right $ case flag of
                "-U" -> resizePane pid TopBottom (negate n) lay
                "-D" -> resizePane pid TopBottom n lay
                "-L" -> resizePane pid LeftRight (negate n) lay
                _ -> resizePane pid LeftRight n lay
            Just g -> do
                let vertical = flag `elem` ["-U", "-D"]
                    opposite = flag `elem` ["-U", "-L"]
                    size = (if vertical then g.height else g.width) + n
                    g' | vertical = FloatGeom g.width size g.left
                            (if opposite then g.top - n else g.top)
                       | otherwise = FloatGeom size g.height
                            (if opposite then g.left - n else g.left) g.top
                if n == 0 then Right lay
                else if size < 1 || size > 10000
                    then Left "adjustment change is too big or too small"
                    else Right (setFloatGeom pid g' lay)

-- | Toggle zoom on the target pane's window, the pane becoming active first
-- so the toggle keys off it: @resize-pane -t ! -Z@ zooms the alternate pane
-- even while another pane is already zoomed (as the config's @Z@ binding
-- intends). See 'nextZoom'. A solo pane has nothing to zoom over: the window
-- is left untouched.
zoomTarget :: ServerState -> Maybe Client -> Maybe Text -> IO (Either Text ())
zoomTarget st mclient mtok = do
    res <- findTarget st mclient Target.FindPane mtok
    case res of
        Left e -> pure (Left e)
        Right (sess, _, win, pane) -> Right <$> do
            toggled <- atomically $ do
                ps <- readTVar win.panes
                if Map.size ps < 2 then pure False else do
                    active <- readTVar win.activeId
                    when (active /= pane.id) $ do
                        modifyTVar' win.paneHist (recordVisit active pane.id)
                        writeTVar win.activeId pane.id
                    mz <- readTVar win.zoomed
                    writeTVar win.zoomed (nextZoom mz pane.id)
                    True <$ bumpDirty st
            when toggled $ do
                applySessionSize st sess.id
                mz <- readTVarIO win.zoomed
                case mz of
                    Just _ -> notifyWindow st "window-zoomed"
                        (Just sess.id) win []
                    Nothing -> notifyWindow st "window-unzoomed"
                        (Just sess.id) win []

-- | The zoom state after toggling zoom on a target pane: unzoom only when
-- that pane is the one already zoomed, otherwise zoom it. Keying off the
-- target (not merely whether some pane is zoomed) is what lets @resize-pane
-- -t ! -Z@ zoom the alternate pane even while another pane is zoomed. See
-- 'zoomTarget'.
nextZoom :: Maybe PaneId -> PaneId -> Maybe PaneId
nextZoom mz target
    | mz == Just target = Nothing
    | otherwise         = Just target
