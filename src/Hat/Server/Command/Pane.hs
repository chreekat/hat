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
    , cmdSelectPane
    , cmdLastPane
    , cmdKillPane
    , cmdSwapPane
    , cmdClearHistory
    , cmdResizePane
    , nextZoom
    , zoomTarget
    ) where

import Control.Concurrent.STM
import Control.Monad (forM, forM_, unless, void, when)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import Numeric (showOct)

import Hat.Geometry
import Hat.Model
import Hat.Server.Command.Buffer (storeBuffer)
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import Hat.Server.Layout
import Hat.Model.Options (quoteIfNeeded)
import Hat.Server.FormatEnv (paneFormatEnv)
import Hat.Server.Hooks
    ( NotifyTarget (..), PayloadItem (..), notify, notifyPane, notifyWindow )
import Hat.Server.Locate (findTarget, locatePane, paneIndexOf, siblingPane, targetPane, withCurrentWindow)
import Hat.Server.Mru (recordVisit)
import Hat.Server.Pane
    (killPaneLocs, sessionSpawnEnv, shellStart, spawnPane, startPaneReader)
import Hat.Server.Resize (applySessionSize)
import Hat.Server.View (expandFormat, sessionFormatEnv, windowArrange)
import qualified Hat.Server.Target as Target
import qualified Hat.Term.Cell as Cell
import qualified Hat.Term.Emulator as Emu
import Hat.Term.Emulator.Types (rtrimBlank)

-- | Modifiers that shape a @capture-pane@ grid dump. See 'captureText'.
data CaptureOpts = CaptureOpts
    { captJoin   :: Bool  -- ^ @-J@: join soft-wrapped rows
    , captNoTrim :: Bool  -- ^ @-N@: keep trailing blanks
    , captUsed   :: Bool  -- ^ @-T@: stop at the last used cell
    , captSeq    :: Bool  -- ^ @-e@: emit SGR escape sequences before styled runs
    , captOctal  :: Bool  -- ^ @-C@: octal-escape controls and backslash
    , captNumber :: Bool  -- ^ @-L@: prefix each line with its screen-relative number
    }
    deriving stock (Eq, Show)

-- | One captured grid row: its screen-relative line number (history rows
-- negative), whether it soft-wraps onto the next row, and its cells.
data CaptureRow = CaptureRow
    { number  :: Int
    , wrapped :: Bool
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
        in (numberPrefix r.number <> trimmed <> nl) : go pen' rs
    numberPrefix n
        | opts.captNumber = tshow n <> " "
        | otherwise       = ""
    -- tmux's cellused has no libghostty analogue: the used extent is
    -- approximated as everything up to the last non-default cell, and a
    -- wrapped row under @-J@ is fully used by construction.
    visible r
        | emptyCells || (opts.captJoin && r.wrapped) = V.toList r.cells
        | otherwise = rtrimBlank (V.toList r.cells)
    renderCells pen [] = ("", pen)
    renderCells pen (cell : cls)
        | cell.width == 0 = renderCells pen cls
        | otherwise =
            let lead | opts.captSeq = styleDiffSgr opts.captOctal pen cell.style
                     | otherwise    = ""
                (rest, pen') = renderCells cell.style cls
            in (lead <> cellText cell <> rest, pen')
    cellText cell
        | opts.captOctal = T.concatMap escapeOctal cell.text
        | otherwise      = cell.text

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
            }
    case filter has ["-F", "-H", "-P", "-R"] of
        (bad : _) ->
            pure [RErr ("capture-pane: " <> bad <> " is not implemented")]
        [] -> do
            res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
            case res of
                Left e -> pure [RErr e]
                Right (_, _, _, pane) -> do
                    ecap <- capturePane pane copts (has "-a") (has "-q")
                        (has "-M") (lookup "-S" opts) (lookup "-E" opts)
                    case ecap of
                        Left err -> pure [RErr err]
                        Right txt
                            | has "-p" -> pure
                                [ROutput (fromMaybe txt (T.stripSuffix "\n" txt))]
                            | otherwise -> [] <$ atomically
                                (storeBuffer st (lookup "-b" opts) txt)

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
                | otherwise -> do
                    let fg = pm.frozen
                        (top, bottom) = captureBounds fg.fgHsize fg.fgSy sflag eflag
                    pure . Right . captureText copts $
                        [ CaptureRow (i - fg.fgHsize) False
                            (fromMaybe V.empty (fg.fgRows V.!? i))
                        | i <- [top .. bottom] ]
            Nothing -> do
                scr <- Emu.snapshot pane.emulator
                hsize <- Emu.scrollbackLength pane.emulator
                let sy = V.length scr.cells
                    (top, bottom) = captureBounds hsize sy sflag eflag
                Right . captureText copts <$> mapM (liveRow hsize scr) [top .. bottom]
  where
    liveRow hsize scr i
        | i < hsize = CaptureRow (i - hsize)
            <$> wrapAt (Emu.scrollbackLineWrapped pane.emulator i)
            <*> (fromMaybe V.empty <$> Emu.scrollbackLine pane.emulator i)
        | otherwise = CaptureRow (i - hsize)
            <$> wrapAt (Emu.screenRowWrapped pane.emulator (i - hsize))
            <*> pure (fromMaybe V.empty (scr.cells V.!? (i - hsize)))
    wrapAt act = if copts.captJoin then act else pure False

cmdSplitWindow :: CommandImpl
cmdSplitWindow st mclient args = do
    let (opts, flags, pos) = parseArgs "ctlpeF" args
        orient
            | "-h" `elem` flags = LeftRight
            | otherwise = TopBottom
        placement = if "-b" `elem` flags then Before else After
        -- @-f@: split spans the whole window, not just the active pane.
        full = "-f" `elem` flags
        stay = "-d" `elem` flags
        envPairs = reverse
            [ (n, T.drop 1 v)
            | ("-e", nv) <- opts, let (n, v) = T.breakOn "=" nv ]
        mrun = case pos of
            [] -> Nothing
            ws -> Just (T.unwords ws)
    res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
    case res of
        Left e -> pure [RErr e]
        Right (sess, wix, win, active) -> do
                eff <- readTVarIO sess.lastSize
                (rects, _) <- atomically (windowArrange (eff) win)
                let mrect = List.lookup active.id rects
                    wholeRect = sizeRect (eff)
                    fitRect = if full then Just wholeRect else mrect
                    fits = case (orient, fitRect) of
                        (LeftRight, Just r) -> r.endCol - r.startCol >= 5
                        (TopBottom, Just r) -> r.endRow - r.startRow >= 5
                        _ -> False
                if not fits
                    then pure [RErr "create pane failed: pane too small"]
                    else do
                        pid <- PaneId <$> atomically (freshId st.nextPane)
                        dir <- case lookup "-c" opts of
                            Just d -> do
                                env <- sessionFormatEnv st sess
                                T.unpack <$> expandFormat st env d
                            Nothing -> paneCurrentPath active
                        environ0 <- sessionSpawnEnv st sess
                        let environ = environ0 <> envPairs
                            shellCmd = maybe "/bin/sh" T.unpack
                                (List.lookup "SHELL" environ)
                        pane <- spawnPane st pid sess.id shellCmd (shellStart mrun)
                            dir environ (eff)
                        atomically $ do
                            modifyTVar' win.panes (Map.insert pane.id pane)
                            modifyTVar' win.layout $ if full
                                then splitFull orient placement pane.id
                                else splitLeaf active.id orient placement pane.id
                            unless stay $ do
                                lastA <- readTVar win.activeId
                                modifyTVar' win.paneHist
                                    (recordVisit lastA pane.id)
                                writeTVar win.activeId pane.id
                            writeTVar win.zoomed Nothing
                            bumpDirty st
                        startPaneReader st sess.id win pane
                        applySessionSize st sess.id
                        notifyWindow st "window-layout-changed" (Just sess.id)
                            win []
                        let spawned = case mrun of
                                Just cmdText -> quoteIfNeeded cmdText
                                Nothing -> maybe "/bin/sh" id
                                    (List.lookup "SHELL" environ)
                        notifyPane st "pane-created" pane
                            [ ("pane_command", PText spawned)
                            , ("created_empty", PInt 0)
                            , ("created_respawn", PInt 0) ]
                        if "-P" `elem` flags
                            then do
                                let fmt = fromMaybe "#{session_name}:#{window_index}.#{pane_index}"
                                        (lookup "-F" opts)
                                pix <- paneIndexOf st win pane
                                env <- paneFormatEnv st sess wix win pix pane
                                out <- expandFormat st env fmt
                                pure [ROutput out]
                            else pure []

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

cmdSelectPane :: CommandImpl
cmdSelectPane st mclient args = do
    let (opts, flags, _) = parseArgs "tT" args
        mdir
            | "-L" `elem` flags = Just DirLeft
            | "-R" `elem` flags = Just DirRight
            | "-U" `elem` flags = Just DirUp
            | "-D" `elem` flags = Just DirDown
            | otherwise = Nothing
        -- The @-t@ pane-index tail: @:.+N@ / @:.-N@ cycle by N (default 1)
        -- and @:.N@ (or a bare number) selects an absolute index. See
        -- 'parsePaneIndex'\/'resolvePaneIndex'.
        -- Only bare index forms (@:.+N@, @.N@, @+@, @2@) take the
        -- index shortcut; anything naming a window resolves as a full
        -- target so @-t sess:0.1@ acts on that window, not the caller's.
        indexish t = T.all (`elem` (":.+-0123456789" :: String)) t
        mPaneIndex = do
            t <- lookup "-t" opts
            if indexish t then parsePaneIndex t else Nothing
    case mdir of
        Nothing
            | "-M" `elem` flags -> do
                atomically $ writeTVar st.markedPane Nothing >> bumpDirty st
                pure []
            | "-m" `elem` flags -> do
                res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
                case res of
                    Left e -> pure [RErr e]
                    Right (_, _, _, pane) -> do
                        -- Marking the already-marked pane unmarks it.
                        old <- atomically $ do
                            old <- readTVar st.markedPane
                            writeTVar st.markedPane
                                (if old == Just pane.id
                                    then Nothing else Just pane.id)
                            bumpDirty st
                            pure old
                        let marked = old /= Just pane.id
                        notifyPane st "marked-pane-changed" pane $
                            [ ("marked", PInt (if marked then 1 else 0)) ]
                            <> [ ("new_pane", PPaneRef pane.id) | marked ]
                            <> [ ("old_pane", PPaneRef p)
                               | Just p <- [old], not marked ]
                        pure []
            | "-l" `elem` flags -> cmdLastPane st mclient []
            | Just idx <- mPaneIndex ->
                withCurrentWindow st mclient $ \_ win ->
                  noteActiveChange st win $ do
                    atomically $ do
                        -- Relative cycling walks layout order; an absolute
                        -- index counts panes in window (creation) order.
                        order <- case idx of
                            IndexRelative _ _ -> layoutPanes <$> readTVar win.layout
                            IndexAbsolute _   -> Map.keys <$> readTVar win.panes
                        active <- readTVar win.activeId
                        forM_ (resolvePaneIndex idx order active) $ \next ->
                            when (next /= active) $ do
                                modifyTVar' win.paneHist (recordVisit active next)
                                writeTVar win.activeId next
                                bumpDirty st
                    pure []
            -- A full cmd-find pane target (e.g. @sess:win.%id@) activates
            -- that pane within its own window.
            | Just t <- lookup "-t" opts -> do
                res <- findTarget st mclient Target.FindPane (Just t)
                case res of
                    Left e -> pure [RErr e]
                    Right (_, _, win, pane) -> noteActiveChange st win $ do
                        atomically $ do
                            active <- readTVar win.activeId
                            when (pane.id /= active) $ do
                                modifyTVar' win.paneHist (recordVisit active pane.id)
                                writeTVar win.activeId pane.id
                                bumpDirty st
                        pure []
            | otherwise -> pure [RErr "usage: select-pane -L|-R|-U|-D|-l|-t index|:.[+-][N]"]
        Just dir -> withCurrentWindow st mclient $ \sess win ->
          noteActiveChange st win $ do
            atomically $ do
                eff <- readTVar sess.lastSize
                lay <- readTVar win.layout
                active <- readTVar win.activeId
                forM_ (directionalTarget (eff) lay active dir) $ \next -> do
                    modifyTVar' win.paneHist (recordVisit active next)
                    writeTVar win.activeId next
                    -- Leaving a zoomed pane cancels the zoom (bug 5).
                    writeTVar win.zoomed Nothing
                    bumpDirty st
            pure []

cmdLastPane :: CommandImpl
cmdLastPane st mclient _ =
    withCurrentWindow st mclient $ \_ win -> do
        atomically $ do
            hist <- readTVar win.paneHist
            ps <- readTVar win.panes
            forM_ (listToMaybe hist) $ \lastP -> when (Map.member lastP ps) $ do
                cur <- readTVar win.activeId
                writeTVar win.paneHist (recordVisit cur lastP hist)
                writeTVar win.activeId lastP
                bumpDirty st
        pure []

-- | Kill the target pane: detach it from the model first so the reflow is
-- synchronous with the command, then reap the child behind ('killPaneLocs').
cmdKillPane :: CommandImpl
cmdKillPane st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    mpane <- targetPane st mclient (lookup "-t" opts)
    forM_ mpane $ \pane -> do
        mloc <- atomically (locatePane st pane.id)
        forM_ mloc $ \(sid, win) -> killPaneLocs st [(sid, win, pane)]
    pure []

-- | @swap-pane [-s src] [-t dst] [-U|-D] [-d]@: exchange two panes'
-- positions. @src@ defaults to the active pane; without @-d@ the active
-- pane follows to @dst@'s slot, so the config's @splitw … \; swapp -t !
-- \; killp -t !@ edge-move idiom lands the content and kills the emptied
-- slot.
cmdSwapPane :: CommandImpl
cmdSwapPane st mclient args = do
    let (opts, flags, _) = parseArgs "st" args
        keepActive = "-d" `elem` flags
    withCurrentWindow st mclient $ \sess win -> do
        msrc <- targetPane st mclient (lookup "-s" opts)
        mdst <- case lookup "-t" opts of
            Just t -> targetPane st mclient (Just t)
            Nothing
                | "-U" `elem` flags -> siblingPane st win (-1)
                | "-D" `elem` flags -> siblingPane st win 1
                | otherwise -> pure Nothing
        case (msrc, mdst) of
            (Just src, Just dst) | src.id /= dst.id -> do
                atomically $ do
                    ps <- readTVar win.panes
                    when (Map.member src.id ps && Map.member dst.id ps) $ do
                        modifyTVar' win.layout (swapLeaves src.id dst.id)
                        unless keepActive $ do
                            modifyTVar' win.paneHist (recordVisit src.id dst.id)
                            writeTVar win.activeId dst.id
                        writeTVar win.zoomed Nothing
                        bumpDirty st
                applySessionSize st sess.id
                pure []
            _ -> pure []

-- | @clear-history [-t target]@: drop a pane's scrollback.
cmdClearHistory :: CommandImpl
cmdClearHistory st mclient args = do
    let (opts, _, _) = parseArgs "t" args
    mpane <- targetPane st mclient (lookup "-t" opts)
    forM_ mpane $ \pane -> do
        Emu.clearScrollback pane.emulator
        atomically (bumpDirty st)
    pure []

cmdResizePane :: CommandImpl
cmdResizePane st mclient args = do
    let (opts, flags, pos) = parseArgs "t" args
        delta = case pos of
            (n : _) | Right (v, restT) <- TR.decimal n, T.null restT -> v
            _ -> 1
    if "-Z" `elem` flags
        then zoomTarget st mclient (lookup "-t" opts) >> pure []
        else do
            let mdir
                    | "-L" `elem` flags = Just DirLeft
                    | "-R" `elem` flags = Just DirRight
                    | "-U" `elem` flags = Just DirUp
                    | "-D" `elem` flags = Just DirDown
                    | otherwise = Nothing
            case mdir of
                Nothing ->
                    pure [RErr "usage: resize-pane -L|-R|-U|-D [n] | -Z"]
                Just dir -> withCurrentWindow st mclient $ \sess win -> do
                    atomically $ do
                        eff <- readTVar sess.lastSize
                        active <- readTVar win.activeId
                        modifyTVar' win.layout
                            (resizeSplit active dir delta
                                (sizeRect (eff)))
                        bumpDirty st
                    applySessionSize st sess.id
                    pure []

-- | Toggle zoom on the caller's current window. With a @-t@ target the
-- targeted pane becomes active first and the toggle keys off it, so
-- @resize-pane -t ! -Z@ zooms the alternate pane even while another pane is
-- already zoomed (as the config's @Z@ binding intends). See 'nextZoom'.
-- A solo pane has nothing to zoom over: the window is left untouched.
zoomTarget :: ServerState -> Maybe Client -> Maybe Text -> IO ()
zoomTarget st mclient mtok = do
    mtarget <- targetPane st mclient mtok
    void . withCurrentWindow st mclient $ \sess win -> do
        toggled <- atomically $ do
            ps <- readTVar win.panes
            if Map.size ps < 2 then pure False else do
                forM_ mtarget $ \pane -> when (Map.member pane.id ps) $ do
                    active <- readTVar win.activeId
                    when (active /= pane.id) $ do
                        modifyTVar' win.paneHist (recordVisit active pane.id)
                        writeTVar win.activeId pane.id
                mz <- readTVar win.zoomed
                newActive <- readTVar win.activeId
                writeTVar win.zoomed (nextZoom mz newActive)
                True <$ bumpDirty st
        when toggled $ do
            applySessionSize st sess.id
            mz <- readTVarIO win.zoomed
            case mz of
                Just _ -> notifyWindow st "window-zoomed"
                    (Just sess.id) win []
                Nothing -> notifyWindow st "window-unzoomed"
                    (Just sess.id) win []
        pure []

-- | The zoom state after toggling zoom on a target pane: unzoom only when
-- that pane is the one already zoomed, otherwise zoom it. Keying off the
-- target (not merely whether some pane is zoomed) is what lets @resize-pane
-- -t ! -Z@ zoom the alternate pane even while another pane is zoomed. See
-- 'zoomTarget'.
nextZoom :: Maybe PaneId -> PaneId -> Maybe PaneId
nextZoom mz target
    | mz == Just target = Nothing
    | otherwise         = Just target
