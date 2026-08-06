-- | The pane commands: capturing a pane's grid (@capture-pane@), splitting
-- a window (@split-window@), selecting\/killing\/swapping panes, clearing
-- scrollback, resizing, and zooming.
module Hat.Server.Command.Pane
    ( CaptureOpts (..)
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
import Control.Monad (forM_, unless, void, when)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Data.Vector as V

import Hat.Geometry
import Hat.Model
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import Hat.Server.Layout
import Hat.Server.Locate (findTarget, locatePane, siblingPane, targetPane, withCurrentWindow)
import Hat.Server.Pane
    (killPaneLocs, sessionSpawnEnv, shellStart, spawnPane, startPaneReader)
import Hat.Server.Resize (applySessionSize)
import Hat.Server.View (expandFormat, sessionFormatEnv, windowArrange)
import qualified Hat.Server.Target as Target
import qualified Hat.Term.Cell as Cell
import qualified Hat.Term.Emulator as Emu

-- Only @-p@ (print to stdout, plain text) is supported so far; escape,
-- range, and hyperlink flags are ignored — enough for the light-touch
-- @capturep -p@ uses in upstream tests.
-- | Modifiers that shape a @capture-pane@ grid dump. See 'captureText'.
data CaptureOpts = CaptureOpts
    { captTrim   :: Bool  -- ^ trim trailing blanks (default; @-N@ turns it off)
    , captSeq    :: Bool  -- ^ @-e@: emit SGR escape sequences before styled runs
    , captNumber :: Bool  -- ^ @-L@: prefix each line with its screen-relative number
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

-- | Render captured rows into @capture-pane@ output. Each row is tagged with
-- its screen-relative line number (history lines negative), used by @-L@. A
-- wide character occupies one cell carrying its glyph; its continuation cell
-- carries empty text and is skipped. With @-e@, an SGR sequence precedes each
-- run whose style differs from the previous cell.
captureText :: CaptureOpts -> [(Int, V.Vector Cell.Cell)] -> Text
captureText opts = T.intercalate "\n" . map renderRow
  where
    renderRow (n, r) = numberPrefix n <> rowText r
    numberPrefix n
        | opts.captNumber = tshow n <> " "
        | otherwise       = ""
    rowText r =
        let body = T.concat (go Cell.defaultStyle (V.toList r))
        in if opts.captTrim then T.stripEnd body else body
    go _ [] = []
    go prev (cell : cs)
        | cell.width == 0 = cell.text : go prev cs
        | otherwise =
            let lead
                    | opts.captSeq && cell.style /= prev =
                        TE.decodeUtf8 (Emu.cellSgr cell.style)
                    | otherwise = ""
            in (lead <> cell.text) : go cell.style cs

cmdCapturePane :: CommandImpl
cmdCapturePane st mclient args = do
    let (opts, flags, _) = parseArgs "bESt" args
        has f = f `elem` flags
        rejected = filter has
            ["-J", "-F", "-H", "-R", "-M", "-a", "-P", "-T", "-C"]
    case () of
        _ | not (has "-p") ->
                pure [RErr "capture-pane: only stdout capture (-p) is supported"]
          | (bad : _) <- rejected ->
                pure [RErr ("capture-pane: " <> bad <> " is not supported")]
          | otherwise -> do
                res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
                case res of
                    Left e -> pure [RErr e]
                    Right (_, _, _, pane) -> do
                        scr <- Emu.snapshot pane.emulator
                        hsize <- Emu.scrollbackLength pane.emulator
                        let sy = V.length scr.cells
                            (top, bottom) =
                                captureBounds hsize sy (lookup "-S" opts) (lookup "-E" opts)
                        history <- mapM (Emu.scrollbackLine pane.emulator) [0 .. hsize - 1]
                        let allRows = [ fromMaybe V.empty h | h <- history ]
                                   <> V.toList scr.cells
                            picked =
                                [ (i - hsize, allRows !! i)
                                | i <- [top .. bottom], i >= 0, i < length allRows ]
                            copts = CaptureOpts
                                { captTrim   = not (has "-N")
                                , captSeq    = has "-e"
                                , captNumber = has "-L"
                                }
                        pure [ROutput (captureText copts picked)]

cmdSplitWindow :: CommandImpl
cmdSplitWindow st mclient args = do
    let (opts, flags, pos) = parseArgs "ctlp" args
        orient
            | "-h" `elem` flags = LeftRight
            | otherwise = TopBottom
        placement = if "-b" `elem` flags then Before else After
        -- @-f@: split spans the whole window, not just the active pane.
        full = "-f" `elem` flags
        mrun = case pos of
            [] -> Nothing
            ws -> Just (T.unwords ws)
    res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
    case res of
        Left e -> pure [RErr e]
        Right (sess, _, win, active) -> do
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
                        environ <- sessionSpawnEnv st sess
                        let shellCmd = maybe "/bin/sh" T.unpack
                                (List.lookup "SHELL" environ)
                        pane <- spawnPane st pid sess.id shellCmd (shellStart mrun)
                            dir environ (eff)
                        atomically $ do
                            modifyTVar' win.panes (Map.insert pane.id pane)
                            modifyTVar' win.layout $ if full
                                then splitFull orient placement pane.id
                                else splitLeaf active.id orient placement pane.id
                            lastA <- readTVar win.activeId
                            writeTVar win.lastActive (Just lastA)
                            writeTVar win.activeId pane.id
                            writeTVar win.zoomed Nothing
                            bumpDirty st
                        startPaneReader st sess.id win pane
                        applySessionSize st sess.id
                        pure []

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
        mPaneIndex = lookup "-t" opts >>= parsePaneIndex
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
                        atomically $
                            writeTVar st.markedPane (Just pane.id) >> bumpDirty st
                        pure []
            | "-l" `elem` flags -> cmdLastPane st mclient []
            | Just idx <- mPaneIndex ->
                withCurrentWindow st mclient $ \_ win -> do
                    atomically $ do
                        -- Relative cycling walks layout order; an absolute
                        -- index counts panes in window (creation) order.
                        order <- case idx of
                            IndexRelative _ _ -> layoutPanes <$> readTVar win.layout
                            IndexAbsolute _   -> Map.keys <$> readTVar win.panes
                        active <- readTVar win.activeId
                        forM_ (resolvePaneIndex idx order active) $ \next ->
                            when (next /= active) $ do
                                writeTVar win.lastActive (Just active)
                                writeTVar win.activeId next
                                bumpDirty st
                    pure []
            -- A full cmd-find pane target (e.g. @sess:win.%id@) activates
            -- that pane within its own window.
            | Just t <- lookup "-t" opts -> do
                res <- findTarget st mclient Target.FindPane (Just t)
                case res of
                    Left e -> pure [RErr e]
                    Right (_, _, win, pane) -> do
                        atomically $ do
                            active <- readTVar win.activeId
                            when (pane.id /= active) $ do
                                writeTVar win.lastActive (Just active)
                                writeTVar win.activeId pane.id
                                bumpDirty st
                        pure []
            | otherwise -> pure [RErr "usage: select-pane -L|-R|-U|-D|-l|-t index|:.[+-][N]"]
        Just dir -> withCurrentWindow st mclient $ \sess win -> do
            atomically $ do
                eff <- readTVar sess.lastSize
                lay <- readTVar win.layout
                active <- readTVar win.activeId
                forM_ (directionalTarget (eff) lay active dir) $ \next -> do
                    writeTVar win.lastActive (Just active)
                    writeTVar win.activeId next
                    -- Leaving a zoomed pane cancels the zoom (bug 5).
                    writeTVar win.zoomed Nothing
                    bumpDirty st
            pure []

cmdLastPane :: CommandImpl
cmdLastPane st mclient _ =
    withCurrentWindow st mclient $ \_ win -> do
        atomically $ do
            mlast <- readTVar win.lastActive
            ps <- readTVar win.panes
            forM_ mlast $ \lastP -> when (Map.member lastP ps) $ do
                cur <- readTVar win.activeId
                writeTVar win.lastActive (Just cur)
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
                            writeTVar win.lastActive (Just src.id)
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
                        writeTVar win.lastActive (Just active)
                        writeTVar win.activeId pane.id
                mz <- readTVar win.zoomed
                newActive <- readTVar win.activeId
                writeTVar win.zoomed (nextZoom mz newActive)
                True <$ bumpDirty st
        when toggled $ applySessionSize st sess.id
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
