{-# LANGUAGE MultiWayIf #-}

-- | The layout commands: @break-pane@\/@join-pane@\/@move-pane@ (moving a
-- pane between windows) and @select-layout@\/@next-layout@\/
-- @previous-layout@ (rearranging a window's panes).
module Hat.Server.Command.Layout
    ( cmdBreakPane
    , cmdJoinPane
    , cmdMovePane
    , cmdSelectLayout
    , cmdNextLayout
    , cmdPreviousLayout
    , namedSizesOf
    ) where

import Control.Concurrent.STM
import Control.Monad (forM_, unless, when)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import Hat.Server.ArgNumber (stringPercentage, strtonum)
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs, unknownFlag)
import Hat.Server.Command.Window
    ( SourceLink (..), applyShifts, closeSessions, linkWindow, nextFreeWindowIndex
    , switchToWindow )
import Hat.Server.Floating
    ( Border (..), FloatState (..), Restack (..), borderOf, floatingGeom, GeomArgs (..), modalPane, parseRestack
    , parseSpot, placeAt, restack, restackTo )
import Hat.Server.FormatEnv (expandFormat, paneFormatEnv, sessionFormatEnv)
import Hat.Server.Hooks (PayloadItem (..), notifyPane, notifyWindow)
import Hat.Server.Layout
import Hat.Server.LayoutString (layoutFromString)
import Hat.Server.Locate (findTarget, findWindowIndexTarget, markedTarget, paneIndexOf)
import Hat.Server.Mru (recordVisit)
import Hat.Server.Mouse (Drag (..))
import Hat.Server.MouseIO (clientMouseEvent, mouseDragUpdate, mouseEventPane, startDrag)
import Hat.Server.Pane (rehomePanes, removePaneFromTree, switchActive, wrapPaneInWindow)
import Hat.Server.Resize (applySessionSize, windowArea, windowLayoutAt)
import Hat.Server.Target qualified as Target
import Hat.Server.Winlink (attachWindow, shuffleUp)

-- | @break-pane@ (@cmd-break-pane.c@): move the source pane (@-s@, default
-- the current one) into a new window at the @-t@ index — after (@-a@) or
-- before (@-b@) the target window, shuffling later windows up — named by
-- @-n@. A window's only pane moves its whole window instead. The new window
-- is selected unless @-d@; @-P@ prints where the pane went (@-F@).
cmdBreakPane :: CommandImpl
cmdBreakPane st mclient args
    | Just e <- unknownFlag "break-pane" "abdPW" flags = pure [RErr e]
    | otherwise = do
        srcRes <- findTarget st mclient Target.FindPane (lookup "-s" opts)
        dstRes <- findWindowIndexTarget st mclient (lookup "-t" opts)
        modal <- either (const (pure Nothing))
            (\(_, _, swin, _) -> modalPane <$> readTVarIO swin.floats) srcRes
        case (srcRes, dstRes) of
            (Left e, _) -> pure [RErr e]
            (Right (_, _, _, pane), _) | modal == Just pane.id -> pure [RErr "pane is modal"]
            (Right (ssess, _, swin, pane), _) | "-W" `elem` flags -> floatOut ssess swin pane
            (_, Left e) -> pure [RErr e]
            (Right (ssess, swix, swin, pane), Right (dst, mix)) -> do
                atomically $ writeTVar swin.zoomed Nothing
                npanes <- Map.size <$> readTVarIO swin.panes
                if npanes == 1
                    then breakWholeWindow ssess swix swin pane dst
                    else breakOut ssess swix swin pane dst mix
  where
    (opts, flags, _) = parseArgs "FnstxXyY" args
    detached = "-d" `elem` flags
    -- -W: float the pane where it is (@cmd_break_pane_float@), at the
    -- -x/-y/-X/-Y geometry with the window's border.
    floatOut sess win pane = do
        eff <- atomically (windowArea st win)
        border <- borderOf . (.paneBorderLines) <$> atomically (resolveForWindow st sess win)
        env <- sessionFormatEnv st sess
        let expand v = case T.stripSuffix "%" v of
                Just body -> (<> "%") <$> expandFormat st env body
                Nothing -> expandFormat st env v
        geomArgs <- GeomArgs
            <$> traverse expand (lookup "-x" opts)
            <*> traverse expand (lookup "-y" opts)
            <*> traverse expand (lookup "-X" opts)
            <*> traverse expand (lookup "-Y" opts)
        r <- atomically $ do
            lay <- windowLayoutAt eff win
            fs <- readTVar win.floats
            zoomed <- isJust <$> readTVar win.zoomed
            if | isFloating pane.id lay -> pure (Left "pane is already floating")
               | zoomed -> pure (Left "can't float a pane while window is zoomed")
               | otherwise -> case floatingGeom eff border Nothing fs.lastNew geomArgs of
                    Left e -> pure (Left ("failed to float pane: " <> e))
                    Right (g, cascade) -> case floatTiled pane.id g lay of
                        Nothing -> pure (Left "pane is already floating")
                        Just lay' -> do
                            writeTVar win.layout lay'
                            writeTVar win.floats
                                (restack ToFront pane.id fs { lastNew = cascade } lay')
                            unless detached (() <$ switchActive win pane.id)
                            Right () <$ bumpDirty st
        case r of
            Left e -> pure [RErr e]
            Right () -> do
                applySessionSize st sess.id
                notifyWindow st "window-layout-changed" (Just sess.id) win []
                pure []
    shuffle = if
        | "-b" `elem` flags -> Just 0
        | "-a" `elem` flags -> Just 1
        | otherwise -> Nothing
    -- tmux relinks a lone pane's window rather than rebuilding it.
    breakWholeWindow ssess swix swin pane dst = do
        let srcTok = "$" <> tshow (rawSession ssess.id) <> ":" <> tshow swix
            dstTok = fromMaybe "" (lookup "-t" opts)
            linkFlags = filter (`elem` ["-a", "-b", "-d"]) flags
            linkOpts = [("-s", srcTok)] <> [ ("-t", dstTok) | isJust (lookup "-t" opts) ]
        replies <- linkWindow DropSource st mclient linkOpts linkFlags
        case [ e | RErr e <- replies ] of
            e : _ -> pure [RErr e]
            [] -> do
                forM_ (lookup "-n" opts) $ \nm -> atomically $ do
                    writeTVar swin.name nm
                    writeTVar swin.autoRename False
                links <- atomically $ do
                    ws <- readTVar dst.windows
                    pure [ ix | (ix, w) <- Map.toList ws, w.id == swin.id ]
                case links of
                    nix : _ -> do
                        movedNotice pane (swin, swix) (swin, nix)
                        printed dst nix swin pane
                    [] -> pure []
    breakOut ssess swix swin pane dst mix = do
        sopts <- atomically (resolveForSession st dst)
        ws0 <- readTVarIO dst.windows
        cur <- readTVarIO dst.currentIx
        let anchor = case mix of
                Just i | Map.member i ws0 -> i
                _ -> cur
            (shifts, requested) = case shuffle of
                Just off -> let ix = anchor + off in (shuffleUp ix ws0, Just ix)
                Nothing -> ([], mix)
            ws1 = applyShifts shifts ws0
        case requested of
            Just ix | Map.member ix ws1 -> pure [RErr ("index in use: " <> tshow ix)]
            _ -> do
                win2 <- wrapPaneInWindow st pane
                forM_ (lookup "-n" opts) $ \nm -> atomically $ do
                    writeTVar win2.name nm
                    writeTVar win2.autoRename False
                (ix, emptied) <- atomically $ do
                    let remap i = fromMaybe i (lookup i shifts)
                    modifyTVar' dst.windows (applyShifts shifts)
                    modifyTVar' dst.currentIx remap
                    modifyTVar' dst.windowHist (map remap)
                    emptied <- removePaneFromTree st pane.id
                    ws <- readTVar dst.windows
                    let ix = fromMaybe (nextFreeWindowIndex sopts.baseIndex ws) requested
                    _ <- attachWindow st dst ix win2
                    rehomePanes dst.id win2  -- See Note [Pane home]
                    bumpDirty st
                    pure (ix, emptied)
                closeSessions st emptied
                unless detached (switchToWindow st dst ix)
                notifyWindow st "window-layout-changed" (Just ssess.id) swin []
                movedNotice pane (swin, swix) (win2, ix)
                applySessionSize st ssess.id
                applySessionSize st dst.id
                printed dst ix win2 pane
    printed dst ix win pane
        | "-P" `elem` flags = do
            let fmt = fromMaybe "#{session_name}:#{window_index}.#{pane_index}"
                    (lookup "-F" opts)
            pix <- paneIndexOf st win pane
            env <- paneFormatEnv st dst ix win pix pane
            (\o -> [ROutput o]) <$> expandFormat st env fmt
        | otherwise = pure []
    movedNotice pane (ow, oix) (nw, nix) = do
        own <- readTVarIO ow.name
        nwn <- readTVarIO nw.name
        paneMoved st pane (ow, own, oix) (nw, nwn, nix)

-- | tmux's @window_fire_pane_moved@.
paneMoved :: ServerState -> Pane -> (Window, Text, Int) -> (Window, Text, Int) -> IO ()
paneMoved st pane (ow, own, oix) (nw, nwn, nix) = notifyPane st "pane-moved" pane
    [ ("window", PWindowRef nw.id nwn)
    , ("old_window", PWindowRef ow.id own)
    , ("new_window", PWindowRef nw.id nwn)
    , ("old_window_index", PInt oix)
    , ("window_index", PInt nix)
    , ("new_window_index", PInt nix) ]

-- | @join-pane@ (@cmd-join-pane.c@): move the source pane (@-s@, else the
-- marked pane, else the current one) into the target pane's window, split
-- off the target like @split-window@ (@-h@\/@-v@, @-b@, @-f@, @-l@\/@-p@).
-- An emptied source window is destroyed. Unless @-d@, the moved pane
-- becomes active and its window current.
cmdJoinPane :: CommandImpl
cmdJoinPane st mclient args
    | Just e <- unknownFlag "join-pane" "bdfhv" flags = pure [RErr e]
    | otherwise = do
        dstRes <- findTarget st mclient Target.FindPane (lookup "-t" opts)
        srcTok <- maybe (markedTarget st) (pure . Just) (lookup "-s" opts)
        srcRes <- findTarget st mclient Target.FindPane srcTok
        case (dstRes, srcRes) of
            (Left e, _) -> pure [RErr e]
            (_, Left e) -> pure [RErr e]
            (Right dst, Right src) -> joinPane st dst src opts flags
  where
    (opts, flags, _) = parseArgs "lpst" args

-- | @move-pane@ (@cmd-join-pane.c@): reposition a floating pane — to a
-- named spot or a stacking move (@-P@), a stacking depth (@-z@), a
-- position (@-X@\/@-Y@) or by a cell (@-U@\/@-D@\/@-L@\/@-R@) — or, with
-- none of those, join the source to it as @join-pane@ does. A tiled target
-- is refused. @-M@ moves the floating pane under the mouse with the drag
-- that ran it, making it active; without one, or over a tiled pane, it does
-- nothing.
cmdMovePane :: CommandImpl
cmdMovePane st mclient args
    | Just e <- unknownFlag "move-pane" "bdDfhLMRUv" flags = pure [RErr e]
    | "-M" `elem` flags = do
        mev <- clientMouseEvent mclient
        forM_ ((,) <$> mclient <*> mev) $ \(client, ev) -> do
            mp <- mouseEventPane st ev
            forM_ mp $ \(sess, win, pane, _) -> do
                lay <- readTVarIO win.layout
                when (isFloating pane.id lay) $ do
                    switched <- atomically (switchActive win pane.id)
                    when switched $ do
                        atomically (bumpDirty st)
                        applySessionSize st sess.id
                    startDrag client (DragMove pane.id)
                    mouseDragUpdate st (DragMove pane.id) ev
        pure []
    | otherwise = do
        res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
        case res of
            Left e -> pure [RErr e]
            Right dst@(dsess, _, win, pane) -> do
                eff <- atomically (windowArea st win)
                floating <- atomically $ do
                    z <- readTVar win.zoomed
                    lay <- readTVar win.layout
                    pure (isNothing z && isFloating pane.id lay)
                let moved = isJust (lookup "-X" opts) || isJust (lookup "-Y" opts)
                        || any (`elem` flags) ["-U", "-D", "-L", "-R"]
                if | not floating -> pure [RErr "pane is not floating"]
                   | Just s <- lookup "-P" opts ->
                        reposition dsess win eff pane (placeFloat s)
                   | Just s <- lookup "-z" opts -> case strtonum 0 maxBound s of
                        Left e -> pure [RErr ("z-index " <> e)]
                        Right z -> reposition dsess win eff pane
                            (\pid _ _ lay fs -> Right (lay, restackTo z pid fs lay))
                   | moved -> do
                        env <- sessionFormatEnv st dsess
                        let expand v = case T.stripSuffix "%" v of
                                Just body -> (<> "%") <$> expandFormat st env body
                                Nothing -> expandFormat st env v
                        mx <- traverse expand (lookup "-X" opts)
                        my <- traverse expand (lookup "-Y" opts)
                        reposition dsess win eff pane (shiftFloat mx my)
                   | otherwise -> do
                        srcTok <- maybe (markedTarget st) (pure . Just) (lookup "-s" opts)
                        srcRes <- findTarget st mclient Target.FindPane srcTok
                        case srcRes of
                            Left e -> pure [RErr e]
                            Right src -> joinPane st dst src opts flags
  where
    (opts, flags, _) = parseArgs "lPstXYz" args
    -- Change the window's layout and floating state, unzoomed, as one
    -- step; @how@ sees the pane, the window area, the pane's border, and
    -- both.
    reposition sess win eff pane how = do
        border <- borderOf . (.paneBorderLines)
            <$> atomically (resolveForPane st sess win pane)
        r <- atomically $ do
            writeTVar win.zoomed Nothing
            lay <- windowLayoutAt eff win
            fs <- readTVar win.floats
            case how pane.id eff border lay fs of
                Left e -> pure (Left e)
                Right (lay', fs') -> do
                    writeTVar win.layout lay'
                    writeTVar win.floats fs'
                    Right () <$ bumpDirty st
        case r of
            Left e -> pure [RErr e]
            Right () -> do
                notifyWindow st "window-layout-changed" (Just sess.id) win []
                pure []
    placeFloat s target eff border lay fs = case (parseRestack s, parseSpot s) of
        (Just how, _) -> Right (lay, restack how target fs lay)
        (_, Just spot) -> Right (moveTo (placeAt eff border spot), fs)
        _ -> Left ("unknown position: " <> s)
      where
        moveTo f = maybe lay (\g -> setFloatGeom target (f g) lay) (floatGeom target lay)
    shiftFloat mx my target eff border lay fs = do
        let wsx = fromIntegral eff.cols
            wsy = fromIntegral eff.rows
            shift = if border == Bordered then 1 else 0
            position e = either (Left . ("position " <>)) Right e
        g <- maybe (Left "pane is not floating") Right (floatGeom target lay)
        x <- maybe (Right g.left)
            (\v -> (+ shift) <$> position (stringPercentage (negate wsx) wsx wsx v)) mx
        y <- maybe (Right g.top)
            (\v -> (+ shift) <$> position (stringPercentage (negate wsy) wsy wsy v)) my
        steps <- traverse (\f -> either (Left . ("offset " <>)) (Right . (,) f)
                (strtonum minBound maxBound "1"))
            [ f | f <- ["-U", "-D", "-L", "-R"], f `elem` flags ]
        let (x', y') = foldl (\(cx, cy) (f, n) -> case f of
                "-U" -> (cx, cy - n)
                "-D" -> (cx, cy + n)
                "-L" -> (cx - n, cy)
                _ -> (cx + n, cy)) (x, y) steps
        pure (setFloatGeom target (FloatGeom g.width g.height x' y') lay, fs)

joinPane
    :: ServerState -> (Session, Int, Window, Pane) -> (Session, Int, Window, Pane)
    -> [(Text, Text)] -> [Text] -> IO [Reply]
joinPane st dst@(dsess, _, dwin, dpane) src@(_, _, swin, spane) opts flags = do
    (smodal, dmodal) <- atomically $
        (,) <$> (modalPane <$> readTVar swin.floats) <*> (modalPane <$> readTVar dwin.floats)
    atomically $ writeTVar dwin.zoomed Nothing >> writeTVar swin.zoomed Nothing
    floating <- atomically (isFloating spane.id <$> readTVar swin.layout)
    dfloating <- atomically (isFloating dpane.id <$> readTVar dwin.layout)
    if | smodal == Just spane.id || dmodal == Just dpane.id -> pure [RErr "pane is modal"]
       | spane.id == dpane.id && floating -> tileBack st dsess dwin spane flags
       | spane.id == dpane.id -> pure [RErr "source and target panes must be different"]
       | dfloating -> pure [RErr "size or position can't split a floating pane"]
       | otherwise -> joinTiled st dst src opts flags

-- | Tile a floating pane back where its cell sits (@cmd_join_pane_tile@).
tileBack :: ServerState -> Session -> Window -> Pane -> [Text] -> IO [Reply]
tileBack st sess win pane flags = do
    eff <- atomically (windowArea st win)
    ok <- atomically $ do
        lay <- windowLayoutAt eff win
        case tileFloating eff pane.id lay of
            Nothing -> pure False
            Just lay' -> do
                writeTVar win.layout lay'
                unless ("-d" `elem` flags) (() <$ switchActive win pane.id)
                True <$ bumpDirty st
    if not ok then pure [RErr "no space for a new pane"] else do
        applySessionSize st sess.id
        notifyWindow st "window-layout-changed" (Just sess.id) win []
        pure []

-- | 'joinPane' into a tiled target.
joinTiled
    :: ServerState -> (Session, Int, Window, Pane) -> (Session, Int, Window, Pane)
    -> [(Text, Text)] -> [Text] -> IO [Reply]
joinTiled st (dsess, dwix, dwin, dpane) (ssess, swix, swin, spane) opts flags = do
            eff <- atomically (windowArea st dwin)
            lay <- atomically (windowLayoutAt eff dwin)
            let expand t = sessionFormatEnv st dsess >>= \env -> expandFormat st env t
            let orient = if "-h" `elem` flags then LeftRight else TopBottom
                spanning = if "-f" `elem` flags then SpanWindow else SpanPane
                whole = case spanning of
                    SpanWindow -> eff
                    SpanPane -> maybe eff layoutArea (paneCell dpane.id lay)
                cur = case orient of
                    LeftRight -> fromIntegral whole.cols
                    TopBottom -> fromIntegral whole.rows
            msize <- case (lookup "-l" opts, lookup "-p" opts) of
                (Just l, _) -> do
                    v <- case T.stripSuffix "%" l of
                        Just body -> (<> "%") <$> expand body
                        Nothing -> expand l
                    pure (Just <$> stringPercentage 0 maxBound cur v)
                (Nothing, Just p) -> do
                    v <- expand p
                    pure (Just . (\n -> cur * n `div` 100) <$> strtonum 0 100 v)
                _ -> pure (Right Nothing)
            case msize of
                Left e -> pure [RErr ("size or position invalid tiled geometry " <> e)]
                Right size -> do
                    let spec = SplitSpec
                            { orient = orient, size = size
                            , placement = if "-b" `elem` flags then Before else After
                            , span = spanning }
                        -- The new cell is cut before the source's old one
                        -- closes, so it is planned under a stand-in id.
                        placeholder = PaneId (-1)
                    r <- atomically $ do
                        dlay <- windowLayoutAt eff dwin
                        case splitPane spec dpane.id placeholder dlay of
                            Nothing -> pure (Left "size or position no space for a new pane")
                            Just dlay' -> do
                                writeTVar dwin.layout dlay'
                                emptied <- removePaneFromTree st spane.id
                                modifyTVar' dwin.layout
                                    (swapLeaves placeholder spane.id)
                                modifyTVar' dwin.panes (Map.insert spane.id spane)
                                writeTVar spane.home (Just (dsess.id, dwin))  -- See Note [Pane home]
                                unless ("-d" `elem` flags) $ do
                                    a <- readTVar dwin.activeId
                                    when (a /= spane.id) $ do
                                        modifyTVar' dwin.paneHist (recordVisit a spane.id)
                                        writeTVar dwin.activeId spane.id
                                bumpDirty st
                                pure (Right emptied)
                    case r of
                        Left e -> pure [RErr e]
                        Right emptied -> do
                            closeSessions st emptied
                            unless ("-d" `elem` flags) (switchToWindow st dsess dwix)
                            own <- readTVarIO swin.name
                            nwn <- readTVarIO dwin.name
                            paneMoved st spane (swin, own, swix) (dwin, nwn, dwix)
                            alive <- not . Map.null <$> readTVarIO swin.panes
                            when (alive && swin.id /= dwin.id) $
                                notifyWindow st "window-layout-changed" (Just ssess.id) swin []
                            notifyWindow st "window-layout-changed" (Just dsess.id) dwin []
                            applySessionSize st ssess.id
                            applySessionSize st dsess.id
                            pure []

-- | @select-layout@ (@cmd-select-layout.c@): apply a named layout (a unique
-- prefix will do), the last one applied when none is named, or a layout
-- string; @-n@\/@-p@ step through the named layouts like @next-layout@\/
-- @previous-layout@ and @-E@ spreads the target pane's siblings evenly.
cmdSelectLayout :: CommandImpl
cmdSelectLayout st mclient args
    | Just e <- unknownFlag "select-layout" "Enop" flags = pure [RErr e]
    | "-o" `elem` flags = pure [RErr "select-layout -o is not implemented"]
    | otherwise = withLayoutTarget st mclient (lookup "-t" opts) $ \sess win pane -> if
        | "-n" `elem` flags -> stepLayout st sess win nextLayoutName
        | "-p" `elem` flags -> stepLayout st sess win previousLayoutName
        | "-E" `elem` flags -> relayout st sess win (pure . Right . spreadOut pane.id)
        | otherwise -> case pos of
            [] -> do
                mlast <- readTVarIO win.layoutName
                maybe (pure []) (applyNamed st sess win) mlast
            name : _ -> case lookupLayoutName name of
                Just n -> applyNamed st sess win n
                Nothing -> do
                    pids <- atomically (map (.id) <$> windowPanes win)
                    relayout st sess win $ \_ -> pure $ case layoutFromString name pids of
                        Left cause -> Left (cause <> ": " <> name)
                        Right lay -> Right lay
  where
    (opts, flags, pos) = parseArgs "t" args

cmdNextLayout :: CommandImpl
cmdNextLayout = cycleLayout "next-layout" nextLayoutName

cmdPreviousLayout :: CommandImpl
cmdPreviousLayout = cycleLayout "previous-layout" previousLayoutName

cycleLayout :: Text -> (Maybe LayoutName -> LayoutName) -> CommandImpl
cycleLayout name step st mclient args
    | Just e <- unknownFlag name "" flags = pure [RErr e]
    | otherwise = withLayoutTarget st mclient (lookup "-t" opts) $ \sess win _ ->
        stepLayout st sess win step
  where
    (opts, flags, _) = parseArgs "t" args

withLayoutTarget
    :: ServerState -> Maybe Client -> Maybe Text
    -> (Session -> Window -> Pane -> IO [Reply]) -> IO [Reply]
withLayoutTarget st mclient mtarget body = do
    res <- findTarget st mclient Target.FindPane mtarget
    case res of
        Left e -> pure [RErr e]
        Right (sess, _, win, pane) -> do
            atomically $ writeTVar win.zoomed Nothing
            body sess win pane

stepLayout :: ServerState -> Session -> Window -> (Maybe LayoutName -> LayoutName) -> IO [Reply]
stepLayout st sess win step = do
    cur <- readTVarIO win.layoutName
    applyNamed st sess win (step cur)

-- | Arrange a window into a named layout and remember it, so @next-layout@
-- cycles onward from it and a bare @select-layout@ reapplies it.
applyNamed :: ServerState -> Session -> Window -> LayoutName -> IO [Reply]
applyNamed st sess win name = do
    eff <- atomically (windowArea st win)
    sizes <- namedSizesOf <$> atomically (resolveForWindow st sess win)
    atomically $ writeTVar win.layoutName (Just name)
    relayout st sess win $ \lay -> pure . Right $
        fromMaybe lay (namedLayout sizes eff name (layoutPanes lay))

-- | The named layouts' sizing options.
namedSizesOf :: Options -> NamedSizes
namedSizesOf opts = NamedSizes
    { mainWidth = optionText "main-pane-width" opts
    , mainHeight = optionText "main-pane-height" opts
    , otherWidth = optionText "other-pane-width" opts
    , otherHeight = optionText "other-pane-height" opts
    , maxColumns = optionNumber "tiled-layout-max-columns" opts }

-- | Replace a window's layout (fitted to its area first) and announce it.
relayout
    :: ServerState -> Session -> Window -> (Layout -> IO (Either Text Layout))
    -> IO [Reply]
relayout st sess win change = do
    eff <- atomically (windowArea st win)
    lay <- atomically (windowLayoutAt eff win)
    r <- change lay
    case r of
        Left e -> pure [RErr e]
        Right lay' -> do
            atomically $ writeTVar win.layout lay' >> bumpDirty st
            applySessionSize st sess.id
            notifyWindow st "window-layout-changed" (Just sess.id) win []
            pure []
