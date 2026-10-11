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
import Data.Maybe (fromMaybe, isJust)
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
import Hat.Server.FormatEnv (expandFormat, paneFormatEnv, sessionFormatEnv)
import Hat.Server.Hooks (PayloadItem (..), notifyPane, notifyWindow)
import Hat.Server.Layout
import Hat.Server.LayoutString (layoutFromString)
import Hat.Server.Locate (findTarget, findWindowIndexTarget, markedTarget, paneIndexOf)
import Hat.Server.Mru (recordVisit)
import Hat.Server.Pane (rehomePanes, removePaneFromTree, wrapPaneInWindow)
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
    | "-W" `elem` flags = pure [RErr "break-pane -W (floating panes) is not implemented"]
    | f : _ <- [ f | f <- ["-x", "-X", "-y", "-Y"], isJust (lookup f opts) ] =
        pure [RErr ("break-pane " <> f <> " (floating panes) is not implemented")]
    | otherwise = do
        srcRes <- findTarget st mclient Target.FindPane (lookup "-s" opts)
        dstRes <- findWindowIndexTarget st mclient (lookup "-t" opts)
        case (srcRes, dstRes) of
            (Left e, _) -> pure [RErr e]
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

-- | @move-pane@: on this tmux branch it repositions a /floating/ pane, and
-- hat has none, so every pane it can target is refused as tmux refuses a
-- tiled one. @-M@ without a mouse event does nothing.
cmdMovePane :: CommandImpl
cmdMovePane st mclient args
    | Just e <- unknownFlag "move-pane" "bdDfhLMRUv" flags = pure [RErr e]
    | "-M" `elem` flags = pure []
    | otherwise = do
        res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
        pure $ case res of
            Left e -> [RErr e]
            Right _ -> [RErr "pane is not floating"]
  where
    (opts, flags, _) = parseArgs "lPstXYz" args

joinPane
    :: ServerState -> (Session, Int, Window, Pane) -> (Session, Int, Window, Pane)
    -> [(Text, Text)] -> [Text] -> IO [Reply]
joinPane st (dsess, dwix, dwin, dpane) (ssess, swix, swin, spane) opts flags = do
    atomically $ writeTVar dwin.zoomed Nothing >> writeTVar swin.zoomed Nothing
    if spane.id == dpane.id
        then pure [RErr "source and target panes must be different"]
        else do
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

-- | The named layouts' sizing options. @other-pane-width@\/@-height@ and
-- @tiled-layout-max-columns@ are not hat options yet, so they keep tmux's
-- defaults.
namedSizesOf :: Options -> NamedSizes
namedSizesOf opts = defaultNamedSizes
    { mainWidth = tshow opts.mainPaneWidth
    , mainHeight = tshow opts.mainPaneHeight }

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
