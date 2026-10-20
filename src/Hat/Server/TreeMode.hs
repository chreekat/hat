-- | The list modes built on "Hat.Server.ModeTree": choose-tree's
-- @tree-mode@ (tmux's @window-tree.c@), choose-buffer's @buffer-mode@
-- (@window-buffer.c@) and choose-client's @client-mode@
-- (@window-client.c@). Each rebuilds its items from the live server on
-- every draw and key, so the list follows the tree as it changes.
module Hat.Server.TreeMode
    ( cmdChooseTree
    , cmdChooseBuffer
    , cmdChooseClient
    , treeModeKey
    , drawTreeMode
    , modeClient
    , previewCells
    , styleIn
    ) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.ByteString qualified as B
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V

import Hat.Geometry (Pos (..), Rect (..), Size (..))
import Hat.Model
import Hat.Model.Options (Options (..), StatusPosition (..), optionText)
import Hat.Model.PasteBuffers (PasteBuffer (..), pasteWalk)
import Hat.Server.Command.Buffer (bufferEnv, formatTrue)
import Hat.Server.Command.Types
    (CommandImpl, Dispatch (..), Reply (..), parseArgs, templateReplace, unknownFlag)
import Hat.Server.Format (FormatEnv)
import Hat.Server.FormatDraw (FormatColours (..), formatTrimLeft, formatWidth, styleCell)
import Hat.Server.FormatEnv
    ( clientFormatEnv, clientTheme, expandFormat, paneFormatEnv, sessionFormatEnv
    , windowFormatEnv )
import Hat.Server.Keys (Key (..))
import Hat.Server.Locate (findTarget, paneIndexOf)
import Hat.Server.ModeTree
import Hat.Server.PaneMode (modifyTopMode, resetPaneMode, setPaneMode, zoomForMode)
import Hat.Server.Prompt (PromptEdit (..), editPrompt, noPromptEnv, promptFor)
import Hat.Server.Target qualified as Target
import Hat.Server.Render (overlayGrid)
import Hat.Server.Toast (showToast)
import Hat.Server.Theme (Theme)
import Hat.Term.Cell qualified as Cell
import Hat.Term.Emulator qualified as Emu

-- Entering --------------------------------------------------------------

-- | @choose-tree [-GhkNrswyZ] [-F format] [-f filter] [-K key-format]
-- [-O sort-order] [-t target-pane] [template]@.
cmdChooseTree :: CommandImpl
cmdChooseTree = enterTreeMode "choose-tree" "GhkNrswyZ" SessionTree

-- | @choose-buffer [-kNrZ] [-F format] [-f filter] [-K key-format]
-- [-O sort-order] [-t target-pane] [template]@: nothing to choose from
-- without a buffer.
cmdChooseBuffer :: CommandImpl
cmdChooseBuffer = enterTreeMode "choose-buffer" "kNryZ" BufferList

-- | @choose-client [-hikNrZ] [-F format] [-f filter] [-K key-format]
-- [-O sort-order] [-t target-pane] [template]@: nothing to choose from
-- without an attached client.
cmdChooseClient :: CommandImpl
cmdChooseClient = enterTreeMode "choose-client" "hikNryZ" ClientList

enterTreeMode :: Text -> [Char] -> TreeKind -> CommandImpl
enterTreeMode cmdName allowed kind st mclient args
    | Just e <- unknownFlag cmdName allowed flags = pure [RErr e]
    | Just o <- lookup "-O" opts, isNothing (sortOrderFromText o) =
        pure [RErr "invalid sort order"]
    | otherwise = do
        res <- findTarget st mclient Target.FindPane (lookup "-t" opts)
        case res of
            Left e -> pure [RErr e]
            Right (sess, wix, win, pane) -> do
                empty <- case kind of
                    SessionTree -> pure False
                    BufferList -> null . pasteWalk <$> readTVarIO st.buffers
                    ClientList -> null <$> attachedClients st
                if empty then pure [] else do
                    _ <- setPaneMode st pane (kindName kind)
                        (TreeMode <$> start sess wix win pane) exit
                    pure []
  where
    (opts, flags, pos) = parseArgs "FfKOt" args
    has f = f `elem` flags
    exit = if has "-k" then KillPane else KeepPane
    start sess wix win pane = do
        zoomed <- if has "-Z" then zoomForMode st sess win pane else pure ModeLeftZoom
        let origin = TreeOrigin
                { fromSession = sess.id, fromWinlink = wix, fromPane = pane.id }
            st0 = TreeState
                { kind = kind
                , origin = origin
                , itemFormat = fromMaybe (defaultFormat kind) (lookup "-F" opts)
                , keyFormat = lookup "-K" opts
                , command = case pos of
                    t : _ -> t
                    [] -> defaultCommand kind
                , filter = lookup "-f" opts
                , sort = TreeSort
                    { order = fromMaybe (defaultSort kind)
                        (lookup "-O" opts >>= sortOrderFromText)
                    , reversed = has "-r" }
                , preview = case length (filter (== "-N") flags) of
                    0 -> PreviewNormal
                    1 -> PreviewOff
                    _ -> PreviewBig
                , view = ViewPreview
                , expanded = Map.empty
                , tagged = []
                , current = Nothing
                , currentIx = 0
                , offset = 0
                , modePrompt = Nothing
                , search = Nothing
                , hidePane = if has "-h" then Just pane.id else Nothing
                , groups = if has "-G" then ShowAllGroups else SquashGroups
                , confirm = if has "-y" then KillWithoutAsking else AskBeforeKill
                , zoom = zoomed
                , showHelp = False
                }
        (items, _) <- buildItems st st0
        pure (startSelection items st0)
    -- tmux builds the first list with -s collapsing sessions and windows
    -- and -w collapsing windows, starting on the target; every later item
    -- arrives expanded.
    startSelection items st0 = case kind of
        SessionTree ->
            let o = st0.origin
                sessions = [ i | i <- items ]
                windows = concatMap (.children) sessions
                collapse ts = Map.fromList [ (t, False) | t <- ts ]
                expansion
                    | has "-s" = collapse (map (.tag) sessions <> map (.tag) windows)
                    | has "-w" = collapse (map (.tag) windows)
                    | otherwise = Map.empty
                paneCount = length
                    [ () | w <- windows, w.tag == TagWinlink o.fromSession o.fromWinlink
                         , _ <- w.children ]
                start'
                    | has "-s" = TagSession o.fromSession
                    | has "-w" || paneCount == 1 = TagWinlink o.fromSession o.fromWinlink
                    | otherwise = TagPane o.fromPane
            in st0 { expanded = expansion, current = Just start' }
        _ -> st0

-- | A tree kind's mode name, as @#{pane_mode}@ shows it.
kindName :: TreeKind -> Text
kindName = \case
    SessionTree -> "tree-mode"
    BufferList -> "buffer-mode"
    ClientList -> "client-mode"

defaultFormat :: TreeKind -> Text
defaultFormat = \case
    SessionTree ->
        "#{?pane_format,\
            \#{?pane_marked,#[fg=thememagenta],}#{?pane_floating_flag,#[underscore],}\
            \#{pane_current_command}#[fg=themelightgrey]#{pane_flags}\
            \#{?#{&&:#{pane_title},#{!=:#{pane_title},#{host_short}}},: \"#{pane_title}\",}\
        \,window_format,\
            \#{?window_marked_flag,#[fg=thememagenta],}\
            \#{window_name}#[fg=themelightgrey]#{window_flags}\
            \#{?#{&&:#{==:#{window_panes},1},#{&&:#{pane_title},#{!=:#{pane_title},#{host_short}}}},: \"#{pane_title}\",}\
        \,\
            \#[fg=themelightgrey]#{session_windows} windows\
            \#{?session_grouped, (group #{session_group}: #{session_group_list}),}\
            \#{?session_attached, (attached),}\
        \}"
    BufferList -> "#{t/p:buffer_created}: #{buffer_sample}"
    ClientList ->
        "#[fg=themelightgrey]#{t/p:client_activity}: session #[default]#{session_name}"

defaultCommand :: TreeKind -> Text
defaultCommand = \case
    SessionTree -> "switch-client -Zt '%%'"
    BufferList -> "paste-buffer -p -b '%%'"
    ClientList -> "detach-client -t '%%'"

-- | The order a kind's list starts in: the first its @O@ key cycles.
defaultSort :: TreeKind -> SortOrder
defaultSort kind = case sortSequence kind of
    o : _ -> o
    [] -> SortNatural

sortSequence :: TreeKind -> SortSequence
sortSequence = \case
    SessionTree -> [SortIndex, SortName, SortActivity, SortZ]
    BufferList -> [SortCreation, SortName, SortSize]
    ClientList -> [SortName, SortSize, SortCreation, SortActivity]

helpText :: TreeKind -> HelpText
helpText = \case
    SessionTree -> HelpText "item" 51 $ map ("#[fg=themelightgrey]" <>)
        [ "      Enter #[#{E:tree-mode-border-style},acs]x#[default] Choose selected item"
        , "       S-Up #[#{E:tree-mode-border-style},acs]x#[default] Swap current and previous window"
        , "     S-Down #[#{E:tree-mode-border-style},acs]x#[default] Swap current and next window"
        , "          x #[#{E:tree-mode-border-style},acs]x#[default] Kill selected item"
        , "          X #[#{E:tree-mode-border-style},acs]x#[default] Kill tagged items"
        , "          < #[#{E:tree-mode-border-style},acs]x#[default] Scroll previews left"
        , "          > #[#{E:tree-mode-border-style},acs]x#[default] Scroll previews right"
        , "          m #[#{E:tree-mode-border-style},acs]x#[default] Set the marked pane"
        , "          M #[#{E:tree-mode-border-style},acs]x#[default] Clear the marked pane"
        , "          i #[#{E:tree-mode-border-style},acs]x#[default] Toggle session, window and pane information"
        , "          : #[#{E:tree-mode-border-style},acs]x#[default] Run a command for each tagged item"
        , "          f #[#{E:tree-mode-border-style},acs]x#[default] Enter a format"
        , "          H #[#{E:tree-mode-border-style},acs]x#[default] Jump to the starting pane"
        ]
    BufferList -> HelpText "buffer" 0 $ map ("#[fg=themelightgrey]" <>)
        [ "      Enter #[#{E:tree-mode-border-style},acs]x#[default] Paste selected %1"
        , "          p #[#{E:tree-mode-border-style},acs]x#[default] Paste selected %1"
        , "          P #[#{E:tree-mode-border-style},acs]x#[default] Paste tagged %1s"
        , "          d #[#{E:tree-mode-border-style},acs]x#[default] Delete selected %1"
        , "          D #[#{E:tree-mode-border-style},acs]x#[default] Delete tagged %1s"
        , "          e #[#{E:tree-mode-border-style},acs]x#[default] Open %1 in editor"
        , "          f #[#{E:tree-mode-border-style},acs]x#[default] Enter a filter"
        ]
    ClientList -> HelpText "client" 0 $ map ("#[fg=themelightgrey]" <>)
        [ "          i #[#{E:tree-mode-border-style},acs]x#[default] Toggle info view"
        , "      Enter #[#{E:tree-mode-border-style},acs]x#[default] Choose selected %1"
        , "          d #[#{E:tree-mode-border-style},acs]x#[default] Detach selected %1"
        , "          D #[#{E:tree-mode-border-style},acs]x#[default] Detach tagged %1s"
        , "          x #[#{E:tree-mode-border-style},acs]x#[default] Detach selected %1"
        , "          X #[#{E:tree-mode-border-style},acs]x#[default] Detach tagged %1s"
        , "          z #[#{E:tree-mode-border-style},acs]x#[default] Suspend selected %1"
        , "          Z #[#{E:tree-mode-border-style},acs]x#[default] Suspend tagged %1s"
        , "          f #[#{E:tree-mode-border-style},acs]x#[default] Enter a filter"
        ]

-- Building --------------------------------------------------------------

-- | The mode's items under its filter; when the filter matches nothing,
-- every item, flagged as no matches (tmux's @mode_tree_build@).
buildItems :: ServerState -> TreeState -> IO ([Item], Bool)
buildItems st ts = do
    filtered <- itemsFor st ts ts.filter
    case (filtered, ts.filter) of
        ([], Just _) -> (\is -> (is, True)) <$> itemsFor st ts Nothing
        _ -> pure (filtered, False)

itemsFor :: ServerState -> TreeState -> Maybe Text -> IO [Item]
itemsFor st ts mfilter = case ts.kind of
    SessionTree -> sessionItems st ts mfilter
    BufferList -> bufferItems st ts mfilter
    ClientList -> clientItems st ts mfilter

-- | Whether a filter keeps an item with this environment.
keeps :: ServerState -> Maybe Text -> FormatEnv -> IO Bool
keeps st mfilter env = case mfilter of
    Nothing -> pure True
    Just f -> formatTrue <$> expandFormat st env f

-- | Order a list by a sort, ties broken by name, reversed on request; the
-- natural order is only ever reversed.
sortedBy :: TreeSort -> (a -> Text) -> (SortOrder -> Maybe (a -> a -> Ordering)) -> [a] -> [a]
sortedBy srt nameOf primary xs = case primary srt.order of
    _ | srt.order == SortNatural -> (if srt.reversed then reverse else id) xs
    Nothing -> sortBy (flipIf (comparing nameOf)) xs
    Just cmp -> sortBy (flipIf (\a b -> cmp a b <> comparing nameOf a b)) xs
  where
    flipIf c a b = if srt.reversed then c b a else c a b

sessionItems :: ServerState -> TreeState -> Maybe Text -> IO [Item]
sessionItems st ts mfilter = do
    sessions <- Map.elems <$> readTVarIO st.sessions
    infos <- forM sessions $ \s -> atomically $ do
        nm <- readTVar s.name
        g <- readTVar s.group
        ws <- Map.elems <$> readTVar s.windows
        acts <- mapM (\w -> readTVar w.activityAt) ws
        pure (s, nm, g, maximum (0 : acts))
    let byName = sortedBy (TreeSort SortName False) (\(_, n, _, _) -> n) (const Nothing) infos
        originGroup = listToMaybe [ g | (s, _, g, _) <- infos, s.id == ts.origin.fromSession ]
        shown (s, _, g, _) = case (ts.groups, g) of
            (SquashGroups, Just grp)
                | Just (Just grp) == originGroup -> s.id == ts.origin.fromSession
                | otherwise -> Just s.id == listToMaybe
                    [ s'.id | (s', _, g', _) <- sortOn' infos, g' == Just grp ]
            _ -> True
        sortOn' = sortBy (comparing (\(s, _, _, _) -> s.id))
        ordered = sortedBy ts.sort (\(_, n, _, _) -> n) (\case
            SortIndex -> Just (comparing (\(s, _, _, _) -> s.id))
            SortCreation -> Just (comparing (\(s, _, _, _) -> s.createdAt))
            SortActivity -> Just (comparing (\(_, _, _, a) -> Down a))
            _ -> Nothing) byName
    catMaybes <$> mapM (sessionItem . (\(s, _, _, _) -> s)) (filter shown ordered)
  where
    sessionItem sess = do
        nm <- readTVarIO sess.name
        ws <- Map.toAscList <$> readTVarIO sess.windows
        winfos <- forM ws $ \(ix, w) -> atomically $
            (,,,) ix w <$> readTVar w.name <*> readTVar w.activityAt
        let ordered = sortedBy ts.sort (\(_, _, n, _) -> n) (\case
                SortIndex -> Just (comparing (\(ix, _, _, _) -> ix))
                SortCreation -> Just (comparing (\(_, w, _, _) -> w.id))
                SortActivity -> Just (comparing (\(_, _, _, a) -> Down a))
                _ -> Nothing) winfos
        wins <- catMaybes <$> mapM (\(ix, w, _, _) -> windowItem sess ix w) ordered
        if null wins then pure Nothing else do
            env <- sessionFormatEnv st sess
            text <- expandFormat st (Map.union (levelVars SessionLevel) env) ts.itemFormat
            pure $ Just Item
                { tag = TagSession sess.id, name = nm, text = Just text
                , children = wins, startExpanded = True, align = AlignNone
                , searchText = [nm], fuzzyName = Just nm }
    windowItem sess ix win = do
        panes <- atomically (windowPanes win)
        pinfos <- forM panes $ \p -> do
            pix <- paneIndexOf st win p
            env <- paneFormatEnv st sess ix win pix p
            pure (p, pix, env)
        hist <- readTVarIO win.paneHist
        let titled (_, _, env) = Map.findWithDefault "" "pane_title" env
            ordered = sortedBy ts.sort titled (\case
                SortIndex -> Just (comparing (\(_, pix, _) -> pix))
                SortCreation -> Just (comparing (\(p, _, _) -> p.id))
                SortActivity -> Just (comparing (\(p, _, _) ->
                    fromMaybe maxBound (lookup p.id (zip hist [0 :: Int ..]))))
                _ -> Nothing) pinfos
        matched <- filterM (\(_, _, env) -> keeps st mfilter env) ordered
        if null matched then pure Nothing else do
            paneItems <- forM [ m | m@(p, _, _) <- matched, Just p.id /= ts.hidePane ] $
                \(p, pix, env) -> do
                    text <- expandFormat st (Map.union (levelVars PaneLevel) env) ts.itemFormat
                    pure Item
                        { tag = TagPane p.id, name = tshow pix, text = Just text
                        , children = [], startExpanded = True, align = AlignRight
                        , searchText = [Map.findWithDefault "" "pane_current_command" env]
                        , fuzzyName = Nothing }
            wenv <- windowFormatEnv st sess ix win
            text <- expandFormat st (Map.union (levelVars WindowLevel) wenv) ts.itemFormat
            wname <- readTVarIO win.name
            pure $ Just Item
                { tag = TagWinlink sess.id ix, name = tshow ix, text = Just text
                , children = paneItems, startExpanded = True, align = AlignRight
                , searchText = [wname], fuzzyName = Just wname }

-- | Which level of the session tree an item stands for.
data ItemLevel = SessionLevel | WindowLevel | PaneLevel
    deriving (Eq)

-- | tmux's @session_format@, @window_format@ and @pane_format@: which kind
-- of item a format is expanding for.
levelVars :: ItemLevel -> FormatEnv
levelVars level = Map.fromList
    [ ("session_format", flag SessionLevel)
    , ("window_format", flag WindowLevel)
    , ("pane_format", flag PaneLevel) ]
  where
    flag l = if l == level then "1" else "0"

bufferItems :: ServerState -> TreeState -> Maybe Text -> IO [Item]
bufferItems st ts mfilter = do
    bufs <- pasteWalk <$> readTVarIO st.buffers
    targetEnv <- originEnv st ts
    let ordered = sortedBy ts.sort (.name) (\case
            SortCreation -> Just (comparing (Down . (.order)))
            SortSize -> Just (comparing (B.length . (.body)))
            _ -> Nothing) bufs
    fmap catMaybes . forM ordered $ \pb -> do
        let env = Map.union (bufferEnv pb) targetEnv
        ok <- keeps st mfilter env
        if not ok then pure Nothing else do
            text <- expandFormat st env ts.itemFormat
            pure $ Just Item
                { tag = TagBuffer pb.order, name = pb.name, text = Just text
                , children = [], startExpanded = True, align = AlignNone
                , searchText = [pb.name, TE.decodeUtf8Lenient pb.body]
                , fuzzyName = Just pb.name }

clientItems :: ServerState -> TreeState -> Maybe Text -> IO [Item]
clientItems st ts mfilter = do
    cs <- attachedClients st
    infos <- forM cs $ \c -> do
        sz <- readTVarIO c.size
        act <- readTVarIO c.lastActive
        pure (c, clientName c, sz, act)
    let ordered = sortedBy ts.sort (\(_, n, _, _) -> n) (\case
            SortSize -> Just (comparing (\(_, _, sz, _) -> (sz.cols, sz.rows)))
            SortCreation -> Just (comparing (\(c, _, _, _) -> c.id))
            SortActivity -> Just (comparing (\(_, _, _, a) -> Down a))
            _ -> Nothing) infos
    fmap catMaybes . forM ordered $ \(c, nm, _, _) -> do
        menv <- clientFormatEnv st c
        case menv of
            Nothing -> pure Nothing
            Just env -> do
                ok <- keeps st mfilter env
                if not ok then pure Nothing else do
                    text <- expandFormat st env ts.itemFormat
                    pure $ Just Item
                        { tag = TagClient c.id, name = nm, text = Just text
                        , children = [], startExpanded = True, align = AlignNone
                        , searchText = [nm], fuzzyName = Just nm }

-- | Clients attached to a live session, the ones client-mode lists.
attachedClients :: ServerState -> IO [Client]
attachedClients st = do
    cs <- Map.elems <$> readTVarIO st.clients
    filterM (\c -> isAttachedTo c) [ c | c <- cs, c.role == Attached ]
  where
    isAttachedTo c = atomically $ do
        sid <- readTVar c.session
        Map.member sid <$> readTVar st.sessions

-- | The format environment of the pane the mode was entered on.
originEnv :: ServerState -> TreeState -> IO FormatEnv
originEnv st ts = do
    mloc <- atomically (findPaneLoc st ts.origin.fromPane)
    case mloc of
        Nothing -> pure Map.empty
        Just (sess, ix, win, pane) -> do
            pix <- paneIndexOf st win pane
            paneFormatEnv st sess ix win pix pane

findPaneLoc :: ServerState -> PaneId -> STM (Maybe (Session, Int, Window, Pane))
findPaneLoc st pid = do
    sessions <- Map.elems <$> readTVar st.sessions
    hits <- forM sessions $ \s -> do
        ws <- Map.toList <$> readTVar s.windows
        fmap catMaybes . forM ws $ \(ix, w) -> do
            ps <- readTVar w.panes
            pure ((,,,) s ix w <$> Map.lookup pid ps)
    pure (listToMaybe (concat hits))

-- | The mode's lines over its rebuilt items, each with the key that
-- chooses it, and the state settled onto them.
layoutTree
    :: ServerState -> Size -> TreeState -> IO ([Item], [Line], Bool, Int, TreeState)
layoutTree st size ts = do
    (items, noMatches) <- buildItems st ts
    let (shown, lts) = shownItems ts items
        ls0 = buildLines lts shown
    ls <- forM (zip [0 ..] ls0) $ \(i, l) -> do
        k <- lineKey i l
        pure l { key = k }
    let h = listHeight (fromIntegral size.rows) (length ls) ts.preview
    pure (items, ls, noMatches, h, settle h ls ts)
  where
    lineKey i l = case ts.keyFormat of
        Nothing -> pure (defaultLineKey i)
        Just kf -> do
            env <- itemEnv st l.item.tag
            k <- expandFormat st (Map.insert "line" (tshow i) env) kf
            pure (if T.null k then Nothing else Just k)

-- | The format environment of an item, for a custom key format.
itemEnv :: ServerState -> TreeTag -> IO FormatEnv
itemEnv st = \case
    TagSession sid -> do
        msess <- Map.lookup sid <$> readTVarIO st.sessions
        maybe (pure Map.empty) (sessionFormatEnv st) msess
    TagWinlink sid ix -> do
        msess <- Map.lookup sid <$> readTVarIO st.sessions
        case msess of
            Nothing -> pure Map.empty
            Just sess -> do
                mwin <- Map.lookup ix <$> readTVarIO sess.windows
                maybe (pure Map.empty) (windowFormatEnv st sess ix) mwin
    TagPane pid -> do
        mloc <- atomically (findPaneLoc st pid)
        case mloc of
            Nothing -> pure Map.empty
            Just (sess, ix, win, pane) -> do
                pix <- paneIndexOf st win pane
                paneFormatEnv st sess ix win pix pane
    TagBuffer o -> do
        bufs <- pasteWalk <$> readTVarIO st.buffers
        pure (maybe Map.empty bufferEnv (listToMaybe [ b | b <- bufs, b.order == o ]))
    TagClient cid -> do
        mc <- Map.lookup cid <$> readTVarIO st.clients
        maybe (pure Map.empty) (fmap (fromMaybe Map.empty) . clientFormatEnv st) mc

-- Drawing ---------------------------------------------------------------

-- | Draw a pane's tree mode at the pane's size: the list, and the preview
-- of the selected item in its box. The cursor shows only for a prompt.
drawTreeMode
    :: ServerState -> TreeState -> Size
    -> IO (V.Vector (V.Vector Cell.Cell), Maybe Pos)
drawTreeMode st ts size = do
    (_, ls, noMatches, _, settled) <- layoutTree st size ts
    opts <- readTVarIO st.options
    look <- treeLook st ts
    help <- if ts.showHelp
        then mapM look.expand (helpFormats (helpText ts.kind))
        else pure []
    let drawn = drawTree DrawInput
            { size = size
            , lines = ls
            , noMatches = noMatches
            , theme = look.theme
            , selectStyle = look.selection
            , borderStyle = look.border
            , promptStyle = Cell.defaultStyle
                { Cell.fg = Cell.Indexed 0, Cell.bg = Cell.Indexed 3 }
            , promptTop = opts.statusPosition == StatusTop
            , sortShown = True
            , viewName = case (ts.kind, ts.view) of
                (BufferList, _) -> Nothing
                (_, ViewPreview) -> Just "preview"
                (_, ViewInfo) -> Just "info"
            , help = help
            , helpWidth = (helpText ts.kind).width
            } settled
    cells <- case drawn.previewArea of
        Nothing -> pure drawn.cells
        Just (rect, tag) -> do
            let area = Size { rows = fromIntegral (rect.endRow - rect.startRow)
                            , cols = fromIntegral (rect.endCol - rect.startCol) }
            pv <- previewOf look st ts tag area
            pure (overlayGrid drawn.cells rect pv)
    pure (cells, drawn.cursor)

-- | What the preview box shows for an item (tmux's @window_tree_draw@,
-- @window_buffer_draw@, @window_client_draw@).
previewOf
    :: TreeLook -> ServerState -> TreeState -> TreeTag -> Size
    -> IO (V.Vector (V.Vector Cell.Cell))
previewOf look st ts tag size = case (ts.view, tag) of
    (ViewInfo, _) | ts.kind == SessionTree -> do
        env <- itemEnv st tag
        let groups = case tag of
                TagPane _ -> [paneInfoLines, windowInfoLines, sessionInfoLines]
                TagWinlink _ _ -> [windowInfoLines, sessionInfoLines]
                _ -> [sessionInfoLines]
        expanded <- mapM (mapM (expandFormat st env)) groups
        pure (infoGrid look size expanded)
    (_, TagPane pid)
        | ts.hidePane == Just pid -> pure blank
        | otherwise -> do
            mloc <- atomically (findPaneLoc st pid)
            case mloc of
                Nothing -> pure blank
                Just (_, _, _, pane) -> do
                    scr <- Emu.snapshot pane.emulator
                    pure (fitGrid size (screenPreview scr w h))
    (_, TagWinlink sid ix) -> do
        msess <- Map.lookup sid <$> readTVarIO st.sessions
        mwin <- maybe (pure Nothing) (\s -> Map.lookup ix <$> readTVarIO s.windows) msess
        case (msess, mwin) of
            (Just sess, Just win) -> do
                panes <- atomically (windowPanes win)
                active <- readTVarIO win.activeId
                let shown = [ p | p <- panes, Just p.id /= ts.hidePane ]
                cols <- forM shown $ \p -> do
                    scr <- Emu.snapshot p.emulator
                    pix <- paneIndexOf st win p
                    env <- paneFormatEnv st sess ix win pix p
                    opts <- resolveForPaneIO st sess win p
                    labelled scr env opts (p.id == active)
                pure (fitGrid size (columns cols))
            _ -> pure blank
    (_, TagSession sid) -> do
        msess <- Map.lookup sid <$> readTVarIO st.sessions
        case msess of
            Nothing -> pure blank
            Just sess -> do
                ws <- Map.toAscList <$> readTVarIO sess.windows
                cur <- readTVarIO sess.currentIx
                cols <- fmap catMaybes . forM ws $ \(wix, win) -> do
                    mp <- atomically (activePane win)
                    forM mp $ \p -> do
                        scr <- Emu.snapshot p.emulator
                        env <- windowFormatEnv st sess wix win
                        opts <- resolveForWindowIO st sess win
                        labelled scr env opts (wix == cur)
                pure (fitGrid size (columns cols))
    (_, TagBuffer o) -> do
        bufs <- pasteWalk <$> readTVarIO st.buffers
        pure $ case [ b | b <- bufs, b.order == o ] of
            b : _ -> fitGrid size
                [ take w [ Cell.glyphCell c Cell.defaultStyle | c <- T.unpack line ]
                | line <- take h (T.splitOn "\n" (TE.decodeUtf8Lenient b.body)) ]
            [] -> blank
    (_, TagClient cid) -> do
        mc <- Map.lookup cid <$> readTVarIO st.clients
        mpane <- case mc of
            Nothing -> pure Nothing
            Just c -> atomically $ do
                sid <- readTVar c.session
                msess <- Map.lookup sid <$> readTVar st.sessions
                case msess of
                    Nothing -> pure Nothing
                    Just sess -> currentWindow sess >>= maybe (pure Nothing) activePane
        rows <- case mpane of
            Just p | Just p.id /= ts.hidePane -> do
                scr <- Emu.snapshot p.emulator
                pure (screenPreview scr w (h - 2))
            _ -> pure []
        let rule = replicate w (Cell.glyphCell '─' look.border)
        pure (fitGrid size (take (h - 1) (rows <> repeat []) <> [rule]))
  where
    w = fromIntegral size.cols
    h = fromIntegral size.rows
    blank = fitGrid size []
    -- An item's column: its screen, and its label (@tree-mode-preview-format@)
    -- in @tree-mode-preview-style@ over the background of its border
    -- (@tree-mode-border-style@), both in the item's options and context.
    labelled scr env opts isCur = do
        let style name = styleIn look.theme (expandFormat st env) (optionText name opts)
        edge <- style "tree-mode-border-style"
        sty <- style "tree-mode-preview-style"
        label <- expandFormat st env (optionText "tree-mode-preview-format" opts)
        pure Column
            { screen = scr, label = label, labelStyle = sty { Cell.bg = edge.bg }
            , edge = edge, centre = isCur }
    -- tmux's window_tree_draw_session/_window: the screens side by side,
    -- as many as fit 24 columns each, centred on the current one, each
    -- under a boxed label.
    columns entries
        | null entries || w <= 0 || h <= 0 = []
        | otherwise =
            let total = length entries
                visible = if w `div` total < 24 then max 1 (w `div` 24) else total
                current = fromMaybe 0 (lookup True (zip (map (.centre) entries) [0 ..]))
                (start, end)
                    | current < visible = (0, visible)
                    | current >= total - visible = (total - visible, total)
                    | otherwise = let s0 = current - visible `div` 2 in (s0, s0 + visible)
                left = start /= 0
                right = end /= total
                (leftOn, rightOn)
                    | (left && right && w <= 6) || ((left || right) && w <= 3) = (False, False)
                    | otherwise = (left, right)
                avail = w - (if leftOn then 3 else 0) - (if rightOn then 3 else 0)
                each = avail `div` visible
                remaining = avail - visible * each
                shown = take (end - start) (drop start entries)
                colWidth i = if i == length shown - 1 then each + remaining else each - 1
                rowAt y = concat
                    [ arrowsLeft y
                    , concat [ cellsOf i e y | (i, e) <- zip [0 ..] shown ]
                    , arrowsRight y ]
                arrowsLeft y
                    | leftOn = [ if y == h `div` 2 then border '<' else Cell.blankCell
                               , Cell.blankCell, border '│' ]
                    | otherwise = []
                arrowsRight y
                    | rightOn = [ border '│', Cell.blankCell
                                , if y == h `div` 2 then border '>' else Cell.blankCell ]
                    | otherwise = []
                cellsOf i col y =
                    let cw = colWidth i
                        body = withLabel cw col (screenPreview col.screen cw h)
                        row = take cw (fromMaybe [] (listToMaybe (drop y body)) <> repeat Cell.blankCell)
                    in row <> [ Cell.glyphCell '│' col.edge | i /= length shown - 1 ]
            in if each <= 0 then [] else [ take w (rowAt y) | y <- [0 .. h - 1] ]
    border c = Cell.glyphCell c look.border
    -- tmux's window_tree_draw_label: the label boxed in the middle.
    withLabel cw col rows
        | cw < 5 || h < 3 || lw == 0 = rows
        | otherwise =
            let ox = (cw - lw + 1) `div` 2
                oy = (h + 1) `div` 2
                edge c = Cell.glyphCell c col.edge
                clear = Cell.glyphCell ' ' Cell.defaultStyle { Cell.bg = col.edge.bg }
                boxRow y
                    | y == oy - 1 = [ (ox - 2, [edge '┌'] <> replicate (lw + 2) (edge '─') <> [edge '┐']) ]
                    | y == oy = [ (ox - 2, [edge '│', clear] <> labelCells <> [clear, edge '│']) ]
                    | y == oy + 1 = [ (ox - 2, [edge '└'] <> replicate (lw + 2) (edge '─') <> [edge '┘']) ]
                    | otherwise = []
                padded = take h (map (\r -> take cw (r <> repeat Cell.blankCell)) rows
                    <> repeat (replicate cw Cell.blankCell))
            in [ foldl (\r (x, cs) -> take x r <> cs <> drop (x + length cs) r) row (boxRow y)
               | (y, row) <- zip [0 ..] padded ]
      where
        fitted
            | formatWidth col.label > cw - 4 = formatTrimLeft (cw - 4) col.label
            | otherwise = col.label
        labelCells = drawFormat look.theme OwnColours col.labelStyle fitted
        lw = length labelCells

-- | One previewed screen beside its neighbours, and how it is labelled.
data Column = Column
    { screen     :: !Emu.Screen
    , label      :: !Text        -- ^ @tree-mode-preview-format@, expanded
    , labelStyle :: !Cell.Style
    , edge       :: !Cell.Style  -- ^ its @tree-mode-border-style@
    , centre     :: !Bool        -- ^ the columns centre on this one
    }

-- | How a tree mode draws in its pane (tmux's @mode_tree_draw@ styles): its
-- window's tree-mode styles, and its formats expanded in the pane's context.
data TreeLook = TreeLook
    { theme     :: !Theme
    , selection :: !Cell.Style  -- ^ @tree-mode-selection-style@
    , border    :: !Cell.Style  -- ^ @tree-mode-border-style@
    , expand    :: Text -> IO Text
    }

treeLook :: ServerState -> TreeState -> IO TreeLook
treeLook st ts = do
    th <- clientTheme st
    global <- readTVarIO st.options
    mloc <- atomically (findPaneLoc st ts.origin.fromPane)
    (opts, expand) <- case mloc of
        Nothing -> pure (global, pure)
        Just (sess, ix, win, pane) -> do
            pix <- paneIndexOf st win pane
            env <- paneFormatEnv st sess ix win pix pane
            o <- resolveForWindowIO st sess win
            pure (o, expandFormat st env)
    let style name = styleIn th expand (optionText name opts)
    selection <- style "tree-mode-selection-style"
    border <- style "tree-mode-border-style"
    pure TreeLook { theme = th, selection = selection, border = border, expand = expand }

-- | A style option's cells, its formats expanded first (tmux's
-- @style_apply@ with a format tree).
styleIn :: Theme -> (Text -> IO Text) -> Text -> IO Cell.Style
styleIn th expand raw
    | "#{" `T.isInfixOf` raw = styleCell th <$> expand raw
    | otherwise = pure (styleCell th raw)

-- | tmux's @screen_write_preview@: an @nx@ by @ny@ window of a screen,
-- around its cursor when that shows (marked in reverse), else the top left.
screenPreview :: Emu.Screen -> Int -> Int -> [[Cell.Cell]]
screenPreview scr = previewCells scr.cells
    (if scr.cursorVisible then Just scr.cursor else Nothing)

-- | 'screenPreview' over a screen's cells and its cursor, if shown.
previewCells :: V.Vector (V.Vector Cell.Cell) -> Maybe Pos -> Int -> Int -> [[Cell.Cell]]
previewCells cells mcursor nx ny
    | nx <= 0 || ny <= 0 = []
    | otherwise =
        [ [ cellAt (px + x) (py + y) | x <- [0 .. nx - 1] ] | y <- [0 .. ny - 1] ]
  where
    sy = V.length cells
    sx = maybe 0 V.length (cells V.!? 0)
    (px, py) = case mcursor of
        Just c -> (origin c.col nx sx, origin c.row ny sy)
        Nothing -> (0, 0)
    origin c n total =
        let o = if c < n `div` 3 then 0 else c - n `div` 3
        in if o + n > total then (if n > total then 0 else total - n) else o
    cellAt x y =
        let c = fromMaybe Cell.blankCell (cells V.!? y >>= (V.!? x))
        in if mcursor == Just (Pos y x)
            then c { Cell.style = c.style { Cell.reverse = True } }
            else c

-- | Rows of cells as a grid exactly @size@, padded with blanks.
fitGrid :: Size -> [[Cell.Cell]] -> V.Vector (V.Vector Cell.Cell)
fitGrid size rows = V.fromList
    [ V.fromList (take w (r <> repeat Cell.blankCell))
    | r <- take h (rows <> repeat []) ]
  where
    w = fromIntegral size.cols
    h = fromIntegral size.rows

-- | tmux's @window_tree_draw_info@: each group of expanded lines, a rule
-- between groups, and the column rule carried down below them.
infoGrid :: TreeLook -> Size -> [[Text]] -> V.Vector (V.Vector Cell.Cell)
infoGrid look size groups = fitGrid size (take h (body <> tailRows))
  where
    w = fromIntegral size.cols
    h = fromIntegral size.rows
    rule = [ Cell.glyphCell (if x == 14 && w > 14 then '┼' else '─') look.border
           | x <- [0 .. w - 1] ]
    drawn t = take w (drawFormat look.theme OwnColours Cell.defaultStyle t)
    body = concat (zipWith (\i g -> [ rule | i /= (0 :: Int) ] <> map drawn g) [0 ..] groups)
    tailRows
        | w > 14 = repeat (replicate 14 Cell.blankCell <> [Cell.glyphCell '│' look.border])
        | otherwise = []

infoFlag :: Text -> Text -> Text
infoFlag label cond =
    "#{?" <> cond <> ",#[fg=themegreen],#[fg=themelightgrey]}" <> label <> "#[default]"

infoLabel :: Text -> Text
infoLabel l = "#[fg=themelightgrey]" <> l <> " #[#{E:tree-mode-border-style},acs]x#[default] "

paneInfoLines :: [Text]
paneInfoLines =
    [ infoLabel "Pane         " <> "#{pane_index} #[fg=themelightgrey](#{pane_id})#[default]"
    , infoLabel "Title        " <> "#{pane_title}"
    , infoLabel "Command      " <> "#{pane_current_command} #[fg=themelightgrey](PID #{pane_pid})#[default]"
    , infoLabel "Path         " <> "#{pane_current_path}"
    , infoLabel "TTY          " <> "#{pane_tty}"
    , infoLabel "Position     " <> "#{pane_x},#{pane_y} #{pane_width}x#{pane_height}"
    , infoLabel "Mode         " <> "#{?pane_in_mode,#{pane_mode},none}"
    , infoLabel "Flags        " <> T.intercalate " "
        [ infoFlag "active" "pane_active", infoFlag "zoomed" "window_zoomed_flag"
        , infoFlag "marked" "pane_marked", infoFlag "sync" "pane_synchronized"
        , infoFlag "dead" "pane_dead", infoFlag "piped" "pane_pipe" ]
    ]

windowInfoLines :: [Text]
windowInfoLines =
    [ infoLabel "Window       " <> "#{window_index}: #{window_name} #[fg=themelightgrey](#{window_id})#[default]"
    , infoLabel "Size         " <> "#{window_width}x#{window_height}"
    , infoLabel "Panes        " <> "#{window_panes}"
    , infoLabel "Activity Time" <> "#{t:window_activity} #[fg=themelightgrey](#{t/r:window_activity})#[default]"
    , infoLabel "Sessions     " <> "#{s/,/ /:window_linked_sessions_list}"
    , infoLabel "Flags        " <> T.intercalate " "
        [ infoFlag "active" "window_active", infoFlag "last" "window_last_flag"
        , infoFlag "bell" "window_bell_flag", infoFlag "activity" "window_activity_flag"
        , infoFlag "silence" "window_silence_flag", infoFlag "zoomed" "window_zoomed_flag"
        , infoFlag "marked" "window_marked_flag" ]
    ]

sessionInfoLines :: [Text]
sessionInfoLines =
    [ infoLabel "Session      " <> "#{session_name} #[fg=themelightgrey](#{session_id})#[default]"
    , infoLabel "Created Time " <> "#{t:session_created} #[fg=themelightgrey](#{t/r:session_created})#[default]"
    , infoLabel "Activity Time" <> "#{t:session_activity} #[fg=themelightgrey](#{t/r:session_activity})#[default]"
    , infoLabel "Attached Time" <> "#{?#{t:session_last_attached},#{t:session_last_attached} #[fg=themelightgrey](#{t/r:session_last_attached})#[default],never}"
    , infoLabel "Clients      " <> "#{s/,/ /:session_attached_list}"
    , infoLabel "Windows      " <> "#{session_windows}"
    , infoLabel "Path         " <> "#{session_path}"
    , infoLabel "Group        " <> "#{?session_grouped,#{session_group} (#{session_group_size}),none}"
    , infoLabel "Flags        " <> T.intercalate " "
        [ infoFlag "attached" "session_attached", infoFlag "grouped" "session_grouped"
        , infoFlag "marked" "session_marked", infoFlag "bell" "session_bell_flag"
        , infoFlag "activity" "session_activity_flag", infoFlag "silence" "session_silence_flag" ]
    ]

-- Keys ------------------------------------------------------------------

-- | Feed one key to the pane's tree mode (tmux's @window_tree_key@,
-- @window_buffer_key@, @window_client_key@): its prompt, when one is open,
-- takes it; else the generic list keys, then the kind's own. @mclient@ is
-- who pressed it; commands the mode runs go to the client it resolves to.
treeModeKey :: Dispatch -> ServerState -> Maybe Client -> Pane -> Key -> IO ()
treeModeKey d st mclient pane key = do
    mmode <- atomically (topMode pane)
    case mmode of
        Just (TreeMode ts0) -> do
            size <- readTVarIO pane.size
            (items, ls, _, h, ts) <- layoutTree st size ts0
            case ts.modePrompt of
                Just p | PromptSearch _ <- p.use -> searchKey items ls h ts p
                Just p -> promptKey ls ts p
                Nothing -> do
                    let r = treeKey h (sortSequence ts.kind) items ls ts key.name
                    put r.state
                    forM_ r.unhandled $ \k -> kindKey ls r.state k
                    when r.finished (resetPaneMode st pane)
        _ -> pure ()
  where
    put ts = atomically $ modifyTopMode st pane $ \case
        TreeMode _ -> TreeMode ts
        other -> other
    client = modeClient st mclient pane
    runFor template name = do
        mc <- client
        out <- d.runCommandText st mc (templateReplace template name 1)
        forM_ mc $ \c -> forM_ [ e | RErr e <- out ] (showToast st c)
    toast msg = client >>= mapM_ (\c -> showToast st c msg)
    current ls ts = listToMaybe (drop ts.currentIx ls)

    promptKey ls ts p = case p.entry of
        PromptSingleKey
            | T.length key.name == 1 -> answer ls ts p (Just key.name)
            | otherwise -> answer ls ts p Nothing
        PromptLine -> case editPrompt noPromptEnv p.edit key of
            Editing e -> put ts { modePrompt = Just p { edit = e } }
            Answering e _ -> put ts { modePrompt = Just p { edit = e } }
            Submit line -> submitted line
            SubmitPassing line -> submitted line
            Cancel -> answer ls ts p Nothing
          where
            submitted line = answer ls ts p (if T.null line then Nothing else Just line)
    -- See Note [Fuzzy tree search].
    searchKey items ls h ts p
        | key.name `elem` ["Up", "C-p", "Down", "C-n"] =
            let moved = treeKey h (sortSequence ts.kind) items ls ts { modePrompt = Nothing } key.name
            in put moved.state { modePrompt = Just p }
        | otherwise = case editPrompt noPromptEnv p.edit key of
            Editing e -> edited e
            Answering e _ -> edited e
            Submit line -> submitted line
            SubmitPassing line -> submitted line
            Cancel -> answer ls ts p Nothing
      where
        edited e = put ts
            { modePrompt = Just p { edit = e }
            , current = (fuzzyView e.input items).best <|> ts.current }
        submitted line = do
            let chosen = ts
                    { modePrompt = Nothing
                    , search = if T.null line then Nothing else Just line }
            put chosen
            unless (null ls) $ kindKey ls chosen "Enter"
    answer ls ts p mline = do
        let closed = ts { modePrompt = Nothing }
        case p.use of
            PromptFilter -> put closed { filter = mline }
            PromptSearch back -> put closed { current = back }
            PromptKill -> do
                put closed
                when (yes mline) $ forM_ (current ls ts) (killItem . (.item.tag))
            PromptKillTagged -> do
                put closed
                when (yes mline) $ mapM_ (killItem . (.item.tag)) (taggedLines ts ls)
            PromptEachCommand -> do
                put closed
                forM_ mline $ \c -> forEachTagged ls ts $ \t ->
                    itemTarget st t >>= mapM_ (runFor c)
    yes = \case
        Just a -> T.toLower a == "y"
        Nothing -> False
    forEachTagged ls ts act = case taggedLines ts ls of
        [] -> forM_ (current ls ts) (act . (.item.tag))
        tagged -> mapM_ (act . (.item.tag)) tagged

    -- The kind's own keys, after the generic ones passed them on.
    kindKey ls ts k = case ts.kind of
        SessionTree -> treeKindKey ls ts k
        BufferList -> bufferKindKey ls ts k
        ClientList -> clientKindKey ls ts k

    treeKindKey ls ts k = case k of
        "Enter" -> do
            resetPaneMode st pane
            forM_ (current ls ts) $ \l -> do
                name <- itemTarget st l.item.tag
                forM_ name (runFor ts.command)
        "x" -> forM_ (current ls ts) $ \l -> do
            mlabel <- killLabel l.item.tag
            forM_ mlabel $ \lbl -> askKill ts PromptKill lbl
        "X" -> do
            let n = length (taggedLines ts ls)
            unless (n == 0) $ askKill ts PromptKillTagged
                ("Kill " <> tshow n <> " tagged? ")
        ":" -> do
            let n = length (taggedLines ts ls)
                label = if n /= 0 then "(" <> tshow n <> " tagged) " else "(current) "
            put ts { modePrompt = Just (linePromptFor label PromptEachCommand) }
        "H" -> do
            let o = ts.origin
                ex = Map.insert (TagSession o.fromSession) True
                    (Map.insert (TagWinlink o.fromSession o.fromWinlink) True ts.expanded)
                onPane = any ((== TagPane o.fromPane) . (.item.tag)) ls
            put ts { expanded = ex
                   , current = Just (if onPane then TagPane o.fromPane
                                     else TagWinlink o.fromSession o.fromWinlink) }
        "m" -> forM_ (current ls ts) $ \l -> do
            mp <- itemPane st l.item.tag
            forM_ mp $ \pid -> atomically $ do
                writeTVar st.markedPane (Just pid)
                bumpDirty st
        "M" -> atomically $ do
            writeTVar st.markedPane Nothing
            bumpDirty st
        "i" -> put ts { view = case ts.view of
            ViewPreview -> ViewInfo
            ViewInfo -> ViewPreview }
        _ | k `elem` ["K", "S-Up"] -> swapWindow ls ts Backward
          | k `elem` ["J", "S-Down"] -> swapWindow ls ts Forward
        _ -> pure ()
    askKill ts use label = case ts.confirm of
        KillWithoutAsking -> do
            (_, ls, _, _, ts') <- layoutTree' ts
            case use of
                PromptKillTagged -> mapM_ (killItem . (.item.tag)) (taggedLines ts' ls)
                _ -> forM_ (current ls ts') (killItem . (.item.tag))
        AskBeforeKill -> put ts { modePrompt = Just TreePrompt
            { edit = (blankPrompt label), use = use, entry = PromptSingleKey } }
    layoutTree' ts = do
        size <- readTVarIO pane.size
        layoutTree st size ts
    killLabel = \case
        TagSession sid -> fmap (\n -> "Kill session " <> n <> "? ")
            <$> sessionName st sid
        TagWinlink _ ix -> pure (Just ("Kill window " <> tshow ix <> "? "))
        TagPane pid -> do
            mloc <- atomically (findPaneLoc st pid)
            case mloc of
                Nothing -> pure Nothing
                Just (_, _, win, p) -> do
                    pix <- paneIndexOf st win p
                    pure (Just ("Kill pane " <> tshow pix <> "? "))
        _ -> pure Nothing
    killItem t = do
        mname <- itemTarget st t
        mc <- client
        forM_ mname $ \target -> do
            let cmd = case t of
                    TagSession _ -> Just ["kill-session", "-t", target]
                    TagWinlink _ _ -> Just ["kill-window", "-t", target]
                    TagPane _ -> Just ["kill-pane", "-t", target]
                    _ -> Nothing
            forM_ cmd $ \argv -> d.runArgv st mc argv
    swapWindow ls ts dir = forM_ (current ls ts) $ \l -> case l.item.tag of
        TagWinlink sid ix | ts.sort.order == SortIndex -> do
            let siblings = [ (i', l') | (i', l') <- zip [0 :: Int ..] ls
                           , l'.depth == l.depth, l'.ancestors == l.ancestors ]
                here = [ i' | (i', l') <- siblings, l'.item.tag == l.item.tag ]
                next = case here of
                    [i] -> listToMaybe $ case dir of
                        Forward -> [ l' | (i', l') <- siblings, i' > i ]
                        Backward -> [ l' | (i', l') <- reverse siblings, i' < i ]
                    _ -> Nothing
            forM_ next $ \other -> case other.item.tag of
                TagWinlink _ oix -> do
                    mname <- sessionName st sid
                    forM_ mname $ \sname -> do
                        mc <- client
                        _ <- d.runArgv st mc
                            [ "swap-window", "-d", "-s", "=" <> sname <> ":" <> tshow ix
                            , "-t", "=" <> sname <> ":" <> tshow oix ]
                        put ts { current = Just (TagWinlink sid oix) }
                _ -> pure ()
        _ -> pure ()

    bufferKindKey ls ts k = do
        case k of
            "e" -> toast "choose-buffer: e (edit buffer) is not supported"
            "d" -> forM_ (current ls ts) $ \l -> do
                put (stepOff ls ts)
                deleteBuffer l.item
            "D" -> mapM_ (deleteBuffer . (.item)) (taggedLines ts ls)
            "P" -> do
                resetPaneMode st pane
                mapM_ (runFor ts.command . (.item.name)) (taggedLines ts ls)
            _ | k `elem` ["p", "Enter"] -> do
                resetPaneMode st pane
                forM_ (current ls ts) (runFor ts.command . (.item.name))
            _ -> pure ()
        -- No buffers left: nothing to choose from.
        none <- null . pasteWalk <$> readTVarIO st.buffers
        stillHere <- isTreeTop
        when (none && stillHere) (resetPaneMode st pane)
    deleteBuffer i = do
        mc <- client
        _ <- d.runArgv st mc ["delete-buffer", "-b", i.name]
        pure ()

    clientKindKey ls ts k = do
        case k of
            "Enter" -> do
                resetPaneMode st pane
                forM_ (current ls ts) (runFor ts.command . (.item.name))
            "d" -> forM_ (current ls ts) $ \l -> do
                put (stepOff ls ts)
                detachClient l.item
            "D" -> mapM_ (detachClient . (.item)) (taggedLines ts ls)
            "i" -> put ts { view = case ts.view of
                ViewPreview -> ViewInfo
                ViewInfo -> ViewPreview }
            _ | k `elem` ["x", "X", "z", "Z"] ->
                toast ("choose-client: " <> k <> " is not supported")
            _ -> pure ()
        none <- null <$> attachedClients st
        stillHere <- isTreeTop
        when (none && stillHere) (resetPaneMode st pane)
    detachClient i = do
        mc <- client
        _ <- d.runArgv st mc ["detach-client", "-t", i.name]
        pure ()
    -- Deleting the selected item moves the selection off it first: down,
    -- or up from the bottom.
    stepOff ls ts
        | ts.currentIx < length ls - 1 = sel (ts.currentIx + 1)
        | ts.currentIx > 0 = sel (ts.currentIx - 1)
        | otherwise = ts
      where
        sel i = ts { currentIx = i, current = Just (ls !! i).item.tag }
    isTreeTop = atomically (topMode pane) >>= \case
        Just (TreeMode _) -> pure True
        _ -> pure False

blankPrompt :: Text -> PromptState
blankPrompt label = promptFor label "" ""

linePromptFor :: Text -> TreePromptUse -> TreePrompt
linePromptFor label use = TreePrompt
    { edit = blankPrompt label, use = use, entry = PromptLine }

sessionName :: ServerState -> SessionId -> IO (Maybe Text)
sessionName st sid = do
    msess <- Map.lookup sid <$> readTVarIO st.sessions
    traverse (readTVarIO . (.name)) msess

-- | The target an item names in a command (tmux's
-- @window_tree_get_target@): @=session:@, @=session:index.@ or
-- @=session:index.%pane@; a buffer or client by its name.
itemTarget :: ServerState -> TreeTag -> IO (Maybe Text)
itemTarget st = \case
    TagSession sid -> fmap (\n -> "=" <> n <> ":") <$> sessionName st sid
    TagWinlink sid ix -> fmap (\n -> "=" <> n <> ":" <> tshow ix <> ".") <$> sessionName st sid
    TagPane pid -> do
        mloc <- atomically (findPaneLoc st pid)
        case mloc of
            Nothing -> pure Nothing
            Just (sess, ix, _, _) -> do
                n <- readTVarIO sess.name
                pure (Just ("=" <> n <> ":" <> tshow ix <> ".%" <> tshow (rawPane pid)))
    TagBuffer o -> do
        bufs <- pasteWalk <$> readTVarIO st.buffers
        pure (listToMaybe [ b.name | b <- bufs, b.order == o ])
    TagClient cid -> do
        mc <- Map.lookup cid <$> readTVarIO st.clients
        pure (clientName <$> mc)

-- | The pane an item stands for: a session's or window's active pane.
itemPane :: ServerState -> TreeTag -> IO (Maybe PaneId)
itemPane st = \case
    TagPane pid -> pure (Just pid)
    TagWinlink sid ix -> atomically $ do
        msess <- Map.lookup sid <$> readTVar st.sessions
        case msess of
            Nothing -> pure Nothing
            Just sess -> do
                mwin <- Map.lookup ix <$> readTVar sess.windows
                traverse (readTVar . (.activeId)) mwin
    TagSession sid -> atomically $ do
        msess <- Map.lookup sid <$> readTVar st.sessions
        case msess of
            Nothing -> pure Nothing
            Just sess -> do
                mwin <- currentWindow sess
                traverse (readTVar . (.activeId)) mwin
    _ -> pure Nothing

-- | The client a mode's commands act for (tmux's target client): the one
-- that pressed the key when it is attached, else the most recently active
-- client attached to the pane's session, else any attached client.
modeClient :: ServerState -> Maybe Client -> Pane -> IO (Maybe Client)
modeClient st mclient pane = case mclient of
    Just c | c.role == Attached -> pure (Just c)
    _ -> do
        cs <- attachedClients st
        msid <- atomically (fmap (\(s, _, _, _) -> s.id) <$> findPaneLoc st pane.id)
        withAct <- forM cs $ \c -> atomically $
            (,,) c <$> readTVar c.session <*> readTVar c.lastActive
        let mine = [ x | x@(_, sid, _) <- withAct, Just sid == msid ]
            best xs = listToMaybe
                [ c | (c, _, _) <- sortBy (comparing (\(_, _, a) -> Down a)) xs ]
        pure (best mine <|> best withAct)
