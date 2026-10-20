-- | customize-mode's @options-mode@ (tmux's @window-customize.c@) on the
-- mode tree: the options, hooks, environments and key bindings in reach of
-- the pane it was entered on, listed from the option registry and the live
-- stores, with a preview describing the selected item and prompts that set,
-- unset, reset and edit what it lists. "Hat.Server.TreeMode" drives it as
-- one of its list kinds.
module Hat.Server.Customize
    ( CustomizeCtx (..)
    , customizeFormat
    , customizeHelp
    , customizeItems
    , customizePreview
    , customizeKey
    , customizeAnswer
      -- * The preview's text, for tests
    , Pen
    , penGrid
    , blankPen
    , penText
    , penValue
    , penSkipLine
    ) where

import Control.Concurrent.STM
import Control.Monad (foldM, forM, forM_, unless, void, when)
import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Vector qualified as V

import Hat.Geometry (Size (..))
import Hat.Model
import Hat.Model.OptionTable
import Hat.Model.Options (deltaEntries)
import Hat.Server.Command.Buffer (formatTrue)
import Hat.Server.Command.ListKeys (inKeyOrder, printCommand)
import Hat.Server.Command.Option
    ( Scope (..), assignOption, hookEntryValue, hookScopeOf, ownValue, removeOption )
import Hat.Server.Command.Types (Dispatch (..))
import Hat.Server.Editor (spawnEditor)
import Hat.Server.Environ
import Hat.Server.Format (FormatEnv)
import Hat.Server.FormatDraw (styleCell)
import Hat.Server.FormatEnv (expandFormat, paneFormatEnv)
import Hat.Server.Script (resolveCommandText)
import Hat.Server.HookMonitor (monitorTargetText, monitorsAt)
import Hat.Server.HookTypes (HookEntry (..), Monitor (..))
import Hat.Server.Hooks (hookEntriesAt, isRegisteredEvent)
import Hat.Server.Keymap (defaultKeymap)
import Hat.Server.Keys (Binding (..), Key (..), Repeat (..), parseKeyName)
import Hat.Server.Locate (locatePane, paneIndexOf)
import Hat.Server.ModeTree (Item (..), Line (..), NameAlign (..), PreviewFrom (..), Tagging (..))
import Hat.Server.Prompt (promptFor)
import Hat.Server.Theme (Theme)
import Hat.Term.Cell qualified as Cell
import Hat.TextWidth (textWidth)

-- | What the mode's key and prompt handling needs of the pane it runs in.
data CustomizeCtx = CustomizeCtx
    { dispatch :: Dispatch
    , client   :: IO (Maybe Client)  -- ^ who the mode acts for
    , pane     :: Pane               -- ^ the pane showing the mode
    , put      :: TreeState -> IO ()  -- ^ replace the mode's state
    , message  :: Text -> IO ()      -- ^ tell the client, as a status message
    }

-- | tmux's @WINDOW_CUSTOMIZE_DEFAULT_FORMAT@.
customizeFormat :: Text
customizeFormat = T.concat
    [ "#{?is_option,"
    ,     "#{?option_is_global,,#[reverse](#{option_scope})#[default] }"
    ,     "#[fg=themelightgrey]#[ignore]#{option_value}"
    ,     "#{?option_unit, #{option_unit},}"
    , ","
    ,     "#{?is_environment,"
    ,         "#[fg=themelightgrey]#[ignore]#{environment_value}"
    ,     ","
    ,         "#{key}"
    ,     "}"
    , "}" ]

-- | tmux's @window_customize_help_lines@.
customizeHelp :: [Text]
customizeHelp = map line
    [ ("   Enter, s", "Set %1 value")
    , ("          S", "Set global %1 value")
    , ("          w", "Set window %1 value")
    , ("          d", "Set to default value")
    , ("          D", "Set tagged %1s to default value")
    , ("          u", "Unset an %1")
    , ("          U", "Unset tagged %1s")
    , ("          a", "Change array key")
    , ("          e", "Open %1 value in editor")
    , ("          f", "Enter a filter")
    , ("          C", "Toggle only changed items")
    , ("          v", "Toggle information") ]
  where
    line (k, what) = "#[fg=themelightgrey]" <> k
        <> " #[#{E:tree-mode-border-style},acs]x#[default] " <> what

-- Where the mode is -----------------------------------------------------

-- | The session, window and pane the mode acts in (tmux's @data->fs@, or
-- when that went, where the mode's pane is now), with their format
-- environment.
data Here = Here
    { session     :: Session
    , sessionName :: Text
    , winlink     :: Int
    , window      :: Window
    , pane        :: Pane
    , paneIndex   :: Int
    , env         :: FormatEnv
    }

hereOf :: ServerState -> TreeState -> IO (Maybe Here)
hereOf st ts = do
    mloc <- atomically $ do
        sessions <- readTVar st.sessions
        let o = ts.origin
        atOrigin <- case Map.lookup o.fromSession sessions of
            Nothing -> pure Nothing
            Just s -> do
                mw <- Map.lookup o.fromWinlink <$> readTVar s.windows
                case mw of
                    Nothing -> pure Nothing
                    Just w -> fmap (\p -> (s, o.fromWinlink, w, p)) . Map.lookup o.fromPane
                        <$> readTVar w.panes
        case atOrigin of
            Just loc -> pure (Just loc)
            Nothing -> locatePane st o.fromPane >>= \case
                Just (sid, w) | Just s <- Map.lookup sid sessions -> do
                    ws <- Map.toList <$> readTVar s.windows
                    p <- Map.lookup o.fromPane <$> readTVar w.panes
                    pure $ (\ix pn -> (s, ix, w, pn))
                        <$> listToMaybe [ ix | (ix, w') <- ws, w'.id == w.id ] <*> p
                _ -> pure Nothing
    forM mloc $ \(s, ix, w, p) -> do
        pix <- paneIndexOf st w p
        nm <- readTVarIO s.name
        e <- paneFormatEnv st s ix w pix p
        pure Here { session = s, sessionName = nm, winlink = ix, window = w
                  , pane = p, paneIndex = pix, env = e }

-- Tables ----------------------------------------------------------------

-- | An option table in reach of the mode.
data Table = Table
    { ref   :: OptionTableRef
    , scope :: Scope
    }

tableAt :: Here -> OptionTableRef -> Maybe Table
tableAt h ref = Table ref <$> case ref of
    ServerTableRef -> Just ServerTable
    GlobalSessionTableRef -> Just GlobalSessionTable
    GlobalWindowTableRef -> Just GlobalWindowTable
    SessionTableRef sid | sid == h.session.id -> Just (SessionTable h.session)
    WindowTableRef wid | wid == h.window.id -> Just (WindowTable h.window)
    PaneTableRef pid | pid == h.pane.id -> Just (PaneTable h.pane)
    _ -> Nothing

isGlobal :: OptionTableRef -> Bool
isGlobal = \case
    ServerTableRef -> True
    GlobalSessionTableRef -> True
    GlobalWindowTableRef -> True
    _ -> False

-- | A table's parent tables, nearest first, which reset walks
-- (@options_get_parent@).
parentRefs :: Here -> OptionTableRef -> [OptionTableRef]
parentRefs h = \case
    SessionTableRef _ -> [GlobalSessionTableRef]
    WindowTableRef _ -> [GlobalWindowTableRef]
    PaneTableRef _ -> [WindowTableRef h.window.id, GlobalWindowTableRef]
    _ -> []

-- | tmux's @window_customize_scope_text@.
scopeText :: Here -> OptionTableRef -> Text
scopeText h = \case
    PaneTableRef _ -> "pane " <> tshow h.paneIndex
    SessionTableRef _ -> "session " <> h.sessionName
    WindowTableRef _ -> "window " <> tshow h.winlink
    _ -> ""

-- | A group's tables, broadest first.
groupTables :: Here -> CustomizeGroup -> [OptionTableRef]
groupTables h = \case
    ServerOptions -> [ServerTableRef]
    SessionOptions -> sessionTables
    SessionHooks -> sessionTables
    WindowPaneOptions -> windowTables
    WindowPaneHooks -> windowTables
    _ -> []
  where
    sessionTables = [GlobalSessionTableRef, SessionTableRef h.session.id]
    windowTables =
        [GlobalWindowTableRef, WindowTableRef h.window.id, PaneTableRef h.pane.id]

groupFamily :: CustomizeGroup -> OptionFamily
groupFamily g = if g `elem` [SessionHooks, WindowPaneHooks] then HookFamily else PlainFamily

familyRole :: OptionFamily -> OptionRole
familyRole = \case
    PlainFamily -> PlainOption
    HookFamily -> HookOption

-- | The registered rows a group lists, by name.
groupRows :: CustomizeGroup -> [OptionDef]
groupRows g = sortOn (.name)
    [ d | d <- optionTable, isLive d, d.role == familyRole (groupFamily g)
    , d.scope `elem` scopes ]
  where
    scopes = case g of
        ServerOptions -> [ServerScope]
        SessionOptions -> [SessionScope]
        SessionHooks -> [SessionScope]
        _ -> [WindowScope, WindowPaneScope]

groupTitle :: CustomizeGroup -> Text
groupTitle = \case
    ServerOptions -> "Server Options"
    SessionOptions -> "Session Options"
    WindowPaneOptions -> "Window & Pane Options"
    SessionHooks -> "Session Hooks"
    WindowPaneHooks -> "Window & Pane Hooks"
    GlobalEnvironment -> "Global Environment"
    SessionEnvironment -> "Session Environment"

-- Values ----------------------------------------------------------------

-- | An option or hook's own value in a table: a global table holds every
-- registered option (its default when unset), the global hook table every
-- hook.
valueIn :: ServerState -> OptionFamily -> Scope -> Text -> IO (Maybe OptionValue)
valueIn st fam sc n = case fam of
    PlainFamily -> atomically (ownValue st sc n)
    HookFamily -> case hookScopeOf sc of
        Nothing -> pure Nothing
        Just hs -> do
            entries <- hookEntriesAt st hs
            mons <- map fst <$> monitorsAt st hs
            pure $ case hookEntryValue (lookupOptionDef n) hs (Map.lookup n entries) of
                Just v -> Just v
                Nothing | "@" `T.isPrefixOf` n, n `elem` mons -> Just (OVScalar "")
                _ -> Nothing

-- | The narrowest of the tables (broadest first) holding a value for the
-- option (tmux's @options_get@ from the narrowest).
locateValue
    :: ServerState -> Here -> OptionFamily -> [OptionTableRef] -> Text
    -> IO (Maybe (Table, OptionValue))
locateValue st h fam refs n = go (reverse refs)
  where
    go [] = pure Nothing
    go (r : rest) = case tableAt h r of
        Nothing -> go rest
        Just t -> valueIn st fam t.scope n >>= \case
            Just v -> pure (Just (t, v))
            Nothing -> go rest

-- | The user names a table holds, in name order.
userNamesIn :: ServerState -> OptionFamily -> Scope -> IO [Text]
userNamesIn st fam sc = case fam of
    PlainFamily -> do
        own <- readOwnIO (ownOf sc)
        pure [ n | (n, _) <- deltaEntries own, "@" `T.isPrefixOf` n ]
    HookFamily -> case hookScopeOf sc of
        Nothing -> pure []
        Just hs -> do
            entries <- Map.keys <$> hookEntriesAt st hs
            mons <- map fst <$> monitorsAt st hs
            pure (Set.toAscList (Set.fromList
                (filter ("@" `T.isPrefixOf`) (entries <> mons))))
  where
    ownOf = \case
        ServerTable -> st.serverOptions
        GlobalSessionTable -> st.globalSessionOptions
        GlobalWindowTable -> st.globalWindowOptions
        SessionTable s -> s.options
        WindowTable w -> w.options
        PaneTable p -> p.options

-- | A value as one string, or one array item of it (tmux's
-- @options_to_string@).
valueString :: Maybe ArrayKey -> OptionValue -> Text
valueString mkey v = case (mkey, v) of
    (Just k, OVArray m) -> Map.findWithDefault "" k m
    _ -> valueText v

-- | tmux's @window_customize_option_is_changed@.
isChanged :: Maybe OptionDef -> Bool -> Maybe ArrayKey -> OptionValue -> Bool
isChanged mdef monitored mkey v = case mdef of
    Nothing -> True
    Just _ | monitored -> True
    Just d -> case (mkey, v, d.defaultValue) of
        (Just k, OVArray m, OVArray dm) -> Map.lookup k m /= Map.lookup k dm
        _ -> valueText v /= valueText d.defaultValue

-- | Whether a hook name is a monitor at a table.
monitorAt :: ServerState -> Scope -> Text -> IO (Maybe Monitor)
monitorAt st sc n = case hookScopeOf sc of
    Nothing -> pure Nothing
    Just hs -> lookup n <$> monitorsAt st hs

-- Building --------------------------------------------------------------

-- | The mode's items under a filter (tmux's @window_customize_build@).
customizeItems :: ServerState -> TreeState -> Maybe Text -> IO [Item]
customizeItems st ts mfilter = hereOf st ts >>= \case
    Nothing -> pure []
    Just h -> do
        opts <- mapM (optionGroup st ts h mfilter)
            [ServerOptions, SessionOptions, WindowPaneOptions, SessionHooks, WindowPaneHooks]
        envs <- if ts.itemsShown == ChangedItems then pure [] else do
            genv <- readTVarIO st.globalEnviron
            senv <- readTVarIO h.session.environ
            sequence
                [ envGroup st ts h mfilter GlobalEnvironment genv
                , envGroup st ts h mfilter SessionEnvironment senv ]
        km <- readTVarIO st.keymap
        keys <- mapM (keyGroup st ts h mfilter) [ (t, b) | (t, b) <- Map.toAscList km, not (Map.null b) ]
        pure (catMaybes opts <> envs <> catMaybes keys)

heading :: TreeTag -> Text -> [Item] -> Item
heading tag nm cs = Item
    { tag = tag, name = nm, text = Nothing, children = cs, startExpanded = False
    , align = AlignNone, searchText = [nm], fuzzyName = Just nm
    , tagging = Untaggable, previewFrom = OwnPreview }

leaf :: TreeTag -> Text -> Maybe Text -> Item
leaf tag nm txt = Item
    { tag = tag, name = nm, text = txt, children = [], startExpanded = False
    , align = AlignNone, searchText = [nm], fuzzyName = Just nm
    , tagging = Taggable, previewFrom = OwnPreview }

keeps :: ServerState -> Maybe Text -> FormatEnv -> IO Bool
keeps st mfilter env = maybe (pure True) (fmap formatTrue . expandFormat st env) mfilter

flag :: Bool -> Text
flag b = if b then "1" else "0"

optionGroup
    :: ServerState -> TreeState -> Here -> Maybe Text -> CustomizeGroup
    -> IO (Maybe Item)
optionGroup st ts h mfilter g = do
    let refs = groupTables h g
        fam = groupFamily g
        -- tmux keeps global user hooks with the global session options.
        userRefs = case g of
            WindowPaneHooks -> drop 1 refs
            _ -> refs
    users <- fmap (nub . concat) . forM userRefs $ \r ->
        maybe (pure []) (userNamesIn st fam . (.scope)) (tableAt h r)
    built <- forM (users <> map (.name) (groupRows g)) $ \n ->
        locateValue st h fam refs n >>= \case
            Nothing -> pure Nothing
            Just (t, v) -> optionItem st ts h mfilter g t n v
    let kids = catMaybes built
    pure $ if ts.itemsShown == ChangedItems && null kids
        then Nothing
        else Just (heading (TagGroup g) (groupTitle g) kids)

optionItem
    :: ServerState -> TreeState -> Here -> Maybe Text -> CustomizeGroup -> Table
    -> Text -> OptionValue -> IO (Maybe Item)
optionItem st ts h mfilter g t n v = do
    let mdef = lookupOptionDef n
        isArray = maybe False (isJust . (.array)) mdef
        global = isGlobal t.ref
        isHook = maybe False ((== HookOption) . (.role)) mdef
    mmon <- monitorAt st t.scope n
    let changed = isChanged mdef (isJust mmon) Nothing v
    if (ts.globalsShown == GlobalsHidden && global)
        || (ts.itemsShown == ChangedItems && not changed)
      then pure Nothing
      else do
        let vars = Map.fromList $
                [ ("is_option", "1"), ("is_key", "0"), ("is_environment", "0")
                , ("option_name", n), ("option_is_global", flag global)
                , ("option_is_array", flag isArray), ("option_is_hook", flag isHook)
                , ("option_is_monitor", flag (isJust mmon))
                , ("option_scope", scopeText h t.ref)
                , ("option_unit", fromMaybe "" (mdef >>= (.unit)))
                , ("option_monitor", maybe "" (monitorSpec n) mmon) ]
                <> [ ("option_value", valueText v) | not isArray ]
            env = Map.union vars h.env
        ok <- keeps st mfilter env
        if not ok then pure Nothing else do
            text <- if isArray then pure Nothing
                else Just <$> expandFormat st env ts.itemFormat
            items <- case v of
                OVArray m | isArray -> fmap catMaybes . forM (Map.toAscList m) $ \(k, iv) ->
                    if ts.itemsShown == ChangedItems && not (isChanged mdef False (Just k) v)
                        then pure Nothing
                        else do
                            let nm = n <> "[" <> arrayKeyText k <> "]"
                                ienv = Map.insert "option_name" nm
                                    (Map.insert "option_value" iv env)
                            itext <- expandFormat st ienv ts.itemFormat
                            pure (Just (leaf (TagOption g n (Just k)) nm (Just itext))
                                { startExpanded = True })
                _ -> pure []
            pure (Just (leaf (TagOption g n Nothing) n text) { children = items })

monitorSpec :: Text -> Monitor -> Text
monitorSpec n m = n <> ":" <> monitorTargetText m.target <> ":" <> m.format

envGroup
    :: ServerState -> TreeState -> Here -> Maybe Text -> CustomizeGroup -> Environ
    -> IO Item
envGroup st ts h mfilter g environ = do
    let global = g == GlobalEnvironment
        scopeVars = Map.fromList
            [ ("is_option", "0"), ("is_key", "0"), ("is_environment", "1")
            , ("environment_is_global", flag global)
            , ("environment_scope", if global then "" else "session " <> h.sessionName) ]
    kids <- fmap catMaybes . forM (Map.toAscList environ) $ \(n, e) -> do
        let env = Map.union (Map.fromList
                [ ("environment_name", n)
                , ("environment_hidden", flag (e.visibility == EnvHidden))
                , ("environment_removed", flag (isNothing e.value))
                , ("environment_value", fromMaybe "" e.value) ])
                (Map.union scopeVars h.env)
        ok <- keeps st mfilter env
        if not ok then pure Nothing else case e.value of
            Nothing -> pure (Just (leaf (TagEnvironment g n) ("-" <> n) Nothing))
            Just _ -> do
                text <- expandFormat st env ts.itemFormat
                pure (Just (leaf (TagEnvironment g n) n (Just text)))
    pure (heading (TagGroup g) (groupTitle g) kids)

keyGroup
    :: ServerState -> TreeState -> Here -> Maybe Text -> (Text, Map Text Binding)
    -> IO (Maybe Item)
keyGroup st ts h mfilter (table, binds) = do
    let base = Map.union (Map.fromList
            [("is_option", "0"), ("is_key", "1"), ("is_environment", "0")]) h.env
    kids <- fmap catMaybes . forM (inKeyOrder binds) $ \(k, b) ->
        if ts.itemsShown == ChangedItems && not (keyChanged table k b)
          then pure Nothing
          else do
            let env = Map.insert "key" k (maybe id (Map.insert "key_note") b.note base)
            ok <- keeps st mfilter env
            if not ok then pure Nothing else do
                nm <- expandFormat st env ts.itemFormat
                let part p label value = (leaf (TagKeyPart table k p) label
                        (Just ("#[fg=themelightgrey]#[ignore]" <> value)))
                        { tagging = Untaggable, previewFrom = ParentPreview
                        , startExpanded = True }
                pure $ Just (leaf (TagKey table k) nm Nothing)
                    { searchText = [k], fuzzyName = Just k
                    , children =
                        [ part KeyCommand "Command" (printList b.commands)
                        , part KeyNote "Note" (fromMaybe "" b.note)
                        , part KeyRepeat "Repeat" (if b.repeats == Repeats then "on" else "off") ] }
    pure $ if ts.itemsShown == ChangedItems && null kids
        then Nothing
        else Just (heading (TagKeyTable table) ("Key Table - " <> table) kids)

-- | A binding's commands as tmux's @cmd_list_print@ prints them unescaped.
printList :: [[Text]] -> Text
printList = T.intercalate " ; " . map printCommand

-- | tmux's @window_customize_key_is_changed@.
keyChanged :: Text -> Text -> Binding -> Bool
keyChanged table k b = case Map.lookup table defaultKeymap >>= Map.lookup k of
    Nothing -> True
    Just d -> d.repeats /= b.repeats || d.note /= b.note
        || printList d.commands /= printList b.commands

defaultBinding :: Text -> Text -> Maybe Binding
defaultBinding table k = Map.lookup table defaultKeymap >>= Map.lookup k

-- What an item is ------------------------------------------------------

-- | The option, environment variable or binding an item's tag names, as it
-- stands now (tmux's item data, checked as @window_customize_check_item@
-- checks it).
data Target
    = OptionItem !OptionTarget !Table !OptionValue
    | EnvItem !EnvTarget !(TVar Environ) !EnvEntry
    | KeyItem !KeyTarget !Binding !(Maybe KeyPart)

targetOf :: ServerState -> Here -> TreeTag -> IO (Maybe Target)
targetOf st h = \case
    TagOption g n mkey -> do
        let fam = groupFamily g
        located <- locateValue st h fam (groupTables h g) n
        pure $ (\(t, v) -> OptionItem (OptionTarget t.ref fam n mkey) t v) <$> located
    TagEnvironment g n -> do
        let sref = case g of
                GlobalEnvironment -> GlobalEnvRef
                _ -> SessionEnvRef h.session.id
        case envVar st h sref of
            Nothing -> pure Nothing
            Just var -> do
                e <- environFind n <$> readTVarIO var
                pure $ (\entry -> EnvItem (EnvTarget sref n entry.visibility) var entry) <$> e
    TagKey table k -> keyTarget table k Nothing
    TagKeyPart table k p -> keyTarget table k (Just p)
    _ -> pure Nothing
  where
    keyTarget table k mp = do
        mb <- (\km -> Map.lookup table km >>= Map.lookup k) <$> readTVarIO st.keymap
        pure ((\b -> KeyItem (KeyTarget table k) b mp) <$> mb)

-- | An environment's store, when the mode can still reach it.
envVar :: ServerState -> Here -> EnvScopeRef -> Maybe (TVar Environ)
envVar st h = \case
    GlobalEnvRef -> Just st.globalEnviron
    SessionEnvRef sid | sid == h.session.id -> Just h.session.environ
    _ -> Nothing

-- Preview ---------------------------------------------------------------

-- | A writer over the preview box (tmux's @screen_write_text@ and the
-- customize-mode helpers over it): a cursor, and the cells written so far.
data Pen = Pen
    { width  :: !Int
    , height :: !Int
    , x      :: !Int
    , y      :: !Int
    , cells  :: !(Map (Int, Int) Cell.Cell)
    }

blankPen :: Int -> Int -> Pen
blankPen w h = Pen { width = w, height = h, x = 0, y = 0, cells = Map.empty }

-- | The box's rows of cells.
penGrid :: Pen -> [[Cell.Cell]]
penGrid p =
    [ [ Map.findWithDefault Cell.blankCell (r, c) p.cells | c <- [0 .. p.width - 1] ]
    | r <- [0 .. p.height - 1] ]

-- | tmux's @screen_write_text@ from the left edge: the text wrapped at
-- spaces down to the bottom row; 'Left' when it ran out of room (what it
-- could write kept). With @more@, the cursor stays after the text for what
-- follows on the line.
penText :: Bool -> Cell.Style -> Text -> Pen -> Either Pen Pen
penText more style t p0
    | p0.y >= p0.height = Left p0
    | otherwise = go (T.unpack t) p0 (p0.width - p0.x)
  where
    go s p left =
        let (end, next) = breakAt s left
            p' = foldl put p end
        in if p'.y == p'.height - 1 || null next
            then finish next p'
            else go next p' { x = 0, y = p'.y + 1 } p'.width
    put p c = p { x = p.x + w, cells = Map.insert (p.y, p.x) (Cell.glyphCell c style) p.cells }
      where w = textWidth (T.singleton c)
    finish rest p
        | (p.y == p.height - 1 && (not more || p.x == p.width)) || not (null rest) = Left p
        | not more || p.x == p.width = Right p { x = 0, y = p.y + 1 }
        | otherwise = Right p
    -- What fits on the line, and what is left after it (a space or newline
    -- ending the line consumed; a word that would split carried over whole
    -- when the line has a space to break at).
    breakAt s left =
        let fit = fitting s left 0
            (taken, rest) = splitAt fit s
        in case rest of
            [] -> (taken, [])
            ('\n' : r) -> (taken, r)
            (' ' : r) -> (taken, r)
            _ -> case lastSpace taken of
                Just i | i > 0 -> (take i taken, drop (i + 1) s)
                _ -> (taken, rest)
    fitting [] _ n = n
    fitting (c : cs) left n
        | c == '\n' = n
        | textWidth (T.singleton c) > left = n
        | otherwise = fitting cs (left - textWidth (T.singleton c)) (n + 1)
    lastSpace s = listToMaybe [ i | (i, c) <- reverse (zip [0 :: Int ..] s), c == ' ' ]

-- | tmux's @window_customize_write_value@: a label, then its value in light
-- grey.
penValue :: Theme -> Text -> Text -> Pen -> Either Pen Pen
penValue th label value p0
    | p0.height == 0 = Left p0
    | otherwise = do
        p1 <- penText True Cell.defaultStyle label p0
        if p1.y >= p1.height then Left p1
            else penText False (styleCell th "fg=themelightgrey") value p1

-- | Skip a line; 'Left' when that reaches the bottom row.
penSkipLine :: Pen -> Either Pen Pen
penSkipLine p
    | p'.y >= p'.height - 1 = Left p'
    | otherwise = Right p'
  where
    p' = p { x = 0, y = p.y + 1 }

plain :: Text -> Pen -> Either Pen Pen
plain = penText False Cell.defaultStyle

-- | The selected item's description in the preview box (tmux's
-- @window_customize_draw@).
customizePreview
    :: ServerState -> Theme -> TreeState -> TreeTag -> Size
    -> IO (V.Vector (V.Vector Cell.Cell))
customizePreview st th ts tag size = do
    let w = fromIntegral size.cols
        hgt = fromIntegral size.rows
        pen0 = blankPen w hgt
    mh <- hereOf st ts
    drawn <- case mh of
        Nothing -> pure pen0
        Just h -> targetOf st h tag >>= \case
            Nothing -> pure pen0
            Just target -> either id id <$> previewOf st th h target pen0
    pure (V.fromList (map V.fromList (penGrid drawn)))

previewOf :: ServerState -> Theme -> Here -> Target -> Pen -> IO (Either Pen Pen)
previewOf st th h target p0 = case target of
    KeyItem kt b _ -> pure $ do
        let note = fromMaybe "There is no note for this key." b.note
            period = if not (T.null note) && T.last note /= '.' then "." else ""
            cmd = printList b.commands
        p1 <- plain (note <> period) p0 >>= penSkipLine
        p2 <- plain ("This key is in the " <> kt.keyTable <> " table.") p1
            >>= penValue th "Repeat: " (if b.repeats == Repeats then "on" else "off")
            >>= penSkipLine
        p3 <- penValue th "Command: " cmd p2
        case defaultBinding kt.keyTable kt.keyName of
            Just d | printList d.commands /= cmd -> penValue th "The default is: " (printList d.commands) p3
            _ -> Right p3
    EnvItem et _ entry -> do
        genv <- readTVarIO st.globalEnviron
        pure $ do
            let global = et.envScope == GlobalEnvRef
            p1 <- plain ("This is a " <> (if global then "global" else "session")
                <> " environment variable.") p0
            p2 <- (if entry.visibility == EnvHidden then plain "This variable is hidden." else Right) p1
                >>= penSkipLine
            p3 <- case entry.value of
                Nothing -> plain "Variable is removed." p2
                Just v -> penValue th "Variable value: " v p2
            case (global, environFind et.name genv) of
                (False, Just parent) -> case parent.value of
                    Nothing -> plain "Global variable is removed." p3
                    Just v -> penValue th "Global value: " v p3
                _ -> Right p3
    OptionItem ot t v -> optionPreview st th h ot t v p0

optionPreview
    :: ServerState -> Theme -> Here -> OptionTarget -> Table -> OptionValue -> Pen
    -> IO (Either Pen Pen)
optionPreview st th h ot t v p0 = do
    let n = ot.name
        mdef = lookupOptionDef n
    mmon <- monitorAt st t.scope n
    userHook <- if "@" `T.isPrefixOf` n then isRegisteredEvent st n else pure False
    fire <- hookFire st t.scope n mmon
    fireText <- forM fire $ \(count, mtime) -> case mtime of
        Nothing -> pure ("This hook has been fired " <> tshow count <> " times.")
        Just tm -> do
            when' <- expandFormat st (Map.insert "hook_fire_time" (epoch tm) h.env)
                "#{t/p:hook_fire_time}"
            pure ("This hook has been fired " <> tshow count <> " times, last " <> when' <> ".")
    let isHook = maybe False ((== HookOption) . (.role)) mdef
        isMonitor = isJust mmon
        anyHook = isHook || isMonitor || userHook
        isArray = maybe False (isJust . (.array)) mdef
        (space, unit) = case mdef >>= (.unit) of
            Just u -> (" ", u)
            Nothing -> ("", "")
        value = valueString ot.arrayKey v
        description = case mdef >>= (.description) of
            Just d -> d
            Nothing
                | isMonitor -> "This hook runs when a monitor changes."
                | userHook -> "This hook doesn't have a description."
                | otherwise -> "This option doesn't have a description."
        what = case (.scope) <$> mdef of
            Nothing -> "user"
            Just WindowPaneScope -> "window and pane"
            Just WindowScope -> "window"
            Just SessionScope -> "session"
            Just ServerScope -> "server"
        defaultText = case mdef of
            Just d | isNothing ot.arrayKey, valueText d.defaultValue /= value -> Just (valueText d.defaultValue)
            _ -> Nothing
        fireLine p = maybe (Right p) (`plain` p) fireText
    expanded <- case (.kind) <$> mdef of
        Just k | k /= KString -> pure Nothing
        _ -> do
            e <- expandFormat st h.env value
            pure (if e /= value then Just e else Nothing)
    example <- exampleStyle st th h mdef value
    parents <- parentValues st h ot t
    pure $ do
        p1 <- plain description p0 >>= penSkipLine
        -- tmux stops after the array lines when no key is selected.
        let body p
                | isMonitor = plain "This is a monitor hook." p >>= monitorLine
                | otherwise = do
                    p' <- plain (if userHook then "This is a user hook."
                        else if isHook then "This is a " <> what <> " hook."
                        else "This is a " <> what <> " option.") p
                    monitorLine p'
            monitorLine p = maybe (Right p) (\m -> penValue th "Monitor: " (monitorSpec n m) p) mmon
        p2 <- body p1
        p3 <- if not isArray then Right p2 else case (isHook, ot.arrayKey) of
            (True, Nothing) -> plain "This is an array hook." p2 >>= fireLine >>= Left
            (True, Just _) -> Right p2
            (False, Just k) -> plain ("This is an array option, key " <> arrayKeyText k <> ".") p2
            (False, Nothing) -> plain "This is an array option." p2 >>= Left
        p4 <- penSkipLine p3
        p5 <- if anyHook
            then penValue th "Hook command: " (value <> space <> unit) p4 >>= fireLine
            else penValue th "Option value: " (value <> space <> unit) p4
        p6 <- maybe (Right p5) (\e -> penValue th "This expands to: " e p5) expanded
        p7 <- case (.kind) <$> mdef of
            Just (KChoice cs) -> penValue th "Available values are: " (T.intercalate ", " cs) p6
            _ -> Right p6
        p8 <- case example of
            Just (label, sty) -> penText True Cell.defaultStyle label p7
                >>= penText False sty "EXAMPLE"
            Nothing -> Right p7
        p9 <- maybe (Right p8) (\d -> penValue th "The default is: " (d <> space <> unit) p8) defaultText
        let p10 = p9 { x = 0, y = p9.y + 1 }
        if p10.y > p10.height - 1 then Left p10 else
            foldM (\p (label, pv) -> penValue th label (pv <> space <> unit) p) p10 parents
  where
    epoch tm = tshow (floor tm :: Integer)

-- | A hook's fire count and last fire time (a monitor's own).
hookFire :: ServerState -> Scope -> Text -> Maybe Monitor -> IO (Maybe (Int, Maybe POSIXTime))
hookFire st sc n mmon = case mmon of
    Just m -> do
        c <- readTVarIO m.fireCount
        t <- readTVarIO m.fireTime
        pure (Just (c, t))
    Nothing -> case hookScopeOf sc of
        Nothing -> pure Nothing
        Just hs -> do
            me <- Map.lookup n <$> hookEntriesAt st hs
            let isHook = maybe False ((== HookOption) . (.role)) (lookupOptionDef n)
            registered <- isRegisteredEvent st n
            pure $ case me of
                Just e -> Just (e.fireCount, e.fireTime)
                Nothing | isHook || registered -> Just (0, Nothing)
                _ -> Nothing

-- | A colour or style option's "EXAMPLE", in its own colour or style.
exampleStyle
    :: ServerState -> Theme -> Here -> Maybe OptionDef -> Text
    -> IO (Maybe (Text, Cell.Style))
exampleStyle st th h mdef value = case (.kind) <$> mdef of
    Just KColour -> pure (Just ("This is a colour option: ", styleCell th ("fg=" <> value)))
    Just KColourString -> do
        v <- expandFormat st h.env value
        pure (Just ("This is a colour option: ", styleCell th ("fg=" <> v)))
    Just KStyle -> do
        v <- if "#{" `T.isInfixOf` value then expandFormat st h.env value else pure value
        pure (Just ("This is a style option: ", styleCell th v))
    _ -> pure Nothing

-- | The window's and global values behind an option a narrower table sets
-- (tmux's "Window value" and "Global value" lines).
parentValues :: ServerState -> Here -> OptionTarget -> Table -> IO [(Text, Text)]
parentValues st h ot t
    | isArrayOpt = pure []
    | otherwise = do
        let (wref, gref) = case t.ref of
                PaneTableRef _ -> (Just (WindowTableRef h.window.id), Just GlobalWindowTableRef)
                WindowTableRef _ -> (Nothing, Just GlobalWindowTableRef)
                SessionTableRef _ -> (Nothing, Just GlobalSessionTableRef)
                _ -> (Nothing, Nothing)
            only r = case tableAt h =<< r of
                Nothing -> pure Nothing
                Just pt -> do
                    own <- case ot.family of
                        PlainFamily -> atomically (ownOnly pt.scope)
                        HookFamily -> valueIn st HookFamily pt.scope ot.name
                    pure (valueText <$> own)
        wv <- only wref
        gv <- only gref
        pure $ [ ("Window value (from window " <> tshow h.winlink <> "): ", v) | Just v <- [wv] ]
            <> [ ("Global value: ", v) | Just v <- [gv] ]
  where
    isArrayOpt = maybe False (isJust . (.array)) (lookupOptionDef ot.name)
    ownOnly sc = ownValue st sc ot.name

-- Keys ------------------------------------------------------------------

-- | customize-mode's own keys (tmux's @window_customize_key@), after the
-- mode tree's: @ls@ are the mode's lines and @ts@ its state settled on them.
customizeKey
    :: CustomizeCtx -> ServerState -> [Line] -> TreeState -> Text -> IO ()
customizeKey ctx st ls ts k = hereOf st ts >>= \case
    Nothing -> pure ()
    Just h -> do
        let cur = listToMaybe (drop ts.currentIx ls)
        mtarget <- maybe (pure Nothing) (targetOf st h . (.item.tag)) cur
        case k of
            "e" -> forM_ mtarget (startEdit ctx st)
            "a" -> case mtarget of
                Just (OptionItem ot _ _) | Just key <- ot.arrayKey -> prompt
                    ("(" <> ot.name <> "[" <> arrayKeyText key <> "]) ")
                    (arrayKeyText key) (PromptArrayKey ot)
                _ -> pure ()
            _ | k `elem` ["Enter", "s"] -> case (cur, mtarget) of
                (Just l, Nothing) -> addCurrent h l.item.tag
                (_, Just t) -> setItem h t SetHere
                _ -> pure ()
            "w" -> case mtarget of
                Just t@OptionItem {} -> setItem h t SetWindow
                _ -> pure ()
            _ | k `elem` ["S", "W"] -> case mtarget of
                Just KeyItem {} -> pure ()
                Just t -> setItem h t SetGlobal
                Nothing -> pure ()
            "d" -> case mtarget of
                Just (OptionItem ot _ _) | isNothing ot.arrayKey -> confirm
                    ("Reset " <> ot.name <> " to default? ") (PromptChange ResetItem CurrentItem)
                Just (KeyItem kt _ _) -> confirm
                    ("Reset " <> kt.keyName <> " to default? ") (PromptChange ResetItem CurrentItem)
                _ -> pure ()
            "D" -> do
                let n = length (taggedOf ls)
                unless (n == 0) $ confirm ("Reset " <> tshow n <> " tagged to default? ")
                    (PromptChange ResetItem TaggedItems)
            "u" -> forM_ mtarget $ \t -> confirm ("Unset " <> targetLabel t <> "? ")
                (PromptChange UnsetItem CurrentItem)
            "U" -> do
                let n = length (taggedOf ls)
                unless (n == 0) $ confirm ("Unset " <> tshow n <> " tagged? ")
                    (PromptChange UnsetItem TaggedItems)
            "H" -> ctx.put ts { globalsShown = case ts.globalsShown of
                GlobalsShown -> GlobalsHidden
                GlobalsHidden -> GlobalsShown }
            "C" -> ctx.put ts { itemsShown = case ts.itemsShown of
                AllItems -> ChangedItems
                ChangedItems -> AllItems }
            _ -> pure ()
  where
    taggedOf = filter ((`elem` ts.tagged) . (.item.tag))
    prompt label initial use = ctx.put ts { modePrompt = Just TreePrompt
        { edit = promptFor label initial "", use = use, entry = PromptLine } }
    confirm label use = case ts.confirm of
        KillWithoutAsking -> customizeAnswer ctx st ls ts use (Just "y")
        AskBeforeKill -> ctx.put ts { modePrompt = Just TreePrompt
            { edit = promptFor label "" "", use = use, entry = PromptSingleKey } }
    targetLabel = \case
        OptionItem ot _ _ -> ot.name <> maybe "" (\key -> "[" <> arrayKeyText key <> "]") ot.arrayKey
        EnvItem et _ _ -> et.name
        KeyItem kt _ _ -> kt.keyName
    -- tmux's @window_customize_add_current@: a heading adds to its group.
    addCurrent h = \case
        TagGroup g -> case g of
            GlobalEnvironment -> prompt "New environment: " "" (PromptNewEnvironment GlobalEnvRef)
            SessionEnvironment ->
                prompt "New environment: " "" (PromptNewEnvironment (SessionEnvRef h.session.id))
            _ -> do
                let ref = case g of
                        ServerOptions -> ServerTableRef
                        SessionOptions -> SessionTableRef h.session.id
                        SessionHooks -> SessionTableRef h.session.id
                        _ -> PaneTableRef h.pane.id
                    fam = groupFamily g
                    what = if fam == HookFamily then "hook" else "option"
                prompt ("New user " <> what <> ": ") "@" (PromptNewOption ref fam)
        TagKeyTable t -> prompt ("New key in " <> t <> ": ") "" (PromptNewKey t)
        _ -> pure ()
    setItem h t how = case t of
        KeyItem kt b mp -> case mp of
            Just KeyRepeat -> atomically $ do
                modifyTVar' st.keymap $ Map.adjust (Map.adjust (\bd -> bd
                    { repeats = if b.repeats == Repeats then Once else Repeats }) kt.keyName) kt.keyTable
                bumpDirty st
            Just KeyCommand -> prompt ("(" <> kt.keyName <> ") ") (printList b.commands) (PromptKeyCommand kt)
            Just KeyNote -> prompt ("(" <> kt.keyName <> ") ") (fromMaybe "" b.note) (PromptKeyNote kt)
            Nothing -> pure ()
        EnvItem et _ entry -> do
            let (sref, label) = case how of
                    SetGlobal -> (GlobalEnvRef, ", global")
                    _ -> case et.envScope of
                        GlobalEnvRef -> (GlobalEnvRef, ", global")
                        s -> (s, ", for session " <> h.sessionName)
            prompt ("(" <> et.name <> label <> ") ") (fromMaybe "" entry.value)
                (PromptEnvironmentValue et { envScope = sref })
        OptionItem ot tbl v -> setOption h ot tbl v how
    -- tmux's @window_customize_set_option@.
    setOption h ot tbl v how = do
        let mdef = lookupOptionDef ot.name
            isArray = maybe False (isJust . (.array)) mdef
            paneOk = maybe True ((== WindowPaneScope) . (.scope)) mdef
            ref
                | isArray = tbl.ref
                | otherwise = case how of
                    SetGlobal -> case tbl.ref of
                        SessionTableRef _ -> GlobalSessionTableRef
                        WindowTableRef _ -> GlobalWindowTableRef
                        PaneTableRef _ -> GlobalWindowTableRef
                        r -> r
                    _ -> case tbl.ref of
                        r@(SessionTableRef _) -> r
                        GlobalSessionTableRef -> SessionTableRef h.session.id
                        r | r `elem` [GlobalWindowTableRef, WindowTableRef h.window.id, PaneTableRef h.pane.id] ->
                            if how == SetHere && paneOk then PaneTableRef h.pane.id
                                else WindowTableRef h.window.id
                        r -> r
            target = OptionTarget
                { table = ref, family = ot.family, name = ot.name, arrayKey = ot.arrayKey }
        case (.kind) <$> mdef of
            Just KFlag | not isArray -> toggle target (\cur -> if cur == "on" then "off" else "on")
            Just (KChoice cs) | not isArray -> toggle target (nextChoice (honoured mdef cs))
            _ -> do
                let st' = scopeText h ref
                    spacing
                        | not (T.null st') = ", for "
                        | ref /= ServerTableRef = ", global"
                        | otherwise = ""
                    keyText = case (isArray, ot.arrayKey) of
                        (True, Nothing) -> "[+]"
                        (True, Just key) -> "[" <> arrayKeyText key <> "]"
                        _ -> ""
                prompt ("(" <> ot.name <> keyText <> spacing <> st' <> ") ")
                    (valueString ot.arrayKey v) (PromptOptionValue target)
    toggle :: OptionTarget -> (Text -> Text) -> IO ()
    toggle target next = hereOf st ts >>= mapM_ (\h -> forM_ (tableAt h target.table) $ \t -> do
        -- The value in effect at the table (tmux's @options_get_number@).
        cur <- maybe "" (valueText . snd) <$> locateValue st h target.family
            (reverse (target.table : parentRefs h target.table)) target.name
        r <- assignOption st t.scope (familyRole target.family) target.name Nothing (next cur)
        either ctx.message pure r)
    honoured mdef cs = case (.gate) <$> mdef of
        Just (Live s) | Just only <- s.onlyChoices -> filter (`elem` only) cs
        _ -> cs
    nextChoice cs cur = case dropWhile (/= cur) cs of
        _ : c : _ -> c
        _ -> fromMaybe cur (listToMaybe cs)

-- | Which table a set addresses: the item's own (Enter, @s@), the window
-- rather than the pane (@w@), or the global one (@S@, @W@).
data SetHow = SetHere | SetWindow | SetGlobal
    deriving (Eq)

-- | Open an item's value in the editor (tmux's
-- @window_customize_start_edit@): what the editor saves becomes its value.
startEdit :: CustomizeCtx -> ServerState -> Target -> IO ()
startEdit ctx st target = do
    mc <- ctx.client
    let open text apply = forM_ mc $ \c ->
            spawnEditor ctx.dispatch st c (TE.encodeUtf8 (if T.null text then "\n" else text))
                (\w -> atomically $ modifyMode (\t -> t { editor = Just w }))
                (\w mbytes -> do
                    mine <- atomically $ do
                        es <- readTVar ctx.pane.modes
                        case es of
                            e@ModeEntry { mode = TreeMode t } : rest
                                | t.kind == OptionTree, t.editor == Just w -> do
                                    writeTVar ctx.pane.modes
                                        (e { mode = TreeMode t { editor = Nothing } } : rest)
                                    True <$ bumpDirty st
                            _ -> pure False
                    forM_ (if mine then mbytes else Nothing) $ \bytes -> do
                        let saved = TE.decodeUtf8Lenient bytes
                            value = fromMaybe saved (T.stripSuffix "\n" saved)
                        mts <- currentState
                        forM_ mts $ \ts' -> hereOf st ts' >>= mapM_ (\h -> apply h value))
    case target of
        OptionItem ot _ v | editable ot -> open (valueString ot.arrayKey v) $ \h value ->
            whenOption h ot $ \t -> void (assignOption st t.scope (familyRole ot.family)
                ot.name ot.arrayKey value)
        KeyItem kt b (Just KeyCommand) -> open (printList b.commands) $ \_ value ->
            void (setKeyCommand ctx.dispatch st kt value)
        KeyItem kt b (Just KeyNote) -> open (fromMaybe "" b.note) $ \_ value ->
            setKeyNote st kt value
        EnvItem et _ entry | Just v <- entry.value -> open v $ \h value ->
            forM_ (envVar st h et.envScope) $ \var -> atomically $ do
                modifyTVar' var $ \env -> environSet
                    (maybe et.visibility (.visibility) (environFind et.name env)) et.name value env
                bumpDirty st
        _ -> pure ()
  where
    modifyMode f = do
        es <- readTVar ctx.pane.modes
        case es of
            e@ModeEntry { mode = TreeMode t } : rest | t.kind == OptionTree ->
                writeTVar ctx.pane.modes (e { mode = TreeMode (f t) } : rest) >> bumpDirty st
            _ -> pure ()
    currentState = atomically $ do
        es <- readTVar ctx.pane.modes
        pure $ case es of
            ModeEntry { mode = TreeMode t } : _ | t.kind == OptionTree -> Just t
            _ -> Nothing
    editable ot = case (.kind) <$> lookupOptionDef ot.name of
        Just KFlag -> False
        Just (KChoice _) -> False
        _ -> True
    whenOption h ot act = forM_ (tableAt h ot.table) $ \t -> do
        exists <- isJust <$> valueIn st ot.family t.scope ot.name
        when exists (act t)

-- | Replace a binding's commands with a command line; the error when it does
-- not parse or names no command.
setKeyCommand :: Dispatch -> ServerState -> KeyTarget -> Text -> IO (Either Text ())
setKeyCommand d st kt line = resolveCommandText st Nothing line >>= \case
    Left e -> pure (Left e)
    Right cmds -> do
        bad <- catMaybes <$> mapM (d.unknownCommand st) cmds
        case bad of
            e : _ -> pure (Left e)
            [] -> Right () <$ atomically (do
                modifyTVar' st.keymap $ Map.adjust (Map.adjust (\b -> Binding
                    { commands = cmds, repeats = b.repeats, note = b.note }) kt.keyName) kt.keyTable
                bumpDirty st)

setKeyNote :: ServerState -> KeyTarget -> Text -> IO ()
setKeyNote st kt note = atomically $ do
    modifyTVar' st.keymap $ Map.adjust (Map.adjust (\b -> b
        { note = if T.null note then Nothing else Just note }) kt.keyName) kt.keyTable
    bumpDirty st

-- Prompts ---------------------------------------------------------------

-- | Act on a customize-mode prompt's answer (tmux's prompt callbacks);
-- @Nothing@ is a cancelled prompt. The mode's state @ts@ has the prompt
-- closed already.
customizeAnswer
    :: CustomizeCtx -> ServerState -> [Line] -> TreeState -> TreePromptUse -> Maybe Text
    -> IO ()
customizeAnswer ctx st ls ts use manswer = hereOf st ts >>= mapM_ (\h -> case (use, manswer) of
    (PromptEnvironmentValue et, Just v) -> forM_ (envVar st h et.envScope) $ \var -> atomically $ do
        modifyTVar' var $ \env -> environSet
            (maybe et.visibility (.visibility) (environFind et.name env)) et.name v env
        bumpDirty st
    (_, Nothing) -> pure ()
    (_, Just "") -> pure ()
    (PromptOptionValue ot, Just v) -> forM_ (tableAt h ot.table) $ \t -> do
        exists <- isJust <$> optionReachable h ot t
        when exists $ do
            key <- arrayKeyFor ot t
            r <- assignOption st t.scope (familyRole ot.family) ot.name key v
            either (ctx.message . capitalise) pure r
    (PromptArrayKey ot, Just k) -> forM_ ((,) <$> tableAt h ot.table <*> ot.arrayKey) $ \(t, old) ->
        case parseArrayKey k of
            Nothing -> ctx.message (capitalise ("invalid array key: " <> k))
            Just new -> valueIn st ot.family t.scope ot.name >>= \case
                Just (OVArray m) | Just v <- Map.lookup old m, not (Map.member new m) -> do
                    r <- assignOption st t.scope (familyRole ot.family) ot.name (Just new) v
                    case r of
                        Left e -> ctx.message (capitalise e)
                        Right () -> removeOption st t.scope (familyRole ot.family) ot.name (Just old)
                _ -> pure ()
    (PromptNewOption ref fam, Just s) -> forM_ (tableAt h ref) $ \t -> do
        let (nm, rest) = T.break (`elem` [' ', '\t']) s
            value = T.dropWhile (`elem` [' ', '\t']) rest
            what = if fam == HookFamily then "hook" else "option"
        if T.null nm || T.null value
            then ctx.message ("User " <> what <> " must be @name value")
            else case matchOptionName nm of
                Right (n, Nothing) | "@" `T.isPrefixOf` n -> do
                    r <- assignOption st t.scope (familyRole fam) n Nothing value
                    either (ctx.message . capitalise) pure r
                _ -> ctx.message ("User " <> what <> " name must start with @")
    (PromptNewEnvironment sref, Just s) -> forM_ (envVar st h sref) $ \var -> case T.uncons s of
        Just ('-', n)
            | T.null n || "=" `T.isInfixOf` n -> ctx.message ("Bad environment variable: " <> s)
            | otherwise -> atomically (modifyTVar' var (environClear n) >> bumpDirty st)
        _ -> case T.breakOn "=" s of
            (n, rest) | not (T.null n), Just v <- T.stripPrefix "=" rest ->
                atomically (modifyTVar' var (environSet EnvVisible n v) >> bumpDirty st)
            _ -> ctx.message "Environment variable must be NAME=value"
    (PromptNewKey table, Just s) -> do
        let (keyName, rest) = T.break (`elem` [' ', '\t']) s
            command = T.dropWhile (`elem` [' ', '\t']) rest
        if T.null keyName || T.null command
            then ctx.message "Key binding must be key command"
            else case parseKeyName keyName of
                Nothing -> ctx.message ("Unknown key: " <> keyName)
                Just key -> resolveCommandText st Nothing command >>= \case
                    Left e -> ctx.message (capitalise e)
                    Right cmds -> do
                        bad <- catMaybes <$> mapM (ctx.dispatch.unknownCommand st) cmds
                        case bad of
                            e : _ -> ctx.message (capitalise e)
                            [] -> atomically $ do
                                modifyTVar' st.keymap $ Map.insertWith Map.union table
                                    (Map.singleton key.name Binding
                                        { commands = cmds, repeats = Once, note = Nothing })
                                bumpDirty st
    (PromptKeyCommand kt, Just s) -> do
        r <- setKeyCommand ctx.dispatch st kt s
        either (ctx.message . capitalise) pure r
    (PromptKeyNote kt, Just s) -> setKeyNote st kt s
    (PromptChange change reach, Just s)
        | T.toLower s == "y" -> do
            let cur = listToMaybe (drop ts.currentIx ls)
                tags = case reach of
                    CurrentItem -> map (.item.tag) (maybe [] pure cur)
                    TaggedItems -> [ l.item.tag | l <- ls, l.item.tag `elem` ts.tagged ]
            forM_ tags $ \tag -> targetOf st h tag >>= mapM_ (\t -> do
                when (reach == CurrentItem && movesUp change t) $ case drop (ts.currentIx - 1) ls of
                    prev : _ | ts.currentIx > 0 -> ctx.put ts { current = Just prev.item.tag
                                                              , currentIx = ts.currentIx - 1 }
                    _ -> pure ()
                applyChange h change t)
    _ -> pure ())
  where
    capitalise t = case T.uncons t of
        Just (c, r) -> T.cons (toUpperAscii c) r
        Nothing -> t
    toUpperAscii c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c
    optionReachable h ot t = do
        own <- valueIn st ot.family t.scope ot.name
        case own of
            Just v -> pure (Just v)
            Nothing -> fmap snd <$> locateValue st h ot.family (parentRefs h ot.table) ot.name
    -- An array set with no key appends at the first free index.
    arrayKeyFor ot t = case (ot.arrayKey, lookupOptionDef ot.name >>= (.array)) of
        (Nothing, Just _) -> do
            own <- valueIn st ot.family t.scope ot.name
            pure $ Just $ firstFreeIndex $ case own of
                Just (OVArray m) -> m
                _ -> Map.empty
        (k, _) -> pure k
    -- tmux moves the selection up when the selected item will go.
    movesUp change = \case
        OptionItem ot _ _ -> change == UnsetItem && isJust ot.arrayKey
        EnvItem {} -> change == UnsetItem
        KeyItem kt _ _ -> change == UnsetItem
            || isNothing (defaultBinding kt.keyTable kt.keyName)
    applyChange h change = \case
        OptionItem ot t _ -> case change of
            UnsetItem -> removeOption st t.scope (familyRole ot.family) ot.name ot.arrayKey
            ResetItem -> unless (isJust ot.arrayKey) $
                forM_ (ot.table : parentRefs h ot.table) $ \r -> forM_ (tableAt h r) $ \pt -> do
                    own <- case ot.family of
                        PlainFamily -> atomically (ownValue st pt.scope ot.name)
                        HookFamily -> valueIn st HookFamily pt.scope ot.name
                    when (isJust own) $
                        removeOption st pt.scope (familyRole ot.family) ot.name Nothing
        EnvItem et var _ -> when (change == UnsetItem) $ atomically $ do
            modifyTVar' var (environUnset et.name)
            bumpDirty st
        KeyItem kt _ _ -> atomically $ do
            modifyTVar' st.keymap $ Map.adjust (Map.alter (\_ -> case change of
                UnsetItem -> Nothing
                ResetItem -> defaultBinding kt.keyTable kt.keyName) kt.keyName) kt.keyTable
            bumpDirty st
