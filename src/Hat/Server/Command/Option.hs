-- | The option and environment commands: @set-option@\/@set-window-option@,
-- @show-options@, and @set-environment@\/@show-environment@. Option
-- handling is generic over the registry ("Hat.Model.OptionTable"), following
-- tmux's @cmd-set-option.c@ and @cmd-show-options.c@: an option's row picks
-- its scope, its kind validates the value ("Hat.Server.OptionParse"), and
-- its gate refuses what hat does not implement.
module Hat.Server.Command.Option
    ( SetDefault (..)
    , SetMode (..)
    , setOption
    , ShowLine (..)
    , Inheritance (..)
    , Display (..)
    , ValuePresence (..)
    , renderShowLine
    , cmdSet
    , cmdShow
    , cmdSetEnvironment
    , cmdShowEnvironment
    , refreshGlobalOptions
    , statusRefreshInterval
    , applyHistoryLimit
    ) where

import Control.Concurrent.STM
import Control.Monad (forM, forM_, when)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Model
import Hat.Model.Options
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import Hat.Server.Environ
import Hat.Server.Format (FormatEnv, quoteArgsT)
import Hat.Server.FormatEnv (paneEnvById, paneFormatEnv)
import Hat.Server.HookTypes (HookAmbient (..), HookEntry (..), HookScope (..))
import Hat.Server.Hooks (ambientFor, hookEntriesAt)
import Hat.Server.Locate
    ( findTarget, locatePane, noSuchTarget, paneIndexOf
    , targetCurrentWindow, targetPaneScoped, targetSession )
import Hat.Server.OptionParse
import Hat.Server.Target qualified as Target
import Hat.Server.FormatEnv (expandFormat, sessionFormatEnv)
import Hat.Term.Emulator qualified as Emu

-- | Which command was typed: @set-window-option@ addresses windows for a
-- @\@user@ option the way @-w@ does.
data SetDefault = DefaultSession | DefaultWindow
    deriving (Eq, Show)

-- | The option table a @set-option@ or @show-options@ addresses.
data Scope
    = ServerTable
    | GlobalSessionTable
    | GlobalWindowTable
    | SessionTable Session
    | WindowTable Window
    | PaneTable Pane

-- | tmux's @options_scope_from_name@: a registered option's row fixes its
-- table (@-g@ picks the global one, @-p@ a pane for a window-and-pane
-- option); a @\@user@ option goes by the flags ('scopeFromFlags').
scopeFor
    :: ServerState -> Maybe Client -> Maybe Text -> SetDefault -> [Text]
    -> Maybe OptionDef -> IO (Either Text Scope)
scopeFor st mclient mtarget sd flags = \case
    Nothing -> scopeFromFlags st mclient mtarget sd flags
    Just d -> case d.scope of
        ServerScope -> pure (Right ServerTable)
        SessionScope
            | "-g" `elem` flags -> pure (Right GlobalSessionTable)
            | otherwise -> sessionScope st mclient mtarget
        WindowPaneScope | "-p" `elem` flags -> paneScope st mclient mtarget
        _   | "-g" `elem` flags -> pure (Right GlobalWindowTable)
            | otherwise -> windowScope st mclient mtarget

-- | tmux's @options_scope_from_flags@.
scopeFromFlags
    :: ServerState -> Maybe Client -> Maybe Text -> SetDefault -> [Text]
    -> IO (Either Text Scope)
scopeFromFlags st mclient mtarget sd flags
    | has "-s" = pure (Right ServerTable)
    | has "-p" = paneScope st mclient mtarget
    | sd == DefaultWindow || has "-w" = if has "-g"
        then pure (Right GlobalWindowTable)
        else windowScope st mclient mtarget
    | has "-g" = pure (Right GlobalSessionTable)
    | otherwise = sessionScope st mclient mtarget
  where
    has f = f `elem` flags

sessionScope, windowScope, paneScope
    :: ServerState -> Maybe Client -> Maybe Text -> IO (Either Text Scope)
sessionScope st mclient mtarget =
    maybe (Left (noSuchTarget "session" mtarget)) (Right . SessionTable)
        <$> targetSession st mclient mtarget
windowScope st mclient mtarget =
    maybe (Left (noSuchTarget "window" mtarget)) (Right . WindowTable)
        <$> targetWindowScoped st mclient mtarget
paneScope st mclient mtarget =
    maybe (Left (noSuchTarget "pane" mtarget)) (Right . PaneTable)
        <$> targetPaneScoped st mclient mtarget

deltaVar :: ServerState -> Scope -> TVar OptionsDelta
deltaVar st = \case
    ServerTable -> st.serverOptions
    GlobalSessionTable -> st.globalSessionOptions
    GlobalWindowTable -> st.globalWindowOptions
    SessionTable s -> s.options
    WindowTable w -> w.options
    PaneTable p -> p.options

-- | Whether a registered option row lists at a table (tmux's
-- @oe->scope & scope@).
listsAt :: Scope -> OptionScope -> Bool
listsAt sc os = case sc of
    ServerTable -> os == ServerScope
    GlobalSessionTable -> os == SessionScope
    SessionTable _ -> os == SessionScope
    GlobalWindowTable -> os `elem` [WindowScope, WindowPaneScope]
    WindowTable _ -> os `elem` [WindowScope, WindowPaneScope]
    PaneTable _ -> os == WindowPaneScope

-- | A table's own value for an option. tmux's global tables hold every
-- registered option, so there an unset entry reads as the color scheme's
-- value or the registry default.
ownValue :: ServerState -> Scope -> Text -> STM (Maybe OptionValue)
ownValue st sc n = do
    own <- lookupDelta n <$> readTVar (deltaVar st sc)
    case (own, lookupOptionDef n) of
        (Just v, _) -> pure (Just v)
        (Nothing, Just d) | globalTable sc, d.role == PlainOption -> do
            scheme <- lookupDelta n <$> readTVar st.schemeOptions
            pure (Just (fromMaybe d.defaultValue scheme))
        _ -> pure Nothing
  where
    globalTable = \case
        ServerTable -> True
        GlobalSessionTable -> True
        GlobalWindowTable -> True
        _ -> False

-- | The tables an option inherits from, nearest first.
parentScopes :: ServerState -> Scope -> STM [Scope]
parentScopes st = \case
    SessionTable _ -> pure [GlobalSessionTable]
    WindowTable _ -> pure [GlobalWindowTable]
    PaneTable p -> do
        mloc <- locatePane st p.id
        pure (maybe [] (\(_, w) -> [WindowTable w]) mloc <> [GlobalWindowTable])
    _ -> pure []

-- | The value in effect at a table: its own, else the nearest parent's.
inheritedValue :: ServerState -> Scope -> Text -> STM (Maybe OptionValue)
inheritedValue st sc n = do
    ps <- parentScopes st sc
    firstJust <$> mapM (\p -> ownValue st p n) ps
  where
    firstJust = listToMaybe . concatMap (maybe [] pure)

-- | Refuse an option the registry knows but hat does not implement.
requireLive :: OptionDef -> Either Text ()
requireLive d = case d.gate of
    NotYet -> Left (notYetError d.name)
    Live _ -> Right ()

-- | Refuse a pane-scope value of an option whose consumer reads only the
-- window's.
requirePaneValues :: Scope -> Maybe OptionDef -> Either Text ()
requirePaneValues sc md = case (sc, md) of
    (PaneTable _, Just d)
        | Live s <- d.gate, s.paneValues == PaneValuesIgnored ->
            Left (d.name <> ": pane values not yet supported by hat")
    _ -> Right ()

-- | Whether @set -u@\/@-U@ removes the option rather than setting it.
data Unsetting = SetValue | UnsetValue
    deriving (Eq, Show)

-- | @set -o@: refuse to replace a value the table already holds.
data Overwrite = OverwriteAny | OnlyIfUnset
    deriving (Eq, Show)

-- | @-q@: swallow the errors tmux lets it swallow.
data Quietness = Loud | Quiet
    deriving (Eq, Show)

data SetRequest = SetRequest
    { mode :: SetMode
    , unsetting :: Unsetting
    , overwrite :: Overwrite
    , quietness :: Quietness
    }

-- | What a set does to its table's entry.
data SetAction = WriteValue OptionValue | RemoveValue | LeaveValue

-- | tmux's @cmd_set_option_exec@ decision for one option at one table,
-- given the table's own value and the value in effect there.
planSet
    :: Maybe OptionDef  -- ^ the option's row; 'Nothing' for a @\@user@ option
    -> SetRequest
    -> Text             -- ^ the option as typed, for messages
    -> Maybe ArrayKey
    -> Maybe OptionValue  -- ^ the table's own value
    -> Maybe OptionValue  -- ^ the value in effect at the table
    -> Maybe Text         -- ^ the value given, if any
    -> Either Text SetAction
planSet mdef req spelled mkey own effective mvalue
    | isJust mkey, not isArray = Left ("not an array: " <> spelled)
    | req.unsetting == SetValue, req.overwrite == OnlyIfUnset, alreadySet =
        if req.quietness == Quiet then Right LeaveValue
        else Left ("already set: " <> spelled)
    | req.unsetting == UnsetValue = Right $ case (own, mkey) of
        (Nothing, _) -> LeaveValue
        (Just (OVArray m), Just k) -> WriteValue (OVArray (Map.delete k m))
        _ -> RemoveValue
    | otherwise = WriteValue <$> case mdef of
        Nothing -> case mvalue of
            Nothing -> Left "empty value"
            Just v -> Right $ OVScalar $ case (req.mode, own) of
                (Append, Just old) -> scalarText old <> v
                _ -> v
        Just d | Nothing <- d.array -> do
            new <- parseScalar d req.mode (scalarText <$> own)
                (maybe "" scalarText effective) mvalue
            supportedValue d new
            pure (OVScalar new)
        Just d -> case mvalue of
            Nothing -> Left "empty value"
            Just v -> OVArray <$> case mkey of
                Nothing -> assignArray d
                    (if req.mode == Append then ownArray else Map.empty) v
                Just k -> setArrayKey d req.mode ownArray k v
  where
    isArray = maybe False (isJust . (.array)) mdef
    ownArray = case own of
        Just (OVArray m) -> m
        _ -> Map.empty
    alreadySet = case mkey of
        Nothing -> isJust own
        Just k -> Map.member k ownArray

scalarText :: OptionValue -> Text
scalarText = valueText

-- | Apply one @set-option@ to a resolved 'Options' as though it were the
-- global table — the config-load burn-down's model of @set -g@.
setOption :: SetMode -> Options -> Text -> Text -> Either Text Options
setOption mode opts spelled value = do
    (n, mkey) <- either (\(MatchError e) -> Left e) Right (matchOptionName spelled)
    let mdef = lookupOptionDef n
        own = if "@" `T.isPrefixOf` n
            then OVScalar <$> Map.lookup n opts.user
            else Map.lookup n opts.values
    mapM_ requireLive mdef
    act <- planSet mdef (SetRequest mode SetValue OverwriteAny Loud)
        spelled mkey own own (Just value)
    pure $ case act of
        WriteValue v -> applyEntry n v opts
        _ -> opts

cmdSet :: SetDefault -> CommandImpl
cmdSet sd st mclient args = do
    let (opts, flags, pos) = parseArgs "t" args
        has f = f `elem` flags
        mtarget = lookup "-t" opts
        quiet = if has "-q" then Quiet else Loud
        req = SetRequest
            { mode = if has "-a" then Append else Assign
            , unsetting = if has "-u" || has "-U" then UnsetValue else SetValue
            , overwrite = if has "-o" then OnlyIfUnset else OverwriteAny
            , quietness = quiet }
        orQuiet e = pure [RErr e | quiet == Loud]
    case pos of
        [] -> pure [RErr "usage: set-option [-aFgopqsuUw] [-t target] option [value]"]
        (spelled : rest) -> case matchOptionName spelled of
            Left (MatchError e) -> orQuiet e
            Right (n, mkey) -> do
                let mdef = lookupOptionDef n
                mvalue <- case rest of
                    [] -> pure Nothing
                    _ | has "-F" -> do
                        env <- setFormatEnv st mclient mtarget
                        Just <$> expandFormat st env (T.unwords rest)
                      | otherwise -> pure (Just (T.unwords rest))
                case mapM_ requireLive mdef of
                    Left e -> pure [RErr e]
                    Right ()
                        | Just d <- mdef, d.role == HookOption ->
                            pure [RErr (n <> ": set hooks with set-hook")]
                        | otherwise -> do
                            esc <- scopeFor st mclient mtarget sd flags mdef
                            case esc of
                                Left e -> orQuiet e
                                Right sc -> case requirePaneValues sc mdef of
                                    Left e -> pure [RErr e]
                                    Right () -> do
                                        when (has "-U") $
                                            clearPaneCopies st mclient mtarget sc n mkey
                                        writeOption st sc n
                                            (\own eff -> planSet mdef req spelled
                                                mkey own eff mvalue)

-- | Decide and write one option's entry atomically, refreshing the cached
-- global resolution; a changed @history-limit@ reaches open panes.
writeOption
    :: ServerState -> Scope -> Text
    -> (Maybe OptionValue -> Maybe OptionValue -> Either Text SetAction)
    -> IO [Reply]
writeOption st sc n decide = do
    result <- atomically $ do
        own <- ownValue st sc n
        inherited <- inheritedValue st sc n
        case decide own (maybe inherited Just own) of
            Left e -> pure (Left e)
            Right LeaveValue -> pure (Right ())
            Right act -> do
                modifyTVar' (deltaVar st sc) $ case act of
                    WriteValue v -> insertDelta n v
                    _ -> deleteDelta n
                refreshGlobalOptions st
                bumpDirty st
                pure (Right ())
    case result of
        Left e -> pure [RErr e]
        Right () -> do
            when (n == "history-limit") (applyHistoryLimit st)
            pure []

-- | @set -U@ at a window table: also drop the option (or its key) from
-- each of the target window's panes, so they fall back to inheritance.
clearPaneCopies
    :: ServerState -> Maybe Client -> Maybe Text -> Scope -> Text
    -> Maybe ArrayKey -> IO ()
clearPaneCopies st mclient mtarget sc n mkey = case sc of
    WindowTable win -> clear win
    GlobalWindowTable -> mapM_ clear =<< targetWindowScoped st mclient mtarget
    _ -> pure ()
  where
    clear win = atomically $ do
        ps <- readTVar win.panes
        forM_ (Map.elems ps) $ \p -> modifyTVar' p.options $ \d ->
            case (mkey, lookupDelta n d) of
                (Just k, Just (OVArray m)) -> insertDelta n (OVArray (Map.delete k m)) d
                (Just _, _) -> d
                (Nothing, _) -> deleteDelta n d
        bumpDirty st

-- | The window a @-w@ option scope acts on: a full window target
-- (@sess:ix@) via the cmd-find grammar, else the target session's current
-- window.
targetWindowScoped
    :: ServerState -> Maybe Client -> Maybe Text -> IO (Maybe Window)
targetWindowScoped st mclient mtarget = do
    res <- findTarget st mclient Target.FindWindow mtarget
    case res of
        Right (_, _, win, _) -> pure (Just win)
        Left _ -> targetCurrentWindow st mclient mtarget

-- | The environment a @set -F@ value (or a @show -F@ template) expands
-- against: the target pane's (the hook context's pane when a hook command
-- has no explicit target), degrading to the target session's.
setFormatEnv :: ServerState -> Maybe Client -> Maybe Text -> IO FormatEnv
setFormatEnv st mclient mtarget = do
    mamb <- ambientFor st
    let mambPane = case mclient of
            Just _ -> Nothing
            Nothing -> mamb >>= (.targetPane)
    menv <- case (mtarget, mambPane) of
        (Nothing, Just pid) -> paneEnvById st pid
        _ -> do
            res <- findTarget st mclient Target.FindPane mtarget
            case res of
                Right (sess, wix, win, pane) -> do
                    pix <- paneIndexOf st win pane
                    Just <$> paneFormatEnv st sess wix win pix pane
                Left _ -> pure Nothing
    case menv of
        Just env -> pure env
        Nothing -> do
            msess <- targetSession st mclient mtarget
            case msess of
                Just sess -> sessionFormatEnv st sess
                -- No session yet: user options still resolve as #{@foo}.
                Nothing -> (.user) <$> readTVarIO st.options

-- | Recompute the cached global option resolution. Runs in the same
-- transaction as every global-scope delta write so the cache never drifts.
refreshGlobalOptions :: ServerState -> STM ()
refreshGlobalOptions st = writeTVar st.options =<< resolveGlobal st

-- | The seconds the clock daemon sleeps between status redraws: the fastest
-- session-resolved @status-interval@ (global when no sessions); 'Nothing'
-- when every interval is 0 (periodic redraw disabled).
statusRefreshInterval :: ServerState -> STM (Maybe Int)
statusRefreshInterval st = do
    sessions <- Map.elems <$> readTVar st.sessions
    ivs <- if null sessions
        then (: []) . (.statusInterval) <$> resolveGlobal st
        else mapM (fmap (.statusInterval) . resolveForSession st) sessions
    pure $ case filter (> 0) ivs of
        [] -> Nothing
        positive -> Just (minimum positive)

-- | Push a changed @history-limit@ into every open pane's emulator so the new
-- cap governs existing panes' scrollback immediately, not just panes created
-- afterward. Each session resolves its own limit, so a session-scoped set
-- reaches only that session's panes.
applyHistoryLimit :: ServerState -> IO ()
applyHistoryLimit st = do
    sessMap <- readTVarIO st.sessions
    forM_ (Map.elems sessMap) $ \sess -> do
        limit <- (.historyLimit) <$> atomically (resolveForSession st sess)
        winMap <- readTVarIO sess.windows
        forM_ (Map.elems winMap) $ \win -> do
            paneMap <- readTVarIO win.panes
            forM_ (Map.elems paneMap) $ \pane ->
                Emu.setScrollbackLimit pane.emulator limit

-- Showing ---------------------------------------------------------------

-- | Whether a shown value is the table's own or inherited (@-A@'s @*@).
data Inheritance = OwnValue | InheritedValue
    deriving (Eq, Show)

-- | @show -v@: the value alone, or the name and the value.
data Display = ValueOnly | NameAndValue
    deriving (Eq, Show)

-- | An empty array shows its bare name: it has no value to print.
data ValuePresence = HasValue | NoValue
    deriving (Eq, Show)

-- | One @show-options@ line before rendering: the fields tmux's
-- @SHOW_OPTIONS_TEMPLATE@ (and a @-F@ template) reads.
data ShowLine = ShowLine
    { option :: Text
    , definition :: Maybe OptionDef  -- ^ 'Nothing' for a @\@user@ option
    , key :: Maybe ArrayKey
    , value :: Text
    , inheritance :: Inheritance
    , presence :: ValuePresence
    }

-- | tmux's default @show-options@ template: string values are quoted as
-- command arguments, everything else prints bare.
renderShowLine :: Display -> ShowLine -> Text
renderShowLine display l = case display of
    ValueOnly -> l.value
    NameAndValue -> l.option
        <> maybe "" (\k -> "[" <> arrayKeyText k <> "]") l.key
        <> (if l.inheritance == InheritedValue then "*" else "")
        <> case l.presence of
            NoValue -> ""
            HasValue -> " " <> if isString l then quoteArgsT l.value else l.value

isString :: ShowLine -> Bool
isString l = case (.kind) <$> l.definition of
    Nothing -> True
    Just KString -> True
    Just KStyle -> True
    Just KColourString -> True
    Just _ -> False

-- | The @option_*@ formats a @show -F@ template reads for one line.
showLineFormats :: Display -> ShowLine -> FormatEnv
showLineFormats display l = Map.fromList
    [ ("option_name", l.option)
    , ("option_value", l.value)
    , ("option_value_only", bit (display == ValueOnly))
    , ("option_is_parent", bit (l.inheritance == InheritedValue))
    , ("option_is_array", bit (maybe False (isJust . (.array)) l.definition))
    , ("option_is_string", bit (isString l))
    , ("option_is_hook", bit (fmap (.role) l.definition == Just HookOption))
    , ("option_is_user", bit (isNothing l.definition))
    , ("option_has_value", bit (l.presence == HasValue))
    , ("option_array_key", maybe "" arrayKeyText l.key)
    , ("option_has_array_key", bit (isJust l.key))
    ]
  where
    bit b = if b then "1" else "0"

-- | tmux's @cmd_show_options_print@: an array without a key yields one line
-- per entry, or a valueless line when empty.
optionLines
    :: Text -> Maybe OptionDef -> Maybe ArrayKey -> Inheritance
    -> OptionValue -> [ShowLine]
optionLines n md mkey inh v = case (v, mkey) of
    (OVArray m, Just k) -> [line (Just k) (Map.findWithDefault "" k m) HasValue]
    (OVArray m, Nothing)
        | Map.null m -> [line Nothing "" NoValue]
        | otherwise -> [ line (Just k) t HasValue | (k, t) <- Map.toAscList m ]
    (OVScalar t, _) -> [line Nothing t HasValue]
  where
    line k t p = ShowLine
        { option = n, definition = md, key = k, value = t
        , inheritance = inh, presence = p }

-- | Render lines through the default template (where @-v@ drops a
-- valueless line) or a @-F@ one.
renderLines
    :: ServerState -> Maybe Client -> Maybe Text -> Display -> Maybe Text
    -> [ShowLine] -> IO [Reply]
renderLines st mclient mtarget display mfmt ls = case mfmt of
    Nothing -> pure
        [ ROutput (renderShowLine display l)
        | l <- ls, display == NameAndValue || l.presence == HasValue ]
    Just fmt -> do
        base <- setFormatEnv st mclient mtarget
        forM ls $ \l ->
            ROutput <$> expandFormat st (Map.union (showLineFormats display l) base) fmt

-- | @show-options@: one option, or (with no name) the table's listing.
cmdShow :: CommandImpl
cmdShow st mclient args = do
    let (opts, flags, pos) = parseArgs "tF" args
        has f = f `elem` flags
        mtarget = lookup "-t" opts
        mfmt = lookup "-F" opts
        display = if has "-v" then ValueOnly else NameAndValue
        quiet = has "-q"
        orQuiet e = pure [RErr e | not quiet]
    case pos of
        [] -> do
            esc <- scopeFromFlags st mclient mtarget DefaultSession flags
            case esc of
                Left e -> orQuiet e
                Right sc -> do
                    ls <- showListing st sc (has "-A") (has "-H")
                    renderLines st mclient mtarget display mfmt ls
        [spelled] -> case matchOptionName spelled of
            Left (MatchError e) -> orQuiet e
            Right (n, mkey) -> do
                let mdef = lookupOptionDef n
                case mapM_ requireLive mdef of
                    Left e -> pure [RErr e]
                    Right () -> do
                        esc <- scopeFor st mclient mtarget DefaultSession flags mdef
                        case esc of
                            Left e -> orQuiet e
                            Right sc -> do
                                found <- lookupShown st sc mdef n (has "-A")
                                case found of
                                    Just (v, inh) -> renderLines st mclient mtarget
                                        display mfmt
                                        (optionLines n mdef mkey
                                            (emptyArrayOwn mkey v inh) v)
                                    Nothing
                                        | isJust mdef -> pure []
                                        | otherwise -> orQuiet ("invalid option: " <> spelled)
        _ -> pure [RErr "usage: show-options [-AgHpqsvw] [-F format] [-t target] [option]"]
  where
    -- A named empty array never shows the inherited marker.
    emptyArrayOwn mkey v inh = case (mkey, v) of
        (Nothing, OVArray m) | Map.null m -> OwnValue
        _ -> inh

-- | One option's value at a table — its own, or with @-A@ the inherited
-- one — and whether it was inherited.
lookupShown
    :: ServerState -> Scope -> Maybe OptionDef -> Text -> Bool
    -> IO (Maybe (OptionValue, Inheritance))
lookupShown st sc mdef n withParents = case mdef of
    Just d | d.role == HookOption -> do
        own <- hookValue st sc n
        pure ((\v -> (v, OwnValue)) <$> own)
    _ -> atomically $ do
        own <- ownValue st sc n
        case own of
            Just v -> pure (Just (v, OwnValue))
            Nothing
                | withParents -> fmap (\v -> (v, InheritedValue)) <$> inheritedValue st sc n
                | otherwise -> pure Nothing

-- | A hook's commands at a table, as the array tmux keeps them in. The
-- global tables hold every hook, so there an unbound one is an empty array.
hookValue :: ServerState -> Scope -> Text -> IO (Maybe OptionValue)
hookValue st sc n = case hookScopeOf sc of
    Nothing -> pure Nothing
    Just hs -> do
        entries <- hookEntriesAt st hs
        pure $ case (Map.lookup n entries, hs) of
            (Just e, _) -> Just (OVArray (Map.fromList
                (zip (map KeyIndex [0 ..]) e.commands)))
            (Nothing, HookGlobal) -> Just (OVArray Map.empty)
            _ -> Nothing

hookScopeOf :: Scope -> Maybe HookScope
hookScopeOf = \case
    ServerTable -> Nothing
    GlobalSessionTable -> Just HookGlobal
    GlobalWindowTable -> Just HookGlobal
    SessionTable s -> Just (HookSession s.id)
    WindowTable w -> Just (HookWindow w.id)
    PaneTable p -> Just (HookPane p.id)

-- | tmux's @cmd_show_options_all@: the table's @\@user@ options, then every
-- registered option that lists there, in registry order — own values, plus
-- inherited ones under @-A@; hooks only under @-H@. Options hat does not
-- implement are left out.
showListing :: ServerState -> Scope -> Bool -> Bool -> IO [ShowLine]
showListing st sc withParents withHooks = do
    own <- readTVarIO (deltaVar st sc)
    let users = concat
            [ optionLines n Nothing Nothing OwnValue v
            | (n, v) <- deltaEntries own, "@" `T.isPrefixOf` n ]
    rows <- forM (filter shown optionTable) $ \d -> do
        found <- lookupShown st sc (Just d) d.name withParents
        pure $ case found of
            Just (v, inh) -> optionLines d.name (Just d) Nothing inh v
            Nothing -> []
    pure (users <> concat rows)
  where
    shown d = isLive d && listsAt sc d.scope
        && (withHooks || d.role == PlainOption)

-- | The environment store a command's flags address: the global scope under
-- @-g@, else the target session's. Like tmux, @-g@ ignores any @-t@.
environScope
    :: ServerState -> Maybe Client -> Maybe Text -> [Text]
    -> IO (Either Text (TVar Environ))
environScope st mclient mtarget flags
    | "-g" `elem` flags = pure (Right st.globalEnviron)
    | otherwise = do
        msess <- targetSession st mclient mtarget
        pure $ case (msess, mtarget) of
            (Just sess, _)      -> Right sess.environ
            (Nothing, Just t)   -> Left ("no such session: " <> t)
            (Nothing, Nothing)  -> Left "no current session"

-- | tmux @set-environment@: write a variable at global (@-g@) or session
-- scope. @-u@ removes it, @-r@ clears it (masking inheritance), @-h@ hides
-- it, @-F@ expands the value as a format against the target session.
cmdSetEnvironment :: CommandImpl
cmdSetEnvironment st mclient args
    | any (`notElem` ["-F", "-h", "-g", "-r", "-u"]) flags = usage
    | (name : rest) <- pos, length rest <= 1 = run name (listToMaybe rest)
    | otherwise = usage
  where
    (opts, flags, pos) = parseArgs "t" args
    usage = pure
        [RErr "usage: set-environment [-Fhgru] [-t target-session] name [value]"]
    run name rawValue
        | T.null name = pure [RErr "empty variable name"]
        | "=" `T.isInfixOf` name = pure [RErr "variable name contains ="]
        | otherwise = do
            mvalue <- case rawValue of
                Just v | "-F" `elem` flags -> do
                    msess <- targetSession st mclient (lookup "-t" opts)
                    env <- maybe (pure Map.empty) (sessionFormatEnv st) msess
                    Just <$> expandFormat st env v
                _ -> pure rawValue
            escope <- environScope st mclient (lookup "-t" opts) flags
            case escope of
                Left err -> pure [RErr err]
                Right envVar
                    | "-u" `elem` flags ->
                        valueless mvalue "-u" (environUnset name) envVar
                    | "-r" `elem` flags ->
                        valueless mvalue "-r" (environClear name) envVar
                    | otherwise -> case mvalue of
                        Nothing -> pure [RErr "no value specified"]
                        Just v -> [] <$ atomically
                            (modifyTVar' envVar (environSet vis name v))
    vis = if "-h" `elem` flags then EnvHidden else EnvVisible
    valueless mvalue flagName f envVar = case mvalue of
        Just _ -> pure [RErr ("can't specify a value with " <> flagName)]
        Nothing -> [] <$ atomically (modifyTVar' envVar f)

-- | tmux @show-environment@: print variables at global or session scope,
-- cleared entries as @-NAME@. @-s@ emits shell re-export lines, @-h@ selects
-- hidden variables instead of plain ones, a name prints just that variable.
cmdShowEnvironment :: CommandImpl
cmdShowEnvironment st mclient args
    | any (`notElem` ["-h", "-g", "-s"]) flags = usage
    | length pos > 1 = usage
    | otherwise = do
        -- Like tmux, an explicit but unresolvable -t fails even with -g.
        badTarget <- case lookup "-t" opts of
            Nothing -> pure Nothing
            Just t -> do
                msess <- targetSession st mclient (Just t)
                pure $ maybe (Just ("no such session: " <> t))
                    (const Nothing) msess
        case badTarget of
            Just err -> pure [RErr err]
            Nothing -> do
                escope <- environScope st mclient (lookup "-t" opts) flags
                case escope of
                    Left err -> pure [RErr err]
                    Right envVar -> do
                        env <- readTVarIO envVar
                        pure $ case pos of
                            [name] -> case environFind name env of
                                Nothing ->
                                    [RErr ("unknown variable: " <> name)]
                                Just entry -> line (name, entry)
                            _ -> concatMap line (Map.toAscList env)
  where
    (opts, flags, pos) = parseArgs "t" args
    usage = pure
        [RErr "usage: show-environment [-hgs] [-t target-session] [name]"]
    wanted = if "-h" `elem` flags then EnvHidden else EnvVisible
    form = if "-s" `elem` flags then EnvShellExport else EnvPlain
    line (name, entry)
        | entry.visibility == wanted = [ROutput (renderEnvLine form name entry)]
        | otherwise = []
