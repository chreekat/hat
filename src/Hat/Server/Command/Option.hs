-- | The option and environment commands: @set-option@\/@set-window-option@,
-- @show-options@, and @set-environment@\/@show-environment@ — parsing an
-- option value ('setOption'), routing a write to its scope ('chooseScope'),
-- and the effects a changed option triggers.
module Hat.Server.Command.Option
    ( SetDefault (..)
    , SetScope (..)
    , SetMode (..)
    , chooseScope
    , setOption
    , listingLines
    , cmdSet
    , cmdShow
    , cmdSetEnvironment
    , cmdShowEnvironment
    , refreshGlobalOptions
    , statusRefreshInterval
    , applyHistoryLimit
    ) where

import Control.Concurrent.STM
import Control.Monad (forM_, when)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR

import Hat.Model
import Hat.Model.Options
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import Hat.Server.Environ
import Hat.Server.Keys
import Hat.Server.Format (FormatEnv)
import Hat.Server.FormatEnv (paneEnvById, paneFormatEnv)
import Hat.Server.HookTypes (HookAmbient (..))
import Hat.Server.Hooks (ambientFor)
import Hat.Server.Locate
    ( currentResolved, findTarget, locatePane, noSuchTarget, paneIndexOf
    , targetCurrentWindow, targetPaneScoped, targetSession )
import Hat.Server.Target qualified as Target
import Hat.Server.Style (parseColor, parseStyle)
import Hat.Server.FormatEnv (expandFormat, sessionFormatEnv)
import Hat.Term.Cell qualified as Cell
import Hat.Term.Emulator qualified as Emu

data SetDefault = DefaultSession | DefaultWindow
    deriving (Eq, Show)

-- | The overlay table a @set-option@ writes into. See 'chooseScope'.
data SetScope
    = SetServer
    | SetGlobalSession
    | SetGlobalWindow
    | SetLocalSession
    | SetLocalWindow
    | SetLocalPane
    deriving (Eq, Show)

-- | Route a set to its scope from the command's default, its flags, and the
-- option's class. @-g@ is lenient (it picks the session- or window-global
-- table by the option's class, never erroring, so a real @~/.tmux.conf@'s
-- @set -g mode-keys@ / @set -gw display-time@ both load). An /explicit/ local
-- window scope (@setw@ or @-w@ without @-g@) or @-s@ that contradicts the
-- option's class fails loud, matching tmux's @setw prefix@ rejection.
chooseScope :: SetDefault -> [Text] -> OptionName -> Either Text SetScope
chooseScope def flags name
    | hasS = SetServer <$ validateScope ServerOption name
    | hasG = Right $ case cls of
        WindowOption -> SetGlobalWindow
        ServerOption -> SetServer
        SessionOption -> SetGlobalSession
    | hasP = if allowedAtPane name
        then Right SetLocalPane
        else Left (optionNameText name <> " is not a pane option")
    | wantWindow = SetLocalWindow <$ validateScope WindowOption name
    | otherwise = Right $ case cls of
        WindowOption -> SetLocalWindow
        ServerOption -> SetServer
        SessionOption -> SetLocalSession
  where
    cls = optionScopeClass name
    hasS = "-s" `elem` flags
    hasG = "-g" `elem` flags
    hasP = "-p" `elem` flags
    wantWindow = "-w" `elem` flags || def == DefaultWindow

cmdSet :: SetDefault -> CommandImpl
cmdSet def st mclient args = do
    let (opts, flags, pos) = parseArgs "t" args
        mode = if "-a" `elem` flags then Append else Assign
        mtarget = lookup "-t" opts
        quiet = "-q" `elem` flags   -- tolerate unknown/ambiguous names
        unsetPanes = "-U" `elem` flags
        unset = "-u" `elem` flags || unsetPanes
    case pos of
        (nameT : rest) -> case resolveIndexedName nameT of
            Left err -> pure [RErr err | not quiet]
            Right (n, midx) -> case chooseScope def flags n of
                Left err -> pure [RErr err]
                Right scope -> do
                    etv <- scopeTargetVar st mclient mtarget scope
                    case etv of
                        Left err -> pure [RErr err]
                        Right deltaVar
                            | unset, (v : _) <- rest ->
                                pure [RErr ("value passed to unset option: " <> v)]
                            | unset, Just i <- midx -> do
                                curOpts <- currentResolved st mclient mtarget
                                case optionValueOf curOpts n of
                                    Just (OVIndexed m) -> writeScoped st
                                        deltaVar n (OVIndexed (Map.delete i m))
                                    _ -> pure [RErr (optionNameText n
                                        <> " is not an array option")]
                            | unset -> do
                                unsetScoped st deltaVar n
                                when unsetPanes $
                                    clearPaneCopies st mclient mtarget scope n
                                pure []
                            | otherwise -> do
                                curOpts <- currentResolved st mclient mtarget
                                value <- if "-F" `elem` flags
                                    then do
                                        env <- setFormatEnv st mclient mtarget
                                        expandFormat st env (T.unwords rest)
                                    else pure (T.unwords rest)
                                case setOptionEntry mode curOpts
                                        (spelled n midx) value of
                                    Left err -> pure [RErr err]
                                    Right (n', v) -> writeScoped st deltaVar n' v
        [] -> pure [RErr "usage: set [-gsw] [-t target] option value"]
  where
    spelled n midx = optionNameText n
        <> maybe "" (\i -> "[" <> T.pack (show i) <> "]") midx

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

-- | The environment a @set -F@ value expands against: the target pane's
-- (the hook context's pane when a hook command has no explicit target),
-- degrading to the target session's.
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


-- | Insert a resolved entry into its scope's overlay, refresh the cached
-- global resolution ('ServerState.options'), and push a changed
-- @history-limit@ into each session's open panes.
writeScoped
    :: ServerState -> TVar OptionsDelta -> OptionName -> OptionValue
    -> IO [Reply]
writeScoped st deltaVar n v = do
    atomically $ do
        modifyTVar' deltaVar (insertDelta n v)
        refreshGlobalOptions st
        bumpDirty st
    when (n == OptHistoryLimit) (applyHistoryLimit st)
    pure []

-- | @set -u@: drop a scope's own entry so the parent scopes (or the compiled
-- default) show through, with the same cache refresh as 'writeScoped'.
unsetScoped :: ServerState -> TVar OptionsDelta -> OptionName -> IO ()
unsetScoped st deltaVar n = do
    atomically $ do
        modifyTVar' deltaVar (deleteDelta n)
        refreshGlobalOptions st
        bumpDirty st
    when (n == OptHistoryLimit) (applyHistoryLimit st)

-- | @set -U@ on a window scope: additionally clear each pane's own copy, so
-- every pane in the window (or everywhere, under @-g@) falls back to
-- inheritance.
clearPaneCopies
    :: ServerState -> Maybe Client -> Maybe Text -> SetScope -> OptionName
    -> IO ()
clearPaneCopies st mclient mtarget scope n = case scope of
    SetLocalWindow -> do
        mwin <- targetWindowScoped st mclient mtarget
        forM_ mwin $ \win -> atomically $ do
            clearWindow win
            bumpDirty st
    SetGlobalWindow -> atomically $ do
        sessions <- readTVar st.sessions
        forM_ (Map.elems sessions) $ \sess -> do
            ws <- readTVar sess.windows
            mapM_ clearWindow (Map.elems ws)
        bumpDirty st
    _ -> pure ()
  where
    clearWindow win = do
        ps <- readTVar win.panes
        forM_ (Map.elems ps) $ \p -> modifyTVar' p.options (deleteDelta n)

-- | The overlay table a scope writes into, resolving the target session,
-- its current window, or a pane; a missing target fails loud, naming the
-- target and the scope's kind (upstream options-scope.sh).
scopeTargetVar
    :: ServerState -> Maybe Client -> Maybe Text -> SetScope
    -> IO (Either Text (TVar OptionsDelta))
scopeTargetVar st mclient mtarget = \case
    SetServer -> pure (Right st.serverOptions)
    SetGlobalSession -> pure (Right st.globalSessionOptions)
    SetGlobalWindow -> pure (Right st.globalWindowOptions)
    SetLocalSession -> do
        msess <- targetSession st mclient mtarget
        pure (orNoSuch "session" ((.options) <$> msess))
    SetLocalWindow -> do
        mwin <- targetWindowScoped st mclient mtarget
        pure (orNoSuch "window" ((.options) <$> mwin))
    SetLocalPane -> do
        mpane <- targetPaneScoped st mclient mtarget
        pure (orNoSuch "pane" ((.options) <$> mpane))
  where
    orNoSuch kind = maybe (Left (noSuchTarget kind mtarget)) Right

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

-- | @show-options@. tmux semantics: a plain read answers from the target
-- scope's own table only — no parent walk — so an option unset at that scope
-- prints nothing (success); @-A@ walks the parents and marks an inherited
-- value with a trailing @*@. The global scopes answer from the resolved
-- global chain, which includes the compiled defaults. Without a name, the
-- scope's own entries are listed (@-H@ adds the hook table, which hat keeps
-- empty).
cmdShow :: CommandImpl
cmdShow st mclient args = do
    let (opts, flags, pos) = parseArgs "t" args
        mtarget = lookup "-t" opts
        valueOnly = "-v" `elem` flags
        quiet = "-q" `elem` flags
        withParents = "-A" `elem` flags
    case pos of
        [] -> showListing st mclient mtarget flags
        [nameT] -> case resolveIndexedName nameT of
            Left err -> pure [RErr err | not quiet]
            Right (n, midx) -> case chooseScope DefaultSession flags n of
                Left err -> pure [RErr err]
                Right scope -> do
                    ev <- showScopeValue st mclient mtarget withParents scope n
                    pure $ case ev of
                        Left err -> [RErr err]
                        Right Nothing -> []
                        Right (Just (v, inherited)) ->
                            let render nm t = ROutput $ if valueOnly
                                    then t
                                    else nm <> (if inherited then "*" else "")
                                        <> " " <> quoteIfNeeded t
                            in case (v, midx) of
                                (OVIndexed m, Just i) ->
                                    [ render (indexedName n i) t
                                    | Just t <- [Map.lookup i m] ]
                                (OVIndexed m, Nothing) ->
                                    [ render (indexedName n i) t
                                    | (i, t) <- Map.toAscList m ]
                                _ -> [render (optionNameText n)
                                        (formatOptionValue v)]
        _ -> pure [RErr "usage: show-options [-AgHpqsvw] [-t target] [option]"]

-- | One option's value at one scope: 'Nothing' when the scope does not set
-- it (or, with the parent walk on, when nothing in the chain does); the
-- 'Bool' is the @-A@ inherited marker. See 'cmdShow'.
showScopeValue
    :: ServerState -> Maybe Client -> Maybe Text -> Bool -> SetScope
    -> OptionName -> IO (Either Text (Maybe (OptionValue, Bool)))
showScopeValue st mclient mtarget withParents scope n = case scope of
    SetServer -> globalAnswer
    SetGlobalSession -> globalAnswer
    SetGlobalWindow -> globalAnswer
    SetLocalSession -> do
        msess <- targetSession st mclient mtarget
        case msess of
            Nothing -> pure (Left (noSuchTarget "session" mtarget))
            Just sess -> ownOr sess.options (resolveGlobal st)
    SetLocalWindow -> do
        mwin <- targetWindowScoped st mclient mtarget
        case mwin of
            Nothing -> pure (Left (noSuchTarget "window" mtarget))
            Just win -> ownOr win.options (resolveGlobal st)
    SetLocalPane -> do
        mpane <- targetPaneScoped st mclient mtarget
        case mpane of
            Nothing -> pure (Left (noSuchTarget "pane" mtarget))
            Just pane -> do
                mloc <- atomically (locatePane st pane.id)
                let parentChain = case mloc of
                        Just (sid, win) -> do
                            msess <- Map.lookup sid <$> readTVar st.sessions
                            case msess of
                                Just sess -> resolveForWindow st sess win
                                Nothing -> resolveGlobal st
                        Nothing -> resolveGlobal st
                ownOr pane.options parentChain
  where
    globalAnswer = do
        resolved <- atomically (resolveGlobal st)
        pure (Right (asOwn <$> optionValueOf resolved n))
    ownOr deltaVar parentChain = do
        own <- readTVarIO deltaVar
        case lookupDelta n own of
            Just v -> pure (Right (Just (asOwn v)))
            Nothing
                | withParents -> do
                    resolved <- atomically parentChain
                    pure (Right (asInherited <$> optionValueOf resolved n))
                | otherwise -> pure (Right Nothing)
    asOwn v = (v, False)
    asInherited v = (v, True)

-- | The no-name form of @show-options@: list the scope's own table (plus,
-- under @-A@, the parent tables' unshadowed entries). See 'cmdShow'.
showListing :: ServerState -> Maybe Client -> Maybe Text -> [Text] -> IO [Reply]
showListing st mclient mtarget flags = do
    etables <- scopeTables
    case etables of
        Left err -> pure [RErr err]
        Right (own, parents) -> do
            let ls = listingLines valueOnly own
                    (if withParents then parents else [])
                hooks = if withHooks && hasG && not (hasW || hasS)
                    then hookNames else []
            pure (map ROutput (List.sort (ls <> hooks)))
  where
    hasS = "-s" `elem` flags
    hasG = "-g" `elem` flags
    hasW = "-w" `elem` flags
    hasP = "-p" `elem` flags
    valueOnly = "-v" `elem` flags
    withParents = "-A" `elem` flags
    withHooks = "-H" `elem` flags
    noParents d = Right (d, [])
    scopeTables
        | hasS = noParents <$> readTVarIO st.serverOptions
        | hasG && hasW = noParents <$> readTVarIO st.globalWindowOptions
        | hasG = noParents <$> readTVarIO st.globalSessionOptions
        | hasP = do
            mpane <- targetPaneScoped st mclient mtarget
            case mpane of
                Nothing -> pure (Left (noSuchTarget "pane" mtarget))
                Just pane -> do
                    own <- readTVarIO pane.options
                    mloc <- atomically (locatePane st pane.id)
                    gw <- readTVarIO st.globalWindowOptions
                    parents <- case mloc of
                        Just (_, win) -> do
                            w <- readTVarIO win.options
                            pure [w, gw]
                        Nothing -> pure [gw]
                    pure (Right (own, parents))
        | hasW = do
            mwin <- targetWindowScoped st mclient mtarget
            case mwin of
                Nothing -> pure (Left (noSuchTarget "window" mtarget))
                Just win -> do
                    own <- readTVarIO win.options
                    gw <- readTVarIO st.globalWindowOptions
                    pure (Right (own, [gw]))
        | otherwise = do
            msess <- targetSession st mclient mtarget
            case msess of
                Nothing -> pure (Left (noSuchTarget "session" mtarget))
                Just sess -> do
                    own <- readTVarIO sess.options
                    gs <- readTVarIO st.globalSessionOptions
                    pure (Right (own, [gs]))

-- | Format a scope's entries plus unshadowed parent entries (marked @*@),
-- name-ordered, as @show-options@ prints them.
listingLines :: Bool -> OptionsDelta -> [OptionsDelta] -> [Text]
listingLines valueOnly own parents = concatMap lines' (List.sortOn name entries)
  where
    parentMerged = mergeDeltas parents
    entries = deltaEntries own <>
        [ e | e@(n, _) <- deltaEntries parentMerged
        , Nothing <- [lookupDelta n own] ]
    name (n, _) = optionNameText n
    inherited (n, _) = deltaMember n parentMerged && not (deltaMember n own)
    lines' e@(n, v) = case v of
        -- An array prints one line per entry, index in the name.
        OVIndexed m ->
            [ line e (indexedName n i) t | (i, t) <- Map.toAscList m ]
        _ -> [ line e (optionNameText n) (formatOptionValue v) ]
    line e nm v
        | valueOnly = v
        | otherwise = nm <> (if inherited e then "*" else "")
            <> " " <> quoteIfNeeded v

-- | @show-options@'s spelling of one array entry: @command-alias[0]@.
indexedName :: OptionName -> Int -> Text
indexedName n i = optionNameText n <> "[" <> T.pack (show i) <> "]"

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

-- | Whether a @set-option@ replaces the option or concatenates onto it.
data SetMode
    = Assign  -- ^ replace the current value outright
    | Append  -- ^ tmux's @-a@: append onto a string option's current value
    deriving (Eq)

-- | Apply a @set-option@. For string-valued options 'Append' concatenates
-- onto the current value (used to build up @status-right@ across several
-- lines). Unknown non-@\@@ options are rejected so a config never looks
-- supported when its behavior is not yet implemented.
setOption :: SetMode -> Options -> Text -> Text -> Either Text Options
setOption mode opts name value = do
    (n, v) <- setOptionEntry mode opts name value
    pure (applyEntry n v opts)

-- | Parse and validate a @set-option@ into the single scoped entry it writes.
-- For string-valued options 'Append' concatenates onto the current value (used
-- to build up @status-right@ across several lines). Unknown non-@\@@ options
-- are rejected so a config never looks supported when its behavior is not yet
-- implemented.
setOptionEntry
    :: SetMode -> Options -> Text -> Text
    -> Either Text (OptionName, OptionValue)
setOptionEntry mode opts spelledName value = do
    (name, midx) <- splitArrayIndex spelledName
    case midx of
        Just i -> indexedOptionEntry mode opts name i value
        Nothing -> scalarOptionEntry mode opts name value

-- | A single-index array set: @command-alias[100] zoom=…@ replaces (or, with
-- @-a@, extends) just that entry of the current array.
indexedOptionEntry
    :: SetMode -> Options -> Text -> Int -> Text
    -> Either Text (OptionName, OptionValue)
indexedOptionEntry mode opts name i value = case name of
    "command-alias" -> Right
        (OptCommandAlias, OVIndexed (Map.alter place i opts.commandAlias))
    _ -> Left (name <> " is not an array option")
  where
    place old = Just $ case (mode, old) of
        (Append, Just prev) -> prev <> value
        _ -> value

-- | A whole-array set: values split on tmux's @,@ separator; an append
-- continues after the highest existing index.
arrayEntries :: SetMode -> Map Int Text -> Text -> Map Int Text
arrayEntries mode current value = case mode of
    Assign -> Map.fromList (zip [0 ..] items)
    Append -> Map.union current (Map.fromList (zip [next ..] items))
  where
    items = if T.null value then [] else T.splitOn "," value
    next = maybe 0 ((+ 1) . fst) (Map.lookupMax current)

scalarOptionEntry
    :: SetMode -> Options -> Text -> Text
    -> Either Text (OptionName, OptionValue)
scalarOptionEntry mode opts name value = case name of
    "command-alias" ->
        Right (OptCommandAlias,
            OVIndexed (arrayEntries mode opts.commandAlias value))
    "prefix" -> case parseKeyName value of
        Just k -> Right (OptPrefix, OVText k.name)
        Nothing -> Left ("bad prefix key: " <> value)
    "base-index" -> withInt OptBaseIndex
    "pane-base-index" -> withInt OptPaneBaseIndex
    "history-limit" -> withInt OptHistoryLimit
    "default-terminal" -> Right (OptDefaultTerminal, OVText value)
    "word-separators" -> Right (OptWordSeparators, OVText value)
    "status" -> case value of
        "off" -> Right (OptStatus, OVStatusLines 0)
        "on"  -> Right (OptStatus, OVStatusLines 1)
        -- tmux accepts 2..5 too, but each extra line needs its own
        -- status-format[i] (pane/session lists via #{P:}/#{S:}), which hat
        -- does not model yet; reject loudly rather than draw a blank bar.
        _ | value `elem` ["2", "3", "4", "5"] ->
                Left "status: multi-line status (2-5) not yet supported"
          | otherwise -> Left "status: off, on, or 2-5"
    "status-position" -> case value of
        "top" -> Right (OptStatusPosition, OVStatusPosition StatusTop)
        "bottom" -> Right (OptStatusPosition, OVStatusPosition StatusBottom)
        _ -> Left "status-position: top or bottom"
    "mode-keys" -> case value of
        "vi" -> Right (OptModeKeys, OVModeKeys KeysVi)
        "emacs" -> Right (OptModeKeys, OVModeKeys KeysEmacs)
        _ -> Left "mode-keys: vi or emacs"
    "status-left" ->
        Right (OptStatusLeft, OVText (withAppend opts.statusLeft))
    "status-left-length" -> withInt OptStatusLeftLength
    "status-right" ->
        Right (OptStatusRight, OVText (withAppend opts.statusRight))
    "status-right-length" -> withInt OptStatusRightLength
    "status-interval" -> withInt OptStatusInterval
    "window-status-format" ->
        Right (OptWindowStatusFormat,
            OVText (withAppend opts.windowStatusFormat))
    "window-status-current-format" ->
        Right (OptWindowStatusCurrentFormat,
            OVText (withAppend opts.windowStatusCurrentFormat))
    "status-style" -> Right (OptStatusStyle, OVStyle (parseStyle value))
    "window-status-style" ->
        Right (OptWindowStatusStyle, OVStyle (parseStyle value))
    "window-status-current-style" ->
        Right (OptWindowStatusCurrentStyle, OVStyle (parseStyle value))
    "window-status-bell-style" ->
        Right (OptWindowStatusBellStyle, OVStyle (parseStyle value))
    "pane-border-style" ->
        Right (OptPaneBorderStyle, OVStyle (parseStyle value))
    "pane-active-border-style" ->
        Right (OptPaneActiveBorderStyle, OVStyle (parseStyle value))
    "mode-style" -> Right (OptModeStyle, OVStyle (parseStyle value))
    "cursor-colour"
        -- "default"/"" clear it; anything else must be a colour parseColor
        -- recognizes (it answers DefaultColor only for those two and junk).
        | T.null value || value == "default"
            || parseColor value /= Cell.DefaultColor ->
                Right (OptCursorColour, OVText value)
        | otherwise -> Left ("bad colour: " <> value)
    "pane-border-lines" -> case value of
        "single" -> Right (OptPaneBorderLines, OVBorderLines BorderSingle)
        "heavy"  -> Right (OptPaneBorderLines, OVBorderLines BorderHeavy)
        "double" -> Right (OptPaneBorderLines, OVBorderLines BorderDouble)
        "simple" -> Right (OptPaneBorderLines, OVBorderLines BorderSimple)
        _ -> Left "pane-border-lines: single, heavy, double, or simple"
    "pane-border-indicators" -> case value of
        "off"    -> Right (OptPaneBorderIndicators, OVBorderIndicators IndicatorsOff)
        "colour" -> Right (OptPaneBorderIndicators, OVBorderIndicators IndicatorsColour)
        "color"  -> Right (OptPaneBorderIndicators, OVBorderIndicators IndicatorsColour)
        "arrows" -> Right (OptPaneBorderIndicators, OVBorderIndicators IndicatorsArrows)
        "both"   -> Right (OptPaneBorderIndicators, OVBorderIndicators IndicatorsBoth)
        _ -> Left "pane-border-indicators: off, colour, arrows, or both"
    "set-titles" -> withOnOff OptSetTitles
    "escape-time" -> withInt OptEscapeTime
    "display-time" -> withInt OptDisplayTime
    "focus-events" -> withOnOff OptFocusEvents
    "aggressive-resize" -> withOnOff OptAggressiveResize
    "monitor-activity" -> withOnOff OptMonitorActivity
    "monitor-bell" -> withOnOff OptMonitorBell
    "monitor-silence" -> withInt OptMonitorSilence
    "bell-action" -> case value of
        "any" -> Right (OptBellAction, OVAlertAction AlertAny)
        "none" -> Right (OptBellAction, OVAlertAction AlertNone)
        "current" -> Right (OptBellAction, OVAlertAction AlertCurrent)
        "other" -> Right (OptBellAction, OVAlertAction AlertOther)
        _ -> Left "bell-action: any, none, current, or other"
    "automatic-rename" -> withOnOff OptAutomaticRename
    "remain-on-exit" -> withOnOff OptRemainOnExit
    "automatic-rename-format" -> Right (OptAutomaticRenameFormat, OVText value)
    "update-environment" ->
        Right (OptUpdateEnvironment, OVTextList (T.words value))
    "main-pane-width" -> withInt OptMainPaneWidth
    "main-pane-height" -> withInt OptMainPaneHeight
    _
        | "@" `T.isPrefixOf` name -> Right (OptUser name, OVText value)
        | otherwise -> Left ("invalid option: " <> name)
  where
    withInt n = case TR.decimal value of
        Right (m, restT) | T.null restT -> Right (n, OVInt m)
        _ -> Left (name <> ": not a number: " <> value)
    withOnOff n = case value of
        "on"  -> Right (n, OVBool True)
        "off" -> Right (n, OVBool False)
        _ -> Left (name <> ": on or off")
    withAppend old = case mode of
        Append -> old <> value
        Assign -> value
