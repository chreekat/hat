-- | @set-hook@ and @show-hooks@: binding hook command lists at a scope,
-- firing them by hand (@-R@), firing user events (@-E@), and listing what
-- is bound. The engine they drive lives in "Hat.Server.Hooks".
module Hat.Server.Command.Hook
    ( cmdSetHook
    , cmdShowHooks
    , resolveHookTarget
    ) where

import Control.Concurrent.STM (readTVarIO)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (POSIXTime)

import Hat.Model
import Hat.Model.Options (quoteIfNeeded)
import Hat.Server.Command.Types (CommandImpl, Reply (..), parseArgs)
import Hat.Server.HookMonitor
    (addMonitor, monitorTargetText, monitorsAt, parseMonitorSpec, removeMonitor)
import Hat.Server.HookTypes
import Hat.Server.Hooks
import Hat.Server.Locate (findTarget, targetPaneScoped, targetSession)
import Hat.Server.Target qualified as Target
import Hat.Server.FormatEnv (expandFormat, sessionFormatEnv)

-- | Resolve @-t@ to the notify-style target the flags select, pane-deep
-- when possible (tmux's @CMD_FIND_PANE@ with @CANFAIL@).
resolveHookTarget
    :: ServerState -> Maybe Client -> Maybe Text -> IO NotifyTarget
resolveHookTarget st mclient mtarget = do
    res <- findTarget st mclient Target.FindPane mtarget
    case res of
        Right (sess, _, win, pane) -> pure NotifyTarget
            { session = Just sess.id
            , window = Just win.id
            , pane = Just pane.id }
        Left _ -> pure noTarget

-- | The hook table the flags address: global under @-g@, else the target
-- pane (@-p@), window (@-w@), or session.
hookScopeOf
    :: ServerState -> Maybe Client -> [Text] -> Maybe Text
    -> IO (Either Text HookScope)
hookScopeOf st mclient flags mtarget
    | "-g" `elem` flags = pure (Right HookGlobal)
    | "-p" `elem` flags = do
        mpane <- targetPaneScoped st mclient mtarget
        pure $ maybe (Left "no such pane") (Right . HookPane . (.id)) mpane
    | "-w" `elem` flags = do
        res <- findTarget st mclient Target.FindWindow mtarget
        pure $ case res of
            Right (_, _, win, _) -> Right (HookWindow win.id)
            Left e -> Left e
    | otherwise = do
        msess <- targetSession st mclient mtarget
        pure $ maybe (Left "no such session") (Right . HookSession . (.id)) msess

cmdSetHook :: CommandImpl
cmdSetHook st mclient args = do
    let (opts, flags, pos) = parseArgs "tB" args
        mtarget = lookup "-t" opts
    case (lookup "-B" opts, flags, pos) of
        (Just spec, _, _) -> monitorCmd st mclient mtarget flags pos spec
        (_, _, [name]) | "-E" `elem` flags -> fireEventCmd st mclient mtarget name
        (_, _, [name]) | "-R" `elem` flags -> do
            tgt <- resolveHookTarget st mclient mtarget
            [] <$ runHookNow st name tgt
        (_, _, [name]) | "-u" `elem` flags -> withValidName st name $ do
            escope <- hookScopeOf st mclient flags mtarget
            case escope of
                Left e -> pure [RErr e]
                Right scope -> [] <$ unsetHook st scope name
        (_, _, [name, cmd]) -> withValidName st name $ do
            escope <- hookScopeOf st mclient flags mtarget
            case escope of
                Left e -> pure [RErr e]
                Right scope ->
                    [] <$ setHook st scope name ("-a" `elem` flags) cmd
        (_, _, [_]) -> pure [RErr "set-hook: empty value"]
        _ -> pure [RErr "usage: set-hook [-agpRuw] [-t target] hook [command]"]

withValidName :: ServerState -> Text -> IO [Reply] -> IO [Reply]
withValidName st name act = do
    ok <- validSetHookName st name
    if ok then act else pure [RErr ("invalid option: " <> name)]

-- | @set-hook -B@: register (or with @-u@ remove) a format monitor; a
-- command argument also binds the hook the monitor fires at this scope.
monitorCmd
    :: ServerState -> Maybe Client -> Maybe Text -> [Text] -> [Text] -> Text
    -> IO [Reply]
monitorCmd st mclient mtarget flags pos spec
    | "-u" `elem` flags = do
        let name = case parseMonitorSpec spec of
                Right (n, _, _) -> n
                Left _ -> spec
        withScope $ \scope -> [] <$ removeMonitor st scope name
    | otherwise = case parseMonitorSpec spec of
        Left e -> pure [RErr e]
        Right (name, target, fmt)
            | not ("@" `T.isPrefixOf` name) ->
                pure [RErr "monitor hook name must start with @"]
            | otherwise -> withScope $ \scope -> do
                case pos of
                    [cmd] -> setHook st scope name False cmd
                    [] -> pure ()
                    _ -> pure ()
                msid <- if "-g" `elem` flags
                    then pure Nothing
                    else (.session) <$> resolveHookTarget st mclient mtarget
                addMonitor st scope name target fmt msid
                pure []
  where
    withScope body = do
        escope <- hookScopeOf st mclient flags mtarget
        either (pure . (: []) . RErr) body escope

-- | @set-hook -E@: fire a user event carrying the target as its payload.
fireEventCmd :: ServerState -> Maybe Client -> Maybe Text -> Text -> IO [Reply]
fireEventCmd st mclient mtarget name
    | not ("@" `T.isPrefixOf` name) =
        pure [RErr "event name must start with @"]
    | otherwise = do
        res <- findTarget st mclient Target.FindPane mtarget
        (tgt, payload) <- case res of
            Right (sess, wix, win, pane) -> do
                sname <- readTVarIO sess.name
                wname <- readTVarIO win.name
                pure ( NotifyTarget (Just sess.id) (Just win.id) (Just pane.id)
                     , [ ("session", PSessionRef sess.id sname)
                       , ("window", PWindowRef win.id wname)
                       , ("window_index", PInt wix)
                       , ("pane", PPaneRef pane.id) ] )
            Left _ -> pure (noTarget, [])
        let withClient = case mclient of
                Just c -> ("client", PClientRef (clientNameOf (Just c))) : payload
                Nothing -> payload
        [] <$ fireUserEvent st name tgt withClient

cmdShowHooks :: CommandImpl
cmdShowHooks st mclient args = do
    let (opts, flags, pos) = parseArgs "tF" args
        mtarget = lookup "-t" opts
        mfmt = lookup "-F" opts
    if "-B" `elem` flags
        then do
            escope <- hookScopeOf st mclient flags mtarget
            case escope of
                Left e -> pure [RErr e]
                Right scope -> do
                    mons0 <- monitorsAt st scope
                    let mons = case pos of
                            [name] -> [ m | m@(n, _) <- mons0, n == name ]
                            _ -> mons0
                    concat <$> mapM (printMonitor st mclient mtarget mfmt) mons
        else do
            escope <- hookScopeOf st mclient flags mtarget
            case escope of
                Left e -> pure [RErr e]
                Right scope -> do
                    entries <- hookEntriesAt st scope
                    case pos of
                        [name] -> case Map.lookup name entries of
                            Nothing -> pure []
                            Just e -> printEntry st mclient mtarget mfmt name e
                        [] -> fmap concat . mapM
                            (\(n, e) -> printEntry st mclient mtarget mfmt n e)
                            $ Map.toAscList entries
                        _ -> pure [RErr "usage: show-hooks [-Bgpw] [-F format] [-t target] [hook]"]

-- A monitor prints as its spec (@name:target:format@), or through @-F@
-- with the monitor formats.
printMonitor
    :: ServerState -> Maybe Client -> Maybe Text -> Maybe Text
    -> (Text, Monitor) -> IO [Reply]
printMonitor st mclient mtarget mfmt (name, mon) = do
    let targetText = monitorTargetText mon.target
        specText = name <> ":" <> targetText <> ":" <> mon.format
    case mfmt of
        Nothing -> pure [ROutput specText]
        Just fmt -> do
            count <- readTVarIO mon.fireCount
            mtime <- readTVarIO mon.fireTime
            msess <- targetSession st mclient mtarget
            base <- maybe (pure Map.empty) (sessionFormatEnv st) msess
            let env = Map.union (Map.fromList $
                    [ ("option_name", name)
                    , ("option_value", specText)
                    , ("option_is_hook", "1")
                    , ("option_is_user", "1")
                    , ("hook_monitor_target", targetText)
                    , ("hook_monitor_format", mon.format)
                    , ("hook_fire_count", tshow count)
                    ] <> [ ("hook_fire_time", epochText t)
                         | Just t <- [mtime] ]) base
            out <- expandFormat st env fmt
            pure [ROutput out]

-- One line per bound command: user hooks print bare (@name value@), built-in
-- hooks print each array item (@name[i] command@); @-F@ substitutes the
-- caller's template with the per-hook formats.
printEntry
    :: ServerState -> Maybe Client -> Maybe Text -> Maybe Text -> Text
    -> HookEntry -> IO [Reply]
printEntry st mclient mtarget mfmt name e = case mfmt of
    Nothing -> pure (map ROutput defaultLines)
    Just fmt -> do
        msess <- targetSession st mclient mtarget
        base <- maybe (pure Map.empty) (sessionFormatEnv st) msess
        mapM (\perItem -> ROutput <$>
                expandFormat st (Map.union perItem base) fmt)
            (perItemEnvs name e)
  where
    defaultLines
        | "@" `T.isPrefixOf` name =
            [ name <> " " <> quoteIfNeeded cmd | cmd <- e.commands ]
        | otherwise =
            [ name <> "[" <> tshow i <> "] " <> cmd
            | (i, cmd) <- zip [0 :: Int ..] e.commands ]

-- The formats @show-hooks -F@ exposes, one map per array item.
perItemEnvs :: Text -> HookEntry -> [Map.Map Text Text]
perItemEnvs name e =
    [ Map.fromList $
        [ ("option_name", name)
        , ("option_value", cmd)
        , ("option_is_hook", "1")
        , ("option_is_user", if "@" `T.isPrefixOf` name then "1" else "0")
        , ("option_is_array", if userHook then "0" else "1")
        , ("option_array_key", if userHook then "" else tshow i)
        , ("option_has_array_key", if userHook then "0" else "1")
        , ("hook_fire_count", tshow e.fireCount)
        ] <> [ ("hook_fire_time", epochText t) | Just t <- [e.fireTime] ]
    | (i, cmd) <- zip [0 :: Int ..] e.commands ]
  where
    userHook = "@" `T.isPrefixOf` name

epochText :: POSIXTime -> Text
epochText t = tshow (floor t :: Integer)
