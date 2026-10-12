-- | @list-keys@ (tmux's @cmd-list-keys.c@), and the printing of a key
-- binding's commands the way tmux's @cmd_list_print@ does.
module Hat.Server.Command.ListKeys
    ( cmdListKeys
    , printCommands
    , keyCode
    ) where

import Control.Concurrent.STM (atomically, readTVar, readTVarIO)
import Data.Bits (shiftL, (.|.))
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR

import Hat.Model
import Hat.Model.Options (Options (..))
import Hat.Server.Command.Types (CommandImpl, Reply (..))
import Hat.Server.Format (quoteArgsT)
import Hat.Server.FormatEnv (clientContextFormatEnv, expandFormat)
import Hat.Server.Keys (Binding (..), Key (..), Repeat (..), parseKeyName)
import Hat.Server.Toast (showToast)
import Hat.TextWidth (textWidth)

-- | tmux's @LIST_KEYS_TEMPLATE@.
listKeysTemplate :: Text
listKeysTemplate = T.concat
    [ "#{?notes_only,"
    , "#{key_prefix} "
    , "#{p|#{key_string_width}:key_string} "
    , "#{?key_note,#{key_note},#{key_command}}"
    , ","
    , "bind-key #{?key_has_repeat,#{?key_repeat,-r,  },} "
    , "-T #{p|#{key_table_width}:key_table} "
    , "#{p|#{key_string_width}:#{q|a:key_string}} "
    , "#{key_command}}" ]

-- | One binding as @list-keys@ shows it.
data Listed = Listed
    { table :: Text
    , key :: Text
    , commands :: [[Text]]
    , repeats :: Bool  -- ^ bound with @-r@
    }

-- | The order @-O@ asks for.
data Order = KeyTreeOrder | ByKeyCode | ByModifiers
    deriving (Eq, Show)

-- | @list-keys [-1aNr] [-F format] [-O order] [-P prefix-string]
-- [-T key-table] [key]@: the bindings in tables by name and keys by key
-- code (or as @-O@ sorts), expanded through @-F@ or tmux's own template.
cmdListKeys :: CommandImpl
cmdListKeys st mclient args = case parseTmuxArgs "1aF:NO:P:rT:" args of
    Left e -> pure [RErr ("command list-keys: " <> e)]
    Right (entries, pos) -> listKeys st mclient entries pos

listKeys :: ServerState -> Maybe Client -> [(Char, Entry)] -> [Text] -> IO [Reply]
listKeys st mclient entries pos
    | length pos > 1 =
        pure [RErr "command list-keys: too many arguments (need at most 1)"]
    | has 'N' = pure [RErr "list-keys -N: key notes are not implemented"]
    | Just k <- only, isNothing (parseKeyName k) = pure [RErr ("invalid key: " <> k)]
    | otherwise = case order of
        Left e -> pure [RErr e]
        Right ord -> do
            (keymap, gopts) <- atomically $ (,) <$> readTVar st.keymap <*> resolveGlobal st
            case value 'T' of
                Just t | not (Map.member t keymap) ->
                    pure [RErr ("table " <> t <> " doesn't exist")]
                mtable -> do
                    let tables = maybe (Map.toList keymap)
                            (\t -> [(t, Map.findWithDefault Map.empty t keymap)]) mtable
                        all' = sortListed ord
                            [ listed t k b | (t, binds) <- tables, (k, b) <- inKeyOrder binds ]
                        wanted = case parseKeyName =<< only of
                            Just k -> filter (\l -> l.key == k.name) all'
                            Nothing -> all'
                        prefix = fromMaybe (promptPrefix gopts.prefix) (value 'P')
                    case only of
                        Just k | null wanted -> pure [RErr ("unknown key: " <> k)]
                        _ -> render prefix (if single then take 1 wanted else wanted)
  where
    has c = isJust (lookup c entries)
    value c = case lookup c entries of
        Just (Valued vs@(_ : _)) -> Just (last vs)
        _ -> Nothing
    only = case pos of
        [k] -> Just k
        _ -> Nothing
    single = has '1'
    order = case T.toLower <$> value 'O' of
        Nothing -> Right KeyTreeOrder
        Just o
            | o `elem` ["index", "key"] -> Right ByKeyCode
            | o == "modifier" -> Right ByModifiers
            | o == "order" -> Right KeyTreeOrder
            | o `elem` ["activity", "creation", "name", "title", "size", "z"] ->
                Left ("list-keys -O " <> o <> ": not implemented")
            | otherwise -> Left "invalid sort order"
    sortListed ord ls = (if has 'r' && ord /= KeyTreeOrder then reverse else id) $
        case ord of
            KeyTreeOrder -> ls
            ByKeyCode -> List.sortOn (keyOrder . (.key)) ls
            ByModifiers -> List.sortOn (modifierBits . (.key)) ls
    promptPrefix p = if p == "None" then "" else p
    render prefix ls = do
        base <- clientContextFormatEnv st mclient
        let widthOf f = maximum (0 : map (textWidth . f) ls)
            common = Map.fromList
                [ ("notes_only", "0")
                , ("key_has_repeat", if any (.repeats) ls then "1" else "0")
                , ("key_string_width", tshow (widthOf (.key)))
                , ("key_table_width", tshow (widthOf (.table))) ]
            template = fromMaybe listKeysTemplate (value 'F')
        lines' <- mapM (\l -> expandFormat st (Map.union (bindingVars prefix l) (Map.union common base)) template) ls
        toastClient <- case mclient of
            Just c | single, c.role == Attached, isNothing c.control -> do
                live <- readTVarIO st.sessions
                sid <- readTVarIO c.session
                pure (if Map.member sid live then Just c else Nothing)
            _ -> pure Nothing
        case toastClient of
            Just c -> [] <$ mapM_ (showToast st c) lines'
            Nothing -> pure [ ROutput t | t <- lines', not (T.null t) ]
    bindingVars prefix l = Map.fromList
        [ ("key_repeat", if l.repeats then "1" else "0")
        , ("key_note", "")
        , ("key_prefix", prefix)
        , ("key_table", l.table)
        , ("key_string", l.key)
        , ("key_command", printCommands l.commands) ]

-- | A binding as listed.
listed :: Text -> Text -> Binding -> Listed
listed t k b = Listed { table = t, key = k, commands = b.commands, repeats = b.repeats == Repeats }

-- | A table's bindings in tmux's key tree order: by key code, with keys
-- tmux has no code for after them by name.
inKeyOrder :: Map.Map Text a -> [(Text, a)]
inKeyOrder = List.sortBy (comparing (keyOrder . fst)) . Map.toList

keyOrder :: Text -> (Bool, Integer, Text)
keyOrder k = case keyCode k of
    Just c -> (False, c, k)
    Nothing -> (True, 0, k)

modifierBits :: Text -> Integer
modifierBits k = fst (splitModifiers k)

-- | tmux's @key_code@ for a key name: modifier bits over a type and value,
-- so ordering by it orders as tmux's key tables do.
keyCode :: Text -> Maybe Integer
keyCode k = (mods .|.) <$> baseCode rest
  where
    (mods, rest) = splitModifiers k

splitModifiers :: Text -> (Integer, Text)
splitModifiers = go 0
  where
    go acc t = case T.splitAt 2 t of
        ("C-", r) | not (T.null r) -> go (acc .|. bit 45) r
        ("M-", r) | not (T.null r) -> go (acc .|. bit 44) r
        ("S-", r) | not (T.null r) -> go (acc .|. bit 46) r
        _ -> (acc, t)
    bit n = 1 `shiftL` n

baseCode :: Text -> Maybe Integer
baseCode t
    | [c] <- T.unpack t = Just (fromIntegral (fromEnum c))
    | Just v <- lookup t c0Names = Just v
    | Just i <- List.elemIndex t functionNames = Just (typed 2 (fromIntegral i))
    | otherwise = mouseCode t
  where
    c0Names = [("Enter", 13), ("Escape", 27), ("Space", 32), ("Tab", 9)]

typed :: Integer -> Integer -> Integer
typed ty v = (ty `shiftL` 32) .|. v

-- | tmux's function-key range, from @KEYC_NONE@ on, by key name.
functionNames :: [Text]
functionNames =
    [ "None", "Unknown", "FocusIn", "FocusOut", "Any", "PasteStart", "PasteEnd"
    , "BSpace" ]
    <> [ "F" <> tshow n | n <- [1 .. 12 :: Int] ]
    <> [ "IC", "DC", "Home", "End", "NPage", "PPage", "BTab", "Up", "Down"
       , "Left", "Right", "KP/", "KP*", "KP-", "KP7", "KP8", "KP9", "KP+"
       , "KP4", "KP5", "KP6", "KP1", "KP2", "KP3", "KPEnter", "KP0", "KP." ]

-- | A mouse key's code: its event type, button and location.
mouseCode :: Text -> Maybe Integer
mouseCode t = do
    (ty, afterType) <- firstJust
        [ (,) ty <$> T.stripPrefix nm t | (nm, ty) <- mouseTypes ]
    let (digits, locName) = T.span isDigit afterType
    button <- if T.null digits then Just 0 else case TR.decimal digits of
        Right (b, _) -> Just b
        Left _ -> Nothing
    loc <- List.elemIndex locName mouseLocations
    pure (typed ty ((button `shiftL` 8) .|. fromIntegral loc))
  where
    firstJust = foldr (\m acc -> maybe acc Just m) Nothing
    -- Longest names first, so @MouseDragEnd@ never parses as @MouseDrag@.
    mouseTypes =
        [ ("MouseDragEnd", 7), ("MouseMove", 3), ("MouseDown", 4), ("MouseUp", 5)
        , ("MouseDrag", 6), ("WheelDown", 8), ("WheelUp", 9), ("SecondClick", 10)
        , ("DoubleClick", 11), ("TripleClick", 12) ]

mouseLocations :: [Text]
mouseLocations =
    [ "Pane", "Status", "StatusLeft", "StatusRight", "StatusDefault", "Border"
    , "ScrollbarUp", "ScrollbarSlider", "ScrollbarDown", "Empty" ]
    <> [ "Control" <> tshow n | n <- [0 .. 9 :: Int] ]

-- | A binding's commands as tmux's @cmd_list_print@ prints them for
-- @list-keys@, separated by an escaped semicolon.
printCommands :: [[Text]] -> Text
printCommands = T.intercalate " \\; " . map printCommand

-- | tmux's @cmd_print@: the command's full name and its arguments as
-- @args_print@ orders them — the boolean flags in one bundle, then each
-- flag with a value, then the positional arguments, every value escaped
-- by @args_escape@. A command tmux does not know prints as written.
printCommand :: [Text] -> Text
printCommand [] = ""
printCommand (nm : argv) = case lookupCommand nm of
    Nothing -> T.unwords (nm : map quoteArgsT argv)
    Just (full, template) -> T.unwords (full : printArgs template argv)

lookupCommand :: Text -> Maybe (Text, Text)
lookupCommand nm = case [ (full, tmpl) | (full, alias, tmpl) <- commandTable
                                       , nm == full || (not (T.null alias) && nm == alias) ] of
    hit : _ -> Just hit
    [] -> case [ (full, tmpl) | (full, _, tmpl) <- commandTable, nm `T.isPrefixOf` full ] of
        [hit] -> Just hit
        _ -> Nothing

-- | One parsed argument entry: a flag letter with its values (none for a
-- boolean flag, which counts its repeats; 'Nothing' for an optional value
-- left off).
data Entry = Boolean Int | Valued [Text] | OptionalBare

printArgs :: Text -> [Text] -> [Text]
printArgs template argv = case parseTmuxArgs template argv of
    Left _ -> map quoteArgsT argv
    Right (entries, positional) ->
        let bools = concat [ replicate n c | (c, Boolean n) <- entries ]
            valued = concat
                [ case e of
                    Valued vs -> [ "-" <> T.singleton c <> " " <> quoteArgsT v | v <- vs ]
                    _ -> [ "-" <> T.singleton c ]
                | (c, e) <- entries, not (isBoolean e) ]
            lastOptional = case [ e | (_, e) <- entries, not (isBoolean e) ] of
                [] -> False
                es -> case last es of
                    OptionalBare -> True
                    _ -> False
        in [ T.pack ('-' : bools) | not (null bools) ]
            <> valued <> [ "--" | lastOptional ] <> map quoteArgsT positional
  where
    isBoolean = \case
        Boolean _ -> True
        _ -> False

-- | tmux's @args_parse@ over a template: flags (sorted by letter, as its
-- argument tree keeps them) and the positional arguments, or its error.
parseTmuxArgs :: Text -> [Text] -> Either Text ([(Char, Entry)], [Text])
parseTmuxArgs template = go Map.empty
  where
    spec = T.unpack template
    kindOf c = case dropWhile (/= c) spec of
        (_ : ':' : ':' : _) -> Just OptionalArg
        (_ : ':' : _) -> Just RequiredArg
        (_ : _) -> Just NoArg
        [] -> Nothing
    go acc = \case
        ("--" : rest) -> Right (Map.toList acc, rest)
        (a : rest)
            | Just bundle <- T.stripPrefix "-" a, not (T.null bundle) ->
                flagsIn acc (T.unpack bundle) rest
        rest -> Right (Map.toList acc, rest)
    flagsIn acc [] rest = go acc rest
    flagsIn acc (c : more) rest
        | not (isAlphaNum c) = Left ("invalid flag -" <> T.singleton c)
        | otherwise = case kindOf c of
            Nothing -> Left ("unknown flag -" <> T.singleton c)
            Just NoArg -> flagsIn (Map.insertWith bump c (Boolean 1) acc) more rest
            Just kind
                | not (null more) -> go (addValue c (T.pack more) acc) rest
                | v : rest' <- rest, kind == RequiredArg || takesOptional v ->
                    go (addValue c v acc) rest'
                | kind == OptionalArg -> go (Map.insertWith keepValued c OptionalBare acc) rest
                | otherwise -> Left ("-" <> T.singleton c <> " expects an argument")
    takesOptional v = case T.unpack v of
        ('-' : c2 : _) -> not (c2 == '-' || isAlpha c2)
        _ -> True
    bump (Boolean a) (Boolean b) = Boolean (a + b)
    bump new _ = new
    keepValued new old = case old of
        Valued _ -> old
        _ -> new
    addValue c v = Map.insertWith (\_ old -> case old of
        Valued vs -> Valued (vs <> [v])
        _ -> Valued [v]) c (Valued [v])

data ArgKind = NoArg | RequiredArg | OptionalArg
    deriving (Eq, Show)

-- | tmux's commands: name, alias and argument template, from each
-- @cmd_entry@ of the pinned source.
commandTable :: [(Text, Text, Text)]
commandTable =
    [ ("attach-session", "attach", "c:dEf:rt:x")
    , ("bind-key", "bind", "nrN:T:")
    , ("break-pane", "breakp", "abdPF:n:s:t:Wx:X:y:Y:")
    , ("capture-pane", "capturep", "ab:CeE:FHJLMNpPqRS:Tt:")
    , ("choose-buffer", "", "F:f:K:kNO:rt:yZ")
    , ("choose-client", "", "F:f:hiK:kNO:rt:yZ")
    , ("choose-tree", "", "F:f:GhK:kNO:rst:wyZ")
    , ("clear-history", "clearhist", "Ht:")
    , ("clear-prompt-history", "clearphist", "T:")
    , ("clock-mode", "", "t:")
    , ("command-prompt", "", "1CbeFiklI:NPp:t:T:")
    , ("confirm-before", "confirm", "bc:p:t:y")
    , ("copy-mode", "", "dekHMqSs:t:u")
    , ("customize-mode", "", "F:f:kNt:yZ")
    , ("delete-buffer", "deleteb", "b:")
    , ("detach-client", "detach", "aE:s:t:P")
    , ("display-menu", "menu", "b:c:C:H:s:S:MOt:T:x:y:")
    , ("display-message", "display", "aCc:d:lINpt:F:v")
    , ("display-panes", "displayp", "d:kNs:t:Z")
    , ("display-popup", "popup", "Bb:Cc:d:e:Eh:kNs:S:t:T:w:x:y:")
    , ("find-window", "findw", "CiNrt:TZ")
    , ("has-session", "has", "t:")
    , ("if-shell", "if", "bFt:")
    , ("join-pane", "joinp", "bdfhvp:l:s:t:")
    , ("kill-pane", "killp", "af:t:")
    , ("kill-server", "", "")
    , ("kill-session", "", "aCgf:t:")
    , ("kill-window", "killw", "af:t:")
    , ("last-pane", "lastp", "det:Z")
    , ("last-window", "last", "t:")
    , ("link-window", "linkw", "abdks:t:")
    , ("list-buffers", "lsb", "F:f:O:r")
    , ("list-clients", "lsc", "F:f:O:rt:")
    , ("list-commands", "lscm", "F:")
    , ("list-keys", "lsk", "1aF:NO:P:rT:")
    , ("list-panes", "lsp", "aF:f:O:rst:")
    , ("list-sessions", "ls", "F:f:O:r")
    , ("list-windows", "lsw", "aF:f:O:rt:")
    , ("load-buffer", "loadb", "b:t:w")
    , ("lock-client", "lockc", "t:")
    , ("lock-server", "lock", "")
    , ("lock-session", "locks", "t:")
    , ("move-pane", "movep", "bdD::fhMvl:L::P:R::s:t:U::X:Y:z:")
    , ("move-window", "movew", "abdkrs:t:")
    , ("new-pane", "newp", "bB:Cc:de:EfF:hIkl:LMm:Op:PR:s:S:t:T:vWx:X:y:Y:Z")
    , ("new-session", "new", "Ac:dDe:EF:f:n:Ps:t:x:Xy:")
    , ("new-window", "neww", "abc:de:EF:kn:PSt:")
    , ("next-layout", "nextl", "t:")
    , ("next-window", "next", "at:")
    , ("paste-buffer", "pasteb", "db:prSs:t:")
    , ("pipe-pane", "pipep", "IOot:")
    , ("previous-layout", "prevl", "t:")
    , ("previous-window", "prev", "at:")
    , ("refresh-client", "refresh", "A:B:cC:Df:r:F:lLRSt:U")
    , ("rename-session", "rename", "t:")
    , ("rename-window", "renamew", "t:")
    , ("resize-pane", "resizep", "D::L::MR::Tt:U::x:y:Z")
    , ("resize-window", "resizew", "aADLRt:Ux:y:")
    , ("respawn-pane", "respawnp", "c:e:Ekt:")
    , ("respawn-window", "respawnw", "c:e:Ekt:")
    , ("rotate-window", "rotatew", "Dt:UZ")
    , ("run-shell", "run", "bd:Ct:Es:c:")
    , ("save-buffer", "saveb", "ab:")
    , ("select-layout", "selectl", "Enopt:")
    , ("select-pane", "selectp", "DdegLlMmP:RT:t:UZ")
    , ("select-window", "selectw", "lnpTt:")
    , ("send-keys", "send", "c:FHKlMN:Rt:X")
    , ("send-prefix", "", "2t:")
    , ("server-access", "", "adglrw")
    , ("set-buffer", "setb", "ab:t:n:w")
    , ("set-environment", "setenv", "Fhgrt:u")
    , ("set-hook", "", "agpERTt:uB:w")
    , ("set-option", "set", "aFgopqst:uUw")
    , ("set-window-option", "setw", "aFgoqt:u")
    , ("show-buffer", "showb", "b:")
    , ("show-environment", "showenv", "hgst:")
    , ("show-hooks", "", "BF:gpt:w")
    , ("show-messages", "showmsgs", "JTt:")
    , ("show-options", "show", "AgF:Hpqst:vw")
    , ("show-prompt-history", "showphist", "T:")
    , ("show-window-options", "showw", "F:gvt:")
    , ("source-file", "source", "t:Fnqv")
    , ("split-window", "splitw", "bB:c:de:EfF:hIkl:m:p:PR:s:S:t:T:vWZ")
    , ("start-server", "start", "")
    , ("suspend-client", "suspendc", "t:")
    , ("swap-pane", "swapp", "dDs:t:UZ")
    , ("swap-window", "swapw", "ds:t:")
    , ("switch-client", "switchc", "c:EFlnO:pt:rT:Z")
    , ("switch-mode", "", "F:kst:wZ")
    , ("unbind-key", "unbind", "anqT:")
    , ("unlink-window", "unlinkw", "kt:")
    , ("wait-for", "wait", "EF:LSUlvw:")
    ]
