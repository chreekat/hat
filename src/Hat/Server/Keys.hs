-- | Key recognition: raw tty bytes in, named keys out, and the reverse
-- for @bind-key@'s key names. A 'Key' keeps the raw bytes so unbound
-- keys pass through to the pane untouched.
--
-- A lone ESC at the end of a chunk is ambiguous: the Escape key, or the
-- first byte of a meta/CSI/SS3 sequence whose rest is still in flight.
-- @escape-time@ resolves it (see 'feedKeys'): 0 calls it Escape at once
-- ('EscImmediate', what 'tokenizeKeys' bakes in), non-zero holds it for
-- that many ms ('EscBuffered'), coalescing with the next bytes if they
-- arrive first, else emitting Escape on timeout.
module Hat.Server.Keys
    ( Key (..)
    , PrefixState (..)
    , KeyAction (..)
    , EscTiming (..)
    , EscPending (..)
    , EscTokens (..)
    , tokenizeKeys
    , feedKeys
    , flushEscape
    , parseKeyName
    , namedKeys
    , routeKeys
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data Key = Key
    { name :: Text        -- ^ canonical name, as written in configs
    , raw  :: ByteString  -- ^ bytes to forward when the key is unbound
    }
    deriving (Eq, Ord, Show)

-- | Per-client input state: has the prefix key been pressed?
data PrefixState = NoPrefix | PrefixArmed
    deriving (Eq, Show)

mkKey :: Text -> ByteString -> Key
mkKey n r = Key { name = n, raw = r }

namedKeys :: [(Text, ByteString)]
namedKeys =
    [ ("Enter", "\r")
    , ("Tab", "\t")
    , ("Space", " ")
    , ("BSpace", "\x7f")
    , ("Escape", "\ESC")
    , ("Up", "\ESC[A")
    , ("Down", "\ESC[B")
    , ("Right", "\ESC[C")
    , ("Left", "\ESC[D")
    -- tmux-256color terminfo khome/kend, not the xterm SS3 forms (\EOH/\EOF):
    -- a pager keyed off terminfo reads those as a bare H/F.
    , ("Home", "\ESC[1~")
    , ("End", "\ESC[4~")
    , ("PgUp", "\ESC[5~")
    , ("PgDn", "\ESC[6~")
    , ("Delete", "\ESC[3~")
    , ("Insert", "\ESC[2~")
    , ("BTab", "\ESC[Z")
    -- tmux-256color kf1..kf4 are the SS3 forms.
    , ("F1", "\ESCOP"), ("F2", "\ESCOQ"), ("F3", "\ESCOR"), ("F4", "\ESCOS")
    ] ++ [ (nm, "\ESC[" <> code <> "~") | (code, nm) <- fkeyTildes ]

-- CSI tilde codes for F5-F12 (tmux-256color kf5..kf12).
fkeyTildes :: [(ByteString, Text)]
fkeyTildes =
    [ ("15", "F5"), ("17", "F6"), ("18", "F7"), ("19", "F8")
    , ("20", "F9"), ("21", "F10"), ("23", "F11"), ("24", "F12")
    ]

-- | Alternate spellings tmux accepts, each resolving to the canonical
-- 'namedKeys' name. Compared lowercased.
keyAliases :: [(Text, Text)]
keyAliases =
    [ ("ic", "insert"), ("dc", "delete")
    , ("npage", "pgdn"), ("pagedown", "pgdn")
    , ("ppage", "pgup"), ("pageup", "pgup")
    ]

-- CSI/SS3 finals for the cursor keys in both keypad modes.
csiNames :: [(ByteString, Text)]
csiNames =
    [ ("[A", "Up"), ("[B", "Down"), ("[C", "Right"), ("[D", "Left")
    , ("[H", "Home"), ("[F", "End")
    , ("OA", "Up"), ("OB", "Down"), ("OC", "Right"), ("OD", "Left")
    , ("OH", "Home"), ("OF", "End")
    , ("[5~", "PgUp"), ("[6~", "PgDn"), ("[3~", "Delete")
    , ("[1~", "Home"), ("[4~", "End")
    , ("[2~", "Insert"), ("[Z", "BTab")
    , ("OP", "F1"), ("OQ", "F2"), ("OR", "F3"), ("OS", "F4")
    -- legacy xterm CSI forms of F1-F4
    , ("[11~", "F1"), ("[12~", "F2"), ("[13~", "F3"), ("[14~", "F4")
    , ("[I", "FocusIn"), ("[O", "FocusOut")
    ]
    ++ [ ("[" <> code <> "~", nm) | (code, nm) <- fkeyTildes ]
    ++ modifiedCsiNames

-- Special keys with an xterm modifier parameter (@CSI 1;p F@ and
-- @CSI n;p ~@), named with tmux's canonical @C-M-S-@ prefix order.
modifiedCsiNames :: [(ByteString, Text)]
modifiedCsiNames =
    [ (pre <> B8.pack (show p) <> post, mods <> base)
    | (p, mods) <-
        [ (2 :: Int, "S-"), (3, "M-"), (4, "M-S-"), (5, "C-")
        , (6, "C-S-"), (7, "C-M-"), (8, "C-M-S-") ]
    , (pre, post, base) <-
        [ ("[1;", "A", "Up"), ("[1;", "B", "Down")
        , ("[1;", "C", "Right"), ("[1;", "D", "Left")
        , ("[1;", "H", "Home"), ("[1;", "F", "End")
        , ("[1;", "P", "F1"), ("[1;", "Q", "F2")
        , ("[1;", "R", "F3"), ("[1;", "S", "F4")
        , ("[2;", "~", "Insert"), ("[3;", "~", "Delete")
        , ("[5;", "~", "PgUp"), ("[6;", "~", "PgDn")
        ]
        ++ [ ("[" <> code <> ";", "~", nm) | (code, nm) <- fkeyTildes ]
    ]

-- | @escape-time@ semantics for a lone trailing ESC. See 'feedKeys'.
data EscTiming = EscImmediate | EscBuffered
    deriving (Eq, Show)

-- | A lone trailing ESC held awaiting the escape-time window. See 'feedKeys'.
data EscPending = NoEscPending | EscPending
    deriving (Eq, Show)

-- | Result of 'feedKeys': the keys to act on now, plus any trailing lone ESC
-- held for coalescing with the next chunk.
data EscTokens = EscTokens
    { escKeys    :: [Key]
    , escPending :: EscPending
    }
    deriving (Eq, Show)

-- | The escape-time coalescing core, pure over (held ESC, timing, bytes).
--
-- Any held ESC is prepended to the new bytes and the whole run is tokenized;
-- under 'EscBuffered' a fresh /lone trailing/ ESC is held (reported as
-- 'EscPending') rather than emitted, so the next chunk — or 'flushEscape' on
-- timeout — decides it. Under 'EscImmediate' this matches 'tokenizeKeys'.
feedKeys :: EscTiming -> EscPending -> ByteString -> EscTokens
feedKeys timing pending bs =
    let full = case pending of
            EscPending   -> "\ESC" <> bs
            NoEscPending -> bs
        (keys, pending') = tokenize timing full
    in EscTokens { escKeys = keys, escPending = pending' }

-- | The escape-time timer fired with an ESC still held: it is the Escape key.
flushEscape :: EscPending -> [Key]
flushEscape EscPending   = [mkKey "Escape" "\ESC"]
flushEscape NoEscPending = []

tokenizeKeys :: ByteString -> [Key]
tokenizeKeys = fst . tokenize EscImmediate

tokenize :: EscTiming -> ByteString -> ([Key], EscPending)
tokenize timing = go
  where
    go bs = case B.uncons bs of
        Nothing -> ([], NoEscPending)
        Just (b, rest)
            | b == 0x1b -> escape rest
            | b == 0x00 -> cons (mkKey "C-Space" (B.singleton b)) (go rest)
            | b == 0x09 -> cons (mkKey "Tab" (B.singleton b)) (go rest)
            | b == 0x0d -> cons (mkKey "Enter" (B.singleton b)) (go rest)
            | b == 0x20 -> cons (mkKey "Space" (B.singleton b)) (go rest)
            | b == 0x7f -> cons (mkKey "BSpace" (B.singleton b)) (go rest)
            | b < 0x20 ->
                cons (mkKey (T.pack ['C', '-', ctrlChar b]) (B.singleton b)) (go rest)
            | b < 0x80 ->
                cons (mkKey (T.singleton (toEnum (fromIntegral b))) (B.singleton b))
                    (go rest)
            | otherwise -> utf8 bs
    ctrlChar b = toEnum (fromIntegral b + fromEnum 'a' - 1)
    cons k (ks, p) = (k : ks, p)

    escape rest = case B.uncons rest of
        -- Chunk ends right after ESC: Escape now (escape-time 0), or held for
        -- the next chunk / a timeout flush (escape-time > 0).
        Nothing -> case timing of
            EscImmediate -> ([mkKey "Escape" "\ESC"], NoEscPending)
            EscBuffered  -> ([], EscPending)
        Just (b2, rest2)
            | b2 == 0x1b ->
                -- ESC ESC ...: meta variant of whatever follows
                case escape rest2 of
                    (k : ks, p) | k.name /= "Escape" ->
                        (mkKey ("M-" <> k.name) ("\ESC" <> k.raw) : ks, p)
                    (ks, p) -> (mkKey "Escape" "\ESC" : ks, p)
            | b2 == 0x5b || b2 == 0x4f ->  -- '[' or 'O'
                case matchCsi rest of
                    Just (nm, len) ->
                        cons (mkKey nm ("\ESC" <> B.take len rest)) (go (B.drop len rest))
                    Nothing ->
                        -- Unknown sequence: swallow it whole so garbage
                        -- doesn't leak keys; forward raw.
                        let (seqBytes, rest') = spanCsi rest
                        in cons (mkKey "Unknown" ("\ESC" <> seqBytes)) (go rest')
            | otherwise -> case go (B.cons b2 rest2) of
                (k : ks, p) -> (mkKey ("M-" <> k.name) ("\ESC" <> k.raw) : ks, p)
                ([], p) -> ([mkKey "Escape" "\ESC"], p)

    matchCsi rest =
        let candidates =
                [ (nm, B.length pat)
                | (pat, nm) <- csiNames
                , pat `B.isPrefixOf` rest
                ]
        in case candidates of
            ((nm, len) : _) -> Just (nm, len)
            [] -> Nothing

    -- CSI: parameters then a final byte in 0x40-0x7e.
    spanCsi rest = case B.uncons rest of
        Nothing -> (rest, B.empty)
        Just (intro, params) ->
            let (ps, fin) = B.span (\b -> b < 0x40 || b > 0x7e) params
            in case B.uncons fin of
                Just (f, rest') ->
                    (B.cons intro ps `B.snoc` f, rest')
                Nothing -> (B.cons intro ps, B.empty)

    utf8 bs =
        let len = utf8Len (B.head bs)
            (ch, rest) = B.splitAt len bs
            txt = TE.decodeUtf8Lenient ch
        in cons (mkKey txt ch) (go rest)
    utf8Len b
        | b >= 0xf0 = 4
        | b >= 0xe0 = 3
        | b >= 0xc0 = 2
        | otherwise = 1

-- | What a batch of keys should do, in order. Consecutive unbound
-- keys coalesce into one passthrough write.
data KeyAction
    = Passthrough ByteString
    | RunCommands [[Text]]
    deriving (Eq, Show)

-- | Drive the prefix state machine over tokenized keys, resolving
-- bindings from the given tables.
--
-- When the active pane is in copy mode, its key table name is passed as
-- @Just table@: that table /owns/ every unprefixed key. Bound keys run
-- their command; a miss is /swallowed/ rather than forwarded to the pty
-- (so browsing keys never leak into the shell). The prefix key is not
-- intercepted while in mode — copy mode binds it itself (e.g. @C-b@ ->
-- @cursor-left@ in emacs), matching tmux.
routeKeys
    :: Text                        -- ^ prefix key name
    -> Map Text (Map Text [[Text]]) -- ^ keymap: table -> key -> commands
    -> Maybe Text                  -- ^ active pane's copy-mode table, if any
    -> PrefixState
    -> [Key]
    -> (PrefixState, [KeyAction])
routeKeys prefixName keymap modeTable = go
  where
    rootTable = Map.findWithDefault Map.empty "root" keymap
    prefixTable = Map.findWithDefault Map.empty "prefix" keymap
    go st [] = (st, [])
    go NoPrefix (k : ks)
        | Just table <- modeTable =
            case Map.lookup k.name (Map.findWithDefault Map.empty table keymap) of
                Just cmds -> emit (RunCommands cmds) (go NoPrefix ks)
                Nothing
                    -- The prefix key still arms in copy mode (unless the mode
                    -- table bound it above), so prefix-table commands remain
                    -- reachable without leaving the mode.
                    | k.name == prefixName -> go PrefixArmed ks
                    | otherwise -> go NoPrefix ks  -- unbound in copy mode: swallowed
        | k.name == prefixName = go PrefixArmed ks
        | Just cmds <- Map.lookup k.name rootTable =
            emit (RunCommands cmds) (go NoPrefix ks)
        | otherwise = emit (Passthrough k.raw) (go NoPrefix ks)
    go PrefixArmed (k : ks) = case Map.lookup k.name prefixTable of
        Just cmds -> emit (RunCommands cmds) (go NoPrefix ks)
        Nothing -> go NoPrefix ks  -- unbound prefixed key: swallowed
    emit a (st, as) = (st, merge a as)
    merge (Passthrough a) (Passthrough b : rest) = Passthrough (a <> b) : rest
    merge a rest = a : rest

-- | Key names as written in configs: @x@, @C-b@, @M-n@, @Up@, @C-Space@.
-- Named keys (@Space@, @Enter@, @Up@, ...) match case-insensitively, as
-- tmux does; the caret form @^h@ is an alias for @C-h@.
parseKeyName :: Text -> Maybe Key
parseKeyName t
    | Just (n, r) <- lookupNamed t = Just (mkKey n r)
    | Just (n, r) <- lookupModified t = Just (mkKey n r)
    | Just rest <- T.stripPrefix "C-" t = ctrl rest
    | Just rest <- T.stripPrefix "^" t, not (T.null rest) = ctrl rest
    | Just rest <- T.stripPrefix "M-" t = do
        k <- parseKeyName rest
        pure (mkKey ("M-" <> k.name) ("\ESC" <> k.raw))
    | T.length t == 1 = Just (mkKey t (TE.encodeUtf8 t))
    | otherwise = Nothing
  where
    ctrl rest
        | T.toLower rest == "space" = Just (mkKey "C-Space" "\NUL")
        | Just (nm, r) <- lookupModified ("C-" <> rest) = Just (mkKey nm r)
        | T.length rest == 1
        , c <- toLowerAscii (T.head rest)
        , c >= 'a' && c <= 'z' =
            Just $ mkKey ("C-" <> T.singleton c)
                (B8.singleton (toEnum (fromEnum c - fromEnum 'a' + 1)))
        | otherwise = Nothing
    toLowerAscii c
        | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
        | otherwise = c

-- Case- and prefix-order-insensitive lookup of a modified CSI key,
-- returning its canonical name and bytes.
lookupModified :: Text -> Maybe (Text, ByteString)
lookupModified t =
    case [ (nm, "\ESC" <> pat)
         | (pat, nm) <- modifiedCsiNames, norm nm == norm t ] of
        (pair : _) -> Just pair
        [] -> Nothing
  where
    norm s = let parts = T.splitOn "-" (T.toLower s)
             in (sort (init parts), canonBase (last parts))

-- Case-insensitive lookup of a multi-character named key, returning its
-- canonical name and bytes.
lookupNamed :: Text -> Maybe (Text, ByteString)
lookupNamed t =
    case [ pair | pair@(n, _) <- namedKeys
         , T.toLower n == canonBase (T.toLower t) ] of
        (pair : _) -> Just pair
        [] -> Nothing

-- Resolve a lowercased key name through 'keyAliases'.
canonBase :: Text -> Text
canonBase s = maybe s id (lookup s keyAliases)
