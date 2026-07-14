-- | Key recognition: raw tty bytes in, named keys out, and the reverse
-- for @bind-key@'s key names. A 'Key' keeps the raw bytes so unbound
-- keys pass through to the pane untouched.
--
-- ESC disambiguation follows @escape-time 0@ semantics: a lone ESC at
-- the end of a chunk is the Escape key, ESC followed by more bytes is
-- a meta or CSI/SS3 sequence.
module Hat.Server.Keys
    ( Key (..)
    , PrefixState (..)
    , KeyAction (..)
    , tokenizeKeys
    , parseKeyName
    , routeKeys
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
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
    , ("Home", "\ESC[H")
    , ("End", "\ESC[F")
    , ("PgUp", "\ESC[5~")
    , ("PgDn", "\ESC[6~")
    , ("Delete", "\ESC[3~")
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
    , ("[I", "FocusIn"), ("[O", "FocusOut")
    , ("[1;5A", "C-Up"), ("[1;5B", "C-Down")
    , ("[1;5C", "C-Right"), ("[1;5D", "C-Left")
    ]

tokenizeKeys :: ByteString -> [Key]
tokenizeKeys = go
  where
    go bs = case B.uncons bs of
        Nothing -> []
        Just (b, rest)
            | b == 0x1b -> escape rest
            | b == 0x00 -> mkKey "C-Space" (B.singleton b) : go rest
            | b == 0x09 -> mkKey "Tab" (B.singleton b) : go rest
            | b == 0x0d -> mkKey "Enter" (B.singleton b) : go rest
            | b == 0x20 -> mkKey "Space" (B.singleton b) : go rest
            | b == 0x7f -> mkKey "BSpace" (B.singleton b) : go rest
            | b < 0x20 ->
                mkKey (T.pack ['C', '-', ctrlChar b]) (B.singleton b) : go rest
            | b < 0x80 ->
                mkKey (T.singleton (toEnum (fromIntegral b))) (B.singleton b)
                    : go rest
            | otherwise -> utf8 bs
    ctrlChar b = toEnum (fromIntegral b + fromEnum 'a' - 1)

    escape rest = case B.uncons rest of
        -- escape-time 0: chunk ends right after ESC -> the Escape key
        Nothing -> [mkKey "Escape" "\ESC"]
        Just (b2, rest2)
            | b2 == 0x1b ->
                -- ESC ESC ...: meta variant of whatever follows
                case escape rest2 of
                    (k : ks) | k.name /= "Escape" ->
                        mkKey ("M-" <> k.name) ("\ESC" <> k.raw) : ks
                    ks -> mkKey "Escape" "\ESC" : ks
            | b2 == 0x5b || b2 == 0x4f ->  -- '[' or 'O'
                case matchCsi rest of
                    Just (nm, len) ->
                        mkKey nm ("\ESC" <> B.take len rest) : go (B.drop len rest)
                    Nothing ->
                        -- Unknown sequence: swallow it whole so garbage
                        -- doesn't leak keys; forward raw.
                        let (seqBytes, rest') = spanCsi rest
                        in mkKey "Unknown" ("\ESC" <> seqBytes) : go rest'
            | otherwise -> case go (B.cons b2 rest2) of
                (k : ks) -> mkKey ("M-" <> k.name) ("\ESC" <> k.raw) : ks
                [] -> [mkKey "Escape" "\ESC"]

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
        in mkKey txt ch : go rest
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
                Nothing -> go NoPrefix ks  -- unbound in copy mode: swallowed
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
        | Just (nm, r) <- lookupCtrlArrow rest = Just (mkKey nm r)
        | T.length rest == 1
        , c <- toLowerAscii (T.head rest)
        , c >= 'a' && c <= 'z' =
            Just $ mkKey ("C-" <> T.singleton c)
                (B8.singleton (toEnum (fromEnum c - fromEnum 'a' + 1)))
        | otherwise = Nothing
    toLowerAscii c
        | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
        | otherwise = c

-- Ctrl+arrow keys as xterm CSI sequences with modifier 5. The names
-- match the @csiNames@ finals so name/raw round-trip between parse and
-- decode. The argument is the text after the @C-@ prefix (e.g. @Down@).
lookupCtrlArrow :: Text -> Maybe (Text, ByteString)
lookupCtrlArrow rest =
    case [ pair | pair@(nm, _) <- ctrlArrows
                , T.toLower nm == T.toLower ("C-" <> rest) ] of
        (pair : _) -> Just pair
        [] -> Nothing
  where
    ctrlArrows =
        [ ("C-Up", "\ESC[1;5A"), ("C-Down", "\ESC[1;5B")
        , ("C-Right", "\ESC[1;5C"), ("C-Left", "\ESC[1;5D")
        ]

-- Case-insensitive lookup of a multi-character named key, returning its
-- canonical name and bytes.
lookupNamed :: Text -> Maybe (Text, ByteString)
lookupNamed t =
    case [ pair | pair@(n, _) <- namedKeys, T.toLower n == T.toLower t ] of
        (pair : _) -> Just pair
        [] -> Nothing
