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
    , Binding (..)
    , Repeat (..)
    , Keymap
    , KeyState (..)
    , Repeating (..)
    , initialKeyState
    , prefixArmed
    , expireRepeat
    , KeyContext (..)
    , KeyAction (..)
    , EscTiming (..)
    , EscPending (..)
    , EscTokens (..)
    , tokenizeKeys
    , feedKeys
    , UserKeys
    , userKeys
    , isUserKey
    , backspaceBytes
    , flushEscape
    , parseKeyName
    , namedKeys
    , namedKeyBytes
    , keypadAppBytes
    , extendedKeyCode
    , ExtendedKeys (..)
    , ExtendedFormat (..)
    , KeyEncoding (..)
    , keyEncodingOf
    , paneKeyMode
    , extendedKeyBytes
    , routeKey
    , routeKeys
    ) where

import Data.Bits ((.|.), testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.Char (chr, ord)
import Data.List (sort, sortOn)
import Data.Maybe (fromMaybe, isJust)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR

import Hat.Model.Ids (PaneId, SessionId, WindowId)
import Hat.Model.OptionTable (ArrayKey (..))
import Hat.Server.Mouse
    (Drag, MouseEvent, MouseState, initialMouseState, isMouseKey, isMoveKey
    , mouseKeyByName)

data Key = Key
    { name :: Text        -- ^ canonical name, as written in configs
    , raw  :: ByteString  -- ^ bytes to forward when the key is unbound
    }
    deriving (Eq, Ord, Show)

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
    ++ [ (nm, plain) | (nm, plain, _) <- keypadKeys ]

-- | The keypad keys: their names, what they send with the keypad in numeric
-- mode, and in application mode (DECKPAM).
keypadKeys :: [(Text, ByteString, ByteString)]
keypadKeys =
    [ ("KP/", "/", "\ESCOo"), ("KP*", "*", "\ESCOj"), ("KP-", "-", "\ESCOm")
    , ("KP7", "7", "\ESCOw"), ("KP8", "8", "\ESCOx"), ("KP9", "9", "\ESCOy")
    , ("KP+", "+", "\ESCOk"), ("KP4", "4", "\ESCOt"), ("KP5", "5", "\ESCOu")
    , ("KP6", "6", "\ESCOv"), ("KP1", "1", "\ESCOq"), ("KP2", "2", "\ESCOr")
    , ("KP3", "3", "\ESCOs"), ("KPEnter", "\n", "\ESCOM")
    , ("KP0", "0", "\ESCOp"), ("KP.", ".", "\ESCOn")
    ]

-- | What each keypad key sends with the keypad in application mode.
keypadAppBytes :: Map Text ByteString
keypadAppBytes = Map.fromList [ (nm, app) | (nm, _, app) <- keypadKeys ]

-- | 'namedKeys' as a map, for the per-keystroke forwarding lookup.
namedKeyBytes :: Map Text ByteString
namedKeyBytes = Map.fromList namedKeys

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
    , ("[200~", "PasteStart"), ("[201~", "PasteEnd")
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
-- The @user-keys@ sequences are recognised ahead of every other key.
feedKeys :: EscTiming -> UserKeys -> EscPending -> ByteString -> EscTokens
feedKeys timing users pending bs =
    let full = case pending of
            EscPending   -> "\ESC" <> bs
            NoEscPending -> bs
        (keys, pending') = tokenize timing users full
    in EscTokens { escKeys = keys, escPending = pending' }

-- | The @user-keys@ sequences and the @User@/N/ keys they denote, longest
-- first.
type UserKeys = [(ByteString, Text)]

-- | The @user-keys@ array as 'UserKeys': entry /N/ is the key @User@/N/
-- (tmux's @KEYC_USER + N@); an empty entry denotes nothing.
userKeys :: Map ArrayKey Text -> UserKeys
userKeys m = sortOn (negate . B.length . fst)
    [ (TE.encodeUtf8 sq, "User" <> T.pack (show n))
    | (KeyIndex n, sq) <- Map.toList m, n <= maxUserKey, not (T.null sq) ]

-- | tmux's @KEYC_NUSER@: the highest user key number.
maxUserKey :: Integer
maxUserKey = 1000

-- | Whether a key is a @User@/N/ key, which only bindings act on: it is never
-- typed into a pane.
isUserKey :: Text -> Bool
isUserKey = isJust . userKeyNumber

userKeyNumber :: Text -> Maybe Integer
userKeyNumber t = do
    digits <- T.stripPrefix "User" t
    case TR.decimal digits of
        Right (n, "") | n <= maxUserKey -> Just n
        _ -> Nothing

-- | The byte the @backspace@ option says the Backspace key sends (tmux's
-- @input_key@): a plain or control key's own byte; any other key sends
-- nothing.
backspaceBytes :: Text -> Maybe ByteString
backspaceBytes name = case parseKeyName name of
    Just k | B.length k.raw == 1 -> Just k.raw
    _ -> Nothing

-- | The escape-time timer fired with an ESC still held: it is the Escape key.
flushEscape :: EscPending -> [Key]
flushEscape EscPending   = [mkKey "Escape" "\ESC"]
flushEscape NoEscPending = []

tokenizeKeys :: ByteString -> [Key]
tokenizeKeys = fst . tokenize EscImmediate []

tokenize :: EscTiming -> UserKeys -> ByteString -> ([Key], EscPending)
tokenize timing users = go
  where
    go bs = case B.uncons bs of
        Nothing -> ([], NoEscPending)
        Just _
            | ((sq, nm) : _) <- filter ((`B.isPrefixOf` bs) . fst) users ->
                cons (mkKey nm sq) (go (B.drop (B.length sq) bs))
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
    -- 0x01-0x1a are C-a..C-z; the rest of C0 (0x1c-0x1f here — ESC, NUL,
    -- Tab, Enter never reach this) ctrl the 0x40-offset chars: C-\ C-] C-^ C-_.
    ctrlChar b
        | b <= 0x1a = toEnum (fromIntegral b + fromEnum 'a' - 1)
        | otherwise = toEnum (fromIntegral b + 0x40)
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
            -- A legacy mouse report: ESC [ M and three raw bytes.
            | "[M" `B.isPrefixOf` rest, B.length rest >= 5 ->
                cons (mkKey "Mouse" ("\ESC" <> B.take 5 rest)) (go (B.drop 5 rest))
            | b2 == 0x5b || b2 == 0x4f ->  -- '[' or 'O'
                case matchCsi rest of
                    Just (nm, len) ->
                        cons (mkKey nm ("\ESC" <> B.take len rest)) (go (B.drop len rest))
                    Nothing ->
                        -- Swallow the sequence whole so garbage doesn't leak
                        -- keys: an extended-key form becomes its canonical
                        -- key, anything else an unnamed 'Unknown'.
                        let (seqBytes, rest') = spanCsi rest
                        in cons (extendedOrUnknown seqBytes) (go rest')
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

-- | Name a swallowed CSI sequence as an extended key, or leave it 'Unknown'.
-- An 'Unknown' key is never delivered (see 'Hat.Server.Conn.deliversKey'), so
-- its bytes stay in the sequence only for the log.
extendedOrUnknown :: ByteString -> Key
extendedOrUnknown sq
    | "[<" `B.isPrefixOf` sq, Just (_, f) <- B.unsnoc sq, f == 0x4d || f == 0x6d =
        mkKey "Mouse" ("\ESC" <> sq)  -- an SGR mouse report
    | otherwise = case extendedParams sq >>= uncurry extendedKeyOf of
        Just k  -> k
        Nothing -> mkKey "Unknown" ("\ESC" <> sq)

-- The (codepoint, xterm modifier param) an extended-key CSI carries: xterm's
-- modifyOtherKeys @CSI 27;mod;code~@ and the CSI-u @CSI code;mod u@ that
-- kitty's protocol shares. Kitty's @:@ subparams (alternate keys, event type,
-- associated text) are dropped: the base codepoint is the key.
extendedParams :: ByteString -> Maybe (Int, Int)
extendedParams sq = do
    (intro, body) <- B.uncons sq
    (params, final) <- B.unsnoc body
    fields <- traverse wholeInt [B8.takeWhile (/= ':') f | f <- B8.split ';' params]
    case (intro, final, fields) of
        (0x5b, 0x7e, [27, m, c]) -> Just (c, m)
        (0x5b, 0x75, [c])        -> Just (c, 1)
        (0x5b, 0x75, c : m : _)  -> Just (c, m)
        _                        -> Nothing
  where
    wholeInt f = case B8.readInt f of
        Just (n, rest) | B.null rest -> Just n
        _                            -> Nothing

-- | The key an extended-key (codepoint, modifier param) denotes: its canonical
-- tmux name, and the legacy bytes a pane that enabled no key protocol expects
-- — modifiers with no legacy encoding dropped, Alt kept as an ESC prefix, as
-- tmux does. A pane's own protocol state re-spells it on delivery (see
-- 'Hat.Term.Emulator.encodeKeyPress'). 'Nothing' for a codepoint with no tmux
-- name, such as kitty's functional-key codes.
extendedKeyOf :: Int -> Int -> Maybe Key
extendedKeyOf code param = do
    base <- baseKeyName code
    let bits = max 0 (param - 1)
        (ctrl, alt, shift) = (testBit bits 2, testBit bits 1, testBit bits 0)
        name = T.concat ([ "C-" | ctrl ] ++ [ "M-" | alt ] ++ [ "S-" | shift ])
        legacy = if ctrl then ctrlByte code else charBytes code
    pure (mkKey (name <> base) (if alt then "\ESC" <> legacy else legacy))

-- The name of an extended key's base codepoint: the specials tmux names, then
-- the printable ASCII character itself, lowercased (the codepoint is the
-- unshifted one, so Shift rides the modifier mask).
baseKeyName :: Int -> Maybe Text
baseKeyName c = case c of
    13  -> Just "Enter"
    9   -> Just "Tab"
    27  -> Just "Escape"
    32  -> Just "Space"
    127 -> Just "BSpace"
    _ | c >= 0x21 && c <= 0x7e -> Just (T.toLower (T.singleton (chr c)))
      | otherwise              -> Nothing

-- The C0 byte Ctrl+key produces, for the codepoints that have one; the others
-- have no legacy encoding, so Ctrl is dropped.
ctrlByte :: Int -> ByteString
ctrlByte c
    | c >= ord 'a' && c <= ord 'z' = B.singleton (fromIntegral (c - 0x60))
    | c >= 0x40 && c <= 0x5f       = B.singleton (fromIntegral (c - 0x40))
    | c == 0x20                    = "\NUL"
    | c == ord '?'                 = "\DEL"
    | otherwise                    = charBytes c

charBytes :: Int -> ByteString
charBytes = TE.encodeUtf8 . T.singleton . chr

-- | @extended-keys@: whether panes get modified keys in an extended form.
data ExtendedKeys = ExtendedOff | ExtendedOn | ExtendedAlways
    deriving (Eq, Show)

-- | @extended-keys-format@: which extended form.
data ExtendedFormat = FormatXterm | FormatCsiU
    deriving (Eq, Show)

-- | How keys are encoded for panes, from the server options.
data KeyEncoding = KeyEncoding
    { extended :: ExtendedKeys
    , format   :: ExtendedFormat
    }
    deriving (Eq, Show)

-- | The @extended-keys@ and @extended-keys-format@ option values.
keyEncodingOf :: Text -> Text -> KeyEncoding
keyEncodingOf ext fmt = KeyEncoding
    { extended = case ext of
        "on" -> ExtendedOn
        "always" -> ExtendedAlways
        _ -> ExtendedOff
    , format = if fmt == "csi-u" then FormatCsiU else FormatXterm }

-- | tmux's pane key mode: what @extended-keys@ makes of the mode the app
-- asked for with @CSI > 4 ; n m@ — ignored when off, forced to at least
-- mode 1 when always.
paneKeyMode :: ExtendedKeys -> Int -> Int
paneKeyMode ext asked = case ext of
    ExtendedOff -> 0
    ExtendedOn -> asked
    ExtendedAlways -> if asked == 2 then 2 else 1

-- | tmux's @input_key@ for a modified character key (codepoint and xterm
-- modifier parameter) in a pane key mode: mode 2 spells every one in the
-- extended form; mode 1 keeps the ones with a usable legacy spelling (Alt
-- alone, and Ctrl with a key that has a control code); 'Nothing' means the
-- legacy bytes.
extendedKeyBytes :: ExtendedFormat -> Int -> Int -> Int -> Maybe ByteString
extendedKeyBytes fmt mode code param = case mode of
    2 -> Just spelled
    1 | altOnly || ctrlLegacy -> Nothing
      | otherwise -> Just spelled
    _ -> Nothing
  where
    bits = max 0 (param - 1)
    ctrl = testBit bits 2
    altOnly = testBit bits 1 && not ctrl
    ctrlLegacy = ctrl &&
        (code `elem` map ord " /@^" || (code >= ord '2' && code <= ord '8')
         || (code >= ord '@' && code <= ord '~'))
    spelled = B8.pack $ case fmt of
        FormatXterm -> "\ESC[27;" <> show param <> ";" <> show code <> "~"
        FormatCsiU -> "\ESC[" <> show code <> ";" <> show param <> "u"

-- | The (codepoint, xterm modifier param) a canonical key name denotes, for
-- the keys an extended-key sequence can carry — what a pane's key protocol
-- re-encodes on delivery. 'Nothing' for a name with no modifiers, or whose
-- base is not a codepoint (an arrow, a function key).
extendedKeyCode :: Text -> Maybe (Int, Int)
extendedKeyCode t = do
    (bits, base) <- modifiedName t
    (\c -> (c, bits + 1)) <$> baseKeyCode base

-- | The key a modified character name denotes (@C-Enter@, @C-S-s@, @M-Space@),
-- in any prefix order and any case: the same canonical key the tokenizer makes
-- of that key's wire form, so a binding written this way fires on both.
extendedByName :: Text -> Maybe Key
extendedByName t = do
    (bits, base) <- modifiedName t
    code <- baseKeyCode base
    extendedKeyOf code (bits + 1)

-- A key name's modifier bitmask and the base name under it; 'Nothing' when it
-- carries no modifier prefix at all.
modifiedName :: Text -> Maybe (Int, Text)
modifiedName = go 0
  where
    go bits t
        | Just rest <- stripCI "c-" t = go (bits .|. 4) rest
        | Just rest <- stripCI "m-" t = go (bits .|. 2) rest
        | Just rest <- stripCI "s-" t = go (bits .|. 1) rest
        | bits == (0 :: Int) = Nothing
        | otherwise = Just (bits, t)
    stripCI p t =
        let (pre, rest) = T.splitAt (T.length p) t
        in if T.toLower pre == p then Just rest else Nothing

-- The codepoint a base key name denotes: one of the specials tmux names
-- (case-insensitively, as tmux matches them) or a printable character.
baseKeyCode :: Text -> Maybe Int
baseKeyCode t
    | Just c <- lookup (T.toLower t) specials = Just c
    | T.length t == 1, c <- ord (T.head t), c >= 0x21, c <= 0x7e = Just c
    | otherwise = Nothing
  where
    specials =
        [ ("enter", 13), ("tab", 9), ("escape", 27)
        , ("space", 32), ("bspace", 127) ]

-- | A key binding: the commands it runs and whether it repeats.
data Binding = Binding
    { commands :: [[Text]]
    , repeats  :: Repeat
    , note     :: Maybe Text  -- ^ @bind -N@: what the binding does, for @list-keys -N@
    }
    deriving (Eq, Show)

-- | Whether a binding keeps its table open for @repeat-time@ after it runs
-- (@bind -r@).
data Repeat = Repeats | Once
    deriving (Eq, Show)

-- | table name -> key name -> binding
type Keymap = Map Text (Map Text Binding)

-- | A client's place in the key tables (tmux's @c->keytable@ and its repeat
-- and bracketed-paste flags). Times are monotonic milliseconds.
data KeyState = KeyState
    { table :: Maybe Text
        -- ^ the table the client switched to; 'Nothing' is the default table
        --   (@key-table@)
    , tableSince :: Int
        -- ^ when 'table' was entered, for @prefix-timeout@
    , repeating :: Maybe Repeating
    , bracketPasting :: Bool
        -- ^ inside the outer terminal's bracketed paste
    , mouse :: MouseState
    , mouseEvent :: Maybe MouseEvent
        -- ^ the mouse key being acted on, for the commands it runs
    , drag :: Maybe Drag
        -- ^ what a mouse drag in progress moves (tmux's
        --   @mouse_drag_update@)
    , commandState :: Maybe (SessionId, WindowId, PaneId)
        -- ^ the pane the commands being run act on by default, when it is
        --   not the client's own (tmux's queue state, as a menu sets it)
    }
    deriving (Eq, Show)

-- | An open repeat window: the key whose binding repeats, and when the
-- window closes.
data Repeating = Repeating
    { key   :: Text
    , expires :: Int
    }
    deriving (Eq, Show)

initialKeyState :: KeyState
initialKeyState = KeyState
    { table = Nothing, tableSince = 0, repeating = Nothing
    , bracketPasting = False, mouse = initialMouseState, mouseEvent = Nothing
    , drag = Nothing, commandState = Nothing }

-- | Whether the client is in the prefix table.
prefixArmed :: KeyState -> Bool
prefixArmed ks = ks.table == Just "prefix"

-- | tmux's repeat timer: once the window has closed, the client is back in
-- its default table.
expireRepeat :: Int -> KeyState -> KeyState
expireRepeat now ks = case ks.repeating of
    Just r | now >= r.expires -> ks { table = Nothing, repeating = Nothing }
    _ -> ks

-- | What the client's session options and its active pane say about routing
-- a key, at the moment it arrives.
data KeyContext = KeyContext
    { prefixKeys :: [Text]
        -- ^ @prefix@ and @prefix2@, without a @None@
    , defaultTable :: Text            -- ^ @key-table@
    , modeTable :: Maybe Text         -- ^ the active pane's mode table, if any
    , prefixTimeout :: Int            -- ^ ms; 0 never times out
    , repeatTime :: Int               -- ^ ms
    , initialRepeatTime :: Int        -- ^ ms; 0 uses @repeat-time@
    , mouseOn :: Bool                 -- ^ @mouse@
    , now :: Int
    }

-- | What a key should do. Consecutive passthroughs coalesce into one write.
data KeyAction
    = Passthrough ByteString
    | Paste
        -- ^ the key is part of a bracketed paste, or brackets one: its bytes
        --   as typed go to the pane, unless it is in a mode
    | RunCommands [[Text]]
    deriving (Eq, Show)

-- | Route one key through the key tables, as tmux's
-- @server_client_key_callback@ does: a prefix key switches to the prefix
-- table; a key bound in the current table runs its binding (staying in the
-- table while a @bind -r@ binding repeats); a miss in any table but the
-- default falls back to it; a key that reaches the default table unbound is
-- passed to the pane, unless it started in another table. A pane in a mode
-- puts the mode's table in front of the default one. A bracketed paste
-- bypasses the tables altogether.
routeKey :: KeyContext -> Keymap -> KeyState -> Key -> (KeyState, [KeyAction])
routeKey ctx km st0 k
    | k.name == "PasteStart" = (st1 { bracketPasting = True }, [Paste])
    | k.name == "PasteEnd" = (st1 { bracketPasting = False }, [Paste])
    | st1.bracketPasting = (st1, [Paste])
    | isMouseKey k.name && not ctx.mouseOn = (st1, [Passthrough k.raw])
    | otherwise = tableChanged first0 first0 st1
  where
    st1 = expireRepeat ctx.now st0
    current s = fromMaybe ctx.defaultTable s.table
    isDefault t = t == ctx.defaultTable
    first0 = case ctx.modeTable of
        Just m | isDefault (current st1) -> m
        _ -> current st1
    reset s = s { table = Nothing, tableSince = ctx.now, repeating = Nothing }
    tableChanged first tbl st
        | k.name `elem` ctx.prefixKeys && tbl /= "prefix" =
            (st { table = Just "prefix", tableSince = ctx.now }, [])
        | ctx.prefixTimeout > 0, tbl == "prefix"
        , ctx.now - st.tableSince > ctx.prefixTimeout
        , not (repeatingWith bd st) = retry (reset st)
        | Just b <- bd = case st.repeating of
            Just _ | b.repeats == Once -> retry (reset st)
            _ -> (fire b st, [RunCommands b.commands])
        -- Motion is only reported when an app asks, so binding it is
        -- useless: it never leaves the prefix table.
        | isMoveKey k.name = (st, [Passthrough k.raw])
        | not (isDefault tbl) || isJust st.repeating =
            let st' = reset st
                first' = if isJust st.repeating then current st' else first
            in tableChanged first' (current st') st'
        | first /= tbl = (reset st, [])
        | otherwise = (st, [Passthrough k.raw])
      where
        bd = Map.lookup tbl km >>= Map.lookup k.name
        retry st' = tableChanged (current st') (current st') st'
    repeatingWith bd st = case (bd, st.repeating) of
        (Just b, Just _) -> b.repeats == Repeats
        _ -> False
    fire b st = case repeatFor b st of
        0 -> reset st
        r -> st { repeating = Just Repeating { key = k.name, expires = ctx.now + r } }
    -- tmux's @server_client_repeat_time@: @initial-repeat-time@ opens a
    -- fresh repeat, @repeat-time@ extends one already running on this key.
    repeatFor b st
        | b.repeats == Once || ctx.repeatTime == 0 = 0
        | Just r <- st.repeating, r.key == k.name = ctx.repeatTime
        | ctx.initialRepeatTime /= 0 = ctx.initialRepeatTime
        | otherwise = ctx.repeatTime

-- | 'routeKey' over a run of keys arriving together.
routeKeys :: KeyContext -> Keymap -> KeyState -> [Key] -> (KeyState, [KeyAction])
routeKeys ctx km = go
  where
    go st [] = (st, [])
    go st (k : ks) =
        let (st', as) = routeKey ctx km st k
            (st'', rest) = go st' ks
        in (st'', foldr merge rest as)
    merge (Passthrough a) (Passthrough b : rest) = Passthrough (a <> b) : rest
    merge a rest = a : rest

-- | Key names as written in configs: @x@, @C-b@, @M-n@, @Up@, @C-Space@.
-- Named keys (@Space@, @Enter@, @Up@, ...) match case-insensitively, as
-- tmux does; the caret form @^h@ is an alias for @C-h@.
parseKeyName :: Text -> Maybe Key
parseKeyName t
    | Just n <- userKeyNumber t = Just (mkKey ("User" <> T.pack (show n)) "")
    | Just n <- mouseKeyByName t = Just (mkKey n "")
    | Just (n, r) <- lookupNamed t = Just (mkKey n r)
    | Just (n, r) <- lookupModified t = Just (mkKey n r)
    | Just k <- extendedByName t = Just k
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
        -- ctrl punctuation (^], ^\, ...): same key as the C- spelling.
        | otherwise = extendedByName ("C-" <> rest)
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
