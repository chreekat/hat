-- | A client terminal's capabilities, as tmux models them (@tty-term.c@,
-- @tty-features.c@): the terminfo entries the client read for its @TERM@,
-- amended by the server's @terminal-features@ and @terminal-overrides@
-- options. Pure: the client reads terminfo ("Hat.Client.Terminfo"), the
-- server resolves the term ('createTerm') and hands it back for the
-- client's encoder ("Hat.Client.Draw").
module Hat.Term.Caps
    ( -- * Capabilities
      CapType (..)
    , CapValue (..)
    , capCodes
    , TtyTerm (..)
    , hasCap
    , capString
    , capNumber
    , capFlag
      -- * Features
    , Feature (..)
    , featureName
    , featurePresent
    , featureNames
    , appliedFeatures
      -- * Resolving
    , createTerm
    , vt100Like
      -- * Identifying
    , Identification (..)
    , identificationQueries
    , parseIdentification
    , identify
    , overrideEntries
    , unvis
      -- * Derived flags (tmux's @TERM_*@)
    , rgbColours
    , colours256
    , noAutoMargin
    , clientColourCount
      -- * Transport
    , capList
    , termFromWire
    ) where

import Data.Bits ((.&.), (.|.))
import Data.Char (isDigit)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR

import Hat.Glob (globMatch)

-- | How a terminfo capability holds its value.
data CapType = CapString | CapNumber | CapFlag
    deriving (Eq, Show)

data CapValue = StrCap Text | NumCap Int | FlagCap Bool
    deriving (Eq, Show)

-- | Every capability tmux knows (@tty_term_codes@), in its order: the only
-- names a client reads, an override sets, or @#{I/c:...}@ answers for.
capCodes :: [(Text, CapType)]
capCodes =
    [ ("acsc", CapString), ("am", CapFlag), ("AX", CapFlag), ("bce", CapFlag)
    , ("bel", CapString), ("Bidi", CapString), ("blink", CapString)
    , ("bold", CapString), ("civis", CapString), ("clear", CapString)
    , ("Clmg", CapString), ("Cmg", CapString), ("cnorm", CapString)
    , ("colors", CapNumber), ("Cr", CapString), ("csr", CapString)
    , ("Cs", CapString), ("cub1", CapString), ("cub", CapString)
    , ("cud1", CapString), ("cud", CapString), ("cuf1", CapString)
    , ("cuf", CapString), ("cup", CapString), ("cuu1", CapString)
    , ("cuu", CapString), ("cvvis", CapString), ("dch1", CapString)
    , ("dch", CapString), ("dim", CapString), ("dl1", CapString)
    , ("dl", CapString), ("Dseks", CapString), ("Dsfcs", CapString)
    , ("Dsbp", CapString), ("Dsmg", CapString), ("E3", CapString)
    , ("ech", CapString), ("ed", CapString), ("el1", CapString)
    , ("el", CapString), ("enacs", CapString), ("Enbp", CapString)
    , ("Eneks", CapString), ("Enfcs", CapString), ("Enmg", CapString)
    , ("fsl", CapString), ("Hls", CapString), ("home", CapString)
    , ("hpa", CapString), ("ich1", CapString), ("ich", CapString)
    , ("il1", CapString), ("il", CapString), ("indn", CapString)
    , ("invis", CapString), ("kcbt", CapString), ("kcub1", CapString)
    , ("kcud1", CapString), ("kcuf1", CapString), ("kcuu1", CapString) ]
    <> modKeys "kDC" <> [("kdch1", CapString)] <> modKeys "kDN"
    <> modKeys "kEND" <> [("kend", CapString)]
    <> [ ("kf" <> T.pack (show n), CapString) | n <- [1 .. 63 :: Int] ]
    <> modKeys "kHOM" <> [("khome", CapString)] <> modKeys "kIC"
    <> [("kich1", CapString), ("kind", CapString)] <> modKeys "kLFT"
    <> [("kmous", CapString), ("knp", CapString)] <> modKeys "kNXT"
    <> [("kpp", CapString)] <> modKeys "kPRV" <> modKeys "kRIT"
    <> [("kri", CapString)] <> modKeys "kUP" <>
    [ ("Ms", CapString), ("Nobr", CapString), ("ol", CapString)
    , ("op", CapString), ("Rect", CapString), ("rev", CapString)
    , ("RGB", CapFlag), ("rin", CapString), ("ri", CapString)
    , ("rmacs", CapString), ("rmcup", CapString), ("rmkx", CapString)
    , ("setab", CapString), ("setaf", CapString), ("setal", CapString)
    , ("setrgbb", CapString), ("setrgbf", CapString), ("Setulc", CapString)
    , ("Setulc1", CapString), ("Se", CapString), ("Sxl", CapFlag)
    , ("sgr0", CapString), ("sitm", CapString), ("smacs", CapString)
    , ("smcup", CapString), ("smkx", CapString), ("Smol", CapString)
    , ("smso", CapString), ("Smulx", CapString), ("smul", CapString)
    , ("smxx", CapString), ("Spb", CapString), ("Ss", CapString)
    , ("Swd", CapString), ("Sync", CapString), ("Tc", CapFlag)
    , ("tsl", CapString), ("U8", CapNumber), ("vpa", CapString)
    , ("XT", CapFlag)
    ]
  where
    modKeys k = [ (k <> m, CapString) | m <- ["", "3", "4", "5", "6", "7"] ]

capTypes :: Map Text CapType
capTypes = Map.fromList capCodes

-- | A client's terminal: its @TERM@, the capabilities it has, and the
-- features applied to it (tmux's @struct tty_term@ and the client's
-- @term_features@).
data TtyTerm = TtyTerm
    { name :: Text
    , caps :: Map Text CapValue
    , features :: Set Feature
    , termType :: Text  -- ^ the name and version the terminal gave for itself
    }
    deriving (Eq, Show)

hasCap :: Text -> TtyTerm -> Bool
hasCap n t = Map.member n t.caps

-- | A string capability's value; empty when absent.
capString :: Text -> TtyTerm -> Text
capString n t = case Map.lookup n t.caps of
    Just (StrCap s) -> s
    _ -> ""

-- | A numeric capability's value; 0 when absent.
capNumber :: Text -> TtyTerm -> Int
capNumber n t = case Map.lookup n t.caps of
    Just (NumCap v) -> v
    _ -> 0

-- | A flag capability's value; off when absent.
capFlag :: Text -> TtyTerm -> Bool
capFlag n t = Map.lookup n t.caps == Just (FlagCap True)

-- | tmux's named terminal features, in its order (@tty_features@): the
-- order @client_termfeatures@ lists them in.
data Feature
    = F256 | FBpaste | FCcolour | FClipboard | FHyperlinks | FCstyle
    | FExtkeys | FFocus | FIgnorefkeys | FMargins | FMouse | FOsc7
    | FOverline | FProgressbar | FRectfill | FRGB | FSixel
    | FStrikethrough | FSync | FTitle | FUsstyle
    deriving (Eq, Ord, Show, Enum, Bounded)

featureName :: Feature -> Text
featureName = \case
    F256 -> "256"
    FBpaste -> "bpaste"
    FCcolour -> "ccolour"
    FClipboard -> "clipboard"
    FHyperlinks -> "hyperlinks"
    FCstyle -> "cstyle"
    FExtkeys -> "extkeys"
    FFocus -> "focus"
    FIgnorefkeys -> "ignorefkeys"
    FMargins -> "margins"
    FMouse -> "mouse"
    FOsc7 -> "osc7"
    FOverline -> "overline"
    FProgressbar -> "progressbar"
    FRectfill -> "rectfill"
    FRGB -> "RGB"
    FSixel -> "sixel"
    FStrikethrough -> "strikethrough"
    FSync -> "sync"
    FTitle -> "title"
    FUsstyle -> "usstyle"

-- | The capabilities a feature adds, in tmux's override syntax.
featureCaps :: Feature -> [Text]
featureCaps = \case
    F256 -> ["AX", setab, setaf]
    FBpaste -> ["Enbp=\\E[?2004h", "Dsbp=\\E[?2004l"]
    FCcolour -> ["Cs=\\E]12;%p1%s\\a", "Cr=\\E]112\\a"]
    FClipboard -> ["Ms=\\E]52;%p1%s;%p2%s\\a"]
    FHyperlinks -> ["Hls=\\E]8;%?%p1%l%tid=%p1%s%;;%p2%s\\E\\\\"]
    FCstyle -> ["Ss=\\E[%p1%d q", "Se=\\E[2 q"]
    FExtkeys -> ["Eneks=\\E[>4;2m", "Dseks=\\E[>4m"]
    FFocus -> ["Enfcs=\\E[?1004h", "Dsfcs=\\E[?1004l"]
    FIgnorefkeys -> [ "kf" <> T.pack (show n) <> "@" | n <- [0 .. 63 :: Int] ]
    FMargins ->
        ["Enmg=\\E[?69h", "Dsmg=\\E[?69l", "Clmg=\\E[s", "Cmg=\\E[%i%p1%d;%p2%ds"]
    FMouse -> ["kmous=\\E[M"]
    FOsc7 -> ["Swd=\\E]7;", "fsl=\\a"]
    FOverline -> ["Smol=\\E[53m"]
    FProgressbar -> ["Spb=\\E]9;4;%p1%d;%p2%d\\E\\\\"]
    FRectfill -> ["Rect"]
    FRGB ->
        [ "AX", "setrgbf=\\E[38;2;%p1%d;%p2%d;%p3%dm"
        , "setrgbb=\\E[48;2;%p1%d;%p2%d;%p3%dm", setab, setaf ]
    FSixel -> ["Sxl"]
    FStrikethrough -> ["smxx=\\E[9m"]
    FSync -> ["Sync=\\E[?2026%?%p1%{1}%-%tl%eh%;"]
    FTitle -> ["tsl=\\E]0;", "fsl=\\a"]
    FUsstyle ->
        [ "Smulx=\\E[4::%p1%dm"
        , "Setulc=\\E[58::2::%p1%{65536}%/%d::%p1%{256}%/%{255}%&%d::%p1%{255}%&%d%;m"
        , "Setulc1=\\E[58::5::%p1%dm", "ol=\\E[59m" ]
  where
    setab = "setab=\\E[%?%p1%{8}%<%t4%p1%d%e%p1%{16}%<%t10%p1%{8}%-%d%e48;5;%p1%d%;m"
    setaf = "setaf=\\E[%?%p1%{8}%<%t3%p1%d%e%p1%{16}%<%t9%p1%{8}%-%d%e38;5;%p1%d%;m"

-- | The terminal flags a feature needs before its capabilities alone can
-- vouch for it (the @flags@ of tmux's @struct tty_feature@).
data FeatureFlag = Need256 | NeedRGB | NeedDECSLRM | NeedDECFRA | NeedSixel
    deriving (Eq, Show)

featureFlags :: Feature -> [FeatureFlag]
featureFlags = \case
    FRGB -> [Need256, NeedRGB]
    F256 -> [Need256]
    FMargins -> [NeedDECSLRM]
    FRectfill -> [NeedDECFRA]
    FSixel -> [NeedSixel]
    _ -> []

termHasFlag :: TtyTerm -> FeatureFlag -> Bool
termHasFlag t = \case
    Need256 -> colours256 t
    NeedRGB -> rgbColours t
    NeedDECSLRM -> hasCap "Cmg" t && hasCap "Clmg" t
    NeedDECFRA -> hasCap "Rect" t
    NeedSixel -> Set.member FSixel t.features

-- | tmux's @TERM_RGBCOLOURS@: the terminal can set 24-bit colours.
rgbColours :: TtyTerm -> Bool
rgbColours t = hasCap "setrgbf" t && hasCap "setrgbb" t

-- | tmux's @TERM_256COLOURS@: only ever set by the @256@ or @RGB@ feature.
colours256 :: TtyTerm -> Bool
colours256 t = Set.member F256 t.features || Set.member FRGB t.features

-- | tmux's @TERM_NOAM@: no automatic right margin.
noAutoMargin :: TtyTerm -> Bool
noAutoMargin t = not (capFlag "am" t)

-- | @client_colours@ (tmux's @format_cb_client_colours@).
clientColourCount :: TtyTerm -> Int
clientColourCount t
    | rgbColours t = 16777216
    | colours256 t = 256
    | n < 8 = 2
    | n < 16 = 8
    | otherwise = 16
  where
    n = capNumber "colors" t

-- | @client_termfeatures@: the applied features' names, comma-joined.
featureNames :: TtyTerm -> Text
featureNames = T.intercalate "," . appliedFeatures

-- | The applied features' names, in tmux's order.
appliedFeatures :: TtyTerm -> [Text]
appliedFeatures t = map featureName (Set.toAscList t.features)

-- | @#{I/f:name}@ (tmux's @tty_feature_present@): applied, or every one
-- of its capabilities (and flags) present anyway. As in tmux, a name no
-- feature has is looked up as the last feature in the table.
featurePresent :: Text -> TtyTerm -> Bool
featurePresent n t = case List.find ((== n) . featureName) allFeatures of
    Just f | Set.member f t.features -> True
    found ->
        let f = fromMaybe maxBound found
        in f /= FIgnorefkeys && n /= "ignorefkeys"
            && all (termHasFlag t) (featureFlags f)
            && all (\c -> hasCap (T.takeWhile (/= '=') c) t) (featureCaps f)

allFeatures :: [Feature]
allFeatures = [minBound .. maxBound]

-- | Add the named features from a list (tmux's @tty_add_features@):
-- matched case-insensitively; the first unknown name ends the list.
addFeatures :: [Text] -> Set Feature -> Set Feature
addFeatures names fs = case names of
    [] -> fs
    n : rest -> case List.find ((== T.toLower n) . T.toLower . featureName) allFeatures of
        Nothing -> fs
        Just f -> addFeatures rest (Set.insert f fs)

-- | Split an override or feature entry at its unescaped colons (tmux's
-- @tty_term_override_next@): @::@ is a literal colon.
overrideEntries :: Text -> [Text]
overrideEntries s0
    | T.null s0 = []
    | otherwise = go s0
  where
    go s =
        let (field, rest) = takeField s
        in field : case T.uncons rest of
            Just (':', more) | not (T.null more) -> go more
            _ -> []
    takeField s = case T.break (== ':') s of
        (pre, rest) | Just (':', more) <- T.uncons rest
                    , Just (':', more') <- T.uncons more ->
                        let (f, r) = takeField more'
                        in (pre <> ":" <> f, r)
        r -> r

-- | Apply one override entry list's capabilities (tmux's
-- @tty_term_apply@): @name=value@ sets, @name@ sets an empty string or a
-- flag, @name\@@ removes. Unknown names are ignored, as is a number that
-- does not parse.
applyCaps :: [Text] -> Map Text CapValue -> Map Text CapValue
applyCaps entries m0 = List.foldl' one m0 (filter (not . T.null) entries)
  where
    one m s = case T.breakOn "=" s of
        (n, rest) | not (T.null rest) -> set n (unvisOr (T.drop 1 rest)) m
        _ | Just n <- T.stripSuffix "@" s -> Map.delete n m
          | otherwise -> set s "" m
    set n v m = case Map.lookup n capTypes of
        Just CapString -> Map.insert n (StrCap v) m
        Just CapNumber -> maybe m (\k -> Map.insert n (NumCap k) m) (number v)
        Just CapFlag -> Map.insert n (FlagCap True) m
        Nothing -> m
    unvisOr v = fromMaybe v (unvis v)

-- | A non-negative decimal no larger than @INT_MAX@ (tmux's @strtonum@).
number :: Text -> Maybe Int
number v = case TR.signed TR.decimal v of
    Right (k, "") | k >= 0, k <= (2147483647 :: Integer) -> Just (fromIntegral k)
    _ -> Nothing

-- | Decode @vis(3)@ escapes (OpenBSD's @strunvis@): @\\E@ for escape,
-- @\\n@, @\\a@ and the like, octal, @\\^X@ controls and @\\M-@ meta.
-- 'Nothing' on a malformed escape.
unvis :: Text -> Maybe Text
unvis = fmap T.pack . go . T.unpack
  where
    go = \case
        [] -> Just []
        '\\' : rest -> escape rest
        c : rest -> (c :) <$> go rest
    escape = \case
        [] -> Just []
        c : rest -> case c of
            '\\' -> ('\\' :) <$> go rest
            'n' -> ('\n' :) <$> go rest
            'r' -> ('\r' :) <$> go rest
            'b' -> ('\b' :) <$> go rest
            'a' -> ('\a' :) <$> go rest
            'v' -> ('\v' :) <$> go rest
            't' -> ('\t' :) <$> go rest
            'f' -> ('\f' :) <$> go rest
            's' -> (' ' :) <$> go rest
            'E' -> ('\ESC' :) <$> go rest
            '\n' -> go rest
            '$' -> go rest
            '^' -> control 0 rest
            'M' -> case rest of
                '-' : x : more -> (toEnum ((0x80 .|. fromEnum x) .&. 0xff) :) <$> go more
                '^' : more -> control 0x80 more
                [] -> Just []
                ['-'] -> Just []
                _ -> Nothing
            _ | isOctal c -> octal (digit c) (1 :: Int) rest
              | otherwise -> Nothing
    control base = \case
        '?' : more -> (toEnum (base .|. 0x7f) :) <$> go more
        x : more -> (toEnum ((base .|. (fromEnum x .&. 0x1f)) .&. 0xff) :) <$> go more
        [] -> Just []
    octal acc n = \case
        x : more | n < 3, isOctal x -> octal (acc * 8 + digit x) (n + 1) more
        more -> (toEnum (acc .&. 0xff) :) <$> go more
    isOctal x = x >= '0' && x <= '7'
    digit x = fromEnum x - fromEnum '0'

-- | Drop @$<...>@ padding from a terminfo string (tmux's
-- @tty_term_strip@).
stripPadding :: Text -> Text
stripPadding s = case T.breakOn "$<" s of
    (pre, rest)
        | T.null rest -> pre
        | otherwise -> pre <> stripPadding (T.drop 1 (T.dropWhile (/= '>') rest))

-- | Read the client's @name=value@ list (tmux's @MSG_IDENTIFY_TERMINFO@)
-- into capabilities, keeping only names tmux knows.
readCaps :: [Text] -> Map Text CapValue
readCaps = Map.fromList . mapMaybe one
  where
    one entry =
        let (n, rest) = T.breakOn "=" entry
            v = T.drop 1 rest
        in if T.null n then Nothing else case Map.lookup n capTypes of
            Just CapString -> Just (n, StrCap (stripPadding v))
            Just CapNumber -> (\k -> (n, NumCap k)) <$> number v
            Just CapFlag -> Just (n, FlagCap (T.take 1 v == "1"))
            Nothing -> Nothing

-- | Apply each @terminal-overrides@ entry whose pattern matches the term.
applyOverrides :: [Text] -> TtyTerm -> TtyTerm
applyOverrides overrides t = t
    { caps = List.foldl' (flip applyCaps) t.caps
        [ rest | o <- overrides, pat : rest <- [overrideEntries o]
               , globMatch False pat t.name ] }

-- | Build a client's terminal (tmux's @tty_term_create@) from its @TERM@,
-- the capabilities its client read, the @terminal-overrides@ and
-- @terminal-features@ option values, and its @COLORTERM@. A terminal that
-- cannot clear or position the cursor is refused.
createTerm :: Text -> [Text] -> [Text] -> [Text] -> Maybe Text -> Either Text TtyTerm
createTerm nm raw overrides featureOpt colorterm
    | not (hasCap "clear" t1) = Left "terminal does not support clear"
    | not (hasCap "cup" t1) = Left "terminal does not support cup"
    | otherwise = Right (applyFeatures feat2 t1)
  where
    t0 = TtyTerm { name = nm, caps = readCaps raw, features = Set.empty, termType = "" }
    fromOpt = List.foldl' (\fs o -> case overrideEntries o of
                    pat : rest | globMatch False pat nm -> addFeatures rest fs
                    _ -> fs)
                Set.empty featureOpt
    fromColorterm = case T.toLower <$> colorterm of
        Just v | v `elem` ["truecolor", "24bit"] -> Set.insert FRGB
               | "256" `T.isInfixOf` v -> Set.insert F256
        _ -> id
    feat0 = fromColorterm fromOpt
    t1 = applyOverrides overrides t0
    feat1 | vt100Like t1 = Set.union (Set.fromList [FBpaste, FFocus, FTitle]) feat0
          | otherwise = feat0
    feat2 | (capFlag "Tc" t1 || hasCap "RGB" t1)
            && not (hasCap "setrgbf" t1 && hasCap "setrgbb" t1) = Set.insert FRGB feat1
          | otherwise = feat1
    applyFeatures fs = addTermFeatures overrides (Set.toAscList fs)

-- | Apply the features a term does not have yet, then the overrides again
-- so they win (tmux's @tty_apply_features@ and @tty_term_apply_overrides@).
addTermFeatures :: [Text] -> [Feature] -> TtyTerm -> TtyTerm
addTermFeatures overrides fs t = case Set.toAscList fresh of
    [] -> t
    new -> applyOverrides overrides t
        { caps = List.foldl' (\m f -> applyCaps (featureCaps f) m) t.caps new
        , features = Set.union t.features fresh }
  where
    fresh = Set.fromList fs `Set.difference` t.features

-- | tmux's @TERM_VT100LIKE@: the terminal claims xterm descent, or clears
-- with a CSI sequence.
vt100Like :: TtyTerm -> Bool
vt100Like t = capFlag "XT" t || "\ESC[" `T.isPrefixOf` capString "clear" t

-- | A terminal's answer to the queries 'identificationQueries' sends:
-- primary or secondary device attributes, or its name and version
-- (XTVERSION).
data Identification = PrimaryDA [Int] | SecondaryDA [Int] | TerminalVersion Text
    deriving (Eq, Show)

-- | What tmux asks a VT100-like terminal when it attaches (tmux's
-- @tty_send_requests@): DA, DA2 and XTVERSION; nothing for any other.
identificationQueries :: TtyTerm -> Text
identificationQueries t
    | vt100Like t = "\ESC[c\ESC[>c\ESC[>q"
    | otherwise = ""

-- | Read a reply to 'identificationQueries': @ESC [ ? Ps c@, @ESC [ > Ps c@
-- or @ESC P > | text ESC \\@.
parseIdentification :: Text -> Maybe Identification
parseIdentification s
    | Just body <- T.stripPrefix "\ESC[?" s >>= T.stripSuffix "c" = Just (PrimaryDA (params body))
    | Just body <- T.stripPrefix "\ESC[>" s >>= T.stripSuffix "c" = Just (SecondaryDA (params body))
    | Just body <- T.stripPrefix "\ESCP>|" s >>= T.stripSuffix "\ESC\\" = Just (TerminalVersion body)
    | otherwise = Nothing
  where
    -- A parameter that is not a number counts as 0, as tmux's strtoul.
    params = map (\p -> fromMaybe 0 (number p)) . T.splitOn ";"

-- | Add the features an identification reveals (tmux's
-- @tty_keys_device_attributes@, @tty_keys_device_attributes2@ and
-- @tty_keys_extended_device_attributes@, then @tty_update_features@).
identify :: [Text] -> Identification -> TtyTerm -> TtyTerm
identify overrides ident t = case ident of
    PrimaryDA (level : ps) | level >= 61, level <= 65 ->
        addTermFeatures overrides (mapMaybe (`lookup` daFeatures) ps) t
    PrimaryDA _ -> t
    SecondaryDA (kind : _) -> addTermFeatures overrides (knownFeatures (case kind of
        77 -> "mintty"
        84 -> "tmux"
        85 -> "rxvt-unicode"
        _ -> "")) t
    SecondaryDA [] -> t
    TerminalVersion v -> (addTermFeatures overrides (knownFeatures (versionName v)) t)
        { termType = v }
  where
    daFeatures = [(4, FSixel), (21, FMargins), (28, FRectfill), (52, FClipboard)]
    versionName v = maybe "" snd $ List.find ((`T.isPrefixOf` v) . fst)
        [ ("iTerm2 ", "iTerm2"), ("tmux ", "tmux"), ("XTerm(", "XTerm")
        , ("mintty ", "mintty"), ("foot(", "foot"), ("WezTerm ", "WezTerm")
        , ("ghostty ", "ghostty") ]

-- | The features tmux knows a recognised terminal has (tmux's
-- @tty_default_features@).
knownFeatures :: Text -> [Feature]
knownFeatures = \case
    "mintty" -> base <> [FCcolour, FCstyle, FExtkeys, FMargins, FOverline, FUsstyle]
    "tmux" -> base <> [FCcolour, FCstyle, FExtkeys, FFocus, FOverline, FUsstyle
        , FHyperlinks, FProgressbar]
    "rxvt-unicode" -> [F256, FBpaste, FCcolour, FCstyle, FMouse, FTitle, FIgnorefkeys]
    "iTerm2" -> base <> [FCstyle, FExtkeys, FMargins, FUsstyle, FSync, FOsc7
        , FHyperlinks, FProgressbar]
    "foot" -> base <> [FCcolour, FCstyle, FExtkeys, FUsstyle, FSync, FOsc7, FHyperlinks]
    "WezTerm" -> base <> [FCcolour, FCstyle, FExtkeys, FFocus, FHyperlinks, FUsstyle]
    "ghostty" -> base <> [FCcolour, FCstyle, FExtkeys, FFocus, FOverline, FHyperlinks
        , FOsc7, FSync, FUsstyle, FProgressbar]
    "XTerm" -> base <> [FCcolour, FCstyle, FExtkeys, FFocus]
    _ -> []
  where
    base = [F256, FRGB, FBpaste, FClipboard, FMouse, FStrikethrough, FTitle]

-- | A term's capabilities as @name=value@ entries, the form 'termFromWire'
-- reads back: numbers in decimal, flags as @1@ or @0@.
capList :: TtyTerm -> [Text]
capList t =
    [ n <> "=" <> v
    | (n, _) <- capCodes, Just cv <- [Map.lookup n t.caps]
    , let v = case cv of
            StrCap s -> s
            NumCap k -> T.pack (show k)
            FlagCap b -> if b then "1" else "0" ]

-- | A resolved term from its name, 'capList' and feature names. Unknown
-- capability and feature names are skipped.
termFromWire :: Text -> [Text] -> [Text] -> TtyTerm
termFromWire nm cs fs = TtyTerm
    { name = nm
    , termType = ""
    , caps = Map.fromList (mapMaybe exact cs)
    , features = Set.fromList
        [ f | n <- fs, f <- allFeatures, featureName f == n ]
    }
  where
    exact entry =
        let (n, rest) = T.breakOn "=" entry
            v = T.drop 1 rest
        in case Map.lookup n capTypes of
            Just CapString -> Just (n, StrCap v)
            Just CapNumber | T.all isDigit v, not (T.null v) -> (\k -> (n, NumCap k)) <$> number v
            Just CapFlag -> Just (n, FlagCap (v == "1"))
            _ -> Nothing
