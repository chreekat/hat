-- | Typed option values from user text, as tmux's @options_from_string@ and
-- @options_array_*@ parse them: each registry kind's validation, canonical
-- spelling, and error message, plus hat's implementation gate.
module Hat.Server.OptionParse
    ( SetMode (..)
    , parseScalar
    , supportedValue
    , assignArray
    , setArrayKey
    ) where

import Data.Char (isSpace)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR

import Hat.Command.Parser (parseScript)
import Hat.Command.Script (Script (..))
import Hat.Model.OptionTable
import Hat.Colour (colourFromText, colourToText)
import Hat.Server.Keys (Key (..), parseKeyName)
import Hat.Server.Style (colourValid, styleValid)

-- | Whether a @set-option@ replaces the value or extends it (@-a@).
data SetMode = Assign | Append
    deriving (Eq, Show)

-- | A scalar option's new canonical value.
parseScalar
    :: OptionDef
    -> SetMode
    -> Maybe Text  -- ^ the scope's own value, which an append extends
    -> Text        -- ^ the value in effect, which a bare flag or choice toggles
    -> Maybe Text  -- ^ the value given, if any
    -> Either Text Text
parseScalar d mode own current mvalue = case (d.kind, mvalue) of
    (KFlag, _) -> flagValue current mvalue
    (KChoice cs, _) -> choiceValue cs current (choiceSpelling d <$> mvalue)
    (_, Nothing) -> Left "empty value"
    (KString, Just v) ->
        let new = appended "" v
        in if maybe True (`fnmatch` new) d.pattern then Right new
           else Left ("value is invalid: " <> new)
    (KStyle, Just v) ->
        let new = appended "," v
        in if formatBearing new || styleValid new then Right new
           else Left ("invalid style: " <> new)
    (KColourString, Just v) ->
        let new = appended "" v
        in if formatBearing new || T.null new || colourValid new then Right new
           else Left ("invalid colour: " <> new)
    (KNumber lo hi, Just v) -> T.pack . show <$> strtonum lo hi v
    (KKey, Just v) -> keyValue v
    (KColour, Just v) -> colourValue v
    (KCommand, Just v) -> commandValue v
  where
    appended sep v = case (mode, own) of
        (Append, Just old) -> old <> sep <> v
        _ -> v
    formatBearing = T.isInfixOf "#{"

-- | @fnmatch(3)@ with no flags: @*@, @?@, bracket sets (ranges, @!@\/@^@
-- negation) and backslash escapes.
fnmatch :: Text -> Text -> Bool
fnmatch pat = go (T.unpack pat) . T.unpack
  where
    go [] s = null s
    go ('*' : ps) s = any (go ps) (List.tails s)
    go ('?' : ps) (_ : s) = go ps s
    go ('[' : ps) (c : s)
        | Just (matched, rest) <- bracket ps c = matched && go rest s
    go ('\\' : p : ps) (c : s) = p == c && go ps s
    go (p : ps) (c : s) = p == c && go ps s
    go _ [] = False
    -- A set's members up to its closing @]@ (a leading @]@ is a member);
    -- 'Nothing' when unterminated, so the @[@ matches itself.
    bracket ps c = do
        let (neg, body) = case ps of
                x : r | x `elem` ("!^" :: String) -> (True, r)
                _ -> (False, ps)
        (members, rest) <- splitSet body
        pure (inSet members /= neg, rest)
      where
        inSet = \case
            a : '-' : b : r -> (a <= c && c <= b) || inSet r
            a : r -> a == c || inSet r
            [] -> False
    splitSet body = case body of
        ']' : r -> first (']' :) <$> close r
        _ -> close body
    close = \case
        ']' : r -> Just ([], r)
        x : r -> first (x :) <$> close r
        [] -> Nothing
    first f (a, b) = (f a, b)

flagValue :: Text -> Maybe Text -> Either Text Text
flagValue current = \case
    Nothing -> Right (toggled current)
    Just "" -> Right (toggled current)
    Just v
        | lv `elem` ["1", "on", "yes"] -> Right "on"
        | lv `elem` ["0", "off", "no"] -> Right "off"
        | otherwise -> Left ("bad value: " <> v)
      where
        lv = T.toLower v
  where
    toggled c = if c == "on" then "off" else "on"

-- | A choice by name; with no value, the first two choices toggle (tmux
-- flips index 0 and 1 and leaves any later choice where it is).
choiceValue :: [Text] -> Text -> Maybe Text -> Either Text Text
choiceValue cs current = \case
    Nothing -> Right $ case List.elemIndex current cs of
        Just 0 | (_ : c1 : _) <- cs -> c1
        Just 1 | (c0 : _) <- cs -> c0
        _ -> current
    Just v
        | v `elem` cs -> Right v
        | otherwise -> Left ("unknown value: " <> v)

keyValue :: Text -> Either Text Text
keyValue v
    | T.toLower v == "none" = Right "None"
    | Just k <- parseKeyName v = Right k.name
    | otherwise = Left ("bad key: " <> v)

colourValue :: Text -> Either Text Text
colourValue v = case colourFromText v of
    Just c -> Right (colourToText c)
    Nothing
        | T.toLower v `elem` themeColours -> Right (T.toLower v)
        | otherwise -> Left ("bad colour: " <> v)
  where
    themeColours =
        [ "themeblack", "themewhite", "themelightgrey", "themedarkgrey"
        , "themegreen", "themeyellow", "themered", "themeblue", "themecyan"
        , "thememagenta" ]

-- | @strtonum(3)@ with tmux's message wording.
strtonum :: Integer -> Integer -> Text -> Either Text Integer
strtonum lo hi v = case TR.signed TR.decimal (T.dropWhile isSpace v) of
    Right (n, "")
        | n < lo -> Left ("value is too small: " <> v)
        | n > hi -> Left ("value is too large: " <> v)
        | otherwise -> Right n
    _ -> Left ("value is invalid: " <> v)

-- | Refuse a valid value of a live option that hat does not honour yet,
-- and any value of an option that is not live at all.
supportedValue :: OptionDef -> Text -> Either Text ()
supportedValue d v = case d.gate of
    NotYet -> Left (notYetError d.name)
    Live s
        | Just cs <- s.onlyChoices, v `notElem` cs ->
            Left (d.name <> ": value not yet supported by hat: " <> v)
        | Just why <- s.refuse v -> Left why
        | otherwise -> Right ()

-- | One array entry, validated and canonicalised by the option's kind.
arrayEntry :: OptionDef -> Text -> Either Text Text
arrayEntry d v = case d.kind of
    KColour -> colourValue v
    KCommand -> commandValue v
    _ -> Right v

-- | A command option's value must parse as a command list; tmux reports
-- any malformed one as a syntax error.
commandValue :: Text -> Either Text Text
commandValue v = case (parseScript v).failure of
    Nothing -> Right v
    Just _ -> Left "syntax error"

-- | tmux's @options_array_assign@: split on the option's separator
-- characters (skipping empty pieces) and place each piece at the first free
-- index. An empty separator places the whole value as one entry.
assignArray :: OptionDef -> Map ArrayKey Text -> Text -> Either Text (Map ArrayKey Text)
assignArray d current v
    | T.null v = Right current
    | otherwise = List.foldl' place (Right current) pieces
  where
    sep = maybe " ," T.unpack d.array
    pieces
        | null sep = [v]
        | otherwise = filter (not . T.null) (T.split (`elem` sep) v)
    place acc piece = do
        m <- acc
        entry <- arrayEntry d piece
        pure (Map.insert (firstFreeIndex m) entry m)

-- | tmux's @options_array_set@ for one key: replace it, or with 'Append'
-- extend a string entry that already exists.
setArrayKey
    :: OptionDef -> SetMode -> Map ArrayKey Text -> ArrayKey -> Text
    -> Either Text (Map ArrayKey Text)
setArrayKey d mode current k v = do
    entry <- arrayEntry d v
    let new = case (mode, d.kind, Map.lookup k current) of
            (Append, KString, Just old) -> old <> entry
            _ -> entry
    pure (Map.insert k new current)
