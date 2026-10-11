-- | tmux's theme colours (@themeblack@ … @thememagenta@): ten named slots
-- a style can use, each resolving to a colour from the @dark-theme-*@ or
-- @light-theme-*@ options by the client's theme, or to a plain terminal
-- colour under @theme terminal@ (@colour.c@'s @colour_theme_table@,
-- @server_client_update_theme_colours@).
module Hat.Server.Theme
    ( Theme
    , terminalTheme
    , resolveTheme
    , themeColour
    , colourCell
    , clientColours
    ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)

import Hat.Model.Options (Options, optionText)
import Hat.Server.ColorScheme (ColorScheme (..))
import Hat.Server.Format.Colour (Colour (..), colourFromText)
import Hat.Term.Cell qualified as Cell

-- | The colour each theme slot draws, in 'slots' order.
newtype Theme = Theme [Cell.Color]
    deriving (Eq, Show)

-- | Each slot: its name, its dark and light options, its terminal colour.
slots :: [(Text, Text, Text, Word8)]
slots =
    [ ("themeblack", "dark-theme-black", "light-theme-black", 0)
    , ("themewhite", "dark-theme-white", "light-theme-white", 7)
    , ("themelightgrey", "dark-theme-light-grey", "light-theme-light-grey", 7)
    , ("themedarkgrey", "dark-theme-dark-grey", "light-theme-dark-grey", 0)
    , ("themegreen", "dark-theme-green", "light-theme-green", 2)
    , ("themeyellow", "dark-theme-yellow", "light-theme-yellow", 3)
    , ("themered", "dark-theme-red", "light-theme-red", 1)
    , ("themeblue", "dark-theme-blue", "light-theme-blue", 4)
    , ("themecyan", "dark-theme-cyan", "light-theme-cyan", 6)
    , ("thememagenta", "dark-theme-magenta", "light-theme-magenta", 5) ]

-- | @theme terminal@: every slot its ANSI colour.
terminalTheme :: Theme
terminalTheme = Theme [ Cell.Indexed c | (_, _, _, c) <- slots ]

-- | The theme a client draws with: @theme@ forces @light@, @dark@ or
-- @terminal@; @detect@ follows the client's reported scheme, dark when it
-- has none. A palette option is expanded as a format first (@expand@), and
-- one that is not a plain colour leaves its slot the default colour.
resolveTheme :: (Text -> Text) -> Options -> Maybe ColorScheme -> Theme
resolveTheme expand opts scheme = case optionText "theme" opts of
    "terminal" -> terminalTheme
    choice ->
        let light = case choice of
                "light" -> True
                "dark" -> False
                _ -> scheme == Just SchemeLight
            slotColour (_, dark, lightOpt, _) =
                case colourFromText (expand (optionText (if light then lightOpt else dark) opts)) of
                    Just c | Just cell <- colourCell c -> cell
                    _ -> Cell.DefaultColor
        in Theme (map slotColour slots)

-- | A theme slot's colour by name, any case.
themeColour :: Theme -> Text -> Maybe Cell.Color
themeColour (Theme cs) name =
    lookup (T.toLower name) (zip [ n | (n, _, _, _) <- slots ] cs)

-- | A parsed colour as a cell colour; 'Nothing' is @default@.
colourCell :: Colour -> Maybe Cell.Color
colourCell = \case
    CDefault -> Nothing
    CTerminal -> Just Cell.DefaultColor
    CBase n
        | n >= 90 -> Just (Cell.Indexed (fromIntegral (n - 90 + 8)))
        | otherwise -> Just (Cell.Indexed (fromIntegral n))
    C256 n -> Just (Cell.Indexed (fromIntegral n))
    CRGB rgb -> Just (Cell.RGB (byte 16) (byte 8) (byte 0))
      where byte s = fromIntegral ((rgb `div` (2 ^ (s :: Int))) `mod` 256)

-- | @client_colours@ for a hat client: its terminal draws RGB colours
-- directly (see "Hat.Client.Draw").
clientColours :: Text
clientColours = "16777216"
