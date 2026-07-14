-- | The desktop's light\/dark preference, as published by GNOME's
-- @org.gnome.desktop.interface color-scheme@ setting. The server tails
-- @gsettings monitor@ for changes (see 'Hat.Server.watchColorScheme') and
-- reapplies the user's per-scheme config.
module Hat.Server.ColorScheme
    ( ColorScheme (..)
    , parseSchemeLine
    , schemeName
    ) where

import Data.Text (Text)
import qualified Data.Text as T

data ColorScheme = SchemeLight | SchemeDark
    deriving (Eq, Show)

-- | One line of @gsettings get@ (@'prefer-dark'@) or @gsettings monitor@
-- (@color-scheme: 'prefer-dark'@) output. GNOME's only schemes are
-- @prefer-dark@, @prefer-light@ and @default@; @default@ means the
-- desktop has no preference, which reads as light.
parseSchemeLine :: Text -> Maybe ColorScheme
parseSchemeLine line = case T.words line of
    [] -> Nothing
    ws | keyed && not aboutScheme -> Nothing
       | otherwise -> case T.dropAround (`elem` ("'\"" :: String)) (last ws) of
            "prefer-dark"  -> Just SchemeDark
            "prefer-light" -> Just SchemeLight
            "default"      -> Just SchemeLight
            _              -> Nothing
      where
        keyed = length ws > 1
        aboutScheme = "color-scheme:" `elem` init ws

-- | The value @#{color_scheme}@ expands to.
schemeName :: ColorScheme -> Text
schemeName SchemeDark = "dark"
schemeName SchemeLight = "light"
