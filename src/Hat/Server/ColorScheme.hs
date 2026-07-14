-- | The desktop's light\/dark preference, as published by GNOME's
-- @org.gnome.desktop.interface color-scheme@ setting. The server tails
-- @gsettings monitor@ for changes (see 'Hat.Server.watchColorScheme'),
-- restyles hat's own chrome to a matching default palette, and
-- reapplies the user's per-scheme config.
module Hat.Server.ColorScheme
    ( ColorScheme (..)
    , parseSchemeLine
    , schemeName
    , applyPalette
    ) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)

import Hat.Model.Options (Options (..))
import qualified Hat.Term.Cell as Cell

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

-- | Restyle hat's chrome (status bar, pane borders) to defaults that
-- suit the scheme. Options the user has ever set explicitly
-- ('Options.explicit') are left alone — config always wins over a
-- palette; without a detected scheme nothing is touched, so the classic
-- defaults stand.
applyPalette :: ColorScheme -> Options -> Options
applyPalette scheme opts0 =
    foldl (\o (name, put) ->
            if Set.member name o.explicit then o else put o)
        opts0
        [ ("status-style",
            \o -> o { statusStyle = p.bar })
        , ("window-status-style",
            \o -> o { windowStatusStyle = p.bar })
        , ("window-status-current-style",
            \o -> o { windowStatusCurrentStyle = p.current })
        , ("window-status-bell-style",
            \o -> o { windowStatusBellStyle = p.bell })
        , ("pane-border-style",
            \o -> o { paneBorderStyle = p.border })
        , ("pane-active-border-style",
            \o -> o { paneActiveBorderStyle = p.activeBorder })
        ]
  where
    p = palette scheme

-- Scheme palettes: green status bars (a nod to tmux and screen) with a
-- bold highlight for the current window, an amber bell, and borders
-- that recede without vanishing against either background. The active
-- border keeps a green accent in both schemes.
data Palette = Palette
    { bar          :: Cell.Style
    , current      :: Cell.Style
    , bell         :: Cell.Style
    , border       :: Cell.Style
    , activeBorder :: Cell.Style
    }

palette :: ColorScheme -> Palette
palette SchemeDark = Palette
    { bar          = fgBg 151 22
    , current      = (fgBg 255 28) { Cell.bold = True }
    , bell         = (fgBg 214 22) { Cell.bold = True }
    , border       = onlyFg 238
    , activeBorder = onlyFg 2
    }
palette SchemeLight = Palette
    { bar          = fgBg 22 151
    , current      = (fgBg 255 28) { Cell.bold = True }
    , bell         = (fgBg 166 151) { Cell.bold = True }
    , border       = onlyFg 244
    , activeBorder = onlyFg 28
    }

fgBg :: Word8 -> Word8 -> Cell.Style
fgBg f b = Cell.defaultStyle
    { Cell.fg = Cell.Indexed f, Cell.bg = Cell.Indexed b }

onlyFg :: Word8 -> Cell.Style
onlyFg f = Cell.defaultStyle { Cell.fg = Cell.Indexed f }
