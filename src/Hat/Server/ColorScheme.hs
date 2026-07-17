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

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)

import Hat.Model.Options
    ( OptionName (..), OptionValue (..), OptionsDelta
    , emptyDelta, insertDelta )
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

-- | The scheme's chrome (status bar, pane borders) as an option overlay. It
-- is applied as the lowest-priority layer under every user scope (see
-- 'Hat.Server.applyScheme'), so a user's own @set@ always wins; without a
-- detected scheme nothing is applied, so the classic defaults stand.
applyPalette :: ColorScheme -> OptionsDelta
applyPalette scheme = foldr (uncurry insertDelta) emptyDelta
    [ (OptStatusStyle, OVStyle p.bar)
    , (OptWindowStatusStyle, OVStyle p.bar)
    , (OptWindowStatusCurrentStyle, OVStyle p.current)
    , (OptWindowStatusBellStyle, OVStyle p.bell)
    , (OptPaneBorderStyle, OVStyle p.border)
    , (OptPaneActiveBorderStyle, OVStyle p.activeBorder)
    ]
  where
    p = palette scheme

-- Scheme palettes: green status bars (a nod to tmux and screen) with a
-- bold highlight for the current window, an amber bell, and inactive
-- borders that recede without vanishing against their background — a
-- receding gray on dark, a lighter gray on light so they stay clearly
-- distinct from the active border. The active border keeps a green
-- accent in both schemes.
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
    , border       = onlyFg 65
    , activeBorder = (onlyFg 10) { Cell.bold = True }
    }
palette SchemeLight = Palette
    { bar          = fgBg 22 151
    , current      = (fgBg 255 28) { Cell.bold = True }
    , bell         = (fgBg 166 151) { Cell.bold = True }
    , border       = onlyFg 250
    , activeBorder = (onlyFg 22) { Cell.bold = True }
    }

fgBg :: Word8 -> Word8 -> Cell.Style
fgBg f b = Cell.defaultStyle
    { Cell.fg = Cell.Indexed f, Cell.bg = Cell.Indexed b }

onlyFg :: Word8 -> Cell.Style
onlyFg f = Cell.defaultStyle { Cell.fg = Cell.Indexed f }
