-- | A pane's own colours, as its options and its program set them: the
-- default foreground and background its cells draw in (@window-style@,
-- @window-active-style@), its palette (@pane-colours@ under the program's
-- OSC 4 changes), and what it answers its program's colour queries with.
module Hat.Server.PaneColours
    ( Expand
    , paneStyleOption
    , paneDefaultColours
    , panePalette
    , oscColourReply
    , queryDefaultColour
    , queryPaletteColour
    ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)

import Hat.Model
import Hat.Model.Options
import Hat.Server.FormatDraw (colourValue, styleCell)
import Hat.Server.FormatEnv
    (FormatScope (..), FormatTarget (..), clientTheme, expandAt)
import Hat.Server.Locate (paneIndexOf)
import Hat.Server.Render (paneDefaults)
import Hat.Server.Theme (Theme, colourRgb)
import Hat.Term.Cell qualified as Cell
import Hat.Term.Emulator qualified as Emu

-- | How a format expands at a target: 'expandAt', or through a frame's
-- 'Hat.Server.FormatEnv.memoExpand'.
type Expand = FormatTarget -> Text -> IO Text

-- | A pane's style option, its formats expanded in the pane's context
-- first (tmux's @style_add@ with the pane's format tree).
paneStyleOption
    :: ServerState -> Expand -> Theme -> Session -> Window -> Pane -> Options -> Text
    -> IO Cell.Style
paneStyleOption st expand th sess win pane popts name
    | "#{" `T.isInfixOf` raw = do
        wix <- atomically (readTVar sess.currentIx)
        pix <- paneIndexOf st win pane
        styleCell th <$> expand (FormatTarget (InPane sess wix win pix pane) Map.empty) raw
    | otherwise = pure (styleCell th raw)
  where
    raw = optionText name popts

-- | The foreground and background a pane's default-coloured cells draw in
-- (tmux's @tty_default_colours@).
paneDefaultColours
    :: ServerState -> Expand -> Theme -> Session -> Window -> Pane -> Options
    -> IO (Cell.Color, Cell.Color)
paneDefaultColours st expand th sess win pane popts = do
    active <- readTVarIO win.activeId
    ws <- paneStyleOption st expand th sess win pane popts "window-style"
    was <- paneStyleOption st expand th sess win pane popts "window-active-style"
    (pfg, pbg) <- Emu.appDefaultColours pane.emulator
    let rgb = maybe Cell.DefaultColor (\(r, g, b) -> Cell.RGB r g b)
    pure (paneDefaults (pane.id == active) (rgb pfg, rgb pbg) ws was)

-- | A pane's palette (tmux's @colour_palette_get@): the colours its
-- program set (OSC 4) over its @pane-colours@, which also become the
-- terminal's palette defaults.
panePalette :: Theme -> Pane -> Options -> IO (Map.Map Int Cell.Color)
panePalette th pane popts = do
    let configured = Map.fromList
            [ (fromIntegral n, c)
            | (KeyIndex n, v) <- case optionValue "pane-colours" popts of
                OVArray m -> Map.toList m
                OVScalar _ -> []
            , n < 256
            , Just (Just c) <- [colourValue th v] ]
    Emu.setPaletteDefaults pane.emulator (Map.mapMaybe colourRgb configured)
    program <- Emu.paletteOverrides pane.emulator
    pure (Map.union (Map.map (\(r, g, b) -> Cell.RGB r g b) program) configured)

-- | A pane's session, window and options, while it is in the tree.
paneContext :: ServerState -> Pane -> IO (Maybe (Session, Window, Options))
paneContext st pane = atomically $ do
    mhome <- readTVar pane.home
    case mhome of
        Nothing -> pure Nothing
        Just (sid, win) -> do
            msess <- Map.lookup sid <$> readTVar st.sessions
            case msess of
                Nothing -> pure Nothing
                Just sess -> Just . (,,) sess win <$> resolveForPane st sess win pane

-- | What a pane answers its program's OSC 10\/11 query with when its
-- options give it its own colour (tmux's @window_pane_get_bg@);
-- 'Nothing' leaves the answer to the terminal's.
queryDefaultColour :: ServerState -> Pane -> Emu.OscColorTarget -> IO (Maybe Emu.Rgb)
queryDefaultColour st pane target = paneContext st pane >>= \case
    Nothing -> pure Nothing
    Just (sess, win, popts) -> do
        th <- clientTheme st
        (fg, bg) <- paneDefaultColours st (expandAt st) th sess win pane popts
        pure (colourRgb (case target of
            Emu.Foreground -> fg
            Emu.Background -> bg))

-- | What a pane answers an OSC 4 query of one palette entry with: its
-- palette's colour, else the entry's own.
queryPaletteColour :: ServerState -> Pane -> Int -> IO (Maybe Emu.Rgb)
queryPaletteColour st pane n = do
    th <- clientTheme st
    mctx <- paneContext st pane
    palette <- maybe (pure Map.empty) (\(_, _, popts) -> panePalette th pane popts) mctx
    pure (colourRgb (Map.findWithDefault (Cell.Indexed (fromIntegral n)) n palette))

-- | The reply to an OSC colour query (tmux's @input_osc_colour_reply@):
-- @OSC n ; [index ;] rgb:rrrr/gggg/bbbb@, with the query's terminator.
oscColourReply :: Text -> Emu.Rgb -> Emu.OscTerm -> ByteString
oscColourReply prefix (r, g, b) term = TE.encodeUtf8 $ T.concat
    [ "\ESC]", prefix, ";rgb:", hex4 r, "/", hex4 g, "/", hex4 b
    , case term of
        Emu.TermBel -> "\a"
        Emu.TermSt -> "\ESC\\" ]
  where
    hex4 w = let h = hex2 w in h <> h
    hex2 :: Word8 -> Text
    hex2 w = T.pack [digit (w `div` 16), digit (w `mod` 16)]
    digit d = "0123456789abcdef" !! fromIntegral d
