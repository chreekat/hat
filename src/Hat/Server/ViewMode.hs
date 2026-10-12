-- | View mode (tmux's @window_view_mode@): copy mode over text shown in a
-- pane apart from its program's grid — command output and configuration
-- errors with nowhere else to go.
module Hat.Server.ViewMode
    ( ViewText (..)
    , viewModeAdd
    , viewToClient
    , leaveViewMode
    ) where

import Control.Concurrent.STM
import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Hat.Model
import Hat.Model.Options
import Hat.Server.CopyMode qualified as CopyMode
import Hat.Server.Hooks (PayloadItem (..), notifyPane)
import Hat.Server.Locate (clientActivePane)
import Hat.Term.Emulator qualified as Emu

-- | How 'viewModeAdd' writes its lines (tmux's @parse@ argument).
data ViewText
    = ViewParsed   -- ^ through the terminal parser, escape sequences and all
    | ViewLiteral  -- ^ as printable text only
    deriving (Eq, Show)

-- | tmux's @window_copy_add@: show lines at the bottom of a pane's view
-- mode, entering it first unless it is the pane's mode already. The view
-- holds its place as the lines push earlier ones up, as tmux's does.
viewModeAdd :: ServerState -> Pane -> ViewText -> [Text] -> IO ()
viewModeAdd st pane how ls = do
    (mmode, mbacking) <- atomically $ (,) <$> readTVar pane.mode <*> readTVar pane.viewBacking
    (backing, entering, pm0) <- case (mmode, mbacking) of
        (Just pm, Just b) | pm.kind == ModeView -> pure (b, False, Just pm)
        _ -> do
            sz <- readTVarIO pane.size
            opts <- maybe (readTVarIO st.options) pure =<< paneOptionsNow st pane
            b <- Emu.newEmulator sz (optionNumber "history-limit" opts)
            pure (b, True, Nothing)
    oldHsize <- Emu.scrollbackLength backing
    -- Every line but a fresh screen's first starts on a new one.
    forM_ (zip (not entering : repeat True) ls) $ \(newline, l) ->
        Emu.feed backing $ TE.encodeUtf8 $
            (if newline then "\r\n" else "") <> render how l
    frozen <- CopyMode.freezeGrid backing
    opts <- readTVarIO st.options
    let table = case opts.modeKeys of
            KeysVi -> "copy-mode-vi"
            KeysEmacs -> "copy-mode"
        base = fromMaybe PaneMode
            { kind = ModeView, frozen = frozen
            , copyState = (copyModeAt table 0 0)
                { lineNumbers = LinesHidden, markShown = MarkHidden } } pm0
        grown = frozen.fgHsize - oldHsize
        pm = base
            { frozen = frozen
            , copyState = base.copyState
                { viewportOffY = base.copyState.viewportOffY + grown } }
    atomically $ do
        writeTVar pane.viewBacking (Just backing)
        writeTVar pane.mode (Just pm)
        modifyTVar' pane.clock $ \case
            ClockOnTop -> ClockUnderCopy
            c -> c
        bumpDirty st
    when entering $ do
        notifyPane st "pane-mode-changed" pane []
        notifyPane st "pane-mode-entered" pane
            [ ("mode_entered", PInt 1)
            , ("current_mode", PText "view-mode")
            , ("previous_mode", PText "") ]

-- | tmux's @server_client_print@ to an attached client: the lines go to
-- the view mode of the pane it is looking at.
viewToClient :: ServerState -> Client -> ViewText -> [Text] -> IO ()
viewToClient st client how ls =
    clientActivePane st client >>= mapM_ (\p -> viewModeAdd st p how ls)

-- | Drop the view mode's screen once the pane has left the mode.
leaveViewMode :: Pane -> STM ()
leaveViewMode pane = writeTVar pane.viewBacking Nothing

-- | A line as the backing screen is fed it.
render :: ViewText -> Text -> Text
render ViewParsed l = l
render ViewLiteral l = T.concatMap keep l
  where
    keep c
        | c == '\n' = "\r\n"
        | c == '\t' || (c >= ' ' && c /= '\DEL') = T.singleton c
        | otherwise = ""
