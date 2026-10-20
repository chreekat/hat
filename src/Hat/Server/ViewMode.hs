-- | View mode (tmux's @window_view_mode@): copy mode over text shown in a
-- pane apart from its program's grid — command output and configuration
-- errors with nowhere else to go.
module Hat.Server.ViewMode
    ( ViewText (..)
    , viewModeAdd
    , viewToClient
    ) where

import Control.Concurrent.STM
import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Hat.Model
import Hat.Model.Options
import Hat.Server.CopyMode qualified as CopyMode
import Hat.Server.Locate (clientActivePane)
import Hat.Server.ModeStack (modifyTopMode, setPaneMode)
import Hat.Term.Emulator qualified as Emu

-- | How 'viewModeAdd' writes its lines (tmux's @parse@ argument).
data ViewText
    = ViewParsed   -- ^ through the terminal parser, escape sequences and all
    | ViewLiteral  -- ^ as printable text only
    deriving (Eq, Show)

-- | tmux's @window_copy_add@ after @server_client_print@: show lines at the
-- bottom of a pane's view mode, bringing the mode to the top of the pane's
-- stack first (entering it, unless the pane holds it already). The view
-- holds its place as the lines push earlier ones up, as tmux's does.
viewModeAdd :: ServerState -> Pane -> ViewText -> [Text] -> IO ()
viewModeAdd st pane how ls = do
    mbacking <- readTVarIO pane.viewBacking
    held <- atomically (any ((== "view-mode") . modeName . (.mode)) <$> readTVar pane.modes)
    (backing, fresh) <- case mbacking of
        Just b | held -> pure (b, False)
        _ -> do
            sz <- readTVarIO pane.size
            opts <- maybe (readTVarIO st.options) pure =<< paneOptionsNow st pane
            b <- Emu.newEmulator sz (optionNumber "history-limit" opts)
            atomically $ writeTVar pane.viewBacking (Just b)
            pure (b, True)
    opts <- readTVarIO st.options
    let table = case opts.modeKeys of
            KeysVi -> "copy-mode-vi"
            KeysEmacs -> "copy-mode"
        start = do
            frozen <- CopyMode.freezeGrid backing
            pure $ CopyMode CopyModeData
                { kind = ModeView, frozen = frozen
                , copyState = (copyModeAt table 0 0)
                    { lineNumbers = LinesHidden, markShown = MarkHidden } }
    _ <- setPaneMode st pane "view-mode" start KeepPane
    oldHsize <- Emu.scrollbackLength backing
    -- Every line but a fresh screen's first starts on a new one.
    forM_ (zip (not fresh : repeat True) ls) $ \(newline, l) ->
        Emu.feed backing $ TE.encodeUtf8 $
            (if newline then "\r\n" else "") <> render how l
    frozen <- CopyMode.freezeGrid backing
    let grown = frozen.fgHsize - oldHsize
    atomically $ modifyTopMode st pane $ \case
        CopyMode pm | pm.kind == ModeView -> CopyMode pm
            { frozen = frozen
            , copyState = pm.copyState
                { viewportOffY = pm.copyState.viewportOffY + grown } }
        other -> other

-- | tmux's @server_client_print@ to an attached client: the lines go to
-- the view mode of the pane it is looking at.
viewToClient :: ServerState -> Client -> ViewText -> [Text] -> IO ()
viewToClient st client how ls =
    clientActivePane st client >>= mapM_ (\p -> viewModeAdd st p how ls)

-- | A line as the backing screen is fed it.
render :: ViewText -> Text -> Text
render ViewParsed l = l
render ViewLiteral l = T.concatMap keep l
  where
    keep c
        | c == '\n' = "\r\n"
        | c == '\t' || (c >= ' ' && c /= '\DEL') = T.singleton c
        | otherwise = ""
