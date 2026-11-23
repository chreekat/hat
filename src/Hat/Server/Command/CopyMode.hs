-- | The @-X@ copy-mode commands: one keystroke's worth of motion,
-- selection or scroll, named the way @send-keys -X@ and the copy-mode key
-- tables name them.
module Hat.Server.Command.CopyMode
    ( SearchString (..)
    , runCopyModeCommand
    ) where

import Control.Concurrent.STM
import Control.Monad (when)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Hat.Model
import Hat.Model.Options (ModeKeys (..), Options (..))
import Hat.Server.Command.Types (Reply (..))
import Hat.Server.CopyMode qualified as CopyMode
import Hat.Server.FormatEnv (FormatScope (..), FormatTarget (..), expandAt, expandFormat)
import Hat.Server.Locate (findPaneLoc, paneIndexOf)
import Hat.Server.PaneMode (modifyTopMode, resetPaneMode)
import Hat.Server.Scrollbars (revealScrollbar, scrollShowsScrollbar)

-- | Whether a copy-mode search's string is a format, expanded against the
-- pane before it searches (@send-keys -F@).
data SearchString = SearchLiteral | SearchFormat

runCopyModeCommand :: ServerState -> Pane -> SearchString -> Text -> [Text] -> IO [Reply]
runCopyModeCommand st pane strings name cmdArgs = do
    mmode <- atomically (topMode pane)
    case mmode of
        Just (CopyMode pm)
            -- A digit key builds the @[count]@ prefix rather than running
            -- a motion; @0@ with no count pending is @start-of-line@.
            | name == "digit", Just d <- readDigit cmdArgs -> do
                atomically $ modifyTopMode st pane
                    (reMode (CopyMode.pushDigit d state))
                pure []
            | otherwise -> case (Map.lookup name CopyMode.handlers, CopyMode.commandPositionals name cmdArgs) of
                (Nothing, _)
                    | Map.member name CopyMode.tmuxCommands ->
                        pure [RErr (name <> ": not yet supported by hat")]
                    | otherwise -> pure [RErr ("unknown copy-mode command: " <> name)]
                (Just _, Left err) -> pure [RErr err]
                (Just h, Right pos0) -> searchArgs pos0 >>= \case
                  -- An empty expansion leaves nothing to search for.
                  Nothing -> pure []
                  Just pos -> do
                    -- Motions repeat [count] times; yanks never do. Every
                    -- command clears the pending count.
                    let count
                            | name `elem` ["copy-selection", "copy-selection-no-clear", "copy-pipe"] = 1
                            | otherwise = min 1000 (maybe 1 (max 1) state.numPrefix)
                    result <- applyN h pos (state { numPrefix = Nothing }) count
                    keys <- maybe KeysEmacs (.modeKeys) <$> paneOptionsNow st pane
                    let unmarked s
                            | CopyMode.clearsMarks keys name = s { searchMarks = Nothing }
                            | otherwise = s
                    case fmap (fmap unmarked) result of
                        -- A failed command leaves the mode untouched.
                        Left err -> pure [RErr err]
                        Right Nothing -> [] <$ resetPaneMode st pane
                        Right (Just r) -> do
                            r' <- scrollPaneToCursor pane r
                            atomically $ modifyTopMode st pane (reMode r')
                            when (scrollShowsScrollbar name state.viewportOffY r'.viewportOffY) $
                                revealScrollbar st pane
                            pure []
          where
            state = pm.copyState
            reMode s = \case
                CopyMode cm -> CopyMode cm { copyState = s }
                other -> other
        -- not in copy mode; -X is a no-op
        _ -> pure []
  where
    searchArgs pos = case (strings, pos) of
        (SearchFormat, [ss]) | name `elem` searches -> do
            mloc <- atomically (findPaneLoc st pane.id)
            expanded <- case mloc of
                Just (sess, ix, win, _) -> do
                    pix <- paneIndexOf st win pane
                    expandAt st (FormatTarget (InPane sess ix win pix pane) Map.empty) ss
                Nothing -> expandFormat st Map.empty ss
            pure (if T.null expanded then Nothing else Just [expanded])
        _ -> pure (Just pos)
    searches = ["search-forward", "search-backward", "search-forward-text", "search-backward-text"]
    readDigit (a : _) = case TR.decimal a of
        Right (d, rest) | T.null rest, d >= 0, d <= 9 -> Just d
        _ -> Nothing
    readDigit [] = Nothing
    -- Run a handler @n@ times, threading the state and stopping early if
    -- it errors or exits copy mode.
    applyN _ _ s 0 = pure (Right (Just s))
    applyN h pos s n = do
        r <- h st pane s pos
        case r of
            Left err -> pure (Left err)
            Right Nothing -> pure (Right Nothing)
            Right (Just s') -> applyN h pos s' (n - 1)

-- | Re-center a pane's copy-mode viewport on its cursor after a motion,
-- over the pane's frozen snapshot (a no-op when not in copy mode).
scrollPaneToCursor :: Pane -> CopyModeState -> IO CopyModeState
scrollPaneToCursor pane s = do
    mmode <- atomically (topMode pane)
    pure $ case mmode of
        Just (CopyMode pm) -> CopyMode.scrollToCursor pm.frozen.fgHsize pm.frozen.fgSy s
        _ -> s
