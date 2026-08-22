-- | The @-X@ copy-mode commands: one keystroke's worth of motion,
-- selection or scroll, named the way @send-keys -X@ and the copy-mode key
-- tables name them.
module Hat.Server.Command.CopyMode
    ( runCopyModeCommand
    ) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Hat.Model
import Hat.Server.Command.Types (Reply (..))
import qualified Hat.Server.CopyMode as CopyMode
import Hat.Server.Hooks (notifyPane)

runCopyModeCommand :: ServerState -> Pane -> Text -> [Text] -> IO [Reply]
runCopyModeCommand st pane name cmdArgs = do
    mmode <- readTVarIO pane.mode
    case mmode of
        Nothing -> pure []  -- not in copy mode; -X is a no-op
        Just pm
            -- A digit key builds the @[count]@ prefix rather than running
            -- a motion; @0@ with no count pending is @start-of-line@.
            | name == "digit", Just d <- readDigit cmdArgs -> do
                atomically $ do
                    writeTVar pane.mode
                        (Just (reMode (CopyMode.pushDigit d state)))
                    bumpDirty st
                pure []
            | otherwise -> case Map.lookup name CopyMode.handlers of
                Nothing -> pure []
                Just h -> do
                    -- Motions repeat [count] times; yanks never do. Every
                    -- command clears the pending count.
                    let count
                            | name `elem` ["copy-selection", "copy-pipe"] = 1
                            | otherwise = min 1000 (maybe 1 (max 1) state.numPrefix)
                    result <- applyN h (state { numPrefix = Nothing }) count
                    case result of
                        -- A failed command leaves the mode untouched.
                        Left err -> pure [RErr err]
                        Right r -> do
                            r' <- traverse (scrollPaneToCursor pane) r
                            atomically $ do
                                writeTVar pane.mode (reMode <$> r')
                                bumpDirty st
                            case r' of
                                Nothing ->
                                    notifyPane st "pane-mode-changed" pane []
                                Just _ -> pure ()
                            pure []
          where
            state = pm.copyState
            reMode s = pm { copyState = s }
  where
    readDigit (a : _) = case TR.decimal a of
        Right (d, rest) | T.null rest, d >= 0, d <= 9 -> Just d
        _ -> Nothing
    readDigit [] = Nothing
    -- Run a handler @n@ times, threading the state and stopping early if
    -- it errors or exits copy mode.
    applyN _ s 0 = pure (Right (Just s))
    applyN h s n = do
        r <- h st pane s cmdArgs
        case r of
            Left err -> pure (Left err)
            Right Nothing -> pure (Right Nothing)
            Right (Just s') -> applyN h s' (n - 1)

-- | Re-center a pane's copy-mode viewport on its cursor after a motion,
-- over the pane's frozen snapshot (a no-op when not in copy mode).
scrollPaneToCursor :: Pane -> CopyModeState -> IO CopyModeState
scrollPaneToCursor pane s = do
    mmode <- readTVarIO pane.mode
    pure $ case mmode of
        Just pm -> CopyMode.scrollToCursor pm.frozen.fgHsize pm.frozen.fgSy s
        Nothing -> s
