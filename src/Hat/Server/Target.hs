-- | Parsing tmux's @-t@ pane-target grammar into a small domain type,
-- so command implementations resolve targets by case analysis rather
-- than re-inspecting raw strings (avoiding stringly-typed blindness).
module Hat.Server.Target
    ( PaneTarget (..)
    , parsePaneTarget
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

-- | A resolved @-t@ pane target — the subset of tmux's grammar the
-- author's config exercises: the current pane (no @-t@), the last
-- (alternate) pane (@!@), the marked pane (@~@ / @{marked}@), or a
-- specific @%N@ pane id.
data PaneTarget
    = PaneCurrent
    | PaneLast
    | PaneMarked
    | PaneById Int
    deriving (Eq, Show)

parsePaneTarget :: Maybe Text -> PaneTarget
parsePaneTarget = \case
    Nothing -> PaneCurrent
    Just t -> case t of
        "!"         -> PaneLast
        "{last}"    -> PaneLast
        "~"         -> PaneMarked
        "{marked}"  -> PaneMarked
        _ | Just n <- paneId t -> PaneById n
          | otherwise          -> PaneCurrent
  where
    paneId t = do
        rest <- T.stripPrefix "%" t
        case TR.decimal rest of
            Right (n, "") -> Just n
            _             -> Nothing
