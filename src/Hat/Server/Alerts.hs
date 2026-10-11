-- | Telling a session's clients of a bell, activity or silence in one of its
-- windows, as tmux's @alerts.c@ does: whether the alert applies there
-- (@bell-action@, @activity-action@, @silence-action@), then per client a
-- bell, a message, or both (@visual-bell@, @visual-activity@,
-- @visual-silence@).
module Hat.Server.Alerts
    ( AlertKind (..)
    , Visual (..)
    , AlertWhere (..)
    , BellRing (..)
    , alertApplies
    , alertNotice
    , deliverAlert
    ) where

import Control.Concurrent.STM
import Control.Monad (forM_, when)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Model
import Hat.Model.Options (Options, optionText)
import Hat.Server.ClientIO (sendAs)
import Hat.Server.Outbound (Delivery (..))
import Hat.Server.Toast (showToast)
import Hat.Transport.Wire (ServerToClient (..))

data AlertKind = BellAlert | ActivityAlert | SilenceAlert
    deriving (Eq, Show)

-- | A @visual-*@ option: a bell, a message instead, or both.
data Visual = VisualOff | VisualOn | VisualBoth
    deriving (Eq, Show)

-- | Where the alerting window is, from the session's side.
data AlertWhere = InCurrentWindow | InWindow Int
    deriving (Eq, Show)

data BellRing = Rings | Quiet
    deriving (Eq, Show)

-- | tmux's @alerts_action_applies@: whether the session's action option
-- admits an alert in its current window or another.
alertApplies :: AlertKind -> Options -> AlertWhere -> Bool
alertApplies kind opts at = case optionText (actionOption kind) opts of
    "any" -> True
    "current" -> at == InCurrentWindow
    "other" -> at /= InCurrentWindow
    _ -> False

-- | tmux's @alerts_set_message@ for one client: the bell it hears and the
-- message it is shown.
alertNotice :: AlertKind -> Visual -> AlertWhere -> (BellRing, Maybe Text)
alertNotice kind visual at = (ring, message)
  where
    ring = if visual == VisualOn then Quiet else Rings
    message
        | visual == VisualOff = Nothing
        | otherwise = Just $ alertName kind <> case at of
            InCurrentWindow -> " in current window"
            InWindow ix -> " in window " <> T.pack (show ix)

-- | Tell the session's clients (not control clients) of an alert in its
-- window at index @ix@; the caller has already decided the alert applies.
deliverAlert :: ServerState -> Session -> Options -> AlertKind -> Int -> IO ()
deliverAlert st sess opts kind ix = do
    (cur, clients) <- atomically $
        (,) <$> readTVar sess.currentIx <*> sessionClients st sess.id
    let at = if ix == cur then InCurrentWindow else InWindow ix
        (ring, message) = alertNotice kind (visualOf kind opts) at
    forM_ (filter (isNothing . (.control)) clients) $ \c -> do
        -- See Note [Falling behind]
        when (ring == Rings) $ sendAs Expendable c RingBell
        forM_ message (showToast st c)

visualOf :: AlertKind -> Options -> Visual
visualOf kind opts = case optionText (visualOption kind) opts of
    "on" -> VisualOn
    "both" -> VisualBoth
    _ -> VisualOff

alertName :: AlertKind -> Text
alertName = \case
    BellAlert -> "Bell"
    ActivityAlert -> "Activity"
    SilenceAlert -> "Silence"

actionOption, visualOption :: AlertKind -> Text
actionOption = \case
    BellAlert -> "bell-action"
    ActivityAlert -> "activity-action"
    SilenceAlert -> "silence-action"
visualOption = \case
    BellAlert -> "visual-bell"
    ActivityAlert -> "visual-activity"
    SilenceAlert -> "visual-silence"
