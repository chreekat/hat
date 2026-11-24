-- | What a client attached to a Rex session decides from what the server
-- tells it.
module Hat.Rex.Session
    ( chooseSession
    , focusedPane
    , endsAttach
    , StreamEnd (..)
    , streamEnd
    ) where

import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Rex.Conn (Close (..))
import Hat.Rex.Control
import Hat.Rex.View

-- | The session to attach to: the one whose label or id was asked for, or
-- the first the server lists.
chooseSession :: Maybe Text -> [SessionEntry] -> Either Text SessionEntry
chooseSession wanted sessions = case wanted of
    Nothing -> maybe (Left "the Rex server has no sessions") Right (listToMaybe sessions)
    Just w -> maybe (Left ("can't find session: " <> w)) Right $
        find (\s -> s.label == w) sessions
            `orElse` find (\s -> s.sessionId == SessionId w) sessions
  where
    orElse a b = maybe b Just a

-- | The pane a view shows the user typing into: the focused block of the
-- active window, which must be a terminal.
focusedPane :: View -> Either Text BlockId
focusedPane view = do
    wid <- maybe (Left "the session has no windows") Right view.activeWindow
    win <- maybe (Left "the session's active window is not in its view") Right $
        find (\w -> w.windowId == wid) view.windows
    bid <- maybe (Left "the session's active window has no focused pane") Right win.focusedBlock
    block <- maybe (Left "the focused pane is not in its window's view") Right $
        find (\b -> b.blockId == bid) win.blocks
    case block.creatorName of
        Just c | c == terminalCreator -> Right bid
        other -> Left ("the focused pane is not a terminal (its creator is "
            <> maybe "unnamed" id other <> ")")

-- | Whether an event means the pane being shown, in the session attached
-- to, is gone.
endsAttach :: SessionId -> BlockId -> Event -> Bool
endsAttach sid bid = \case
    SessionDestroyed s -> s == sid
    SessionEvent s (BlockClosed b) -> s == sid && b == bid
    SessionEvent _ (UnknownSessionEvent _) -> False
    UnknownEvent _ -> False

-- | What the close of a pane's data connection means.
data StreamEnd
    = PaneGone           -- ^ the pane closed, or its stream ended for good
    | StreamBroken Text  -- ^ why, for the user
    deriving (Eq, Show)

streamEnd :: Close -> StreamEnd
streamEnd c = case c.code of
    4000 -> PaneGone
    1000 -> PaneGone
    code -> StreamBroken $ "the pane's stream closed: "
        <> (if T.null c.reason then "" else c.reason <> " ")
        <> "(status " <> T.pack (show code) <> ")"
