-- | tmux's rule for the names commands give sessions, windows, groups,
-- buffers and pane titles, and the error that refuses one.
module Hat.Server.Name
    ( NameKind (..)
    , checkName
    , admitName
    ) where

import Data.Text (Text)
import Data.Text qualified as T

import Hat.Command.Arg (Arg (..))
import Hat.Server.Command.Types (Reply, errQuoting)

-- | What a name names, which picks the words of its refusal.
data NameKind = SessionName | WindowName | SessionGroupName | BufferName | PaneTitle
    deriving (Eq, Show)

-- | tmux's @check_name@ (@utf8_isvalid@): UTF-8 holding no C0 control
-- and no DEL. Empty names and @:@, @.@ and @#@ pass.
checkName :: Arg -> Either Arg Text
checkName = \case
    Utf8Arg t | T.all (\c -> c >= ' ' && c /= '\DEL') t -> Right t
    arg -> Left arg

-- | 'checkName', refusing in tmux's words with the name quoted verbatim.
admitName :: NameKind -> Arg -> Either Reply Text
admitName kind = either (Left . errQuoting refusal) Right . checkName
  where
    refusal = case kind of
        SessionName -> "invalid session name: "
        WindowName -> "invalid window name: "
        SessionGroupName -> "invalid session group name: "
        BufferName -> "invalid buffer name: "
        PaneTitle -> "invalid pane title: "
