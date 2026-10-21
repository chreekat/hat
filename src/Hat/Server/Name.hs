-- | tmux's rule for the names commands give sessions, windows, groups,
-- buffers and pane titles, and the error that refuses one.
module Hat.Server.Name
    ( NameKind (..)
    , checkName
    , admitName
    , NameSlot (..)
    , admittingNames
    ) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Hat.Command.Arg (Arg (..))
import Hat.Server.Command.Types
    (ArgvImpl, CommandImpl, Reply, errQuoting, parseArgsOver, textOnly)

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

-- | Where a command takes a name: a value flag's value, or its first
-- positional word.
data NameSlot = NameFlag Text NameKind | NamePositional NameKind
    deriving (Eq, Show)

-- | The command @name@, parsing its value flags @spec@, taking its names
-- in @slots@ as bytes: a name that is not UTF-8 is refused as tmux refuses
-- it, in slot order; any other such word as 'textOnly' refuses it. The
-- handler checks its text names itself, after expanding them.
admittingNames :: Text -> [Char] -> [NameSlot] -> CommandImpl -> ArgvImpl
admittingNames name spec slots impl st mclient args =
    case [ r | slot <- slots, Just raw@(RawArg _) <- [slotWord slot]
             , Left r <- [admitName (kindOf slot) raw] ] of
        r : _ -> pure [r]
        [] -> textOnly name impl st mclient args
  where
    (opts, _, pos) = parseArgsOver spec [] args
    slotWord = \case
        NameFlag flag _ -> lookup flag opts
        NamePositional _ -> listToMaybe pos
    kindOf = \case
        NameFlag _ k -> k
        NamePositional k -> k
