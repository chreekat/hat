-- | State carried by a tmux control-mode client (@hat -C@). The protocol
-- itself lives in "Hat.Server.Control"; this module only defines the state
-- so 'Hat.Model.Client' can hold it without a module cycle.
module Hat.Server.ControlTypes
    ( ControlState (..)
    , ControlItem (..)
    , ExitState (..)
    , BlockState (..)
    , OutputMode (..)
    , PaneFlow (..)
    , ControlFlags (..)
    , defaultControlFlags
    , Subscription (..)
    , newControlState
    ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Word (Word64)

import Hat.Geometry (Size)
import Hat.Model.Ids
import Hat.Server.HookTypes (MonitorKey, MonitorTarget)
import Hat.Transport.Wire (ExitWait (..))

-- | One entry of a control client's outbound stream, in send order.
data ControlItem
    = ControlLine ByteString
        -- ^ a protocol line, without its newline
    | ControlOutput PaneId Word64 ByteString
        -- ^ raw pane output not yet escaped into an @%output@ line, with the
        --   monotonic millisecond it was first queued (its age's origin)

-- | Whether the client is still running or has been told to exit, and why.
data ExitState = Running | Exiting (Maybe Text)

-- | Whether a command's @%begin@\/@%end@ block is being written.
data BlockState = BlockClosed | BlockOpen

-- | @refresh-client -f no-output@: whether pane output is sent at all.
data OutputMode = SendOutput | NoOutput
    deriving (Eq, Show)

-- | A pane's @refresh-client -A@ state; a pane with none is on.
data PaneFlow = PaneOff | PanePaused
    deriving (Eq, Show)

-- | The control-only client flags @refresh-client -f@ sets.
data ControlFlags = ControlFlags
    { output :: OutputMode
    , exitWait :: ExitWait
    , pauseAfter :: Maybe Word64  -- ^ @pause-after@ age in milliseconds
    }
    deriving (Eq, Show)

defaultControlFlags :: ControlFlags
defaultControlFlags = ControlFlags
    { output = SendOutput, exitWait = ExitNow, pauseAfter = Nothing }

-- | A @refresh-client -B@ subscription and the values it last reported.
data Subscription = Subscription
    { target :: MonitorTarget
    , format :: Text
    , lastValues :: Map MonitorKey Text
    }

data ControlState = ControlState
    { queue :: TVar (Seq ControlItem)
    , outputBytes :: TVar Int
        -- ^ raw bytes held in the queue's 'ControlOutput' items
    , block :: TVar BlockState
    , held :: TVar (Seq ByteString)
        -- ^ notifications waiting for the open block to close
    , exiting :: TVar ExitState
    , retval :: TVar Int  -- ^ exit status: 1 once a command has failed
    , flags :: TVar ControlFlags
    , panes :: TVar (Map PaneId PaneFlow)
    , size :: TVar (Maybe Size)
        -- ^ @refresh-client -C WxH@; until set the client does not size
        --   windows
    , windowSizes :: TVar (Maybe (Map WindowId Size))
        -- ^ @refresh-client -C \@id:WxH@; 'Nothing' until any is set
    , subscriptions :: TVar (Map Text Subscription)
    , commandNumber :: TVar Int  -- ^ the last @%begin@ number used
    }

newControlState :: IO ControlState
newControlState = ControlState
    <$> newTVarIO Seq.empty
    <*> newTVarIO 0
    <*> newTVarIO BlockClosed
    <*> newTVarIO Seq.empty
    <*> newTVarIO Running
    <*> newTVarIO 0
    <*> newTVarIO defaultControlFlags
    <*> newTVarIO Map.empty
    <*> newTVarIO Nothing
    <*> newTVarIO Nothing
    <*> newTVarIO Map.empty
    <*> newTVarIO 0
