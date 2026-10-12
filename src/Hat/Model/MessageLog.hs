-- | The server's message log, mirroring tmux's @message_log@: what it ran
-- and what it showed, numbered in order, for @show-messages@. Like tmux's,
-- it lives only in memory: it crosses no reload, store or wire boundary, so
-- a @restart-server@ starts it afresh.
module Hat.Model.MessageLog
    ( MessageEntry (..)
    , MessageLog (..)
    , emptyMessageLog
    , addMessage
    ) where

import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)

data MessageEntry = MessageEntry
    { number :: !Int
    , time :: !POSIXTime
    , text :: !Text
    }
    deriving (Eq, Show)

data MessageLog = MessageLog
    { next :: !Int  -- ^ the number the next entry takes
    , entries :: !(Seq MessageEntry)  -- ^ oldest first
    }
    deriving (Eq, Show)

emptyMessageLog :: MessageLog
emptyMessageLog = MessageLog { next = 0, entries = Seq.empty }

-- | tmux's @server_add_message@: append, then drop every entry but the last
-- @limit@ (@message-limit@) — the new one too at a limit of 0.
addMessage :: Int -> POSIXTime -> Text -> MessageLog -> MessageLog
addMessage limit t msg l = MessageLog
    { next = next'
    , entries = Seq.dropWhileL (\e -> e.number + limit < next')
        (l.entries |> MessageEntry { number = l.next, time = t, text = msg }) }
  where
    next' = l.next + 1
