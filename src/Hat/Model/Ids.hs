-- | Stable identifiers for the model tree. Each keeps its tmux-style
-- printed form for targets and format strings: @$N@, @\@N@, @%N@.
module Hat.Model.Ids
    ( SessionId (..)
    , WindowId (..)
    , PaneId (..)
    , ClientId (..)
    ) where

newtype SessionId = SessionId Int deriving (Eq, Ord, Show)  -- prints as "$N"
newtype WindowId = WindowId Int deriving (Eq, Ord, Show)    -- prints as "@N"
newtype PaneId = PaneId Int deriving (Eq, Ord, Show)        -- prints as "%N"
newtype ClientId = ClientId Int deriving (Eq, Ord, Show)
