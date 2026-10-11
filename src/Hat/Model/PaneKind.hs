-- | Whether a pane runs a program.
module Hat.Model.PaneKind (PaneKind (..)) where

-- | A pane running a program, or one of tmux's empty panes (@split-window
-- -E@ and friends): no process, only what is written to its screen.
data PaneKind = ProgramPane | EmptyPane
    deriving (Eq, Show)
