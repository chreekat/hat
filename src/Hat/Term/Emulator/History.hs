{-# LANGUAGE StrictData #-}

-- | tmux's history bookkeeping, laid over libghostty's scrollback.
-- libghostty retains scrollback rows by a byte budget; the history hat
-- exposes is the newest 'size' of them, with 'size' following tmux's grid
-- rules (grid.c): a row scrolling in is first preceded by trimming a tenth of
-- the limit when the history is full ('grid_collect_history'), a reflow keeps
-- every row it produces and trims nothing ('grid_reflow'), and a lowered
-- limit trims down to it ('session_update_history'). Rows libghostty keeps
-- above the history are the ones tmux freed.
module Hat.Term.Emulator.History
    ( History (..)
    , Recording (..)
    , PrimaryRows (..)
    , newHistory
    , exposedRows
    , afterOutput
    , afterLimit
    , linesBegun
    , rowsHolding
    ) where

-- | Whether rows scrolling off the top enter the history at all: tmux's
-- @GRID_HISTORY@, fixed when the pane is made (a zero limit then leaves it
-- off for good).
data Recording = Recording | NotRecording
    deriving (Eq, Show)

-- | Whether 'seen' still describes the primary screen's scrollback, or a
-- resize reshaped it while the alternate screen was active and unreadable.
data PrimaryRows = RowsSeen | ResizedUnseen
    deriving (Eq, Show)

data History = History
    { recording :: Recording
    , limit     :: Int  -- ^ @history-limit@
    , size      :: Int  -- ^ tmux's @hsize@: how many of the newest scrollback
                        --   rows are history
    , seen      :: Int  -- ^ libghostty's scrollback row count when last
                        --   reconciled
    , primary   :: PrimaryRows
    }
    deriving (Eq, Show)

-- | A fresh pane's empty history under @limit@.
newHistory :: Int -> History
newHistory lim = History
    { recording = if lim /= 0 then Recording else NotRecording
    , limit = lim, size = 0, seen = 0, primary = RowsSeen }

-- | How many history rows to expose when libghostty holds @phys@.
exposedRows :: History -> Int -> Int
exposedRows h phys = max 0 (min h.size phys)

-- | Reconcile with libghostty's primary scrollback count @phys@ after output.
-- Growth is rows scrolled in, each trimmed for as tmux's grid does; a
-- shrink is libghostty dropping its oldest rows, or a clear, and leaves at
-- most what remains. After a resize nobody saw, growth is the resize's and
-- trims nothing.
afterOutput :: Int -> History -> History
afterOutput phys h = h { size = min phys size', seen = phys, primary = RowsSeen }
  where
    grown = phys - h.seen
    size'
        | h.primary == ResizedUnseen = h.size + max 0 grown
        | grown <= 0 = h.size
        | h.recording == NotRecording = 0
        | otherwise = scrollIn h.limit grown h.size

-- | @n@ rows scrolling into a history of @s@ rows under @lim@, one at a time
-- as tmux's @grid_collect_history@ then @grid_scroll_history@ take them.
scrollIn :: Int -> Int -> Int -> Int
scrollIn lim = go
  where
    chunk = max 1 (lim `div` 10)
    go n s
        | n <= 0 = s
        | s == 0 || s < lim =
            let k = min n (max 1 (lim - s)) in go (n - k) (s + k)
        -- Full: each row trims a chunk and adds itself; with a chunk of one
        -- that is a fixed point.
        | chunk == 1 = s
        | otherwise =
            let net = chunk - 1
                untilRoom = (s - lim) `div` net + 1
            in if n >= untilRoom
                then go (n - untilRoom) (s - untilRoom * net)
                else s - n * net

-- | A @history-limit@ change: tmux's @session_update_history@, which trims
-- a full history down to the new limit, and always by at least one row.
afterLimit :: Int -> History -> History
afterLimit lim h
    | h.size == 0 || h.size < lim = h { limit = lim }
    | otherwise = h { limit = lim, size = h.size - max 1 (h.size - lim) }

-- | How many logical lines begin in rows with these wrap flags (oldest
-- first): the first row begins one, as does each row after an unwrapped one.
linesBegun :: [Bool] -> Int
linesBegun [] = 0
linesBegun ws = 1 + length (filter not (init ws))

-- | How many rows, from the oldest, hold the first @k@ logical lines, reading
-- each row's wrap flag with @wrapped@ for rows below @n@.
rowsHolding :: Monad m => Int -> Int -> (Int -> m Bool) -> m Int
rowsHolding k0 n wrapped = go k0 0
  where
    go k y
        | k <= 0 || y >= n = pure y
        | otherwise = do
            w <- wrapped y
            go (if w then k else k - 1) (y + 1)
