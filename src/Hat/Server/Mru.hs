-- | Most-recently-used history for @last-window@/@last-pane@/@last-session@.
--
-- Each navigable collection (a session's windows, a window's panes, a client's
-- sessions) keeps an MRU list beside its current item: most-recent first, the
-- current item never in its own history, and no duplicates. @last-*@ toggles
-- against the head; closing the current item pops the head so the next-most-
-- recent becomes the new @last@.
module Hat.Server.Mru
    ( recordVisit
    , scrub
    , popOnClose
    ) where

-- | The history after switching current from @from@ to @to@: the departed
-- item goes on top and the destination leaves the history (current is never in
-- its own history). A repeated @last-*@ therefore toggles — @to@ is the old
-- head, so the result is @from : rest@.
recordVisit :: Eq a => a -> a -> [a] -> [a]
recordVisit from to hist = from : filter (\y -> y /= to && y /= from) hist

-- | Drop entries that no longer survive (a non-current item was removed).
scrub :: (a -> Bool) -> [a] -> [a]
scrub = filter

-- | The current item was removed: pop the most-recent surviving item as the
-- new current, paired with the shrunk history. Falls back to @fallback@ (e.g.
-- the lowest-numbered survivor) with an empty history when nothing in the
-- history survives; 'Nothing' when nothing survives at all.
popOnClose :: (a -> Bool) -> Maybe a -> [a] -> Maybe (a, [a])
popOnClose alive fallback hist = case filter alive hist of
    (top : rest) -> Just (top, rest)
    []           -> (\f -> (f, [])) <$> fallback
