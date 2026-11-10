-- | A live session's status line: where it sits and what it shows.
module Hat.Server.StatusLine
    ( statusLayout  -- ^ exported for the status-position effect test
    , gatherStatus
    , statusRangesFor
    ) where

import Control.Concurrent.STM (readTVarIO)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as V

import Hat.Model
import Hat.Model.Options
import Hat.Server.Format qualified as F
import Hat.Server.FormatDraw (DrawRange)
import Hat.Server.FormatEnv
import Hat.Server.Status

-- | Where the status bar sits in a client of @rows@ rows: the vertical offset
-- the window content takes below a top bar, and the row the bar occupies
-- (@Nothing@ when @status@ is off — no bar, content fills the height). See
-- 'renderOnce'.
statusLayout :: StatusPosition -> Int -> Int -> (Int, Maybe Int)
statusLayout _   0     _    = (0, Nothing)
statusLayout pos count rows = case pos of
    StatusTop    -> (count, Just 0)
    StatusBottom -> (0, Just (rows - count))

-- | Everything the status rows' expansion consumes beyond the options,
-- read fresh: the session's format environment and each window's own
-- (the @W:@ loop's items).
gatherStatus :: ServerState -> Session -> IO (Map.Map Text Text, [Map.Map Text Text])
gatherStatus st sess = do
    env <- sessionFormatEnv st sess
    ws <- Map.toAscList <$> readTVarIO sess.windows
    wins <- mapM (\(ix, win) -> windowOwnEnv st sess ix win) ws
    pure (env, wins)

-- | The ranges across status line @i@ as the client sees it, for a mouse
-- click there.
statusRangesFor :: ServerState -> Session -> Options -> Int -> Int -> IO (V.Vector DrawRange)
statusRangesFor st sess sopts width i = do
    (env, winEnvs) <- gatherStatus st sess
    ctx <- formatCtxFor st env
    pure (statusRanges ctx { F.windows = winEnvs } sopts width i)
