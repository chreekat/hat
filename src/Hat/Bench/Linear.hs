-- | Least-squares line fit for the perf benchmark: instructions over workload
-- size, whose slope is the marginal per-unit cost. Pure so the fit is
-- testable away from perf.
module Hat.Bench.Linear
    ( Line (..)
    , fitLinear
    , fitFloor
    ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | The fitted line @y = slope · x + intercept@.
data Line = Line
    { slope :: Double
    , intercept :: Double
    }
    deriving stock (Eq, Show)

-- | Fit @y = a·x + b@ by least squares. Fewer than two points, or points
-- sharing one x, determine no line and are a 'Left'.
fitLinear :: [(Double, Double)] -> Either Text Line
fitLinear pts
    | length pts < 2 = Left "fitLinear: need at least two points"
    | sxx == 0 = Left "fitLinear: all points share one x"
    | otherwise = Right Line { slope = a, intercept = ybar - a * xbar }
  where
    n = fromIntegral (length pts)
    xbar = sum (map fst pts) / n
    ybar = sum (map snd pts) / n
    sxx = sum [ (x - xbar) ^ (2 :: Int) | (x, _) <- pts ]
    sxy = sum [ (x - xbar) * (y - ybar) | (x, y) <- pts ]
    a = sxy / sxx

-- | 'fitLinear' through each x's least y. Run-to-run noise in an
-- instruction count only ever adds, so a size's cheapest repeat is its cost.
fitFloor :: [(Double, Double)] -> Either Text Line
fitFloor = fitLinear . Map.toList . Map.fromListWith min
