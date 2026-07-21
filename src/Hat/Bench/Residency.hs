-- | Verdict logic for the memory-residency benchmark: given a measured
-- live-heap figure and a committed baseline, decide regression vs improvement
-- vs within-band. Pure so it can be unit-tested away from the RTS.
module Hat.Bench.Residency
    ( LiveBytes (..)
    , Baseline (..)
    , Tolerance
    , mkTolerance
    , toleranceFraction
    , Verdict (..)
    , classify
    ) where

import Data.Word (Word64)

-- | Live Haskell-heap bytes retained after a forced major GC, as reported by
-- 'GHC.Stats.gcdetails_live_bytes'.
newtype LiveBytes = LiveBytes Word64
    deriving stock (Eq, Ord, Show)

-- | The committed, expected 'LiveBytes' that a fresh measurement is judged
-- against.
newtype Baseline = Baseline LiveBytes
    deriving stock (Eq, Ord, Show)

-- | The fractional half-width of the accepted band around a 'Baseline' (0.03
-- means ±3%). Held in [0, 1) so the lower edge stays positive; build with
-- 'mkTolerance'.
newtype Tolerance = Tolerance Double
    deriving stock (Eq, Ord, Show)

-- | Build a 'Tolerance', clamping into [0, 1) so a fat-fingered value can
-- never invert the band or drop its lower edge to zero.
mkTolerance :: Double -> Tolerance
mkTolerance t = Tolerance (max 0 (min 0.999 t))

-- | The (clamped) fraction a 'Tolerance' represents, e.g. 0.05 for a ±5% band.
toleranceFraction :: Tolerance -> Double
toleranceFraction (Tolerance t) = t

-- | The outcome of 'classify'. 'Regressed' and 'Improved' carry the measured
-- figure so the caller can report it and, on an improvement, ratchet the
-- baseline down to it.
data Verdict
    = WithinBand
    | Regressed LiveBytes
    | Improved LiveBytes
    deriving stock (Eq, Show)

-- | Judge a measurement against the band @baseline ± tolerance·baseline@: a
-- figure above the band is a 'Regressed', below it an 'Improved', and anything
-- on or inside the (inclusive) edges is 'WithinBand'.
classify :: Tolerance -> Baseline -> LiveBytes -> Verdict
classify (Tolerance tol) (Baseline (LiveBytes base)) measured@(LiveBytes m)
    | m > hi    = Regressed measured
    | m < lo    = Improved measured
    | otherwise = WithinBand
  where
    hi = floor   (fromIntegral base * (1 + tol))
    lo = ceiling (fromIntegral base * (1 - tol))
