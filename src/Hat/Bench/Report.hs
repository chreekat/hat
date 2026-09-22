-- | Human-readable reports over fitted benchmark series: a summary table
-- with hat:tmux ratios, a before\/after comparison, and a pass\/fail check
-- against a recorded baseline. Pure so every report is testable away from
-- perf; discovering and fitting the series is the caller's IO.
module Hat.Bench.Report
    ( SeriesKey (..)
    , renderKey
    , parseSeriesFile
    , humanize
    , summaryTable
    , compareTable
    , Baseline (..)
    , parseBaseline
    , renderBaseline
    , Check (..)
    , Verdict (..)
    , checkBaseline
    , checkPassed
    , renderChecks
    ) where

import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Numeric (showFFloat)

import Hat.Bench.Linear

-- | One measured series: which multiplexer, which workload, which process.
data SeriesKey = SeriesKey
    { mux :: Text
    , workload :: Text
    , role :: Text
    }
    deriving stock (Eq, Ord, Show)

renderKey :: SeriesKey -> Text
renderKey k = T.unwords [k.mux, k.workload, k.role]

-- | The series and size a perf CSV belongs to, from the
-- @\<mux>-\<workload>-\<role>-\<N>.csv@ name the bench driver writes.
parseSeriesFile :: Text -> Maybe (SeriesKey, Int)
parseSeriesFile name = do
    stem <- T.stripSuffix ".csv" name
    case T.splitOn "-" stem of
        [m, w, r, n] -> case TR.decimal n of
            Right (size, "") ->
                Just (SeriesKey { mux = m, workload = w, role = r }, size)
            _ -> Nothing
        _ -> Nothing

-- | Three significant digits with an engineering suffix: @28.1M@, @41.0k@.
humanize :: Double -> Text
humanize v
    | v < 0 = "-" <> humanize (negate v)
    | v >= 1e9 = sig3 (v / 1e9) <> "G"
    | v >= 1e6 = sig3 (v / 1e6) <> "M"
    | v >= 1e3 = sig3 (v / 1e3) <> "k"
    | otherwise = sig3 v

-- | Three significant digits, no suffix.
sig3 :: Double -> Text
sig3 m = T.pack (showFFloat (Just decimals) m "")
  where
    decimals
        | m >= 100 = 0
        | m >= 10 = 1
        | otherwise = 2

percent :: Double -> Text
percent p = sign <> T.pack (showFFloat (Just 1) p "") <> "%"
  where
    sign = if p >= 0 then "+" else ""

-- | One line per series (slope humanized, intercept aside), then a
-- @hat = Nx tmux@ ratio line for every series both muxes measured. A flat
-- tmux series (slope under one instruction) gets no ratio; against ~0 the
-- quotient is noise, not a comparison.
summaryTable :: [(SeriesKey, Line)] -> [Text]
summaryTable series = map row (sortOn fst series) <> ratios
  where
    row (k, l) = renderKey k <> ": " <> humanize l.slope
        <> " instr/unit (intercept " <> humanize l.intercept <> ")"
    ratios =
        [ k.workload <> " " <> k.role <> ": hat = "
            <> sig3 (l.slope / tl.slope) <> "x tmux"
        | (k, l) <- sortOn fst series
        , k.mux == "hat"
        , (tk, tl) <- series
        , tk == k { mux = "tmux" }
        , tl.slope >= 1
        ]

-- | Series against series: the slope before, after, and the change. A series
-- measured on only one side is named rather than dropped.
compareTable :: [(SeriesKey, Line)] -> [(SeriesKey, Line)] -> [Text]
compareTable before after =
    [ renderKey k <> ": " <> humanize old.slope <> " -> "
        <> humanize new.slope
        <> " (" <> percent ((new.slope - old.slope) / old.slope * 100) <> ")"
    | (k, old) <- sortOn fst before
    , Just new <- [lookup k after]
    ]
    <> [ renderKey k <> ": only in before" | (k, _) <- unmatched before after ]
    <> [ renderKey k <> ": only in after" | (k, _) <- unmatched after before ]
  where
    unmatched xs ys =
        sortOn fst [ x | x@(k, _) <- xs, Nothing <- [lookup k ys] ]

-- | Recorded slopes the check compares against, with the relative band
-- (e.g. 0.2 = ±20%) a measurement may drift within.
data Baseline = Baseline
    { tolerance :: Double
    , entries :: [(SeriesKey, Double)]
    }
    deriving stock (Eq, Show)

-- | Read a baseline file: @#@ comments and blanks skipped, one
-- @tolerance \<fraction>@ line, and one @\<mux> \<workload> \<role> \<slope>@
-- line per series.
parseBaseline :: Text -> Either Text Baseline
parseBaseline input = do
    rows <- traverse row (filter meaningful (T.lines input))
    case [ t | Left t <- rows ] of
        [t] -> Right Baseline
            { tolerance = t, entries = [ e | Right e <- rows ] }
        [] -> Left "baseline has no tolerance line"
        _ -> Left "baseline has more than one tolerance line"
  where
    meaningful l = not (T.null (T.strip l)) && not ("#" `T.isPrefixOf` l)
    row l = case T.words l of
        ["tolerance", v] -> Left <$> number v
        [m, w, r, v] -> Right . (SeriesKey { mux = m, workload = w, role = r },)
            <$> number v
        _ -> Left ("not a baseline line: " <> l)
    number v = case TR.double v of
        Right (x, "") -> Right x
        _ -> Left ("not a number: " <> v)

-- | The file 'parseBaseline' reads, slopes rounded to whole instructions.
renderBaseline :: Baseline -> Text
renderBaseline b = T.unlines $
    [ "# Fitted slope per series; `bench-report check` trips outside the band."
    , "tolerance " <> T.pack (showFFloat (Just 2) b.tolerance "")
    ]
    <> [ renderKey k <> " " <> T.pack (show (round v :: Integer))
       | (k, v) <- sortOn fst b.entries
       ]

-- | One baseline series' outcome. See 'checkBaseline'.
data Check = Check
    { key :: SeriesKey
    , verdict :: Verdict
    }
    deriving stock (Eq, Show)

data Verdict
    = InBand Double Double     -- ^ measured, baseline
    | Regressed Double Double  -- ^ measured, baseline
    | Improved Double Double   -- ^ measured, baseline
    | Unmeasured
    deriving stock (Eq, Show)

-- | Judge each baseline series against the measured slopes. Anything but
-- in-band — a regression, an improvement that makes the baseline stale, or
-- a series that went unmeasured — fails 'checkPassed'.
checkBaseline :: Baseline -> [(SeriesKey, Line)] -> [Check]
checkBaseline b measured =
    [ Check { key = k, verdict = judge base (lookup k measured) }
    | (k, base) <- sortOn fst b.entries
    ]
  where
    judge _ Nothing = Unmeasured
    judge base (Just l)
        | l.slope > base * (1 + b.tolerance) = Regressed l.slope base
        | l.slope < base * (1 - b.tolerance) = Improved l.slope base
        | otherwise = InBand l.slope base

checkPassed :: [Check] -> Bool
checkPassed = all $ \c -> case c.verdict of
    InBand {} -> True
    _ -> False

renderChecks :: [Check] -> [Text]
renderChecks = map render
  where
    render c = renderKey c.key <> ": " <> case c.verdict of
        InBand m base -> against m base
        Regressed m base -> against m base <> " REGRESSED"
        Improved m base ->
            against m base <> " improved; record a new baseline"
        Unmeasured -> "no measurement"
    against m base = humanize m <> " vs baseline " <> humanize base
        <> " (" <> percent ((m - base) / base * 100) <> ")"
