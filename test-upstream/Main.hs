-- | Runs the pinned upstream tmux @regress/@ corpus against the real @hat@
-- binary via @tools/run-upstream-tests.sh@, so @tools/upstream-xfail.txt@ stays
-- honest (a newly-passing script or a newly-failing one both fail this suite).
--
-- Opt-in: a bare @cabal test@ (the fast inner loop) skips it, because the
-- runner drives ~120 shell scripts and would blow the suite's time budget.
-- Set @HAT_UPSTREAM=1@ to actually run it (CI, or a deliberate regression
-- check). Opting in without the nix-pinned corpus ('HAT_TMUX_SRC') is a
-- misconfiguration and fails loud rather than skipping silently.
module Main (main) where

import Control.Monad (when)
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process (spawnProcess, waitForProcess)

main :: IO ()
main = do
    optIn <- (== Just "1") <$> lookupEnv "HAT_UPSTREAM"
    when (not optIn) $ do
        hPutStrLn stderr
            "hat-upstream: set HAT_UPSTREAM=1 to run the upstream regress suite; skipping."
        exitSuccess
    msrc <- lookupEnv "HAT_TMUX_SRC"
    case msrc of
        Nothing -> do
            hPutStrLn stderr
                "hat-upstream: HAT_UPSTREAM=1 but HAT_TMUX_SRC is unset \
                \(enter the nix dev shell for the pinned tmux corpus)."
            exitFailure
        Just _ ->
            spawnProcess "tools/run-upstream-tests.sh" []
                >>= waitForProcess >>= exitWith
