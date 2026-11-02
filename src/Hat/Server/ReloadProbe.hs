-- | Asking a binary, before exec'ing into it, which reload eras it can read.
-- See Note [Probing a reload target].
module Hat.Server.ReloadProbe
    ( ReloadEras
    , erasFromRanges
    , erasRanges
    , erasMember
    , probeArg
    , renderProbeAnswer
    , parseProbeAnswer
    , ProbeOutcome (..)
    , ProbeFailure (..)
    , judgeReloadTarget
    , probeReloadTarget
    ) where

import Control.Exception (IOException, bracket, try)
import Data.ByteString qualified as B
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (IOMode (..), withFile)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Posix.Temp (mkdtemp)
import System.Process
    ( CreateProcess (..), StdStream (..), getPid, proc, waitForProcess
    , withCreateProcess )
import System.Timeout (timeout)

-- Note [Probing a reload target]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- An in-place reload execs whatever binary @hat@ names on PATH, which can be
-- older than the running server (a stale system profile). A build that cannot
-- read the handover hangs up every pane, so before exec'ing, the server asks
-- the target @hat --reload-eras@ and goes ahead only on an answer that names
-- its own era ('judgeReloadTarget'). Only a positive answer counts: builds
-- older than the probe take the flag for a command word, print an error and
-- exit, which is a refusal like any other non-answer.
--
-- The probe runs with a private, throwaway @HOME@ and @TMUX_TMPDIR@, no
-- @TMUX@, no inherited fds and no terminal, so a build that does take it for
-- a command can neither reach a live server nor start one we would leak.
--
-- A restarting client re-execs too, and does the same probe on its own
-- target: an answer of any kind means a build that understands the restart
-- argv; anything else and it re-execs itself instead.

-- | A set of reload eras, as sorted, disjoint, non-adjacent inclusive ranges.
newtype ReloadEras = ReloadEras [(Int, Int)]
    deriving stock (Eq, Show)

erasFromRanges :: [(Int, Int)] -> ReloadEras
erasFromRanges = ReloadEras . merge . sortOn fst . filter (uncurry (<=))
  where
    merge ((a, b) : (c, d) : rest)
        | c <= b + 1 = merge ((a, max b d) : rest)
    merge (r : rest) = r : merge rest
    merge [] = []

erasRanges :: ReloadEras -> [(Int, Int)]
erasRanges (ReloadEras rs) = rs

erasMember :: Int -> ReloadEras -> Bool
erasMember e (ReloadEras rs) = any (\(lo, hi) -> lo <= e && e <= hi) rs

-- | The argv word that asks a build for its 'renderProbeAnswer'. Frozen.
probeArg :: String
probeArg = "--reload-eras"

-- | The probe's one-line answer: @hat-reload-eras 1 \<eras\>@, the eras as
-- comma-separated @lo-hi@ ranges or single eras. Format 1 may grow only by
-- fields appended after the eras.
renderProbeAnswer :: ReloadEras -> Text
renderProbeAnswer es = T.unwords [answerTag, "1", renderEras es]

answerTag :: Text
answerTag = "hat-reload-eras"

renderEras :: ReloadEras -> Text
renderEras = T.intercalate "," . map range . erasRanges
  where
    range (lo, hi)
        | lo == hi = tshow lo
        | otherwise = tshow lo <> "-" <> tshow hi

-- | Read a probe's output. Anything but a format-1 answer is a 'Left'.
parseProbeAnswer :: Text -> Either Text ReloadEras
parseProbeAnswer out = case T.words (T.takeWhile (/= '\n') out) of
    (tag : "1" : eras : _) | tag == answerTag ->
        erasFromRanges <$> traverse range (T.splitOn "," eras)
    (tag : fmt : _) | tag == answerTag -> Left ("unknown answer format " <> fmt)
    _ -> Left ("no answer line in " <> tshow (T.take 80 out))
  where
    range r = case T.splitOn "-" r of
        [e] -> (\n -> (n, n)) <$> era e
        [lo, hi] -> do
            l <- era lo
            h <- era hi
            if l <= h then Right (l, h) else Left ("backwards range " <> r)
        _ -> Left ("bad range " <> r)
    era t = case TR.decimal t of
        Right (n, "") | n >= 1 -> Right n
        _ -> Left ("bad era " <> t)

-- | What probing a target found.
data ProbeOutcome
    = Answered ReloadEras
    | NoAnswer ProbeFailure
    deriving stock (Eq, Show)

data ProbeFailure
    = ProbeTimedOut
    | ProbeExited Int        -- ^ a nonzero exit, with no answer
    | ProbeUnreadable Text   -- ^ exited 0, but printed no answer we can read
    | ProbeUnrunnable Text   -- ^ could not be started at all
    deriving stock (Eq, Show)

-- | Why the reload named @name@ must not exec @target@ from a server at
-- @era@, or 'Nothing' when the target reads that era.
-- See Note [Probing a reload target].
judgeReloadTarget :: Text -> FilePath -> Int -> ProbeOutcome -> Maybe Text
judgeReloadTarget name target era = \case
    Answered es
        | erasMember era es -> Nothing
        | otherwise -> refuse $ "cannot read this server's state (it reads eras "
            <> renderEras es <> ", this is era " <> tshow era <> ")"
    NoAnswer why -> refuse $ "cannot say which reload eras it reads ("
        <> failure why <> "), so it may not read this server's state (era "
        <> tshow era <> ")"
  where
    refuse why = Just (name <> ": " <> T.pack target <> " " <> why <> "; not restarting")
    failure = \case
        ProbeTimedOut -> "it gave no answer within " <> tshow probeSeconds <> "s"
        ProbeExited n -> "it exited " <> tshow n <> " without an answer"
        ProbeUnreadable e -> "its answer was unreadable: " <> e
        ProbeUnrunnable e -> "it could not be run: " <> e

-- | Ask @target@ which reload eras it reads. See Note [Probing a reload target].
probeReloadTarget :: FilePath -> IO ProbeOutcome
probeReloadTarget target = do
    tmp <- getTemporaryDirectory
    r <- try $ bracket (mkdtemp (tmp <> "/hat-probe-")) removeDirectoryRecursive $ \dir ->
        withFile "/dev/null" ReadWriteMode $ \null' -> do
            env <- isolated dir <$> getEnvironment
            withCreateProcess (proc target [probeArg])
                { env = Just env, close_fds = True
                , std_in = UseHandle null', std_out = CreatePipe
                , std_err = UseHandle null' } $ \_ mout _ ph -> do
                answer <- timeout (probeSeconds * 1_000_000) $ do
                    out <- maybe (pure B.empty) B.hGetContents mout
                    code <- waitForProcess ph
                    pure (code, out)
                case answer of
                    Nothing -> do
                        getPid ph >>= mapM_ (signalProcess sigKILL)
                        NoAnswer ProbeTimedOut <$ waitForProcess ph
                    Just (ExitFailure n, _) -> pure (NoAnswer (ProbeExited n))
                    Just (ExitSuccess, out) ->
                        pure $ either (NoAnswer . ProbeUnreadable) Answered
                            (parseProbeAnswer (TE.decodeUtf8Lenient out))
    pure $ either (\(e :: IOException) -> NoAnswer (ProbeUnrunnable (tshow e))) id r
  where
    isolated dir env =
        [("HOME", dir), ("TMUX_TMPDIR", dir)]
            <> filter ((`notElem` ["HOME", "TMUX_TMPDIR", "TMUX"]) . fst) env

-- | The probe's deadline.
probeSeconds :: Int
probeSeconds = 2

tshow :: Show a => a -> Text
tshow = T.pack . show
