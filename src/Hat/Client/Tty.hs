-- | The client's own terminal: raw mode discipline and size queries.
module Hat.Client.Tty
    ( withRawMode
    , ttySize
    , TermProbe (..)
    , probeTerminal
    , diagnoseTerminal
    ) where

import Control.Exception (bracket, displayException, try)
import Data.Bifunctor (bimap)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO.Exception (IOException)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdin, stdout)
import System.Posix.IO (stdInput, stdOutput)
import System.Posix.Terminal
import System.Posix.Types (Fd)

import Hat.Geometry (Size)
import Hat.Term.Pty (getWinsize)

-- | What probing one fd found: whether @isatty@ accepts it, and whether the
-- @tcgetattr@ that raw mode actually needs succeeded (@Right@) or why it
-- failed (@Left@ the error text). See 'probeTerminal' and 'diagnoseTerminal'.
data TermProbe = TermProbe
    { isTty :: Bool
    , canTermios :: Either Text ()
    }
    deriving (Eq, Show)

-- | Probe an fd for the terminal control the client needs, recording both the
-- @isatty@ verdict and the @tcgetattr@ capability so a rejection can report
-- exactly what the environment offered.
probeTerminal :: Fd -> IO TermProbe
probeTerminal fd = do
    tty <- queryTerminal fd
    tio <- try (getTerminalAttributes fd) :: IO (Either IOException TerminalAttributes)
    pure TermProbe
        { isTty = tty
        , canTermios = bimap (T.pack . displayException) (const ()) tio
        }

-- | Decide whether the client can take over this terminal, from probes of
-- stdin and stdout. The gate is the real capability — @tcgetattr@ on stdin,
-- which raw mode requires — not the @isatty@ proxy, so a terminal whose
-- @isatty@ is a false negative (some emulated\/proot ptys, e.g. nix-on-droid)
-- is still accepted when its termios works. On failure the message names both
-- fds' @isatty@ and @tcgetattr@ results, so a headless or emulated environment
-- says why it was refused instead of a bare \"not a terminal\". 'Nothing'
-- means go ahead.
diagnoseTerminal :: TermProbe -> TermProbe -> Maybe Text
diagnoseTerminal inp out = case inp.canTermios of
    Right () -> Nothing
    Left _ -> Just $ T.intercalate "\n"
        [ "not a terminal"
        , "  stdin  (fd 0): " <> describe inp
        , "  stdout (fd 1): " <> describe out
        , "  attach needs a terminal on stdin for raw-mode input"
        ]
  where
    describe p =
        "isatty=" <> yesno p.isTty <> ", tcgetattr="
            <> either ("failed: " <>) (const "ok") p.canTermios
    yesno b = if b then "yes" else "no"

-- | Run an action with the controlling terminal in raw mode and stdio
-- unbuffered, restoring the original attributes afterwards no matter what.
--
-- The buffering changes live here because their order against the raw
-- switch matters twice over: @hSetBuffering stdin NoBuffering@ does a
-- hidden tcsetattr, and the first such call also makes GHC snapshot the
-- termios, which the RTS re-imposes at process exit. Setting buffering
-- while the terminal is still cooked keeps both our @saved@ copy and
-- that RTS snapshot equal to the state we must leave behind; setting it
-- after the raw switch made the RTS restore raw mode over our own
-- restore on every exit.
withRawMode :: IO a -> IO a
withRawMode body = do
    saved <- getTerminalAttributes stdInput
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    bracket
        (setTerminalAttributes stdInput (rawAttributes saved) Immediately)
        (\_ -> setTerminalAttributes stdInput saved Immediately)
        (\_ -> body)

rawAttributes :: TerminalAttributes -> TerminalAttributes
rawAttributes attrs =
    foldl withoutMode attrs
        [ EnableEcho
        , ProcessInput        -- ICANON
        , KeyboardInterrupts  -- ISIG
        , StartStopOutput     -- IXON
        , ExtendedFunctions   -- IEXTEN
        , ProcessOutput       -- OPOST
        , MapCRtoLF           -- ICRNL
        , InterruptOnBreak
        , CheckParity
        , StripHighBit
        ]
    `withMinInput` 1
    `withTime` 0

ttySize :: IO Size
ttySize = getWinsize stdOutput
