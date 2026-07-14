-- | The client's own terminal: raw mode discipline and size queries.
module Hat.Client.Tty
    ( withRawMode
    , ttySize
    ) where

import Control.Exception (bracket)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdin, stdout)
import System.Posix.IO (stdInput, stdOutput)
import System.Posix.Terminal

import Hat.Geometry (Size)
import Hat.Term.Pty (getWinsize)

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
