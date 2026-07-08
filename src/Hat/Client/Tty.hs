-- | The client's own terminal: raw mode discipline and size queries.
module Hat.Client.Tty
    ( withRawMode
    , ttySize
    ) where

import Control.Exception (bracket)
import System.Posix.IO (stdInput, stdOutput)
import System.Posix.Terminal

import Hat.Geometry (Size)
import Hat.Pty (getWinsize)

-- | Run an action with the controlling terminal in raw mode, restoring
-- the original attributes afterwards no matter what.
withRawMode :: IO a -> IO a
withRawMode body = do
    saved <- getTerminalAttributes stdInput
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
