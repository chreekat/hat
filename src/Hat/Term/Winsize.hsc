{-# LANGUAGE CApiFFI #-}

-- | The pty window-size ioctls and SIGWINCH — the kernel-header slice of
-- "Hat.Term.Pty", kept alone so a header change rebuilds only this module.
module Hat.Term.Winsize
    ( getWinsize
    , setWinsize
    , sigWinch
    ) where

#include <signal.h>
#include <sys/ioctl.h>
#include <termios.h>

import Foreign
import Foreign.C.Types
import System.Posix.Signals (Signal)
import System.Posix.Types (Fd (..))

import Hat.Geometry

foreign import capi "sys/ioctl.h ioctl"
    c_ioctl :: CInt -> CULong -> Ptr () -> IO CInt

setWinsize :: Fd -> Size -> IO ()
setWinsize (Fd fd) sz =
    allocaBytes #{size struct winsize} $ \ws -> do
        #{poke struct winsize, ws_row} ws (fromIntegral sz.rows :: CUShort)
        #{poke struct winsize, ws_col} ws (fromIntegral sz.cols :: CUShort)
        #{poke struct winsize, ws_xpixel} ws (0 :: CUShort)
        #{poke struct winsize, ws_ypixel} ws (0 :: CUShort)
        _ <- c_ioctl fd #{const TIOCSWINSZ} ws
        pure ()

-- | The unix package doesn't export SIGWINCH.
sigWinch :: Signal
sigWinch = #{const SIGWINCH}

-- | Current size of a terminal, e.g. the client's own tty. Falls back
-- to 80x24 when the fd is not a terminal.
getWinsize :: Fd -> IO Size
getWinsize (Fd fd) =
    allocaBytes #{size struct winsize} $ \ws -> do
        rc <- c_ioctl fd #{const TIOCGWINSZ} ws
        if rc /= 0
            then pure Size { rows = 24, cols = 80 }
            else do
                r <- #{peek struct winsize, ws_row} ws :: IO CUShort
                c <- #{peek struct winsize, ws_col} ws :: IO CUShort
                if r == 0 || c == 0
                    then pure Size { rows = 24, cols = 80 }
                    else pure Size
                        { rows = fromIntegral r, cols = fromIntegral c }
