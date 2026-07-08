-- | Unix-socket location and (later) listen/accept plumbing.
module Hat.Socket
    ( socketDir
    , socketPath
    ) where

import System.Posix.Types (CUid)

-- | Directory holding hat sockets: @$TMUX_TMPDIR/hat-$UID@, or
-- @/tmp/hat-$UID@ when the variable is unset.
socketDir :: Maybe FilePath -> CUid -> FilePath
socketDir tmpdir uid =
    maybe "/tmp" id tmpdir <> "/hat-" <> show (toInteger uid)

-- | Full path of a named socket inside 'socketDir'.
socketPath :: Maybe FilePath -> CUid -> String -> FilePath
socketPath tmpdir uid name = socketDir tmpdir uid <> "/" <> name
