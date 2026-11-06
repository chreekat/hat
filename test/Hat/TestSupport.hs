-- Helpers shared by more than one spec.
module Hat.TestSupport
    (bashPath, nodeOf, clientTheme, addClient, realPanes, withPanesReaped) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar)
import Control.Exception (finally)
import Control.Monad (forM_)
import Data.IORef (newIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (utc, utcToZonedTime)
import Data.Vector qualified as V
import Network.Socket (Family (AF_UNIX), SocketType (Stream), defaultProtocol, socket)
import System.Directory (findExecutable)
import System.Timeout (timeout)

import Hat.Geometry (Pos (..), Size (..))
import Hat.Model
import Hat.Model.Options (Options)
import Hat.Server.Dispatch (runArgv)
import Hat.Server.Format (renderFormat)
import Hat.Server.Keys (EscPending (NoEscPending), initialKeyState)
import Hat.Server.Layout (Cell (..), Layout (..), Orientation (..), fitLayout, fixOffsets)
import Hat.Server.Outbound (newOutbound)
import Hat.Server.Render (blankFrame)
import Hat.Server.Theme (Theme, clientColours, resolveTheme)
import Hat.Term.HostProtocol (defaultCursorModel)
import Hat.Term.Pty qualified as Pty
import Hat.Transport.Wire (Autostart (Joined), protocolVersion)

-- The dev shell's bash, for tests that need a bashism (@exec -a@). Not
-- /bin/sh: that is bash only on NixOS, and dash elsewhere.
bashPath :: IO FilePath
bashPath = fromMaybe (error "bash is not on PATH") <$> findExecutable "bash"

-- | A layout node over the given children at the smallest size that holds
-- them; laying it out ('fitLayout', 'Hat.Server.Layout.arrange') spreads it
-- over a real window, roughly evenly.
nodeOf :: Orientation -> [Layout] -> Layout
nodeOf o cs = fixOffsets (Layout w h 0 0 (NodeCell o cs'))
  where
    across = case o of
        LeftRight -> maximum (map (.sy) cs)
        TopBottom -> maximum (map (.sx) cs)
    cs' = [ case o of
                LeftRight -> fitLayout (size c.sx across) c
                TopBottom -> fitLayout (size across c.sy) c
          | c <- cs ]
    along = sum [ case o of LeftRight -> c.sx; TopBottom -> c.sy | c <- cs' ]
        + length cs' - 1
    (w, h) = case o of
        LeftRight -> (along, across)
        TopBottom -> (across, along)
    size cols rows = Size { rows = fromIntegral rows, cols = fromIntegral cols }

-- | A client attached to the given session with a fixed size and activity
-- stamp (larger stamp = more recently active), registered with the server.
-- Its socket is a bare fd that is never driven: for what reads a client's
-- size, activity, session and history, not for what sends to it.
addClient :: ServerState -> SessionId -> Size -> Int -> IO Client
addClient st sid sz stamp = do
    sock <- socket AF_UNIX Stream defaultProtocol
    outboundQ <- newOutbound
    client <- Client (ClientId stamp) Attached Joined sock protocolVersion outboundQ
        <$> newTVarIO sz
        <*> newTVarIO stamp
        <*> newTVarIO sid
        <*> newTVarIO []
        <*> newTVarIO True
        <*> newIORef initialKeyState
        <*> newIORef NoEscPending
        <*> newIORef (blankFrame sz)
        <*> newIORef V.empty
        <*> newIORef Nothing
        <*> newIORef Nothing
        <*> newIORef Map.empty
        <*> newIORef Map.empty
        <*> newIORef (Pos 0 0, True)
        <*> newIORef Nothing
        <*> newIORef defaultCursorModel
        <*> newIORef 0        -- lastMouseMode
        <*> newTVarIO True
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing
        <*> newTVarIO Nothing  -- popup
        <*> newTVarIO True     -- outerFocused
        <*> newTVarIO ImportEnv
        <*> pure []
        <*> pure ""
        <*> pure ""  -- term
        <*> pure Nothing  -- ttyName
        <*> newTVarIO Nothing  -- ttyTerm
        <*> pure 0   -- createdAt
        <*> pure NoStdin
        <*> pure Nothing
        <*> newTVarIO unpannedViewport
        <*> newIORef (Pos 0 0)
    atomically $ modifyTVar' st.clients (Map.insert client.id client)
    pure client

-- | The theme a hat client draws these options with when the desktop
-- reports no colour scheme.
clientTheme :: Options -> Theme
clientTheme opts = resolveTheme expand opts Nothing
  where
    expand = renderFormat (Map.fromList [("client_colours", clientColours)])
        (const "") (utcToZonedTime utc (posixSecondsToUTCTime 0))

-- | Every pane of every window, in id order.
realPanes :: ServerState -> IO [Pane]
realPanes st = atomically $ do
    sessions <- Map.elems <$> readTVar st.sessions
    wins <- concat <$> mapM (fmap Map.elems . readTVar . (.windows)) sessions
    concat <$> mapM (fmap Map.elems . readTVar . (.panes)) wins

-- | Run @body@ on a server whose panes are real; afterwards kill them all
-- and reap their children, however the body ends.
withPanesReaped :: ServerState -> IO a -> IO a
withPanesReaped st body = body `finally` do
    panes <- realPanes st
    _ <- runArgv st Nothing ["kill-server"]
    forM_ panes $ \p -> timeout 2_000_000 (Pty.waitExit p.pty)
