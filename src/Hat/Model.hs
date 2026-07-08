-- | The server's state tree: sessions > windows > panes, all under STM.
-- Renderers subscribe to a generation counter; any visible change bumps it.
module Hat.Model
    ( SessionId (..)
    , WindowId (..)
    , PaneId (..)
    , ClientId (..)
    , ServerState (..)
    , Session (..)
    , Window (..)
    , Pane (..)
    , Client (..)
    , newServerState
    , bumpDirty
    , freshId
    , sessionClients
    , currentWindow
    , activePane
    , windowPanes
    ) where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Network.Socket (Socket)

import Hat.Geometry
import Hat.Log (Logger)
import Hat.Model.Ids
import qualified Hat.Pty
import Hat.Server.Input (KeyState)
import Hat.Server.Layout (Layout)
import Hat.Server.Render (Frame)
import qualified Hat.Term.Emulator as Emu

data ServerState = ServerState
    { sessions    :: TVar (Map SessionId Session)
    , clients     :: TVar (Map ClientId Client)
    , nextSession :: TVar Int
    , nextWindow  :: TVar Int
    , nextPane    :: TVar Int
    , nextClient  :: TVar Int
    , dirty       :: TVar Int    -- ^ render generation; renderers wait on it
    , everAttached :: TVar Bool  -- ^ exit-when-empty arms only after first session
    , logger      :: Logger
    , sockPath    :: FilePath
    }

data Session = Session
    { id       :: SessionId
    , name     :: TVar Text
    , windows  :: TVar (Map Int Window)  -- ^ keyed by window index (sparse)
    , currentIx :: TVar Int
    , lastIx   :: TVar (Maybe Int)
    , lastSize :: TVar Size              -- ^ effective size while no client is attached
    , environ  :: [(Text, Text)]         -- ^ from the creating client; used for new panes
    }

data Window = Window
    { id         :: WindowId
    , name       :: TVar Text
    , layout     :: TVar Layout
    , panes      :: TVar (Map PaneId Pane)
    , activeId   :: TVar PaneId
    , lastActive :: TVar (Maybe PaneId)
    , bellFlag   :: TVar Bool
    , zoomed     :: TVar (Maybe PaneId)
    }

data Pane = Pane
    { id       :: PaneId
    , pty      :: Hat.Pty.PtyHandle
    , emulator :: Emu.Emulator
    , size     :: TVar Size
    , dead     :: TVar Bool
    , startCwd :: FilePath
    }

data Client = Client
    { id        :: ClientId
    , sock      :: Socket
    , sendLock  :: MVar ()
    , size      :: TVar Size
    , session   :: TVar SessionId
    , keyState  :: IORef KeyState  -- input thread only
    , lastFrame :: IORef Frame     -- render thread only
    , lastCursor :: IORef (Pos, Bool)
    , needsFull :: TVar Bool
    }

newServerState :: Logger -> FilePath -> IO ServerState
newServerState lg path = ServerState
    <$> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO False
    <*> pure lg
    <*> pure path

bumpDirty :: ServerState -> STM ()
bumpDirty st = modifyTVar' st.dirty (+ 1)

freshId :: TVar Int -> STM Int
freshId counter = do
    n <- readTVar counter
    writeTVar counter (n + 1)
    pure n

sessionClients :: ServerState -> SessionId -> STM [Client]
sessionClients st sid = do
    cs <- readTVar st.clients
    filterM' (Map.elems cs)
  where
    filterM' [] = pure []
    filterM' (c : rest) = do
        csid <- readTVar c.session
        others <- filterM' rest
        pure $ if csid == sid then c : others else others

currentWindow :: Session -> STM (Maybe Window)
currentWindow sess = do
    ix <- readTVar sess.currentIx
    ws <- readTVar sess.windows
    pure (Map.lookup ix ws)

activePane :: Window -> STM (Maybe Pane)
activePane win = do
    pid <- readTVar win.activeId
    ps <- readTVar win.panes
    pure (Map.lookup pid ps)

windowPanes :: Window -> STM [Pane]
windowPanes win = Map.elems <$> readTVar win.panes
