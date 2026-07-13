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
    , PipeHandle (..)
    , Client (..)
    , CopyModeState (..)
    , CharSearch (..)
    , PromptState (..)
    , PickerState (..)
    , PickerNode (..)
    , SelKind (..)
    , newServerState
    , bumpDirty
    , freshId
    , sessionClients
    , currentWindow
    , activePane
    , windowPanes
    ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM
import Data.IORef (IORef)
import System.IO (Handle)
import System.Process (ProcessHandle)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.Socket (Socket)

import Hat.Geometry
import Hat.Log (Logger)
import Hat.Model.Ids
import qualified Hat.Pty
import Hat.Model.Options (Keymap, Options, defaultOptions)
import Hat.Server.Keys (PrefixState)
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
    , nextBuffer  :: TVar Int    -- ^ counter for auto-named paste buffers
    , dirty       :: TVar Int    -- ^ render generation; renderers wait on it
    , everAttached :: TVar Bool  -- ^ exit-when-empty arms only after first session
    , configLoading :: TVar Bool  -- ^ suppress waitIdle exit while config runs
    , restoring   :: TVar Bool  -- ^ a bare attach waits on this so it joins the
                                --   restored tree instead of making a fresh session
    , options     :: TVar Options
    , keymap      :: TVar Keymap
    , buffers     :: TVar (Seq (Text, Text))
        -- ^ paste-buffer stack; front = most recently added (@buffer0 = head@).
    , shellCache  :: TVar (Map Text (UTCTime, Text))  -- ^ #(cmd) results
    , cmdHistory  :: TVar [Text]  -- ^ command-prompt history, most-recent first
    , markedPane  :: TVar (Maybe PaneId)  -- ^ the marked pane (@select-pane -m@)
    , logger      :: Logger
    , sockPath    :: FilePath
    , store       :: Maybe FilePath
        -- ^ SQLite persistence file for this socket; 'Nothing' disables
        --   persistence entirely (used by tests via @HAT_PERSIST=0@).
    }

data Session = Session
    { id       :: SessionId
    , name     :: TVar Text
    , windows  :: TVar (Map Int Window)  -- ^ keyed by window index (sparse)
    , currentIx :: TVar Int
    , lastIx   :: TVar (Maybe Int)
    , lastSize :: TVar Size              -- ^ effective size while no client is attached
    , environ  :: TVar [(Text, Text)]    -- ^ env for new panes; refreshed on attach (update-environment)
    , startCwd :: FilePath               -- ^ default working directory for new windows
    }

data Window = Window
    { id         :: WindowId
    , name       :: TVar Text
    , layout     :: TVar Layout
    , panes      :: TVar (Map PaneId Pane)
    , activeId   :: TVar PaneId
    , lastActive :: TVar (Maybe PaneId)
    , bellFlag   :: TVar Bool
    , activity   :: TVar Bool  -- ^ output since last viewed (monitor-activity)
    , zoomed     :: TVar (Maybe PaneId)
    , autoRename :: TVar Bool
        -- ^ when set, the name tracks the active pane's foreground command
        -- (@automatic-rename@); an explicit @rename-window@ clears it.
    }

data Pane = Pane
    { id       :: PaneId
    , pty      :: Hat.Pty.PtyHandle
    , emulator :: Emu.Emulator
    , size     :: TVar Size
    , dead     :: TVar Bool
    , startCwd :: FilePath
    , mode     :: TVar (Maybe CopyModeState)
        -- ^ 'Nothing' = normal shell input; 'Just' = copy mode holding
        -- its own cursor/selection over the pane's scrollback + screen.
    , pipe     :: TVar (Maybe PipeHandle)
        -- ^ an active @pipe-pane@ subprocess, if any.
    }

-- | A @pipe-pane@ subprocess. Pane output is forwarded to 'toStdin'
-- (@-O@); 'reader' is the thread pumping the process's stdout back into
-- the pane pty (@-I@).
data PipeHandle = PipeHandle
    { process :: ProcessHandle
    , toStdin :: Maybe Handle
    , reader  :: Maybe ThreadId
    }

-- | Per-pane copy-mode state. Cursor rows are absolute in the pane's
-- grid: row @0@ is the oldest scrollback line, rows @hsize@ through
-- @hsize+sy-1@ are the live screen.
data CopyModeState = CopyModeState
    { cursorRow    :: !Int
    , cursorCol    :: !Int
    , selection    :: !(Maybe ((Int, Int), SelKind))
        -- ^ anchor position + kind; the current cursor is the other endpoint.
    , keyTable     :: !Text   -- ^ @copy-mode@ or @copy-mode-vi@
    , viewportOffY :: !Int    -- ^ lines scrolled up from the bottom
    , numPrefix    :: !(Maybe Int)
        -- ^ the @[count]@ being typed; the next motion runs this many times.
    , pendingSearch :: !(Maybe CharSearch)
        -- ^ an @f@/@F@/@t@/@T@ awaiting its target character.
    , lastSearch    :: !(Maybe (CharSearch, Char))
        -- ^ the most recent char search, for @;@ (repeat) and @,@ (reverse).
    , lastQuery     :: !(Maybe (Text, Bool))
        -- ^ the most recent string search (@/@ @?@): query + forward flag,
        -- for @n@ (repeat) and @N@ (reverse).
    }
    deriving (Eq, Show)

-- | Selection granularity, matching tmux's @SEL_CHAR@ / @SEL_WORD@ /
-- @SEL_LINE@ (plus rectangle).
data SelKind = SelChar | SelWord | SelLine | SelRect
    deriving (Eq, Show)

-- | A vi character search on the current line: @f@\/@t@ go forward, @F@\/@T@
-- backward; @t@\/@T@ ('till') stop one cell short of the target.
data CharSearch = CharSearch
    { searchForward :: !Bool
    , searchTill    :: !Bool
    }
    deriving (Eq, Show)

-- | Per-client command-prompt state: the line being edited, the cursor's
-- index into it, and (when browsing) the position in command history.
data PromptState = PromptState
    { input   :: !Text
    , cursor  :: !Int          -- ^ index into 'input', @0..T.length input@
    , histIx  :: !(Maybe Int)  -- ^ 'Nothing' while editing a fresh line;
                               --   @Just n@ while showing history entry @n@
    , pending :: !Text         -- ^ the fresh line stashed while browsing history
    , template :: !Text        -- ^ @command-prompt@ template; the submitted
                               --   line is spliced in for @%%@/@%1@. Empty
                               --   means run the line verbatim.
    , promptLabel :: !Text     -- ^ text shown before the edit buffer (@:@ for
                               --   the bare prompt, @(rename-window) @ etc.)
    , killed  :: !Text         -- ^ the last killed text, yanked back by @C-y@
    }
    deriving (Eq, Show)

-- | A modal chooser overlay (@choose-tree@ / @choose-window@): a
-- filterable tree of sessions, windows and panes where each node carries
-- the command to run when selected. Opens in menu mode; @/@ switches to
-- search, where keys type into 'query' instead of navigating.
data PickerState = PickerState
    { title     :: !Text
    , roots     :: ![PickerNode]
    , cursor    :: !Int    -- ^ index into the /visible/ (flattened) rows
    , query     :: !Text
    , searching :: !Bool
    , zoomed    :: !Bool   -- ^ @-Z@: fill the window, not just the pane
    }
    deriving (Eq, Show)

-- | One node in the chooser tree. Sessions hold window nodes, which hold
-- pane nodes; a childless node (e.g. @choose-window@) is a plain leaf.
data PickerNode = PickerNode
    { label    :: !Text  -- ^ shown, and matched against 'query'
    , command  :: !Text  -- ^ command line run when the node is chosen
    , preview  :: !(Maybe PaneId)  -- ^ pane whose contents preview this node
    , children :: ![PickerNode]
    , expanded :: !Bool  -- ^ whether 'children' are shown
    }
    deriving (Eq, Show)

data Client = Client
    { id        :: ClientId
    , sock      :: Socket
    , sendLock  :: MVar ()
    , size      :: TVar Size
    , session   :: TVar SessionId
    , lastSession :: TVar (Maybe SessionId)
    , keyState  :: IORef PrefixState  -- input thread only
    , lastFrame :: IORef Frame        -- render thread only
    , lastCursor :: IORef (Pos, Bool)
    , needsFull :: TVar Bool
    , toast     :: TVar (Maybe Text)  -- ^ display-message overlay
    , prompt    :: TVar (Maybe PromptState)  -- ^ command-prompt line editor
    , picker    :: TVar (Maybe PickerState)  -- ^ choose-tree/window overlay
    , env       :: [(Text, Text)]
    , cwd       :: Text
    }

newServerState :: Keymap -> Logger -> FilePath -> Maybe FilePath -> IO ServerState
newServerState defaultKeymap lg path storePath = ServerState
    <$> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO False
    <*> newTVarIO False
    <*> newTVarIO False
    <*> newTVarIO defaultOptions
    <*> newTVarIO defaultKeymap
    <*> newTVarIO Seq.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO []
    <*> newTVarIO Nothing
    <*> pure lg
    <*> pure path
    <*> pure storePath

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
