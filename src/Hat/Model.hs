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
    , SearchDirection (..)
    , flipDirection
    , CharStop (..)
    , CharSearch (..)
    , PromptState (..)
    , PickerState (..)
    , PickerNode (..)
    , Expansion (..)
    , PickerMode (..)
    , PickerFill (..)
    , PreviewTarget (..)
    , SelKind (..)
    , newServerState
    , bumpDirty
    , freshId
    , resolveGlobal
    , resolveForSession
    , resolveForWindow
    , sessionClients
    , currentWindow
    , activePane
    , windowPanes
    , windowArea
    , paneCurrentPath
    , findPaneById
    , findWindowById
    , findSessionById
    , rawClient
    , rawPane
    , rawSession
    , tshow
    ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM
import Control.Exception (IOException, try)
import Control.Monad (forM)
import Data.IORef (IORef)
import qualified Data.List as List
import System.IO (Handle)
import System.Process (ProcessHandle)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Network.Socket (Socket)
import qualified System.Posix.Files as PFiles

import Hat.Geometry
import Hat.Log (Logger)
import Hat.Model.Ids
import qualified Hat.Term.Pty
import Hat.Model.Options
    (Keymap, Options, OptionsDelta, defaultOptions, emptyDelta, resolveOptions)
import Hat.Server.ColorScheme (ColorScheme)
import Hat.Server.Keys (PrefixState)
import Hat.Server.Layout (Layout, LayoutName)
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
    , everAttached :: TVar Bool  -- ^ a session has existed at some point.
                                --   See 'waitIdle'.
    , served      :: TVar Bool  -- ^ a client connection has been accepted.
                                --   See 'waitIdle'.
    , configLoading :: TVar Bool  -- ^ config sourcing is in progress. See 'waitIdle'.
    , restoring   :: TVar Bool  -- ^ a persisted tree is being restored. See
                                --   'ensureSession'.
    , preserveStore :: TVar Bool
        -- ^ the store holds an explicitly saved final tree that must
        --   survive shutdown. Set by @kill-server@; off across a natural
        --   drain. See 'runServer' (keep vs drop at drain) and
        --   'persistDecision' (the pin against mirror writes).
    , colorScheme :: TVar (Maybe ColorScheme)
        -- ^ the desktop's light/dark preference, when a watcher could
        --   determine it ('Nothing' outside a desktop session). Drives
        --   @#{color_scheme}@ and sourcing of the @\@color-scheme-dark@ /
        --   @\@color-scheme-light@ config files.
    , options     :: TVar Options
    , serverOptions :: TVar OptionsDelta          -- ^ see 'resolveGlobal'
    , globalSessionOptions :: TVar OptionsDelta   -- ^ see 'resolveForSession'
    , globalWindowOptions :: TVar OptionsDelta     -- ^ see 'resolveForWindow'
    , schemeOptions :: TVar OptionsDelta            -- ^ see 'resolveGlobal'
    , keymap      :: TVar Keymap
    , buffers     :: TVar (Seq (Text, Text))
        -- ^ paste-buffer stack; front = most recently added (@buffer0 = head@).
    , shellCache  :: TVar (Map Text (UTCTime, Text))  -- ^ #(cmd) results
    , cmdHistory  :: TVar [Text]  -- ^ command-prompt history, most-recent first
    , markedPane  :: TVar (Maybe PaneId)  -- ^ the marked pane (@select-pane -m@)
    , lastActiveSession :: TVar (Maybe SessionId)
        -- ^ the session most recently focused by any client; captured into
        --   the snapshot by name. See 'pickAttachSession'.
    , logger      :: Logger
    , sockPath    :: FilePath
    , store       :: Maybe FilePath
        -- ^ SQLite persistence file for this socket; 'Nothing' disables
        --   persistence entirely (used by tests via @HAT_PERSIST=0@).
    , listenFd    :: TVar (Maybe Int)
        -- ^ the listening socket's fd, published once it is bound. An
        --   in-place reload hands this fd to its re-exec'd image; see
        --   'Hat.Server.cmdRestartServer'.
    , serverConfig :: TVar (Maybe FilePath)
        -- ^ the config file this server was started with, replayed into the
        --   reload re-exec argv. See 'Hat.Server.cmdRestartServer'.
    }

data Session = Session
    { id       :: SessionId
    , name     :: TVar Text
    , windows  :: TVar (Map Int Window)  -- ^ keyed by window index (sparse)
    , currentIx :: TVar Int
    , lastIx   :: TVar (Maybe Int)
    , lastSize :: TVar Size              -- ^ effective size while no client is attached
    , environ  :: TVar [(Text, Text)]    -- ^ env for new panes; refreshed on attach (update-environment)
    , startCwd :: TVar FilePath          -- ^ default working directory for new windows; @attach-session -c@ re-anchors it
    , options  :: TVar OptionsDelta      -- ^ session-scoped set-option; see 'resolveForSession'
    }

data Window = Window
    { id         :: WindowId
    , name       :: TVar Text
    , layout     :: TVar Layout
    , layoutName :: TVar (Maybe LayoutName)
        -- ^ the last named layout applied, if any; see 'cmdNextLayout'.
    , panes      :: TVar (Map PaneId Pane)
    , activeId   :: TVar PaneId
    , lastActive :: TVar (Maybe PaneId)
    , bellFlag   :: TVar Bool
    , activity   :: TVar Bool  -- ^ output since last viewed (monitor-activity)
    , zoomed     :: TVar (Maybe PaneId)
    , autoRename :: TVar Bool
        -- ^ when set, the name tracks the active pane's foreground command
        -- (@automatic-rename@); an explicit @rename-window@ clears it.
    , options    :: TVar OptionsDelta  -- ^ window-scoped set-option; see 'resolveForWindow'
    }

data Pane = Pane
    { id       :: PaneId
    , pty      :: Hat.Term.Pty.PtyHandle
    , emulator :: Emu.Emulator
    , size     :: TVar Size
    , dead     :: TVar Bool
    , startCwd :: FilePath
    , mode     :: TVar (Maybe CopyModeState)
        -- ^ 'Nothing' = normal shell input; 'Just' = copy mode holding
        -- its own cursor/selection over the pane's scrollback + screen.
    , pipe     :: TVar (Maybe PipeHandle)
        -- ^ an active @pipe-pane@ subprocess, if any.
    , readerTid :: TVar (Maybe ThreadId)
        -- ^ the pane's output-reader thread, stored by the thread itself;
        -- 'Nothing' only in the spawn race before it runs. See 'hangupPane'.
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
    , lastQuery     :: !(Maybe (Text, SearchDirection))
        -- ^ the most recent string search (@/@ @?@): query + direction, for
        -- @n@ (repeat) and @N@ (reverse).
    }
    deriving (Eq, Show)

-- | Selection granularity, matching tmux's @SEL_CHAR@ / @SEL_WORD@ /
-- @SEL_LINE@ (plus rectangle).
data SelKind = SelChar | SelWord | SelLine | SelRect
    deriving (Eq, Show)

-- | The direction a search runs: @/@ and @f@\/@t@ go 'Forward' (toward the
-- end of the line / bottom of the grid); @?@ and @F@\/@T@ go 'Backward'.
data SearchDirection = Forward | Backward
    deriving (Eq, Show)

-- | Reverse a search direction, for @;@\/@,@ and @n@\/@N@ repeats.
flipDirection :: SearchDirection -> SearchDirection
flipDirection Forward  = Backward
flipDirection Backward = Forward

-- | Where a vi @f@\/@F@\/@t@\/@T@ search lands: 'OnTarget' is @f@\/@F@;
-- 'ShortOfTarget' is @t@\/@T@ ('till'), one cell short of the target.
data CharStop = OnTarget | ShortOfTarget
    deriving (Eq, Show)

-- | A vi character search on the current line: 'direction' selects
-- @f@\/@t@ (forward) from @F@\/@T@ (backward); 'stop' selects @t@\/@T@
-- (short) from @f@\/@F@ (on target).
data CharSearch = CharSearch
    { direction :: !SearchDirection
    , stop      :: !CharStop
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
-- the command to run when selected. Opens in 'Browsing' mode; @/@ switches
-- to 'Searching', where keys type into 'query' instead of navigating.
data PickerState = PickerState
    { title     :: !Text
    , roots     :: ![PickerNode]
    , cursor    :: !Int    -- ^ index into the /visible/ (flattened) rows
    , query     :: !Text
    , search    :: !Text  -- ^ committed search term; see 'Hat.Server.Picker.editPicker'
    , mode      :: !PickerMode
    , fill      :: !PickerFill
    }
    deriving (Eq, Show)

-- | Whether the picker is navigating its menu or typing a search query.
data PickerMode
    = Browsing   -- ^ keys navigate and expand\/collapse the tree
    | Searching  -- ^ keys type into 'query'
    deriving (Eq, Show)

-- | @-Z@: how much of the screen the overlay paints over.
data PickerFill
    = FillWindow  -- ^ the whole content area, not just the active pane
    | PaneRegion  -- ^ only the active pane's rectangle
    deriving (Eq, Show)

-- | What the preview column shows for a node: a single pane's contents, a
-- whole window composited in its split layout, or a session as a stack of
-- its windows' thumbnails.
data PreviewTarget
    = PreviewPane !PaneId
    | PreviewWindow !WindowId
    | PreviewSession !SessionId
    deriving (Eq, Show)

-- | One node in the chooser tree. Sessions hold window nodes, which hold
-- pane nodes; a childless node (e.g. @choose-window@) is a plain leaf.
data PickerNode = PickerNode
    { label    :: !Text  -- ^ shown, and matched against 'query'
    , command  :: !Text  -- ^ command line run when the node is chosen
    , preview  :: !(Maybe PreviewTarget)  -- ^ what to show beside the list
    , children :: ![PickerNode]
    , expanded :: !Expansion  -- ^ whether 'children' are shown
    }
    deriving (Eq, Show)

-- | Whether a tree node's children are revealed.
data Expansion
    = Expanded
    | Collapsed
    deriving (Eq, Show)

data Client = Client
    { id        :: ClientId
    , sock      :: Socket
    , sendLock  :: MVar ()
    , size      :: TVar Size
    , session   :: TVar SessionId
    , lastSession :: TVar (Maybe SessionId)
    , ready     :: TVar Bool  -- ^ the client's Welcome greeting has been sent.
                             --   See 'send'.
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
    <*> newTVarIO False  -- everAttached
    <*> newTVarIO False  -- served
    <*> newTVarIO False  -- configLoading
    <*> newTVarIO False  -- restoring
    <*> newTVarIO False  -- preserveStore
    <*> newTVarIO Nothing
    <*> newTVarIO defaultOptions
    <*> newTVarIO emptyDelta  -- serverOptions
    <*> newTVarIO emptyDelta  -- globalSessionOptions
    <*> newTVarIO emptyDelta  -- globalWindowOptions
    <*> newTVarIO emptyDelta  -- schemeOptions
    <*> newTVarIO defaultKeymap
    <*> newTVarIO Seq.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO []
    <*> newTVarIO Nothing  -- markedPane
    <*> newTVarIO Nothing  -- lastActiveSession
    <*> pure lg
    <*> pure path
    <*> pure storePath
    <*> newTVarIO Nothing  -- listenFd
    <*> newTVarIO Nothing  -- serverConfig

bumpDirty :: ServerState -> STM ()
bumpDirty st = modifyTVar' st.dirty (+ 1)

-- | Resolve the effective options with no session or window in context: the
-- server-wide chain (global-window, global-session, server, then the color
-- scheme's base layer) folded onto the defaults.
resolveGlobal :: ServerState -> STM Options
resolveGlobal st = do
    gw <- readTVar st.globalWindowOptions
    gs <- readTVar st.globalSessionOptions
    sv <- readTVar st.serverOptions
    sc <- readTVar st.schemeOptions
    pure (resolveOptions [gw, gs, sv, sc])

-- | Resolve the effective options for a session: its own set-option overlay
-- shadows the global chain, so a bare @set@ affects only that session.
resolveForSession :: ServerState -> Session -> STM Options
resolveForSession st sess = do
    s <- readTVar sess.options
    gw <- readTVar st.globalWindowOptions
    gs <- readTVar st.globalSessionOptions
    sv <- readTVar st.serverOptions
    sc <- readTVar st.schemeOptions
    pure (resolveOptions [s, gw, gs, sv, sc])

-- | Resolve the effective options for a window: its window overlay and its
-- session's overlay shadow the global chain (window options and session
-- options occupy disjoint names, so layering both is safe).
resolveForWindow :: ServerState -> Session -> Window -> STM Options
resolveForWindow st sess win = do
    w <- readTVar win.options
    s <- readTVar sess.options
    gw <- readTVar st.globalWindowOptions
    gs <- readTVar st.globalSessionOptions
    sv <- readTVar st.serverOptions
    sc <- readTVar st.schemeOptions
    pure (resolveOptions [w, s, gw, gs, sv, sc])

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

-- | The pane area of the screen: everything except the status line.
windowArea :: Size -> Size
windowArea sz = sz { rows = max 1 (sz.rows - 1) }

-- | Where is a pane's child process now? /proc, with a fallback.
paneCurrentPath :: Pane -> IO FilePath
paneCurrentPath pane = do
    r <- try (PFiles.readSymbolicLink
        ("/proc/" <> show (Hat.Term.Pty.pid pane.pty) <> "/cwd"))
    pure $ case r of
        Left (_ :: IOException) -> pane.startCwd
        Right dir -> dir

-- | Find a pane by its numeric id across every session and window.
findPaneById :: ServerState -> Int -> STM (Maybe Pane)
findPaneById st n = do
    sessions <- readTVar st.sessions
    panes <- fmap concat . forM (Map.elems sessions) $ \sess -> do
        ws <- readTVar sess.windows
        fmap concat . forM (Map.elems ws) $ windowPanes
    pure (List.find (\p -> rawPane p.id == n) panes)

-- | Find a window by its id across every session.
findWindowById :: ServerState -> WindowId -> STM (Maybe Window)
findWindowById st wid = do
    sessions <- readTVar st.sessions
    wins <- fmap concat . forM (Map.elems sessions) $ \sess ->
        Map.elems <$> readTVar sess.windows
    pure (List.find (\w -> w.id == wid) wins)

-- | Find a session by its id.
findSessionById :: ServerState -> SessionId -> STM (Maybe Session)
findSessionById st sid = Map.lookup sid <$> readTVar st.sessions

rawClient :: ClientId -> Int
rawClient (ClientId n) = n

rawPane :: PaneId -> Int
rawPane (PaneId n) = n

rawSession :: SessionId -> Int
rawSession (SessionId n) = n

tshow :: Show a => a -> Text
tshow = T.pack . show
