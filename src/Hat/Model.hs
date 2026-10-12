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
    , OutputSync (..)
    , noOutputSync
    , PaneStart (..)
    , PaneKind (..)
    , PaneExit (..)
    , PipeHandle (..)
    , Client (..)
    , ClientRole (..)
    , StdinSource (..)
    , RowOrigin (..)
    , ChromeCache (..)
    , StatusCache (..)
    , WindowKey (..)
    , EnvImport (..)
    , StartupPhase (..)
    , Shutdown (..)
    , DestroyHold (..)
    , AcceptGate (..)
    , ReadGate (..)
    , CopyModeState (..)
    , copyModeAt
    , Recentre (..)
    , SelAnchor (..)
    , PaneClock (..)
    , clockOnTop
    , paneClockOnTop
    , PromptCursor (..)
    , CursorShape (..)
    , LineNumbers (..)
    , MarkShown (..)
    , FrozenGrid (..)
    , PaneMode (..)
    , SearchDirection (..)
    , SearchKind (..)
    , flipDirection
    , CharStop (..)
    , CharSearch (..)
    , Toast (..)
    , Markup (..)
    , Flash (..)
    , PanesKeyHold (..)
    , DisplayPanesState (..)
    , PromptState (..)
    , PromptKeys (..)
    , PromptMode (..)
    , PickerState (..)
    , Popup (..)
    , PickerNode (..)
    , Expansion (..)
    , PickerMode (..)
    , PickerFill (..)
    , PreviewTarget (..)
    , SelKind (..)
    , newServerState
    , bumpDirty
    , withCommandBatch
    , markActive
    , tickActivity
    , freshId
    , resolveGlobal
    , resolveForSession
    , resolveForWindow
    , resolveForPane
    , paneOptionsNow
    , sessionClients
    , currentWindow
    , activePane
    , windowPanes
    , paneCurrentPath
    , paneCommandName
    , commandName
    , findPaneById
    , findWindowById
    , findSessionById
    , rawClient
    , rawPane
    , rawSession
    , rawWindow
    , tshow
    ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM
import Control.Exception (IOException, bracket_, try)
import Control.Monad (forM, forM_)
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.List qualified as List
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import System.IO (Handle)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Vector qualified as V
import Data.Word (Word16, Word64)
import Network.Socket (Socket)
import System.Posix.Files qualified as PFiles

import Hat.Geometry
import Hat.Log (Logger)
import Hat.Model.Ids
import Hat.Model.PaneKind (PaneKind (..))
import Hat.Model.PasteBuffers (PasteStore, emptyPasteStore)
import Hat.Term.Pty qualified
import Hat.Model.Options
    ( ModeKeys, Options, OptionsDelta, applyDelta
    , defaultOptions, emptyDelta, resolveOptions )
import Hat.Server.ColorScheme (ColorScheme, MonitorRegistry, newMonitorRegistry)
import Hat.Server.ControlTypes (ControlState)
import Hat.Server.Outbound (Outbound)
import Hat.Server.HookTypes (HooksState, newHooksState)
import Hat.Server.Environ (Environ, emptyEnviron)
import Hat.Server.Keys (EscPending, KeyState, Keymap)
import Hat.Server.Layout (Layout, LayoutName, paneOrder)
import Hat.Server.Floating (FloatState)
import Hat.Server.Menu (WindowMenu)
import Hat.Server.Popup (JobState, PopupState)
import Hat.PtrEq (samePtr)
import Hat.Server.Render (Chrome, Frame, RowOrigin (..))
import Hat.Server.Scene (ChromeKind, ChromeLook, Scene, SceneInput)
import Hat.Transport.Wire (Autostart)
import Hat.Term.Cell qualified as Cell
import Hat.Term.Emulator qualified as Emu

data ServerState = ServerState
    { sessions    :: TVar (Map SessionId Session)
    , clients     :: TVar (Map ClientId Client)
    , nextSession :: TVar Int
    , nextWindow  :: TVar Int
    , nextPane    :: TVar Int
    , nextClient  :: TVar Int
    , dirty       :: TVar Int    -- ^ render generation; renderers wait on it
    , reconciled  :: TVar Int    -- ^ dirty generation reconcileLoop has resized
                                --   panes through; see 'awaitReconciled'
    , commandDepth :: TVar Int   -- ^ open user-command batches; see
                                --   'Hat.Server.Resize.awaitReconcileTick'
    , activityClock :: TVar Int  -- ^ monotonic stamp source; see 'markActive'
    , livePanes   :: TVar Int    -- ^ panes whose reader thread is still alive
                                --   (running or reaping its child); see
                                --   'Hat.Server.serverIdle'
    , activeConns :: TVar Int   -- ^ connection handlers currently running; see
                                --   'waitIdle'.
    , served      :: TVar Bool  -- ^ a client connection other than a
                                --   config-spawned one has been accepted.
                                --   See 'waitIdle'.
    , startupPhase :: TVar StartupPhase  -- ^ see 'Hat.Server.startupGate'.
    , acceptGate  :: TVar AcceptGate    -- ^ see 'Hat.Server.Conn.acceptLoop'.
    , readGate    :: TVar ReadGate      -- ^ see 'Hat.Server.Pane.pauseReaders'.
    , readsInFlight :: TVar Int
        -- ^ pty chunks consumed but not yet fed to their emulators; see
        --   'Hat.Server.Pane.pauseReaders'.
    , preserveStore :: TVar Bool
        -- ^ the store holds an explicitly saved final tree that must
        --   survive shutdown. Set by @kill-server@; off across a natural
        --   drain. See 'runServer' (keep vs drop at drain) and
        --   'persistDecision' (the pin against mirror writes).
    , shutdown :: TVar Shutdown  -- ^ see 'Hat.Server.serverIdle'
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
    , resolvedGlobal :: TVar (Maybe ResolvedOpts)   -- ^ see 'memoResolve'
    , globalEnviron :: TVar Environ
        -- ^ server-wide variables (@set-environment -g@, config @NAME=value@
        --   lines); see 'Hat.Server.sessionSpawnEnv' for how they reach panes.
    , keymap      :: TVar Keymap
    , buffers     :: TVar PasteStore
    , shellCache  :: TVar (Map Text (UTCTime, Text))  -- ^ #(cmd) results
    , cmdHistory  :: TVar [Text]  -- ^ command-prompt history, most-recent first
    , searchHistory :: TVar [Text]
        -- ^ search-prompt history, most-recent first: hat has no search
        --   prompt, so it only carries a @history-file@'s search entries
        --   through to the next save. See 'Hat.Server.PromptHistory'.
    , markedPane  :: TVar (Maybe PaneId)  -- ^ the marked pane (@select-pane -m@)
    , lastActiveSession :: TVar (Maybe SessionId)
        -- ^ the session most recently focused by any client; captured into
        --   the snapshot by name. See 'pickAttachSession'.
    , lastSession :: TVar (Maybe SessionId)
        -- ^ the alternate session a switching client left (@switch-client -l@
        --   returns to it), collapsed server-wide so a reattaching client can
        --   adopt it across a reload. See 'Hat.Server.switchClientTo'.
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
    , monitorRegistry :: MonitorRegistry
        -- ^ the live @gsettings monitor@ child, killed before a reload's
        --   execve so it is not orphaned. See 'Hat.Server.cmdRestartServer'
        --   and 'Hat.Server.ColorScheme.reapMonitor'.
    , hooks :: HooksState
        -- ^ hook bindings, monitors, and wait-for channels; see
        --   'Hat.Server.Hooks'.
    }

data Session = Session
    { id       :: SessionId
    , createdAt :: POSIXTime  -- ^ first creation time; see 'Hat.Server.Rebuild.rebuildSession'
    , restores :: Int  -- ^ rebuilds from serialized state; see 'Hat.Server.Rebuild.rebuildSession'
    , name     :: TVar Text
    , windows  :: TVar (Map Int Window)
        -- ^ keyed by window index (sparse); the members of a session group
        --   share one map. See 'Hat.Server.Winlink.groupMembers'.
    , currentIx :: TVar Int
    , windowHist :: TVar [Int]  -- ^ MRU window indices; see 'Hat.Server.Mru'
    , lastSize :: TVar Size              -- ^ the window's pane area; see 'sessionWindowArea'
    , environ  :: TVar Environ           -- ^ env for new panes; refreshed on attach (update-environment)
    , startCwd :: TVar FilePath          -- ^ default working directory for new windows; @attach-session -c@ re-anchors it
    , options  :: TVar OptionsDelta      -- ^ session-scoped set-option; see 'resolveForSession'
    , resolvedOptions :: TVar (Maybe ResolvedOpts)  -- ^ see 'memoResolve'
    , group    :: TVar (Maybe Text)
        -- ^ the name of the session group this session belongs to, if any
    , activity :: TVar Int
        -- ^ when a client last attached to or drove it, on the server's
        --   activity clock (tmux's @activity_time@); see 'markActive'
    , destroyHold :: TVar DestroyHold
        -- ^ see 'Hat.Server.Command.Session.checkUnattached'
    }

-- | Whether @destroy-unattached@ must spare a session no client has
-- attached to since it was rebuilt from serialized state.
data DestroyHold = HeldUntilAttached | NotHeld
    deriving (Eq, Show)

data Window = Window
    { id         :: WindowId
    , name       :: TVar Text
    , layout     :: TVar Layout
    , layoutName :: TVar (Maybe LayoutName)
        -- ^ the last named layout applied, if any; see 'cmdNextLayout'.
    , panes      :: TVar (Map PaneId Pane)
    , activeId   :: TVar PaneId
    , paneHist   :: TVar [PaneId]  -- ^ MRU pane ids; see 'Hat.Server.Mru'
    , bellFlag   :: TVar Bool
    , activity   :: TVar Bool  -- ^ output since last viewed (monitor-activity)
    , silenceFlag :: TVar Bool  -- ^ quiet past monitor-silence; see 'Hat.Server.HookMonitor.silenceSweep'
    , activityAt :: TVar POSIXTime  -- ^ when output last arrived (the silence timer's anchor)
    , zoomed     :: TVar (Maybe PaneId)
    , autoRename :: TVar Bool
        -- ^ when set, the name tracks the active pane's foreground command
        -- (@automatic-rename@); an explicit @rename-window@ clears it.
    , options    :: TVar OptionsDelta  -- ^ window-scoped set-option; see 'resolveForWindow'
    , arrangeCache :: TVar (Maybe ((Rect, Layout), ([(PaneId, Rect)], [(Pos, Char)])))
        -- ^ the last layout arrangement, keyed by its inputs; see
        -- 'Hat.Server.Resize.windowArrange'
    , floats     :: TVar FloatState
        -- ^ the floating panes' stacking and the modal pane; their cells
        -- live in 'layout'
    , menu       :: TVar (Maybe WindowMenu)  -- ^ the menu shown over the window
    , menuLast   :: TVar (Int, Int)
        -- ^ where the window's last menu was placed (@-x L@\/@-y L@)
    }

data Pane = Pane
    { id       :: PaneId
    , pty      :: Hat.Term.Pty.PtyHandle
    , emulator :: Emu.Emulator
    , size     :: TVar Size
    , dead     :: TVar Bool
    , startCwd :: FilePath
    , options  :: TVar OptionsDelta  -- ^ pane-scoped set-option; see 'resolveForPane'
    , mode     :: TVar (Maybe PaneMode)
        -- ^ 'Nothing' = normal shell input; 'Just' = copy mode holding
        -- its own cursor/selection over a frozen snapshot of the pane's
        -- scrollback + screen.
    , pipe     :: TVar (Maybe PipeHandle)
        -- ^ an active @pipe-pane@ subprocess, if any.
    , readerTid :: TVar (Maybe ThreadId)
        -- ^ the pane's output-reader thread, stored by the thread itself;
        -- 'Nothing' only in the spawn race before it runs. See 'hangupPane'.
    , home     :: TVar (Maybe (SessionId, Window))
        -- ^ the window currently holding the pane, and the session that
        -- window lives in; see Note [Pane home]
    , pendingInput :: Maybe Text
        -- ^ a one-shot command line the reader types into the pane once its
        -- shell first prints; see 'startPaneReader'. 'Nothing' for a plain pane.
    , start    :: PaneStart
        -- ^ what the pane was launched with; see 'Hat.Server.Pane.respawnStart'.
    , exited   :: TVar (Maybe PaneExit)
        -- ^ 'Just' for a dead pane: its program ended and @remain-on-exit@
        -- kept it; see 'Hat.Server.Pane.paneEof'.
    , kind     :: PaneKind
    , inputOff :: TVar Bool
        -- ^ @select-pane -d@: keys typed or sent to the pane are dropped.
    , onAlternate :: TVar Bool
        -- ^ whether the pane's program is on the alternate screen, as of
        -- the last output its reader fed
    , outputSync :: TVar OutputSync
        -- ^ requests that the reader catch up on pending output; see
        -- 'Hat.Server.Pane.syncPanes'.
    , clock :: TVar PaneClock
    }

-- | Generations of a pane's output-sync requests.
data OutputSync = OutputSync
    { requested :: !Word64  -- ^ the latest request posted
    , answered  :: !Word64  -- ^ the latest request the reader has served
    }

noOutputSync :: OutputSync
noOutputSync = OutputSync { requested = 0, answered = 0 }

-- | A pane's clock mode, and whether it was entered over its copy mode or
-- under it (tmux keeps a pane's modes as a stack, the top one shown).
data PaneClock = NoClock | ClockOnTop | ClockUnderCopy
    deriving (Eq, Show)

-- | Whether clock mode is the pane's top mode: the one shown, and given
-- the keys.
clockOnTop :: PaneClock -> Maybe PaneMode -> Bool
clockOnTop c copy = case c of
    ClockOnTop -> True
    ClockUnderCopy -> isNothing copy
    NoClock -> False

-- | 'clockOnTop' for a pane now.
paneClockOnTop :: Pane -> STM Bool
paneClockOnTop pane = clockOnTop <$> readTVar pane.clock <*> readTVar pane.mode

-- | How a freshly-spawned pane chooses its process.
data PaneStart
    = FreshShell         -- ^ the session's login shell, no command
    | ShellCommand Text  -- ^ a user-supplied command line, run via @sh -c@
                         --   (@new-window@\/@split-window@ with an argument)
    | ExecArgv [Text]    -- ^ exec this argv directly, no shell. See
                         --   'Hat.Server.Pane.restoreRun'.
    | ShellInject [Text] -- ^ spawn a fresh interactive shell and type this
                         --   argv into it as a command line. See
                         --   'Hat.Server.Pane.restoreRun'.
    deriving (Eq, Show)

-- | How a dead pane's program ended: its exit status or terminating signal
-- (neither when the reap itself failed), and when.
data PaneExit = PaneExit
    { status :: Maybe Int
    , signal :: Maybe Int
    , diedAt :: POSIXTime
    }
    deriving (Eq, Show)

-- Note [Pane home]
-- ~~~~~~~~~~~~~~~~
-- The reader thread routes a pane's output — repaints, bells, desktop
-- notifications — by the window holding the pane NOW, but scanning the tree
-- ('locatePane') on every output chunk is too costly, so the location is
-- cached in 'home'. 'startPaneReader' seeds it ('Nothing' only in the spawn
-- race before that), and every transaction that re-parents the pane
-- (break-pane, join-pane) or moves its window to another session
-- (move-window; unlink-window and kill-session when the window survives
-- linked elsewhere) must rewrite it. Teardown never trusts the cache:
-- 'detachPaneCurrent' scans the tree.

-- | A live @pipe-pane@. Pane output is forwarded to 'toStdin' (@-O@) on the
-- hot path; 'super' owns the subprocess, its handles, and the stdout-pump
-- thread (@-I@) as one scoped sub-resource that a cancel releases in full.
data PipeHandle = PipeHandle
    { super   :: Async ()
    , toStdin :: Maybe Handle
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
    , lastQuery     :: !(Maybe (Text, SearchDirection, SearchKind))
        -- ^ the most recent string search (@/@ @?@): query + direction +
        -- kind, for @n@ (repeat) and @N@ (reverse).
    , scrollExit    :: !Bool
        -- ^ @copy-mode -e@: scrolling down to the bottom leaves the mode
    , hidePosition  :: !Bool
        -- ^ @copy-mode -H@: no position indicator
    , pinnedEnd     :: !(Maybe (Int, Int))
        -- ^ the selection's far end once it stops following the cursor
        -- (tmux's @CURSORDRAG_NONE@); 'Nothing' while the cursor is that end
    , anchorAt      :: !SelAnchor
    , recentre      :: !(Maybe (Int, Recentre))
        -- ^ the row @recentre-top-bottom@ last moved and where it goes next
    , lineNumbers   :: !LineNumbers
    , mark          :: !(Int, Int)
        -- ^ the @set-mark@ position, absolute; the entry cursor until set
    , markShown     :: !MarkShown
    , searchMarks   :: !(Maybe SearchDirection)
        -- ^ while the last search's matches are highlighted, the way it ran
    }
    deriving (Eq, Show)

-- | A fresh copy mode over the given key table, its cursor at an absolute
-- @(row, col)@ and nothing selected.
copyModeAt :: Text -> Int -> Int -> CopyModeState
copyModeAt table row col = CopyModeState
    { cursorRow = row
    , cursorCol = col
    , selection = Nothing
    , keyTable = table
    , viewportOffY = 0
    , numPrefix = Nothing
    , pendingSearch = Nothing
    , lastSearch = Nothing
    , lastQuery = Nothing
    , scrollExit = False
    , hidePosition = False
    , pinnedEnd = Nothing
    , anchorAt = AnchorAtStart
    , recentre = Nothing
    , lineNumbers = LinesFromOption
    , mark = (row, col)
    , markShown = MarkHidden
    , searchMarks = Nothing
    }

-- | Whether copy mode numbers its lines (tmux's @line_numbers@): not at
-- all, as @copy-mode-line-numbers@ says, or as it says but at least
-- @default@.
data LineNumbers = LinesHidden | LinesFromOption | LinesForced
    deriving (Eq, Show)

-- | Whether copy mode draws its mark in @copy-mode-mark-style@.
data MarkShown = MarkHidden | MarkShown
    deriving (Eq, Show)

-- | Which of tmux's two selection ends (@selx@ where it began, @endselx@)
-- the anchor is; @other-end@ swaps them.
data SelAnchor = AnchorAtStart | AnchorAtEnd
    deriving (Eq, Show)

-- | Where @recentre-top-bottom@ puts the cursor's line next.
data Recentre = RecentreMiddle | RecentreTop | RecentreBottom
    deriving (Eq, Show)

-- | A pane's active copy mode: a frozen snapshot of the pane's grid
-- captured on entry, plus the navigation state ('CopyModeState') that
-- browses it. The snapshot never changes while the mode is open, so
-- program output produced during copy mode stays invisible until exit.
data PaneMode = PaneMode
    { frozen    :: !FrozenGrid
    , copyState :: !CopyModeState
    }
    deriving (Eq, Show)

-- | An immutable capture of a pane's absolute grid (oldest scrollback
-- line first, then the live screen) taken when copy mode is entered.
-- 'fgRows' holds every absolute row; 'fgHsize' is the scrollback line
-- count, so rows @fgHsize .. fgHsize+fgSy-1@ are the screen.
data FrozenGrid = FrozenGrid
    { fgHsize :: !Int
    , fgSy    :: !Int
    , fgSx    :: !Int
    , fgRows  :: !(V.Vector (V.Vector Cell.Cell))
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

-- | How a string search interprets its pattern: as a POSIX extended
-- regex ('SearchRegex', the @search-forward@\/@search-backward@ default)
-- or verbatim ('SearchText', the @-text@ command variants).
data SearchKind = SearchRegex | SearchText
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

-- | A display-message overlay.
data Toast = Toast
    { text     :: !Text
    , markup   :: !Markup
    , deadline :: !(Maybe Word64)  -- ^ monotonic ns, from 'toastDeadline';
                                   --   'Nothing' = until a key is pressed
    }

-- | Whether a message's @#[…]@ directives style it (@display-message@) or
-- show as text (errors and other server messages).
data Markup = MarkupStyled | MarkupLiteral
    deriving (Eq, Show)

-- | Whether a non-selecting key closes the @display-panes@ overlay
-- (default) or leaves it showing (@-N@); see
-- 'Hat.Server.DisplayPanes.panesKeyAction'.
data PanesKeyHold = PanesClose | PanesHold
    deriving (Eq, Show)

-- | The armed @display-panes@ overlay on one client; see
-- 'Hat.Server.DisplayPanes'.
data DisplayPanesState = DisplayPanesState
    { deadline :: !(Maybe Word64)
        -- ^ monotonic ns, from 'Hat.Server.DisplayPanes.panesDeadline';
        --   'Nothing' = until a key
    , hold     :: !PanesKeyHold
    , template :: !Text  -- ^ run on select, @%%@ = the chosen pane's id
    }
    deriving (Eq, Show)

-- | The active-pane highlight shown while the prefix is armed; see
-- 'Hat.Server.Flash.armFlash'.
data Flash
    = FlashArmed               -- ^ held while the prefix is armed
    | FlashLingerPending       -- ^ after a pane move, until the move's frame is drawn
    | FlashLinger !Word64      -- ^ fading after the move's frame; monotonic-ns deadline
    deriving (Eq)

-- | How a prompt reads keys: @status-keys@, and the @word-separators@ its
-- vi word motions stop at.
data PromptKeys = PromptKeys
    { statusKeys :: !ModeKeys
    , wordSeparators :: !Text
    , insertCursor :: !PromptCursor   -- ^ @prompt-cursor-colour@ and @-style@
    , commandCursor :: !PromptCursor  -- ^ @prompt-command-cursor-*@, vi command mode
    }
    deriving (Eq, Show)

-- | How the terminal's cursor looks while it sits in a prompt.
data PromptCursor = PromptCursor
    { colour :: !Text  -- ^ a colour option's value; empty for the terminal's own
    , shape :: !CursorShape
    }
    deriving (Eq, Show)

-- | A cursor shape as DECSCUSR sets it: @cursor-style@'s choices, in its
-- order, which is DECSCUSR's numbering.
data CursorShape
    = CursorDefault
    | CursorBlinkingBlock
    | CursorBlock
    | CursorBlinkingUnderline
    | CursorUnderline
    | CursorBlinkingBar
    | CursorBar
    deriving (Eq, Show, Enum, Bounded)

-- | A vi-keys prompt's mode (tmux's @PROMPT_COMMANDMODE@); an emacs-keys
-- prompt is always inserting.
data PromptMode = PromptInsert | PromptCommand
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
    , promptKeys :: !PromptKeys  -- ^ fixed when the prompt opens
    , promptMode :: !PromptMode
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

-- | Whether a connected client renders a session or is a bare control
-- connection (@hat \<command\>@ from a shell). See 'Hat.Server.restartClientAction'.
data ClientRole = Attached | Control
    deriving (Eq, Show)

-- | Whether an attach folds the client's environment into the session's
-- via @update-environment@, or leaves it untouched (@attach-session -E@).
-- See 'Hat.Server.refreshSessionEnv'.
data EnvImport = ImportEnv | SkipEnvImport
    deriving (Eq, Show)

-- | Where server startup stands: sourcing the config, restoring or
-- re-adopting a session tree, or serving normally. See
-- 'Hat.Server.startupGate' for what each phase admits.
data StartupPhase = LoadingConfig | Restoring | Ready
    deriving (Eq, Show)

-- | Whether the server was asked to end (@kill-server@), whatever its
-- @exit-empty@ says.
data Shutdown = Serving | ShuttingDown
    deriving (Eq, Show)

-- | Whether the accept loop takes new connections. A reload closes the gate
-- ('AcceptClosing'), the loop confirms ('AcceptParked'), and arrivals wait in
-- the listen backlog; 'AcceptOpen' resumes. See 'Hat.Server.Conn.acceptLoop'.
data AcceptGate
    = AcceptOpen
    | AcceptClosing
    | AcceptParked
    | AcceptRetiring  -- ^ the server no longer owns the socket path
    deriving (Eq, Show)

-- | Whether pane readers may pump pty bytes into their emulators;
-- 'ReadersPaused' parks them all. See 'Hat.Server.Pane.pauseReaders'.
data ReadGate = ReadersFlowing | ReadersPaused
    deriving (Eq, Show)

-- | A client's window scene and its drawn chrome, with the inputs that
-- produced them; the render thread's cache. See
-- 'Hat.Server.View.cachedChrome'.
data ChromeCache = ChromeCache
    { input   :: SceneInput
    , scene   :: Scene
    , kinds   :: Set ChromeKind  -- ^ 'Hat.Server.Scene.sceneKinds' of 'scene'
    , look    :: ChromeLook
    , rowOff  :: Int
    , cells   :: [(Pos, Cell.Cell)]
    , chrome  :: Chrome
    }

-- | A client's rendered status rows with everything their expansion
-- consumed; the render thread's cache. See
-- 'Hat.Server.View.statusRowsCached'.
-- Fields are strict so the identity-matched ones hold the very objects
-- they were built from, never a selector thunk 'samePtr' would miss on.
-- | What a window's status entry usually changes with, cheap to read each
-- frame: the status cache's per-window key. See
-- 'Hat.Server.View.statusRowsCached'.
data WindowKey = WindowKey
    { index   :: !Int
    , name    :: !Text
    , flags   :: !Text  -- ^ @#{window_flags}@
    , viewers :: !Int   -- ^ @#{window_active_clients}@
    , options :: !OptionsDelta
    }
    deriving (Eq)

data StatusCache = StatusCache
    { opts    :: !Options                    -- ^ matched by object identity
    , shells  :: !(Map Text (UTCTime, Text)) -- ^ matched by object identity
    , second  :: !Int                        -- ^ POSIX second of the render
    , width   :: !Int
    , env     :: !(Map Text Text)
    , wins    :: ![WindowKey]
    , plain   :: !Bool  -- ^ no format reads the tree beyond @wins@, or animates; see 'Hat.Server.Format.formatReadsLoops'
    , animated :: !Bool  -- ^ a format animates (@A:@), so the rows change with time alone
    , rowCells :: ![V.Vector Cell.Cell]
    }

data Client = Client
    { id        :: ClientId
    , role      :: ClientRole
    , autostart :: Autostart  -- ^ from the hello; see 'Hat.Server.startupGate'
    , sock      :: Socket
    , wireLevel :: Word16  -- ^ negotiated dialect; see 'Hat.Transport.Wire.negotiate'
    , outbound  :: Outbound  -- ^ see "Hat.Server.ClientIO"
    , size      :: TVar Size
    , lastActive :: TVar Int  -- ^ activity stamp; see 'markActive'
    , session   :: TVar SessionId
    , sessionHist :: TVar [SessionId]  -- ^ MRU sessions (@switch-client -l@); see 'Hat.Server.Mru'
    , ready     :: TVar Bool  -- ^ the client's Welcome greeting has been sent.
                             --   See 'send'.
    , keyState  :: IORef KeyState  -- input thread only
    , escState  :: IORef EscPending   -- ^ held trailing ESC; input thread only
    , lastFrame :: IORef Frame        -- render thread only
    , lastOrigins :: IORef (V.Vector RowOrigin)
        -- ^ what produced each row of 'lastFrame'; render thread only
    , lastChrome :: IORef (Maybe ChromeCache)  -- render thread only
    , lastStatus :: IORef (Maybe StatusCache)  -- render thread only
    , lastCursor :: IORef (Pos, Bool)
    , lastCursorColour :: IORef (Maybe Emu.Rgb)
        -- ^ OSC 12 in effect; render thread only
    , lastCursorShape :: IORef CursorShape
        -- ^ DECSCUSR in effect; render thread only
    , lastMouseMode :: IORef Int
        -- ^ the outer terminal's mouse tracking: 0 off, 1 clicks, 2 drags,
        --   3 all motion; render thread only
    , needsFull :: TVar Bool
    , toast     :: TVar (Maybe Toast)  -- ^ display-message overlay
    , flash     :: TVar (Maybe Flash)  -- ^ prefix flash of the active pane
    , prompt    :: TVar (Maybe PromptState)  -- ^ command-prompt line editor
    , displayPanes :: TVar (Maybe DisplayPanesState)  -- ^ pane-number overlay
    , picker    :: TVar (Maybe PickerState)  -- ^ choose-tree/window overlay
    , popup     :: TVar (Maybe Popup)  -- ^ display-popup overlay
    , outerFocused :: TVar Bool
        -- ^ whether the client's outer terminal has OS focus (?1004); see
        -- 'noteOuterFocus' and 'attentionSeen'.
    , envImport :: TVar EnvImport  -- ^ set by @attach-session -E@
    , env       :: [(Text, Text)]
    , cwd       :: Text
    , stdinSource :: StdinSource
    , control   :: Maybe ControlState
        -- ^ a tmux control-mode client's protocol state; 'Nothing' for
        --   every other client. See "Hat.Server.Control".
    }

-- | A client's popup: its box, and the command running on a pty of its
-- own. See "Hat.Server.PopupIO".
data Popup = Popup
    { state :: TVar PopupState
    , jobPty :: Hat.Term.Pty.PtyHandle  -- ^ the command's pty
    , screen :: Emu.Emulator  -- ^ what the command has drawn
    , job :: TVar JobState
    , status :: TVar Int
        -- ^ what the popup reports when it closes: the command's exit
        --   status or signal, 128 + SIGHUP while it still runs
    , reader :: TVar (Maybe ThreadId)
    , closed :: TVar Bool
    }

-- | Where a command's @-@ path reads from: the issuing command client's
-- standard input while its command batch runs, else nowhere.
data StdinSource = NoStdin | StdinFrom (IO (Either Text ByteString))

newServerState :: Keymap -> Logger -> FilePath -> Maybe FilePath -> IO ServerState
newServerState defaultKeymap lg path storePath = ServerState
    <$> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0      -- dirty
    <*> newTVarIO 0      -- reconciled
    <*> newTVarIO 0      -- commandDepth
    <*> newTVarIO 0      -- activityClock
    <*> newTVarIO 0      -- livePanes
    <*> newTVarIO 0      -- activeConns
    <*> newTVarIO False  -- served
    -- Ready, not LoadingConfig: a bare state (tests) is idle. 'runServerWith'
    -- arms LoadingConfig before its accept loop can serve anyone.
    <*> newTVarIO Ready  -- startupPhase
    <*> newTVarIO AcceptOpen
    <*> newTVarIO ReadersFlowing
    <*> newTVarIO 0      -- readsInFlight
    <*> newTVarIO False  -- preserveStore
    <*> newTVarIO Serving
    <*> newTVarIO Nothing
    <*> newTVarIO defaultOptions
    <*> newTVarIO emptyDelta  -- serverOptions
    <*> newTVarIO emptyDelta  -- globalSessionOptions
    <*> newTVarIO emptyDelta  -- globalWindowOptions
    <*> newTVarIO emptyDelta  -- schemeOptions
    <*> newTVarIO Nothing     -- resolvedGlobal
    <*> newTVarIO emptyEnviron
    <*> newTVarIO defaultKeymap
    <*> newTVarIO emptyPasteStore
    <*> newTVarIO Map.empty
    <*> newTVarIO []
    <*> newTVarIO []       -- searchHistory
    <*> newTVarIO Nothing  -- markedPane
    <*> newTVarIO Nothing  -- lastActiveSession
    <*> newTVarIO Nothing  -- lastSession
    <*> pure lg
    <*> pure path
    <*> pure storePath
    <*> newTVarIO Nothing  -- listenFd
    <*> newTVarIO Nothing  -- serverConfig
    <*> newMonitorRegistry
    <*> newHooksState

bumpDirty :: ServerState -> STM ()
bumpDirty st = modifyTVar' st.dirty (+ 1)

-- | Run a user action's whole command sequence as one batch: 'reconcileLoop'
-- holds off sizing panes until every command has committed (see
-- 'awaitReconcileTick'). Nestable — an @if-shell@ that runs more commands just
-- deepens the count; only the outermost exit reopens reconciliation.
withCommandBatch :: ServerState -> IO a -> IO a
withCommandBatch st = bracket_
    (atomically (modifyTVar' st.commandDepth (+ 1)))
    (atomically (modifyTVar' st.commandDepth (subtract 1)))

-- | Stamp a client as the most-recently-active on its session: advance the
-- server's activity clock and record the new value on the client and its
-- session, so 'effectiveWindowSize' under @window-size latest@ sizes a
-- window to whichever client most recently drove it.
markActive :: ServerState -> Client -> STM ()
markActive st client = do
    n <- tickActivity st
    writeTVar client.lastActive n
    sid <- readTVar client.session
    msess <- Map.lookup sid <$> readTVar st.sessions
    forM_ msess $ \sess -> do
        writeTVar sess.activity n
        writeTVar sess.destroyHold NotHeld

-- | Advance the server's activity clock, yielding the new stamp.
tickActivity :: ServerState -> STM Int
tickActivity st = do
    n <- (+ 1) <$> readTVar st.activityClock
    writeTVar st.activityClock n
    pure n

-- | Resolve the effective options with no session or window in context: the
-- server-wide chain (global-window, global-session, server, then the color
-- scheme's base layer) folded onto the defaults.
resolveGlobal :: ServerState -> STM Options
resolveGlobal st = do
    gw <- readTVar st.globalWindowOptions
    gs <- readTVar st.globalSessionOptions
    sv <- readTVar st.serverOptions
    sc <- readTVar st.schemeOptions
    memoResolve st.resolvedGlobal [gw, gs, sv, sc]

-- | An options resolution held with the delta objects it was computed
-- from; see 'memoResolve'.
data ResolvedOpts = ResolvedOpts
    { deltas :: [OptionsDelta]
    , opts   :: Options
    }

-- | @resolveOptions deltas@ through a memo: served from the cache when
-- every delta is the very object of the previous resolution, so any
-- set-option that writes a scope's TVar naturally misses.
memoResolve :: TVar (Maybe ResolvedOpts) -> [OptionsDelta] -> STM Options
memoResolve cache deltas = do
    hit <- readTVar cache
    case hit of
        Just r | sameDeltas r.deltas deltas -> pure r.opts
        _ -> do
            let o = resolveOptions deltas
            writeTVar cache (Just ResolvedOpts { deltas, opts = o })
            pure o
  where
    sameDeltas (a : as) (b : bs) = samePtr a b && sameDeltas as bs
    sameDeltas as bs = null as && null bs

-- | Resolve the effective options for a session: its own set-option overlay
-- shadows the global chain, so a bare @set@ affects only that session.
resolveForSession :: ServerState -> Session -> STM Options
resolveForSession st sess = do
    s <- readTVar sess.options
    gw <- readTVar st.globalWindowOptions
    gs <- readTVar st.globalSessionOptions
    sv <- readTVar st.serverOptions
    sc <- readTVar st.schemeOptions
    memoResolve sess.resolvedOptions [s, gw, gs, sv, sc]

-- | Resolve the effective options for a window: its window overlay and its
-- session's overlay shadow the global chain (window options and session
-- options occupy disjoint names, so layering both is safe).
resolveForWindow :: ServerState -> Session -> Window -> STM Options
resolveForWindow st sess win = do
    w <- readTVar win.options
    applyDelta w <$> resolveForSession st sess

-- | Resolve the effective options for one pane: its own overlay (@set -p@)
-- shadows its window's chain.
resolveForPane :: ServerState -> Session -> Window -> Pane -> STM Options
resolveForPane st sess win pane = do
    p <- readTVar pane.options
    rest <- resolveForWindow st sess win
    pure (applyDelta p rest)

-- | A pane's options as of now, from its current home; 'Nothing' for a pane
-- not yet homed.
paneOptionsNow :: ServerState -> Pane -> IO (Maybe Options)
paneOptionsNow st pane = atomically $ do
    mhome <- readTVar pane.home  -- See Note [Pane home]
    sessions <- readTVar st.sessions
    case mhome of
        Just (sid, win) | Just sess <- Map.lookup sid sessions ->
            Just <$> resolveForPane st sess win pane
        _ -> pure Nothing

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

-- | A window's panes in pane-index order ('paneOrder').
windowPanes :: Window -> STM [Pane]
windowPanes win = do
    ps <- readTVar win.panes
    lay <- readTVar win.layout
    pure (mapMaybe (`Map.lookup` ps) (paneOrder lay ps))

-- | Where is a pane's child process now? /proc, with a fallback.
paneCurrentPath :: Pane -> IO FilePath
paneCurrentPath pane = do
    closed <- Hat.Term.Pty.isClosed pane.pty
    if closed then pure pane.startCwd else do
        r <- try (PFiles.readSymbolicLink
            ("/proc/" <> show (Hat.Term.Pty.pid pane.pty) <> "/cwd"))
        pure $ case r of
            Left (_ :: IOException) -> pane.startCwd
            Right dir -> dir

-- | The program a captured foreground command names: its last path
-- segment with NixOS's @.<name>-wrapped@ decoration stripped, so a pane
-- running @\/nix\/store\/…\/.vim-wrapped@ is recorded (and matched) as
-- @vim@.
commandName :: Text -> Text
commandName raw =
    let base  = T.takeWhileEnd (/= '/') (firstWord raw)
        undot = T.dropWhile (== '.') base
    in fromMaybe undot (T.stripSuffix "-wrapped" undot)
  where
    firstWord t = case T.words t of
        (w : _) -> w
        []      -> ""

-- | A pane's foreground program (from @/proc@) as a display name:
-- normalized by 'commandName', so a NixOS-wrapped @vim@ shows as @vim@
-- everywhere (window titles, @#{pane_current_command}@, the persisted
-- tree). Falls back to @sh@. A dead pane's pid may already belong to some
-- other process, so it names what the pane was started with instead.
paneCommandName :: Pane -> IO Text
paneCommandName pane = do
    closed <- Hat.Term.Pty.isClosed pane.pty
    mfg <- if closed then pure (Just (startedCommand pane.start))
        else Hat.Term.Pty.foregroundCommand pane.pty
    raw <- case mfg of
        Just cmd -> pure cmd
        Nothing -> do
            r <- try (TIO.readFile ("/proc/" <> show (Hat.Term.Pty.pid pane.pty) <> "/comm"))
            pure $ case r of
                Right s -> let t = T.strip s in if T.null t then "sh" else t
                Left (_ :: IOException) -> "sh"
    pure (commandName raw)

-- | The command line a pane was launched with, for naming a dead pane.
startedCommand :: PaneStart -> Text
startedCommand = \case
    ShellCommand t -> t
    ExecArgv argv@(_ : _) -> T.unwords argv
    _ -> "sh"

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

rawWindow :: WindowId -> Int
rawWindow (WindowId n) = n

tshow :: Show a => a -> Text
tshow = T.pack . show
