-- | The desktop's light\/dark preference, as published by GNOME's
-- @org.gnome.desktop.interface color-scheme@ setting. The server tails
-- @gsettings monitor@ for changes (see 'Hat.Server.watchColorScheme'),
-- restyles hat's own chrome to a matching default palette, and
-- reapplies the user's per-scheme config.
module Hat.Server.ColorScheme
    ( ColorScheme (..)
    , parseSchemeLine
    , schemeName
    , applyPalette
    , WatcherFault (..)
    , watcherFault
    , MonitorRegistry
    , monitorHandle
    , newMonitorRegistry
    , registerMonitor
    , withRegisteredMonitor
    , reapMonitor
    ) where

import Control.Concurrent.STM
    (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception
    (SomeException, SomeAsyncException, finally, fromException)
import GHC.IO.Exception (IOException)
import System.IO.Error (isDoesNotExistError)
import System.Process (ProcessHandle, terminateProcess, waitForProcess)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import System.Exit (ExitCode)

import Hat.Model.Options
    ( OptionName (..), OptionValue (..), OptionsDelta
    , emptyDelta, insertDelta )
import qualified Hat.Term.Cell as Cell

data ColorScheme = SchemeLight | SchemeDark
    deriving (Eq, Show)

-- | One line of @gsettings get@ (@'prefer-dark'@) or @gsettings monitor@
-- (@color-scheme: 'prefer-dark'@) output. GNOME's only schemes are
-- @prefer-dark@, @prefer-light@ and @default@; @default@ means the
-- desktop has no preference, which reads as light.
parseSchemeLine :: Text -> Maybe ColorScheme
parseSchemeLine line = case T.words line of
    [] -> Nothing
    ws | keyed && not aboutScheme -> Nothing
       | otherwise -> case T.dropAround (`elem` ("'\"" :: String)) (last ws) of
            "prefer-dark"  -> Just SchemeDark
            "prefer-light" -> Just SchemeLight
            "default"      -> Just SchemeLight
            _              -> Nothing
      where
        keyed = length ws > 1
        aboutScheme = "color-scheme:" `elem` init ws

-- | What to do when the color-scheme watcher's inner loop ends by throwing.
-- See 'watcherFault'.
data WatcherFault
    = RestartWatcher  -- ^ a transient sync failure — respawn the monitor
    | PropagateFault  -- ^ async cancellation or 'ExitCode' — re-raise, never restart
    | AbandonWatcher  -- ^ the @gsettings@ binary is absent — give up (logged), never restart
    deriving (Eq, Show)

-- | Decide how a watcher death is handled. A missing @gsettings@ binary (a
-- host without it, e.g. nix-on-droid) surfaces as a does-not-exist
-- 'IOException' on the spawn: retrying can never succeed, so abandon the
-- watcher (the caller logs it) rather than back off forever. Any other
-- synchronous failure (the @gsettings monitor@ subprocess crashing, EOF on
-- its pipe, a transient @IOException@) is transient: respawn so
-- theme-following is durable. An asynchronous exception — the
-- 'Control.Concurrent.Async.cancel' that 'Hat.Server.withDaemons' uses to
-- tear the daemon down at shutdown — must be re-raised, never swallowed and
-- never restarted, or teardown would spin forever (the broad-catch-in-a-loop
-- trap). 'ExitCode' likewise propagates: swallowing it would keep the process
-- from exiting.
watcherFault :: SomeException -> WatcherFault
watcherFault e
    | isJustAsync = PropagateFault
    | isExit      = PropagateFault
    | isMissing   = AbandonWatcher
    | otherwise   = RestartWatcher
  where
    isJustAsync = case fromException e :: Maybe SomeAsyncException of
        Just _  -> True
        Nothing -> False
    isExit = case fromException e :: Maybe ExitCode of
        Just _  -> True
        Nothing -> False
    isMissing = case fromException e :: Maybe IOException of
        Just ioe -> isDoesNotExistError ioe
        Nothing  -> False

-- | The live @gsettings monitor@ child, if one is currently running. See
-- 'reapMonitor': the server execve's over itself on @restart-server@, so the
-- watcher's own 'withCreateProcess' cleanup never fires and the child would be
-- orphaned; this handle is the pre-exec kill's only reference to it.
newtype MonitorRegistry = MonitorRegistry (TVar (Maybe ProcessHandle))

-- | The registry's underlying cell — the live monitor handle, or 'Nothing'.
monitorHandle :: MonitorRegistry -> TVar (Maybe ProcessHandle)
monitorHandle (MonitorRegistry v) = v

newMonitorRegistry :: IO MonitorRegistry
newMonitorRegistry = MonitorRegistry <$> newTVarIO Nothing

-- | Record the currently-spawned monitor so 'reapMonitor' can find it.
registerMonitor :: MonitorRegistry -> ProcessHandle -> IO ()
registerMonitor reg ph = atomically (writeTVar (monitorHandle reg) (Just ph))

-- | Run @body@ with @ph@ registered as the live monitor, clearing the
-- registration when it ends however it ends. Pairs with the watcher's
-- 'System.Process.withCreateProcess' so the registry never outlives the child
-- on the normal (exception-driven) teardown; only an execve escapes it, which
-- is exactly what 'reapMonitor' covers.
withRegisteredMonitor :: MonitorRegistry -> ProcessHandle -> IO a -> IO a
withRegisteredMonitor reg ph body =
    (registerMonitor reg ph >> body)
        `finally` atomically (writeTVar (monitorHandle reg) Nothing)

-- | Terminate the registered @gsettings monitor@ child and clear the registry.
-- The server calls this immediately before it execve's itself on a reload:
-- the exec replaces the whole image atomically, so no bracket unwinds and the
-- watcher's own cleanup never runs — without this the monitor is orphaned on
-- every @restart-server@ (each reload leaked one). Reaps synchronously
-- ('waitForProcess') so the child is gone, not merely signalled, before the
-- exec. A no-op when no monitor is registered (a host without gsettings).
reapMonitor :: MonitorRegistry -> IO ()
reapMonitor reg = do
    mph <- atomically $ do
        cur <- readTVar (monitorHandle reg)
        writeTVar (monitorHandle reg) Nothing
        pure cur
    case mph of
        Nothing -> pure ()
        Just ph -> terminateProcess ph >> () <$ waitForProcess ph

-- | The value @#{color_scheme}@ expands to.
schemeName :: ColorScheme -> Text
schemeName SchemeDark = "dark"
schemeName SchemeLight = "light"

-- | The scheme's chrome (status bar, pane borders) as an option overlay. It
-- is applied as the lowest-priority layer under every user scope (see
-- 'Hat.Server.applyScheme'), so a user's own @set@ always wins; without a
-- detected scheme nothing is applied, so the classic defaults stand.
applyPalette :: ColorScheme -> OptionsDelta
applyPalette scheme = foldr (uncurry insertDelta) emptyDelta
    [ (OptStatusStyle, OVStyle p.bar)
    , (OptWindowStatusStyle, OVStyle p.bar)
    , (OptWindowStatusCurrentStyle, OVStyle p.current)
    , (OptWindowStatusBellStyle, OVStyle p.bell)
    , (OptPaneBorderStyle, OVStyle p.border)
    , (OptPaneActiveBorderStyle, OVStyle p.activeBorder)
    , (OptModeStyle, OVStyle p.mode)
    ]
  where
    p = palette scheme

-- Scheme palettes: green status bars (a nod to tmux and screen) with a
-- bold highlight for the current window, an amber bell, and inactive
-- borders that recede without vanishing against their background — a
-- receding gray on dark, a lighter gray on light so they stay clearly
-- distinct from the active border. The active border keeps a green
-- accent in both schemes. The copy-mode selection is a warm amber
-- highlight (black text on gold) that stands apart from the green chrome
-- while tracking the scheme — deeper gold on dark, lighter amber on light.
data Palette = Palette
    { bar          :: Cell.Style
    , current      :: Cell.Style
    , bell         :: Cell.Style
    , border       :: Cell.Style
    , activeBorder :: Cell.Style
    , mode         :: Cell.Style
    }

palette :: ColorScheme -> Palette
palette SchemeDark = Palette
    { bar          = fgBg 151 22
    , current      = (fgBg 255 28) { Cell.bold = True }
    , bell         = (fgBg 214 22) { Cell.bold = True }
    , border       = onlyFg 65
    , activeBorder = (onlyFg 10) { Cell.bold = True }
    , mode         = fgBg 0 179
    }
palette SchemeLight = Palette
    { bar          = fgBg 22 151
    , current      = (fgBg 255 28) { Cell.bold = True }
    , bell         = (fgBg 166 151) { Cell.bold = True }
    , border       = onlyFg 250
    , activeBorder = (onlyFg 22) { Cell.bold = True }
    , mode         = fgBg 0 222
    }

fgBg :: Word8 -> Word8 -> Cell.Style
fgBg f b = Cell.defaultStyle
    { Cell.fg = Cell.Indexed f, Cell.bg = Cell.Indexed b }

onlyFg :: Word8 -> Cell.Style
onlyFg f = Cell.defaultStyle { Cell.fg = Cell.Indexed f }
