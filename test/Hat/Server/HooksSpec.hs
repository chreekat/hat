-- | The hooks engine: binding, scope-chain lookup, ambient hook formats,
-- recursion suppression, event waiters, and wait-for channels.
module Hat.Server.HooksSpec (spec) where

import Test.Hspec

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM
import Control.Monad (void)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text qualified as T
import Network.Socket qualified as N
import System.Timeout (timeout)

import Hat.Geometry (Pos (..), Size (..))
import Hat.Log (newLogger)
import Hat.Model
import Hat.Model.Options (AlertAction (..), Options (..), alertAllows, emptyDelta)
import Hat.Server.Command.Types (Reply (..))
import Hat.Server.Command.Wait (cmdWaitFor)
import Hat.Server.Command.Hook (cmdSetHook, cmdShowHooks)
import Hat.Server.Dispatch (installHooks, runCommandText)
import Hat.Server.Environ (emptyEnviron)
import Hat.Server.HookMonitor (addMonitor, parseMonitorSpec, sampleMonitors)
import Hat.Server.HookTypes
import Hat.Server.Hooks
import Hat.Server.Keys (EscPending (..), PrefixState (..))
import Hat.Server.Render (blankFrame)
import Hat.Transport.Wire (Autostart (..))

newState :: IO ServerState
newState = do
    lg <- newLogger "/dev/null"
    st <- newServerState Map.empty lg "/tmp/hat-hooksspec.sock" Nothing
    installHooks st
    pure st

userOpt :: ServerState -> T.Text -> IO (Maybe T.Text)
userOpt st k = Map.lookup k . (.user) <$> readTVarIO st.options

sid1 :: SessionId
sid1 = SessionId 1

spec :: Spec
spec = describe "hooks engine" $ do
    it "a global hook fires on notify and its command runs" $ do
        st <- newState
        setHook st HookGlobal "session-created" False "set -g @seen 1"
        notify st "session-created" noTarget []
        userOpt st "@seen" `shouldReturn` Just "1"

    it "hook commands see the event payload as hook formats" $ do
        st <- newState
        setHook st HookGlobal "session-created" False
            "set -gF @seen \"#{hook}:#{hook_session_name}\""
        notify st "session-created" (sessionTarget sid1)
            [("session", PSessionRef sid1 "two")]
        userOpt st "@seen" `shouldReturn` Just "session-created:two"

    it "a session hook fires only for events targeting that session" $ do
        st <- newState
        setHook st (HookSession sid1) "session-renamed" False "set -g @seen 1"
        notify st "session-renamed" (sessionTarget (SessionId 2)) []
        userOpt st "@seen" `shouldReturn` Nothing
        notify st "session-renamed" (sessionTarget sid1) []
        userOpt st "@seen" `shouldReturn` Just "1"

    it "an appended command runs after the first" $ do
        st <- newState
        setHook st HookGlobal "session-created" False "set -g @a 1"
        setHook st HookGlobal "session-created" True "set -gF @b \"#{@a}x\""
        notify st "session-created" noTarget []
        userOpt st "@b" `shouldReturn` Just "1x"

    it "unset removes the whole hook" $ do
        st <- newState
        setHook st HookGlobal "session-created" False "set -g @seen 1"
        unsetHook st HookGlobal "session-created"
        notify st "session-created" noTarget []
        userOpt st "@seen" `shouldReturn` Nothing

    it "a hook fired from inside a hook is suppressed" $ do
        st <- newState
        calls <- newIORef (0 :: Int)
        installHookEngine st
            (\txt -> do
                modifyIORef' calls (+ 1)
                -- A hook command that itself raises the same event.
                notify st "session-created" noTarget []
                void (runCommandText st Nothing txt))
            (\_ t -> pure t)
            []
        setHook st HookGlobal "session-created" False "set -g @seen 1"
        notify st "session-created" noTarget []
        readIORef calls `shouldReturn` 1

    it "firing bumps the entry's fire count and time" $ do
        st <- newState
        setHook st HookGlobal "session-created" False "set -g @seen 1"
        notify st "session-created" noTarget []
        entries <- hookEntriesAt st HookGlobal
        case Map.lookup "session-created" entries of
            Nothing -> expectationFailure "entry vanished"
            Just e -> do
                e.fireCount `shouldBe` 1
                e.fireTime `shouldSatisfy` isJust

    it "set-hook rejects an unknown hook name" $ do
        st <- newState
        rs <- cmdSetHook st Nothing ["-g", "no-such-hook", "display x"]
        rs `shouldBe` [RErr "invalid option: no-such-hook"]

    it "set-hook -E rejects a non-@ event name" $ do
        st <- newState
        rs <- cmdSetHook st Nothing ["-E", "window-renamed"]
        rs `shouldBe` [RErr "event name must start with @"]

    it "show-hooks lists array items and user hooks" $ do
        st <- newState
        setHook st HookGlobal "session-created" False "set -g @a 1"
        setHook st HookGlobal "session-created" True "set -g @b 2"
        setHook st HookGlobal "@user-hook" False "lsk"
        rs <- cmdShowHooks st Nothing ["-g"]
        rs `shouldBe`
            [ ROutput "@user-hook lsk"
            , ROutput "session-created[0] set -g @a 1"
            , ROutput "session-created[1] set -g @b 2"
            ]

    it "a user event runs a same-named registered hook" $ do
        st <- newState
        _ <- cmdSetHook st Nothing ["-g", "@manual", "set -g @seen 1"]
        fireUserEvent st "@manual" noTarget []
        userOpt st "@seen" `shouldReturn` Just "1"

    it "an unregistered user event fires no hooks but succeeds" $ do
        st <- newState
        fireUserEvent st "@nobody" noTarget []
        userOpt st "@seen" `shouldReturn` Nothing

    it "alertAllows follows tmux's action table" $ do
        alertAllows AlertAny True `shouldBe` True
        alertAllows AlertAny False `shouldBe` True
        alertAllows AlertNone True `shouldBe` False
        alertAllows AlertCurrent True `shouldBe` True
        alertAllows AlertCurrent False `shouldBe` False
        alertAllows AlertOther True `shouldBe` False
        alertAllows AlertOther False `shouldBe` True

    describe "-B monitors" $ do
        it "parses a monitor spec into name, target, and format" $ do
            parseMonitorSpec "@x::#{session_name}"
                `shouldBe` Right ("@x", MonSession, "#{session_name}")
            parseMonitorSpec "@x:%5:#{f}"
                `shouldBe` Right ("@x", MonPane 5, "#{f}")
            parseMonitorSpec "@x:%*:#{f}"
                `shouldBe` Right ("@x", MonAllPanes, "#{f}")
            parseMonitorSpec "bare" `shouldBe`
                Left "invalid subscription: bare"

        it "fires its exact-scope hook when the sampled value changes" $ do
            st <- newState
            sess <- fakeSession
            atomically $ modifyTVar' st.sessions (Map.insert sess.id sess)
            addMonitor st HookGlobal "@m" MonSession "#{@value}" Nothing
            setHook st HookGlobal "@m" False
                "set -g @seen \"#{hook_last}->#{hook_value}\""
            _ <- runCommandText st Nothing "set -g @value one"
            sampleMonitors st          -- first sample only records
            userOpt st "@seen" `shouldReturn` Nothing
            sampleMonitors st          -- unchanged: no fire
            userOpt st "@seen" `shouldReturn` Nothing
            _ <- runCommandText st Nothing "set -g @value two"
            sampleMonitors st
            userOpt st "@seen" `shouldReturn` Just "one->two"

        it "a monitor change wakes wait-for -E waiters on its name" $ do
            st <- newState
            sess <- fakeSession
            atomically $ modifyTVar' st.sessions (Map.insert sess.id sess)
            client <- fakeClient st
            addMonitor st HookGlobal "@m" MonSession "#{@value}" Nothing
            sampleMonitors st
            done <- newEmptyTMVarIO
            _ <- forkIO $ do
                r <- cmdWaitFor st (Just client) ["-E", "@m"]
                atomically (putTMVar done r)
            awaitEventWaiters st "@m"
            _ <- runCommandText st Nothing "set -g @value new"
            sampleMonitors st
            r <- timeout 1_000_000 (atomically (takeTMVar done))
            r `shouldBe` Just []

    describe "wait-for" $ do
        it "signal then wait returns immediately" $ do
            st <- newState
            client <- fakeClient st
            _ <- cmdWaitFor st Nothing ["-S", "chan"]
            r <- timeout 1_000_000 (cmdWaitFor st (Just client) ["chan"])
            r `shouldBe` Just []

        it "wait then signal wakes the waiter" $ do
            st <- newState
            client <- fakeClient st
            done <- newEmptyTMVarIO
            _ <- forkIO $ do
                r <- cmdWaitFor st (Just client) ["chan"]
                atomically (putTMVar done r)
            awaitWaiters st "chan"
            _ <- cmdWaitFor st Nothing ["-S", "chan"]
            r <- timeout 1_000_000 (atomically (takeTMVar done))
            r `shouldBe` Just []

        it "an event wakes a wait-for -E waiter whose filter matches" $ do
            st <- newState
            client <- fakeClient st
            done <- newEmptyTMVarIO
            _ <- forkIO $ do
                r <- cmdWaitFor st (Just client)
                    ["-E", "-v", "-F", "#{==:#{value},3}", "@ev"]
                atomically (putTMVar done r)
            awaitEventWaiters st "@ev"
            fireUserEvent st "@ev" noTarget [("value", PText "2")]
            fireUserEvent st "@ev" noTarget [("value", PText "3")]
            r <- timeout 1_000_000 (atomically (takeTMVar done))
            r `shouldBe` Just [ROutput "event=@ev", ROutput "value=3"]

        it "wait-for -E rejects an unknown event" $ do
            st <- newState
            rs <- cmdWaitFor st Nothing ["-E", "foobar"]
            rs `shouldBe` [RErr "invalid event: foobar"]

-- A monitor needs a session to sample in; an empty one will do.
fakeSession :: IO Session
fakeSession = do
    nameVar <- newTVarIO "s"
    windowsVar <- newTVarIO Map.empty
    currentVar <- newTVarIO 0
    histVar <- newTVarIO []
    sizeVar <- newTVarIO (Size 80 24)
    environVar <- newTVarIO emptyEnviron
    cwdVar <- newTVarIO "/"
    optionsVar <- newTVarIO emptyDelta
    pure Session
        { id = SessionId 9
        , name = nameVar
        , windows = windowsVar
        , currentIx = currentVar
        , windowHist = histVar
        , lastSize = sizeVar
        , environ = environVar
        , startCwd = cwdVar
        , options = optionsVar
        }

-- A waiter needs a client only for its name; the lightest real one will do.
fakeClient :: ServerState -> IO Client
fakeClient _ = do
    (sock, _) <- N.socketPair N.AF_UNIX N.Stream N.defaultProtocol
    sendLock <- newMVar ()
    sizeVar <- newTVarIO (Size 80 24)
    activeVar <- newTVarIO 0
    sessVar <- newTVarIO (SessionId (-1))
    histVar <- newTVarIO []
    readyVar <- newTVarIO False
    keyVar <- newIORef NoPrefix
    escVar <- newIORef NoEscPending
    frameVar <- newIORef (blankFrame (Size 80 24))
    cursorVar <- newIORef (Pos 0 0, True)
    colourVar <- newIORef ""
    fullVar <- newTVarIO True
    toastVar <- newTVarIO Nothing
    promptVar <- newTVarIO Nothing
    pickerVar <- newTVarIO Nothing
    focusVar <- newTVarIO True
    envImportVar <- newTVarIO ImportEnv
    pure Client
        { id = ClientId 0
        , role = Control
        , autostart = Joined
        , sock = sock
        , wireLevel = 4
        , sendLock = sendLock
        , size = sizeVar
        , lastActive = activeVar
        , session = sessVar
        , sessionHist = histVar
        , ready = readyVar
        , keyState = keyVar
        , escState = escVar
        , lastFrame = frameVar
        , lastCursor = cursorVar
        , lastCursorColour = colourVar
        , needsFull = fullVar
        , toast = toastVar
        , prompt = promptVar
        , picker = pickerVar
        , outerFocused = focusVar
        , envImport = envImportVar
        , env = []
        , cwd = "/"
        }

-- Wait (bounded) until a channel waiter registers.
awaitWaiters :: ServerState -> T.Text -> IO ()
awaitWaiters st name = do
    r <- timeout 1_000_000 . atomically $ do
        chans <- readTVar st.hooks.channels
        case Map.lookup name chans of
            Just ch | not (null ch.waiters) -> pure ()
            _ -> retry
    maybe (expectationFailure "no waiter registered") pure r

awaitEventWaiters :: ServerState -> T.Text -> IO ()
awaitEventWaiters st name = do
    r <- timeout 1_000_000 . atomically $ do
        ws <- readTVar st.hooks.eventWaiters
        if any (\w -> w.name == name) ws then pure () else retry
    maybe (expectationFailure "no event waiter registered") pure r
