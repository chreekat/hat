{-# LANGUAGE OverloadedStrings #-}

module Hat.Server.ControlSpec (spec) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Network.Socket (Family (AF_UNIX), SocketType (Stream), defaultProtocol, socketPair)
import Test.Hspec

import Hat.Geometry
import Hat.Log (newLogger)
import Hat.Model (ServerState (..), Session (..), newServerState)
import Hat.Model.Ids
import Hat.Server (dispatch, welcome)
import Hat.Server.Control
import Hat.Server.ControlQueue
import Hat.Server.ControlTypes
import Hat.Server.Dispatch (resolveCommand, runArgv)
import Hat.Server.Resize (reconcileLoop)
import Hat.TestSupport (addClient, withPanesReaped)
import Hat.Transport.Wire
    ( ArgvBytes (..), ClientToServer (..), ExitWait (..), Hello (..), Inbound (..)
    , Intent (..), ServerToClient (..), newReadEnd, protocolVersion, recvMessage
    , sendMessage )

lines' :: ControlState -> IO [String]
lines' cs = do
    q <- readTVarIO cs.queue
    pure [ show l | ControlLine l <- toList q ]

spec :: Spec
spec = do
    describe "escapeOutput" $ do
        let esc = BL.toStrict . BB.toLazyByteString . escapeOutput
        it "octal-escapes control bytes and backslash, nothing else" $
            esc "a\ESC[1m\\b\r\n\200" `shouldBe` "a\\033[1m\\134b\\015\\012\200"

    describe "guardLine" $
        it "carries time, number, and the origin's flag" $ do
            guardLine "begin" 1363006971 2 ClientLine
                `shouldBe` "%begin 1363006971 2 1"
            guardLine "end" 5 1 SetupCommand `shouldBe` "%end 5 1 0"

    describe "splitInputLines" $
        it "keeps the unterminated tail, and empty lines as lines" $
            splitInputLines "a\n\nb" `shouldBe` (["a", ""], "b")

    describe "resolveCommand" $ do
        it "takes a unique prefix of a command name" $
            resolveCommand "a" `shouldBe` Right "attach-session"
        it "lists the candidates of an ambiguous prefix" $
            resolveCommand "li" `shouldSatisfy` either (const True) (const False)
        it "never abbreviates to a hat-only command" $
            resolveCommand "restore" `shouldBe` Left "unknown command: restore"

    describe "refresh-client -f" $ do
        it "sets and clears the control flags it names, skipping others" $
            applyControlFlags
                (parseControlFlags "no-output,wait-exit,read-only,pause-after=2")
                defaultControlFlags
                `shouldBe` ControlFlags
                    { output = NoOutput, exitWait = AwaitEmptyLine
                    , pauseAfter = Just 2000 }
        it "clears with !" $
            applyControlFlags (parseControlFlags "!wait-exit,!pause-after")
                defaultControlFlags { exitWait = AwaitEmptyLine, pauseAfter = Just 0 }
                `shouldBe` defaultControlFlags

    describe "refresh-client -C" $ do
        it "reads a client size as WxH or W,H" $ do
            parseControlSize "100,50" `shouldBe` Right (ClientSize (Size 50 100))
            parseControlSize "200x200" `shouldBe` Right (ClientSize (Size 200 200))
        it "reads a window size, and its clearing" $ do
            parseControlSize "@3:40x10"
                `shouldBe` Right (WindowSize (WindowId 3) (Size 10 40))
            parseControlSize "@3:" `shouldBe` Right (ClearWindowSize (WindowId 3))
        it "rejects sizes out of tmux's bounds and garbage" $ do
            parseControlSize "0x5" `shouldBe` Left "size too small or too big"
            parseControlSize "big" `shouldBe` Left "bad size argument"

    describe "the outbound queue" $ do
        it "holds notifications until the open command block closes" $ do
            cs <- newControlState
            atomically $ do
                openBlock cs
                controlEmit cs "%begin"
                controlNotify cs "%window-renamed @0 x"
                controlEmit cs "%end"
                closeBlock cs
            lines' cs `shouldReturn`
                map show ["%begin", "%end", "%window-renamed @0 x" :: String]
        it "drops new notifications and queued output once exiting, not reply lines" $ do
            cs <- newControlState
            atomically $ do
                controlOutput cs (PaneId 1) 0 "out"
                controlNotify cs "%sessions-changed"
                requestExit cs Nothing
                controlNotify cs "%window-renamed @0 late"
                controlEmit cs "%end 1 1 1"
            lines' cs `shouldReturn` map show ["%sessions-changed", "%end 1 1 1" :: String]
            n <- length . toList <$> readTVarIO cs.queue
            n `shouldBe` 2
        it "coalesces a pane's consecutive output" $ do
            cs <- newControlState
            atomically $ do
                controlOutput cs (PaneId 1) 5 "ab"
                controlOutput cs (PaneId 1) 9 "cd"
            q <- toList <$> readTVarIO cs.queue
            [ (p, t, b) | ControlOutput p t b <- q ]
                `shouldBe` [(PaneId 1, 5, "abcd")]
            readTVarIO cs.outputBytes `shouldReturn` 4
        it "makes a client exit once a pane's queued output is too old for more" $ do
            cs <- newControlState
            atomically $ do
                controlOutput cs (PaneId 1) 0 "old"
                controlOutput cs (PaneId 1) 300000 "new"
            ex <- readTVarIO cs.exiting
            case ex of
                Exiting (Just "too far behind") -> pure ()
                _ -> expectationFailure "not exiting as too far behind"
            readTVarIO cs.outputBytes `shouldReturn` 0
        it "pauses, under pause-after, a pane whose queued output is too old" $ do
            cs <- newControlState
            atomically $ do
                modifyTVar' cs.flags (\fl -> fl { pauseAfter = Just 1000 })
                controlOutput cs (PaneId 1) 0 "old"
                controlOutput cs (PaneId 2) 0 "other"
                controlOutput cs (PaneId 1) 1000 "new"
            Map.lookup (PaneId 1) <$> readTVarIO cs.panes `shouldReturn` Just PanePaused
            q <- toList <$> readTVarIO cs.queue
            [ (p, b) | ControlOutput p _ b <- q ] `shouldBe` [(PaneId 2, "other")]
            lines' cs `shouldReturn` [show ("%pause %1" :: String)]
            readTVarIO cs.outputBytes `shouldReturn` 5
        it "turns a detach into an exit with no reason" $ do
            cs <- newControlState
            atomically (controlSend cs DetachOk)
            ex <- readTVarIO cs.exiting
            case ex of
                Exiting Nothing -> pure ()
                _ -> expectationFailure "not exiting without a reason"

    describe "a command client's exit status" $ do
        it "is the last one its commands reported" $ do
            st <- freshServer
            exitStatuses st [["run-shell", "exit 3"]] `shouldReturn` [3]
            exitStatuses st [["run-shell", "exit 3"], ["run-shell", "true"]] `shouldReturn` [0]
        it "is a waited-for popup's command's" $ do
            st <- freshServer
            withPanesReaped st $ do
                runArgv st Nothing ["new-session", "-d", "-s", "S", "cat"] `shouldReturn` []
                [sess] <- Map.elems <$> readTVarIO st.sessions
                _ <- addClient st sess.id (Size 24 80) 1
                exitStatuses st [["display-popup", "-E", "exit 4"]] `shouldReturn` [4]

freshServer :: IO ServerState
freshServer = do
    lg <- newLogger "/dev/null"
    newServerState Map.empty lg "/tmp/hat-controlspec.sock" Nothing

-- | The exit statuses the server sends a command client for these commands,
-- as one connection over a socketpair. A command is reported done only once
-- the panes are reconciled, so the loop that reconciles them runs too.
exitStatuses :: ServerState -> [[Text]] -> IO [Int]
exitStatuses st cmds = withAsync (reconcileLoop st) $ \_ -> do
    (server, client) <- socketPair AF_UNIX Stream defaultProtocol
    serverRe <- newReadEnd server
    clientRe <- newReadEnd client
    let hello = Hello
            { protoVersion = protocolVersion, term = "xterm", env = []
            , size = Size 24 80, cwd = "/", intent = ControlIntent
            , autostarted = False, sessionHist = [], termCaps = Nothing
            , argvBytes = TextIsExact, ttyName = Nothing }
        collect acc = recvMessage clientRe >>= \case
            Just (Known (ExitStatus n)) -> collect (acc <> [n])
            Just (Known CommandDone) -> pure acc
            Just _ -> collect acc
            Nothing -> pure acc
    withAsync (welcome dispatch st serverRe hello) $ \_ -> do
        Just (Known (Welcome _)) <- recvMessage clientRe
        Just (Known (ServerVersion _)) <- recvMessage clientRe
        sendMessage client (Command cmds)
        statuses <- collect []
        statuses <$ sendMessage client Detach

