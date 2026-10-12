module Hat.Client.TtySpec (spec) where

import Control.Exception (ErrorCall (..), finally, throwIO, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import System.Posix.IO (OpenMode (ReadOnly), closeFd, defaultFileFlags, openFd)
import System.Posix.Terminal
    ( TerminalMode (ProcessInput), getTerminalAttributes, openPseudoTerminal
    , terminalMode )
import Test.Hspec

import Hat.Client.Tty

spec :: Spec
spec = do
    describe "probeTerminal" $
        it "reports a non-terminal fd as failing both isatty and tcgetattr" $ do
            -- /dev/null is a character device but not a tty: isatty is false
            -- and tcgetattr fails with ENOTTY.
            fd <- openFd "/dev/null" ReadOnly defaultFileFlags
            probe <- probeTerminal fd
            closeFd fd
            probe.isTty `shouldBe` False
            probe.canTermios `shouldSatisfy` isLeft

    describe "withCookedMode" $
        it "hands back the cooked terminal and makes it raw again, even on failure" $ do
            (master, slave) <- openPseudoTerminal
            let cooked = terminalMode ProcessInput <$> getTerminalAttributes slave
            flip finally (closeFd master >> closeFd slave) $ do
                withRawModeOn slave $ \mode -> do
                    cooked `shouldReturn` False
                    seen <- newIORef False
                    r <- try (withCookedMode mode
                        (cooked >>= writeIORef seen >> throwIO (ErrorCall "lock failed")))
                    r `shouldBe` (Left (ErrorCall "lock failed") :: Either ErrorCall ())
                    readIORef seen `shouldReturn` True
                    cooked `shouldReturn` False
                cooked `shouldReturn` True

    describe "diagnoseTerminal" $ do
        it "accepts stdin whose termios works, whatever isatty says" $ do
            -- The gate is the real capability (tcgetattr on stdin), not the
            -- isatty proxy: an emulated pts whose isatty is a false negative
            -- is still usable when tcgetattr succeeds.
            let inp = TermProbe { isTty = False, canTermios = Right () }
                out = TermProbe { isTty = False, canTermios = Right () }
            diagnoseTerminal inp out `shouldBe` Nothing

        it "rejects and diagnoses stdin whose termios fails" $ do
            let inp = TermProbe { isTty = False, canTermios = Left "ENOTTY" }
                out = TermProbe { isTty = True, canTermios = Right () }
            case diagnoseTerminal inp out of
                Nothing -> expectationFailure "expected a rejection"
                Just msg -> do
                    msg `shouldSatisfy` T.isInfixOf "not a terminal"
                    msg `shouldSatisfy` T.isInfixOf "stdin"
                    msg `shouldSatisfy` T.isInfixOf "ENOTTY"
                    -- both fds are reported, so the environment is legible
                    msg `shouldSatisfy` T.isInfixOf "stdout"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
