-- | Regenerate the emulator golden fixtures in test/fixtures/.
--
-- Captures the raw byte stream of a program running in a pty, saves it,
-- and blesses the emulator's rendering of that stream as the expected
-- grid. Run from the repo root: @cabal run gen-fixtures@.
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getEnvironment)

import Hat.Geometry
import Hat.Pty
import qualified Hat.Term.Emulator as Emu

captureSize :: Size
captureSize = Size { rows = 24, cols = 80 }

-- Read pty output until it stays quiet for half a second.
captureQuiescent :: PtyHandle -> IO B.ByteString
captureQuiescent pty = do
    acc <- newMVar []
    _ <- forkIO (reader acc)
    waitQuiet acc 0 (0 :: Int)
  where
    reader acc = do
        chunk <- readPty pty
        if B.null chunk
            then pure ()
            else do
                modifyMVar_ acc (pure . (chunk :))
                reader acc
    waitQuiet acc prev quiet = do
        threadDelay 100_000
        chunks <- readMVar acc
        let len = sum (map B.length chunks)
        if len == prev && len > 0 && quiet >= 5
            then pure (B.concat (reverse chunks))
            else waitQuiet acc len (if len == prev then quiet + 1 else 0)

renderGrid :: Emu.Emulator -> IO T.Text
renderGrid emu = do
    scr <- Emu.snapshot emu
    let h = fromIntegral scr.size.rows
    pure $ T.unlines
        [T.stripEnd (Emu.screenRowText scr r) | r <- [0 .. h - 1]]

fixture :: String -> String -> [String] -> IO ()
fixture name cmd args = do
    env0 <- getEnvironment
    let env' = [(k, v) | (k, v) <- env0, k /= "TERM"] <> [("TERM", "xterm-256color")]
    pty <- spawn Spawn
        { cmd = cmd
        , args = args
        , env = env'
        , cwd = Nothing
        , size = captureSize
        }
    raw <- captureQuiescent pty
    closePty pty
    emu <- Emu.newEmulator captureSize 1000
    _ <- Emu.feed emu raw
    grid <- renderGrid emu
    B.writeFile ("test/fixtures/" <> name <> ".raw") raw
    TIO.writeFile ("test/fixtures/" <> name <> ".expected") grid
    putStrLn $ name <> ": " <> show (B.length raw) <> " bytes captured"
    TIO.putStrLn grid

main :: IO ()
main = do
    fixture "vim-readme" "vim" ["-u", "NONE", "-i", "NONE", "README.md"]
