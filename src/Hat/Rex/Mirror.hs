-- | Following a Rex terminal stream with a local emulator.
module Hat.Rex.Mirror
    ( mirrorChunk
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Hat.Rex.Data (Nonce)
import Hat.Rex.Stream
import Hat.Term.Emulator (Emulator, feedMirror, resize)

-- | Apply the next chunk of a pane's stream, after its snapshot, to the
-- pane's emulator: the program's bytes are fed and Rex's messages acted on,
-- each in its place. Gives the carry for the chunk after, or why the stream
-- cannot be followed further.
mirrorChunk :: Nonce -> Emulator -> Carry -> ByteString -> IO (Either Text Carry)
mirrorChunk nonce emu carry chunk = go pieces
  where
    (carry', pieces) = splitRex nonce carry chunk
    go = \case
        [] -> pure (Right carry')
        TermBytes bs : rest -> feedMirror emu bs >> go rest
        RexMessage (Resize sz) : rest -> resize emu sz >> go rest
        RexMessage (UnknownStreamMessage _) : rest -> go rest
        StreamError why : _ -> pure (Left why)
