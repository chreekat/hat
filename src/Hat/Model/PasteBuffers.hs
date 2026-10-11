-- | The paste-buffer store, mirroring tmux's @paste.c@: named byte buffers
-- in recency order, where unnamed additions take automatic @bufferN@ names
-- and only those automatic buffers count against @buffer-limit@.
module Hat.Model.PasteBuffers
    ( PasteBuffer (..)
    , BufferOrigin (..)
    , PasteStore
    , emptyPasteStore
    , defaultBufferLimit
    , pasteWalk
    , pasteTop
    , pasteGet
    , pasteAdd
    , pasteSet
    , pasteRename
    , pasteFree
    , BufferChange (..)
    , pasteChanges
    , cleanBufferName
    , pasteSample
    , pasteVisSafe
    ) where

import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Char (chr)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)

-- | One paste buffer. Its data is bytes, not text: a buffer can hold
-- anything @load-buffer@ read.
data PasteBuffer = PasteBuffer
    { name    :: Text
    , body    :: ByteString
    , origin  :: BufferOrigin
    , order   :: Int      -- ^ recency stamp; larger is more recent
    , created :: Integer  -- ^ creation time, seconds since the epoch
    }
    deriving (Eq, Show)

-- | Whether a buffer took an automatic @bufferN@ name or was named by the
-- user (@-b@, or a rename).
data BufferOrigin = Automatic | Named
    deriving (Eq, Show)

data PasteStore = PasteStore
    { byName    :: Map Text PasteBuffer
    , nextIndex :: Int  -- ^ the next automatic name's number; never reused
    , nextOrder :: Int
    }
    deriving (Eq, Show)

emptyPasteStore :: PasteStore
emptyPasteStore = PasteStore Map.empty 0 0

-- | tmux's default @buffer-limit@.
defaultBufferLimit :: Int
defaultBufferLimit = 50

-- | Every buffer, most recent first.
pasteWalk :: PasteStore -> [PasteBuffer]
pasteWalk = sortOn (Down . (.order)) . Map.elems . (.byName)

-- | The most recent automatic buffer: what a command without @-b@ acts on.
pasteTop :: PasteStore -> Maybe PasteBuffer
pasteTop s = case filter ((== Automatic) . (.origin)) (pasteWalk s) of
    pb : _ -> Just pb
    [] -> Nothing

pasteGet :: Text -> PasteStore -> Maybe PasteBuffer
pasteGet n s = Map.lookup n s.byName

-- | Add an automatic buffer named @prefix@N, first evicting the oldest
-- automatic buffers so at most @limit@ remain with it. Empty data adds
-- nothing.
pasteAdd :: Int -> Text -> Integer -> ByteString -> PasteStore -> PasteStore
pasteAdd limit prefix now bytes s0
    | B.null bytes = s0
    | otherwise =
        let s = evict s0
            (ix, nm) = freshName s s.nextIndex
            pb = PasteBuffer
                { name = nm, body = bytes, origin = Automatic
                , order = s.nextOrder, created = now }
        in s { byName = Map.insert nm pb s.byName
             , nextIndex = ix + 1
             , nextOrder = s.nextOrder + 1 }
  where
    freshName s i
        | isJust (Map.lookup nm s.byName) = freshName s (i + 1)
        | otherwise = (i, nm)
      where nm = prefix <> T.pack (show i)
    evict s =
        let autos = filter ((== Automatic) . (.origin)) (reverse (pasteWalk s))
            excess = length autos - (limit - 1)
            doomed = map (.name) (take excess autos)
        in s { byName = foldr Map.delete s.byName doomed }

-- | Store data under a name, replacing any namesake, or as a fresh automatic
-- buffer when unnamed. Empty data stores nothing.
pasteSet
    :: Int -> Maybe Text -> Integer -> ByteString -> PasteStore
    -> Either Text PasteStore
pasteSet limit mname now bytes s
    | B.null bytes = Right s
    | otherwise = case mname of
        Nothing -> Right (pasteAdd limit "buffer" now bytes s)
        Just n
            | T.null n -> Left "empty buffer name"
            | otherwise -> case cleanBufferName n of
                Nothing -> Left ("invalid buffer name: " <> n)
                Just clean ->
                    let pb = PasteBuffer
                            { name = clean, body = bytes, origin = Named
                            , order = s.nextOrder, created = now }
                    in Right s { byName = Map.insert clean pb s.byName
                               , nextOrder = s.nextOrder + 1 }

-- | Rename a buffer, replacing any buffer already holding the new name. A
-- renamed buffer is no longer automatic.
pasteRename :: Text -> Text -> PasteStore -> Either Text PasteStore
pasteRename old new s
    | T.null old = Left "no buffer"
    | T.null new = Left "new name is empty"
    | otherwise = case cleanBufferName new of
        Nothing -> Left ("invalid buffer name: " <> new)
        Just clean -> case pasteGet old s of
            Nothing -> Left ("no buffer " <> old)
            Just pb
                | clean == old -> Right s
                | otherwise ->
                    let pb' = pb { name = clean, origin = Named }
                    in Right s { byName = Map.insert clean pb'
                                    (Map.delete old s.byName) }

pasteFree :: Text -> PasteStore -> PasteStore
pasteFree n s = s { byName = Map.delete n s.byName }

-- | How a buffer changed between two stores: tmux's paste-buffer events.
data BufferChange = BufferDeleted | BufferChanged
    deriving (Eq, Show)

-- | The buffers a store update deleted, then those it added or replaced,
-- each in name order.
pasteChanges :: PasteStore -> PasteStore -> [(BufferChange, Text)]
pasteChanges old new =
    [ (BufferDeleted, n) | n <- Map.keys (Map.difference old.byName new.byName) ]
    <> [ (BufferChanged, n)
       | (n, pb) <- Map.toList new.byName, Map.lookup n old.byName /= Just pb ]

-- | tmux's @clean_name@: a name must be printable UTF-8; backslashes are
-- escaped.
cleanBufferName :: Text -> Maybe Text
cleanBufferName n
    | T.any bad n = Nothing
    | otherwise = Just (T.replace "\\" "\\\\" n)
  where
    bad c = c < ' ' || c == '\DEL' || (c >= '\x80' && c < '\xa0')

-- | The @#{buffer_sample}@ rendering: the first 200 bytes made visible
-- C-style (@\\n@, @\\t@, octal for the rest), with @...@ when cut short.
pasteSample :: PasteBuffer -> Text
pasteSample pb =
    let width = 200
        shown = visBytes VisSample (B.take width pb.body)
        cut = B.length pb.body > width || B.length shown > width
    in lenientText (if cut then B.take width shown <> "..." else shown)

-- | The bytes @paste-buffer@ writes for one line of a buffer: valid UTF-8
-- and printable or harmless control characters as-is, anything else in
-- caret notation, so a paste cannot inject terminal escapes.
pasteVisSafe :: ByteString -> ByteString
pasteVisSafe = visBytes VisPaste

-- | The two @vis(3)@ flag sets tmux uses on buffer data: @VIS_OCTAL |
-- VIS_CSTYLE | VIS_TAB | VIS_NL@ for samples and @VIS_SAFE | VIS_NOSLASH@
-- for pastes.
data VisStyle = VisSample | VisPaste

-- | tmux's @utf8_strvis@: complete UTF-8 characters pass through, every
-- other byte goes through 'visByte'.
visBytes :: VisStyle -> ByteString -> ByteString
visBytes style = BL.toStrict . BB.toLazyByteString . go
  where
    go bs = case B.uncons bs of
        Nothing -> mempty
        Just (c, rest)
            | Just n <- utf8Len c
            , B.length bs >= n
            , let ch = B.take n bs
            , B.all isCont (B.drop 1 ch)
            , Right _ <- TE.decodeUtf8' ch ->
                BB.byteString ch <> go (B.drop n bs)
            | otherwise ->
                visByte style c (B.uncons rest >>= Just . fst) <> go rest
    utf8Len c
        | c >= 0xc2 && c <= 0xdf = Just 2
        | c >= 0xe0 && c <= 0xef = Just 3
        | c >= 0xf0 && c <= 0xf4 = Just 4
        | otherwise = Nothing
    isCont b = b .&. 0xc0 == 0x80

-- | One byte through @vis(3)@ under a 'VisStyle'; the following byte decides
-- whether a NUL needs zero padding.
visByte :: VisStyle -> Word8 -> Maybe Word8 -> BB.Builder
visByte style c next
    | visible = case style of
        VisSample | c == 0x5c -> "\\\\"
        _ -> BB.word8 c
    | VisSample <- style, Just e <- cstyle = BB.string7 ['\\', e] <> pad
    | c .&. 0x7f == 0x20 || isSample = octal
    | otherwise = meta <> caret (c .&. 0x7f)
  where
    isSample = case style of
        VisSample -> True
        VisPaste -> False
    graph = c >= 0x21 && c <= 0x7e
    visible = graph || c == 0x20 || case style of
        VisSample -> False
        VisPaste -> c `elem` [0x09, 0x0a, 0x08, 0x07, 0x0d]
    cstyle = lookup c
        [ (0x0a, 'n'), (0x0d, 'r'), (0x08, 'b'), (0x07, 'a'), (0x0b, 'v')
        , (0x09, 't'), (0x0c, 'f'), (0x20, 's'), (0x00, '0') ]
    pad = case next of
        Just d | c == 0, d >= 0x30, d <= 0x37 -> "00"
        _ -> mempty
    octal = BB.char7 '\\' <> foldMap (BB.char7 . digit) [6, 3, 0]
    digit sh = chr (fromIntegral ((c `shiftR` sh) .&. 7) + 0x30)
    meta = if c .&. 0x80 /= 0 then BB.char7 'M' else mempty
    caret b
        | b == 0x7f = "^?"
        | b < 0x20 = BB.char7 '^' <> BB.word8 (b + 0x40)
        | otherwise = BB.char7 '-' <> BB.word8 b

lenientText :: ByteString -> Text
lenientText = TE.decodeUtf8Lenient
