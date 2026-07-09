-- | Copy-mode motion, selection, and extraction. Each handler runs on
-- one pane's 'CopyModeState' and returns the new state (or 'Nothing'
-- to leave copy mode).
--
-- Coordinate space: rows are ABSOLUTE within the pane's grid. Row @0@
-- is the oldest scrollback line; rows @hsize..hsize+sy-1@ are the
-- live screen. Columns are 0-indexed within a row and clamp at the
-- pane's width.
module Hat.Server.CopyMode
    ( Handler
    , handlers
    , extractSelection
    , yankSelection
    ) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Hat.Geometry
import Hat.Model
import Hat.Model.Options
import qualified Hat.Term.Cell as Cell
import qualified Hat.Term.Emulator as Emu

-- | A copy-mode @-X@ handler. 'Nothing' means \"exit copy mode\".
type Handler
    = ServerState -> Pane -> CopyModeState -> IO (Maybe CopyModeState)

-- | Full handler table, keyed by tmux's @send-keys -X <name>@.
handlers :: Map Text Handler
handlers = Map.fromList
    [ ("cancel",            \_ _ _ -> pure Nothing)
    , ("cursor-left",       pure_ cursorLeft)
    , ("cursor-right",      io1 cursorRight)
    , ("cursor-up",         io1 cursorUp)
    , ("cursor-down",       io1 cursorDown)
    , ("start-of-line",     pure_ startOfLine)
    , ("end-of-line",       io1 endOfLine)
    , ("history-top",       io1 historyTop)
    , ("history-bottom",    io1 historyBottom)
    , ("top-line",          io1 topLine)
    , ("middle-line",       io1 middleLine)
    , ("bottom-line",       io1 bottomLine)
    , ("begin-selection",   pure_ beginSelection)
    , ("clear-selection",   pure_ clearSelection)
    , ("select-line",       io1 selectLine)
    , ("rectangle-toggle",  pure_ rectangleToggle)
    , ("next-word",         withOpts (nextWord False))
    , ("next-word-end",     withOpts (nextWordEnd False))
    , ("previous-word",     withOpts (previousWord False))
    , ("next-space",        withOpts (nextWord True))
    , ("next-space-end",    withOpts (nextWordEnd True))
    , ("previous-space",    withOpts (previousWord True))
    , ("copy-selection",
        \st p s -> yankSelection st p s >> pure (Just (clearSel s)))
    , ("copy-selection-and-cancel",
        \st p s -> yankSelection st p s >> pure Nothing)
    ]
  where
    pure_ f _ _ s = pure (Just (f s))
    io1 f _ p s = Just <$> f p s
    withOpts f st p s = do
        opts <- readTVarIO st.options
        Just <$> f opts p s

-- ---------------------------------------------------------------------
-- Grid access

-- | The pane's grid dimensions: (hsize, sy, sx).
paneDims :: Pane -> IO (Int, Int, Int)
paneDims pane = do
    hsize <- Emu.scrollbackLength pane.emulator
    scr <- Emu.snapshot pane.emulator
    pure ( hsize
         , fromIntegral scr.size.rows
         , fromIntegral scr.size.cols
         )

-- | Read the cells of one absolute row. Rows outside the grid come
-- back as an empty list (treated as blank line).
rowCells :: Pane -> Int -> IO [Cell.Cell]
rowCells pane row = do
    hsize <- Emu.scrollbackLength pane.emulator
    if row < 0
        then pure []
        else if row < hsize
            then maybe [] id <$> Emu.scrollbackLine pane.emulator row
            else do
                scr <- Emu.snapshot pane.emulator
                let vrow = row - hsize
                pure $ case scr.cells V.!? vrow of
                    Nothing -> []
                    Just v -> V.toList v

-- | Read the cell at (absolute-row, col). Blanks come back as
-- 'Cell.blankCell'.
cellAt :: Pane -> Int -> Int -> IO Cell.Cell
cellAt pane row col = do
    cs <- rowCells pane row
    pure $ case drop col cs of
        (c : _) -> c
        [] -> Cell.blankCell

-- | Character stored in one cell (empty for zero-width continuations).
cellChar :: Cell.Cell -> Text
cellChar = (.text)

-- | Length of the visible content on this row: index one past the
-- last non-blank cell.
lineLength :: Pane -> Int -> IO Int
lineLength pane row = do
    cs <- rowCells pane row
    let indexed = zip [0 :: Int ..] cs
        lastFilled = foldr keep (-1) indexed
        keep (i, c) acc
            | isBlank c = acc
            | otherwise = max acc i
    pure (lastFilled + 1)
  where
    isBlank c = T.strip c.text == ""

-- ---------------------------------------------------------------------
-- Simple motions

clampCol :: Int -> Int -> Int
clampCol sx c = max 0 (min (max 0 (sx - 1)) c)

cursorLeft :: CopyModeState -> CopyModeState
cursorLeft s = s { cursorCol = max 0 (s.cursorCol - 1) }

cursorRight :: Pane -> CopyModeState -> IO CopyModeState
cursorRight pane s = do
    (_, _, sx) <- paneDims pane
    pure s { cursorCol = clampCol sx (s.cursorCol + 1) }

cursorUp :: Pane -> CopyModeState -> IO CopyModeState
cursorUp _ s = pure s { cursorRow = max 0 (s.cursorRow - 1) }

cursorDown :: Pane -> CopyModeState -> IO CopyModeState
cursorDown pane s = do
    (hsize, sy, _) <- paneDims pane
    pure s { cursorRow = min (hsize + sy - 1) (s.cursorRow + 1) }

startOfLine :: CopyModeState -> CopyModeState
startOfLine s = s { cursorCol = 0 }

endOfLine :: Pane -> CopyModeState -> IO CopyModeState
endOfLine pane s = do
    n <- lineLength pane s.cursorRow
    (_, _, sx) <- paneDims pane
    pure s { cursorCol = clampCol sx (max 0 (n - 1)) }

historyTop :: Pane -> CopyModeState -> IO CopyModeState
historyTop _ s = pure s { cursorRow = 0, cursorCol = 0 }

historyBottom :: Pane -> CopyModeState -> IO CopyModeState
historyBottom pane s = do
    (hsize, sy, _) <- paneDims pane
    pure s { cursorRow = hsize + sy - 1, cursorCol = 0 }

topLine :: Pane -> CopyModeState -> IO CopyModeState
topLine pane s = do
    (hsize, _, _) <- paneDims pane
    pure s { cursorRow = hsize, cursorCol = 0 }

middleLine :: Pane -> CopyModeState -> IO CopyModeState
middleLine pane s = do
    (hsize, sy, _) <- paneDims pane
    pure s { cursorRow = hsize + sy `div` 2, cursorCol = 0 }

bottomLine :: Pane -> CopyModeState -> IO CopyModeState
bottomLine pane s = do
    (hsize, sy, _) <- paneDims pane
    pure s { cursorRow = hsize + sy - 1, cursorCol = 0 }

-- ---------------------------------------------------------------------
-- Selection

beginSelection :: CopyModeState -> CopyModeState
beginSelection s = s
    { selection = Just ((s.cursorRow, s.cursorCol), SelChar) }

clearSelection :: CopyModeState -> CopyModeState
clearSelection s = s { selection = Nothing }

selectLine :: Pane -> CopyModeState -> IO CopyModeState
selectLine _ s = pure s
    { selection = Just ((s.cursorRow, 0), SelLine)
    , cursorCol = 0
    }

rectangleToggle :: CopyModeState -> CopyModeState
rectangleToggle s = case s.selection of
    Just (a, SelRect) -> s { selection = Just (a, SelChar) }
    Just (a, _)      -> s { selection = Just (a, SelRect) }
    Nothing          -> s { selection = Just ((s.cursorRow, s.cursorCol), SelRect) }

clearSel :: CopyModeState -> CopyModeState
clearSel s = s { selection = Nothing }

-- ---------------------------------------------------------------------
-- Word / space motion (tmux grid-reader semantics)

whitespace :: Char -> Bool
whitespace c = c == ' ' || c == '\t'

isSep :: Options -> Bool -> Char -> Bool
isSep _ True _   = False              -- \"space\" mode: only whitespace separates
isSep opts False c = T.any (== c) opts.wordSeparators

cellFirstChar :: Cell.Cell -> Maybe Char
cellFirstChar c = case T.uncons c.text of
    Just (ch, _) -> Just ch
    Nothing -> Nothing

-- | Advance the (row, col) by one cell, wrapping to the next row at
-- the pane's width. Returns 'Nothing' at end-of-grid.
step :: Pane -> Int -> Int -> IO (Maybe (Int, Int))
step pane row col = do
    (hsize, sy, sx) <- paneDims pane
    let col' = col + 1
    if col' < sx
        then pure (Just (row, col'))
        else if row + 1 < hsize + sy
            then pure (Just (row + 1, 0))
            else pure Nothing

-- | Same but backwards.
stepBack :: Pane -> Int -> Int -> IO (Maybe (Int, Int))
stepBack pane row col
    | col > 0 = pure (Just (row, col - 1))
    | row > 0 = do
        (_, _, sx) <- paneDims pane
        pure (Just (row - 1, sx - 1))
    | otherwise = pure Nothing

-- | Skip forward while a predicate on the current cell is 'True'. On
-- exit, cursor sits on the first cell that failed the predicate (or
-- at end-of-grid).
skipForward
    :: Pane -> Int -> Int -> (Char -> Bool) -> IO (Int, Int)
skipForward pane r c predicate = loop r c
  where
    loop row col = do
        ch <- charAt pane row col
        if predicate ch
            then do
                nxt <- step pane row col
                case nxt of
                    Nothing -> pure (row, col)
                    Just (r', c') -> loop r' c'
            else pure (row, col)

skipBackward
    :: Pane -> Int -> Int -> (Char -> Bool) -> IO (Int, Int)
skipBackward pane r c predicate = loop r c
  where
    loop row col = do
        ch <- charAt pane row col
        if predicate ch
            then do
                prev <- stepBack pane row col
                case prev of
                    Nothing -> pure (row, col)
                    Just (r', c') -> loop r' c'
            else pure (row, col)

charAt :: Pane -> Int -> Int -> IO Char
charAt pane row col = do
    cell <- cellAt pane row col
    pure $ case cellFirstChar cell of
        Just c -> c
        Nothing -> ' '

-- | @next-word@: advance to the start of the next word. The @spaceMode@
-- flag turns this into @next-space@ (empty separators).
nextWord :: Bool -> Options -> Pane -> CopyModeState -> IO CopyModeState
nextWord spaceMode opts pane s = do
    let sep = isSep opts spaceMode
    ch0 <- charAt pane s.cursorRow s.cursorCol
    -- Skip current run: separators if starting on one, else word-chars.
    (r1, c1) <- if not (whitespace ch0)
        then if sep ch0
            then skipForward pane s.cursorRow s.cursorCol
                (\c -> sep c && not (whitespace c))
            else skipForward pane s.cursorRow s.cursorCol
                (\c -> not (sep c) && not (whitespace c))
        else pure (s.cursorRow, s.cursorCol)
    -- Then skip whitespace to the next word's start.
    (r2, c2) <- skipForward pane r1 c1 whitespace
    pure s { cursorRow = r2, cursorCol = c2 }

-- | @next-word-end@: move to the end of the next word. If we start
-- inside a word, this is the position of the last cell of THIS word.
nextWordEnd :: Bool -> Options -> Pane -> CopyModeState -> IO CopyModeState
nextWordEnd spaceMode opts pane s = do
    let sep = isSep opts spaceMode
    -- Advance one cell first (so calling from end-of-word walks to the
    -- next word).
    mstart <- step pane s.cursorRow s.cursorCol
    case mstart of
        Nothing -> pure s
        Just (r0, c0) -> do
            -- Skip whitespace.
            (r1, c1) <- skipForward pane r0 c0 whitespace
            ch <- charAt pane r1 c1
            -- Then skip word / separator run to its last cell.
            (r2, c2) <- if sep ch
                then skipForward pane r1 c1
                    (\c -> sep c && not (whitespace c))
                else skipForward pane r1 c1
                    (\c -> not (sep c) && not (whitespace c))
            -- @skipForward@ leaves us on the first failing cell; the
            -- word-end is one step back.
            back <- stepBack pane r2 c2
            let (er, ec) = maybe (r2, c2) id back
            pure s { cursorRow = er, cursorCol = ec }

-- | @previous-word@: move backward to the start of the previous word.
previousWord :: Bool -> Options -> Pane -> CopyModeState -> IO CopyModeState
previousWord spaceMode opts pane s = do
    let sep = isSep opts spaceMode
    -- Step back once (so calling from start-of-word walks to the prior).
    mprev <- stepBack pane s.cursorRow s.cursorCol
    case mprev of
        Nothing -> pure s
        Just (r0, c0) -> do
            -- Skip whitespace backwards.
            (r1, c1) <- skipBackward pane r0 c0 whitespace
            ch <- charAt pane r1 c1
            -- Skip current word/separator run to its first cell.
            (r2, c2) <- if sep ch
                then skipBackward pane r1 c1
                    (\c -> sep c && not (whitespace c))
                else skipBackward pane r1 c1
                    (\c -> not (sep c) && not (whitespace c))
            -- @skipBackward@ leaves us on the first failing cell going
            -- backwards; step forward one to land on the word's start.
            fwd <- step pane r2 c2
            let (br, bc) = maybe (r2, c2) id fwd
            pure s { cursorRow = br, cursorCol = bc }

-- ---------------------------------------------------------------------
-- Copy: extract the selection and push it onto the buffer store.

-- | Extract the cells covered by the current selection as text. Rows
-- are joined with @\\n@. Selection is honored per 'ModeKeys': vi copy
-- is INCLUSIVE at the end cell; emacs is EXCLUSIVE.
extractSelection
    :: ModeKeys -> Pane -> CopyModeState -> IO (Maybe Text)
extractSelection _ _ s | Nothing <- s.selection = pure Nothing
extractSelection keys pane s = case s.selection of
    Nothing -> pure Nothing
    Just ((ar, ac), _kind) -> do
        let (sr, sc, er, ec) =
                if (s.cursorRow, s.cursorCol) < (ar, ac)
                    then (s.cursorRow, s.cursorCol, ar, ac)
                    else (ar, ac, s.cursorRow, s.cursorCol)
            endExclusive = case keys of
                KeysEmacs -> ec
                KeysVi -> ec + 1
        chunks <- mapM (extractRow pane sr sc er endExclusive)
            [sr .. er]
        pure . Just $ T.intercalate "\n" chunks

-- One row of the selection, with correct start/end columns.
extractRow :: Pane -> Int -> Int -> Int -> Int -> Int -> IO Text
extractRow pane sr sc er ec row = do
    cs <- rowCells pane row
    (_, _, sx) <- paneDims pane
    lineLen <- lineLength pane row
    let firstCol = if row == sr then sc else 0
        lastColX = if row == er then min ec lineLen else max sx lineLen
        slice = takeCells firstCol (lastColX - firstCol) cs
    pure . T.concat . map (.text) $ slice

takeCells :: Int -> Int -> [Cell.Cell] -> [Cell.Cell]
takeCells start n = take n . drop start

-- | Copy the current selection into a fresh paste-buffer at the front
-- of the stack. Clears the pane's selection.
yankSelection :: ServerState -> Pane -> CopyModeState -> IO ()
yankSelection st pane s = do
    opts <- readTVarIO st.options
    mtext <- extractSelection opts.modeKeys pane s
    case mtext of
        Nothing -> pure ()
        Just body -> atomically $ do
            n <- readTVar st.nextBuffer
            writeTVar st.nextBuffer (n + 1)
            let name = "buffer" <> T.pack (show n)
            modifyTVar' st.buffers ((name, body) Seq.<|)
            bumpDirty st
