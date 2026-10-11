-- | The boxes menus and popups draw in (tmux's @screen_write_box@ and
-- @screen_write_hline@): a frame of border glyphs in one of
-- @menu-border-lines@'s \/ @popup-border-lines@'s line sets, with a title
-- drawn over its top edge.
module Hat.Server.Box
    ( BoxLines (..)
    , boxLinesOf
    , boxGlyph
    , Grid
    , blankGrid
    , setCell
    , drawBox
    , drawHLine
    , drawFormatAt
    ) where

import Data.Text (Text)
import Data.Vector qualified as V

import Hat.Server.FormatDraw (acsGlyph, formatDraw)
import Hat.Server.Scene (CellType (..))
import Hat.Server.Theme (Theme)
import Hat.Term.Cell qualified as Cell

-- | tmux's @enum box_lines@.
data BoxLines
    = BoxSingle | BoxDouble | BoxHeavy | BoxSimple | BoxRounded | BoxPadded | BoxNone
    deriving (Eq, Show)

-- | A @menu-border-lines@ \/ @popup-border-lines@ choice.
boxLinesOf :: Text -> Maybe BoxLines
boxLinesOf = \case
    "single" -> Just BoxSingle
    "double" -> Just BoxDouble
    "heavy" -> Just BoxHeavy
    "simple" -> Just BoxSimple
    "rounded" -> Just BoxRounded
    "padded" -> Just BoxPadded
    "none" -> Just BoxNone
    _ -> Nothing

-- | The glyph a box cell of a type draws in (tmux's
-- @screen_write_box_border_set@); a box without lines keeps a blank.
boxGlyph :: BoxLines -> CellType -> Char
boxGlyph lns ct = case lns of
    BoxNone -> ' '
    BoxPadded -> ' '
    BoxSimple -> " |-+++++++++." !! (fromEnum ct + 1)
    BoxSingle -> acsGlyph (" xqlkmjwvtun~" !! (fromEnum ct + 1))
    BoxDouble -> "\x2551\x2550\x2554\x2557\x255a\x255d\x2566\x2569\x2560\x2563\x256c\x00b7"
        !! fromEnum ct
    BoxHeavy -> "\x2503\x2501\x250f\x2513\x2517\x251b\x2533\x253b\x2523\x252b\x254b\x00b7"
        !! fromEnum ct
    BoxRounded -> "\x2502\x2500\x256d\x256e\x2570\x256f\x2533\x253b\x2524\x251c\x254b\x00b7"
        !! fromEnum ct

-- | Rows of cells: the private screen a menu or popup draws into.
type Grid = V.Vector (V.Vector Cell.Cell)

-- | A cleared screen of @w@ by @h@.
blankGrid :: Int -> Int -> Grid
blankGrid w h = V.replicate (max 0 h) (V.replicate (max 0 w) Cell.blankCell)

-- | One cell written, if it is on the grid.
setCell :: Int -> Int -> Cell.Cell -> Grid -> Grid
setCell x y c g = case g V.!? y of
    Just row | x >= 0, x < V.length row -> g V.// [(y, row V.// [(x, c)])]
    _ -> g

-- | tmux's @screen_write_box@: a @w@ by @h@ frame at the grid's origin in
-- @sty@, its title drawn from the third cell of the top edge.
drawBox :: Theme -> BoxLines -> Cell.Style -> Text -> Int -> Int -> Grid -> Grid
drawBox th lns sty title w h g0 = drawFormatAt th sty title 2 0 (w - 4) withSides
  where
    glyph ct = Cell.glyphCell (boxGlyph lns ct) sty
    edge y l r g = foldr (\x -> setCell x y (glyph (pick x))) g [0 .. w - 1]
      where
        pick x
            | x == 0 = l
            | x == w - 1 = r
            | otherwise = CellLR
    withEdges = edge (h - 1) CellRU CellLU (edge 0 CellRD CellLD g0)
    withSides = foldr (\y -> setCell 0 y (glyph CellUD) . setCell (w - 1) y (glyph CellUD))
        withEdges [1 .. h - 2]

-- | tmux's @screen_write_hline@ with both ends joined: a separator across
-- row @y@, @w@ cells wide.
drawHLine :: BoxLines -> Cell.Style -> Int -> Int -> Grid -> Grid
drawHLine lns sty w y g = foldr (\x -> setCell x y (Cell.glyphCell (boxGlyph lns (pick x)) sty)) g
    [0 .. w - 1]
  where
    pick x
        | x == 0 = CellURD
        | x == w - 1 = CellULD
        | otherwise = CellLR

-- | tmux's @format_draw@ at a cursor: an expanded format drawn over the
-- @width@ cells of row @y@ from column @x@, in @base@.
drawFormatAt :: Theme -> Cell.Style -> Text -> Int -> Int -> Int -> Grid -> Grid
drawFormatAt th base expanded x y width g = case g V.!? y of
    Just row | width > 0, x >= 0, x < V.length row ->
        let n = min width (V.length row - x)
            drawn = formatDraw th base expanded (V.slice x n row)
        in g V.// [(y, V.take x row <> drawn <> V.drop (x + n) row)]
    _ -> g
