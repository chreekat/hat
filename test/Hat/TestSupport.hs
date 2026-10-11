-- Helpers shared by more than one spec.
module Hat.TestSupport (bashPath, nodeOf) where

import Data.Maybe (fromMaybe)
import System.Directory (findExecutable)

import Hat.Geometry (Size (..))
import Hat.Server.Layout (Cell (..), Layout (..), Orientation (..), fitLayout, fixOffsets)

-- The dev shell's bash, for tests that need a bashism (@exec -a@). Not
-- /bin/sh: that is bash only on NixOS, and dash elsewhere.
bashPath :: IO FilePath
bashPath = fromMaybe (error "bash is not on PATH") <$> findExecutable "bash"

-- | A layout node over the given children at the smallest size that holds
-- them; laying it out ('fitLayout', 'Hat.Server.Layout.arrange') spreads it
-- over a real window, roughly evenly.
nodeOf :: Orientation -> [Layout] -> Layout
nodeOf o cs = fixOffsets (Layout w h 0 0 (NodeCell o cs'))
  where
    across = case o of
        LeftRight -> maximum (map (.sy) cs)
        TopBottom -> maximum (map (.sx) cs)
    cs' = [ case o of
                LeftRight -> fitLayout (size c.sx across) c
                TopBottom -> fitLayout (size across c.sy) c
          | c <- cs ]
    along = sum [ case o of LeftRight -> c.sx; TopBottom -> c.sy | c <- cs' ]
        + length cs' - 1
    (w, h) = case o of
        LeftRight -> (along, across)
        TopBottom -> (across, along)
    size cols rows = Size { rows = fromIntegral rows, cols = fromIntegral cols }
