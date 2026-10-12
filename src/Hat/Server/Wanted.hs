-- | Which format variables an environment is built with. Built by
-- 'Hat.Server.FormatEnv.onlyVars', which plans each part once.
module Hat.Server.Wanted
    ( Wanted (..)
    , WantedNames (..)
    ) where

import Data.Set (Set)
import Data.Text (Text)

data Wanted
    = AllVars
    | OnlyVars WantedNames  -- ^ these, where they exist, and no others

-- | Wanted variable names, with what building them takes worked out once
-- for every holder of the same set.
data WantedNames = WantedNames
    { names :: Set Text
    , optionNames :: [Text]
        -- ^ those an option could give ('Hat.Model.Options.optionFormat'); lazy
    , sessionGroups :: [Int]  -- ^ the session layer's groups giving any; lazy
    , windowGroups :: [Int]   -- ^ the window layer's; lazy
    , paneGroups :: [Int]     -- ^ the pane layer's; lazy
    , environNames :: [Text]
        -- ^ those nothing but an environment variable could give; lazy
    }
