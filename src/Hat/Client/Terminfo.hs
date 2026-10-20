{-# OPTIONS_GHC -Wno-deprecations #-}  -- tiGetStr: the raw string is what tmux reads; tiGetOutput only renders it

-- | The client's half of tmux's terminal model: reading its @TERM@'s
-- terminfo entry. See "Hat.Term.Caps".
module Hat.Client.Terminfo
    ( readTermCaps
    ) where

import Control.Exception (try)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Console.Terminfo.Base
    (SetupTermError, getCapability, setupTerm, tiGetFlag, tiGetNum, tiGetStr)

import Hat.Term.Caps (CapType (..), capCodes)

-- | Every capability tmux knows that the terminal's terminfo entry has, as
-- @name=value@ (tmux's @tty_term_read_list@): numbers in decimal, set
-- flags as @1@. A flag the entry leaves unset is omitted, since terminfo
-- does not tell it from one it lacks.
readTermCaps :: Text -> IO (Either Text [Text])
readTermCaps nm = do
    r <- try (setupTerm (T.unpack nm))
    pure $ case r of
        Left (_ :: SetupTermError) -> Left ("missing or unsuitable terminal: " <> nm)
        Right t -> Right (mapMaybe (entry t) capCodes)
  where
    entry t (n, ty) =
        let key = T.unpack n
            named v = n <> "=" <> v
        in case ty of
            CapString -> named . T.pack <$> getCapability t (tiGetStr key)
            CapNumber -> named . T.pack . show <$> getCapability t (tiGetNum key)
            CapFlag -> case getCapability t (tiGetFlag key) of
                Just True -> Just (named "1")
                _ -> Nothing
