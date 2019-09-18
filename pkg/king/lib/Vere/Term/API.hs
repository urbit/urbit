module Vere.Term.API (Ev(..), Client(..), trace, spin, stopSpin) where

import UrbitPrelude hiding (trace)

import Arvo (Blit, Belt)


-- External Types --------------------------------------------------------------

{-
    Input Event for terminal driver:

    %blits -- list of blits from arvo.
    %trace -- stderr line from runtime.
    %blank -- print a blank line
    %spinr -- Start or stop the spinner
-}
data Ev = Blits [Blit]
        | Trace Cord
        | Blank
        | Spinr (Maybe (Maybe Cord))
  deriving (Show)

data Client = Client
    { take :: STM Belt
    , give :: Ev -> STM ()
    }

deriveNoun ''Ev


-- Utilities -------------------------------------------------------------------

trace :: Client -> Text -> STM ()
trace ts = give ts . Trace . Cord

spin :: Client -> Maybe Text -> STM ()
spin ts = give ts . Spinr . Just . fmap Cord

stopSpin :: Client -> STM ()
stopSpin ts = give ts (Spinr Nothing)
