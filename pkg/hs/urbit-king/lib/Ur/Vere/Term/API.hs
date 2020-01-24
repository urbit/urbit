{-|
    Interface Terminal API.
-}
module Ur.Vere.Term.API (Ev(..), Client(..), trace, spin, stopSpin) where

import Ur.Prelude hiding (trace)

import Ur.Arvo (Belt, Blit)


-- External Types --------------------------------------------------------------

{-|
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
    { take :: STM (Maybe Belt)
    , give :: [Ev] -> STM ()
    }

deriveNoun ''Ev


-- Utilities -------------------------------------------------------------------

trace :: Client -> Text -> STM ()
trace ts = give ts . singleton . Trace . Cord

spin :: Client -> Maybe Text -> STM ()
spin ts = give ts . singleton . Spinr . Just . fmap Cord

stopSpin :: Client -> STM ()
stopSpin ts = give ts [Spinr Nothing]
