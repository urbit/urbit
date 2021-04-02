{-|
    Interface Terminal API.
-}
module Urbit.Vere.Term.API (Ev(..),
                            Client(..),
                            ClientTake(..),
                            trace,
                            slog,
                            spin,
                            stopSpin) where

import Urbit.Prelude hiding (trace)

import Urbit.Arvo (Belt, Blit)

import Control.Monad.Fail (fail)
import Urbit.TermSize

-- External Types --------------------------------------------------------------

{-|
    Input Event for terminal driver:

    %blits -- list of blits from arvo.
    %trace -- stderr line from runtime (without trailing newline).
    %slog  -- nock worker logging with priority
    %blank -- print a blank line
    %spinr -- Start or stop the spinner
-}
data Ev = Blits ![Blit]
        | Trace !Cord
        | Slog !(Atom, Tank)
        | Blank
        | Spinr !(Maybe (Maybe Cord))
  deriving (Show)

data ClientTake
  = ClientTakeBelt Belt
  | ClientTakeSize TermSize
  deriving (Show)

instance ToNoun ClientTake where
  toNoun = \case
    ClientTakeBelt b              -> toNoun $ (Cord "belt", b)
    ClientTakeSize (TermSize w h) -> toNoun $ (Cord "size", (w, h))

instance FromNoun ClientTake where
  parseNoun n = named "ClientTake" $ do
    (Cord name, rest) <- parseNoun n
    case name of
      "belt" -> do
        b <- parseNoun rest
        pure (ClientTakeBelt b)
      "size" -> do
        (w, h) <- parseNoun rest
        pure (ClientTakeSize (TermSize w h))
      _ -> fail "weird client take"


data Client = Client
    { take :: STM (Maybe ClientTake)
    , give :: [Ev] -> STM ()
    }

deriveNoun ''Ev


-- Utilities -------------------------------------------------------------------

trace :: Client -> Text -> STM ()
trace ts = give ts . singleton . Trace . Cord

slog :: Client -> (Atom, Tank) -> STM ()
slog ts = give ts . singleton . Slog

spin :: Client -> Maybe Text -> STM ()
spin ts = give ts . singleton . Spinr . Just . fmap Cord

stopSpin :: Client -> STM ()
stopSpin ts = give ts [Spinr Nothing]
