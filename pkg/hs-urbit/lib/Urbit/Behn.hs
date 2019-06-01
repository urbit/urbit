{-
  # Behn

  This provides a timer. To use this,

  - Create a new timer with `init`.
  - Use `doze` to start the timer.
  - Call `wait` to wait until the timer fires.

  Then, `wait` will return when the specified time has come.

  - If the specified time was in the past, `wait` will return immediately.
  - If a timer is set again, the old timer will not fire. The new time
    replaces the old one.
  - If a timer is unset (with `doze _ Nothing`), the timer will not fire
    until a new time has been set.
-}

module Urbit.Behn (Behn, init, wait, doze) where

import Prelude hiding (init)
import Control.Lens

import Control.Concurrent.MVar (MVar, takeMVar, newEmptyMVar, putMVar)
import Control.Monad           (void, when)
import Data.IORef              (IORef, writeIORef, readIORef, newIORef)

import qualified Urbit.Timer as Timer
import qualified Urbit.Time  as Time
import qualified GHC.Event   as Ev


-- Behn Stuff ------------------------------------------------------------------

data Behn = Behn
  { bTimer  :: Timer.Timer
  , bSignal :: MVar ()
  }

init :: IO Behn
init = do
  tim <- Timer.init
  sig <- newEmptyMVar
  pure (Behn tim sig)

wait :: Behn -> IO ()
wait (Behn _ sig) = takeMVar sig

doze :: Behn -> Maybe Time.Wen -> IO ()
doze behn Nothing  = Timer.stop (bTimer behn)
doze (Behn tim sig) (Just t) =
  Timer.start tim (t ^. Time.systemTime) (putMVar sig ())
