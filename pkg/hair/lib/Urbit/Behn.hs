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

  ## Implementation Notes

  We use `tryPutMVar` when the timer fires, so that things will continue
  to work correctly if the user does not call `wait`. If a timer fires
  before `wait` is called, `wait` will return immediatly.

  To handle race conditions, the MVar in `bState` is used as a lock. The
  code for setting a timer and the thread that runs when the timer fires
  (which causes `wait` to return) both take that MVar before acting.

  So, if the timer fires conncurently with a call to `doze`,
  then one of those threads will get the lock and the other will wait:

  - If the `doze` call gets the lock first, it will kill the timer thread
    before releasing it.
  - If the timer gets the the lock first, it will fire (causeing `wait`
    to return) first, and then `doze` action will wait until that finishes.

  ## TODO

  `threadDelay` has low accuracy. Consider using
  `GHC.Event.registerTimeout` instead. It's API is very close to what
  we want for this anyways.
-}

module Urbit.Behn (Behn, init, wait, doze) where

import Prelude hiding (init)
import Control.Lens

import Data.LargeWord
import Control.Concurrent.MVar

import Control.Concurrent.Async (Async, async, cancel, asyncThreadId)
import Control.Concurrent       (threadDelay, killThread)
import Control.Monad            (void, when)
import Data.Time.Clock.System   (SystemTime(..), getSystemTime)
import Urbit.Time               (Wen)

import qualified Control.Concurrent.Async as Async
import qualified Urbit.Time               as Time


-- Behn Stuff ------------------------------------------------------------------

data Behn = Behn
  { bState  :: MVar (Maybe (Wen, Async ()))
  , bSignal :: MVar Wen
  }

init :: IO Behn
init = do
  st  <- newMVar Nothing
  sig <- newEmptyMVar
  pure (Behn st sig)

wait :: Behn -> IO Wen
wait (Behn _ sig) = takeMVar sig

startTimerThread :: Behn -> Wen -> IO (Async ())
startTimerThread (Behn vSt sig) time =
  async $ do
    now <- Time.now
    Time.sleepUntil time
    takeMVar vSt
    void $ tryPutMVar sig time
    putMVar vSt Nothing

doze :: Behn -> Maybe Wen -> IO ()
doze behn@(Behn vSt sig) mNewTime = do
  takeMVar vSt >>= \case Nothing        -> pure ()
                         Just (_,timer) -> cancel timer

  newSt <- mNewTime & \case
    Nothing   -> pure (Nothing :: Maybe (Wen, Async ()))
    Just time -> do timer <- startTimerThread behn time
                    pure (Just (time, timer))

  void (putMVar vSt newSt)
