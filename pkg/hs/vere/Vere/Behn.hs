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
-}

module Vere.Behn (Behn, init, wait, doze) where

import Control.Concurrent
import Control.Concurrent.Async hiding (wait)
import Control.Concurrent.MVar
import Data.LargeWord
import Prelude                  hiding (init)

import Data.Time.Clock.System (SystemTime(..), getSystemTime)
import Control.Lens  ((&))
import Control.Monad (void)


-- Time Stuff ------------------------------------------------------------------

type UrbitTime = Word128

urNow :: IO UrbitTime
urNow = systemTimeToUrbitTime <$> getSystemTime

{-
    TODO This is wrong.

    - The high word should be `(0x8000000cce9e0d80ULL + secs)`
    - The low word should be `(((usecs * 65536ULL) / 1000000ULL) << 48ULL)`
-}
systemTimeToUrbitTime :: SystemTime -> UrbitTime
systemTimeToUrbitTime (MkSystemTime secs ns) =
  LargeKey (fromIntegral secs) (fromIntegral ns)

-- TODO
urbitTimeToMicrosecs :: UrbitTime -> Int
urbitTimeToMicrosecs x = fromIntegral x

-- TODO Double Check this
diffTime :: UrbitTime -> UrbitTime -> UrbitTime
diffTime fst snd | fst >= snd = 0
                 | otherwise  = snd - fst

-- Behn Stuff ------------------------------------------------------------------

data Behn = Behn
  { bState  :: MVar (Maybe (UrbitTime, Async ()))
  , bSignal :: MVar UrbitTime
  }

init :: IO Behn
init = do
  st  <- newMVar Nothing
  sig <- newEmptyMVar
  pure (Behn st sig)

wait :: Behn -> IO UrbitTime
wait (Behn _ sig) = takeMVar sig

startTimerThread :: Behn -> UrbitTime -> IO (Async ())
startTimerThread (Behn vSt sig) time =
  async $ do
    now <- urNow
    threadDelay (urbitTimeToMicrosecs (now `diffTime` time))
    void (swapMVar vSt Nothing >> tryPutMVar sig time)

doze :: Behn -> Maybe UrbitTime -> IO ()
doze behn@(Behn vSt sig) mNewTime = do
  takeMVar vSt >>= \case Nothing        -> pure ()
                         Just (_,timer) -> cancel timer

  newSt <- mNewTime & \case
    Nothing   -> pure (Nothing :: Maybe (UrbitTime, Async ()))
    Just time -> do timer <- startTimerThread behn time
                    pure (Just (time, timer))

  void (putMVar vSt newSt)
