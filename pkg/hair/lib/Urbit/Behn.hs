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

import qualified Urbit.Time as Time
import qualified GHC.Event  as Ev


-- Behn Stuff ------------------------------------------------------------------

data Behn = Behn
  { bState   :: IORef (Maybe Ev.TimeoutKey)
  , bSignal  :: MVar ()
  , bManager :: Ev.TimerManager
  }

init :: IO Behn
init = do
  st  <- newIORef Nothing
  sig <- newEmptyMVar
  man <- Ev.getSystemTimerManager
  pure (Behn st sig man)

wait :: Behn -> IO ()
wait (Behn _ sig _) = takeMVar sig

setTimer :: Behn -> Time.Wen -> IO ()
setTimer behn@(Behn vSt sig man) time = do
  killTimer behn
  now <- Time.now
  case (now >= time) of
      True  -> void (putMVar sig ())
      False -> do
          let microSleep = Time.gap now time ^. Time.microSecs
          let fire       = putMVar sig () >> killTimer behn
          key <- Ev.registerTimeout man microSleep fire
          writeIORef vSt $! Just key

killTimer :: Behn -> IO ()
killTimer (Behn vSt sig man) = do
    mKey <- do st <- readIORef vSt
               writeIORef vSt $! Nothing
               pure st
    mKey & \case
      Just k  -> Ev.unregisterTimeout man k
      Nothing -> pure ()

doze :: Behn -> Maybe Time.Wen -> IO ()
doze behn Nothing  = killTimer behn
doze behn (Just t) = setTimer behn t
