module Urbit.Timer ( Timer(..), init, stop, start
                   , Sys.getSystemTime, sysTimeGapMicroSecs
                   ) where

import Data.IORef
import Prelude    hiding (init)

import qualified Data.Time.Clock.System as Sys
import qualified GHC.Event              as Ev


-- Timer Stuff -----------------------------------------------------------------

data Timer = Timer
  { bState   :: IORef (Maybe Ev.TimeoutKey)
  , bManager :: Ev.TimerManager
  }

init :: IO Timer
init = do
  st  <- newIORef Nothing
  man <- Ev.getSystemTimerManager
  pure (Timer st man)

sysTimeGapMicroSecs :: Sys.SystemTime -> Sys.SystemTime -> Int
sysTimeGapMicroSecs (Sys.MkSystemTime xSec xNs) (Sys.MkSystemTime ySec yNs) =
    (+) (1_000_000 * fromIntegral (ySec - xSec))
        ((fromIntegral yNs - fromIntegral xNs) `quot` 1000)

start :: Timer -> Sys.SystemTime -> IO () -> IO ()
start timer@(Timer vSt man) time cb = do
  let fire = cb >> stop timer
  stop timer
  now <- Sys.getSystemTime
  let sleep = sysTimeGapMicroSecs now time
  if (sleep <= 0) then fire else do
    key <- Ev.registerTimeout man sleep fire
    atomicWriteIORef vSt $! Just key

stop :: Timer -> IO ()
stop (Timer vSt man) =
  atomicModifyIORef' vSt (Nothing,) >>= \case
    Just key -> Ev.unregisterTimeout man key
    Nothing  -> pure ()
