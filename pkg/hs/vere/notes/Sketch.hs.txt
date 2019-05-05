module Main where

import ClassyPrelude hiding (atomically, newTVarIO)
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue

--------------------------------------------------------------------------------

newtype Cpu st ev fx = Cpu { runCpu :: st -> ev -> (st, fx) }

data CpuApi ev st fx = CpuApi
    { caHalt   :: IO ()
    , caInput  :: TBMQueue ev
    , caOutput :: TBMQueue (st, fx)
    }

--------------------------------------------------------------------------------

dummyCpu :: Cpu () () ()
dummyCpu = Cpu $ (\() () -> ((), ()))

runCpuIO :: Cpu st ev fx
         -> TVar st
         -> TBMQueue ev
         -> TBMQueue (st, fx)
         -> IO ()
runCpuIO cpu vSt inp out =
    forever $ atomically $ do
        ev <- readTBMQueue inp >>= maybe (error "No more input") pure
        st <- readTVar vSt
        runCpu cpu st ev & \(st', fx) -> do
            writeTVar vSt st'
            writeTBMQueue out (st', fx)

runCpuThread :: Cpu st ev fx
             -> st
             -> IO (CpuApi ev st fx)
runCpuThread cpu init = do
    inp  <- newTBMQueueIO 1
    out  <- newTBMQueueIO 16
    vSt  <- newTVarIO init
    tid  <- forkIO (runCpuIO cpu vSt inp out)

    let kill = do atomically (closeTBMQueue inp >> closeTBMQueue out)
                  killThread tid

    pure (CpuApi kill inp out)

--------------------------------------------------------------------------------

{-
  - When an event comes in:
    - process the event
    - persist the event
    - run the effects

  - Take a snapshot at any time.
-}

main :: IO ()
main = do
    cpuProc <- runCpuThread dummyCpu ()
    caHalt cpuProc
