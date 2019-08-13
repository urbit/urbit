module TryTimers where

{-
import Prelude
import Control.Lens

import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)
import Control.Concurrent      (threadDelay, forkIO)
import Control.Monad           (replicateM_, when)

import qualified Urbit.Timer            as Timer
import qualified Urbit.Behn             as Behn
import qualified Urbit.Time             as Time
import qualified Data.Time.Clock.System as Sys


--------------------------------------------------------------------------------

benchTimer :: Timer.Timer -> IO ()
benchTimer timer = do
  now <- Sys.getSystemTime
  let wen = case now of Sys.MkSystemTime s ns ->
                          Sys.MkSystemTime s (ns + 5_000_000)
  v <- newEmptyMVar
  Timer.start timer wen (putMVar v ())
  takeMVar v
  end <- Timer.getSystemTime
  print (Timer.sysTimeGapMicroSecs wen end)

bench :: Behn.Behn -> IO ()
bench behn = do
  now  <- Time.now
  let wen = Time.addGap now (5 ^. from Time.milliSecs)
  Behn.doze behn (Just wen)
  ()  <- Behn.wait behn
  aft <- Time.now
  print (Time.gap wen aft ^. Time.microSecs)

main :: IO ()
main = do
  behn  <- Behn.init
  timer <- Timer.init

  putStrLn "<benchTimer>"
  replicateM_ 10 (benchTimer timer)
  putStrLn "</benchTimer>"

  putStrLn "<bench>"
  replicateM_ 10 (bench behn)
  putStrLn "</bench>"
-}
