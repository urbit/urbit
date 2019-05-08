module Main where

import Prelude
import Control.Lens

import Control.Concurrent.MVar
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad  (replicateM_, when)
import Data.LargeWord (Word128, LargeKey(..))

import qualified Urbit.Behn as Behn
import qualified Urbit.Time as Time

--------------------------------------------------------------------------------

threadDelayBench :: IO Time.Gap
threadDelayBench = do
  before <- Time.now
  mvar :: MVar () <- newEmptyMVar
  forkIO $ do threadDelay 100
              putMVar mvar ()
  takeMVar mvar
  after  <- Time.now
  pure (Time.gap before after)

simpleBench :: Behn.Behn -> IO Time.Gap
simpleBench behn = do
  before <- Time.now
  target <- pure (Time.addGap before (10 ^. from Time.milliSecs))
  _      <- Behn.doze behn (Just target)
  after  <- Behn.wait behn >> Time.now

  pure (Time.gap target after)

bench :: Behn.Behn -> IO (Time.Wen, Time.Wen, Time.Wen)
bench behn = do
  now  <- Time.now

  print (now ^. Time.wenUtcTime)

  Behn.doze behn (Just (Time.addGap now (2 ^. from Time.milliSecs)))

  wen <- Behn.wait behn
  aft <- Time.now

  pure (now, wen, aft)

main :: IO ()
main = do
  behn <- Behn.init

  replicateM_ 5 (threadDelayBench >>= (print . view Time.microSecs))
  putStrLn "</threadDelayBench>"

  replicateM_ 5 (simpleBench behn >>= (print . view Time.microSecs))

  (x1,y1,z1) <- bench behn
  (x2,y2,z2) <- bench behn
  (x3,y3,z3) <- bench behn

  putStrLn "----"

  print (Time.gap y1 z1 ^. Time.milliSecs)
  print (Time.gap y2 z2 ^. Time.milliSecs)
  print (Time.gap y3 z3 ^. Time.milliSecs)
