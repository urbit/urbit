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

bench :: Behn.Behn -> IO ()
bench behn = do
  now  <- Time.now

  let wen = Time.addGap now (2 ^. from Time.milliSecs)
  Behn.doze behn (Just wen)

  ()  <- Behn.wait behn
  aft <- Time.now

  print (Time.gap wen aft ^. Time.milliSecs)

main :: IO ()
main = do
  behn <- Behn.init

  putStrLn "<bench>"
  replicateM_ 50 (bench behn)
  putStrLn "</bench>"
