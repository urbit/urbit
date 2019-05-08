module Main where

import Prelude
import Control.Lens

import Data.LargeWord (Word128, LargeKey(..))

import qualified Urbit.Behn as Behn
import qualified Urbit.Time as Time

--------------------------------------------------------------------------------

bench :: Behn.Behn -> IO (Time.Wen, Time.Wen, Time.Wen)
bench behn = do
  now  <- Time.now

  print (now ^. Time.wenUtcTime)

  Behn.doze behn (Just (Time.addGap now (500 ^. from Time.milliSecs)))

  wen <- Behn.wait behn
  aft <- Time.now

  pure (now, wen, aft)

main :: IO ()
main = do
  behn <- Behn.init

  (x1,y1,z1) <- bench behn
  (x2,y2,z2) <- bench behn
  (x3,y3,z3) <- bench behn

  putStrLn "----"

  print (x1 ^. Time.wenUtcTime)
  print (Time.gap x1 y1 ^. Time.milliSecs)
  print (y1 ^. Time.wenUtcTime)
  print (Time.gap y1 z1 ^. Time.milliSecs)
  print (z1 ^. Time.wenUtcTime)

  putStrLn "----"

  print (x2 ^. Time.wenUtcTime)
  print (Time.gap x2 y2 ^. Time.milliSecs)
  print (y2 ^. Time.wenUtcTime)
  print (Time.gap y2 z2 ^. Time.milliSecs)
  print (z2 ^. Time.wenUtcTime)

  putStrLn "----"

  print (x3 ^. Time.wenUtcTime)
  print (Time.gap x3 y3 ^. Time.milliSecs)
  print (y3 ^. Time.wenUtcTime)
  print (Time.gap y3 z3 ^. Time.milliSecs)
  print (z3 ^. Time.wenUtcTime)
