module Vere.TestReadPier where

import ClassyPrelude
import Data.Noun.Jam
import qualified Vere.Log as Log

main :: IO ()
main = do
  let logPath = "/Users/erg/src/urbit/zod/.urb/log/"

  -- junk
  persistQueue <- newTQueueIO
  releaseQueue <- newTQueueIO
  logState <- Log.init logPath persistQueue (writeTQueue releaseQueue)

  -- 
  log <- Log.readLogIdentity logState
  print log

  --
  latestEvent <- Log.latestEventNumber logState
  print latestEvent

  --
  events <- Log.readEvents logState 1000 1
  print $ cue . snd <$> events

