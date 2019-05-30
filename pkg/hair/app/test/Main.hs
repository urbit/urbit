module Main where

import ClassyPrelude
import Vere.Pier.Types
import Data.Noun.Jam hiding (main)
import qualified Vere.Log as Log

main :: IO ()
main = do
  let logPath = "/Users/erg/src/urbit/zod/.urb/log/"
      falselogPath = "/Users/erg/src/urbit/zod/.urb/falselog/"

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
  events <- Log.readEvents logState 30 3000
  --print $ cue . snd <$> events

  --
  persistQueue2 <- newTQueueIO
  releaseQueue2 <- newTQueueIO
  falseLogState <- Log.init falselogPath persistQueue2 (writeTQueue releaseQueue2)

  let writs = events <&> \(id, a) ->
        Writ id Nothing (Jam a) []

  print "About to write"
  for_ writs $ \w -> atomically $ writeTQueue persistQueue2 w

  print "About to wait"

  replicateM_ 100 $ atomically $ readTQueue releaseQueue2
  print "Done"
