module Main where

import ClassyPrelude
import Vere.Pier.Types
import Data.Noun.Jam hiding (main)
import qualified Vere.Log as Log

main :: IO ()
main = do
  let logPath = "/Users/erg/src/urbit/zod/.urb/falselog/"
      falselogPath = "/Users/erg/src/urbit/zod/.urb/falselog2/"

  -- junk
  persistQueue <- newTQueueIO
  releaseQueue <- newTQueueIO
  logState <- Log.init logPath persistQueue (writeTQueue releaseQueue)

  -- 
  logId <- Log.readLogIdentity logState
  print logId

  --
  latestEvent <- Log.latestEventNumber logState
  print latestEvent

  --
  events <- Log.readEvents logState 1 3142
  --print $ cue . snd <$> events

  --
  persistQueue2 <- newTQueueIO
  releaseQueue2 <- newTQueueIO
  falseLogState <- Log.init falselogPath persistQueue2 (writeTQueue releaseQueue2)

  Log.writeLogIdentity falseLogState logId

  let writs = events <&> \(id, a) ->
        Writ id Nothing (Jam a) []

  print "About to write"
  for_ writs $ \w -> atomically $ writeTQueue persistQueue2 w

  print "About to wait"

  replicateM_ 100 $ atomically $ readTQueue releaseQueue2
  print "Done"
