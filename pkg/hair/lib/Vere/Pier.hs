module Vere.Pier where

import ClassyPrelude
import Vere.Pier.Types
import qualified Vere.Log as Log

initPier :: FilePath -> IO Pier
initPier top = do
  let logPath = top <> "/log"

  computeQueue <- newTQueueIO
  persistQueue <- newTQueueIO
  releaseQueue <- newTQueueIO

  logState <- Log.init logPath persistQueue (writeTQueue releaseQueue)

  pure (Pier{..})
