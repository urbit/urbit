{-
    TODO Close the database on uncaught exception.
    TODO `Persist` should just be the thread id.
      the thread should close the database when it is killed.
-}

module Vere.Persist (start, stop) where

import UrbitPrelude hiding (init)

import Vere.Log (EventLog)

import qualified Vere.Log as Log
import Vere.Pier.Types


-- Types -----------------------------------------------------------------------

data Persist = Persist EventLog (Async ())


-- Start and Stop --------------------------------------------------------------

start :: EventLog
      -> TQueue (Writ [Eff])
      -> (Writ [Eff] -> STM ())
      -> IO Persist
start log inp cb = do
    tid <- asyncBound (persistThread log inp cb)
    pure (Persist log tid)

-- TODO: properly handle shutdowns during write
stop :: Persist -> IO ()
stop (Persist log tid) = do
    void (cancel tid)
    void (waitCatch tid)


-- Persist Thread --------------------------------------------------------------

-- TODO: We need to be able to send back an exception to the main thread on an
-- exception on the persistence thread.
persistThread :: EventLog
              -> TQueue (Writ [Eff])
              -> (Writ [Eff] -> STM ())
              -> IO ()
persistThread log inputQueue onPersist =
    forever $ do
        writs  <- atomically $ fmap toNullable $ readQueue inputQueue
        events <- validateWrits writs
        Log.appendEvents log events
        atomically $ traverse_ onPersist writs
  where
    validateWrits :: [Writ [Eff]] -> IO (Vector Atom)
    validateWrits writs = do
        expect <- Log.nextEv log
        fmap fromList
            $ for (zip [expect..] writs)
            $ \(expectedId, Writ{..}) -> do
                guard (expectedId == eventId)
                pure (unJam event)


-- Get eventhing from the input queue. -----------------------------------------

{-
    Read one or more items from a TQueue, only blocking on the first item.
-}
readQueue :: TQueue a -> STM (NonNull [a])
readQueue q =
    readTQueue q >>= go . singleton
  where
    go acc =
      tryReadTQueue q >>= \case
        Nothing   -> pure (reverse acc)
        Just item -> go (item <| acc)
