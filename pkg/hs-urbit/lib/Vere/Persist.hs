{-
    TODO Close the database on uncaught exception.
    TODO `Persist` should just be the thread id.
      the thread should close the database when it is killed.
-}

module Vere.Persist (start, stop) where

import ClassyPrelude hiding (init)

import Database.LMDB.Raw
import Vere.Log
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
    close log


-- Persist Thread --------------------------------------------------------------

-- TODO: We need to be able to send back an exception to the main thread on an
-- exception on the persistence thread.
persistThread :: EventLog
              -> TQueue (Writ [Eff])
              -> (Writ [Eff] -> STM ())
              -> IO ()
persistThread (EventLog env) inputQueue onPersist =
  forever $ do
    writs <- atomically $ readQueue inputQueue
    writeEvents writs
    atomically $ traverse_ onPersist writs
  where
    writeEvents writs = do
      txn <- mdb_txn_begin env Nothing False
      db  <- mdb_dbi_open txn (Just "EVENTS") [MDB_CREATE, MDB_INTEGERKEY]

      let flags = compileWriteFlags [MDB_NOOVERWRITE]

      for_ writs $ \w -> do
        putJam flags txn db (eventId w) (event w)

      mdb_txn_commit txn


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
