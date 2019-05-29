module Vere.Log where

import Database.LMDB.Raw
import ClassyPrelude
import Data.Void
import Data.ByteString.Unsafe

data State = State
  { env :: MDB_env
  , q   :: TQueue Void
  }


init :: FilePath -> IO State
init dir = do
  env <- mdb_env_create
  mdb_env_set_maxdbs env 3
  mdb_env_set_mapsize env (40 * 1024 * 1024 * 1024)
  mdb_env_open env dir []
  tq <- newTQueueIO
  pure (State env tq)


shutdown :: State -> IO ()
shutdown s = mdb_env_close (env s)


readQueue :: TQueue a -> STM [a]
readQueue q = do
  first <- readTQueue q
  go [first]
  where
    go acc = tryReadTQueue q >>= \case
      Nothing -> pure (reverse acc)
      Just item -> go (item:acc)


-- put :: MDB_WriteFlags -> MDB_txn -> MDB_dbi -> ByteString -> ByteString -> IO ()
-- put flags txn db key val = do
--   unsafeUseAsCStringLen key $ \(pkey, skey) ->
--     unsafeUseAsCStringLen val $ \(pval, sval) -> do
--       let m_key = MDB_val skey pkey
--           m_val = MDB_val sval pval
--       success <- mdb_put flags txn db m_key m_val
--       pure ()
