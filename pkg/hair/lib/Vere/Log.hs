module Vere.Log where

import Database.LMDB.Raw
import ClassyPrelude
import Data.Void
import Data.ByteString.Unsafe
import GHC.Ptr (castPtr)

--------------------------------------------------------------------------------

data State = State
  { env :: MDB_env
  , q   :: TQueue Void
  }

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

byteStringAsMdbVal :: ByteString -> (MDB_val -> IO a) -> IO a
byteStringAsMdbVal bs k =
  unsafeUseAsCStringLen bs \(ptr,sz) ->
    k (MDB_val (fromIntegral sz) (castPtr ptr))

putRaw :: MDB_WriteFlags -> MDB_txn -> MDB_dbi -> ByteString -> ByteString
       -> IO Bool
putRaw flags txn db key val =
  byteStringAsMdbVal key \mKey ->
  byteStringAsMdbVal val \mVal ->
  mdb_put flags txn db mKey mVal
