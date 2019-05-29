module Vere.Log where

import Database.LMDB.Raw
import ClassyPrelude hiding ((<|))
import Data.Void
import Data.ByteString.Unsafe
import GHC.Ptr (castPtr)
import Data.List.NonEmpty (NonEmpty(..), (<|))

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

readQueue :: TQueue a -> STM (NonEmpty a)
readQueue q =
    readTQueue q >>= go . singleton
  where
    go acc = tryReadTQueue q >>= \case
      Nothing   -> pure (reverse acc)
      Just item -> go (item <| acc)

byteStringAsMdbVal :: ByteString -> (MDB_val -> IO a) -> IO a
byteStringAsMdbVal bs k =
  unsafeUseAsCStringLen bs \(ptr,sz) ->
    k (MDB_val (fromIntegral sz) (castPtr ptr))

put :: MDB_WriteFlags -> MDB_txn -> MDB_dbi -> ByteString -> ByteString
    -> IO Bool
put flags txn db key val =
  byteStringAsMdbVal key \mKey ->
  byteStringAsMdbVal val \mVal ->
  mdb_put flags txn db mKey mVal
