-- TODO: Make sure transaction closed in all error cases
module Vere.Log where

import ClassyPrelude
import Data.Noun
import Data.Noun.Atom
import Data.Noun.Jam
import Data.Noun.Pill
import Data.Void
import Database.LMDB.Raw
import Foreign.Ptr
import Foreign.Marshal.Alloc

import Foreign.Storable (peek, poke, sizeOf)

import qualified Data.ByteString.Unsafe as BU
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

--------------------------------------------------------------------------------

-- TODO: We are uncertain about q's type. There's some serious entanglement
-- with u3_pier in this logic in the C code, and you might not be able to get
-- away with anything less than passing the full u3_writ around.
data State = State
  { env :: MDB_env
  , q   :: TQueue (Word64,Atom,Noun)
  }

data LogIdentity = LogIdentity
  { who     :: Noun
  , is_fake :: Noun
  , life    :: Noun
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
  BU.unsafeUseAsCStringLen bs \(ptr,sz) ->
    k (MDB_val (fromIntegral sz) (castPtr ptr))

mdbValToAtom :: MDB_val -> IO Atom
mdbValToAtom (MDB_val sz ptr) = do
  packAtom <$> BU.unsafePackCStringLen (castPtr ptr, fromIntegral sz)

mdbValToNoun :: MDB_val -> IO Noun
mdbValToNoun (MDB_val sz ptr) = do
  bs <- BU.unsafePackCStringLen (castPtr ptr, fromIntegral sz)
  maybe (error "mdb bad cue") pure (cue (packAtom bs))

putRaw :: MDB_WriteFlags -> MDB_txn -> MDB_dbi -> ByteString -> ByteString
       -> IO Bool
putRaw flags txn db key val =
  byteStringAsMdbVal key \mKey ->
  byteStringAsMdbVal val \mVal ->
  mdb_put flags txn db mKey mVal

put :: MDB_WriteFlags -> MDB_txn -> MDB_dbi -> ByteString -> Noun -> IO ()
put flags txn db bsKey val =
  putRaw flags txn db bsKey bsVal >>= \case
    True  -> pure ()
    False -> error "mdb bad put"
  where bsVal = nounToBs val

get :: MDB_txn -> MDB_dbi -> ByteString -> IO Noun
get txn db key =
  byteStringAsMdbVal key \mKey ->
  mdb_get txn db mKey >>= maybe (error "mdb bad get") mdbValToNoun

mdbValToWord64 :: MDB_val -> IO Word64
mdbValToWord64 (MDB_val sz ptr) = do
  assertErr (sz == 8) "wrong size in mdbValToWord64"
  peek (castPtr ptr)

--------------------------------------------------------------------------------

withWordPtr :: Word64 -> (Ptr Word64 -> IO a) -> IO a
withWordPtr w cb = do
  allocaBytes (sizeOf w) (\p -> poke p w >> cb p)


writeEvent :: State -> Word64 -> Atom -> Noun -> IO ()
writeEvent s id event effect = atomically $
  writeTQueue (q s) (id, event, effect)


-- TODO: This will read len items and will error if there are less than that
-- available. This differs from the current pier.c's expectations.
readEvents :: MDB_env -> Word64 -> Word64 -> IO (V.Vector (Word64,Atom))
readEvents env first len =
  withWordPtr first $ \pIdx ->
  withKVPtrs (MDB_val 64 (castPtr pIdx)) (MDB_val 0 nullPtr) $ \pKey pVal ->
  do
    txn <- mdb_txn_begin env Nothing True
    db  <- mdb_dbi_open txn (Just "EVENTS") [MDB_CREATE, MDB_INTEGERKEY]
    cur <- mdb_cursor_open txn db

    found <- mdb_cursor_get MDB_SET_KEY cur pKey pVal
    assertErr found "mdb could not read initial event of sequence"

    vec <- V.generateM (int len) \i -> do
      key <- peek pKey >>= mdbValToWord64
      val <- peek pVal >>= mdbValToAtom

      let idx = first + (fromIntegral i)

      assertErr (key /= idx) "missing event in database"

      found <- mdb_cursor_get MDB_NEXT cur pKey pVal
      assertErr found "lmdb: next event not found"
      pure (idx, val)

    mdb_cursor_close cur
    mdb_txn_abort txn

    pure vec


int :: Word64 -> Int
int = fromIntegral

assertErr :: Bool -> String -> IO ()
assertErr True  _ = pure ()
assertErr False m = error m

latestEventNumber :: MDB_env -> IO Word64
latestEventNumber env =
  do
    txn <- mdb_txn_begin env Nothing False
    db  <- mdb_dbi_open txn (Just "EVENTS") [MDB_CREATE, MDB_INTEGERKEY]
    cur <- mdb_cursor_open txn db
    res <- fetch txn db cur
    mdb_cursor_close cur
    mdb_txn_abort txn
    pure res
  where
    key = MDB_val 0 nullPtr
    val = MDB_val 0 nullPtr
    fetch txn db cur =
      withKVPtrs key val $ \pKey pVal ->
        mdb_cursor_get MDB_LAST cur pKey pVal >>= \case
          False -> pure 0
          True -> peek pKey >>= mdbValToWord64


--------------------------------------------------------------------------------

writeLogIdentity :: MDB_env -> LogIdentity -> IO ()
writeLogIdentity env LogIdentity{..} = do
  txn <- mdb_txn_begin env Nothing False
  db  <- mdb_dbi_open txn (Just "META") []
  let flags = compileWriteFlags []
  put flags txn db "who" who
  put flags txn db "is-fake" is_fake
  put flags txn db "life" life
  mdb_txn_commit txn
  pure ()

readLogIdentity :: MDB_env -> IO LogIdentity
readLogIdentity env = do
  txn <- mdb_txn_begin env Nothing True
  db  <- mdb_dbi_open txn (Just "META") []
  who <- get txn db "who"
  is_fake <- get txn db "is-fake"
  life <- get txn db "life"
  mdb_txn_abort txn
  pure (LogIdentity who is_fake life)
