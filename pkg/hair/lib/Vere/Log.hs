-- TODO: Make sure transaction closed in all error cases
-- TODO: Don't allow writing non-contiguous events
module Vere.Log (
  init,
  shutdown,
  -- we don't export write; you use the queue
  readEvents,
  latestEventNumber,
  readLogIdentity,
  writeLogIdentity
) where

import ClassyPrelude hiding (init)
import Data.Noun
import Data.Noun.Atom
import Data.Noun.Jam
import Data.Noun.Pill
import Data.Void
import Database.LMDB.Raw
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Vere.Pier.Types

import Foreign.Storable (peek, poke, sizeOf)

import qualified Data.ByteString.Unsafe as BU
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

--------------------------------------------------------------------------------

-- TODO: Handle throws on the async
init :: FilePath -> TQueue (Writ [Effect]) -> (Writ [Effect] -> STM ())
     -> IO LogState
init dir inp cb = do
  env <- mdb_env_create
  mdb_env_set_maxdbs env 3
  mdb_env_set_mapsize env (40 * 1024 * 1024 * 1024)
  mdb_env_open env dir []
  writer <- persistThread env inp cb
  pure (LogState env inp cb writer)

-- TODO: properly handle shutdowns during write
shutdown :: LogState -> IO ()
shutdown s = do
  void $ waitCancel (writer s)
  mdb_env_close (env s)


waitCancel :: Async a -> IO (Either SomeException a)
waitCancel async = cancel async >> waitCatch async

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

putRaw :: MDB_WriteFlags -> MDB_txn -> MDB_dbi -> MDB_val -> MDB_val -> IO ()
putRaw flags txn db key val =
  mdb_put flags txn db key val >>= \case
    True  -> pure ()
    False -> error "mdb bad put"

putNoun :: MDB_WriteFlags -> MDB_txn -> MDB_dbi -> ByteString -> Noun -> IO ()
putNoun flags txn db key val =
  byteStringAsMdbVal key $ \mKey ->
  byteStringAsMdbVal (nounToBs val) $ \mVal ->
  putRaw flags txn db mKey mVal

putJam :: MDB_WriteFlags -> MDB_txn -> MDB_dbi -> Word64 -> Jam -> IO ()
putJam  flags txn db id (Jam atom) =
  withWord64AsMDBval id $ \idVal ->
  byteStringAsMdbVal (unpackAtom atom) $ \mVal ->
  putRaw flags txn db idVal mVal

get :: MDB_txn -> MDB_dbi -> ByteString -> IO Noun
get txn db key =
  byteStringAsMdbVal key \mKey ->
  mdb_get txn db mKey >>= maybe (error "mdb bad get") mdbValToNoun

mdbValToWord64 :: MDB_val -> IO Word64
mdbValToWord64 (MDB_val sz ptr) = do
  assertErr (sz == 8) "wrong size in mdbValToWord64"
  peek (castPtr ptr)

withWord64AsMDBval :: Word64 -> (MDB_val -> IO a) -> IO a
withWord64AsMDBval w cb = do
  withWordPtr w $ \p ->
    cb (MDB_val (fromIntegral (sizeOf w)) (castPtr p))

--------------------------------------------------------------------------------

withWordPtr :: Word64 -> (Ptr Word64 -> IO a) -> IO a
withWordPtr w cb = do
  allocaBytes (sizeOf w) (\p -> poke p w >> cb p)

-- TODO: We need to be able to send back an exception to the main thread on an
-- exception on the persistence thread.
persistThread :: MDB_env
              -> TQueue (Writ [Effect])
              -> (Writ [Effect] -> STM ())
              -> IO (Async ())
persistThread env inputQueue onPersist = async $
  do
    writs <- atomically $ readQueue inputQueue
    writeEvents writs
    atomically $ traverse_ onPersist writs
  where
    writeEvents writs = do
      txn <- mdb_txn_begin env Nothing False
      db  <- mdb_dbi_open txn (Just "EVENTS") [MDB_CREATE, MDB_INTEGERKEY]

      let flags = compileWriteFlags [MDB_NOOVERWRITE]

      for_ writs $ \w ->
        putJam flags txn db (eventId w) (event w)

      mdb_txn_commit txn

deriving instance Show MDB_val

-- TODO: This will read len items and will error if there are less than that
-- available. This differs from the current pier.c's expectations.
readEvents :: LogState -> Word64 -> Word64 -> IO (V.Vector (Word64,Atom))
readEvents (LogState env _ _ _) first len =
  withWordPtr first $ \pIdx ->
  withKVPtrs (MDB_val 8 (castPtr pIdx)) (MDB_val 0 nullPtr) $ \pKey pVal ->
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

      assertErr (key == idx) ("missing event in database " <> (show idx))

      when (i + 1 /= (int len)) do
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

latestEventNumber :: LogState -> IO Word64
latestEventNumber (LogState env _ _ _) =
  do
    txn <- mdb_txn_begin env Nothing True
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

readLogIdentity :: LogState -> IO LogIdentity
readLogIdentity (LogState env _ _ _) = do
  txn <- mdb_txn_begin env Nothing True
  db  <- mdb_dbi_open txn (Just "META") []
  who <- get txn db "who"
  is_fake <- get txn db "is-fake"
  life <- get txn db "life"
  mdb_txn_abort txn
  pure (LogIdentity who is_fake life)

writeLogIdentity :: LogState -> LogIdentity -> IO ()
writeLogIdentity (LogState env _ _ _) LogIdentity{..} = do
  txn <- mdb_txn_begin env Nothing False
  db  <- mdb_dbi_open txn (Just "META") []
  let flags = compileWriteFlags []
  putNoun flags txn db "who" who
  putNoun flags txn db "is-fake" is_fake
  putNoun flags txn db "life" life
  mdb_txn_commit txn
  pure ()
