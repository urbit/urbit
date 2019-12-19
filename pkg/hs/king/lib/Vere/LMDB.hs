module Vere.LMDB where

import UrbitPrelude hiding (init)

import Data.RAcquire
-- import Data.Conduit
import Database.LMDB.Raw
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Vere.Pier.Types

import Foreign.Storable (peek, poke, sizeOf)

import qualified Data.ByteString.Unsafe as BU
-- import qualified Data.Vector            as V


-- Types -----------------------------------------------------------------------

type Env = MDB_env
type Val = MDB_val
type Txn = MDB_txn
type Dbi = MDB_dbi
type Cur = MDB_cursor

data VereLMDBExn
    = NoLogIdentity
    | MissingEvent EventId
    | BadNounInLogIdentity ByteString DecodeErr ByteString
    | BadKeyInEventLog
    | BadWriteLogIdentity LogIdentity
    | BadWriteEvent EventId
    | BadWriteEffect EventId
  deriving Show

instance Exception VereLMDBExn where


-- Transactions ----------------------------------------------------------------

{-
    A read-only transaction that commits at the end.

    Use this when opening database handles.
-}
openTxn :: Env -> RAcquire e Txn
openTxn env = mkRAcquire begin commit
  where
    begin  = io $ mdb_txn_begin env Nothing True
    commit = io . mdb_txn_commit

{-
    A read-only transaction that aborts at the end.

    Use this when reading data from already-opened databases.
-}
readTxn :: Env -> RAcquire e Txn
readTxn env = mkRAcquire begin abort
  where
    begin = io $ mdb_txn_begin env Nothing True
    abort = io . mdb_txn_abort

{-
    A read-write transaction that commits upon sucessful completion and
    aborts on exception.

    Use this when reading data from already-opened databases.
-}
writeTxn :: Env -> RAcquire e Txn
writeTxn env = mkRAcquireType begin finalize
  where
    begin = io $ mdb_txn_begin env Nothing False
    finalize txn = io . \case
        ReleaseNormal    -> mdb_txn_commit txn
        ReleaseEarly     -> mdb_txn_commit txn
        ReleaseException -> mdb_txn_abort  txn


-- Cursors ---------------------------------------------------------------------

cursor :: Txn -> Dbi -> RAcquire e Cur
cursor txn dbi = mkRAcquire open close
  where
    open  = io $ mdb_cursor_open txn dbi
    close = io . mdb_cursor_close


-- Last Key In Dbi -------------------------------------------------------------

lastKeyWord64 :: Env -> Dbi -> Txn -> RIO e Word64
lastKeyWord64 env dbi txn =
    rwith (cursor txn dbi) $ \cur ->
    withKVPtrs' nullVal nullVal $ \pKey pVal ->
    io $ mdb_cursor_get MDB_LAST cur pKey pVal >>= \case
        False -> pure 0
        True  -> peek pKey >>= mdbValToWord64


-- Delete Rows -----------------------------------------------------------------

deleteAllRows :: Env -> Dbi -> RIO e ()
deleteAllRows env dbi =
    rwith (writeTxn env) $ \txn ->
    rwith (cursor txn dbi) $ \cur ->
    withKVPtrs' nullVal nullVal $ \pKey pVal -> do
        let loop = io (mdb_cursor_get MDB_LAST cur pKey pVal) >>= \case
                False -> pure ()
                True  -> do io $ mdb_cursor_del (compileWriteFlags []) cur
                            loop
        loop

deleteRowsFrom :: HasLogFunc e => Env -> Dbi -> Word64 -> RIO e ()
deleteRowsFrom env dbi start = do
    rwith (writeTxn env) $ \txn -> do
        last <- lastKeyWord64 env dbi txn
        for_ [start..last] $ \eId -> do
            withWordPtr eId $ \pKey -> do
                let key = MDB_val 8 (castPtr pKey)
                found <- io $ mdb_del txn dbi key Nothing
                unless found $
                    throwIO (MissingEvent eId)


-- Append Rows to Sequence -----------------------------------------------------

{-
appendToSequence :: Env -> Dbi -> Vector ByteString -> RIO e ()
appendToSequence env dbi events = do
    numEvs <- readIORef (numEvents log)
    next   <- pure (numEvs + 1)
    doAppend $ zip [next..] $ toList events
    writeIORef (numEvents log) (numEvs + word (length events))
  where
    flags    = compileWriteFlags [MDB_NOOVERWRITE]
    doAppend = \kvs ->
        rwith (writeTxn env) $ \txn ->
        for_ kvs $ \(k,v) -> do
            putBytes flags txn dbi k v >>= \case
                True  -> pure ()
                False -> throwIO (BadWriteEvent k)
-}


-- Insert ----------------------------------------------------------------------

insertWord64 :: Env -> Dbi -> Word64 -> ByteString -> RIO e ()
insertWord64 env dbi k v = do
    rwith (writeTxn env) $ \txn ->
        putBytes flags txn dbi k v >>= \case
            True  -> pure ()
            False -> throwIO (BadWriteEffect k)
  where
    flags = compileWriteFlags []


{-
--------------------------------------------------------------------------------
-- Read Events -----------------------------------------------------------------

streamEvents :: HasLogFunc e
             => EventLog -> Word64
             -> ConduitT () ByteString (RIO e) ()
streamEvents log first = do
    last  <- lift $ lastEv log
    batch <- lift $ readBatch log first
    unless (null batch) $ do
        for_ batch yield
        streamEvents log (first + word (length batch))

streamEffectsRows :: ∀e. HasLogFunc e
                  => EventLog -> EventId
                  -> ConduitT () (Word64, ByteString) (RIO e) ()
streamEffectsRows log = go
  where
    go :: EventId -> ConduitT () (Word64, ByteString) (RIO e) ()
    go next = do
        batch <- lift $ readRowsBatch (env log) (effectsTbl log) next
        unless (null batch) $ do
            for_ batch yield
            go (next + fromIntegral (length batch))

{-
   Read 1000 rows from the events table, starting from event `first`.

   Throws `MissingEvent` if an event was missing from the log.
-}
readBatch :: EventLog -> Word64 -> RIO e (V.Vector ByteString)
readBatch log first = start
  where
    start = do
        last <- lastEv log
        if (first > last)
            then pure mempty
            else readRows $ fromIntegral $ min 1000 $ ((last+1) - first)

    assertFound :: EventId -> Bool -> RIO e ()
    assertFound id found = do
        unless found $ throwIO $ MissingEvent id

    readRows count =
        withWordPtr first $ \pIdx ->
        withKVPtrs' (MDB_val 8 (castPtr pIdx)) nullVal $ \pKey pVal ->
        rwith (readTxn $ env log) $ \txn ->
        rwith (cursor txn $ eventsTbl log) $ \cur -> do
            assertFound first =<< io (mdb_cursor_get MDB_SET_KEY cur pKey pVal)
            fetchRows count cur pKey pVal

    fetchRows count cur pKey pVal = do
        env <- ask
        V.generateM count $ \i -> runRIO env $ do
            key <- io $ peek pKey >>= mdbValToWord64
            val <- io $ peek pVal >>= mdbValToBytes
            idx <- pure (first + word i)
            unless (key == idx) $ throwIO $ MissingEvent idx
            when (count /= succ i) $ do
                assertFound idx =<< io (mdb_cursor_get MDB_NEXT cur pKey pVal)
            pure val

{-
   Read 1000 rows from the database, starting from key `first`.
-}
readRowsBatch :: ∀e. HasLogFunc e
              => Env -> Dbi -> Word64 -> RIO e (V.Vector (Word64, ByteString))
readRowsBatch env dbi first = readRows
  where
    readRows = do
        logDebug $ display ("(readRowsBatch) From: " <> tshow first)
        withWordPtr first $ \pIdx ->
            withKVPtrs' (MDB_val 8 (castPtr pIdx)) nullVal $ \pKey pVal ->
            rwith (readTxn env) $ \txn ->
            rwith (cursor txn dbi) $ \cur ->
            io (mdb_cursor_get MDB_SET_RANGE cur pKey pVal) >>= \case
                False -> pure mempty
                True  -> V.unfoldrM (fetchBatch cur pKey pVal) 1000

    fetchBatch :: Cur -> Ptr Val -> Ptr Val -> Word
              -> RIO e (Maybe ((Word64, ByteString), Word))
    fetchBatch cur pKey pVal 0 = pure Nothing
    fetchBatch cur pKey pVal n = do
        key <- io $ peek pKey >>= mdbValToWord64
        val <- io $ peek pVal >>= mdbValToBytes
        io $ mdb_cursor_get MDB_NEXT cur pKey pVal >>= \case
            False -> pure $ Just ((key, val), 0)
            True  -> pure $ Just ((key, val), pred n)

-}

-- Utils -----------------------------------------------------------------------

withKVPtrs' :: (MonadIO m, MonadUnliftIO m)
            => Val -> Val -> (Ptr Val -> Ptr Val -> m a) -> m a
withKVPtrs' k v cb =
    withRunInIO $ \run ->
        withKVPtrs k v $ \x y -> run (cb x y)

nullVal :: MDB_val
nullVal = MDB_val 0 nullPtr

word :: Int -> Word64
word = fromIntegral

assertExn :: Exception e => Bool -> e -> IO ()
assertExn True  _ = pure ()
assertExn False e = throwIO e

eitherExn :: Exception e => Either a b -> (a -> e) -> IO b
eitherExn eat exn = either (throwIO . exn) pure eat

byteStringAsMdbVal :: ByteString -> (MDB_val -> IO a) -> IO a
byteStringAsMdbVal bs k =
  BU.unsafeUseAsCStringLen bs $ \(ptr,sz) ->
    k (MDB_val (fromIntegral sz) (castPtr ptr))

mdbValToWord64 :: MDB_val -> IO Word64
mdbValToWord64 (MDB_val sz ptr) = do
  assertExn (sz == 8) BadKeyInEventLog
  peek (castPtr ptr)

withWord64AsMDBval :: (MonadIO m, MonadUnliftIO m)
                   => Word64 -> (MDB_val -> m a) -> m a
withWord64AsMDBval w cb = do
  withWordPtr w $ \p ->
    cb (MDB_val (fromIntegral (sizeOf w)) (castPtr p))

withWordPtr :: (MonadIO m, MonadUnliftIO m)
            => Word64 -> (Ptr Word64 -> m a) -> m a
withWordPtr w cb =
    withRunInIO $ \run ->
        allocaBytes (sizeOf w) (\p -> poke p w >> run (cb p))


-- Lower-Level Operations ------------------------------------------------------

getMb :: MonadIO m => Txn -> Dbi -> ByteString -> m (Maybe Noun)
getMb txn db key =
  io $
  byteStringAsMdbVal key $ \mKey ->
  mdb_get txn db mKey >>= traverse (mdbValToNoun key)

mdbValToBytes :: MDB_val -> IO ByteString
mdbValToBytes (MDB_val sz ptr) = do
  BU.unsafePackCStringLen (castPtr ptr, fromIntegral sz)

mdbValToNoun :: ByteString -> MDB_val -> IO Noun
mdbValToNoun key (MDB_val sz ptr) = do
  bs <- BU.unsafePackCStringLen (castPtr ptr, fromIntegral sz)
  let res = cueBS bs
  eitherExn res (\err -> BadNounInLogIdentity key err bs)

putNoun :: MonadIO m
        => MDB_WriteFlags -> Txn -> Dbi -> ByteString -> Noun -> m Bool
putNoun flags txn db key val =
  io $
  byteStringAsMdbVal key $ \mKey ->
  byteStringAsMdbVal (jamBS val) $ \mVal ->
  mdb_put flags txn db mKey mVal


putBytes :: MonadIO m
         => MDB_WriteFlags -> Txn -> Dbi -> Word64 -> ByteString -> m Bool
putBytes flags txn db id bs =
  io $
  withWord64AsMDBval id $ \idVal ->
  byteStringAsMdbVal bs $ \mVal ->
  mdb_put flags txn db idVal mVal
