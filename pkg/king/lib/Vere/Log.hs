{-
    TODO Effects storage logic is messy.
-}

module Vere.Log ( EventLog, identity, nextEv
                , new, existing
                , streamEvents, appendEvents
                , streamEffectsRows, writeEffectsRow
                ) where

import UrbitPrelude hiding (init)

import Data.Acquire
import Data.Conduit
import Database.LMDB.Raw
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Vere.Pier.Types

import Foreign.Storable (peek, poke, sizeOf)

import qualified Data.ByteString.Unsafe as BU
import qualified Data.Vector            as V


-- Types -----------------------------------------------------------------------

type Env = MDB_env
type Txn = MDB_txn
type Dbi = MDB_dbi
type Cur = MDB_cursor

data EventLog = EventLog
    { env        :: Env
    , _metaTbl   :: Dbi
    , eventsTbl  :: Dbi
    , effectsTbl :: Dbi
    , identity   :: LogIdentity
    , numEvents  :: IORef EventId
    }

nextEv :: EventLog -> IO EventId
nextEv = fmap succ . readIORef . numEvents

lastEv :: EventLog -> IO EventId
lastEv = readIORef . numEvents

data EventLogExn
    = NoLogIdentity
    | MissingEvent EventId
    | BadNounInLogIdentity ByteString DecodeErr ByteString
    | BadKeyInEventLog
    | BadWriteLogIdentity LogIdentity
    | BadWriteEvent EventId
    | BadWriteEffect EventId
  deriving Show


-- Instances -------------------------------------------------------------------

instance Exception EventLogExn where


-- Open/Close an Event Log -----------------------------------------------------

rawOpen :: FilePath -> IO Env
rawOpen dir = do
    putStrLn $ pack ("PAX: " <> dir)
    env <- mdb_env_create
    mdb_env_set_maxdbs env 3
    mdb_env_set_mapsize env (40 * 1024 * 1024 * 1024)
    mdb_env_open env dir []
    pure env

create :: FilePath -> LogIdentity -> IO EventLog
create dir id = do
    env       <- rawOpen dir
    (m, e, f) <- createTables env
    clearEvents env e
    writeIdent env m id
    EventLog env m e f id <$> newIORef 0
  where
    createTables env =
      with (writeTxn env) $ \txn ->
        (,,) <$> mdb_dbi_open txn (Just "META")    [MDB_CREATE]
             <*> mdb_dbi_open txn (Just "EVENTS")  [MDB_CREATE, MDB_INTEGERKEY]
             <*> mdb_dbi_open txn (Just "EFFECTS") [MDB_CREATE, MDB_INTEGERKEY]

open :: FilePath -> IO EventLog
open dir = do
    env       <- rawOpen dir
    (m, e, f) <- openTables env
    id        <- getIdent env m
    numEvs    <- getNumEvents env e
    EventLog env m e f id <$> newIORef numEvs
  where
    openTables env =
      with (writeTxn env) $ \txn ->
        (,,) <$> mdb_dbi_open txn (Just "META")    []
             <*> mdb_dbi_open txn (Just "EVENTS")  [MDB_INTEGERKEY]
             <*> mdb_dbi_open txn (Just "EFFECTS") [MDB_CREATE, MDB_INTEGERKEY]

close :: EventLog -> IO ()
close (EventLog env meta events effects _ _) = do
    mdb_dbi_close env meta
    mdb_dbi_close env events
    mdb_dbi_close env effects
    mdb_env_sync_flush env
    mdb_env_close env


-- Create a new event log or open an existing one. -----------------------------

existing :: FilePath -> Acquire EventLog
existing dir = mkAcquire (open dir) close

new :: FilePath -> LogIdentity -> Acquire EventLog
new dir id = mkAcquire (create dir id) close


-- Read/Write Log Identity -----------------------------------------------------

{-
    A read-only transaction that commits at the end.

    Use this when opening database handles.
-}
_openTxn :: Env -> Acquire Txn
_openTxn env = mkAcquire begin commit
  where
    begin  = mdb_txn_begin env Nothing True
    commit = mdb_txn_commit

{-
    A read-only transaction that aborts at the end.

    Use this when reading data from already-opened databases.
-}
readTxn :: Env -> Acquire Txn
readTxn env = mkAcquire begin abort
  where
    begin = mdb_txn_begin env Nothing True
    abort = mdb_txn_abort

{-
    A read-write transaction that commits upon sucessful completion and
    aborts on exception.

    Use this when reading data from already-opened databases.
-}
writeTxn :: Env -> Acquire Txn
writeTxn env = mkAcquireType begin finalize
  where
    begin = mdb_txn_begin env Nothing False
    finalize txn = \case
        ReleaseNormal    -> mdb_txn_commit txn
        ReleaseEarly     -> mdb_txn_commit txn
        ReleaseException -> mdb_txn_abort  txn

cursor :: Txn -> Dbi -> Acquire Cur
cursor txn dbi = mkAcquire open close
  where
    open  = mdb_cursor_open txn dbi
    close = mdb_cursor_close

getIdent :: Env -> Dbi -> IO LogIdentity
getIdent env dbi =
    getTbl env >>= traverse decodeIdent >>= \case
        Nothing -> throwIO NoLogIdentity
        Just li -> pure li
  where
    decodeIdent :: (Noun, Noun, Noun) -> IO LogIdentity
    decodeIdent = fromNounExn . toNoun

    getTbl :: Env -> IO (Maybe (Noun, Noun, Noun))
    getTbl env = do
        with (readTxn env) $ \txn -> do
            who  <- getMb txn dbi "who"
            fake <- getMb txn dbi "is-fake"
            life <- getMb txn dbi "life"
            pure $ (,,) <$> who <*> fake <*> life

writeIdent :: Env -> Dbi -> LogIdentity -> IO ()
writeIdent env metaTbl ident@LogIdentity{..} = do
    let flags = compileWriteFlags []
    with (writeTxn env) $ \txn -> do
        x <- putNoun flags txn metaTbl "who"     (toNoun who)
        y <- putNoun flags txn metaTbl "is-fake" (toNoun isFake)
        z <- putNoun flags txn metaTbl "life"    (toNoun lifecycleLen)
        unless (x && y && z) $ do
            throwIO (BadWriteLogIdentity ident)


-- Latest Event Number ---------------------------------------------------------

getNumEvents :: Env -> Dbi -> IO Word64
getNumEvents env eventsTbl =
    with (readTxn env) $ \txn ->
    with (cursor txn eventsTbl) $ \cur ->
    withKVPtrs nullVal nullVal $ \pKey pVal ->
    mdb_cursor_get MDB_LAST cur pKey pVal >>= \case
        False -> pure 0
        True  -> peek pKey >>= mdbValToWord64


-- Write Events ----------------------------------------------------------------

clearEvents :: Env -> Dbi -> IO ()
clearEvents env eventsTbl =
    with (writeTxn env) $ \txn ->
    with (cursor txn eventsTbl) $ \cur ->
    withKVPtrs nullVal nullVal $ \pKey pVal -> do
        let loop = mdb_cursor_get MDB_LAST cur pKey pVal >>= \case
                False -> pure ()
                True  -> do mdb_cursor_del (compileWriteFlags []) cur
                            loop
        loop

appendEvents :: EventLog -> Vector ByteString -> IO ()
appendEvents log !events = do
    numEvs <- readIORef (numEvents log)
    next   <- pure (numEvs + 1)
    doAppend $ zip [next..] $ toList events
    writeIORef (numEvents log) (numEvs + word (length events))
  where
    flags    = compileWriteFlags [MDB_NOOVERWRITE]
    doAppend = \kvs ->
        with (writeTxn $ env log) $ \txn ->
        for_ kvs $ \(k,v) -> do
            putBytes flags txn (eventsTbl log) k v >>= \case
                True  -> pure ()
                False -> throwIO (BadWriteEvent k)

writeEffectsRow :: EventLog -> EventId -> ByteString -> IO ()
writeEffectsRow log k v = do
    with (writeTxn $ env log) $ \txn ->
        putBytes flags txn (effectsTbl log) k v >>= \case
            True  -> pure ()
            False -> throwIO (BadWriteEffect k)
  where
    flags = compileWriteFlags []


--------------------------------------------------------------------------------
-- Read Events -----------------------------------------------------------------

streamEvents :: MonadIO m
             => EventLog -> Word64
             -> ConduitT () ByteString m ()
streamEvents log first = do
    last <- liftIO $ lastEv log
    batch <- liftIO (readBatch log first)
    unless (null batch) $ do
        for_ batch yield
        streamEvents log (first + word (length batch))

streamEffectsRows :: MonadIO m
                  => EventLog -> EventId
                  -> ConduitT () (Word64, ByteString) m ()
streamEffectsRows log = go
  where
    go next = do
        batch <- liftIO $ readRowsBatch (env log) (effectsTbl log) next
        unless (null batch) $ do
            for_ batch yield
            go (next + fromIntegral (length batch))

{-
   Read 1000 rows from the events table, starting from event `first`.

   Throws `MissingEvent` if an event was missing from the log.
-}
readBatch :: EventLog -> Word64 -> IO (V.Vector ByteString)
readBatch log first = start
  where
    start = do
        last <- lastEv log
        if (first > last)
            then pure mempty
            else readRows $ fromIntegral $ min 1000 $ ((last+1) - first)

    assertFound :: EventId -> Bool -> IO ()
    assertFound id found = do
        unless found $ throwIO $ MissingEvent id

    readRows count =
        withWordPtr first $ \pIdx ->
        withKVPtrs (MDB_val 8 (castPtr pIdx)) nullVal $ \pKey pVal ->
        with (readTxn $ env log) $ \txn ->
        with (cursor txn $ eventsTbl log) $ \cur -> do
            assertFound first =<< mdb_cursor_get MDB_SET_KEY cur pKey pVal
            fetchRows count cur pKey pVal

    fetchRows count cur pKey pVal = do
        V.generateM count $ \i -> do
            key <- peek pKey >>= mdbValToWord64
            val <- peek pVal >>= mdbValToBytes
            idx <- pure (first + word i)
            unless (key == idx) $ throwIO $ MissingEvent idx
            when (count /= succ i) $ do
                assertFound idx =<< mdb_cursor_get MDB_NEXT cur pKey pVal
            pure val

{-
   Read 1000 rows from the database, starting from key `first`.
-}
readRowsBatch :: Env -> Dbi -> Word64 -> IO (V.Vector (Word64, ByteString))
readRowsBatch env dbi first = readRows
  where
    readRows = do
        -- print ("readRows", first)
        withWordPtr first $ \pIdx ->
          withKVPtrs (MDB_val 8 (castPtr pIdx)) nullVal $ \pKey pVal ->
          with (readTxn env) $ \txn ->
          with (cursor txn dbi) $ \cur ->
          mdb_cursor_get MDB_SET_RANGE cur pKey pVal >>= \case
              False -> pure mempty
              True  -> V.unfoldrM (fetchRows cur pKey pVal) 1000

    fetchRows :: Cur -> Ptr MDB_val -> Ptr MDB_val
              -> Word
              -> IO (Maybe ((Word64, ByteString), Word))
    fetchRows cur pKey pVal 0 = pure Nothing
    fetchRows cur pKey pVal n = do
        key <- peek pKey >>= mdbValToWord64
        val <- peek pVal >>= mdbValToBytes
        -- print ("fetchRows", n, key, val)
        mdb_cursor_get MDB_NEXT cur pKey pVal >>= \case
            False -> pure $ Just ((key, val), 0)
            True  -> pure $ Just ((key, val), pred n)


-- Utils -----------------------------------------------------------------------

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

withWord64AsMDBval :: Word64 -> (MDB_val -> IO a) -> IO a
withWord64AsMDBval w cb = do
  withWordPtr w $ \p ->
    cb (MDB_val (fromIntegral (sizeOf w)) (castPtr p))

withWordPtr :: Word64 -> (Ptr Word64 -> IO a) -> IO a
withWordPtr w cb = do
  allocaBytes (sizeOf w) (\p -> poke p w >> cb p)


-- Lower-Level Operations ------------------------------------------------------

getMb :: Txn -> Dbi -> ByteString -> IO (Maybe Noun)
getMb txn db key =
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

putNoun :: MDB_WriteFlags -> Txn -> Dbi -> ByteString -> Noun -> IO Bool
putNoun flags txn db key val =
  byteStringAsMdbVal key $ \mKey ->
  byteStringAsMdbVal (jamBS val) $ \mVal ->
  mdb_put flags txn db mKey mVal

putBytes :: MDB_WriteFlags -> Txn -> Dbi -> Word64 -> ByteString -> IO Bool
putBytes flags txn db id bs = do
  withWord64AsMDBval id $ \idVal -> do
    byteStringAsMdbVal bs $ \mVal -> do
      mdb_put flags txn db idVal mVal
