{-|
    High-Level Event-Log Interface

    TODO Effects storage logic is messy.
-}

module Urbit.EventLog.LMDB
  ( LogIdentity(..)
  , EventLog
  , identity
  , nextEv
  , lastEv
  , new
  , existing
  , streamEvents
  , appendEvents
  , trimEvents
  , streamEffectsRows
  , writeEffectsRow
  )
where

import ClassyPrelude

import Data.RAcquire
import Database.LMDB.Raw

import Data.Conduit          (ConduitT, yield)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr           (Ptr, castPtr, nullPtr)
import Foreign.Storable      (peek, poke, sizeOf)
import RIO                   (HasLogFunc, RIO, display, logDebug, runRIO)
import Urbit.Noun            (DecodeErr, Noun, Ship)
import Urbit.Noun            (deriveNoun, fromNounExn, toNoun, fromNoun)
import Urbit.Noun            (atomBytes, bytesAtom)
import Urbit.Noun.Core       (pattern Atom)


import qualified Data.ByteString.Unsafe as BU
import qualified Data.Vector            as V


-- Public Types ----------------------------------------------------------------

data LogIdentity = LogIdentity
  { who          :: !Ship
  , isFake       :: !Bool
  , lifecycleLen :: !Word
  } deriving (Eq, Ord, Show)

deriveNoun ''LogIdentity


-- Types -----------------------------------------------------------------------

type Env = MDB_env
type Val = MDB_val
type Txn = MDB_txn
type Dbi = MDB_dbi
type Cur = MDB_cursor

data EventLog = EventLog
  { env        :: Env
  , _metaTbl   :: Dbi
  , eventsTbl  :: Dbi
  , effectsTbl :: Dbi
  , identity   :: LogIdentity
  , numEvents  :: TVar Word64
  }

nextEv :: EventLog -> STM Word64
nextEv = fmap (+1) . lastEv

lastEv :: EventLog -> STM Word64
lastEv = readTVar . numEvents

data EventLogExn
  = NoLogIdentity
  | MissingLogVersion
  | BadLogVersion Word64
  | MissingEvent Word64
  | BadNounInLogIdentity ByteString DecodeErr ByteString
  | BadKeyInEventLog
  | BadWriteLogIdentity LogIdentity
  | BadWriteEvent Word64
  | BadWriteEffect Word64
 deriving Show


-- Instances -------------------------------------------------------------------

instance Exception EventLogExn where


-- Utils -----------------------------------------------------------------------

io :: MonadIO m => IO a -> m a
io = liftIO


-- Open/Close an Event Log -----------------------------------------------------

rawOpen :: MonadIO m => FilePath -> m Env
rawOpen dir = io $ do
    env <- mdb_env_create
    mdb_env_set_maxdbs env 3
    mdb_env_set_mapsize env (1024 * 1024 * 1024 * 1024)
    mdb_env_open env dir []
    pure env

create :: HasLogFunc e => FilePath -> LogIdentity -> RIO e EventLog
create dir id = do
    logDebug $ display (pack @Text $ "Creating LMDB database: " <> dir)
    logDebug $ display (pack @Text $ "Log Identity: " <> show id)
    env       <- rawOpen dir
    (m, e, f) <- createTables env
    clearEvents env e
    writeIdent env m id
    EventLog env m e f id <$> newTVarIO 0
  where
    createTables env =
      rwith (writeTxn env) $ \txn -> io $
        (,,) <$> mdb_dbi_open txn (Just "META")    [MDB_CREATE]
             <*> mdb_dbi_open txn (Just "EVENTS")  [MDB_CREATE, MDB_INTEGERKEY]
             <*> mdb_dbi_open txn (Just "EFFECTS") [MDB_CREATE, MDB_INTEGERKEY]

open :: HasLogFunc e => FilePath -> RIO e EventLog
open dir = do
    logDebug $ display (pack @Text $ "Opening LMDB database: " <> dir)
    env       <- rawOpen dir
    (m, e, f) <- openTables env
    id        <- getIdent env m
    logDebug $ display (pack @Text $ "Log Identity: " <> show id)
    numEvs    <- getNumEvents env e
    EventLog env m e f id <$> newTVarIO numEvs
  where
    openTables env =
      rwith (writeTxn env) $ \txn -> io $
        (,,) <$> mdb_dbi_open txn (Just "META")    []
             <*> mdb_dbi_open txn (Just "EVENTS")  [MDB_INTEGERKEY]
             <*> mdb_dbi_open txn (Just "EFFECTS") [MDB_CREATE, MDB_INTEGERKEY]

close :: HasLogFunc e => FilePath -> EventLog -> RIO e ()
close dir (EventLog env meta events effects _ _) = do
    logDebug $ display (pack @Text $ "Closing LMDB database: " <> dir)
    io $ do mdb_dbi_close env meta
            mdb_dbi_close env events
            mdb_dbi_close env effects
            mdb_env_sync_flush env
            mdb_env_close env


-- Create a new event log or open an existing one. -----------------------------

existing :: HasLogFunc e => FilePath -> RAcquire e EventLog
existing dir = mkRAcquire (open dir) (close dir)

new :: HasLogFunc e => FilePath -> LogIdentity -> RAcquire e EventLog
new dir id = mkRAcquire (create dir id) (close dir)


-- Read/Write Log Identity -----------------------------------------------------

{-|
    A read-only transaction that commits at the end.

    Use this when opening database handles.
-}
_openTxn :: Env -> RAcquire e Txn
_openTxn env = mkRAcquire begin commit
  where
    begin  = io $ mdb_txn_begin env Nothing True
    commit = io . mdb_txn_commit

{-|
    A read-only transaction that aborts at the end.

    Use this when reading data from already-opened databases.
-}
readTxn :: Env -> RAcquire e Txn
readTxn env = mkRAcquire begin abort
  where
    begin = io $ mdb_txn_begin env Nothing True
    abort = io . mdb_txn_abort

{-|
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

cursor :: Txn -> Dbi -> RAcquire e Cur
cursor txn dbi = mkRAcquire open close
  where
    open  = io $ mdb_cursor_open txn dbi
    close = io . mdb_cursor_close

getIdent :: HasLogFunc e => Env -> Dbi -> RIO e LogIdentity
getIdent env dbi = do
    logDebug "Reading log identity"
    getTbl env >>= traverse decodeIdent >>= \case
        Nothing -> throwIO NoLogIdentity
        Just li -> pure li
  where
    decodeIdent :: (Noun, Noun, Noun, Noun) -> RIO e LogIdentity
    decodeIdent (ver, who, fake, life) = do
      -- Verify log version
      case fromNoun ver of
        Just 1 -> pure ()
        Just x -> throwIO $ BadLogVersion x
        Nothing -> throwIO $ MissingLogVersion

      fromNounExn $ toNoun (who, fake, life)

    getTbl :: Env -> RIO e (Maybe (Noun, Noun, Noun, Noun))
    getTbl env = do
        rwith (readTxn env) $ \txn -> do
            version <- getMb txn dbi "version"
            who  <- getMb txn dbi "who"
            fake <- getMb txn dbi "fake"
            life <- getMb txn dbi "life"
            pure $ (,,,) <$> version <*> who <*> fake <*> life

writeIdent :: HasLogFunc e => Env -> Dbi -> LogIdentity -> RIO e ()
writeIdent env metaTbl ident@LogIdentity{..} = do
    logDebug "Writing log identity"
    let flags = compileWriteFlags []
    rwith (writeTxn env) $ \txn -> do
        w <- putAtom flags txn metaTbl "version" (toNoun (1 :: Integer))
        x <- putAtom flags txn metaTbl "who"     (toNoun who)
        y <- putAtom flags txn metaTbl "fake"    (toNoun isFake)
        z <- putAtom flags txn metaTbl "life"    (toNoun lifecycleLen)
        unless (w && x && y && z) $ do
            throwIO (BadWriteLogIdentity ident)


-- Latest Event Number ---------------------------------------------------------

getNumEvents :: Env -> Dbi -> RIO e Word64
getNumEvents env eventsTbl =
    rwith (readTxn env) $ \txn ->
    rwith (cursor txn eventsTbl) $ \cur ->
    withKVPtrs' nullVal nullVal $ \pKey pVal ->
    io $ mdb_cursor_get MDB_LAST cur pKey pVal >>= \case
        False -> pure 0
        True  -> peek pKey >>= mdbValToWord64


-- Write Events ----------------------------------------------------------------

clearEvents :: Env -> Dbi -> RIO e ()
clearEvents env eventsTbl =
    rwith (writeTxn env) $ \txn ->
    rwith (cursor txn eventsTbl) $ \cur ->
    withKVPtrs' nullVal nullVal $ \pKey pVal -> do
        let loop = io (mdb_cursor_get MDB_LAST cur pKey pVal) >>= \case
                False -> pure ()
                True  -> do io $ mdb_cursor_del (compileWriteFlags []) cur
                            loop
        loop

appendEvents :: EventLog -> Vector ByteString -> RIO e ()
appendEvents log !events = do
    numEvs <- atomically $ readTVar (numEvents log)
    next   <- pure (numEvs + 1)
    doAppend $ zip [next..] $ toList events
    atomically $ writeTVar (numEvents log) (numEvs + word (length events))
  where
    flags    = compileWriteFlags [MDB_NOOVERWRITE]
    doAppend = \kvs ->
        rwith (writeTxn $ env log) $ \txn ->
        for_ kvs $ \(k,v) -> do
            putBytes flags txn (eventsTbl log) k v >>= \case
                True  -> pure ()
                False -> throwIO (BadWriteEvent k)

writeEffectsRow :: MonadIO m => EventLog -> Word64 -> ByteString -> m ()
writeEffectsRow log k v = io $ runRIO () $ do
  let flags = compileWriteFlags []
  rwith (writeTxn $ env log) $ \txn ->
    putBytes flags txn (effectsTbl log) k v >>= \case
      True  -> pure ()
      False -> throwIO (BadWriteEffect k)


-- Read Events -----------------------------------------------------------------

trimEvents :: HasLogFunc e => EventLog -> Word64 -> RIO e ()
trimEvents log start = do
    last <- atomically (lastEv log)
    rwith (writeTxn $ env log) $ \txn ->
        for_ [start..last] $ \eId ->
        withWordPtr eId $ \pKey -> do
            let key = MDB_val 8 (castPtr pKey)
            found <- io $ mdb_del txn (eventsTbl log) key Nothing
            unless found $
                throwIO (MissingEvent eId)
    atomically $ writeTVar (numEvents log) (pred start)

streamEvents :: MonadIO m => EventLog -> Word64 -> ConduitT () ByteString m ()
streamEvents log first = do
  batch <- io $ runRIO () $ readBatch log first
  unless (null batch) $ do
    for_ batch yield
    streamEvents log (first + word (length batch))

streamEffectsRows :: forall e. HasLogFunc e
                  => EventLog -> Word64
                  -> ConduitT () (Word64, ByteString) (RIO e) ()
streamEffectsRows log = go
  where
    go :: Word64 -> ConduitT () (Word64, ByteString) (RIO e) ()
    go next = do
        batch <- lift $ readRowsBatch (env log) (effectsTbl log) next
        unless (null batch) $ do
            for_ batch yield
            go (next + fromIntegral (length batch))

{-|
   Read 1000 rows from the events table, starting from event `first`.

   Throws `MissingEvent` if an event was missing from the log.
-}
readBatch :: EventLog -> Word64 -> RIO e (V.Vector ByteString)
readBatch log first = start
  where
    start = do
        last <- atomically (lastEv log)
        if (first > last)
            then pure mempty
            else readRows $ fromIntegral $ min 1000 $ ((last+1) - first)

    assertFound :: Word64 -> Bool -> RIO e ()
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

{-|
   Read 1000 rows from the database, starting from key `first`.
-}
readRowsBatch :: forall e. HasLogFunc e
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
  (Atom . bytesAtom) <$> BU.unsafePackCStringLen (castPtr ptr, fromIntegral sz)

putAtom :: MonadIO m
        => MDB_WriteFlags -> Txn -> Dbi -> ByteString -> Noun -> m Bool
putAtom flags txn db key val =
  case val of
    Atom a -> io $
      byteStringAsMdbVal key $ \mKey ->
      byteStringAsMdbVal (atomBytes a) $ \mVal ->
      mdb_put flags txn db mKey mVal
    _ -> error "Impossible putAtom received cell"

putBytes :: MonadIO m
         => MDB_WriteFlags -> Txn -> Dbi -> Word64 -> ByteString -> m Bool
putBytes flags txn db id bs =
  io $
  withWord64AsMDBval id $ \idVal ->
  byteStringAsMdbVal bs $ \mVal ->
  mdb_put flags txn db idVal mVal
