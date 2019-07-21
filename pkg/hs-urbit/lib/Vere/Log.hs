{-# OPTIONS_GHC -Wwarn #-}

module Vere.Log ( EventLog, identity, nextEv
                , new, existing
                , streamEvents, appendEvents
                ) where

import ClassyPrelude hiding (init)
import Control.Lens  hiding ((<|))
import Data.Conduit

import Data.Acquire
import Database.LMDB.Raw
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Noun
import Vere.Pier.Types

import Control.Lens     ((^.))
import Foreign.Storable (peek, poke, sizeOf)

import qualified Data.ByteString.Unsafe as BU
import qualified Data.Vector            as V


-- Types -----------------------------------------------------------------------

type Env = MDB_env
type Txn = MDB_txn
type Dbi = MDB_dbi
type Cur = MDB_cursor

data EventLog = EventLog
    { env       :: Env
    , _metaTbl  :: Dbi
    , eventsTbl :: Dbi
    , identity  :: LogIdentity
    , numEvents :: IORef EventId
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
  deriving Show


-- Instances -------------------------------------------------------------------

instance Exception EventLogExn where


-- Open/Close an Event Log -----------------------------------------------------

rawOpen :: FilePath -> IO Env
rawOpen dir = do
    env <- mdb_env_create
    mdb_env_set_maxdbs env 3
    mdb_env_set_mapsize env (40 * 1024 * 1024 * 1024)
    mdb_env_open env dir []
    pure env

create :: FilePath -> LogIdentity -> IO EventLog
create dir id = do
    env    <- rawOpen dir
    (m, e) <- createTables env
    clearEvents env e
    writeIdent env m id
    EventLog env m e id <$> newIORef 0
  where
    createTables env =
      with (writeTxn env) $ \txn -> do
        m <- mdb_dbi_open txn (Just "META")   [MDB_CREATE]
        e <- mdb_dbi_open txn (Just "EVENTS") [MDB_CREATE, MDB_INTEGERKEY]
        pure (m, e)

open :: FilePath -> IO EventLog
open dir = do
    env    <- rawOpen dir
    (m, e) <- openTables env
    id     <- getIdent env m
    numEvs <- getNumEvents env e
    EventLog env m e id <$> newIORef numEvs
  where
    openTables env =
      with (openTxn env) $ \txn ->
        (,) <$> mdb_dbi_open txn (Just "META")   []
            <*> mdb_dbi_open txn (Just "EVENTS") [MDB_INTEGERKEY]

close :: EventLog -> IO ()
close (EventLog env meta events _ _) = do
    mdb_dbi_close env meta
    mdb_dbi_close env events
    mdb_env_sync_flush env
    mdb_env_close env


-- Create a new event log or open an existing one. -----------------------------

existing :: FilePath -> Acquire EventLog
existing dir = mkAcquire (open dir) close

new :: FilePath -> LogIdentity -> Acquire EventLog
new dir id = mkAcquire (create dir id) close


-- Read/Write Log Identity -----------------------------------------------------

openTxn :: Env -> Acquire Txn
openTxn env = mkAcquire begin commit
  where
    begin  = mdb_txn_begin env Nothing True
    commit = mdb_txn_commit

readTxn :: Env -> Acquire Txn
readTxn env = mkAcquire begin abort
  where
    begin = mdb_txn_begin env Nothing True
    abort = mdb_txn_abort

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
    next   <- nextEv log
    doAppend $ zip [next..] $ toList events
    writeIORef (numEvents log) (numEvs + word (length events))
  where
    flags    = compileWriteFlags [MDB_NOOVERWRITE]
    doAppend = \kvs ->
        with (writeTxn $ env log) \txn ->
        for_ kvs $ \(k,v) -> do
            putEvent flags txn (eventsTbl log) k v >>= \case
                True  -> pure ()
                False -> throwIO (BadWriteEvent k)


-- Read Events -----------------------------------------------------------------

streamEvents :: EventLog -> Word64 -> ConduitT () ByteString IO ()
streamEvents log first = do
    last <- liftIO $ lastEv log
    traceM ("streamEvents: " <> show (first, last))
    batch <- liftIO (readBatch log first)
    unless (null batch) $ do
        for_ batch yield
        streamEvents log (first + word (length batch))

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

putEvent :: MDB_WriteFlags -> Txn -> Dbi -> Word64 -> ByteString -> IO Bool
putEvent flags txn db id bs = do
  withWord64AsMDBval id $ \idVal -> do
    traceM ("putEvent: " <> show (id, length bs))
    byteStringAsMdbVal bs $ \mVal -> do
      mdb_put flags txn db idVal mVal
