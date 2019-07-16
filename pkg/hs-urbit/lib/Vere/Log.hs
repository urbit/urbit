{-
    TODO: Make sure transaction closed in all error cases
    TODO: Don't allow writing non-contiguous events
-}

module Vere.Log ( open
                , close
                , readEvents
                , latestEventNumber
                , readIdent
                , writeIdent
                , putJam
                , wipeEvents
                , deleteEventAndEverythingAfterIt
                ) where

import ClassyPrelude hiding (init)
import Control.Lens  hiding ((<|))

import Database.LMDB.Raw
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Noun
import Vere.Pier.Types

import Control.Concurrent (runInBoundThread)
import Control.Lens       ((^.))
import Foreign.Storable   (peek, poke, sizeOf)

import qualified Data.ByteString.Unsafe as BU
import qualified Data.Vector            as V


-- Open/Close an Event Log -----------------------------------------------------

open :: FilePath -> IO EventLog
open dir = do
  env <- mdb_env_create
  mdb_env_set_maxdbs env 3
  mdb_env_set_mapsize env (40 * 1024 * 1024 * 1024)
  mdb_env_open env dir []
  pure (EventLog env)

close :: EventLog -> IO ()
close (EventLog env) = mdb_env_close env


-- Read/Write Log Identity -----------------------------------------------------

readIdent :: EventLog -> IO LogIdentity
readIdent (EventLog env) = do
  txn  <- mdb_txn_begin env Nothing True
  db   <- mdb_dbi_open txn (Just "META") []
  who  <- get txn db "who"
  fake <- get txn db "is-fake"
  life <- get txn db "life"
  mdb_txn_abort txn
  fromNounExn $ toNoun (who, fake, life)

writeIdent :: EventLog -> LogIdentity -> IO ()
writeIdent (EventLog env) LogIdentity{..} = do
  txn <- mdb_txn_begin env Nothing False
  db  <- mdb_dbi_open txn (Just "META") [MDB_CREATE]
  let flags = compileWriteFlags []
  putNoun flags txn db "who"     $ toNoun who
  putNoun flags txn db "is-fake" $ toNoun isFake
  putNoun flags txn db "life"    $ toNoun lifecycleLen
  mdb_txn_commit txn
  pure ()


-- Latest Event Number ---------------------------------------------------------

latestEventNumber :: EventLog -> IO Word64
latestEventNumber (EventLog env) =
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


-- Read Events -----------------------------------------------------------------

-- TODO: This will read len items and will error if there are less than that
-- available. This differs from the current pier.c's expectations.
readEvents :: EventLog -> Word64 -> Word64 -> IO (V.Vector (Word64,Atom))
readEvents (EventLog env) first len =
  withWordPtr first $ \pIdx ->
  withKVPtrs (MDB_val 8 (castPtr pIdx)) (MDB_val 0 nullPtr) $ \pKey pVal ->
  do
    txn <- mdb_txn_begin env Nothing True
    db  <- mdb_dbi_open txn (Just "EVENTS") [MDB_CREATE, MDB_INTEGERKEY]
    cur <- mdb_cursor_open txn db

    found <- mdb_cursor_get MDB_SET_KEY cur pKey pVal
    assertErr found "mdb could not read initial event of sequence"

    vec <- V.generateM (int len) $ \i -> do
      key <- peek pKey >>= mdbValToWord64
      val <- peek pVal >>= mdbValToAtom

      let idx = first + (fromIntegral i)

      assertErr (key == idx) ("missing event in database " <> (show idx))

      when (i + 1 /= (int len)) $ do
        found <- mdb_cursor_get MDB_NEXT cur pKey pVal
        assertErr found "lmdb: next event not found"

      pure (idx, val)

    mdb_cursor_close cur
    mdb_txn_abort txn

    pure vec


-- Utils -----------------------------------------------------------------------

int :: Word64 -> Int
int = fromIntegral

assertErr :: Bool -> String -> IO ()
assertErr True  _ = pure ()
assertErr False m = error m

maybeErr :: Maybe a -> String -> IO a
maybeErr (Just x) _   = pure x
maybeErr Nothing  msg = error msg

byteStringAsMdbVal :: ByteString -> (MDB_val -> IO a) -> IO a
byteStringAsMdbVal bs k =
  BU.unsafeUseAsCStringLen bs $ \(ptr,sz) ->
    k (MDB_val (fromIntegral sz) (castPtr ptr))

mdbValToWord64 :: MDB_val -> IO Word64
mdbValToWord64 (MDB_val sz ptr) = do
  assertErr (sz == 8) "wrong size in mdbValToWord64"
  peek (castPtr ptr)

withWord64AsMDBval :: Word64 -> (MDB_val -> IO a) -> IO a
withWord64AsMDBval w cb = do
  withWordPtr w $ \p ->
    cb (MDB_val (fromIntegral (sizeOf w)) (castPtr p))

withWordPtr :: Word64 -> (Ptr Word64 -> IO a) -> IO a
withWordPtr w cb = do
  allocaBytes (sizeOf w) (\p -> poke p w >> cb p)


-- Lower-Level Operations ------------------------------------------------------

get :: MDB_txn -> MDB_dbi -> ByteString -> IO Noun
get txn db key =
  byteStringAsMdbVal key $ \mKey ->
  mdb_get txn db mKey >>= maybe (error "mdb bad get") mdbValToNoun

mdbValToAtom :: MDB_val -> IO Atom
mdbValToAtom (MDB_val sz ptr) = do
  bs <- BU.unsafePackCStringLen (castPtr ptr, fromIntegral sz)
  pure (bs ^. from atomBytes)

mdbValToNoun :: MDB_val -> IO Noun
mdbValToNoun (MDB_val sz ptr) = do
  bs <- BU.unsafePackCStringLen (castPtr ptr, fromIntegral sz)
  let res = bs ^? _Cue
  maybeErr res "mdb bad cue"

putRaw :: MDB_WriteFlags -> MDB_txn -> MDB_dbi -> MDB_val -> MDB_val -> IO ()
putRaw flags txn db key val =
  mdb_put flags txn db key val >>= \case
    True  -> pure ()
    False -> error "mdb bad put"

putNoun :: MDB_WriteFlags -> MDB_txn -> MDB_dbi -> ByteString -> Noun -> IO ()
putNoun flags txn db key val =
  byteStringAsMdbVal key $ \mKey ->
  byteStringAsMdbVal (val ^. re _Cue) $ \mVal ->
  putRaw flags txn db mKey mVal

putJam :: MDB_WriteFlags -> MDB_txn -> MDB_dbi -> Word64 -> Jam -> IO ()
putJam flags txn db id (Jam atom) = do
  withWord64AsMDBval id $ \idVal -> do
    let !bs = atom ^. atomBytes
    byteStringAsMdbVal bs $ \mVal -> do
      putRaw flags txn db idVal mVal


-- Event Pruning ---------------------------------------------------------------

wipeEvents :: FilePath -> IO ()
wipeEvents pax = deleteEventAndEverythingAfterIt pax 1

deleteEventAndEverythingAfterIt :: FilePath -> Word64 -> IO ()
deleteEventAndEverythingAfterIt dir first =
    runInBoundThread $ do
        log@(EventLog env) <- open dir

        last <- latestEventNumber log
        txn  <- mdb_txn_begin env Nothing False
        db   <- mdb_dbi_open txn (Just "EVENTS") [MDB_CREATE, MDB_INTEGERKEY]

        for_ (reverse [first..last]) $ \i ->
            withWord64AsMDBval i $ \val -> do
                mdb_del txn db val Nothing

        mdb_txn_commit txn
