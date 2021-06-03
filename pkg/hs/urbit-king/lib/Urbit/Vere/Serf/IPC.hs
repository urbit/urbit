{-|
  Low-Level IPC flows for interacting with the serf process.

  - Serf process can be started and shutdown with `work` and `stop`.
  - You can ask the serf what its last event was with
    `serfLastEventBlocking`.
  - A running serf can be asked to compact its heap or take a snapshot.
  - You can scry into a running serf.
  - A running serf can be sent a boot event.
-}

module Urbit.Vere.Serf.IPC
  ( Serf
  , work
  , stop
  , serfLastEventBlocking
  , snapshot
  , compact
  , scry
  , boot
  , run
  , sendSIGINT
  , module Urbit.Vere.Serf.Types
  )
where

import Urbit.Prelude hiding ((<|))

import Data.Bits
import Data.Conduit
import System.Process
import Urbit.Vere.Serf.Types
import Urbit.Vere.Serf.IPC.Types

import Control.Monad.STM            (retry)
import Control.Monad.Trans.Resource (MonadResource, allocate, runResourceT)
import Data.Sequence                (Seq((:<|), (:|>)))
import Foreign.Marshal.Alloc        (alloca)
import Foreign.Ptr                  (castPtr)
import Foreign.Storable             (peek, poke)
import RIO.Prelude                  (decodeUtf8Lenient)
import System.Exit                  (ExitCode)
import System.Posix.Signals         (sigINT, sigKILL, signalProcess)
import Urbit.Arvo                   (FX)
import Urbit.Arvo.Event
import Urbit.Noun.Time              (Wen)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import qualified System.IO.Error        as IO
import qualified Urbit.Noun.Time        as Time


-- Serf API --------------------------------------------------------------------

data Serf = Serf
  { serfSend :: Handle
  , serfRecv :: Handle
  , serfProc :: ProcessHandle
  , serfSlog :: Slog -> IO ()
  , serfLock :: MVar (Maybe SerfState)
  }


-- Access Current Serf State ---------------------------------------------------

serfLastEventBlocking :: Serf -> IO EventId
serfLastEventBlocking Serf{serfLock} = readMVar serfLock >>= \case
  Nothing -> throwIO SerfNotRunning
  Just ss -> pure (ssLast ss)


-- Low Level IPC Functions -----------------------------------------------------

fromRightExn :: (Exception e, MonadIO m) => Either a b -> (a -> e) -> m b
fromRightExn (Left m)  exn = throwIO (exn m)
fromRightExn (Right x) _   = pure x

-- TODO Support Big Endian
sendLen :: Serf -> Int -> IO ()
sendLen s i = do
  w <- evaluate (fromIntegral i :: Word64)
  withWord64AsByteString w (hPut (serfSend s))
 where
  withWord64AsByteString :: Word64 -> (ByteString -> IO a) -> IO a
  withWord64AsByteString w k = alloca $ \wp -> do
    poke wp w
    bs <- BS.unsafePackCStringLen (castPtr wp, 8)
    k bs

sendBytes :: Serf -> ByteString -> IO ()
sendBytes s bs = handle onIOError $ do
  sendLen s (length bs)
  hPut (serfSend s) bs
  hFlush (serfSend s)
 where
  onIOError :: IOError -> IO ()
  onIOError = const (throwIO SerfConnectionClosed)

recvBytes :: Serf -> Word64 -> IO ByteString
recvBytes serf = BS.hGet (serfRecv serf) . fromIntegral

recvLen :: Serf -> IO Word64
recvLen w = do
  bs <- BS.hGet (serfRecv w) 8
  case length bs of
    8 -> BS.unsafeUseAsCString bs (peek @Word64 . castPtr)
    _ -> throwIO SerfConnectionClosed

recvResp :: Serf -> IO ByteString
recvResp serf = do
  len <- recvLen serf
  recvBytes serf len


-- Send Task / Recv Gift -------------------------------------------------------

sendBoot :: Serf -> Boot -> IO ()
sendBoot s = sendBytes s . jamBS . toNoun

sendTask :: Serf -> Task -> IO ()
sendTask s = sendBytes s . jamBS . toNoun

recvGift :: Serf -> IO Gift
recvGift w = do
  b <- recvResp w
  n <- fromRightExn (cueBS b) (const $ BadPleaAtom $ bytesAtom b)
  p <- fromRightExn (fromNounErr @Gift n) (\(p, m) -> BadGiftNoun n p m)
  pure p

recvGiftHandlingSlog :: Serf -> IO Gift
recvGiftHandlingSlog serf = loop
 where
  loop = recvGift serf >>= \case
    GSlog info        -> serfSlog serf info >> loop
    GFlog (Cord ofni) -> serfSlog serf (0, Tank $ Leaf $ Tape $ ofni) >> loop
    other             -> pure other

-- Higher-Level IPC Functions --------------------------------------------------

recvLive :: Serf -> IO ()
recvLive serf = recvGiftHandlingSlog serf >>= \case
  GLive () -> pure ()
  gift     -> throwIO (UnexpectedGift (toNoun gift) "expecting %live")

recvPeek :: Serf -> IO (Maybe (Term, Noun))
recvPeek serf = recvGiftHandlingSlog serf >>= \case
  GPeek (EachYes peek) -> pure peek
  -- XX surface error content ; change `scry` return type to Either Goof
  GPeek (EachNo dud)   -> pure Nothing
  gift                 -> throwIO (UnexpectedGift (toNoun gift) "expecting %peek")

recvPoke :: Serf -> IO (Each FX [Goof])
recvPoke serf = recvGiftHandlingSlog serf >>= \case
  GPoke poke -> pure poke
  gift       -> throwIO (UnexpectedGift (toNoun gift) "expecting %poke")

recvRipe :: Serf -> IO Ripe
recvRipe serf = recvGiftHandlingSlog serf >>= \case
  GRipe ripe -> pure ripe
  gift       -> throwIO (UnexpectedGift (toNoun gift) "expecting %play")


-- Request-Response Points -- These don't touch the lock -----------------------

sendSnapshotRequest :: Serf -> EventId -> IO ()
sendSnapshotRequest serf eve = do
  sendTask serf (WLive $ LSave eve)
  recvLive serf

sendCompactionRequest :: Serf -> IO ()
sendCompactionRequest serf = do
  sendTask serf (WLive $ LPack ())
  recvLive serf

sendScryRequest :: Serf -> Gang -> ScryReq -> IO (Maybe (Term, Noun))
sendScryRequest serf g r = do
  sendTask serf (WPeek 0 g r)
  recvPeek serf

sendShutdownRequest :: Serf -> Atom -> IO ()
sendShutdownRequest serf exitCode = do
  sendTask serf (WLive $ LExit exitCode)
  pure ()


-- Starting the Serf -----------------------------------------------------------

compileFlags :: [Flag] -> Word
compileFlags = foldl' (\acc flag -> setBit acc (fromEnum flag)) 0

readStdErr :: Handle -> (Text -> IO ()) -> IO () -> IO ()
readStdErr h onLine onClose = loop
 where
  loop = do
    IO.tryIOError (BS.hGetLine h >>= onLine . decodeUtf8Lenient) >>= \case
      Left exn -> onClose
      Right () -> loop

start :: String -> Config -> IO Serf
start mode (Config exePax pierPath flags onSlog onStdr onDead) = do
  (Just i, Just o, Just e, p) <- createProcess pSpec
  void $ async (readStdErr e onStdr onDead)
  vLock <- newEmptyMVar
  Serf i o p onSlog vLock
 where
  diskKey = ""
  config  = show (compileFlags flags)
  rock    = "0"      -- XX support loading from rock
  cache   = "50000"  -- XX support memo-cache size
  args    = [mode, pierPath, diskKey, config, cache]
  pSpec   = (proc exePax args) { std_in  = CreatePipe
                               , std_out = CreatePipe
                               , std_err = CreatePipe
                               }


work :: Config -> IO (Serf, Ripe)
work fig = do
  serf <- start "work" fig
  info <- recvRipe serf
  putMVar vLock (Just $ siStat info)
  pure (serf, info)



boot :: Config -> Boot -> IO ExitCode
boot fig boot = do
  serf <- start "boot" fig
  sendBoot boot
  tsFlog <- async loop
  flip onException (cancel tSflog) $ waitForProcess serfProc
  where
    loop = do
      x <- recvGiftHandlingSlog serf
      fail ("Illegitimate gift during boot: " <> x)


-- Taking the SerfState Lock ---------------------------------------------------

takeLock :: MonadIO m => Serf -> m SerfState
takeLock serf = io $ do
  takeMVar (serfLock serf) >>= \case
    Nothing -> putMVar (serfLock serf) Nothing >> throwIO SerfNotRunning
    Just ss -> pure ss

serfLockTaken
  :: MonadResource m => Serf -> m (IORef (Maybe SerfState), SerfState)
serfLockTaken serf = snd <$> allocate take release
 where
  take = (,) <$> newIORef Nothing <*> takeLock serf
  release (rv, _) = do
    mRes <- readIORef rv
    when (mRes == Nothing) (forcefullyKillSerf serf)
    putMVar (serfLock serf) mRes

withSerfLock
  :: MonadResource m => Serf -> (SerfState -> m (SerfState, a)) -> m a
withSerfLock serf act = do
  (vState  , initialState) <- serfLockTaken serf
  (newState, result      ) <- act initialState
  writeIORef vState (Just newState)
  pure result

withSerfLockIO :: Serf -> (SerfState -> IO (SerfState, a)) -> IO a
withSerfLockIO s a = runResourceT (withSerfLock s (io . a))


-- SIGINT ----------------------------------------------------------------------

sendSIGINT :: Serf -> IO ()
sendSIGINT serf = do
  getPid (serfProc serf) >>= \case
    Nothing  -> pure ()
    Just pid -> do
      io $ signalProcess sigINT pid


-- Killing the Serf ------------------------------------------------------------


{-|
  Ask the serf to shutdown. If it takes more than 2s, kill it with
  SIGKILL.
-}
stop :: HasLogFunc e => Serf -> RIO e ()
stop serf = do
  race_ niceKill (wait2sec >> forceKill)
 where
  wait2sec = threadDelay 2_000_000

  niceKill = do
    logTrace "Asking serf to shut down"
    io (gracefullyKillSerf serf)
    logTrace "Serf went down when asked."

  forceKill = do
    logTrace "Serf taking too long to go down, kill with fire (SIGTERM)."
    io (forcefullyKillSerf serf)
    logTrace "Serf process killed with SIGTERM."

{-|
  Kill the serf by taking the lock, then asking for it to exit.
-}
gracefullyKillSerf :: Serf -> IO ()
gracefullyKillSerf serf@Serf{..} = do
  finalState <- takeMVar serfLock
  sendShutdownRequest serf 0
  waitForProcess serfProc
  pure ()

{-|
  Kill the serf by sending it a SIGKILL.
-}
forcefullyKillSerf :: Serf -> IO ()
forcefullyKillSerf serf = do
  getPid (serfProc serf) >>= \case
    Nothing  -> pure ()
    Just pid -> do
      io $ signalProcess sigKILL pid
      io $ void $ waitForProcess (serfProc serf)


-- Flows for Interacting with the Serf -----------------------------------------

{-|
  Ask the serf to write a snapshot to disk.
-}
snapshot :: Serf -> IO ()
snapshot serf = withSerfLockIO serf $ \ss -> do
  sendSnapshotRequest serf (ssLast ss)
  pure (ss, ())

{-|
  Ask the serf to de-duplicate and de-fragment its heap.
-}
compact :: Serf -> IO ()
compact serf = withSerfLockIO serf $ \ss -> do
  sendCompactionRequest serf
  pure (ss, ())

{-|
  Peek into the serf state.
-}
scry :: Serf -> Gang -> ScryReq -> IO (Maybe (Term, Noun))
scry serf g r = withSerfLockIO serf $ \ss -> do
  (ss,) <$> sendScryRequest serf g r



-- Running Ship Flow -----------------------------------------------------------

{-|
  TODO Don't take snapshot until event log has processed current event.
-}
run
  :: Serf
  -> Int
  -> STM EventId
  -> STM RunReq
  -> (Maybe Ev -> STM ())
  -> IO ()
run serf maxBatchSize getLastEvInLog onInput spin = topLoop
 where
  topLoop :: IO ()
  topLoop = atomically onInput >>= \case
    RRWork workErr -> doWork workErr
    RRSave ()      -> doSave
    RRKill ()      -> doKill
    RRPack ()      -> doPack
    RRScry g r k   -> doScry g r k

  doPack :: IO ()
  doPack = compact serf >> topLoop

  waitForLog :: IO ()
  waitForLog = do
    serfLast <- serfLastEventBlocking serf
    atomically $ do
      logLast <- getLastEvInLog
      when (logLast < serfLast) retry

  doSave :: IO ()
  doSave = waitForLog >> snapshot serf >> topLoop

  doKill :: IO ()
  doKill = waitForLog >> snapshot serf >> pure ()

  doScry :: Gang -> ScryReq -> (Maybe (Term, Noun) -> IO ()) -> IO ()
  doScry g r k = (scry serf g r >>= k) >> topLoop

  doWork :: EvErr -> IO ()
  doWork firstWorkErr = do
    que   <- newTBMQueueIO 1
    ()    <- atomically (writeTBMQueue que firstWorkErr)
    tWork <- async (processWork serf maxBatchSize que onWorkResp spin)
    -- Avoid wrapping all subsequent runs of the event loop in an exception
    -- handler which retains tWork.
    nexSt <- flip onException (cancel tWork) $ do
      nexSt <- workLoop que
      wait tWork
      pure nexSt
    nexSt

  workLoop :: TBMQueue EvErr -> IO (IO ())
  workLoop que = atomically onInput >>= \case
    RRKill ()      -> atomically (closeTBMQueue que) >> pure doKill
    RRSave ()      -> atomically (closeTBMQueue que) >> pure doSave
    RRPack ()      -> atomically (closeTBMQueue que) >> pure doPack
    RRScry g r k   -> atomically (closeTBMQueue que) >> pure (doScry g r k)
    RRWork workErr -> atomically (writeTBMQueue que workErr) >> workLoop que

  onWorkResp :: Wen -> EvErr -> Each FX [Goof] -> IO ()
  onWorkResp wen (EvErr evn err) = \case
    EachYes fx   -> io $ err (RunOkay fx)
    EachNo goofs -> io $ err (RunBail goofs)


{-|
  Given:

  - A stream of incoming requests
  - A sequence of in-flight requests that haven't been responded to
  - A maximum number of in-flight requests.

  Wait until the number of in-fligh requests is smaller than the maximum,
  and then take the next item from the stream of requests.
-}
pullFromQueueBounded :: Int -> TVar (Seq a) -> TBMQueue b -> STM (Maybe b)
pullFromQueueBounded maxSize vInFlight queue = do
  inFlight <- length <$> readTVar vInFlight
  if inFlight >= maxSize
    then retry
    else readTBMQueue queue

{-|
  Given

  - `maxSize`: The maximum number of jobs to send to the serf before
    getting a response.
  - `q`: A bounded queue (which can be closed)
  - `onResp`: a callback to call for each response from the serf.
  - `spin`: a callback to tell the terminal driver which event is
    currently being processed.

  Pull jobs from the queue and send them to the serf (eagerly, up to
  `maxSize`) and call the callback with each response from the serf.

  When the queue is closed, wait for the serf to respond to all pending
  work, and then return.

  Whenever the serf is idle, call `spin Nothing` and whenever the serf
  is working on an event, call `spin (Just ev)`.
-}
processWork
  :: Serf
  -> Int
  -> TBMQueue EvErr
  -> (Wen -> EvErr -> Each FX [Goof] -> IO ())
  -> (Maybe Ev -> STM ())
  -> IO ()
processWork serf maxSize q onResp spin = do
  vDoneFlag      <- newTVarIO False
  vInFlightQueue <- newTVarIO empty
  recvThread     <- async (recvLoop serf vDoneFlag vInFlightQueue spin)
  flip onException (print "KILLING: processWork" >> cancel recvThread) $ do
    loop vInFlightQueue vDoneFlag
    wait recvThread
 where
  loop :: TVar (Seq (Ev, Each FX [Goof] -> IO ())) -> TVar Bool -> IO ()
  loop vInFlight vDone = do
    atomically (pullFromQueueBounded maxSize vInFlight q) >>= \case
      Nothing -> do
        atomically (writeTVar vDone True)
      Just evErr@(EvErr ev _) -> do
        now <- Time.now
        let cb = onResp now evErr
        atomically $ modifyTVar' vInFlight (:|> (ev, cb))
        sendTask serf (WWork 0 now ev)
        loop vInFlight vDone

{-|
  Given:

  - `vDone`: A flag that no more work will be sent to the serf.

  - `vWork`: A list of work requests that have been sent to the serf,
     haven't been responded to yet.

  If the serf has responded to all work requests, and no more work is
  going to be sent to the serf, then return.

  If we are going to send more work to the serf, but the queue is empty,
  then wait.

  If work requests have been sent to the serf, take the first one,
  wait for a response from the serf, call the associated callback,
  and repeat the whole process.
-}
recvLoop
  :: Serf
  -> TVar Bool
  -> TVar (Seq (Ev, Each FX [Goof] -> IO ()))
  -> (Maybe Ev -> STM ())
  -> IO ()
recvLoop serf vDone vWork spin = do
  withSerfLockIO serf \SerfState {..} -> loop
 where
  loop = do
    atomically $ do
      whenM (null <$> readTVar vWork) $ do
        spin Nothing
    atomically takeCallback >>= \case
      Nothing -> pure (SerfState eve mug, ())
      Just (curEve, cb) -> do
        atomically (spin (Just curEve))
        work <- recvPoke serf
        cb work
        loop

  takeCallback :: STM (Maybe (Ev, Each FX [Goof] -> IO ()))
  takeCallback = do
    ((,) <$> readTVar vDone <*> readTVar vWork) >>= \case
      (False, Empty        ) -> retry
      (True , Empty        ) -> pure Nothing
      (_    , (e, x) :<| xs) -> writeTVar vWork xs $> Just (e, x)
      (_    , _            ) -> error "impossible"
