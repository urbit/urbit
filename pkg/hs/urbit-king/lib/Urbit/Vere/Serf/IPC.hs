{-
|%
::  +writ: from king to serf
::
+$  gang  (unit (set ship))
+$  writ
  $%  $:  %live
          $%  [%exit cod=@]
              [%save eve=@]
              [%pack eve=@]
      ==  ==
      [%peek now=date lyc=gang pat=path]
      [%play eve=@ lit=(list ?((pair date ovum) *))]
      [%work job=(pair date ovum)]
  ==
::  +plea: from serf to king
::
+$  plea
  $%  [%live ~]
      [%ripe [pro=@ hon=@ nok=@] eve=@ mug=@]
      [%slog pri=@ ?(cord tank)]
      [%peek dat=(unit (cask))]
      $:  %play
          $%  [%done mug=@]
              [%bail eve=@ mug=@ dud=goof]
      ==  ==
      $:  %work
          $%  [%done eve=@ mug=@ fec=(list ovum)]
              [%swap eve=@ mug=@ job=(pair date ovum) fec=(list ovum)]
              [%bail lud=(list goof)]
      ==  ==
  ==
-}

module Urbit.Vere.Serf.IPC
  ( Serf
  , Config(..)
  , PlayBail(..)
  , Flag(..)
  , WorkError(..)
  , start
  , serfLastEventBlocking
  , shutdown
  , snapshot
  , bootSeq
  , replay
  , running
  , EvErr(..)
  , ComputeRequest(..)
  , SpinState
  )
where

import Urbit.Prelude hiding ((<|))

import Data.Bits
import Data.Conduit
import System.Process
import Urbit.Arvo
import Urbit.Vere.Pier.Types hiding (Work)

import Control.Monad.STM     (retry)
import Data.Sequence         (Seq((:<|), (:|>)))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr           (castPtr)
import Foreign.Storable      (peek, poke)
import RIO.Prelude           (decodeUtf8Lenient)
import System.Posix.Signals  (sigKILL, signalProcess)
import Urbit.Time            (Wen)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import qualified System.IO.Error        as IO
import qualified Urbit.Time             as Time


-- IPC Types -------------------------------------------------------------------

type Gang = Maybe (HoonSet Ship)

type Goof = (Term, [Tank])

data Live
  = LExit Atom -- exit status code
  | LSave EventId
  | LPack EventId
 deriving (Show)

type PlayBail = (EventId, Mug, Goof)

data Play
  = PDone Mug
  | PBail PlayBail
 deriving (Show)

data Work
  = WDone EventId Mug FX
  | WSwap EventId Mug (Wen, Noun) FX
  | WBail [Goof]
 deriving (Show)

data Writ
  = WLive Live
  | WPeek Wen Gang Path
  | WPlay EventId [Noun]
  | WWork Wen Ev
 deriving (Show)

data RipeInfo = RipeInfo
  { riProt :: Atom
  , riHoon :: Atom
  , riNock :: Atom
  }
 deriving (Show)

data SerfState = SerfState
  { ssLast :: EventId
  , ssHash :: Mug
  }
 deriving (Show)

data SerfInfo = SerfInfo
  { siRipe :: RipeInfo
  , siStat :: SerfState
  }
 deriving (Show)

type Slog = (Atom, Tank)

data Plea
  = PLive ()
  | PRipe SerfInfo
  | PSlog Slog
  | PPeek (Maybe (Term, Noun))
  | PPlay Play
  | PWork Work
 deriving (Show)

deriveNoun ''Live
deriveNoun ''Play
deriveNoun ''Work
deriveNoun ''Writ
deriveNoun ''RipeInfo
deriveNoun ''SerfState
deriveNoun ''SerfInfo
deriveNoun ''Plea


-- Serf API Types --------------------------------------------------------------

data Serf = Serf
  { serfSend :: Handle
  , serfRecv :: Handle
  , serfProc :: ProcessHandle
  , serfSlog :: Slog -> IO ()
  , serfLock :: MVar (Either SomeException SerfState)
  }

data Flag
  = DebugRam
  | DebugCpu
  | CheckCorrupt
  | CheckFatal
  | Verbose
  | DryRun
  | Quiet
  | Hashless
  | Trace
 deriving (Eq, Ord, Show, Enum, Bounded)

data Config = Config
  { scSerf :: FilePath       --  Where is the urbit-worker executable?
  , scPier :: FilePath       --  Where is the pier directory?
  , scFlag :: [Flag]         --  Serf execution flags.
  , scSlog :: Slog -> IO ()  --  What to do with slogs?
  , scStdr :: Text -> IO ()  --  What to do with lines from stderr?
  , scDead :: IO ()          --  What to do when the serf process goes down?
  }

data WorkError
  = RunBail [Goof]
  | RunSwap EventId Mug Wen Noun FX

data EvErr = EvErr Ev (WorkError -> IO ())

data ComputeRequest
  = CRWork EvErr
  | CRSave ()
  | CRKill ()
  | CRPack ()
  | CRScry Wen Gang Path (Maybe (Term, Noun) -> IO ())

type SpinState = Maybe Ev


-- Exceptions ------------------------------------------------------------------

data SerfExn
--  = BadComputeId EventId (Fact, FX)
--  | BadReplacementId EventId ReplacementEv
--  | UnexpectedPlay EventId (EventId, Mug)
    = UnexpectedPlea Plea Text
    | BadPleaAtom Atom
    | BadPleaNoun Noun [Text] Text
--  | ReplacedEventDuringReplay EventId ReplacementEv
--  | ReplacedEventDuringBoot   EventId ReplacementEv
--  | EffectsDuringBoot         EventId FX
    | SerfConnectionClosed
--  | UnexpectedPleaOnNewShip Plea
--  | InvalidInitialPlea Plea
  deriving (Show, Exception)


-- Access Current Serf State ---------------------------------------------------

serfLastEventBlocking :: Serf -> IO EventId
serfLastEventBlocking Serf{serfLock} = readMVar serfLock >>= \case
  Left err -> throwIO err
  Right ss -> pure (ssLast ss)


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


-- Send Writ / Recv Plea -------------------------------------------------------

sendWrit :: Serf -> Writ -> IO ()
sendWrit s = sendBytes s . jamBS . toNoun

recvPlea :: Serf -> IO Plea
recvPlea w = do
  b <- recvResp w
  n <- fromRightExn (cueBS b) (const $ BadPleaAtom $ bytesAtom b)
  p <- fromRightExn (fromNounErr @Plea n) (\(p, m) -> BadPleaNoun n p m)
  pure p

recvPleaHandlingSlog :: Serf -> IO Plea
recvPleaHandlingSlog serf = loop
 where
  loop = recvPlea serf >>= \case
    PSlog info -> serfSlog serf info >> loop
    other      -> pure other


-- Higher-Level IPC Functions --------------------------------------------------

recvRipe :: Serf -> IO SerfInfo
recvRipe serf = recvPleaHandlingSlog serf >>= \case
  PRipe ripe -> pure ripe
  plea       -> throwIO (UnexpectedPlea plea "expecting %play")

recvPlay :: Serf -> IO Play
recvPlay serf = recvPleaHandlingSlog serf >>= \case
  PPlay play -> pure play
  plea       -> throwIO (UnexpectedPlea plea "expecting %play")

recvLive :: Serf -> IO ()
recvLive serf = recvPleaHandlingSlog serf >>= \case
  PLive () -> pure ()
  plea     -> throwIO (UnexpectedPlea plea "expecting %live")

recvWork :: Serf -> IO Work
recvWork serf = do
  recvPleaHandlingSlog serf >>= \case
    PWork work -> pure work
    plea       -> throwIO (UnexpectedPlea plea "expecting %work")

recvPeek :: Serf -> IO (Maybe (Term, Noun))
recvPeek serf = do
  recvPleaHandlingSlog serf >>= \case
    PPeek peek -> pure peek
    plea       -> throwIO (UnexpectedPlea plea "expecting %peek")


-- Request-Response Points -- These don't touch the lock -----------------------

sendSnapshotRequest :: Serf -> EventId -> IO ()
sendSnapshotRequest serf eve = do
  sendWrit serf (WLive $ LSave eve)
  recvLive serf

sendCompactionRequest :: Serf -> EventId -> IO ()
sendCompactionRequest serf eve = do
  sendWrit serf (WLive $ LPack eve)
  recvLive serf

sendScryRequest :: Serf -> Wen -> Gang -> Path -> IO (Maybe (Term, Noun))
sendScryRequest serf w g p = do
  sendWrit serf (WPeek w g p)
  recvPeek serf

sendShutdownRequest :: Serf -> Atom -> IO ()
sendShutdownRequest serf exitCode = do
  sendWrit serf (WLive $ LExit exitCode)
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

start :: Config -> IO (Serf, SerfInfo)
start (Config exePax pierPath flags onSlog onStdr onDead) = do
  (Just i, Just o, Just e, p) <- createProcess pSpec
  void $ async (readStdErr e onStdr onDead)
  vLock <- newEmptyMVar
  let serf = Serf i o p onSlog vLock
  info <- recvRipe serf
  putMVar vLock (Right $ siStat info)
  pure (serf, info)
 where
  diskKey = ""
  config  = show (compileFlags flags)
  args    = [pierPath, diskKey, config]
  pSpec   = (proc exePax args) { std_in  = CreatePipe
                               , std_out = CreatePipe
                               , std_err = CreatePipe
                               }


-- Taking the SerfState Lock ---------------------------------------------------

withSerfLock
  :: MonadIO m
  => (m (SerfState, a) -> m (Either SomeException (SerfState, a)))
  -> Serf
  -> (SerfState -> m (SerfState, a))
  -> m a
withSerfLock tryGen s f = do
  ss <- takeLock
  tryGen (f ss) >>= \case
    Left e -> do
      io (forceKillSerf s)
      putMVar (serfLock s) (Left e)
      throwIO e
    Right (ss', x) -> do
      putMVar (serfLock s) (Right ss')
      pure x
 where
  takeLock = do
    takeMVar (serfLock s) >>= \case
      Left exn -> putMVar (serfLock s) (Left exn) >> throwIO exn
      Right ss -> pure ss


-- Flows for Interacting with the Serf -----------------------------------------

snapshot :: Serf -> IO ()
snapshot serf = withSerfLock try serf $ \ss -> do
  sendSnapshotRequest serf (ssLast ss)
  pure (ss, ())

compact :: Serf -> IO ()
compact serf = withSerfLock try serf $ \ss -> do
  sendCompactionRequest serf (ssLast ss)
  pure (ss, ())

scry :: Serf -> Wen -> Gang -> Path -> (Maybe (Term, Noun) -> IO ()) -> IO ()
scry serf w g p k = withSerfLock try serf $ \ss -> do
  sendScryRequest serf w g p >>= k
  pure (ss, ())

shutdown :: HasLogFunc e => Serf -> RIO e ()
shutdown serf = do
  race_ (wait2sec >> forceKill) $ do
    logTrace "Getting current serf state (taking lock, might block if in use)."
    finalState <- takeMVar (serfLock serf)
    logTrace "Got serf state (and took lock). Requesting shutdown."
    io (sendShutdownRequest serf 0)
    logTrace "Sent shutdown request. Waiting for process to die."
    io $ waitForProcess (serfProc serf)
    logTrace "RIP Serf process."
 where
  wait2sec  = threadDelay 2_000_000
  forceKill = do
    logTrace "Serf taking too long to go down, kill with fire (SIGTERM)."
    io (forceKillSerf serf)
    logTrace "Serf process killed with SIGTERM."

forceKillSerf :: Serf -> IO ()
forceKillSerf serf = do
  getPid (serfProc serf) >>= \case
    Nothing  -> pure ()
    Just pid -> do
      io $ signalProcess sigKILL pid
      io $ void $ waitForProcess (serfProc serf)

bootSeq :: Serf -> [Noun] -> IO (Maybe PlayBail)
bootSeq serf@Serf {..} seq = do
  withSerfLock try serf $ \ss -> do
    recvPlay serf >>= \case
      PBail bail -> pure (ss, Just bail)
      PDone mug  -> pure (SerfState (fromIntegral $ length seq) mug, Nothing)

replay
  :: forall m
   . (MonadUnliftIO m, MonadIO m)
  => Int
  -> Serf
  -> ConduitT Noun Void m (Maybe PlayBail)
replay batchSize serf = do
  withSerfLock tryC serf $ \ss -> do
    (r, ss') <- loop ss
    pure (ss', r)
 where
  loop :: SerfState -> ConduitT Noun Void m (Maybe PlayBail, SerfState)
  loop ss@(SerfState lastEve lastMug) = do
    awaitBatch batchSize >>= \case
      []  -> pure (Nothing, SerfState lastEve lastMug)
      evs -> do
        let nexEve = lastEve + 1
        let newEve = lastEve + fromIntegral (length evs)
        io $ sendWrit serf (WPlay nexEve evs)
        io (recvPlay serf) >>= \case
          PBail bail   -> pure (Just bail, SerfState lastEve lastMug)
          PDone newMug -> loop (SerfState newEve newMug)

{-
  TODO If this is slow, use a mutable vector instead of reversing a list.
-}
awaitBatch :: Monad m => Int -> ConduitT i o m [i]
awaitBatch = go []
 where
  go acc 0 = pure (reverse acc)
  go acc n = await >>= \case
    Nothing -> pure (reverse acc)
    Just x  -> go (x:acc) (n-1)

{-
  TODO Don't take snapshot until event log has processed current event.
-}
running
  :: Serf
  -> Int
  -> STM ComputeRequest
  -> ((Fact, FX) -> STM ())
  -> (SpinState -> STM ())
  -> IO ()
running serf maxBatchSize onInput sendOn spin = topLoop
 where
  topLoop :: IO ()
  topLoop = atomically onInput >>= \case
    CRWork workErr -> doWork workErr
    CRSave ()      -> doSave
    CRKill ()      -> pure ()
    CRPack ()      -> doPack
    CRScry w g p k -> doScry w g p k

  doPack :: IO ()
  doPack = compact serf >> topLoop

  doSave :: IO ()
  doSave = snapshot serf >> topLoop

  doScry :: Wen -> Gang -> Path -> (Maybe (Term, Noun) -> IO ()) -> IO ()
  doScry w g p k = scry serf w g p k >> topLoop

  doWork :: EvErr -> IO ()
  doWork firstWorkErr = do
    que   <- newTBMQueueIO 1
    ()    <- atomically (writeTBMQueue que firstWorkErr)
    tWork <- async (processWork serf maxBatchSize que onWorkResp spin)
    nexSt <- workLoop que
    wait tWork
    nexSt

  workLoop :: TBMQueue EvErr -> IO (IO ())
  workLoop que = atomically onInput >>= \case
    CRKill ()      -> atomically (closeTBMQueue que) >> pure (pure ())
    CRSave ()      -> atomically (closeTBMQueue que) >> pure doSave
    CRPack ()      -> atomically (closeTBMQueue que) >> pure doPack
    CRScry w g p k -> atomically (closeTBMQueue que) >> pure (doScry w g p k)
    CRWork workErr -> atomically (writeTBMQueue que workErr) >> workLoop que

  onWorkResp :: Wen -> EvErr -> Work -> IO ()
  onWorkResp wen (EvErr evn err) = \case
    WDone eid hash fx -> do
      atomically $ sendOn ((Fact eid hash wen (toNoun evn)), fx)
    WSwap eid hash (wen, noun) fx -> do
      io $ err (RunSwap eid hash wen noun fx)
      atomically $ sendOn (Fact eid hash wen noun, fx)
    WBail goofs -> do
      io $ err (RunBail goofs)

{-
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

{-
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
  -> (Wen -> EvErr -> Work -> IO ())
  -> (SpinState -> STM ())
  -> IO ()
processWork serf maxSize q onResp spin = do
  vDoneFlag      <- newTVarIO False
  vInFlightQueue <- newTVarIO empty
  recvThread     <- async (recvLoop serf vDoneFlag vInFlightQueue)
  loop vInFlightQueue vDoneFlag
  wait recvThread
 where
  loop :: TVar (Seq (Ev, Work -> IO ())) -> TVar Bool -> IO ()
  loop vInFlight vDone = do
    atomically (pullFromQueueBounded maxSize vInFlight q) >>= \case
      Nothing -> do
        atomically (writeTVar vDone True)
      Just evErr@(EvErr ev _) -> do
        now <- Time.now
        let cb = onRecv (currentEv vInFlight) now evErr
        atomically $ do
          modifyTVar' vInFlight (:|> (ev, cb))
          currentEv vInFlight >>= spin
        sendWrit serf (WWork now ev)
        loop vInFlight vDone

  onRecv :: STM (Maybe Ev) -> Wen -> EvErr -> Work -> IO ()
  onRecv getCurrentEv now evErr work = do
    atomically (getCurrentEv >>= spin)
    onResp now evErr work

  currentEv :: TVar (Seq (Ev, a)) -> STM (Maybe Ev)
  currentEv vInFlight = readTVar vInFlight >>= \case
    (ev, _) :<| _ -> pure (Just ev)
    _             -> pure Nothing

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
recvLoop :: Serf -> TVar Bool -> TVar (Seq (Ev, Work -> IO ())) -> IO ()
recvLoop serf vDone vWork = do
  withSerfLock try serf \SerfState {..} -> do
    loop ssLast ssHash
 where
  loop eve mug = do
    atomically takeCallback >>= \case
      Nothing -> pure (SerfState eve mug, ())
      Just cb -> recvWork serf >>= \case
        work@(WDone eid hash _)   -> cb work >> loop eid hash
        work@(WSwap eid hash _ _) -> cb work >> loop eid hash
        work@(WBail _)            -> cb work >> loop eve mug

  takeCallback :: STM (Maybe (Work -> IO ()))
  takeCallback = do
    ((,) <$> readTVar vDone <*> readTVar vWork) >>= \case
      (False, Empty        ) -> retry
      (True , Empty        ) -> pure Nothing
      (_    , (_, x) :<| xs) -> writeTVar vWork xs $> Just x
      (_    , _            ) -> error "impossible"
