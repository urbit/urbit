{-
|%
::  +writ: from king to serf
::
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
--
-}

module Urbit.Vere.Serf.IPC where

import Urbit.Prelude hiding ((<|))

import Data.Conduit
import Urbit.Arvo
import Urbit.Vere.Pier.Types hiding (Work)
import System.Process
import Data.Bits

import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr           (castPtr)
import Foreign.Storable      (peek, poke)
import RIO.Prelude           (decodeUtf8Lenient)
import Urbit.Time            (Wen)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import qualified System.IO.Error        as IO
import qualified Urbit.Time             as Time


-- IPC Types -------------------------------------------------------------------

type Gang = Maybe (HoonSet Ship)

type Goof = (Term, [Tank])

data Live
  = LExit Atom
  | LSave EventId
  | LPack EventId
 deriving (Show)

type PlayBail = (EventId, Mug, Goof)

data Play
  = PDone Mug
  | PBail PlayBail
 deriving (Show)

data Work
  = WDone EventId Mug [Ef]
  | WSwap EventId Mug (Wen, Noun) [Ef]
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

data Serf = Serf
  { serfSend :: Handle
  , serfRecv :: Handle
  , serfProc :: ProcessHandle
  , serfSlog :: Slog -> IO ()
  , serfLock :: MVar SerfState
  }


-- API Types -------------------------------------------------------------------

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

data Config = Config -- binary, directory, &c
  { scSerf :: FilePath
  , scPier :: FilePath
  , scFlag :: [Flag]
  , scSlog :: Slog -> IO ()
  , scStdr :: Text -> IO ()
  , scDead :: IO ()
  }

data RunError
  = RunBail [Goof]
  | RunSwap EventId Mug Wen Noun [Ef]

data RunInput
  = RunSnap
  | RunPack
  | RunPeek Wen Gang Path (Maybe (Term, Noun) -> IO ())
  | RunWork Ev (RunError -> IO ())

data RunOutput = RunOutput EventId Mug Wen Noun [Ef]


-- Exceptions ------------------------------------------------------------------

data SerfExn
--  = BadComputeId EventId WorkResult
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

serfCurrentStateBlocking :: Serf -> IO SerfState
serfCurrentStateBlocking Serf{serfLock} = readMVar serfLock

-- Low Level IPC Functions -----------------------------------------------------

fromRightExn :: (Exception e, MonadIO m) => Either a b -> (a -> e) -> m b
fromRightExn (Left m)  exn = throwIO (exn m)
fromRightExn (Right x) _   = pure x

withWord64AsByteString :: Word64 -> (ByteString -> IO a) -> IO a
withWord64AsByteString w k = alloca $ \wp -> do
  poke wp w
  bs <- BS.unsafePackCStringLen (castPtr wp, 8)
  k bs

sendLen :: Serf -> Int -> IO ()
sendLen s i = do
  w <- evaluate (fromIntegral i :: Word64)
  withWord64AsByteString (fromIntegral i) (hPut (serfSend s))

sendBytes :: Serf -> ByteString -> IO ()
sendBytes s bs = handle onIOError $ do
  sendLen s (length bs)
  hPut (serfSend s) bs
  hFlush (serfSend s)
 where
  onIOError :: IOError -> IO ()
  onIOError = const (throwIO SerfConnectionClosed)

recvBytes :: Serf -> Word64 -> IO ByteString
recvBytes serf = io . BS.hGet (serfRecv serf) . fromIntegral

recvLen :: Serf -> IO Word64
recvLen w = do
  bs <- BS.hGet (serfRecv w) 8
  case length bs of
    8 -> BS.unsafeUseAsCString bs (peek . castPtr)
    _ -> throwIO SerfConnectionClosed

recvAtom :: Serf -> IO Atom
recvAtom w = do
  len <- recvLen w
  bytesAtom <$> recvBytes w len


-- Send Writ / Recv Plea -------------------------------------------------------

sendWrit :: Serf -> Writ -> IO ()
sendWrit s w = do
  sendBytes s $ jamBS $ toNoun w

recvPlea :: Serf -> IO Plea
recvPlea w = do
  a <- recvAtom w
  n <- fromRightExn (cue a) (const $ BadPleaAtom a)
  p <- fromRightExn (fromNounErr n) (\(p, m) -> BadPleaNoun n p m)
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


-- Request-Response Points -----------------------------------------------------

sendSnapshotRequest :: Serf -> EventId -> IO ()
sendSnapshotRequest serf eve = do
  sendWrit serf (WLive $ LSave eve)
  recvLive serf

compact :: Serf -> EventId -> IO ()
compact serf eve = do
  sendWrit serf (WLive $ LPack eve)
  recvLive serf

scry :: Serf -> Wen -> Gang -> Path -> IO (Maybe (Term, Noun))
scry serf w g p = do
  sendWrit serf (WPeek w g p)
  recvPeek serf


-- Serf Usage Flows ------------------------------------------------------------

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
  async (readStdErr e onStdr onDead)
  vLock <- newEmptyMVar
  let serf = Serf i o p onSlog vLock
  info <- recvRipe serf -- Gross: recvRipe doesn't care about lock so this is fine.
  putMVar vLock (siStat info)
  pure (serf, info)
 where
  diskKey = ""
  config  = show (compileFlags flags)
  args    = [pierPath, diskKey, config]
  pSpec   = (proc exePax args) { std_in  = CreatePipe
                               , std_out = CreatePipe
                               , std_err = CreatePipe
                               }

{-
  TODO wait for process exit?
  TODO force shutdown after time period? Not our job?
-}
shutdown :: Serf -> Atom -> IO ()
shutdown serf exitCode = do
  sendWrit serf (WLive $ LExit exitCode)
  pure ()

bootSeq :: Serf -> [Noun] -> IO (Maybe PlayBail)
bootSeq serf@Serf{..} seq = do
  oldInfo <- takeMVar serfLock
  sendWrit serf (WPlay 1 seq)
  (res, newInfo) <- recvPlay serf >>= \case
    PBail bail   -> pure (Just bail, oldInfo)
    PDone newMug -> pure (Nothing, SerfState (fromIntegral $ length seq) newMug)
  putMVar serfLock newInfo
  pure res

{-
  If this throws an exception, the serf will be in an unusable state. Kill
  the process.

  TODO Take advantage of IPC support for batching.
  TODO Maybe take snapshots
-}
replay
  :: forall m
   . MonadIO m
  => Serf
  -> ConduitT Noun Void m (Maybe PlayBail)
replay serf = do
  initState <- takeMVar (serfLock serf)
  (mErr, newState) <- loop initState
  putMVar (serfLock serf) newState
  pure mErr
 where
  loop :: SerfState -> ConduitT Noun Void m (Maybe PlayBail, SerfState)
  loop (SerfState lastEve lastMug) = do
   await >>= \case
    Nothing -> pure (Nothing, SerfState lastEve lastMug)
    Just ev -> do
      let newEve = lastEve + 1
      io $ sendWrit serf (WPlay newEve [ev])
      res <- io (recvPlay serf) >>= \case
        PBail bail   -> pure (Just bail, SerfState lastEve lastMug)
        PDone newMug -> loop (SerfState newEve newMug)
      pure res

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _    = pure ()
whenJust (Just a) act = act a

{-
  If this throws an exception, the serf will be in an unusable state. Kill
  the process.

  TODO callbacks on snapshot and compaction?
  TODO Take advantage of async IPC to fill pipe with more than one thing.
-}
running
  :: forall m
   . MonadIO m
  => Serf
  -> (Maybe RunInput -> IO ())
  -> ConduitT RunInput RunOutput m ()
running serf notice = do
  SerfState {..} <- takeMVar (serfLock serf)
  newState       <- loop ssHash ssLast
  putMVar (serfLock serf) newState
  pure ()
 where
  loop :: Mug -> EventId -> ConduitT RunInput RunOutput m SerfState
  loop mug eve = do
    print "Serf.running.loop"
    io (notice Nothing)
    nex <- await
    print ("Serf.running.loop: Got something")
    io (notice nex)
    nex & \case
      Nothing -> do
        pure $ SerfState eve mug
      Just RunSnap -> do
        io (sendSnapshotRequest serf eve)
        loop mug eve
      Just RunPack -> do
        io (compact serf eve)
        loop mug eve
      Just (RunPeek wen gang pax act) -> do
        res <- io (scry serf wen gang pax)
        io (act res)
        loop mug eve
      Just (RunWork evn err) -> do
        wen <- io Time.now
        io (sendWrit serf (WWork wen evn))
        io (recvWork serf) >>= \case
          WDone eid hash fx -> do
            yield (RunOutput eid hash wen (toNoun evn) fx)
            loop hash eid
          WSwap eid hash (wen, noun) fx -> do
            io $ err (RunSwap eid hash wen noun fx)
            yield (RunOutput eid hash wen noun fx)
            loop hash eid
          WBail goofs -> do
            io $ err (RunBail goofs)
            loop mug eve
