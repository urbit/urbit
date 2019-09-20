{-# OPTIONS_GHC -Wwarn #-}

{-
    - TODO: `recvLen` is not big-endian safe.
-}

module Vere.Serf ( Serf, sStderr, SerfState(..), doJob
                 , run, shutdown, kill
                 , replay, bootFromSeq, snapshot
                 , collectFX
                 , Config(..), Flags, Flag(..)
                 ) where

import UrbitPrelude

import Arvo
import Data.Conduit
import System.Process
import System.ProgressBar
import Vere.Pier.Types

import Data.Bits              (setBit)
import Data.ByteString        (hGet)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Foreign.Marshal.Alloc  (alloca)
import Foreign.Ptr            (castPtr)
import Foreign.Storable       (peek, poke)
import System.Exit            (ExitCode)

import qualified Data.ByteString.Unsafe as BS
import qualified Data.Text              as T
import qualified System.IO              as IO
import qualified System.IO.Error        as IO
import qualified Urbit.Ob               as Ob
import qualified Urbit.Time             as Time
import qualified Vere.Log               as Log


-- Serf Config -----------------------------------------------------------------

type Flags = [Flag]

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

compileFlags :: [Flag] -> Word
compileFlags = foldl' (\acc flag -> setBit acc (fromEnum flag)) 0

data Config = Config FilePath [Flag]
  deriving (Show)

serf :: HasLogFunc e => Text -> RIO e ()
serf msg = logInfo $ display ("SERF: " <> msg)


-- Types -----------------------------------------------------------------------

data SerfState = SerfState
    { ssNextEv  :: EventId
    , ssLastMug :: Mug
    }
  deriving (Eq, Ord, Show)

ssLastEv :: SerfState -> EventId
ssLastEv = pred . ssNextEv

data Serf e = Serf
  { sendHandle :: Handle
  , recvHandle :: Handle
  , process    :: ProcessHandle
  , sStderr    :: MVar (Text -> RIO e ())
  }

data ShipId = ShipId Ship Bool
  deriving (Eq, Ord, Show)

type Play = Maybe (EventId, Mug, ShipId)

data Plea
    = PPlay Play
    | PWork Work
    | PDone EventId Mug FX
    | PStdr EventId Cord
    | PSlog EventId Word32 Tank
  deriving (Eq, Show)

type ReplacementEv = Job
type WorkResult    = (SerfState, FX)
type SerfResp      = Either ReplacementEv WorkResult

data SerfExn
    = BadComputeId EventId WorkResult
    | BadReplacementId EventId ReplacementEv
    | UnexpectedPlay EventId Play
    | BadPleaAtom Atom
    | BadPleaNoun Noun [Text] Text
    | ReplacedEventDuringReplay EventId ReplacementEv
    | ReplacedEventDuringBoot   EventId ReplacementEv
    | EffectsDuringBoot         EventId FX
    | SerfConnectionClosed
    | UnexpectedPleaOnNewShip Plea
    | InvalidInitialPlea Plea
  deriving (Show)


-- Instances -------------------------------------------------------------------

instance Exception SerfExn

deriveNoun ''ShipId
deriveNoun ''Plea


-- Utils -----------------------------------------------------------------------

printTank :: HasLogFunc e => MVar (Text -> RIO e ()) -> Word32 -> Tank -> RIO e ()
printTank log _pri tank =
    ((printErr log) . unlines . fmap unTape . wash (WashCfg 0 80)) tank

guardExn :: (Exception e, MonadIO m) => Bool -> e -> m ()
guardExn ok = io . unless ok . throwIO

fromRightExn :: (Exception e, MonadIO m) => Either a b -> (a -> e) -> m b
fromRightExn (Left m)  exn = throwIO (exn m)
fromRightExn (Right x) _   = pure x

printErr :: MVar (Text -> RIO e ()) -> Text -> RIO e ()
printErr m txt = do
  f <- readMVar m
  f txt

-- Process Management ----------------------------------------------------------

run :: HasLogFunc e => Config -> RAcquire e (Serf e)
run config = mkRAcquire (startUp config) tearDown

startUp :: HasLogFunc e => Config -> RIO e (Serf e)
startUp conf@(Config pierPath flags) = do
    logTrace "STARTING SERF"
    logTrace (displayShow conf)

    (i, o, e, p) <- io $ do
        (Just i, Just o, Just e, p) <- createProcess pSpec
        pure (i, o, e, p)

    stderr <- newMVar serf
    async (readStdErr e stderr)
    pure (Serf i o p stderr)
  where
    diskKey = ""
    config  = show (compileFlags flags)
    args    = [pierPath, diskKey, config]
    pSpec   = (proc "urbit-worker" args)
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }

readStdErr :: ∀e. HasLogFunc e => Handle -> MVar (Text -> RIO e ()) -> RIO e ()
readStdErr h print =
    untilEOFExn $ do
        raw <- io $ IO.hGetLine h
        let ln = T.strip (pack raw)
        printErr print ln
        serf ("[stderr] " <> ln)
  where
    eofMsg = "[Serf.readStdErr] serf stderr closed"

    untilEOFExn :: RIO e () -> RIO e ()
    untilEOFExn act = loop
      where
        loop :: RIO e ()
        loop = do
            env <- ask
            res <- io $ IO.tryIOError $ runRIO env act
            case res of
                Left exn | IO.isEOFError exn -> logDebug eofMsg
                Left exn                     -> io (IO.ioError exn)
                Right ()                     -> loop

tearDown :: HasLogFunc e => Serf e -> RIO e ()
tearDown serf = do
    io $ terminateProcess (process serf)
    void $ waitForExit serf

    -- race_ waitThenKill (shutdownAndWait serf 0)
  where
    -- killedMsg =
      -- "[Serf.tearDown]: Serf didn't die when asked, killing it"

    -- waitThenKill = do
        -- threadDelay 1000000
        -- debug killedMsg
        -- terminateProcess (process serf)

waitForExit :: HasLogFunc e => Serf e -> RIO e ExitCode
waitForExit = io . waitForProcess . process

kill :: HasLogFunc e => Serf e -> RIO e ExitCode
kill serf = io (terminateProcess $ process serf) >> waitForExit serf

_shutdownAndWait :: HasLogFunc e => Serf e -> Word8 -> RIO e ExitCode
_shutdownAndWait serf code = do
    shutdown serf code
    waitForExit serf


-- Basic Send and Receive Operations -------------------------------------------

withWord64AsByteString :: Word64 -> (ByteString -> RIO e a) -> RIO e a
withWord64AsByteString w k = do
    env <- ask
    io $ alloca $ \wp -> do
        poke wp w
        bs <- BS.unsafePackCStringLen (castPtr wp, 8)
        runRIO env (k bs)

sendLen :: HasLogFunc e => Serf e -> Int -> RIO e ()
sendLen s i = do
  w <- evaluate (fromIntegral i :: Word64)
  withWord64AsByteString (fromIntegral i) (hPut (sendHandle s))

sendOrder :: HasLogFunc e => Serf e -> Order -> RIO e ()
sendOrder w o = do
  -- logDebug $ display ("(sendOrder) " <> tshow o)
  sendBytes w $ jamBS $ toNoun o
  -- logDebug "(sendOrder) Done"

sendBytes :: HasLogFunc e => Serf e -> ByteString -> RIO e ()
sendBytes s bs = handle ioErr $ do
    sendLen s (length bs)
    hFlush (sendHandle s)

    hack

    hPut (sendHandle s) bs
    hFlush (sendHandle s)

    hack

  where
    ioErr :: IOError -> RIO e ()
    ioErr _ = throwIO SerfConnectionClosed

    -- TODO WHY DOES THIS MATTER?????
    hack = threadDelay 10000

recvLen :: (MonadIO m, HasLogFunc e) => Serf e -> m Word64
recvLen w = io $ do
  bs <- hGet (recvHandle w) 8
  case length bs of
    8 -> unsafeUseAsCString bs (peek . castPtr)
    _ -> throwIO SerfConnectionClosed

recvBytes :: HasLogFunc e => Serf e -> Word64 -> RIO e ByteString
recvBytes serf =
  io . hGet (recvHandle serf) . fromIntegral

recvAtom :: HasLogFunc e => Serf e -> RIO e Atom
recvAtom w = do
    len <- recvLen w
    bs <- recvBytes w len
    pure (packAtom bs)
  where
    packAtom :: ByteString -> Atom
    packAtom = view (from atomBytes)

cordText :: Cord -> Text
cordText = T.strip . unCord


--------------------------------------------------------------------------------

snapshot :: HasLogFunc e => Serf e -> SerfState -> RIO e ()
snapshot serf ss = sendOrder serf $ OSave $ ssLastEv ss

shutdown :: HasLogFunc e => Serf e -> Word8 -> RIO e ()
shutdown serf code = sendOrder serf (OExit code)

{-
    TODO Find a cleaner way to handle `PStdr` Pleas.
-}
recvPlea :: HasLogFunc e => Serf e -> RIO e Plea
recvPlea w = do
  -- logDebug "(recvPlea) Waiting"
  a <- recvAtom w
  -- logDebug "(recvPlea) Got atom"
  n <- fromRightExn (cue a) (const $ BadPleaAtom a)
  p <- fromRightExn (fromNounErr n) (\(p,m) -> BadPleaNoun (traceShowId n) p m)

  case p of PStdr e msg   -> do printErr (sStderr w) (cordText msg)
                                recvPlea w
            PSlog _ pri t -> do printTank (sStderr w) pri t
                                recvPlea w
            _             -> do -- logTrace "recvPlea got something else"
                                pure p

{-
    Waits for initial plea, and then sends boot IPC if necessary.
-}
handshake :: HasLogFunc e => Serf e -> LogIdentity -> RIO e SerfState
handshake serf ident = do
    ss@SerfState{..} <- recvPlea serf >>= \case
      PPlay Nothing          -> pure $ SerfState 1 (Mug 0)
      PPlay (Just (e, m, _)) -> pure $ SerfState e m
      x                      -> throwIO (InvalidInitialPlea x)

    when (ssNextEv == 1) $ do
        sendOrder serf (OBoot ident)

    pure ss

sendWork :: ∀e. HasLogFunc e => Serf e -> Job -> RIO e SerfResp
sendWork w job =
  do
    sendOrder w (OWork job)
    res <- loop
    -- logTrace ("[sendWork] Got response")
    pure res
  where
    eId = jobId job

    produce :: WorkResult -> RIO e SerfResp
    produce (ss@SerfState{..}, o) = do
      guardExn (ssNextEv == (1+eId)) (BadComputeId eId (ss, o))
      pure $ Right (ss, o)

    replace :: ReplacementEv -> RIO e SerfResp
    replace job = do
      guardExn (jobId job == eId) (BadReplacementId eId job)
      pure (Left job)

    loop :: RIO e SerfResp
    loop = recvPlea w >>= \case
      PPlay p       -> throwIO (UnexpectedPlay eId p)
      PDone i m o   -> produce (SerfState (i+1) m, o)
      PWork work    -> replace (DoWork work)
      PStdr _ cord  -> printErr (sStderr w) (cordText cord) >> loop
      PSlog _ pri t -> printTank (sStderr w) pri t >> loop


--------------------------------------------------------------------------------

doJob :: HasLogFunc e => Serf e -> Job -> RIO e (Job, SerfState, FX)
doJob serf job = do
    sendWork serf job >>= \case
        Left replaced  -> doJob serf replaced
        Right (ss, fx) -> pure (job, ss, fx)

bootJob :: HasLogFunc e => Serf e -> Job -> RIO e (Job, SerfState)
bootJob serf job = do
    doJob serf job >>= \case
        (job, ss, _) -> pure (job, ss)
--        (job, ss, fx) -> throwIO (EffectsDuringBoot (jobId job) fx)

replayJob :: HasLogFunc e => Serf e -> Job -> RIO e SerfState
replayJob serf job = do
    sendWork serf job >>= \case
        Left replace  -> throwIO (ReplacedEventDuringReplay (jobId job) replace)
        Right (ss, _) -> pure ss

--------------------------------------------------------------------------------

updateProgressBar :: Int -> Text -> Maybe (ProgressBar ())
                  -> RIO e (Maybe (ProgressBar ()))
updateProgressBar count startMsg = \case
    Nothing -> do
      -- We only construct the progress bar on the first time that we
      -- process an event so that we don't display an empty progress
      -- bar when the snapshot is caught up to the log.
      putStrLn startMsg
      let style = defStyle { stylePostfix = exact }
      pb <- io $ newProgressBar style 10 (Progress 0 count ())
      pure (Just pb)
    Just pb -> do
      io $ incProgress pb 1
      pure (Just pb)

--------------------------------------------------------------------------------

type BootSeqFn = EventId -> Mug -> Time.Wen -> Job

data BootExn = ShipAlreadyBooted
  deriving stock    (Eq, Ord, Show)
  deriving anyclass (Exception)

bootFromSeq :: ∀e. HasLogFunc e => Serf e -> BootSeq -> RIO e ([Job], SerfState)
bootFromSeq serf (BootSeq ident nocks ovums) = do
    handshake serf ident >>= \case
        ss@(SerfState 1 (Mug 0)) -> loop [] ss Nothing bootSeqFns
        _                        -> throwIO ShipAlreadyBooted

  where
    loop :: [Job] -> SerfState -> Maybe (ProgressBar ()) -> [BootSeqFn]
         -> RIO e ([Job], SerfState)
    loop acc ss pb = \case
        []   -> do
          pb        <- updateProgressBar 0 bootMsg pb
          pure (reverse acc, ss)
        x:xs -> do
          wen       <- io Time.now
          job       <- pure $ x (ssNextEv ss) (ssLastMug ss) wen
          pb        <- updateProgressBar (1 + length xs) bootMsg pb
          (job, ss) <- bootJob serf job
          loop (job:acc) ss pb xs

    bootSeqFns :: [BootSeqFn]
    bootSeqFns = fmap muckNock nocks <> fmap muckOvum ovums
      where
        muckNock nok eId mug _   = RunNok $ LifeCyc eId mug nok
        muckOvum ov  eId mug wen = DoWork $ Work eId mug wen ov

    bootMsg = "Booting " ++ (fakeStr (isFake ident)) ++
              (Ob.renderPatp (Ob.patp (fromIntegral (who ident))))
    fakeStr True  = "fake "
    fakeStr False = ""

{-
    The ship is booted, but it is behind. shove events to the worker
    until it is caught up.
-}
replayJobs :: HasLogFunc e
           => Serf e -> Int -> SerfState -> ConduitT Job Void (RIO e) SerfState
replayJobs serf lastEv = go Nothing
  where
    go pb ss = do
      await >>= \case
        Nothing -> pure ss
        Just job -> do
          pb <- lift $ updatePb ss pb
          played <- lift $ replayJob serf job
          go pb played

    updatePb ss = do
      let start = lastEv - (fromIntegral (ssNextEv ss))
      let msg = pack ("Replaying events #" ++ (show (ssNextEv ss)) ++
                      " to #" ++ (show lastEv))
      updateProgressBar start msg


replay :: HasLogFunc e => Serf e -> Log.EventLog -> RIO e SerfState
replay serf log = do
    ss <- handshake serf (Log.identity log)

    lastEv <- Log.lastEv log
    runConduit $  Log.streamEvents log (ssNextEv ss)
               .| toJobs (Log.identity log) (ssNextEv ss)
               .| replayJobs serf (fromIntegral lastEv) ss

toJobs :: HasLogFunc e
       => LogIdentity -> EventId -> ConduitT ByteString Job (RIO e) ()
toJobs ident eId =
    await >>= \case
        Nothing -> pure () -- lift $ logTrace "[toJobs] no more jobs"
        Just at -> do yield =<< lift (fromAtom at)
                      -- lift $ logTrace $ display ("[toJobs] " <> tshow eId)
                      toJobs ident (eId+1)
  where
    isNock = eId <= fromIntegral (lifecycleLen ident)

    fromAtom :: ByteString -> RIO e Job
    fromAtom bs | isNock = do
        noun       <- cueBSExn bs
        (mug, nok) <- fromNounExn noun
        pure $ RunNok (LifeCyc eId mug nok)
    fromAtom bs = do
        noun            <- cueBSExn bs
        (mug, wen, ovm) <- fromNounExn noun
        pure $ DoWork (Work eId mug wen ovm)


-- Collect Effects for Parsing -------------------------------------------------

collectFX :: HasLogFunc e => Serf e -> Log.EventLog -> RIO e ()
collectFX serf log = do
    ss <- handshake serf (Log.identity log)

    runConduit $  Log.streamEvents log (ssNextEv ss)
               .| toJobs (Log.identity log) (ssNextEv ss)
               .| doCollectFX serf ss
               .| persistFX log

persistFX :: Log.EventLog -> ConduitT (EventId, FX) Void (RIO e) ()
persistFX log = loop
  where
    loop = await >>= \case
        Nothing        -> pure ()
        Just (eId, fx) -> do
            lift $ Log.writeEffectsRow log eId (jamBS $ toNoun fx)
            loop

doCollectFX :: ∀e. HasLogFunc e
            => Serf e -> SerfState -> ConduitT Job (EventId, FX) (RIO e) ()
doCollectFX serf = go
  where
    go :: SerfState -> ConduitT Job (EventId, FX) (RIO e) ()
    go ss = await >>= \case
        Nothing -> pure ()
        Just jb -> do
            -- jb <- pure $ replaceMug jb (ssLastMug ss)
            (_, ss, fx) <- lift $ doJob serf jb
            -- lift $ logTrace $ displayShow (jobId jb)
            yield (jobId jb, fx)
            go ss

replaceMug :: Job -> Mug -> Job
replaceMug jb mug =
  case jb of
    DoWork (Work eId _ w o)  -> DoWork (Work eId mug w o)
    RunNok (LifeCyc eId _ n) -> RunNok (LifeCyc eId mug n)
