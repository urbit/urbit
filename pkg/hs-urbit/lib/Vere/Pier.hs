{-# OPTIONS_GHC -Wwarn #-}

module Vere.Pier (booted, resumed, runPersist, runCompute) where

import UrbitPrelude

import Arvo
import Data.Acquire
import Vere.Pier.Types

import System.Directory   (createDirectoryIfMissing)
import System.Posix.Files (ownerModes, setFileMode)
import Vere.Ames          (ames)
import Vere.Behn          (behn)
import Vere.Log           (EventLog)
import Vere.Serf          (Serf, SerfState(..), doJob)

import qualified System.Entropy as Ent
import qualified Urbit.Time     as Time
import qualified Vere.Log       as Log
import qualified Vere.Serf      as Serf


--------------------------------------------------------------------------------

_ioDrivers = [] :: [IODriver]

_setupPierDirectory :: FilePath -> IO ()
_setupPierDirectory shipPath = do
   for_ ["put", "get", "log", "chk"] $ \seg -> do
     let pax = shipPath <> "/.urb/" <> seg
     createDirectoryIfMissing True pax
     setFileMode pax ownerModes


-- Load pill into boot sequence. -----------------------------------------------

genEntropy :: IO Word512
genEntropy = fromIntegral . view (from atomBytes) <$> Ent.getEntropy 64

generateBootSeq :: Ship -> Pill -> IO BootSeq
generateBootSeq ship Pill{..} = do
    ent <- genEntropy
    let ovums = preKern ent <> pKernelOvums <> pUserspaceOvums
    pure $ BootSeq ident pBootFormulas ovums
  where
    ident       = LogIdentity ship True (fromIntegral $ length pBootFormulas)
    blip        = EvBlip
    preKern ent = [ blip $ BlipEvTerm $ TermEvBoot (1,()) (Fake (who ident))
                  , blip $ BlipEvArvo $ ArvoEvWhom ()     ship
                  , blip $ BlipEvArvo $ ArvoEvWack ()     ent
                  ]


-- Write a batch of jobs into the event log ------------------------------------

writeJobs :: EventLog -> Vector Job -> IO ()
writeJobs log !jobs = do
    expect <- Log.nextEv log
    events <- fmap fromList $ traverse fromJob (zip [expect..] $ toList jobs)
    Log.appendEvents log events
  where
    fromJob :: (EventId, Job) -> IO ByteString
    fromJob (expectedId, job) = do
        guard (expectedId == jobId job)
        pure $ jamBS $ jobPayload job

    jobPayload :: Job -> Noun
    jobPayload (RunNok (LifeCyc _ m n)) = toNoun (m, n)
    jobPayload (DoWork (Work _ m d o))  = toNoun (m, d, o)


-- Boot a new ship. ------------------------------------------------------------

booted :: FilePath -> FilePath -> Serf.Flags -> Ship
       -> Acquire (Serf, EventLog, SerfState)
booted pillPath top flags ship = do
  pill <- liftIO $ loadFile @Pill pillPath >>= \case
            Left l  -> error (show l)
            Right p -> pure p

  seq@(BootSeq ident x y) <- liftIO $ generateBootSeq ship pill

  log  <- Log.new (top <> "/.urb/log") ident
  serf <- Serf.run (Serf.Config top flags)

  liftIO $ do
      (events, serfSt) <- Serf.bootFromSeq serf seq
      Serf.snapshot serf serfSt
      writeJobs log (fromList events)
      pure (serf, log, serfSt)


-- Resume an existing ship. ----------------------------------------------------

resumed :: FilePath -> Serf.Flags -> Acquire (Serf, EventLog, SerfState)
resumed top flags = do
    log    <- Log.existing (top <> "/.urb/log")
    serf   <- Serf.run (Serf.Config top flags)
    serfSt <- liftIO (Serf.replay serf log)

    liftIO (Serf.snapshot serf serfSt)

    pure (serf, log, serfSt)


-- Run Pier --------------------------------------------------------------------

pier :: Maybe Port
     -> (Serf, EventLog, SerfState)
     -> Acquire Int
pier mPort (serf, log, ss) = do
    computeQ <- newTQueueIO :: Acquire (TQueue Ev)
    persistQ <- newTQueueIO :: Acquire (TQueue (Job, FX))
    executeQ <- newTQueueIO :: Acquire (TQueue FX)

    let inst = KingInst 0
        ship = who (Log.identity log)

    let (bootEvents, startDrivers) =
          drivers inst ship mPort (writeTQueue computeQ)

    liftIO $ atomically $ for_ bootEvents (writeTQueue computeQ)

    dExe  <- startDrivers >>= router (readTQueue executeQ)
    tDisk <- runPersist log persistQ (writeTQueue executeQ)
    tCpu  <- runCompute serf ss (readTQueue computeQ) (writeTQueue persistQ)

    undefined [dExe, tDisk, tCpu]


-- Start All Drivers -----------------------------------------------------------

data Drivers = Drivers
    { dAmes       :: EffCb AmesEf
    , dBehn       :: EffCb BehnEf
    , dHttpClient :: EffCb HttpClientEf
    , dHttpServer :: EffCb HttpServerEf
    , dNewt       :: EffCb NewtEf
    , dSync       :: EffCb SyncEf
    , dTerm       :: EffCb TermEf
    }

drivers :: KingInstance
        -> Ship
        -> Maybe Port
        -> (Ev -> STM ())
        -> ([Ev], Acquire Drivers)
drivers inst who mPort plan =
    (initialEvents, runDrivers)
  where
    (behnBorn, runBehn) = behn inst plan
    (amesBorn, runAmes) = ames inst who mPort plan
    initialEvents       = mconcat [behnBorn, amesBorn]
    runDrivers          = do
        dNewt       <- runAmes
        dBehn       <- runBehn
        dAmes       <- pure undefined
        dHttpClient <- pure undefined
        dHttpServer <- pure undefined
        dSync       <- pure undefined
        dTerm       <- pure undefined
        pure (Drivers{..})


-- Route Effects to Drivers ----------------------------------------------------

router :: STM FX -> Drivers -> Acquire (Async ())
router waitFx Drivers{..} = mkAcquire start cancel
  where
    start = async $ forever $ do
        fx <- atomically waitFx
        for_ fx $ \case
            EfVega _ _               -> error "TODO"
            EfExit _ _               -> error "TODO"
            EfVane (VEAmes ef)       -> dAmes ef
            EfVane (VEBehn ef)       -> dBehn ef
            EfVane (VEBoat ef)       -> dSync ef
            EfVane (VEClay ef)       -> dSync ef
            EfVane (VEHttpClient ef) -> dHttpClient ef
            EfVane (VEHttpServer ef) -> dHttpServer ef
            EfVane (VENewt ef)       -> dNewt ef
            EfVane (VESync ef)       -> dSync ef
            EfVane (VETerm ef)       -> dTerm ef


-- Compute Thread --------------------------------------------------------------

runCompute :: Serf -> SerfState -> STM Ev -> ((Job, FX) -> STM ())
           -> Acquire (Async ())
runCompute serf ss getEvent putResult =
    mkAcquire (async (go ss)) cancel
  where
    go :: SerfState -> IO ()
    go ss = do
        ev  <- atomically getEvent
        wen <- Time.now
        eId <- pure (ssNextEv ss)
        mug <- pure (ssLastMug ss)

        (job', ss', fx) <- doJob serf (DoWork (Work eId mug wen ev))
        atomically (putResult (job', fx))
        go ss'


-- Persist Thread --------------------------------------------------------------

data PersistExn = BadEventId EventId EventId
  deriving Show

instance Exception PersistExn where
  displayException (BadEventId expected got) =
    unlines [ "Out-of-order event id send to persist thread."
            , "\tExpected " <> show expected <> " but got " <> show got
            ]

runPersist :: EventLog
           -> TQueue (Job, FX)
           -> (FX -> STM ())
           -> Acquire (Async ())
runPersist log inpQ out =
    mkAcquire runThread cancelWait
  where
    cancelWait :: Async () -> IO ()
    cancelWait tid = cancel tid >> wait tid

    runThread :: IO (Async ())
    runThread = asyncBound $ forever $ do
        writs  <- atomically getBatchFromQueue
        events <- validateJobsAndGetBytes (toNullable writs)
        Log.appendEvents log events
        atomically $ for_ writs $ \(_,fx) -> out fx

    validateJobsAndGetBytes :: [(Job, FX)] -> IO (Vector ByteString)
    validateJobsAndGetBytes writs = do
        expect <- Log.nextEv log
        fmap fromList
            $ for (zip [expect..] writs)
            $ \(expectedId, (j, fx)) -> do
                unless (expectedId == jobId j) $
                    throwIO (BadEventId expectedId (jobId j))
                case j of
                  RunNok _ ->
                      error "This shouldn't happen here!"
                  DoWork (Work eId mug wen ev) ->
                      pure $ jamBS $ toNoun (mug, wen, ev)

    getBatchFromQueue :: STM (NonNull [(Job, FX)])
    getBatchFromQueue =
        readTQueue inpQ >>= go . singleton
      where
        go acc =
          tryReadTQueue inpQ >>= \case
            Nothing   -> pure (reverse acc)
            Just item -> go (item <| acc)
