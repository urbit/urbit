{-# OPTIONS_GHC -Wwarn #-}

module Vere.Pier
  ( booted, resumed, pier, runPersist, runCompute, generateBootSeq
  ) where

import UrbitPrelude

import Arvo
import Vere.Pier.Types
import System.Random

import System.Directory   (createDirectoryIfMissing)
import System.Posix.Files (ownerModes, setFileMode)
import Vere.Ames          (ames)
import Vere.Behn          (behn)
import Vere.Http.Server   (serv)
import Vere.Log           (EventLog)
import Vere.Serf          (Serf, SerfState(..), doJob)

import qualified System.Entropy as Ent
import qualified Urbit.Time     as Time
import qualified Vere.Log       as Log
import qualified Vere.Serf      as Serf


--------------------------------------------------------------------------------

_ioDrivers = [] :: [IODriver]

setupPierDirectory :: FilePath -> IO ()
setupPierDirectory shipPath = do
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
    preKern ent = [ EvBlip $ BlipEvTerm $ TermEvBoot (1,()) (Fake (who ident))
                  , EvBlip $ BlipEvArvo $ ArvoEvWhom ()     ship
                  , EvBlip $ BlipEvArvo $ ArvoEvWack ()     ent
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
booted pillPath pierPath flags ship = do
  putStrLn "LOADING PILL"

  pill <- liftIO (loadFile pillPath >>= either throwIO pure)

  putStrLn "PILL LOADED"

  seq@(BootSeq ident x y) <- liftIO $ generateBootSeq ship pill

  putStrLn "BootSeq Computed"

  liftIO (setupPierDirectory pierPath)

  putStrLn "Directory Setup"

  log  <- Log.new (pierPath <> "/.urb/log") ident

  putStrLn "Event Log Initialized"

  serf <- Serf.run (Serf.Config pierPath flags)

  putStrLn "Serf Started"

  liftIO $ do
      (events, serfSt) <- Serf.bootFromSeq serf seq
      putStrLn "Boot Sequence completed"
      Serf.snapshot serf serfSt
      putStrLn "Snapshot taken"
      writeJobs log (fromList events)
      putStrLn "Events written"
      pure (serf, log, serfSt)


-- Resume an existing ship. ----------------------------------------------------

resumed :: FilePath -> Serf.Flags
        -> Acquire (Serf, EventLog, SerfState)
resumed top flags = do
    log    <- Log.existing (top <> "/.urb/log")
    serf   <- Serf.run (Serf.Config top flags)
    serfSt <- liftIO (Serf.replay serf log)

    liftIO (Serf.snapshot serf serfSt)

    pure (serf, log, serfSt)


-- Run Pier --------------------------------------------------------------------

pier :: FilePath
     -> Maybe Port
     -> (Serf, EventLog, SerfState)
     -> Acquire ()
pier pierPath mPort (serf, log, ss) = do
    computeQ <- newTQueueIO :: Acquire (TQueue Ev)
    persistQ <- newTQueueIO :: Acquire (TQueue (Job, FX))
    executeQ <- newTQueueIO :: Acquire (TQueue FX)

    inst <- liftIO (KingId . UV . fromIntegral <$> randomIO @Word16)

    let ship = who (Log.identity log)

    let (bootEvents, startDrivers) =
          drivers pierPath inst ship mPort (writeTQueue computeQ)

    liftIO $ atomically $ for_ bootEvents (writeTQueue computeQ)

    tExe  <- startDrivers >>= router (readTQueue executeQ)
    tDisk <- runPersist log persistQ (writeTQueue executeQ)
    tCpu  <- runCompute serf ss (readTQueue computeQ) (writeTQueue persistQ)

    -- Wait for something to die.

    let ded = asum [ death "effect thread" tExe
                   , death "persist thread" tDisk
                   , death "compute thread" tCpu
                   ]

    atomically ded >>= \case
      Left (txt, exn) -> print ("Somthing died", txt, exn)
      Right tag       -> print ("something simply exited", tag)

death :: Text -> Async () -> STM (Either (Text, SomeException) Text)
death tag tid = do
  waitCatchSTM tid <&> \case
    Left exn -> Left (tag, exn)
    Right () -> Right tag

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

drivers :: FilePath
        -> KingId
        -> Ship
        -> Maybe Port
        -> (Ev -> STM ())
        -> ([Ev], Acquire Drivers)
drivers pierPath inst who mPort plan =
    (initialEvents, runDrivers)
  where
    (behnBorn, runBehn) = behn inst plan
    (amesBorn, runAmes) = ames inst who mPort plan
    (httpBorn, runHttp) = serv pierPath inst plan
    initialEvents       = mconcat [behnBorn, amesBorn, httpBorn]
    runDrivers          = do
        dNewt       <- runAmes
        dBehn       <- runBehn
        dAmes       <- pure $ const $ pure ()
        dHttpClient <- pure $ const $ pure ()
        dHttpServer <- runHttp
        dSync       <- pure $ const $ pure ()
        dTerm       <- pure $ const $ pure ()
        pure (Drivers{..})


-- Route Effects to Drivers ----------------------------------------------------

router :: STM FX -> Drivers -> Acquire (Async ())
router waitFx Drivers{..} = mkAcquire start cancel
  where
    start = async $ forever $ do
        fx <- atomically waitFx
        for_ fx $ \ef -> do
            putStrLn ("[EFFECT]\n" <> pack (ppShow ef) <> "\n\n")
            case ef of
              GoodParse (EfVega _ _)               -> error "TODO"
              GoodParse (EfExit _ _)               -> error "TODO"
              GoodParse (EfVane (VEAmes ef))       -> dAmes ef
              GoodParse (EfVane (VEBehn ef))       -> dBehn ef
              GoodParse (EfVane (VEBoat ef))       -> dSync ef
              GoodParse (EfVane (VEClay ef))       -> dSync ef
              GoodParse (EfVane (VEHttpClient ef)) -> dHttpClient ef
              GoodParse (EfVane (VEHttpServer ef)) -> dHttpServer ef
              GoodParse (EfVane (VENewt ef))       -> dNewt ef
              GoodParse (EfVane (VESync ef))       -> dSync ef
              GoodParse (EfVane (VETerm ef))       -> dTerm ef
              FailParse n                          -> pPrint n


-- Compute Thread --------------------------------------------------------------

runCompute :: Serf -> SerfState -> STM Ev -> ((Job, FX) -> STM ())
           -> Acquire (Async ())
runCompute serf ss getEvent putResult =
    mkAcquire (async (go ss)) cancel
  where
    go :: SerfState -> IO ()
    go ss = do
        ev  <- atomically getEvent
        putStrLn ("[EVENT]\n" <> pack (ppShow ev) <> "\n\n")
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
