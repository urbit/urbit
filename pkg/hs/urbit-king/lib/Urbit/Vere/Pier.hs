{-|
    Top-Level Pier Management

    This is the code that starts the IO drivers and deals with
    communication between the serf, the log, and the IO drivers.
-}
module Urbit.Vere.Pier
  ( booted, resumed, getSnapshot, pier, runPersist, runCompute, generateBootSeq
  ) where

import Urbit.Prelude

import Control.Monad.Trans.Maybe
import RIO.Directory
import Urbit.Arvo
import Urbit.King.Config
import Urbit.Vere.Pier.Types

import Data.Text              (append)
import System.Posix.Files     (ownerModes, setFileMode)
import Urbit.King.App         (HasConfigDir(..), HasKingId(..),
                               HasStderrLogFunc(..))
import Urbit.Vere.Ames        (ames)
import Urbit.Vere.Behn        (behn)
import Urbit.Vere.Clay        (clay)
import Urbit.Vere.Eyre        (eyre)
import Urbit.Vere.Eyre.Multi  (MultiEyreApi)
import Urbit.Vere.Http.Client (client)
import Urbit.Vere.Log         (EventLog)
import Urbit.Vere.Serf        (Serf, SerfState(..), doJob, sStderr)

import qualified System.Entropy         as Ent
import qualified Urbit.King.API         as King
import qualified Urbit.Time             as Time
import qualified Urbit.Vere.Log         as Log
import qualified Urbit.Vere.Serf        as Serf
import qualified Urbit.Vere.Term        as Term
import qualified Urbit.Vere.Term.API    as Term
import qualified Urbit.Vere.Term.Demux  as Term
import qualified Urbit.Vere.Term.Render as Term


--------------------------------------------------------------------------------

_ioDrivers = [] :: [IODriver]

setupPierDirectory :: FilePath -> RIO e ()
setupPierDirectory shipPath = do
   for_ ["put", "get", "log", "chk"] $ \seg -> do
       let pax = shipPath <> "/.urb/" <> seg
       createDirectoryIfMissing True pax
       io $ setFileMode pax ownerModes


-- Load pill into boot sequence. -----------------------------------------------

genEntropy :: RIO e Word512
genEntropy = fromIntegral . bytesAtom <$> io (Ent.getEntropy 64)

generateBootSeq :: Ship -> Pill -> Bool -> LegacyBootEvent -> RIO e BootSeq
generateBootSeq ship Pill{..} lite boot = do
    ent <- genEntropy
    let ovums = preKern ent <> pKernelOvums <> postKern <> pUserspaceOvums
    pure $ BootSeq ident pBootFormulas ovums
  where
    ident       = LogIdentity ship isFake (fromIntegral $ length pBootFormulas)
    preKern ent = [ EvBlip $ BlipEvArvo $ ArvoEvWhom ()     ship
                  , EvBlip $ BlipEvArvo $ ArvoEvWack ()     ent
                  ]
    postKern = [ EvBlip $ BlipEvTerm $ TermEvBoot (1,()) lite boot ]
    isFake = case boot of
      Fake _ -> True
      _      -> False


-- Write a batch of jobs into the event log ------------------------------------

writeJobs :: EventLog -> Vector Job -> RIO e ()
writeJobs log !jobs = do
    expect <- Log.nextEv log
    events <- fmap fromList $ traverse fromJob (zip [expect..] $ toList jobs)
    Log.appendEvents log events
  where
    fromJob :: (EventId, Job) -> RIO e ByteString
    fromJob (expectedId, job) = do
        unless (expectedId == jobId job) $
            error $ show ("bad job id!", expectedId, jobId job)
        pure $ jamBS $ jobPayload job

    jobPayload :: Job -> Noun
    jobPayload (RunNok (LifeCyc _ m n)) = toNoun (m, n)
    jobPayload (DoWork (Work _ m d o))  = toNoun (m, d, o)


-- Boot a new ship. ------------------------------------------------------------

booted :: (HasPierConfig e, HasStderrLogFunc e, HasLogFunc e)
       => Pill -> Bool -> Serf.Flags -> Ship -> LegacyBootEvent
       -> RAcquire e (Serf e, EventLog, SerfState)
booted pill lite flags ship boot = do
  seq@(BootSeq ident x y) <- rio $ generateBootSeq ship pill lite boot

  rio $ logTrace "BootSeq Computed"

  pierPath <- view pierPathL

  liftRIO (setupPierDirectory pierPath)

  rio $ logTrace "Directory Setup"

  log  <- Log.new (pierPath <> "/.urb/log") ident

  rio $ logTrace "Event Log Initialized"

  serf <- Serf.run (Serf.Config pierPath flags)

  rio $ logTrace "Serf Started"

  rio $ do
      (events, serfSt) <- Serf.bootFromSeq serf seq
      logTrace "Boot Sequence completed"
      Serf.snapshot serf serfSt
      logTrace "Snapshot taken"
      writeJobs log (fromList events)
      logTrace "Events written"
      pure (serf, log, serfSt)


-- Resume an existing ship. ----------------------------------------------------

resumed :: (HasStderrLogFunc e, HasPierConfig e, HasLogFunc e)
        => Maybe Word64 -> Serf.Flags
        -> RAcquire e (Serf e, EventLog, SerfState)
resumed event flags = do
    rio $ logTrace "Resuming ship"
    top    <- view pierPathL
    tap    <- fmap (fromMaybe top) $ rio $ runMaybeT $ do
                ev <- MaybeT (pure event)
                MaybeT (getSnapshot top ev)

    rio $ logTrace $ display @Text ("pier: " <> pack top)
    rio $ logTrace $ display @Text ("running serf in: " <> pack tap)

    log    <- Log.existing (top <> "/.urb/log")

    serf   <- Serf.run (Serf.Config tap flags)

    serfSt <- rio $ Serf.replay serf log event

    rio $ Serf.snapshot serf serfSt

    pure (serf, log, serfSt)

getSnapshot :: forall e. FilePath -> Word64 -> RIO e (Maybe FilePath)
getSnapshot top last = do
    lastSnapshot <- lastMay <$> listReplays
    pure (replayToPath <$> lastSnapshot)
  where
    replayDir        = top </> ".partial-replay"
    replayToPath eId = replayDir </> show eId

    listReplays :: RIO e [Word64]
    listReplays = do
        createDirectoryIfMissing True replayDir
        snapshotNums <- mapMaybe readMay <$> listDirectory replayDir
        pure $ sort (filter (<= fromIntegral last) snapshotNums)


-- Run Pier --------------------------------------------------------------------

acquireWorker :: RIO e () -> RAcquire e (Async ())
acquireWorker act = mkRAcquire (async act) cancel

pier
  :: forall e
   . ( HasConfigDir e
     , HasLogFunc e
     , HasNetworkConfig e
     , HasPierConfig e
     , HasKingId e
     )
  => (Serf e, EventLog, SerfState)
  -> MVar ()
  -> MultiEyreApi
  -> RAcquire e ()
pier (serf, log, ss) mStart multi = do
    computeQ  <- newTQueueIO
    persistQ  <- newTQueueIO
    executeQ  <- newTQueueIO
    saveM     <- newEmptyTMVarIO
    shutdownM <- newEmptyTMVarIO

    kapi ← King.kingAPI

    termApiQ <- atomically $ do
        q <- newTQueue
        writeTVar (King.kTermConn kapi) (Just $ writeTQueue q)
        pure q

    let shutdownEvent = putTMVar shutdownM ()

    -- (sz, local) <- Term.localClient

    -- (waitExternalTerm, termServPort) <- Term.termServer

    (demux, muxed) <- atomically $ do
        res <- Term.mkDemux
        --  Term.addDemux local res
        pure (res, Term.useDemux res)

    -- rio $ logInfo $ display $
        -- "TERMSERV Terminal Server running on port: " <> tshow termServPort

    acquireWorker $ forever $ do
            logTrace "TERMSERV Waiting for external terminal."
            atomically $ do
                ext <- Term.connClient <$> readTQueue termApiQ
                Term.addDemux ext demux
            logTrace "TERMSERV External terminal connected."

    swapMVar (sStderr serf) (atomically . Term.trace muxed)

    let logId = Log.identity log
    let ship = who logId

    -- Our call above to set the logging function which echos errors from the
    -- Serf doesn't have the appended \r\n because those \r\n s are added in
    -- the c serf code. Logging output from our haskell process must manually
    -- add them.
    let showErr = atomically . Term.trace muxed . (flip append "\r\n")

    env <- ask

    let (bootEvents, startDrivers) =
            drivers env multi ship (isFake logId)
                (writeTQueue computeQ)
                shutdownEvent
                (Term.TSize{tsWide=80, tsTall=24}, muxed)
                showErr

    io $ atomically $ for_ bootEvents (writeTQueue computeQ)

    tExe  <- startDrivers >>= router (readTQueue executeQ)
    tDisk <- runPersist log persistQ (writeTQueue executeQ)
    tCpu  <- runCompute serf ss
               (readTQueue computeQ)
               (takeTMVar saveM)
               (takeTMVar shutdownM)
               (Term.spin muxed)
               (Term.stopSpin muxed)
               (writeTQueue persistQ)

    tSaveSignal <- saveSignalThread saveM

    putMVar mStart ()

    -- Wait for something to die.

    let ded = asum [ death "effect thread" tExe
                   , death "persist thread" tDisk
                   , death "compute thread" tCpu
                   ]

    atomically ded >>= \case
      Left (txt, exn) -> logError $ displayShow ("Somthing died", txt, exn)
      Right tag       -> logError $ displayShow ("Something simply exited", tag)

    atomically $ (Term.spin muxed) (Just "shutdown")


death :: Text -> Async () -> STM (Either (Text, SomeException) Text)
death tag tid = do
  waitCatchSTM tid <&> \case
    Left exn -> Left (tag, exn)
    Right () -> Right tag

saveSignalThread :: TMVar () -> RAcquire e (Async ())
saveSignalThread tm = mkRAcquire start cancel
  where
    start = async $ forever $ do
      threadDelay (120 * 1000000) -- 120 seconds
      atomically $ putTMVar tm ()

-- Start All Drivers -----------------------------------------------------------

data Drivers e = Drivers
    { dAmes       :: EffCb e AmesEf
    , dBehn       :: EffCb e BehnEf
    , dHttpClient :: EffCb e HttpClientEf
    , dHttpServer :: EffCb e HttpServerEf
    , dNewt       :: EffCb e NewtEf
    , dSync       :: EffCb e SyncEf
    , dTerm       :: EffCb e TermEf
    }

drivers
  :: (HasLogFunc e, HasKingId e, HasNetworkConfig e, HasPierConfig e)
  => e
  -> MultiEyreApi
  -> Ship
  -> Bool
  -> (Ev -> STM ())
  -> STM ()
  -> (Term.TSize, Term.Client)
  -> (Text -> RIO e ())
  -> ([Ev], RAcquire e (Drivers e))
drivers env multi who isFake plan shutdownSTM termSys stderr =
    (initialEvents, runDrivers)
  where
    (behnBorn, runBehn) = behn env plan
    (amesBorn, runAmes) = ames env who isFake plan stderr
    (httpBorn, runHttp) = eyre env multi who plan isFake
    (clayBorn, runClay) = clay env plan
    (irisBorn, runIris) = client env plan
    (termBorn, runTerm) = Term.term env termSys shutdownSTM plan
    initialEvents       = mconcat [behnBorn, clayBorn, amesBorn, httpBorn,
                                   termBorn, irisBorn]
    runDrivers          = do
        dNewt       <- runAmes
        dBehn       <- liftAcquire $ runBehn
        dAmes       <- pure $ const $ pure ()
        dHttpClient <- runIris
        dHttpServer <- runHttp
        dSync       <- runClay
        dTerm       <- runTerm
        pure (Drivers{..})


-- Route Effects to Drivers ----------------------------------------------------

router :: HasLogFunc e => STM FX -> Drivers e -> RAcquire e (Async ())
router waitFx Drivers{..} =
    mkRAcquire start cancel
  where
    start = async $ forever $ do
        fx <- atomically waitFx
        for_ fx $ \ef -> do
            logEffect ef
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
              FailParse n                          -> logError
                                                    $ display
                                                    $ pack @Text (ppShow n)


-- Compute Thread --------------------------------------------------------------

data ComputeRequest
    = CREvent Ev
    | CRSave ()
    | CRShutdown ()
  deriving (Eq, Show)

logEvent :: HasLogFunc e => Ev -> RIO e ()
logEvent ev =
    logDebug $ display $ "[EVENT]\n" <> pretty
  where
    pretty :: Text
    pretty = pack $ unlines $ fmap ("\t" <>) $ lines $ ppShow ev

logEffect :: HasLogFunc e => Lenient Ef -> RIO e ()
logEffect ef =
    logDebug $ display $ "[EFFECT]\n" <> pretty ef
  where
    pretty :: Lenient Ef -> Text
    pretty = \case
       GoodParse e -> pack $ unlines $ fmap ("\t" <>) $ lines $ ppShow e
       FailParse n -> pack $ unlines $ fmap ("\t" <>) $ lines $ ppShow n

runCompute :: ∀e. HasLogFunc e
           => Serf e
           -> SerfState
           -> STM Ev
           -> STM ()
           -> STM ()
           -> (Maybe Text -> STM ())
           -> STM ()
           -> ((Job, FX) -> STM ())
           -> RAcquire e (Async ())
runCompute serf ss getEvent getSaveSignal getShutdownSignal
           showSpinner hideSpinner putResult =
    mkRAcquire (async (go ss)) cancel
  where
    go :: SerfState -> RIO e ()
    go ss = do
        cr  <- atomically $
          CRShutdown <$> getShutdownSignal <|>
          CRSave     <$> getSaveSignal     <|>
          CREvent    <$> getEvent
        case cr of
          CREvent ev -> do
            logEvent ev
            wen <- io Time.now
            eId <- pure (ssNextEv ss)
            mug <- pure (ssLastMug ss)

            atomically $ showSpinner (getSpinnerNameForEvent ev)
            (job', ss', fx) <- doJob serf $ DoWork $ Work eId mug wen ev
            atomically $ hideSpinner
            atomically (putResult (job', fx))
            go ss'
          CRSave () -> do
            logDebug $ "Taking periodic snapshot"
            Serf.snapshot serf ss
            go ss
          CRShutdown () -> do
            -- When shutting down, we first request a snapshot, and then we
            -- just exit this recursive processing, which will cause the serf
            -- to exit from its RAcquire.
            logDebug $ "Shutting down compute system..."
            Serf.snapshot serf ss
            pure ()


-- Persist Thread --------------------------------------------------------------

data PersistExn = BadEventId EventId EventId
  deriving Show

instance Exception PersistExn where
  displayException (BadEventId expected got) =
    unlines [ "Out-of-order event id send to persist thread."
            , "\tExpected " <> show expected <> " but got " <> show got
            ]

runPersist :: ∀e. (HasPierConfig e, HasLogFunc e)
           => EventLog
           -> TQueue (Job, FX)
           -> (FX -> STM ())
           -> RAcquire e (Async ())
runPersist log inpQ out =
    mkRAcquire runThread cancel
  where
    runThread :: RIO e (Async ())
    runThread = asyncBound $ do
        dryRun <- view dryRunL
        forever $ do
            writs  <- atomically getBatchFromQueue
            unless dryRun $ do
                events <- validateJobsAndGetBytes (toNullable writs)
                Log.appendEvents log events
            atomically $ for_ writs $ \(_,fx) -> out fx

    validateJobsAndGetBytes :: [(Job, FX)] -> RIO e (Vector ByteString)
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
