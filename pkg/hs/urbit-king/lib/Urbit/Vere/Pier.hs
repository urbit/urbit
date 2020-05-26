{-# OPTIONS_GHC -Wwarn #-}

{-|
    Top-Level Pier Management

    This is the code that starts the IO drivers and deals with
    communication between the serf, the log, and the IO drivers.
-}
module Urbit.Vere.Pier
  ( booted
  , runSerf
  , resumed
  , getSnapshot
  , pier
  , runPersist
  , runCompute
  , generateBootSeq
  )
where

import Urbit.Prelude

import RIO.Directory
import System.Random
import Urbit.Arvo
import Urbit.King.Config
import Urbit.Vere.Pier.Types
import Control.Monad.Trans.Maybe

import Data.Text              (append)
import System.Posix.Files     (ownerModes, setFileMode)
import Urbit.King.App         (HasConfigDir(..), HasStderrLogFunc(..))
import Urbit.Vere.Ames        (ames)
import Urbit.Vere.Behn        (behn)
import Urbit.Vere.Clay        (clay)
import Urbit.Vere.Http.Client (client)
import Urbit.Vere.Http.Server (serv)
import Urbit.Vere.Log         (EventLog)
import Urbit.Vere.Serf        (Serf, SerfState(..))
import Data.Conduit

import qualified System.Entropy         as Ent
import qualified Urbit.King.API         as King
import qualified Urbit.Time             as Time
import qualified Urbit.Vere.Log         as Log
import qualified Urbit.Vere.Serf        as Serf
import qualified Urbit.Vere.Term        as Term
import qualified Urbit.Vere.Term.API    as Term
import qualified Urbit.Vere.Term.Demux  as Term
import qualified Urbit.Vere.Term.Render as Term
import qualified Data.Conduit.Combinators as CC


--------------------------------------------------------------------------------

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

runSerf :: HasLogFunc e => FilePath -> [Serf.Flag] -> RAcquire e Serf
runSerf pax fax = fst <$> Serf.withSerf config
 where
  config = Serf.Config
    { scSerf = "urbit-worker"
    , scPier = pax
    , scFlag = fax
    , scSlog = \slog -> print ("slog", slog) -- TODO error "TODO: slog"
    , scStdr = \stdr -> print ("stdr", stdr) -- TODO error "TODO: stdr"
    , scDead = pure () -- error "TODO: dead"
    }

bootSeqJobs :: Time.Wen -> BootSeq -> [Job]
bootSeqJobs now (BootSeq ident nocks ovums) = zipWith ($) bootSeqFns [1 ..]
 where
  wen :: EventId -> Time.Wen
  wen off = Time.addGap now ((fromIntegral off - 1) ^. from Time.microSecs)

  bootSeqFns :: [EventId -> Job]
  bootSeqFns = fmap muckNock nocks <> fmap muckOvum ovums
   where
    muckNock nok eId = RunNok $ LifeCyc eId 0 nok
    muckOvum ov eId = DoWork $ Work eId 0 (wen eId) ov

{-
    loop :: [Job] -> SerfState -> Maybe (ProgressBar ()) -> [BootSeqFn]
         -> RIO e ([Job], SerfState)
    loop acc ss pb = \case
        []   -> do
          pb        <- logStderr (updateProgressBar 0 bootMsg pb)
          pure (reverse acc, ss)
        x:xs -> do
          wen       <- io Time.now
          job       <- pure $ x (ssNextEv ss) (ssLastMug ss) wen
          pb        <- logStderr (updateProgressBar (1 + length xs) bootMsg pb)
          (job, ss) <- bootJob serf job
          loop (job:acc) ss pb xs
-}

bootNewShip
  :: (HasPierConfig e, HasStderrLogFunc e, HasLogFunc e)
  => Pill
  -> Bool
  -> [Serf.Flag]
  -> Ship
  -> LegacyBootEvent
  -> RIO e ()
bootNewShip pill lite flags ship bootEv = do
  seq@(BootSeq ident x y) <- generateBootSeq ship pill lite bootEv
  logTrace "BootSeq Computed"

  pierPath <- view pierPathL

  liftRIO (setupPierDirectory pierPath)
  logTrace "Directory setup."

  rwith (Log.new (pierPath <> "/.urb/log") ident) $ \log -> do
    logTrace "Event log initialized."
    jobs <- (\now -> bootSeqJobs now seq) <$> io Time.now
    writeJobs log (fromList jobs)

  logTrace "Finsihed populating event log with boot sequence"

booted :: (HasPierConfig e, HasStderrLogFunc e, HasLogFunc e)
       => Pill -> Bool -> [Serf.Flag] -> Ship -> LegacyBootEvent
       -> RAcquire e (Serf, EventLog)
booted pill lite flags ship boot = do
  rio $ bootNewShip pill lite flags ship boot
  resumed Nothing flags


-- Resume an existing ship. ----------------------------------------------------

resumed
  :: (HasStderrLogFunc e, HasPierConfig e, HasLogFunc e)
  => Maybe Word64
  -> [Serf.Flag]
  -> RAcquire e (Serf, EventLog)
resumed replayUntil flags = do
  rio $ logTrace "Resuming ship"
  top <- view pierPathL
  tap <- fmap (fromMaybe top) $ rio $ runMaybeT $ do
    ev <- MaybeT (pure replayUntil)
    MaybeT (getSnapshot top ev)

  rio $ logTrace $ display @Text ("pier: " <> pack top)
  rio $ logTrace $ display @Text ("running serf in: " <> pack tap)

  log    <- Log.existing (top <> "/.urb/log")
  serf   <- runSerf tap flags

  rio $ do
    logTrace "Replaying events"
    Serf.execReplay serf log replayUntil
    logTrace "Taking snapshot"
    Serf.execSnapshot serf
    logTrace "Shuting down the serf"

  pure (serf, log)

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

pier :: ∀e. (HasConfigDir e, HasLogFunc e, HasNetworkConfig e, HasPierConfig e)
     => (Serf, EventLog)
     -> TVar (Text -> IO ())
     -> MVar ()
     -> RAcquire e ()
pier (serf, log) vStderr mStart = do
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

    inst <- io (KingId . UV . fromIntegral <$> randomIO @Word16)

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

    atomically $ writeTVar vStderr (atomically . Term.trace muxed)

    let logId = Log.identity log
    let ship = who logId

    -- Our call above to set the logging function which echos errors from the
    -- Serf doesn't have the appended \r\n because those \r\n s are added in
    -- the c serf code. Logging output from our haskell process must manually
    -- add them.
    let showErr = atomically . Term.trace muxed . (flip append "\r\n")
    let (bootEvents, startDrivers) =
            drivers inst ship (isFake logId)
                (writeTQueue computeQ)
                shutdownEvent
                (Term.TSize{tsWide=80, tsTall=24}, muxed)
                showErr

    io $ atomically $ for_ bootEvents (writeTQueue computeQ)

    let stubErrCallback = \_ -> pure ()

    tExe  <- startDrivers >>= router (readTQueue executeQ)
    tDisk <- runPersist log persistQ (writeTQueue executeQ)
    tCpu  <- runCompute serf
               ((,stubErrCallback) <$> readTQueue computeQ)
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
      Right tag       -> logError $ displayShow ("something simply exited", tag)

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

drivers :: (HasLogFunc e, HasNetworkConfig e, HasPierConfig e)
        => KingId -> Ship -> Bool -> (Ev -> STM ())
        -> STM()
        -> (Term.TSize, Term.Client)
        -> (Text -> RIO e ())
        -> ([Ev], RAcquire e (Drivers e))
drivers inst who isFake plan shutdownSTM termSys stderr =
    (initialEvents, runDrivers)
  where
    (behnBorn, runBehn) = behn inst plan
    (amesBorn, runAmes) = ames inst who isFake plan stderr
    (httpBorn, runHttp) = serv inst plan isFake
    (clayBorn, runClay) = clay inst plan
    (irisBorn, runIris) = client inst plan
    (termBorn, runTerm) = Term.term termSys shutdownSTM inst plan
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

data ComputeRequest
    = CREvent (Ev, Serf.RunError -> IO ())
    | CRSave ()
    | CRShutdown ()

runCompute
  :: forall e
   . HasLogFunc e
  => Serf
  -> STM (Ev, Serf.RunError -> IO ())
  -> STM ()
  -> STM ()
  -> (Maybe Text -> STM ())
  -> STM ()
  -> ((Job, FX) -> STM ())
  -> RAcquire e (Async ())
runCompute serf getEvent getSaveSignal getShutdownSignal showSpinner hideSpinner putResult = do
  mkRAcquire (async $ newRunCompute serf config) cancel
 where
  config = ComputeConfig
    { ccOnWork = getEvent
    , ccOnKill = getShutdownSignal
    , ccOnSave = getSaveSignal
    , ccPutResult = putResult
    , ccShowSpinner = showSpinner
    , ccHideSpinner = hideSpinner
    }

-- data RunOutput = RunOutput EventId Mug Wen (Either Noun Ev) [Ef]
-- data Work = Work EventId Mug Wen Ev

{-
data ComputeRequest
    = CREvent Ev (Serf.RunError -> IO ())
    | CRSave ()
    | CRShutdown ()
  deriving (Eq, Show)
-}

{-
  TODO Pack and Peek
-}
ipcSource
  :: forall e
   . HasLogFunc e
  => STM (Ev, Serf.RunError -> IO ())
  -> STM ()
  -> STM ()
  -> ConduitT () Serf.RunInput (RIO e) ()
ipcSource onEvent onSave onKill = loop
 where
  loop :: ConduitT () Serf.RunInput (RIO e) ()
  loop = do
    lift $ logTrace "ipcSource waiting for work request."
    let down = CRShutdown <$> onKill
    let save = CRSave <$> onSave
    let work = CREvent <$> onEvent
    atomically (down <|> save <|> work) >>= \case
      CRShutdown () -> do
        pure ()
      CRSave () -> do
        lift $ logTrace "ipcSource: requesting snapshot"
        yield Serf.RunSnap
        loop
      CREvent (ev, cb) -> do
        lift $ logTrace "ipcSource: requesting work"
        yield (Serf.RunWork ev cb)
        loop

fromRightErr :: Either a b -> IO b
fromRightErr (Left l) = error "unexpected Left value"
fromRightErr (Right r) = pure r

data ComputeConfig = ComputeConfig
  { ccOnWork :: STM (Ev, Serf.RunError -> IO ())
  , ccOnKill :: STM ()
  , ccOnSave :: STM ()
  , ccPutResult :: (Job, FX) -> STM ()
  , ccShowSpinner :: Maybe Text -> STM ()
  , ccHideSpinner :: STM ()
  }

newRunCompute
  :: forall e . HasLogFunc e => Serf.Serf -> ComputeConfig -> RIO e ()
newRunCompute serf ComputeConfig {..} = do
  logTrace "newRunCompute"
  runConduit
    $  ipcSource ccOnWork ccOnSave ccOnKill
    .| Serf.running serf (atomically . onStatusChange)
    .| sendResults
 where
  sendResults :: ConduitT Serf.RunOutput Void (RIO e) ()
  sendResults = await >>= \case
    Nothing                               -> pure ()
    Just (Serf.RunOutput e m w nounEv fx) -> do
      lift $ logTrace "newRunCompute: Got play result"
      ev <- io $ fromRightErr nounEv -- TODO
      let job :: Job = DoWork $ Work e m w ev
      atomically (ccPutResult ((job, GoodParse <$> fx))) -- TODO GoodParse
      sendResults

  onStatusChange :: Maybe Serf.RunInput -> STM ()
  onStatusChange = \case
    Nothing                  -> ccHideSpinner
    Just (Serf.RunWork ev _) -> ccShowSpinner (getSpinnerNameForEvent ev)
    _                        -> pure ()


{-
  FIND ME

  send event
    push event
  start spinner
    hook for when event starts running
    hook for when no event is running
  send another event
  first event is done
    push to persistQ
  update spinner to event #2
  second event is done
    push to executeQ
  remove spinner
-}


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
