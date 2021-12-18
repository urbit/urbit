{-|
  Top-Level Pier Management

  This is the code that starts the IO drivers and deals with communication
  between the serf, the event log, and the IO drivers.
-}
module Urbit.Vere.Pier
  ( booted
  , resumed
  , pier
  , runCompute
  )
where

import Urbit.Prelude

import Urbit.Arvo
import Urbit.King.App
import Urbit.Vere.Pier.Types
import Urbit.Vere.Stat

import Control.Monad.STM         (retry)
import System.Environment        (getExecutablePath)
import System.Exit               (ExitCode)
import System.FilePath           (splitFileName)
import Urbit.King.API            (TermConn)
import Urbit.TermSize            (TermSize(..), termSize)
import Urbit.Vere.Serf           (Serf)
import Urbit.Vere.Serf.IPC.Types (Boot(..), Vent(..))

import qualified Data.Text                   as T
import qualified Data.List as L
import qualified System.Entropy              as Ent
import qualified Urbit.King.API              as King
import qualified Urbit.Vere.Ames             as Ames
import qualified Urbit.Vere.Behn             as Behn
import qualified Urbit.Vere.Clay             as Clay
import qualified Urbit.Vere.Eyre             as Eyre
import qualified Urbit.Vere.Eyre.KingSubsite as Site
import qualified Urbit.Vere.Http.Client      as Iris
import qualified Urbit.Vere.Serf             as Serf
import qualified Urbit.Vere.Term             as Term
import qualified Urbit.Vere.Term.API         as Term
import qualified Urbit.Vere.Term.Demux       as Term


-- Load pill into boot sequence. -----------------------------------------------

data CannotBootFromIvoryPill = CannotBootFromIvoryPill
  deriving (Show, Exception)

genEntropy :: MonadIO m => m Entropy
genEntropy = Entropy . fromIntegral . bytesAtom <$> io (Ent.getEntropy 64)

-- Boot a new ship. ------------------------------------------------------------

booted
  :: TVar ((Atom, Tank) -> IO ())
  -> Pill
  -> Bool
  -> Ship  -- XX duplication between this and LegacyBootEvent?
  -> LegacyBootEvent
  -> RIO PierEnv ExitCode
booted vSlog pill lite ship boot = do
  rio $ logTrace "Booting ship"

  config@Config{ scPier } <- getSerfConfig vSlog

  rio $ do
    logTrace $ display @Text ("pier: " <> pack scPier)

  io $ Serf.boot config shoe
 where
  shoe = Boot
    { bPill = (pill, Nothing) 
    , bVent = case boot of
        Fake ship' -> VFake ship'
        Dawn dawn  -> VDawn $ dSeed dawn
    , bMore = []
    }


-- Resume an existing ship. ----------------------------------------------------

resumed
  :: TVar ((Atom, Tank) -> IO ())
  -> RAcquire PierEnv (Serf, Ripe)
resumed vSlog = do
  rio $ logTrace "Resuming ship"

  config@Config{ scPier } <- rio $ getSerfConfig vSlog

  rio $ do
    logTrace $ display @Text ("pier: " <> pack scPier)

  Serf.withSerf config


-- Get Serf.Config for booting or resuming -------------------------------------

getSerfConfig
  :: TVar ((Atom, Tank) -> IO ())
  -> RIO PierEnv Serf.Config
getSerfConfig vSlog = do
  pax <- view pierPathL
  env <- ask
  serfProg <- io getSerfProg
  pure Config
    { scSerf = env ^. pierConfigL . pcSerfExe . to (maybe serfProg unpack)
    , scPier = pax
    , scFlag = env ^. pierConfigL . pcSerfFlags
    , scSlog = slog
    , scStdr = \txt -> slog (0, (textToTank txt))
    , scDead = pure () -- TODO: What can be done?
    }
 where
  slog s = atomically (readTVar vSlog) >>= (\f -> f s)

  getSerfProg :: IO FilePath
  getSerfProg = do
    (path, filename) <- splitFileName <$> getExecutablePath
    pure $ case filename of
      "urbit"      -> path </> "urbit-worker"
      "urbit-king" -> path </> "urbit-worker"
      _            -> "urbit-worker"


-- Run Pier --------------------------------------------------------------------

pier
  :: (Serf, Ripe)
  -> TVar ((Atom, Tank) -> IO ())
  -> MVar ()
  -> [Ev]
  -> RAcquire PierEnv ()
pier (serf, ripe) vSlog startedSig injected = do
  let self = riSelf ripe :: Self
  let ship  = who self :: Ship

  -- TODO Instead of using a TMVar, pull directly from the IO driver
  -- event sources.
  computeQ :: TMVar RunReq      <- newEmptyTMVarIO
  executeQ :: TQueue FX         <- newTQueueIO
  saveSig :: TMVar ()           <- newEmptyTMVarIO
  kingApi :: King.King          <- King.kingAPI

  termApiQ :: TQueue TermConn <- atomically $ do
    q <- newTQueue
    writeTVar (King.kTermConn kingApi) (Just $ writeTQueue q)
    pure q

  initialTermSize <- io $ termSize

  (demux :: Term.Demux, muxed :: Term.Client) <- atomically $ do
    res <- Term.mkDemux initialTermSize
    pure (res, Term.useDemux res)

  void $ acquireWorker "TERMSERV Listener" $ forever $ do
    logInfo "TERMSERV Waiting for external terminal."
    atomically $ do
      ext <- Term.connClient <$> readTQueue termApiQ
      Term.addDemux ext demux
    logInfo "TERMSERV External terminal connected."

  scryQ <- newTQueueIO
  onKill  <- view onKillPierSigL

  -- Our call above to set the logging function which echos errors from the
  -- Serf doesn't have the appended \r\n because those \r\n s are added in
  -- the c serf code. Logging output from our haskell process must manually
  -- add them.
  let compute = putTMVar computeQ
  let sigint  = Serf.sendSIGINT serf
  let scry    = \g r -> do
        res <- newEmptyMVar
        atomically $ writeTQueue scryQ (g, r, putMVar res)
        takeMVar res

  -- Set up the runtime stat counters.
  stat <- newStat

  -- Set up the runtime subsite server and its capability to slog
  -- and display stats.
  siteSlog <- newTVarIO (const $ pure ())
  runtimeSubsite <- Site.kingSubsite ship scry (renderStat stat) siteSlog

  --  Slogs go to stderr, to the runtime subsite, and to the terminal.
  env <- ask
  atomically $ writeTVar vSlog $ \s@(_, tank) -> runRIO env $ do
      atomically $ Term.slog muxed s
      io $ readTVarIO siteSlog >>= ($ s)
      logOther "serf" (display $ T.strip $ tankToText tank)

  let err = atomically . Term.trace muxed . (<> "\r\n")
  (bornEvents, startDrivers) <- do
    env <- ask
    siz <- atomically $ Term.curDemuxSize demux
    let fak = isFake self
    drivers env ship fak compute scry (siz, muxed) err sigint stat runtimeSubsite

  let computeConfig = ComputeConfig { ccOnWork      = takeTMVar computeQ
                                    , ccOnKill      = onKill
                                    , ccOnSave      = takeTMVar saveSig
                                    , ccOnScry      = readTQueue scryQ
                                    , ccExecute     = writeTQueue executeQ
                                    , ccShowSpinner = Term.spin muxed
                                    , ccHideSpinner = Term.stopSpin muxed
                                    }

  tSerf <- acquireWorker "Serf" (runCompute serf computeConfig)

  doVersionNegotiation compute err

  -- Run all born events and retry them until they succeed.
  wackEv <- EvBlip . BlipEvArvo . ArvoEvWack () <$> genEntropy
  rio $ for_ (wackEv : bornEvents) $ \ev -> do
    okaySig <- newEmptyMVar

    let inject n = atomically $ compute $ RRWork $ EvErr ev $ cb n

        --  TODO Make sure this dies cleanly.
        cb :: Int -> WorkError -> IO ()
        cb n | n >= 3 = error ("boot event failed: " <> show ev)
        cb n          = \case
          RunOkay _ -> putMVar okaySig ()
          RunBail _ -> inject (n + 1)

    -- logTrace ("[BOOT EVENT]: " <> display (summarizeEvent ev))
    io (inject 0)

  let slog :: Text -> IO ()
      slog txt = do
        fn <- atomically (readTVar vSlog)
        fn (0, textToTank txt)

  drivz <- startDrivers
  tExec <- acquireWorker "Effects" (router slog (readTQueue executeQ) drivz)

  -- Now that the Serf is configured, the IO drivers are hooked up, their
  -- starting events have been dispatched, and the terminal is live, we can now
  -- handle injecting events requested from the command line.
  for_ (zip [1..] injected) $ \(num, ev) -> rio $ do
    logTrace $ display @Text ("Injecting event " ++ (tshow num) ++ " of " ++
                              (tshow $ length injected) ++ "...")
    okaySig :: MVar (Either [Goof] ()) <- newEmptyMVar

    let inject = atomically $ compute $ RRWork $ EvErr ev $ cb
        cb :: WorkError -> IO ()
        cb = \case
          RunOkay _     -> putMVar okaySig (Right ())
          RunBail goofs -> putMVar okaySig (Left goofs)

    io inject

    takeMVar okaySig >>= \case
      Left goof -> logError $ display @Text ("Goof in injected event: " <>
                                             tshow goof)
      Right ()  -> pure ()


  let snapshotEverySecs = 120

  void $ acquireWorker "Save" $ forever $ do
    threadDelay (snapshotEverySecs * 1_000_000)
    void $ atomically $ tryPutTMVar saveSig ()

  putMVar startedSig ()

  -- Wait for something to die.

  let ded = asum
        [ death "effects thread" tExec
        , death "compute thread" tSerf
        ]

  atomically ded >>= \case
    Left  (tag, exn)       -> logError $ displayShow (tag, "crashed", exn)
    Right "compute thread" -> pure ()
    Right tag              -> logError $ displayShow (tag, "exited unexpectly")

  atomically $ (Term.spin muxed) (Just "shutdown")

death :: Text -> Async () -> STM (Either (Text, SomeException) Text)
death tag tid = do
  waitCatchSTM tid <&> \case
    Left  exn -> Left (tag, exn)
    Right ()  -> Right tag

-- %wyrd version negotiation ---------------------------------------------------

data PierVersionNegotiationFailed = PierVersionNegotiationFailed
 deriving (Show, Exception)

zuseVersion :: Word
zuseVersion = 420

wyrd :: HasKingEnv e => RIO e Ev
wyrd = do
  king <- tshow <$> view kingIdL

  let k   = Wynn [("zuse", zuseVersion),
                  ("lull", 330),
                  ("arvo", 240),
                  ("hoon", 140),
                  ("nock", 4)]
      sen = MkTerm king
      v   = Vere sen [Cord "king-haskell", Cord "1.0"] k

  pure $ EvBlip $ BlipEvArvo $ ArvoEvWyrd () v

doVersionNegotiation
  :: HasPierEnv e
  => (RunReq -> STM ())
  -> (Text -> RIO e ())
  -> RAcquire e ()
doVersionNegotiation compute stderr = do
  ev <- rio wyrd

  okaySig :: MVar (Either [Goof] FX) <- newEmptyMVar
  let inject = atomically $ compute $ RRWork $ EvErr ev $ cb
      cb :: WorkError -> IO ()
      cb = \case
        RunOkay fx    -> putMVar okaySig (Right fx)
        RunBail goofs -> putMVar okaySig (Left goofs)

  rio $ stderr "vere: checking version compatibility"
  io inject

  takeMVar okaySig >>= \case
    Left goof -> do
      rio $ stderr "pier: version negotation failed"
      logError $ display @Text ("Goof in wyrd event: " <> tshow goof)
      throwIO PierVersionNegotiationFailed

    Right fx  -> do
      -- Walk through the returned fx looking for a wend effect. If we find
      -- one, check the zuse versions.
      rio $ for_ fx $ \case
        GoodParse (EfWend (Wynn xs)) -> case L.lookup "zuse" xs of
          Nothing -> pure ()
          Just zuseVerInWynn ->
            if zuseVerInWynn /= zuseVersion
            then do
              rio $ stderr "pier: pier: version negotiation failed; downgrade"
              throwIO PierVersionNegotiationFailed
            else
              pure ()
        _ -> pure ()


-- Start All Drivers -----------------------------------------------------------

data Drivers = Drivers
  { dBehn :: BehnEf -> IO ()
  , dIris :: HttpClientEf -> IO ()
  , dEyre :: HttpServerEf -> IO ()
  , dNewt :: NewtEf -> IO ()
  , dSync :: SyncEf -> IO ()
  , dTerm :: TermEf -> IO ()
  }

drivers
  :: HasPierEnv e
  => e
  -> Ship
  -> Bool
  -> (RunReq -> STM ())
  -> ScryFunc
  -> (TermSize, Term.Client)
  -> (Text -> RIO e ())
  -> IO ()
  -> Stat
  -> Site.KingSubsite
  -> RAcquire e ([Ev], RAcquire e Drivers)
drivers env who isFake plan scry termSys stderr serfSIGINT stat sub = do
  let Stat{..} = stat

  (behnBorn, runBehn) <- rio Behn.behn'
  (termBorn, runTerm) <- rio (Term.term' termSys (renderStat stat) serfSIGINT)
  (amesBorn, runAmes) <- rio (Ames.ames' who isFake statAmes scry stderr)
  (httpBorn, runEyre) <- rio (Eyre.eyre' who isFake stderr sub)
  (clayBorn, runClay) <- rio Clay.clay'
  (irisBorn, runIris) <- rio Iris.client'

  putStrLn ("ship is " <> tshow who)

  let initialEvents = mconcat [behnBorn,clayBorn,amesBorn,httpBorn,irisBorn,termBorn]

  let runDrivers = do
        behn <- runBehn
        term <- runTerm
        ames <- runAmes
        iris <- runIris
        eyre <- runEyre
        clay <- runClay

        -- Sources lower in the list are starved until sources above them
        -- have no events to offer.
        acquireWorker "Event Prioritization" $ forever $ atomically $ do
          let x = diEventSource
          let eventSources = [x term, x clay, x behn, x iris, x eyre, x ames]
          pullEvent eventSources >>= \case
            Nothing -> retry
            Just rr -> plan rr

        pure $ Drivers
          { dTerm = diOnEffect term
          , dBehn = diOnEffect behn
          , dNewt = diOnEffect ames
          , dIris = diOnEffect iris
          , dEyre = diOnEffect eyre
          , dSync = diOnEffect clay
          }

  pure (initialEvents, runDrivers)
 where
  pullEvent :: [STM (Maybe a)] -> STM (Maybe a)
  pullEvent []     = pure Nothing
  pullEvent (d:ds) = d >>= \case
    Just r  -> pure (Just r)
    Nothing -> pullEvent ds


-- Route Effects to Drivers ----------------------------------------------------

router :: HasPierEnv e => (Text -> IO ()) -> STM FX -> Drivers -> RIO e ()
router slog waitFx Drivers {..} = do
  kill <- view killPierActionL
  let exit = io (slog "<<<shutdown>>>\r\n") >> atomically kill
  let vega = io (slog "<<<reset>>>\r\n")
  forever $ do
    fx <- atomically waitFx
    for_ fx $ \ef -> do
      logEffect ef
      case ef of
        GoodParse (EfVega _ _              ) -> vega
        GoodParse (EfExit _ _              ) -> exit
        GoodParse (EfWend _                ) -> pure ()
        GoodParse (EfVane (VEBehn       ef)) -> io (dBehn ef)
        GoodParse (EfVane (VEBoat       ef)) -> io (dSync ef)
        GoodParse (EfVane (VEClay       ef)) -> io (dSync ef)
        GoodParse (EfVane (VEHttpClient ef)) -> io (dIris ef)
        GoodParse (EfVane (VEHttpServer ef)) -> io (dEyre ef)
        GoodParse (EfVane (VENewt       ef)) -> io (dNewt ef)
        GoodParse (EfVane (VESync       ef)) -> io (dSync ef)
        GoodParse (EfVane (VETerm       ef)) -> io (dTerm ef)
        FailParse n -> logError $ display $ pack @Text (ppShow n)


-- Compute (Serf) Thread -------------------------------------------------------

logEvent :: HasLogFunc e => Ev -> RIO e ()
logEvent ev = do
  --logInfo  $ "<- " <> display (summarizeEvent ev)
  logDebug $ "[EVENT]\n" <> display pretty
 where
  pretty :: Text
  pretty = pack $ unlines $ fmap ("\t" <>) $ lines $ ppShow ev

logEffect :: HasLogFunc e => Lenient Ef -> RIO e ()
logEffect ef = do
  --logInfo  $ "  -> " <> display (summarizeEffect ef)
  logDebug $ display $ "[EFFECT]\n" <> pretty ef
 where
  pretty :: Lenient Ef -> Text
  pretty = \case
    GoodParse e -> pack $ unlines $ fmap ("\t" <>) $ lines $ ppShow e
    FailParse n -> pack $ unlines $ fmap ("\t" <>) $ lines $ ppShow n

data ComputeConfig = ComputeConfig
  { ccOnWork      :: STM RunReq
  , ccOnKill      :: STM ()
  , ccOnSave      :: STM ()
  , ccOnScry      :: STM (Gang, ScryReq, Maybe (Term, Noun) -> IO ())
  , ccExecute     :: FX -> STM ()
  , ccShowSpinner :: Maybe Text -> STM ()
  , ccHideSpinner :: STM ()
  }

runCompute :: forall e . HasKingEnv e => Serf.Serf -> ComputeConfig -> RIO e ()
runCompute serf ComputeConfig {..} = do
  logDebug "runCompute"

  let onRR = asum [ ccOnKill <&> Serf.RRKill
                  , ccOnSave <&> Serf.RRSave
                  , ccOnWork
                  , ccOnScry <&> \(g,r,k) -> Serf.RRScry g r k
                  ]

  vEvProcessing :: TMVar Ev <- newEmptyTMVarIO

  void $ async $ forever (atomically (takeTMVar vEvProcessing) >>= logEvent)

  let onSpin :: Maybe Ev -> STM ()
      onSpin = \case
        Nothing -> ccHideSpinner
        Just ev -> do
          ccShowSpinner (getSpinnerNameForEvent ev)
          putTMVar vEvProcessing ev

  let maxBatchSize = 10

  io (Serf.run serf maxBatchSize onRR ccExecute onSpin)
