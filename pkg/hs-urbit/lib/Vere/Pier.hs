{-# OPTIONS_GHC -Wwarn #-}

module Vere.Pier (booted, resumed, runPersist, runCompute) where

import UrbitPrelude

import Arvo
import Data.Acquire
import Vere.Pier.Types

import System.Directory   (createDirectoryIfMissing)
import System.Posix.Files (ownerModes, setFileMode)
import Vere.Log           (EventLog)
import Vere.Serf          (Serf, SerfState(..))

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

{-
performCommonPierStartup :: Serf.Serf
                         -> TQueue Ev
                         -> TQueue (Writ, FX)
                         -> TQueue (Writ, FX)
                         -> LogState
                         -> IO Pier
performCommonPierStartup serf computeQ persistQ releaseQ logState = do
  for ioDrivers $ \x -> do
    bootMessage <- bornEvent x
    atomically $ writeTQueue computeQ bootMessage

  driverThreads <- for ioDrivers $ \x -> do
    startDriver x (writeTQueue computeQ)

  -- TODO: Don't do a bunch of extra work; we send all effects to all drivers
  portingThread <- async $ do
    forever $ do
      r <- atomically (readTQueue releaseQ)
      for_ driverThreads $ \(_, k) ->
        for_ (payload r) $ \eff ->
          k eff

  Serf.workerThread serf (readTQueue computeQ) undefined

  pure (Pier{..})
-}


-- Compute Thread --------------------------------------------------------------

runCompute :: Serf -> STM Ev -> (EventId, Mug) -> IO (Async ())
runCompute w getEvent (evendId, mug) = async $ forever $ do
  ovum <- atomically $ getEvent

  currentDate <- Time.now

  let _mat = jam (undefined (mug, currentDate, ovum))

  undefined


-- Persist Thread --------------------------------------------------------------

data PersistExn = BadEventId EventId EventId
  deriving Show

instance Exception PersistExn where
  displayException (BadEventId expected got) =
    unlines [ "Out-of-order event id send to persist thread."
            , "\tExpected " <> show expected <> " but got " <> show got
            ]

runPersist :: EventLog
           -> TQueue (Writ, FX)
           -> ((Writ, FX) -> STM ())
           -> Acquire ()
runPersist log inpQ out = do
    mkAcquire runThread cancelWait
    pure ()
  where
    cancelWait :: Async () -> IO ()
    cancelWait tid = cancel tid >> wait tid

    runThread :: IO (Async ())
    runThread = asyncBound $ forever $ do
        writs  <- atomically getBatchFromQueue
        events <- validateWritsAndGetBytes (toNullable writs)
        Log.appendEvents log events
        atomically $ traverse_ out writs

    validateWritsAndGetBytes :: [(Writ, FX)] -> IO (Vector ByteString)
    validateWritsAndGetBytes writs = do
        expect <- Log.nextEv log
        fmap fromList
            $ for (zip [expect..] writs)
            $ \(expectedId, (w, fx)) -> do
                unless (expectedId == writId w) $
                    throwIO (BadEventId expectedId (writId w))
                pure (writEv w)

    getBatchFromQueue :: STM (NonNull [(Writ, FX)])
    getBatchFromQueue =
        readTQueue inpQ >>= go . singleton
      where
        go acc =
          tryReadTQueue inpQ >>= \case
            Nothing   -> pure (reverse acc)
            Just item -> go (item <| acc)
