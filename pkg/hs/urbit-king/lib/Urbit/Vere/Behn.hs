{-|
    Behn: Timer Driver
-}

module Urbit.Vere.Behn (behn, DriverApi(..), behn') where

import Urbit.Arvo            hiding (Behn)
import Urbit.Prelude
import Urbit.Vere.Pier.Types

import Urbit.King.App (HasPierEnv(..), HasKingId(..))
import Urbit.Time     (Wen)
import Urbit.Timer    (Timer)

import qualified Urbit.Time  as Time
import qualified Urbit.Timer as Timer


-- Behn Stuff ------------------------------------------------------------------

behn' :: HasPierEnv e => RAcquire e DriverApi
behn' = do
  ventQ <- newTQueueIO
  bornM <- newEmptyTMVarIO
  fectM <- newEmptyTMVarIO

  env <- ask
  let (bootEvs, start) = behn env (writeTQueue ventQ)
  for_ bootEvs (atomically . writeTQueue ventQ)

  diOnEffect <- liftAcquire start

  let diEventSource = fmap RRWork <$> tryReadTQueue ventQ

  let diBlockUntilBorn = readTMVar bornM

  -- TODO Do this after successful born event.
  atomically $ putTMVar bornM ()

  pure (DriverApi {..})

bornEv :: KingId -> Ev
bornEv king = EvBlip $ BlipEvBehn $ BehnEvBorn (king, ()) ()

wakeEv :: Ev
wakeEv = EvBlip $ BlipEvBehn $ BehnEvWake () ()

sysTime = view Time.systemTime

bornFailed :: e -> WorkError -> IO ()
bornFailed env _ = runRIO env $ do
  pure () -- TODO Ship is fucked. Kill it?

wakeErr :: WorkError -> IO ()
wakeErr _ = pure ()

behn
  :: HasKingId e
  => e
  -> (EvErr -> STM ())
  -> ([EvErr], Acquire (BehnEf -> IO ()))
behn env enqueueEv =
    (initialEvents, runBehn)
  where
    king = fromIntegral (env ^. kingIdL)

    initialEvents = [EvErr (bornEv king) (bornFailed env)]

    runBehn :: Acquire (BehnEf -> IO ())
    runBehn = do
        tim <- mkAcquire Timer.init Timer.stop
        pure (runRIO env . handleEf tim)

    handleEf :: Timer -> BehnEf -> RIO e ()
    handleEf b = io . \case
        BehnEfVoid v            -> absurd v
        BehnEfDoze (i, ()) mWen -> do
            when (i == king) (doze b mWen)

    doze :: Timer -> Maybe Wen -> IO ()
    doze tim = \case
        Nothing -> Timer.stop tim
        Just t  -> Timer.start tim (sysTime t) $ atomically (enqueueEv (EvErr wakeEv wakeErr))
