-- This is required due to the use of 'Void' in a constructor slot in
-- combination with 'deriveNoun', which will generate an unreachable pattern.
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

{-|
    Behn: Timer Driver
-}

module Urbit.Vere.Behn (behn, DriverApi(..), behn') where

import Data.Time.Clock.System (SystemTime)

import Urbit.Arvo
import Urbit.Prelude
import Urbit.Vere.Pier.Types

import Urbit.King.App  (HasKingId(..), HasPierEnv(..))
import Urbit.Noun.Time (Wen)
import Urbit.Timer     (Timer)

import qualified Urbit.Noun.Time as Time
import qualified Urbit.Timer     as Timer


-- Behn Stuff ------------------------------------------------------------------

behn' :: HasPierEnv e => RIO e ([Ev], RAcquire e (DriverApi BehnEf))
behn' = do
  env <- ask
  pure ([bornEv (fromIntegral (env ^. kingIdL))], runDriver env)
 where
  runDriver env = do
    ventQ :: TQueue EvErr <- newTQueueIO
    diOnEffect <- liftAcquire (behn env (writeTQueue ventQ))
    let diEventSource = fmap RRWork <$> tryReadTQueue ventQ
    pure (DriverApi {..})

bornEv :: KingId -> Ev
bornEv king = EvBlip $ BlipEvBehn $ BehnEvBorn (king, ()) ()

wakeEv :: Ev
wakeEv = EvBlip $ BlipEvBehn $ BehnEvWake () ()

sysTime :: Wen -> SystemTime
sysTime = view Time.systemTime

wakeErr :: WorkError -> IO ()
wakeErr _ = pure ()

behn
  :: HasKingId e
  => e
  -> (EvErr -> STM ())
  -> Acquire (BehnEf -> IO ())
behn env enqueueEv = runBehn
  where
    king = fromIntegral (env ^. kingIdL)

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
