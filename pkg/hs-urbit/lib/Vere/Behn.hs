module Vere.Behn (behn) where

import UrbitPrelude
import Arvo hiding (Behn)
import Vere.Pier.Types

import Urbit.Time  (Wen)
import Urbit.Timer (Timer)

import qualified Urbit.Time  as Time
import qualified Urbit.Timer as Timer


-- Behn Stuff ------------------------------------------------------------------

bornEv :: KingInstance -> Ev
bornEv inst = EvBlip $ BlipEvBehn $ BehnEvBorn (fromIntegral inst, ()) ()

wakeEv :: Ev
wakeEv = EvBlip $ BlipEvBehn $ BehnEvWake () ()

sysTime = view Time.systemTime

behn :: KingInstance -> QueueEv -> ([Ev], Acquire (EffCb BehnEf))
behn inst enqueueEv =
    (initialEvents, runBehn)
  where
    initialEvents = [bornEv inst]

    runBehn :: Acquire (EffCb BehnEf)
    runBehn = do
        tim <- mkAcquire Timer.init Timer.stop
        pure (handleEf tim)

    handleEf :: Timer -> BehnEf -> IO ()
    handleEf b = \case
        BehnEfVoid v            -> absurd v
        BehnEfDoze (i, ()) mWen -> do
            when (i == fromIntegral inst) (doze b mWen)

    doze :: Timer -> Maybe Wen -> IO ()
    doze tim = \case
        Nothing -> Timer.stop tim
        Just t  -> Timer.start tim (sysTime t) $ atomically (enqueueEv wakeEv)
