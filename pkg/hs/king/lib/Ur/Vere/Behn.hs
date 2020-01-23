{-|
    Behn: Timer Driver
-}

module Ur.Vere.Behn (behn) where

import Ur.Arvo            hiding (Behn)
import Ur.Prelude
import Ur.Vere.Pier.Types

import Ur.Time  (Wen)
import Ur.Timer (Timer)

import qualified Ur.Time  as Time
import qualified Ur.Timer as Timer


-- Behn Stuff ------------------------------------------------------------------

bornEv :: KingId -> Ev
bornEv king = EvBlip $ BlipEvBehn $ BehnEvBorn (king, ()) ()

wakeEv :: Ev
wakeEv = EvBlip $ BlipEvBehn $ BehnEvWake () ()

sysTime = view Time.systemTime

behn :: KingId -> QueueEv -> ([Ev], Acquire (EffCb e BehnEf))
behn king enqueueEv =
    (initialEvents, runBehn)
  where
    initialEvents = [bornEv king]

    runBehn :: Acquire (EffCb e BehnEf)
    runBehn = do
        tim <- mkAcquire Timer.init Timer.stop
        pure (handleEf tim)

    handleEf :: Timer -> BehnEf -> RIO e ()
    handleEf b = io . \case
        BehnEfVoid v            -> absurd v
        BehnEfDoze (i, ()) mWen -> do
            when (i == king) (doze b mWen)

    doze :: Timer -> Maybe Wen -> IO ()
    doze tim = \case
        Nothing -> Timer.stop tim
        Just t  -> Timer.start tim (sysTime t) $ atomically (enqueueEv wakeEv)
