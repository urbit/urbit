module Vere.Drv.Behn (behn) where

import UrbitPrelude

import Arvo                   (KingId)
import Arvo                   (BehnEf(..), BehnEv(..), BlipEv(..), Ev(..))
import Data.Time.Clock.System (SystemTime)
import Urbit.Time             (Wen)
import Urbit.Timer            (Timer)
import Vere.Pier.Types        (IODrv(..), EffCb, QueueEv)

import qualified Urbit.Time  as Time
import qualified Urbit.Timer as Timer


-- Utilities -------------------------------------------------------------------

bornEv :: KingId -> Ev
bornEv king = EvBlip $ BlipEvBehn $ BehnEvBorn (king, ()) ()

wakeEv :: Ev
wakeEv = EvBlip $ BlipEvBehn $ BehnEvWake () ()

sysTime :: Wen -> SystemTime
sysTime = view Time.systemTime


-- Behn Driver -----------------------------------------------------------------

behn :: ∀e. HasLogFunc e
     => KingId -> QueueEv
     -> IODrv e BehnEf
behn king enqueueEv =
    IODrv initialEvents runBehn
  where
    initialEvents = [bornEv king]

    runBehn :: RAcquire e (EffCb e BehnEf)
    runBehn = do
        rio $ logTrace "Behn Starting"
        tim <- liftAcquire (mkAcquire Timer.init Timer.stop)
        pure (handleEf tim)

    handleEf :: Timer -> BehnEf -> RIO e ()
    handleEf b = \case
        BehnEfVoid v            -> absurd v
        BehnEfDoze (i, ()) mWen -> do
            when (i == king) $ do
                doze b mWen

    doze :: Timer -> Maybe Wen -> RIO e ()
    doze tim = io . \case
        Nothing -> Timer.stop tim
        Just t  -> Timer.start tim (sysTime t)
                       $ atomically (enqueueEv wakeEv)
