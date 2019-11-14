module Vere.Pier.Types where

import UrbitPrelude hiding (Term)

import Arvo
import Urbit.Time


-- Avoid touching Nock values. -------------------------------------------------

{-
    Nock values are raw nouns with tons of duplicated structure, so
    printing or comparing them is insane.
-}
newtype Nock = Nock Noun
  deriving newtype (FromNoun, ToNoun)

instance Eq Nock where
  (==) (Nock x) (Nock y) = jamBS x == jamBS y

instance Show Nock where
  show _ = "Nock"


--------------------------------------------------------------------------------

type EventId = Word64

data Pill = Pill
    { pBootFormulas   :: [Nock]
    , pKernelOvums    :: [Ev]
    , pUserspaceOvums :: [Ev]
    }
  deriving (Eq, Show)

data LogIdentity = LogIdentity
    { who          :: Ship
    , isFake       :: Bool
    , lifecycleLen :: Word
    } deriving (Eq, Ord, Show)

data BootSeq = BootSeq LogIdentity [Nock] [Ev]
  deriving (Eq, Show)

deriveNoun ''LogIdentity
deriveNoun ''Pill


-- Jobs ------------------------------------------------------------------------

data Work = Work EventId Mug Wen Ev
  deriving (Eq, Show)

data LifeCyc = LifeCyc EventId Mug Nock
  deriving (Eq, Show)

data Job
    = DoWork Work
    | RunNok LifeCyc
  deriving (Eq, Show)

jobId :: Job -> EventId
jobId (RunNok (LifeCyc eId _ _)) = eId
jobId (DoWork (Work eId _ _ _))  = eId

jobMug :: Job -> Mug
jobMug (RunNok (LifeCyc _ mug _)) = mug
jobMug (DoWork (Work _ mug _ _))  = mug


--------------------------------------------------------------------------------

data Order
    = OBoot LogIdentity
    | OExit Word8
    | OSave EventId
    | OWork Job
  deriving (Eq, Show)

deriveToNoun ''Order

type QueueEv = Ev -> STM ()

type EffCb a = a -> IO ()

type Perform = Ef -> IO ()

data IODrv e ef = IODrv [Ev] (RAcquire e (EffCb ef))


-- Instances -------------------------------------------------------------------

instance ToNoun Work where
  toNoun (Work eid m d o) = toNoun (eid, Jammed (m, d, o))

instance FromNoun Work where
    parseNoun n = named "Work" $ do
        (eid, Jammed (m, d, o)) <- parseNoun n
        pure (Work eid m d o)

instance ToNoun LifeCyc where
  toNoun (LifeCyc eid m n) = toNoun (eid, Jammed (m, n))

instance FromNoun LifeCyc where
  parseNoun n = named "LifeCyc" $ do
      (eid, Jammed (m, n)) <- parseNoun n
      pure (LifeCyc eid m n)

-- No FromNoun instance, because it depends on context (lifecycle length)
instance ToNoun Job where
  toNoun (DoWork w) = toNoun w
  toNoun (RunNok l) = toNoun l
