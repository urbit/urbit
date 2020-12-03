{-|
    A bunch of common types.

    TODO Most of these could probably find better homes.
-}
module Urbit.Vere.Pier.Types
  ( module Urbit.Vere.Serf.Types
  , LogIdentity(..)
  , Pill(..)
  , Job(..)
  , LifeCyc(..)
  , BootSeq(..)
  , Work(..)
  , jobId
  , jobMug
  , DriverApi(..)
  )
where

import Urbit.Prelude hiding (Term)

import Urbit.Arvo
import Urbit.Noun.Time
import Urbit.Vere.Serf.Types

import Urbit.EventLog.LMDB (LogIdentity(..))


-- Avoid touching Nock values. -------------------------------------------------

{-|
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

data Pill = Pill
  { pBootFormulas   :: ![Nock]
  , pKernelOvums    :: ![Ev]
  , pUserspaceOvums :: ![Ev]
  }
 deriving (Eq, Show)

data BootSeq = BootSeq !LogIdentity ![Nock] ![Ev]
  deriving (Eq, Show)

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
jobId (DoWork (Work eId _ _ _ )) = eId

jobMug :: Job -> Mug
jobMug (RunNok (LifeCyc _ mug _)) = mug
jobMug (DoWork (Work _ mug _ _ )) = mug


-- API To IO Drivers -----------------------------------------------------------

data DriverApi ef = DriverApi
  { diEventSource    :: STM (Maybe RunReq)
  , diOnEffect       :: ef -> IO ()
  }


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

-- | No FromNoun instance, because it depends on context (lifecycle length)
instance ToNoun Job where
  toNoun (DoWork w) = toNoun w
  toNoun (RunNok l) = toNoun l
