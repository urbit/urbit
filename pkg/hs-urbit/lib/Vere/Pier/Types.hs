module Vere.Pier.Types where

import UrbitPrelude hiding (Term)
import Urbit.Time
import Vere.Ovum
import Vere.FX


-- Don't show Nock values. -----------------------------------------------------

newtype Nock = Nock Noun
  deriving newtype (Eq, Ord, FromNoun, ToNoun)

instance Show Nock where
  show _ = "Nock"


--------------------------------------------------------------------------------

type EventId = Word64

data Pill = Pill
    { pBootFormulas   :: [Nock]
    , pKernelOvums    :: [Ovum]
    , pUserspaceOvums :: [Ovum]
    }
  deriving (Eq, Ord)

data LogIdentity = LogIdentity
    { who          :: Ship
    , isFake       :: Bool
    , lifecycleLen :: Word
    } deriving (Eq, Ord, Show)

data BootSeq = BootSeq LogIdentity [Nock] [Ovum]
  deriving (Eq, Ord, Show)

newtype Desk = Desk Cord
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

deriveNoun ''LogIdentity
deriveNoun ''Pill


-- Jobs ------------------------------------------------------------------------

data Work = Work EventId Mug Wen Ovum
  deriving (Eq, Ord, Show)

data LifeCyc = LifeCyc EventId Mug Nock
  deriving (Eq, Ord, Show)

data Job
    = DoWork Work
    | RunNok LifeCyc
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

deriveToNoun ''Order


--------------------------------------------------------------------------------

type Perform = Eff -> IO ()

data IODriver = IODriver
  { bornEvent   :: IO Ovum
  , startDriver :: (Ovum -> STM ()) -> IO (Async (), Perform)
  }

data Writ = Writ
  { writId      :: Word64
  , writTimeout :: Maybe Word
  , writEv      :: ByteString -- Jammed atomJam
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

-- No FromNoun instance, because it depends on context (lifecycle length)
instance ToNoun Job where
  toNoun (DoWork w) = toNoun w
  toNoun (RunNok l) = toNoun l

instance Show Pill where
  show (Pill x y z) = show (length x, length y, length z)
