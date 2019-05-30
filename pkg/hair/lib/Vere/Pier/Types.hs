module Vere.Pier.Types where

import ClassyPrelude
import Data.Noun
import Data.Noun.Atom
import Database.LMDB.Raw
import Urbit.Time

data Effect
data Ovum
newtype Mug = Mug Word32

newtype Jam = Jam Atom

data Writ a = Writ
  { eventId          :: Word64
  , job              :: (Wen, Ovum)   -- (pair date ovum)
  , timeout          :: Maybe Word
  , mug              :: Mug
  , event            :: Jam           -- mat
  , payload          :: a
  }

data Pier = Pier
  { computeQueue :: TQueue (Writ Word)
  , persistQueue :: TQueue (Writ [Effect])
  , releaseQueue :: TQueue (Writ [Effect])
  , logState :: LogState
  }

-- TODO: We are uncertain about q's type. There's some serious entanglement
-- with u3_pier in this logic in the C code, and you might not be able to get
-- away with anything less than passing the full u3_writ around.
data LogState = LogState
  { env        :: MDB_env
  , inputQueue :: TQueue (Writ [Effect])
  , onPersist  :: Writ [Effect] -> STM ()
  , writer     :: Async ()
  }

data LogIdentity = LogIdentity
  { who     :: Noun
  , is_fake :: Noun
  , life    :: Noun
  } deriving Show
