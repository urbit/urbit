{-# LANGUAGE UndecidableInstances #-}

module Vere.Pier.Types where

import UrbitPrelude

import Database.LMDB.Raw
import Urbit.Time

import Data.LargeWord (Word128)
import Data.Void      (Void)

import qualified Vere.Ames        as Ames
import qualified Vere.Http.Client as Client
import qualified Vere.Http.Server as Server

--------------------------------------------------------------------------------

newtype Ship = Ship Word128 -- @p
  deriving newtype (Eq, Ord, Show, Num, ToNoun, FromNoun)

newtype ShipId = ShipId (Ship, Bool)
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

newtype FileOcts = FileOcts ByteString
  deriving newtype (Eq, Ord, ToNoun, FromNoun)

newtype BigTape = BigTape Text
  deriving newtype (Eq, Ord, ToNoun, FromNoun)

data LegacyBootEvent
    = Fake Ship
    | Dawn Void
  deriving (Eq, Ord, Show)

newtype Nock = Nock Noun
  deriving newtype (Eq, Ord, FromNoun, ToNoun)

data Pill = Pill
    { pBootFormulas   :: [Nock]
    , pKernelOvums    :: [Ovum]
    , pUserspaceOvums :: [Ovum]
    }
  deriving (Eq, Ord)

data BootSeq = BootSeq LogIdentity [Nock] [Ovum]
  deriving (Eq, Ord, Show)

newtype Desk = Desk Text
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

data LogIdentity = LogIdentity
    { who          :: Ship
    , isFake       :: Bool
    , lifecycleLen :: Atom
    } deriving (Eq, Ord, Show)

data Mime = Mime Path FileOcts
  deriving (Eq, Ord, Show)

data Event
    = Veer Cord Path BigTape
    | Into Desk Bool [(Path, Maybe Mime)]
    | Whom Ship
    | Boot LegacyBootEvent
    | Wack Word512
  deriving (Eq, Ord, Show)

data PutDel = Put | Del
  deriving (Eq, Ord, Show)

data RecEx = RE Word Word
  deriving (Eq, Ord, Show)

data NewtEx = NE Word
  deriving (Eq, Ord, Show)

data Eff
    = HttpServer Server.Eff
    | HttpClient Client.Eff
    | Ames Ames.Eff
    | Bbye Noun
    | Behn Noun
    | Blit [Blit]
    | Boat Noun
    | Clay Noun
    | Crud Noun
    | Dirk Noun
    | Doze (Maybe Wen)
    | Ergo Noun
    | Exit Noun
    | Flog Noun
    | Form Noun
    | Hill [Term]
    | Init
    | Logo Noun
    | Mass Noun
    | Newt Noun
    | Ogre Noun
    | Send [Blit]
    | Sync Noun
    | Term Noun
    | Thou Noun
    | Turf (Maybe (PutDel, [Text])) -- TODO Unsure
    | Vega Noun
    | West Noun
    | Woot Noun
  deriving (Eq, Ord, Show)

data Blit
    = Bel
    | Clr
    | Hop Word64
    | Lin [Char]
    | Mor
    | Sag Path Noun
    | Sav Path Atom
    | Url Text
  deriving (Eq, Ord, Show)

data Varience = Gold | Iron | Lead

type Perform = Eff -> IO ()

data Ovum = Ovum Path Event
  deriving (Eq, Ord, Show)

newtype Jam = Jam Atom

data IODriver = IODriver
  { bornEvent   :: IO Ovum
  , startDriver :: (Ovum -> STM ()) -> IO (Async (), Perform)
  }

data Writ a = Writ
  { eventId :: Word64
  , timeout :: Maybe Word
  , event   :: Jam -- mat
  , payload :: a
  }

data Pier = Pier
  { computeQueue  :: TQueue Ovum
  , persistQueue  :: TQueue (Writ [Eff])
  , releaseQueue  :: TQueue (Writ [Eff])
  , log           :: EventLog
  , driverThreads :: [(Async (), Perform)]
  , portingThread :: Async ()
  }

newtype EventLog = EventLog MDB_env


-- Instances -------------------------------------------------------------------

instance Show FileOcts where
  show (FileOcts bs) = show (take 32 bs <> "...")

instance Show BigTape where
  show (BigTape t) = show (take 32 t <> "...")

instance Show Nock where
  show _ = "Nock"

instance Show Pill where
  show (Pill x y z) = show (length x, length y, length z)

deriveNoun ''Mime
deriveNoun ''Pill
deriveNoun ''LegacyBootEvent
deriveNoun ''Blit
deriveNoun ''Eff
deriveNoun ''Event
deriveNoun ''NewtEx
deriveNoun ''Ovum
deriveNoun ''PutDel
deriveNoun ''RecEx
deriveNoun ''LogIdentity
