module Vere.Pier.Types where

import ClassyPrelude
import Data.Void
import Noun
import Noun.Atom
import Noun.Poet
import Noun.Poet.TH
import Database.LMDB.Raw
import Urbit.Time

import RIO (decodeUtf8Lenient)

import qualified Vere.Http.Client as Client
import qualified Vere.Http.Server as Server

--------------------------------------------------------------------------------

data Event
    = BehnBorn
    | HttpBorn
    | CttpBorn
  deriving (Eq, Ord, Show)

data PutDel = Put | Del
  deriving (Eq, Ord, Show)

data EffBs
    = EBAsdf Word
    | EBLolr Word Word
  deriving (Eq, Ord, Show)

data RecEx = RE Word Word
  deriving (Eq, Ord, Show)

data NewtEx = NE Word
  deriving (Eq, Ord, Show)

data Eff
    = HttpServer Server.Eff
    | HttpClient Client.Eff
    | Ames Noun
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

newtype Path = Path [Knot]
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

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

deriveNoun ''Blit
deriveNoun ''Eff
deriveNoun ''Event
deriveNoun ''PutDel
deriveNoun ''EffBs
deriveNoun ''RecEx
deriveNoun ''NewtEx

data Varience = Gold | Iron | Lead

type Perform = Eff -> IO ()

data Ovum = Ovum Path Event
  deriving (Eq, Ord, Show, Generic, ToNoun)

newtype Mug = Mug Word32
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

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

data LogIdentity = LogIdentity
  { who     :: Noun
  , is_fake :: Noun
  , life    :: Noun
  } deriving (Show)

instance ToNoun LogIdentity where
  toNoun LogIdentity{..} = toNoun (who, is_fake, life)

instance FromNoun LogIdentity where
  parseNoun n = do
    (who, is_fake, life) <- parseNoun n
    pure (LogIdentity{..})
