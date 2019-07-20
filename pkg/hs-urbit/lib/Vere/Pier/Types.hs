{-# LANGUAGE UndecidableInstances #-}

module Vere.Pier.Types where

import UrbitPrelude
import Urbit.Time

import qualified Vere.Ames        as Ames
import qualified Vere.Http.Client as Client
import qualified Vere.Http.Server as Server

--------------------------------------------------------------------------------

newtype ShipId = ShipId (Ship, Bool)
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

newtype Octs = Octs ByteString
  deriving newtype (Eq, Ord, Show, FromNoun, ToNoun)

newtype FileOcts = FileOcts ByteString
  deriving newtype (Eq, Ord, ToNoun, FromNoun)

newtype BigTape = BigTape Text
  deriving newtype (Eq, Ord, ToNoun, FromNoun)

type Life = Noun
type Pass = Noun
type Turf = Noun
type PUrl = Todo Noun
type Seed = Todo Noun
type Czar = Todo Noun -- Map Ship (Life, Pass)
type Bloq = Todo Atom -- @ud

newtype Todo a = Todo a
  deriving newtype (Eq, Ord, ToNoun, FromNoun)

instance Show (Todo a) where
  show (Todo _) = "TODO"

data Dawn = MkDawn
    { dSeed :: Seed
    , dShip :: Ship
    , dCzar :: Czar
    , dTurf :: [Turf]
    , dBloq :: Bloq
    , dNode :: PUrl
    }
  deriving (Eq, Ord, Show)


data LegacyBootEvent
    = Fake Ship
    | Dawn Dawn
  deriving (Eq, Ord, Show)

newtype Nock = Nock Noun
  deriving newtype (Eq, Ord, FromNoun, ToNoun)

data Pill = Pill
    { pBootFormulas   :: [Nock]
    , pKernelOvums    :: [Ovum]
    , pUserspaceOvums :: [Ovum]
    }
  deriving (Eq, Ord)

data LogIdentity = LogIdentity
    { who          :: Ship
    , isFake       :: Bool
    , lifecycleLen :: Atom
    } deriving (Eq, Ord, Show)

data BootSeq = BootSeq LogIdentity [Nock] [Ovum]
  deriving (Eq, Ord, Show)

newtype Desk = Desk Text
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

data Mime = Mime Path FileOcts
  deriving (Eq, Ord, Show)

type EventId = Word64

data JobPayload
    = LifeCycle Nock
    | DateOvum Wen Ovum
  deriving (Eq, Ord, Show)

data Job = Job EventId Mug JobPayload
  deriving (Eq, Ord, Show)

data Order
    = OBoot LogIdentity
    | OExit Word8
    | OSave EventId
    | OWork Job
  deriving (Eq, Ord, Show)

data ResponseHeader = ResponseHeader
    { rhStatus  :: Word
    , rhHeaders :: [(Text, Text)]
    }
  deriving (Eq, Ord, Show)

data HttpEvent
    = Start ResponseHeader (Maybe Octs) Bool
    | Continue (Maybe Octs) Bool
    | Cancel
  deriving (Eq, Ord, Show)

data Lane
    = If Wen Atom Atom           --  {$if p/@da q/@ud r/@if}
    | Is Atom (Maybe Lane) Atom  --  {$is p/@ud q/(unit lane) r/@is}
    | Ix Wen Atom Atom           --  {$ix p/@da q/@ud r/@if}
  deriving (Eq, Ord, Show)

data ArrowKey = D | L | R | U
  deriving (Eq, Ord, Show)

data Address
    = AIpv4 Atom -- @if
    | AIpv6 Atom -- @is
    | AAmes Atom -- @p
  deriving (Eq, Ord, Show)

instance ToNoun Address where
  toNoun = \case
    AIpv4 x -> toNoun (Cord "ipv4", x)
    AIpv6 x -> toNoun (Cord "ipv6", x)
    AAmes x -> toNoun (Cord "ames", x)

instance FromNoun Address where
  parseNoun n = do
    parseNoun n >>= \case
      (Cord "ipv4", at) -> pure (AIpv4 at)
      (Cord "ipv6", at) -> pure (AIpv6 at)
      (Cord "ames", at) -> pure (AAmes at)
      _                 -> fail "Address must be either %ipv4, %ipv6, or %ames"

data Belt
    = Aro ArrowKey
    | Bac
    | Ctl Char
    | Del
    | Met Char
    | Ret
    | Txt Tour
  deriving (Eq, Ord, Show)

type ServerId = Atom

type JSON = Todo Noun

data RequestParams
    = List [JSON]
    | Object [(Text, JSON)]
  deriving (Eq, Ord, Show)

data HttpRequest = HttpRequest
    { reqId       :: Text
    , reqUrl      :: Text
    , reqHeaders  :: [(Text, Text)]
    , reqFinished :: Maybe Octs
    }
  deriving (Eq, Ord, Show)

data Event
    = Veer Cord Path BigTape
    | Into Desk Bool [(Path, Maybe Mime)]
    | Whom Ship
    | Boot LegacyBootEvent
    | Wack Word512
    | Boat
    | Barn
    | Born
    | Blew Word Word
    | Hail
    | Wake
    | Receive ServerId HttpEvent
    | Request ServerId Address HttpRequest
    | Live Text Bool Word
    | Hear Lane Atom
    | Belt Belt
    | Crud Text [Tank]
  deriving (Eq, Ord, Show)

data PutDel = PDPut | PDDel
  deriving (Eq, Ord, Show)

instance ToNoun PutDel where
  toNoun = \case PDPut -> toNoun (Cord "put")
                 PDDel -> toNoun (Cord "del")

instance FromNoun PutDel where
  parseNoun n = do
    parseNoun n >>= \case
      Cord "put" -> pure PDPut
      Cord "del" -> pure PDDel
      _          -> fail "PutDel must be either %put or %del"

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
    | EBoat Noun
    | Clay Noun
    --Crud Noun
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

newtype Jam = Jam { unJam :: Atom }
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

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


-- Instances -------------------------------------------------------------------

instance ToNoun JobPayload where
  toNoun (LifeCycle nok) = toNoun nok
  toNoun (DateOvum d o)   = toNoun (d, o)

-- XX TODO HACK Try to parse a date+ovum, then fall back to nock
instance FromNoun JobPayload where
  parseNoun n = do
      parseDateOvum n <|> parseLifeCycle n
    where
      parseLifeCycle n = LifeCycle <$> parseNoun n
      parseDateOvum n  = do (wen@(Wen w), ov::Ovum) <- parseNoun n
                            if w <= 12 then fail "this is nock"
                                       else pure (DateOvum wen ov)

instance ToNoun Job where
  toNoun (Job e m p) = toNoun (e, Jammed (m, p))

instance FromNoun Job where
  parseNoun n = do
      (e, Jammed (m, p)) <- parseNoun n
      pure (Job e m p)

-- XX TODO Support prefixes in deriveNoun
instance ToNoun Order where
  toNoun (OBoot id)  = toNoun (Cord "boot", id)
  toNoun (OExit cod) = toNoun (Cord "exit", cod)
  toNoun (OSave id)  = toNoun (Cord "save", id)
  toNoun (OWork j)   = toNoun (Cord "work", j)

instance Show FileOcts where
  show (FileOcts bs) = show (take 32 bs <> "...")

instance Show BigTape where
  show (BigTape t) = show (take 32 t <> "...")

instance Show Nock where
  show _ = "Nock"

instance Show Pill where
  show (Pill x y z) = show (length x, length y, length z)

deriveNoun ''ArrowKey
deriveNoun ''Belt
deriveNoun ''Blit
deriveNoun ''Dawn
deriveNoun ''Eff
deriveNoun ''Event
deriveNoun ''HttpEvent
deriveNoun ''Lane
deriveNoun ''LegacyBootEvent
deriveNoun ''LogIdentity
deriveNoun ''Mime
deriveNoun ''NewtEx
deriveNoun ''Ovum
deriveNoun ''Pill
deriveNoun ''RecEx
deriveNoun ''ResponseHeader
deriveNoun ''RequestParams
deriveNoun ''HttpRequest
