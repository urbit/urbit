module Arvo.Event where

import UrbitPrelude hiding (Term)
import Urbit.Time

import Arvo.Common (NounMap, NounSet)
import Arvo.Common (AtomIf, AtomIs, Desk, Lane, Mime, Turf)
import Arvo.Common (HttpEvent, HttpServerConf)


-- Misc Types ------------------------------------------------------------------

type Pass = Atom -- Public Key
type Life = Word -- Number of Azimoth key revs.
type Bloq = Atom -- TODO
type Ring = Atom -- Private Key
type Oath = Atom -- Signature


-- Parsed URLs -----------------------------------------------------------------

type Host = Either Turf AtomIf
type Hart = (Bool, Maybe Atom, Host)
type Pork = (Maybe Knot, [Cord])
type Quay = [(Cord, Cord)]

data PUrl = PUrl Hart Pork Quay
  deriving (Eq, Ord, Show)

deriveNoun ''PUrl


-- Dawn Records ----------------------------------------------------------------

data Seed = Seed Ship Life Ring (Maybe Oath)
  deriving (Eq, Ord, Show)

type Public = (Life, NounMap Life Pass)

data Dnses = Dnses { dPri::Cord, dSec::Cord, dTer::Cord }
  deriving (Eq, Ord, Show)

type EthAddr = Bytes -- 20 bytes
type ContNum = Word

data EthPoint = EthPoint
    { epOwn :: (EthAddr, EthAddr, EthAddr, EthAddr)
    , epNet :: Maybe (Life, Pass, ContNum, (Bool, Ship), Maybe Ship)
    , epKid :: Maybe (EthAddr, NounSet Ship)
    }
  deriving (Eq, Ord, Show)

data EthEventId = EthEventId
    { eeiBlock :: Atom
    , eeiLog   :: Atom
    }
  deriving (Eq, Ord, Show)

data EthBookmark = EthBookmark
    { ebHeard       :: NounSet EthEventId
    , ebLatestBlock :: Atom
    }
  deriving (Eq, Ord, Show)

data Snap = Snap (NounMap Ship Public)
                 (Dnses, NounMap Ship EthPoint)
                 EthBookmark
  deriving (Eq, Ord, Show)

data Dawn = MkDawn
    { dSeed :: Seed
    , dShip :: Ship
    , dCzar :: NounMap Ship (Life, Pass)
    , dTurf :: [Turf]
    , dBloq :: Bloq
    , dNode :: (Maybe PUrl)
    , dSnap :: (Maybe Snap)
    }
  deriving (Eq, Ord, Show)

deriveNoun ''EthEventId
deriveNoun ''EthBookmark
deriveNoun ''Dnses
deriveNoun ''EthPoint
deriveNoun ''Snap
deriveNoun ''Seed
deriveNoun ''Dawn


-- HTTP ------------------------------------------------------------------------

type ServerId = Atom

data Address
    = AIpv4 AtomIf
    | AIpv6 AtomIs
    | AAmes Ship
  deriving (Eq, Ord, Show)

data HttpRequest = HttpRequest
    { reqId   :: Cord
    , reqUrl  :: Cord
    , reqHead :: [(Bytes, Bytes)]
    , reqBody :: Maybe File
    }
  deriving (Eq, Ord, Show)

data HttpServerReq = HttpServerReq
    { hsrSecure  :: Bool
    , hsrAddress :: Address
    , hsrRequest :: HttpRequest
    }
  deriving (Eq, Ord, Show)

data HttpClientEv
    = HttpClientEvReceive (Atom, ()) ServerId HttpEvent
    | HttpClientEvBorn    (Atom, ()) ()
    | HttpClientEvCrud    Path       Cord Tang
  deriving (Eq, Ord, Show)

data HttpServerEv
    = HttpServerEvRequest      (Atom, Word, Word, ()) HttpServerReq
    | HttpServerEvRequestLocal Path                   HttpServerReq
    | HttpServerEvLive         (Atom, ())             Atom (Maybe Word)
    | HttpServerEvBorn         (Atom, ())             ()
    | HttpServerEvSetConfig    (Atom, ())             HttpServerConf
  deriving (Eq, Ord, Show)

deriveNoun ''Address
deriveNoun ''HttpClientEv
deriveNoun ''HttpRequest
deriveNoun ''HttpServerEv
deriveNoun ''HttpServerReq


-- Ames ------------------------------------------------------------------------

data AmesEv
    = AmesEvHear ()   Lane Atom
    | AmesEvWake ()   ()
    | AmesEvWant Path Ship Path Noun
    | AmesEvCrud Path Cord Tang
  deriving (Eq, Ord, Show)

deriveNoun ''AmesEv


-- Arvo Events -----------------------------------------------------------------

data ArvoEv
    = ArvoEvWhom () Ship
    | ArvoEvWack () Word512
  deriving (Eq, Ord, Show)

deriveNoun ''ArvoEv


-- Boat Events -----------------------------------------------------------------

data BoatEv
    = BoatEvBoat () ()
    | BoatEvVoid Void
  deriving (Eq, Ord, Show)

deriveNoun ''BoatEv


-- Timer Events ----------------------------------------------------------------

data BehnEv
    = BehnEvWake ()        ()
    | BehnEvBorn (Wen, ()) ()
  deriving (Eq, Ord, Show)

deriveNoun ''BehnEv


-- Newt Events -----------------------------------------------------------------

data NewtEv
    = NewtEvBarn (Atom, ()) ()
    | NewtEvBorn Void
  deriving (Eq, Ord, Show)

deriveNoun ''NewtEv


-- FileSystem Events -----------------------------------------------------------

data SyncEv
    = SyncEvInto (Nullable (Atom, ())) Desk Bool [(Path, Maybe Mime)]
    | SyncEvCrud Path                  Cord Tang
  deriving (Eq, Ord, Show)

deriveNoun ''SyncEv


-- Terminal Events -------------------------------------------------------------

data LegacyBootEvent
    = Fake Ship
    | Dawn Dawn
  deriving (Eq, Ord, Show)

data ArrowKey = D | L | R | U
  deriving (Eq, Ord, Show)

data Belt
    = Aro ArrowKey
    | Bac ()
    | Ctl Cord
    | Del ()
    | Met Cord
    | Ret ()
    | Txt Tour
  deriving (Eq, Ord, Show)

data TermEv
    = TermEvBelt (Atom, ()) Belt
    | TermEvBlew (Atom, ()) Word Word
    | TermEvBoot (Atom, ()) LegacyBootEvent
    | TermEvHail (Atom, ()) ()
    | TermEvBorn Void
  deriving (Eq, Ord, Show)

deriveNoun ''LegacyBootEvent
deriveNoun ''ArrowKey
deriveNoun ''Belt
deriveNoun ''TermEv


-- Events for Device Drivers ---------------------------------------------------

data BlipEv
    = BlipEvAmes       AmesEv
    | BlipEvArvo       ArvoEv
    | BlipEvBehn       BehnEv
    | BlipEvBoat       BoatEv
    | BlipEvHttpClient HttpClientEv
    | BlipEvHttpServer HttpServerEv
    | BlipEvNewt       NewtEv
    | BlipEvSync       SyncEv
    | BlipEvTerm       TermEv
  deriving (Eq, Ord, Show)

deriveNoun ''BlipEv


-- Boot Events -----------------------------------------------------------------

data Vane
    = VaneVane VaneEv
    | VaneZuse ZuseEv
  deriving (Eq, Ord, Show)

data VaneName
    = Ames | Behn | Clay | Dill | Eyre | Ford | Gall | Iris | Jael
  deriving (Eq, Ord, Show)

data ZuseEv
    = ZOVeer () Cord Path BigTape
    | ZOVoid Void
  deriving (Eq, Ord, Show)

data VaneEv
    = VOVeer (VaneName, ()) Cord Path BigTape
    | VOVoid Void
  deriving (Eq, Ord, Show)

deriveNoun ''Vane
deriveNoun ''VaneName
deriveNoun ''VaneEv
deriveNoun ''ZuseEv


-- The Main Event Type ---------------------------------------------------------

{-
    This parses an ovum in a somewhat complicated way.

    The event structure is not setup to be easily parsed into typed data,
    since the type of the event depends on the head of the path, and
    the shape of the rest of the path depends on the shape of the event.

    To make parsing easier (indeed, to allow use to use `deriveEvent` to
    generate parsers for this) we first re-arrange the data in the ovum.

    And ovum is `[path event]`, but the first two fields of the path
    are used for routing, the event is always a head-tagged structure,
    and the rest of the path is basically data that's a part of the event.

    So, we take something with this struture:

        [[fst snd rest] [tag val]]

    Then restructure it into *this* shape:

        [fst [snd [tag rest val]]]

    And then proceed with parsing as usual.
-}
data Ev
    = EvBlip (Lenient BlipEv)
    | EvVane Vane
  deriving (Eq, Ord, Show)

instance FromNoun Ev where
    parseNoun n = named "Ev" $ do
      (path::Path, tag::Cord, v::Noun) <- parseNoun n
      case path of
        Path (""     : m : p) -> EvBlip <$> parseNoun (toNoun (m, tag, p, v))
        Path ("vane" : m : p) -> EvVane <$> parseNoun (toNoun (m, tag, p, v))
        Path (_:_:_)          -> fail "path must start with %$ or %vane"
        Path (_:_)            -> fail "path too short"
        Path _                -> fail "empty path"

instance ToNoun Ev where
  toNoun o =
      fromNounErr noun & \case
        Left err -> error (show err)
        Right (pathSnd::Knot, tag::Cord, Path path, val::Noun) ->
          toNoun (Path (pathHead:pathSnd:path), (tag, val))
    where
      (pathHead, noun) =
        case o of EvBlip bo -> ("",     toNoun bo)
                  EvVane vo -> ("vane", toNoun vo)
