module Vere.Ovum where

import UrbitPrelude hiding (Term)
import Urbit.Time


-- Misc Types ------------------------------------------------------------------

newtype Octs = Octs ByteString
  deriving newtype (Eq, Ord, Show, FromNoun, ToNoun)

newtype FileOcts = FileOcts ByteString
  deriving newtype (Eq, Ord, ToNoun, FromNoun)

newtype BigTape = BigTape Text
  deriving newtype (Eq, Ord, ToNoun, FromNoun)

newtype Desk = Desk Text
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

data Mime = Mime Path FileOcts
  deriving (Eq, Ord, Show)

instance Show BigTape where
  show (BigTape t) = show (take 32 t <> "...")

instance Show FileOcts where
  show (FileOcts bs) = show (take 32 bs <> "...")

deriveNoun ''Mime


-- Debugging Hack --------------------------------------------------------------

newtype Todo a = Todo a
  deriving newtype (Eq, Ord, ToNoun)

instance Show (Todo a) where
  show (Todo _) = "TODO"

instance FromNoun a => FromNoun (Todo a) where
  parseNoun n = do
      fromNounErr n & \case
        Right x -> pure (Todo x)
        Left er -> do
          -- traceM ("[TODO]: " <> show er <> "\n" <> ppShow n <> "\n")
          fail (show er)


-- Maps and Sets ---------------------------------------------------------------

data NounTreeNode a = HTN
    { ntnNode :: a
    , ntnLeft :: NounTree a
    , ntnRite :: NounTree a
    }
  deriving (Eq, Ord, Show)

type NounTree a = Nullable (NounTreeNode a)

newtype NounMap k v = NounMap (NounTree (k, v))
  deriving newtype (Eq, Ord, Show)

deriveNoun ''NounTreeNode
deriveNoun ''NounMap


-- Json ------------------------------------------------------------------------

type Json = Nullable JsonNode

data JsonNode
    = JNA [Json]
    | JNB Bool
    | JNO (NounMap Text Json)
    | JNN Text -- TODO @ta
    | JNS Text
  deriving (Eq, Ord, Show)

deriveNoun ''JsonNode


-- Dawn Records ----------------------------------------------------------------

type AtomIf = Atom
type Ascii = Text -- TODO @ta

type Ring = Atom -- Private Key
type Oath = Atom -- Signature
type Pass = Atom -- Public Key

type Life = Word
type Turf = Atom
type Czar = NounMap Ship (Life, Pass)
type Bloq = Atom

type Host = Either Turf AtomIf
type Hart = (Bool, Maybe Atom, Host)
type Pork = (Maybe Ascii, [Text])
type Quay = [(Text, Text)]

data PUrl = Prul Hart Pork Quay
  deriving (Eq, Ord, Show)

data Seed = Seed Ship Life Ring (Maybe Oath)
  deriving (Eq, Ord, Show)

data Dawn = MkDawn
    { dSeed :: Seed
    , dShip :: Ship
    , dCzar :: Czar
    , dTurf :: [Turf]
    , dBloq :: Bloq
    , dNode :: Todo PUrl
    }
  deriving (Eq, Ord, Show)

deriveNoun ''PUrl
deriveNoun ''Seed
deriveNoun ''Dawn


-- HTTP ------------------------------------------------------------------------

newtype PEM = PEM Cord
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

type ServerId = Atom
type Key      = PEM
type Cert     = PEM

data Address
    = AIpv4 Atom -- @if
    | AIpv6 Atom -- @is
    | AAmes Atom -- @p
  deriving (Eq, Ord, Show)

data ServerConfig = ServerConfig
    { secure   :: Maybe (Key, Cert)
    , proxy    :: Bool
    , log      :: Bool
    , redirect :: Bool
    }
  deriving (Eq, Ord, Show)

data ResponseHeader = ResponseHeader
    { rhStatus  :: Word
    , rhHeaders :: [(Text, Text)]
    }
  deriving (Eq, Ord, Show)

data HttpEvent
    = Start ResponseHeader (Maybe FileOcts) Bool
    | Continue (Maybe FileOcts) Bool
    | Cancel
  deriving (Eq, Ord, Show)

data HttpRequest = HttpRequest
    { reqId   :: Text
    , reqUrl  :: Text
    , reqHead :: [(Text, Text)]
    , reqBody :: Maybe FileOcts
    }
  deriving (Eq, Ord, Show)

data HttpClient
    = HttpClientReceive (Atom, ()) ServerId HttpEvent
    | HttpClientBorn    (Atom, ()) ()
    | HttpClientCrud    Path       Cord Tang
  deriving (Eq, Ord, Show)

data HttpServer
    = HttpServerRequest   (Atom, Word, Word, ()) ServerId Address HttpRequest
    | HttpServerLive      (Atom, ())             Text (Maybe Word)
    | HttpServerBorn      (Atom, ())             ()
    | HttpServerSetConfig (Atom, ())             ServerConfig
  deriving (Eq, Ord, Show)

deriveNoun ''ServerConfig
deriveNoun ''HttpRequest
deriveNoun ''Address
deriveNoun ''ResponseHeader
deriveNoun ''HttpEvent
deriveNoun ''HttpClient
deriveNoun ''HttpServer


-- Ames ------------------------------------------------------------------------

data Lane
    = If Wen Atom Atom           --  {$if p/@da q/@ud r/@if}
    | Is Atom (Maybe Lane) Atom  --  {$is p/@ud q/(unit lane) r/@is}
    | Ix Wen Atom Atom           --  {$ix p/@da q/@ud r/@if}
  deriving (Eq, Ord, Show)

data Ames
    = AmesHear ()   Lane Atom
    | AmesWake ()   ()
    | AmesCrud Path Cord Tang
  deriving (Eq, Ord, Show)

deriveNoun ''Lane
deriveNoun ''Ames


-- Arvo Events -----------------------------------------------------------------

data Arvo
    = ArvoWhom () Ship
    | ArvoWack () Word512
  deriving (Eq, Ord, Show)

deriveNoun ''Arvo


-- Boat Events -----------------------------------------------------------------

data Boat
    = BoatBoat () ()
    | BoatOvum Void
  deriving (Eq, Ord, Show)

deriveNoun ''Boat


-- Timer Events ----------------------------------------------------------------

data Behn
    = BehnWake ()        ()
    | BehnBorn (Wen, ()) ()
  deriving (Eq, Ord, Show)

deriveNoun ''Behn


-- Newt Events -----------------------------------------------------------------

data Newt
    = NewtBarn (Atom, ()) ()
    | NewtBorn Void
  deriving (Eq, Ord, Show)

deriveNoun ''Newt


-- FileSystem Events -----------------------------------------------------------

data Sync
    = SyncInto (Nullable (Atom, ())) Desk Bool [(Path, Maybe Mime)]
    | SyncCrud Path                  Cord Tang
  deriving (Eq, Ord, Show)

deriveNoun ''Sync


-- Terminal Events -------------------------------------------------------------

data LegacyBootEvent
    = Fake Ship
    | Dawn Dawn
  deriving (Eq, Ord, Show)

data ArrowKey = D | L | R | U
  deriving (Eq, Ord, Show)

data Belt
    = Aro ArrowKey
    | Bac
    | Ctl Char
    | Del
    | Met Char
    | Ret
    | Txt Tour
  deriving (Eq, Ord, Show)

data Term
    = TermBelt (Atom, ()) Belt
    | TermBlew (Atom, ()) Word Word
    | TermBoot (Atom, ()) LegacyBootEvent
    | TermHail (Atom, ()) ()
    | TermBorn Void
  deriving (Eq, Ord, Show)

deriveNoun ''LegacyBootEvent
deriveNoun ''ArrowKey
deriveNoun ''Belt
deriveNoun ''Term


-- Events for Device Drivers ---------------------------------------------------

data Blip
    = BlipAmes       Ames
    | BlipArvo       Arvo
    | BlipBehn       Behn
    | BlipBoat       Boat
    | BlipHttpClient HttpClient
    | BlipHttpServer HttpServer
    | BlipNewt       Newt
    | BlipSync       Sync
    | BlipTerm       Term
  deriving (Eq, Ord, Show)

deriveNoun ''Blip


-- Boot Events -----------------------------------------------------------------

data Vane
    = VaneVane VaneOvum
    | VaneZuse ZuseOvum
  deriving (Eq, Ord, Show)

data VaneName
    = Ames | Behn | Clay | Dill | Eyre | Ford | Gall | Iris | Jael
  deriving (Eq, Ord, Show)

data ZuseOvum
    = ZOVeer () Cord Path BigTape
    | ZOVoid Void
  deriving (Eq, Ord, Show)

data VaneOvum
    = VOVeer (VaneName, ()) Cord Path BigTape
    | VOVoid Void
  deriving (Eq, Ord, Show)

deriveNoun ''Vane
deriveNoun ''VaneName
deriveNoun ''VaneOvum
deriveNoun ''ZuseOvum


-- Ovums -- The Main Event Type ------------------------------------------------

{-
    This parses an ovum in a somewhat complicated way.

    The Ovum structure is not setup to be easily parsed into typed data,
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
data Ovum
    = OvumBlip (Lenient Blip)
    | OvumVane Vane
  deriving (Eq, Ord, Show)

instance FromNoun Ovum where
    parseNoun n = named "Ovum" $ do
      (path::Path, tag::Cord, v::Noun) <- parseNoun n
      case path of
        Path (""     : m : p) -> OvumBlip <$> parseNoun (toNoun (m, tag, p, v))
        Path ("vane" : m : p) -> OvumVane <$> parseNoun (toNoun (m, tag, p, v))
        Path (_:_:_)          -> fail "path must start with %$ or %vane"
        Path (_:_)            -> fail "path too short"
        Path _                -> fail "empty path"

instance ToNoun Ovum where
  toNoun o =
      fromNounErr noun & \case
        Left err -> error (show err)
        Right (pathSnd::Knot, tag::Cord, Path path, val::Noun) ->
          toNoun (Path (pathHead:pathSnd:path), (tag, val))
    where
      (pathHead, noun) =
        case o of OvumBlip bo -> ("",     toNoun bo)
                  OvumVane vo -> ("vane", toNoun vo)
