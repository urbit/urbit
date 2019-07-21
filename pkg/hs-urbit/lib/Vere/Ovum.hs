{-# LANGUAGE UndecidableInstances #-}

module Vere.Ovum (Ovum(..), Event) where

import UrbitPrelude
import Urbit.Time

import qualified Vere.Ames        as Ames
import qualified Vere.Http.Client as Client
import qualified Vere.Http.Server as Server


-- Misc Types ------------------------------------------------------------------

newtype Octs = Octs ByteString
  deriving newtype (Eq, Ord, Show, FromNoun, ToNoun)

newtype FileOcts = FileOcts ByteString
  deriving newtype (Eq, Ord, ToNoun, FromNoun)

newtype BigTape = BigTape Text
  deriving newtype (Eq, Ord, ToNoun, FromNoun)

newtype Todo a = Todo a
  deriving newtype (Eq, Ord, ToNoun, FromNoun)

instance Show (Todo a) where
  show (Todo _) = "TODO"

instance Show FileOcts where
  show (FileOcts bs) = show (take 32 bs <> "...")

instance Show BigTape where
  show (BigTape t) = show (take 32 t <> "...")


-- Maps and Sets ---------------------------------------------------------------

data NounTreeNode a = HTN
    { ntnNode :: a
    , ntnLeft :: NounTree a
    , ntnRite :: NounTree a
    }
  deriving (Eq, Ord, Show)

type NounTree a = Maybe (NounTreeNode a)

deriveNoun ''NounTreeNode

newtype (NounMap k v) = NounMap (NounTree (k, v))
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)


-- Json ------------------------------------------------------------------------

type Json = Maybe JsonNode

data JsonNode
    = JNA [Json]
    | JNB Bool
    | JNO (NounMap Text Json)
    | JNN Text -- TODO @ta
    | JNS Text

deriveNoun ''JsonNode


--------------------------------------------------------------------------------

type Life = Noun
type Pass = Noun
type Turf = Noun
type PUrl = Todo Noun
type Seed = Todo Noun
type Czar = NounMap Ship (Life, Pass)
type Bloq = Atom

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

newtype Desk = Desk Text
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

data Mime = Mime Path FileOcts
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
    = EHttpServer Server.Eff
    | EHttpClient Client.Eff
    | EAmes Ames.Eff
    | EBbye Noun
    | EBehn Noun
    | EBlit [Blit]
    | EBoat Noun
    | EClay Noun
    | ECrud Noun
    | EDirk Noun
    | EDoze (Maybe Wen)
    | EErgo Noun
    | EExit Noun
    | EFlog Noun
    | EForm Noun
    | EHill [Term]
    | EInit
    | ELogo Noun
    | EMass Noun
    | ENewt Noun
    | EOgre Noun
    | ESend [Blit]
    | ESync Noun
    | ETerm Noun
    | EThou Noun
    | ETurf (Maybe (PutDel, [Text])) -- TODO Unsure
    | EVega Noun
    | EWest Noun
    | EWoot Noun
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

--------------------------------------------------------------------------------

{-
    This parses an ovum in a slightly complicated way.

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
  = OBlip BlipOvum
  | OVane VaneOvum

instance FromNoun Ovum where
    parseNoun n = named "Ovum" $ do
      (path::Path, tag::Cord, v::Noun) <- parseNoun n
      case path of
        Path (""     : m : p) -> OBlip <$> parseNoun (toNoun (m, tag, p, v))
        Path ("vane" : m : p) -> OVane <$> parseNoun (toNoun (m, tag, p, v))
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
        case o of OBlip bo -> ("",     toNoun bo)
                  OVane vo -> ("vane", toNoun vo)

--------------------------------------------------------------------------------


type AmesOvum = Void
type ArvoOvum = Void
type BehnOvum = Void
type BoatOvum = Void
type HttpClientOvum = Void
type HttpServerOvum = Void
type NewtOvum = Void
type SyncOvum = Void
type TermOvum = Void

data BlipOvum
    = BOAmes       AmesOvum
    | BOArvo       ArvoOvum
    | BOBehn       BehnOvum
    | BOBoat       BoatOvum
    | BOHttpClient HttpClientOvum
    | BOHttpServer HttpServerOvum
    | BONewt       NewtOvum
    | BOSync       SyncOvum
    | BOTerm       TermOvum

data KernelModule
    = Ames | Behn | Clay | Dill | Eyre | Ford | Gall | Iris | Jael

data VaneOvum
    = VOVane (KernelModule, ()) Void
    | VOZuse ()                 Void


-- Instances -------------------------------------------------------------------

deriveNoun ''ArrowKey
deriveNoun ''Belt
deriveNoun ''BlipOvum
deriveNoun ''Blit
deriveNoun ''Dawn
deriveNoun ''Eff
deriveNoun ''Event
deriveNoun ''HttpEvent
deriveNoun ''HttpRequest
deriveNoun ''KernelModule
deriveNoun ''Lane
deriveNoun ''LegacyBootEvent
deriveNoun ''Mime
deriveNoun ''NewtEx
deriveNoun ''RecEx
deriveNoun ''RequestParams
deriveNoun ''ResponseHeader
deriveNoun ''VaneOvum
