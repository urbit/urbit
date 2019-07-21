{-# OPTIONS_GHC -Wwarn #-}

module Vere.Ovum (Ovum, muckOvum, Todo(..)) where

import UrbitPrelude hiding (Term)
import Urbit.Time


-- Misc Types ------------------------------------------------------------------

newtype Octs = Octs ByteString
  deriving newtype (Eq, Ord, Show, FromNoun, ToNoun)

newtype FileOcts = FileOcts ByteString
  deriving newtype (Eq, Ord, ToNoun, FromNoun)

newtype BigTape = BigTape Text
  deriving newtype (Eq, Ord, ToNoun, FromNoun)

newtype Todo a = Todo a
  deriving newtype (Eq, Ord, ToNoun)

instance Show (Todo a) where
  show (Todo _) = "TODO"

instance FromNoun a => FromNoun (Todo a) where
  parseNoun n = do
      fromNounErr n & \case
        Right x -> pure (Todo x)
        Left er -> do
          traceM ("[TODO]: " <> show er <> "\n" <> ppShow n <> "\n")
          fail (show er)

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

type NounTree a = Nullable (NounTreeNode a)

newtype NounMap k v = NounMap (NounTree (k, v))
  deriving newtype (Eq, Ord, Show)


-- Json ------------------------------------------------------------------------

type Json = Nullable JsonNode

data JsonNode
    = JNA [Json]
    | JNB Bool
    | JNO (NounMap Text Json)
    | JNN Text -- TODO @ta
    | JNS Text
  deriving (Eq, Ord, Show)



-- Parsed Urls -----------------------------------------------------------------

type AtomIf = Atom
type Ascii = Text -- TODO @ta

type Host = Either Turf AtomIf
type Hart = (Bool, Maybe Atom, Host)
type Pork = (Maybe Ascii, [Text])
type Quay = [(Text, Text)]

data PUrl = Prul Hart Pork Quay
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

type Ring = Atom -- Private Key
type Oath = Atom -- Signature
type Pass = Atom -- Public Key

type Life = Word
type Turf = Atom
type Czar = NounMap Ship (Life, Pass)
type Bloq = Atom

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
    = Start ResponseHeader (Maybe FileOcts) Bool
    | Continue (Maybe FileOcts) Bool
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

data RequestParams
    = List [Json]
    | Object [(Text, Json)]
  deriving (Eq, Ord, Show)

data HttpRequest = HttpRequest
    { reqId   :: Text
    , reqUrl  :: Text
    , reqHead :: [(Text, Text)]
    , reqBody :: Maybe FileOcts
    }
  deriving (Eq, Ord, Show)

data Event
    = Veer Path Cord Path BigTape
    | Into Path Desk Bool [(Path, Maybe Mime)]
    | Whom Path Ship
    | Boot Path LegacyBootEvent
    | Wack Path Word512
    | Boat Path ()
    | Barn Path ()
    | Born Path ()
    | Blew Path Word Word
    | Hail Path ()
    | Wake Path ()
    | Receive Path ServerId HttpEvent
    | Request Path ServerId Address HttpRequest
    | Live Path Text Bool Word
    | Hear Path Lane Atom
    | Belt Path Belt
    | Crud Path Text [Tank]
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


-- Ovums -----------------------------------------------------------------------

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
    = OvumBlip Blip
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

muckOvum :: Noun -> Maybe Noun
muckOvum n = do
    (Path(t:m:p), tag::Cord, v::Noun) <- fromNoun n
    pure $ toNoun $ case t of
                      "" -> ("blip", m, tag, Path p, v)
                      _  -> (t     , m, tag, Path p, v)

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

--------------------------------------------------------------------------------


data Ames
    = AmesHear ()   Lane Atom
    | AmesWake ()   ()
    | AmesCrud Path Cord Tang
  deriving (Eq, Ord, Show)

data Arvo
    = ArvoWhom () Ship
    | ArvoWack () Word512
  deriving (Eq, Ord, Show)

data Behn
    = BehnWake ()        ()
    | BehnBorn (Wen, ()) ()
  deriving (Eq, Ord, Show)

data Boat
    = BoatBoat () ()
    | BoatOvum Void
  deriving (Eq, Ord, Show)

data HttpClient
    = HttpClientReceive (Atom, ()) ServerId HttpEvent
    | HttpClientBorn    (Atom, ()) ()
    | HttpClientCrud    Path       Cord Tang
  deriving (Eq, Ord, Show)

data HttpServer
    = HttpServerRequest (Atom, Word, Word, ()) ServerId Address HttpRequest
    | HttpServerLive    (Atom, ())             Text (Maybe Word)
    | HttpServerBorn    (Atom, ())             ()
  deriving (Eq, Ord, Show)

data Newt
    = NewtBarn (Atom, ()) ()
    | NewtBorn Void
  deriving (Eq, Ord, Show)

data Sync
    = SyncInto (Nullable (Atom, ())) Desk Bool [(Path, Maybe Mime)]
    | SyncCrud Path                  Cord Tang
  deriving (Eq, Ord, Show)

data Term
    = TermBelt (Atom, ()) Belt
    | TermBlew (Atom, ()) Word Word
    | TermBoot (Atom, ()) LegacyBootEvent
    | TermHail (Atom, ()) ()
    | TermBorn Void
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

data Vane
    = VaneVane VaneOvum
    | VaneZuse ZuseOvum
  deriving (Eq, Ord, Show)

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

--------------------------------------------------------------------------------

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


-- Instances -------------------------------------------------------------------

deriveNoun ''Seed
deriveNoun ''PUrl
deriveNoun ''Vane
deriveNoun ''VaneOvum
deriveNoun ''ZuseOvum
deriveNoun ''NounTreeNode
deriveNoun ''NounMap
deriveNoun ''JsonNode
deriveNoun ''Ames
deriveNoun ''Arvo
deriveNoun ''Behn
deriveNoun ''Boat
deriveNoun ''HttpClient
deriveNoun ''HttpServer
deriveNoun ''Newt
deriveNoun ''Sync
deriveNoun ''Term
deriveNoun ''Address
deriveNoun ''ArrowKey
deriveNoun ''Belt
deriveNoun ''Blip
deriveNoun ''Blit
deriveNoun ''Dawn
deriveNoun ''Event
deriveNoun ''HttpEvent
deriveNoun ''HttpRequest
deriveNoun ''Lane
deriveNoun ''LegacyBootEvent
deriveNoun ''Mime
deriveNoun ''NewtEx
deriveNoun ''RecEx
deriveNoun ''RequestParams
deriveNoun ''ResponseHeader
deriveNoun ''VaneName
