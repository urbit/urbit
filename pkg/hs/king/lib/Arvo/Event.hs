module Arvo.Event where

import Noun.Tree    (HoonMap, HoonSet)
import UrbitPrelude hiding (Term)

import Arvo.Common (KingId(..), ServId(..))
import Arvo.Common (Desk, Mime)
import Arvo.Common (Header(..), HttpEvent)
import Arvo.Common (AmesDest, Ipv4, Ipv6, Port, Turf)
import Arvo.Common (ReOrg(..), reorgThroughNoun)

import qualified Crypto.Sign.Ed25519       as Ed
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as C
import qualified Network.HTTP.Types.Method as H

-- Misc Types ------------------------------------------------------------------

type Rift = Atom -- Continuity number
type Life = Word -- Number of Azimoth key revs.
type Bloq = Atom -- TODO
type Oath = Atom -- Signature

-- Parsed URLs -----------------------------------------------------------------

type Host = Each Turf Ipv4
type Hart = (Bool, Maybe Atom, Host)
type Pork = (Maybe Knot, [Cord])
type Quay = [(Cord, Cord)]

data PUrl = PUrl Hart Pork Quay
  deriving (Eq, Ord, Show)

deriveNoun ''PUrl


-- Dawn Records ----------------------------------------------------------------

padByteString :: BS.ByteString -> Int -> BS.ByteString
padByteString bs length | remaining > 0 = bs <> (BS.replicate remaining 0)
                        | otherwise = bs
  where remaining = (length - (BS.length bs))

-- A Pass is the Atom concatenation of 'b', the public encryption key, and the
-- public authentication key. (see +pass-from-eth.)
data Pass = Pass { passSign :: Ed.PublicKey, passCrypt :: Ed.PublicKey }
  deriving (Eq, Ord, Show)

passToBS :: Pass -> BS.ByteString
passToBS Pass{..} = C.singleton 'b' <>
                    (Ed.unPublicKey passSign) <>
                    (Ed.unPublicKey passCrypt)

instance ToNoun Pass where
  toNoun p = Atom $ (passToBS p) ^. from atomBytes

instance FromNoun Pass where
  parseNoun n = named "Pass" $ do
    MkBytes unpadded <- parseNoun n
    let bs = padByteString unpadded 65
    when ((C.head bs) /= 'b') $ do
      fail "Expecting 'b' prefix in public key structure"
    let removedPrefix = C.tail bs
    let passSign = Ed.PublicKey (take 32 removedPrefix)
    let passCrypt = Ed.PublicKey (drop 32 removedPrefix)
    unless ((length $ Ed.unPublicKey passSign) == 32) $
      error "Sign pubkey not 32 bytes"
    unless ((length $ Ed.unPublicKey passCrypt) == 32) $
      error "Crypt pubkey not 32 bytes"
    pure $ Pass{..}

-- A Ring isn't the secret keys: it's the ByteString input which generates both
-- the public key and the secret key. A Ring is the concatenation of 'B', the
-- encryption key derivation seed, and the authentication key derivation
-- seed. These aren't actually private keys, but public/private keypairs which
-- can be derived from these seeds.
data Ring = Ring { ringSign :: BS.ByteString, ringCrypt :: BS.ByteString }
  deriving (Eq)

instance ToNoun Ring where
  toNoun Ring{..} =
    Atom $ bs ^. from atomBytes
    where
      bs = C.singleton 'B' <> ringSign <> ringCrypt

instance FromNoun Ring where
  parseNoun n = named "Ring" $ do
      MkBytes unpadded <- parseNoun n
      let bs = padByteString unpadded 65
      when ((C.head bs) /= 'B') $ do
        fail "Expecting 'B' prefix in public key structure"
      let removedPrefix = C.tail bs
      let ringSign = (take 32 removedPrefix)
      let ringCrypt = (drop 32 removedPrefix)
      unless ((length ringSign) == 32) $
        error "Sign seed not 32 bytes"
      unless ((length ringCrypt) == 32) $
        error "Crypt seed not 32 bytes"
      pure $ Ring ringSign ringCrypt

instance Show Ring where
  show r = "(Ring <<seed>> <<seed>>)"

data Seed = Seed
    { sShip :: Ship
    , sLife :: Life
    , sRing :: Ring
    , sOath :: (Maybe Oath)
    }
  deriving (Eq, Show)

type Public = (Life, HoonMap Life Pass)

data Dnses = Dnses { dPri::Cord, dSec::Cord, dTer::Cord }
  deriving (Eq, Ord, Show)

type EthAddr = Atom --Bytes -- 20 bytes
type ContNum = Word

data EthPoint = EthPoint
    { epOwn :: (EthAddr, EthAddr, EthAddr, EthAddr)
    , epNet :: Maybe (Life, Pass, ContNum, (Bool, Ship), Maybe Ship)
    , epKid :: Maybe (EthAddr, HoonSet Ship)
    }
  deriving (Eq, Show)

data Dawn = MkDawn
    { dSeed    :: Seed
    , dSponsor :: [(Ship, EthPoint)]
    , dCzar    :: HoonMap Ship (Rift, Life, Pass)
    , dTurf    :: [Turf]
    , dBloq    :: Bloq
    , dNode    :: (Maybe PUrl)
    }
  deriving (Eq, Show)

deriveNoun ''Dnses
deriveNoun ''EthPoint
deriveNoun ''Seed
deriveNoun ''Dawn


-- HTTP ------------------------------------------------------------------------

type ServerId = Atom

data Address
    = AIpv4 Ipv4
    | AIpv6 Ipv6
    | AAmes Ship
  deriving (Eq, Ord, Show)

data HttpRequest = HttpRequest
    { reqMeth :: H.StdMethod
    , reqUrl  :: Cord
    , reqHead :: [Header]
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
    = HttpClientEvReceive (KingId, ()) ServerId HttpEvent
    | HttpClientEvBorn    (KingId, ()) ()
    | HttpClientEvCrud    Path       Cord Tang
  deriving (Eq, Ord, Show)

data HttpServerEv
    = HttpServerEvRequest       (ServId, Atom, UD, ()) HttpServerReq
    | HttpServerEvCancelRequest (ServId, Atom, UD, ()) ()
    | HttpServerEvRequestLocal  (ServId, Atom, UD, ()) HttpServerReq
    | HttpServerEvLive          (ServId, ())         Port (Maybe Port)
    | HttpServerEvBorn          (KingId, ())         ()
    | HttpServerEvCrud          Path                 Cord Tang
  deriving (Eq, Ord, Show)

deriveNoun ''Address
deriveNoun ''HttpClientEv
deriveNoun ''HttpRequest
deriveNoun ''HttpServerEv
deriveNoun ''HttpServerReq


-- Ames ------------------------------------------------------------------------

data AmesEv
    = AmesEvHear ()   AmesDest Bytes
    | AmesEvHole ()   AmesDest Bytes
    | AmesEvCrud Path Cord Tang
  deriving (Eq, Ord, Show)

deriveNoun ''AmesEv


-- Arvo Events -----------------------------------------------------------------

data ArvoEv
    = ArvoEvWhom () Ship
    | ArvoEvWack () Word512
    | ArvoEvWarn Path Noun
    | ArvoEvCrud Path Cord Tang
    | ArvoEvVeer Atom Noun
  deriving (Eq, Ord, Show)

deriveNoun ''ArvoEv


-- Boat Events -----------------------------------------------------------------

data BoatEv
    = BoatEvBoat ()   ()
    | BoatEvCrud Path Cord Tang
  deriving (Eq, Ord, Show)

deriveNoun ''BoatEv


-- Timer Events ----------------------------------------------------------------

data BehnEv
    = BehnEvWake ()           ()
    | BehnEvBorn (KingId, ()) ()
    | BehnEvCrud Path         Cord Tang
  deriving (Eq, Ord, Show)

deriveNoun ''BehnEv


-- Newt Events -----------------------------------------------------------------

data NewtEv
    = NewtEvBorn (KingId, ()) ()
    | NewtEvCrud Path         Cord Tang
  deriving (Eq, Ord, Show)

deriveNoun ''NewtEv


-- FileSystem Events -----------------------------------------------------------

data SyncEv
    = SyncEvInto (Nullable (KingId, ())) Desk Bool [(Path, Maybe Mime)]
    | SyncEvCrud Path                    Cord Tang
  deriving (Eq, Ord, Show)

deriveNoun ''SyncEv


-- Terminal Events -------------------------------------------------------------

data LegacyBootEvent
    = Fake Ship
    | Dawn Dawn
  deriving (Eq, Show)

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
    = TermEvBelt (UD, ()) Belt
    | TermEvBlew (UD, ()) Word Word
    | TermEvBoot (UD, ()) Bool LegacyBootEvent
    | TermEvHail (UD, ()) ()
    | TermEvCrud Path       Cord Tang
  deriving (Eq, Show)

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
  deriving (Eq, Show)

deriveNoun ''BlipEv


-- Boot Events -----------------------------------------------------------------

data Vane
    = VaneVane VaneEv
    | VaneZuse ZuseEv
  deriving (Eq, Ord, Show)

data VaneName
    = Ames | Behn | Clay | Dill | Eyre | Ford | Gall | Iris | Jael
  deriving (Eq, Ord, Show, Enum, Bounded)

data ZuseEv
    = ZEVeer () Cord Path BigCord
    | ZEVoid Void
  deriving (Eq, Ord, Show)

data VaneEv
    = VEVeer (VaneName, ()) Cord Path BigCord
    | VEVoid Void
  deriving (Eq, Ord, Show)

deriveNoun ''Vane
deriveNoun ''VaneName
deriveNoun ''VaneEv
deriveNoun ''ZuseEv


-- The Main Event Type ---------------------------------------------------------

data Ev
    = EvBlip BlipEv
    | EvVane Vane
  deriving (Eq, Show)

instance ToNoun Ev where
  toNoun = \case
    EvBlip v -> toNoun $ reorgThroughNoun (Cord "",     v)
    EvVane v -> toNoun $ reorgThroughNoun (Cord "vane", v)

instance FromNoun Ev where
  parseNoun = parseNoun >=> \case
    ReOrg ""     s t p v -> fmap EvBlip $ parseNoun $ toNoun (s,t,p,v)
    ReOrg "vane" s t p v -> fmap EvVane $ parseNoun $ toNoun (s,t,p,v)
    ReOrg _      _ _ _ _ -> fail "First path-elem must be ?($ %vane)"

-- Short Event Names -----------------------------------------------------------

{-
    In the case of the user hitting enter, the cause is technically a
    terminal event, but we don't display any name because the cause is
    really the user.
-}
getSpinnerNameForEvent :: Ev -> Maybe Text
getSpinnerNameForEvent = \case
    EvVane _ -> Nothing
    EvBlip b -> case b of
        BlipEvAmes _           -> Just "ames"
        BlipEvArvo _           -> Just "arvo"
        BlipEvBehn _           -> Just "behn"
        BlipEvBoat _           -> Just "boat"
        BlipEvHttpClient _     -> Just "iris"
        BlipEvHttpServer _     -> Just "eyre"
        BlipEvNewt _           -> Just "newt"
        BlipEvSync _           -> Just "clay"
        BlipEvTerm t | isRet t -> Nothing
        BlipEvTerm t           -> Just "term"
  where
    isRet (TermEvBelt _ (Ret ())) = True
    isRet _                       = False
