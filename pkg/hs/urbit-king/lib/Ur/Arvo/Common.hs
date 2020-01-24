{-|
    Types used in both Events and Effects.
-}
module Ur.Arvo.Common
  ( KingId(..), ServId(..)
  , Json, JsonNode(..)
  , Desk(..), Mime(..)
  , Port(..), Turf(..)
  , HttpServerConf(..), PEM(..), Key, Cert
  , HttpEvent(..), Method, Header(..), ResponseHeader(..)
  , ReOrg(..), reorgThroughNoun
  , AmesDest(..), Ipv4(..), Ipv6(..), Patp(..), Galaxy, AmesAddress(..)
  ) where

import Ur.Prelude hiding (Term)

import qualified Network.HTTP.Types.Method as H
import qualified Urbit.Ob                  as Ob


-- Misc Types ------------------------------------------------------------------

{-|
    Domain Name in TLD order:

        ["org", "urbit", "dns"] -> dns.urbit.org
-}
newtype Turf = Turf { unTurf :: [Cord] }
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

newtype KingId = KingId { unKingId :: UV }
  deriving newtype (Eq, Ord, Show, Num, Real, Enum, Integral, FromNoun, ToNoun)

newtype ServId = ServId { unServId :: UV }
  deriving newtype (Eq, Ord, Show, Num, Enum, Integral, Real, FromNoun, ToNoun)


-- Http Common -----------------------------------------------------------------

data Header = Header Cord Bytes
  deriving (Eq, Ord, Show)

data ResponseHeader = ResponseHeader
     { statusCode :: Word
     , headers    :: [Header]
     }
  deriving (Eq, Ord, Show)

data HttpEvent
    = Start ResponseHeader (Maybe File) Bool
    | Continue (Maybe File) Bool
    | Cancel ()
  deriving (Eq, Ord, Show)

deriveNoun ''ResponseHeader
deriveNoun ''Header
deriveNoun ''HttpEvent


-- Http Requests ---------------------------------------------------------------

type Method = H.StdMethod

-- TODO Hack! Don't write instances for library types. Write them for
-- our types instead.

instance ToNoun H.StdMethod where
  toNoun = toNoun . MkBytes . H.renderStdMethod

instance FromNoun H.StdMethod where
  parseNoun n = named "StdMethod" $ do
    MkBytes bs <- parseNoun n
    case H.parseMethod bs of
      Left md -> fail ("Unexpected method: " <> unpack (decodeUtf8 md))
      Right m -> pure m



-- Http Server Configuration ---------------------------------------------------

newtype PEM = PEM { unPEM :: Cord }
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

type Key  = PEM
type Cert = PEM

data HttpServerConf = HttpServerConf
    { hscSecure   :: Maybe (Key, Cert)
    , hscProxy    :: Bool
    , hscLog      :: Bool
    , hscRedirect :: Bool
    }
  deriving (Eq, Ord, Show)

deriveNoun ''HttpServerConf


-- Desk and Mime ---------------------------------------------------------------

newtype Desk = Desk { unDesk :: Cord }
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

data Mime = Mime Path File
  deriving (Eq, Ord, Show)

deriveNoun ''Mime


-- Json ------------------------------------------------------------------------

type Json = Nullable JsonNode

data JsonNode
    = JNA [Json]
    | JNB Bool
    | JNO (HoonMap Cord Json)
    | JNN Knot
    | JNS Cord
  deriving (Eq, Ord, Show)

deriveNoun ''JsonNode


-- Ames Destinations -------------------------------------------------

newtype Patp a = Patp { unPatp :: a }
  deriving newtype (Eq, Ord, Enum, Real, Integral, Num, ToNoun, FromNoun)

-- Network Port
newtype Port = Port { unPort :: Word16 }
  deriving newtype (Eq, Ord, Show, Enum, Real, Integral, Num, ToNoun, FromNoun)

-- @if
newtype Ipv4 = Ipv4 { unIpv4 :: Word32 }
  deriving newtype (Eq, Ord, Show, Enum, Real, Integral, Num, ToNoun, FromNoun)

-- @is
newtype Ipv6 = Ipv6 { unIpv6 :: Word128 }
  deriving newtype (Eq, Ord, Show, Enum, Real, Integral, Num, ToNoun, FromNoun)

type Galaxy = Patp Word8

instance Integral a => Show (Patp a) where
  show = show . Ob.renderPatp . Ob.patp . fromIntegral . unPatp

data AmesAddress
    = AAIpv4 Ipv4 Port
    | AAVoid Void
  deriving (Eq, Ord, Show)

deriveNoun ''AmesAddress

type AmesDest = Each Galaxy (Jammed AmesAddress)


-- Path+Tagged Restructuring ---------------------------------------------------

{-|
    This reorganized events and effects to be easier to parse. This is
    complicated and gross, and a better way should be found!

    ReOrg takes in nouns with the following shape:

        [[fst snd rest] [tag val]]

    And turns that into:

        ReOrg fst snd tag rest val

    For example,

        [//behn/5 %doze ~ 9999]

    Becomes:

        Reorg "" "behn" "doze" ["5"] 9999

    This is convenient, since we can then use our head-tag based FromNoun
    and ToNoun instances.

    NOTE:

        Also, in the wild, I ran into this event:

            [//term/1 %init]

        So, I rewrite atom-events as follows:

            [x y=@] -> [x [y ~]]

        Which rewrites the %init example to:

            [//term/1 [%init ~]]

        TODO The reverse translation is not done yet.

-}
data ReOrg = ReOrg Cord Cord Cord EvilPath Noun

instance FromNoun ReOrg where
  parseNoun = named "ReOrg" . \case
      A _                     -> expected "got atom"
      C (A _)         _       -> expected "empty route"
      C h             (A a)   -> parseNoun (C h (C (A a) (A 0)))
      C (C _ (A _))   (C _ _) -> expected "route is too short"
      C (C f (C s p)) (C t v) -> do
        fst :: Cord     <- named "first-route"   $ parseNoun f
        snd :: Cord     <- named "second-route"  $ parseNoun s
        pax :: EvilPath <- named "rest-of-route" $ parseNoun p
        tag :: Cord     <- named "tag"           $ parseNoun t
        val :: Noun     <- pure v
        pure (ReOrg fst snd tag pax val)
    where
      expected got = fail ("expected route+tagged; " <> got)

instance ToNoun ReOrg where
  toNoun (ReOrg fst snd tag pax val) =
    toNoun ((fst, snd, pax), (tag, val))

{-|
    Given something parsed from a ReOrg Noun, convert that back to
    a ReOrg.

    This code may crash, but only if the FromNoun/ToNoun instances for
    the effects are incorrect.
-}
reorgThroughNoun :: ToNoun x => (Cord, x) -> ReOrg
reorgThroughNoun =
    fromNounCrash . toNoun >>> \case
        (f, s, t, p, v) -> ReOrg f s t p v
  where
    fromNounCrash :: FromNoun a => Noun -> a
    fromNounCrash =
      fromNounErr >>> \case
        Left err -> error (show err)
        Right vl -> vl
