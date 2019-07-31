module Arvo.Common
  ( NounTree(..), NounMap, NounSet
  , Json, JsonNode(..)
  , Desk(..), Mime(..)
  , Lane(..), Port(..), Turf(..)
  , HttpServerConf(..), HttpEvent(..), PEM, Method, Header
  , ReOrg(..), reorgThroughNoun
  , AmesDest(..), Ipv4(..), Ipv6(..), Galaxy(..)
  ) where

import Urbit.Time
import UrbitPrelude hiding (Term)

import qualified Network.HTTP.Types.Method as H


-- Misc Types ------------------------------------------------------------------

{-
    Domain Name in TLD order:

        ["org", "urbit", "dns"] -> dns.urbit.org
-}
newtype Turf = Turf { unTurf :: [Cord] }
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)


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
  toNoun = toNoun . Cord . decodeUtf8 . H.renderStdMethod

instance FromNoun H.StdMethod where
  parseNoun n = named "StdMethod" $ do
    Cord m <- parseNoun n
    case H.parseMethod (encodeUtf8 m) of
      Left bs -> fail ("Unexpected method: " <> unpack (decodeUtf8 bs))
      Right m -> pure m



-- Http Server Configuration ---------------------------------------------------

newtype PEM = PEM Cord
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

type Key  = PEM
type Cert = PEM

data HttpServerConf = HttpServerConf
    { secure   :: Maybe (Key, Cert)
    , proxy    :: Bool
    , log      :: Bool
    , redirect :: Bool
    }
  deriving (Eq, Ord, Show)

deriveNoun ''HttpServerConf


-- Desk and Mime ---------------------------------------------------------------

newtype Desk = Desk { unDesk :: Cord }
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

data Mime = Mime Path File
  deriving (Eq, Ord, Show)

deriveNoun ''Mime


-- Trees, Maps, and Sets -------------------------------------------------------

data NounTreeNode a = HTN
    { ntnNode :: a
    , ntnLeft :: NounTree a
    , ntnRite :: NounTree a
    }
  deriving (Eq, Ord, Show)

type NounTree a = Nullable (NounTreeNode a)

type NounMap k v = NounTree (k, v)
type NounSet a   = NounTree a

deriveNoun ''NounTreeNode


-- Json ------------------------------------------------------------------------

type Json = Nullable JsonNode

data JsonNode
    = JNA [Json]
    | JNB Bool
    | JNO (NounMap Cord Json)
    | JNN Knot
    | JNS Cord
  deriving (Eq, Ord, Show)

deriveNoun ''JsonNode


-- Lanes and Ames Destinations -------------------------------------------------

-- Network Port
newtype Port = Port { unPort :: Word16 }
  deriving newtype (Eq, Ord, Show, Enum, Real, Integral, Num, ToNoun, FromNoun)

-- @if
newtype Ipv4 = Ipv4 { unIpv4 :: Word32 }
  deriving newtype (Eq, Ord, Show, Enum, Real, Integral, Num, ToNoun, FromNoun)

-- @is
newtype Ipv6 = Ipv6 { unIpv6 :: Word128 }
  deriving newtype (Eq, Ord, Show, Enum, Real, Integral, Num, ToNoun, FromNoun)

newtype Galaxy = Galaxy { unGalaxy :: Word8 }
  deriving newtype (Eq, Ord, Show, Enum, Real, Integral, Num, ToNoun, FromNoun)

{-
    The `Wen` field is (IIUC) the last time that we were sure that this
    DNS lookup worked.  This is set when we receive a %hear event.
-}
data Lane
    = If Wen Port Ipv4           -- Ipv4
    | Is Port (Maybe Lane) Ipv6  -- Ipv6 with fallback
    | Ix Wen Port Ipv4           -- Not used (Same behavior as `If`)
  deriving (Eq, Ord, Show)

deriveNoun ''Lane

data AmesDest
    = ADGala Wen Galaxy
    | ADIpv4 Wen Port Ipv4
  deriving (Eq, Ord, Show)

instance ToNoun AmesDest where
  toNoun = toNoun . \case
    ADGala w g   -> If w 0 (256 + fromIntegral g)
    ADIpv4 w p a -> If w p a

instance FromNoun AmesDest where
  parseNoun = named "AmesDest" . (parseNoun >=> parseLane)
    where
    parseLane :: Lane -> Parser AmesDest
    parseLane = \case
      If w _ 0                  -> fail "Sending to 0.0.0.0 is not supported"
      If w _ a | a>255 && a<512 -> pure $ ADGala w $ fromIntegral $ a `mod` 256
      If w p a                  -> pure $ ADIpv4 w p a
      Ix w p a                  -> parseLane (If w p a)
      Is _ (Just fb) _          -> parseLane fb
      Is _ Nothing   _          -> fail "ipv6 is not supported"


-- Path+Tagged Restructuring ---------------------------------------------------

{-
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

{-
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
