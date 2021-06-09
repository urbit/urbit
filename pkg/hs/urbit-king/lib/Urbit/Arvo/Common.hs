{-# LANGUAGE StrictData #-}

-- This is required due to the use of 'Void' in a constructor slot in
-- combination with 'deriveNoun' which generates an unreachable pattern.
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- Hack. See comment above instance ToNoun H.StdMethod
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
    Types used in both Events and Effects.
-}
module Urbit.Arvo.Common
  ( KingId(..), ServId(..)
  , Vere(..), Wynn(..)
  , Json, JsonNode(..)
  , Desk(..), Mime(..)
  , Port(..), Turf(..)
  , HttpServerConf(..), PEM(..), Key, Cert
  , HttpEvent(..), Method, Header(..), ResponseHeader(..)
  , ReOrg(..), reorgThroughNoun
  , AmesDest, Ipv4(..), Ipv6(..), Patp(..), Galaxy, AmesAddress(..), SocketConf(..)
  ) where

import Urbit.Prelude

import Control.Monad.Fail (fail)
import Data.Serialize

import qualified Network.HTTP.Types.Method as H
import qualified Network.Socket            as N
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

-- Arvo Version Negotiation ----------------------------------------------------

-- Information about the king runtime passed to Arvo.
data Vere = Vere { vereName :: Term,
                   vereRev  :: [Cord],
                   vereWynn :: Wynn }
  deriving (Eq, Ord, Show)

instance ToNoun Vere where
  toNoun Vere{..} = toNoun ((vereName, vereRev), vereWynn)

instance FromNoun Vere where
  parseNoun n = named "Vere" $ do
    ((vereName, vereRev), vereWynn) <- parseNoun n
    pure $ Vere {..}

-- A list of names and their kelvin numbers, used in version negotiations.
newtype Wynn = Wynn { unWynn :: [(Term, Word)] }
  deriving newtype (Eq, Ord, Show, FromNoun, ToNoun)

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

newtype PEM = PEM { unPEM :: Wain }
  deriving newtype (Eq, Ord, ToNoun, FromNoun)

instance Show PEM where
  show _ = "\"PEM (secret)\""

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

-- Socket Configuration -------------------------------------------------------
data SocketConf = SocketConf
    { scfilePath :: FilePath
    }
  deriving (Eq, Ord, Show)

deriveNoun ''SocketConf

-- Desk and Mime ---------------------------------------------------------------

newtype Desk = Desk { unDesk :: Cord }
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun, IsString)

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

serializeToNoun :: Serialize a => a -> Noun
serializeToNoun = A . bytesAtom . encode

serializeParseNoun :: Serialize a => String -> Int -> Noun -> Parser a
serializeParseNoun desc len = named (pack desc) . \case
    A (atomBytes -> bs)
      -- Atoms lose leading 0s, but since lsb, these become trailing NULs
      | length bs <= len -> case decode $ bs <> replicate (len - length bs) 0 of
        Right aa -> pure aa
        Left msg -> fail msg
      | otherwise -> fail ("putative " <> desc <> " " <> show bs <> " too long")
    C{} -> fail ("unexpected cell in " <> desc)

newtype Patp a = Patp { unPatp :: a }
  deriving newtype (Eq, Ord, Enum, Real, Integral, Num, ToNoun, FromNoun)

-- Network Port
newtype Port = Port { unPort :: Word16 }
  deriving newtype (Eq, Ord, Show, Enum, Real, Integral, Num, ToNoun, FromNoun)

-- @if
newtype Ipv4 = Ipv4 { unIpv4 :: N.HostAddress }
  deriving newtype (Eq, Ord, Enum)

instance Serialize Ipv4 where
  get = (\a b c d -> Ipv4 $ N.tupleToHostAddress $ (d, c, b, a))
    <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
  put (Ipv4 (N.hostAddressToTuple -> (a, b, c, d))) = for_ [d, c, b, a] putWord8

instance ToNoun Ipv4 where
  toNoun = serializeToNoun

instance FromNoun Ipv4 where
  parseNoun = serializeParseNoun "Ipv4" 4

instance Show Ipv4 where
  show (Ipv4 (N.hostAddressToTuple -> (a, b, c, d))) =
    show a ++ "." ++
    show b ++ "." ++
    show c ++ "." ++
    show d

-- @is
-- should probably use hostAddress6ToTuple here, but no one uses it right now
newtype Ipv6 = Ipv6 { unIpv6 :: Word128 }
  deriving newtype (Eq, Ord, Show, Enum, Real, Integral, Num, ToNoun, FromNoun)

type Galaxy = Patp Word8

instance Integral a => Show (Patp a) where
  show = show . Ob.renderPatp . Ob.patp . fromIntegral . unPatp

data AmesAddress = AAIpv4 Ipv4 Port
  deriving (Eq, Ord, Show)

instance Serialize AmesAddress where
  get = AAIpv4 <$> get <*> (Port <$> getWord16le)
  put (AAIpv4 ip (Port port)) = put ip >> putWord16le port

instance FromNoun AmesAddress where
  parseNoun = serializeParseNoun "AmesAddress" 6

instance ToNoun AmesAddress where
  toNoun = serializeToNoun

type AmesDest = Each Galaxy AmesAddress


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
