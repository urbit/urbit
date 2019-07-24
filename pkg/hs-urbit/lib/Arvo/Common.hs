module Arvo.Common
  ( NounTree(..), NounMap, NounSet
  , Json, JsonNode(..)
  , Desk(..), Mime(..)
  , AtomIf, AtomIs, Lane(..), Port(..), Turf(..)
  , HttpServerConf(..), HttpEvent(..), PEM, Method, Header
  , Routed(..), Reorgd(..), organized, reorg, disorg
  ) where

import Urbit.Time
import UrbitPrelude hiding (Term)

import qualified Network.HTTP.Types.Method as H


-- Misc Types ------------------------------------------------------------------

type AtomIf = Atom -- @if (TODO: What does this mean?)
type AtomIs = Atom -- @is (TODO: What does this mean?)

-- Domain Name
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


-- Lanes -----------------------------------------------------------------------

-- Network Port
newtype Port = Port { unPort :: Word }
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

data Lane
    = If Wen Port AtomIf
    | Is Atom (Maybe Lane) AtomIs
    | Ix Wen Port AtomIf
  deriving (Eq, Ord, Show)

deriveNoun ''Lane


-- Path+Tagged Restructuring ---------------------------------------------------

data Routed = Routed (Cord, Cord, EvilPath) (Cord, Noun)

data Reorgd = Reorgd Cord Cord Cord EvilPath Noun

deriveNoun   ''Reorgd
deriveToNoun ''Routed

instance FromNoun Routed where
  parseNoun = named "Routed" . \case
      A _                         -> expected "got atom"
      C (A _)             _       -> expected "empty route"
      C _                 (A _)   -> expected "value not tagged"
      C (C _     (A _))   (C _ _) -> expected "route is too short"
      C (C f (C s p)) (C t v)     -> do
        f <- named "first-route"   $ parseNoun f
        s <- named "second-route"  $ parseNoun s
        p <- named "rest-of-route" $ parseNoun p
        t <- named "tag"           $ parseNoun t
        pure (Routed (f, s, p) (t, v))
    where
      expected got = fail ("expected route+tagged; " <> got)

organized :: Iso' Routed Reorgd
organized = iso to from
  where
    to   = \case Routed (b, r, p) (t, v) -> Reorgd b r t p v
    from = \case Reorgd b r t p v        -> Routed (b, r, p) (t, v)

reorg :: Routed -> Noun
reorg = toNoun . view organized

{-
    This code may crash, but only if the FromNoun/ToNoun instances for
    the effects are incorrect.
-}
disorg :: Noun -> Noun
disorg = toNoun . view (from organized) . fromNounCrash
  where
    fromNounCrash :: FromNoun a => Noun -> a
    fromNounCrash =
      fromNounErr >>> \case
        Left err -> error (show err)
        Right vl -> vl
