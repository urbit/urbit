module Vere.Ames where

import ClassyPrelude
import Data.IP
import Data.Void
import Noun

import qualified Urbit.Time as Time

type Port = Word

newtype Turf = MkTurf { unTurf :: [Cord] }
  deriving newtype (Eq, Ord, Show)

data Lane
  = Ip4f Time.Wen Port IPv4      -- [%if @da @ud @if]
  | Ip6  Void (Maybe Lane) Void  -- [%is @ud (unit lane) @is]
  | Ip4x Time.Wen Port IPv4      -- [%ix @da @ud @if]
  deriving (Eq, Ord, Show)

-- todo: manual instance needed?
deriveNoun ''IPv4
deriveNoun ''Turf
deriveNoun ''Lane

toIpv4 :: Lane -> IPv4
toIpv4 = \case
  Ip4f _ _ i        -> i
  Ip6  _ (Just l) _ -> toIpv4 l
  Ip6  _ _ _        -> error "IPv6 doesn't exist in practice"
  Ip4x _ _ i        -> i
