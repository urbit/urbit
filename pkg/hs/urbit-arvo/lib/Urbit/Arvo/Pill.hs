module Urbit.Arvo.Pill
  ( Nock(..)
  , Pill(..)
  )
where

import ClassyPrelude
import Urbit.Arvo.Event
import Urbit.Noun

-- Avoid touching Nock values. -------------------------------------------------

{-|
    Nock values are raw nouns with tons of duplicated structure, so
    printing or comparing them is insane.
-}
newtype Nock = Nock Noun
  deriving newtype (FromNoun, ToNoun)

instance Eq Nock where
  (==) (Nock x) (Nock y) = jamBS x == jamBS y

instance Show Nock where
  show _ = "Nock"


-- Pill ------------------------------------------------------------------------

data Pill = Pill
  { pBootFormulas   :: [Nock]
  , pKernelOvums    :: [Ev]
  , pUserspaceOvums :: [Ev]
  }
 deriving (Eq, Show)

deriveNoun ''Pill
