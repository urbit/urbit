module Practice.HoonCommon where

import ClassyPrelude

import Numeric.Natural

type Atom = Natural

type Axis = Atom

-- | A @tas. Frankly, would prefer to rename this `name`.
type Term = Text

type Wing = [Limb]

data Limb
  = Axis Axis
  | Ally Term
  deriving (Eq, Ord, Read, Show)
