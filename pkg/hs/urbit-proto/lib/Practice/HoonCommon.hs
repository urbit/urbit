module Practice.HoonCommon where

import ClassyPrelude

import Numeric.Natural

type Atom = Natural

type Axis = Atom

type Aura = Term

-- | A @tas. Frankly, would prefer to rename this `name`.
type Term = Text

-- | Whether or not an atom should be taken to be of singleton type.
data Grit
  = Rock  -- ^ should be taken to be of singleton type
  | Sand  -- ^ should be taken to be of broader type
  deriving (Eq, Ord, Read, Show)

type Wing = [Limb]

data Limb
  = Axis Axis
  | Ally Term
  deriving (Eq, Ord, Read, Show)
