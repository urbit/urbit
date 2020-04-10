module Urlicht.Simple where

import ClassyPrelude

import Bound
import Bound.Name
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)

type SimpleType = Simple
type B = Name Text ()

-- | The lowest-level unelaborated representation.
data Simple a
  = Var a
  -- types
  | Typ
  | Fun (Simple a) (Scope B Simple a)
  -- introduction forms
  | Lam (Scope B Simple a)
  -- elimination forms
  | App (Simple a) (Simple a)
  -- flow control
  | Let (Simple a) (Simple a) (Scope B Simple a)
  -- meta
  | Hol
  deriving (Functor, Foldable, Traversable)

deriveEq1   ''Simple
deriveOrd1  ''Simple
deriveRead1 ''Simple
deriveShow1 ''Simple
makeBound   ''Simple

deriving instance Eq a   => Eq   (Simple a)
deriving instance Ord a  => Ord  (Simple a)
deriving instance Read a => Read (Simple a)
deriving instance Show a => Show (Simple a)
