module Urlicht.Simple where

import ClassyPrelude

import Bound
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)

import Urlicht.Meta

type SimpleType = Simple

-- | The lowest-level unelaborated representation.
data Simple a
  = Var a
  | Met Meta
  -- types
  | Typ
  | Fun (Simple a) (Scope () Simple a)
  -- introduction forms
  | Lam (Scope () Simple a)
  -- elimination forms
  | App (Simple a) (Simple a)
  -- flow control
  | Let (Simple a) (Scope () Simple a)
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
