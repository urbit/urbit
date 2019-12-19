module Deppy.ToUntyped where

import ClassyPrelude

import Bound
import Bound.Scope (hoistScope)

import Deppy.Core
import qualified Untyped.Core as U

toUntyped :: Exp a -> U.Exp a
toUntyped = \case
  Var v         -> U.Var v
  Typ           -> U.Atm 777
  Fun{}         -> U.Atm 788
  Cel{}         -> U.Atm 799
  Wut{}         -> U.Atm 810
  Lam (Abs _ b) -> U.Lam (hoistScope toUntyped b)
  Cns e f       -> U.Cel (toUntyped e) (toUntyped f)
  Tag a         -> U.Atm a
  App e f       -> U.App (toUntyped e) (toUntyped f)
  Hed e         -> U.Axs 2 (toUntyped e)
  Tal e         -> U.Axs 3 (toUntyped e)
  Cas e cs      -> U.Let (toUntyped e) $ Scope $ foldr step U.Zap $ mapToList cs
    where
      step (tag, f) acc = U.Ift (U.Eql (U.Var (B ())) (U.Atm tag)) (go f) acc
      go = U.Var . F . toUntyped
  Let e b       -> U.Let (toUntyped e) (hoistScope toUntyped b)
  Rec (Abs _ b) -> U.Fix (hoistScope toUntyped b)
