module Deppy.ToUntyped where

import ClassyPrelude

import Bound
import Bound.Name
import Bound.Scope
import Control.Monad.Morph (hoist)

import Deppy.Core
import qualified Untyped.Core as U

toUntyped :: Exp a -> U.Exp a
toUntyped = \case
  Var v         -> U.Var v
  Typ           -> U.Atm 777
  Fun{}         -> U.Atm 788
  Cel{}         -> U.Atm 799
  Atm{}         -> U.Atm 810
  Lam (Abs _ b) -> U.Lam (hoist toUntyped $ forget b)
  Cns e f _     -> U.Cel (toUntyped e) (toUntyped f)
  Nat a         -> U.Atm a
  App e f       -> U.App (toUntyped e) (toUntyped f)
  Hed e         -> U.Axs 2 (toUntyped e)
  Tal e         -> U.Axs 3 (toUntyped e)
  Cas e cs      -> U.Let (toUntyped e) $ Scope $ foldr step U.Zap $ mapToList cs
    where
      step (tag, f) acc = U.Ift (U.Eql (U.Var (B ())) (U.Atm tag)) (go f) acc
      go = U.Var . F . toUntyped
  Mat e bs      -> toUntyped $ Cas (Hed e) (instantiate1 (Tal e) <$> bs)
  Let e b       -> U.Let (toUntyped e) (hoist toUntyped $ forget b)
  Rec (Abs _ b) -> U.Fix (hoist toUntyped $ forget b)

forget = mapBound \(Name n b) -> b
