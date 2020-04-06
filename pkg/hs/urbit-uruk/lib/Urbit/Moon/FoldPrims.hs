{-# OPTIONS_GHC -Werror #-}

module Urbit.Moon.FoldPrims (foldPrims) where

import ClassyPrelude hiding (try, Prim)

import Bound
import Bound.Scope (hoistScope)
import Urbit.Moon.Bracket

import Data.List (nub, iterate, (!!))

import Bound.Var        (unvar)
import Control.Arrow    ((<<<), (>>>))
import Data.Function    ((&))
import Numeric.Natural  (Natural)
import Numeric.Positive (Positive)
import Text.Show.Pretty (ppShow)

import qualified Urbit.Uruk.Fast.Types  as F
import qualified Urbit.Uruk.Refr.Jetted as Ur


--------------------------------------------------------------------------------

{-
  foldPrims turns any application of constant values into a single
  constant value.

  This has the post-condition that: for all applications `e₁e₂`,
  `e₁` and `e₂` are never both primitive

  This allows us to make use of the given `arity`
  function to get better arity information for more complicated constant
  expressions.
-}
foldPrims :: forall p b v . (p -> p -> p) -> Exp p b v -> Exp p b v
foldPrims app = go
 where
  go :: Exp p b a -> Exp p b a
  go = \case
    Var v          -> Var v
    Pri p          -> Pri p
    Pri p :@ Pri q -> Pri (p `app` q)
    x     :@ y     -> go x :@ go y
    Lam   bi b     -> Lam bi (hoistScope go b)
