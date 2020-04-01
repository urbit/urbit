{-- OPTIONS_GHC -Werror #-}

{- |
    Uses `seq` to prevent overly-eager evaluation.

    Without this, `\x -> exensive 9` would compile to `K (expensive
    9)` which normalizes to `K result`. We want to delay the evaluation
    until the argument is supplied, so we instead produce:

        \x → (seq x expensive) 9

    This transformation is especially important in recursive code,
    which will not terminate unless the recursion is delayed.

    TODO Optimize using function arity.
        `(kk)` doesn't need to become `Q0kk` since the head (`k`)
        is undersaturated.
-}

module Urbit.Moon.MakeStrict (makeStrict) where

import ClassyPrelude hiding (try)

import Bound
import Urbit.Uruk.Class
import Urbit.Uruk.Bracket

import Control.Arrow          ((>>>))
import Numeric.Natural        (Natural)
import Numeric.Positive       (Positive)

import qualified Urbit.Uruk.Fast.Types  as F
import qualified Urbit.Uruk.Refr.Jetted as Ur


--------------------------------------------------------------------------------

makeStrict :: (Uruk p, Eq a, Show a) => (p -> a) -> Exp () a -> Exp () a
makeStrict initF = traceShowId . top initF . traceShowId
 where
  top :: (Uruk p, Eq a) => (p -> a) -> Exp () a -> Exp () a
  top f = \case
    Lam () x -> Lam () (toScope $ go f $ fromScope x)
    Var v    -> Var v
    x :@ y   -> top f x :@ top f y

  go :: (Uruk p, Eq a) => (p -> a) -> Exp () (Var () a) -> Exp () (Var () a)
  go f = \case
    Lam () x -> Lam () (toScope $ top (F . F . f) $ fromScope x)
    Var v    -> Var v
    x :@ y   -> case (go f x, go f y) of
      (fv, xv) | hasDep fv || hasDep xv -> fv :@ xv
      (fv, xv)                          -> addDep f fv :@ xv

addDep :: Uruk p => (p -> a) -> Exp () (Var () a) -> Exp () (Var () a)
addDep prim = go
 where
  go = \case
    f :@ x -> go f :@ x
    exp    -> Var (F $ prim uSeq) :@ Var (B ()) :@ exp

hasDep :: Eq a => Exp () (Var () a) -> Bool
hasDep = go (B ())
 where
  go :: Eq a => Var () a -> Exp () (Var () a) -> Bool
  go v = \case
    Var x | x == v -> True
    Var x          -> False
    Lam () b       -> go (F v) (fromScope b)
    x   :@ y       -> go v x || go v y
