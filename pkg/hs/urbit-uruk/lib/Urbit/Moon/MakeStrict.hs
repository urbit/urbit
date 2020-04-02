{-# OPTIONS_GHC -Werror #-}

{- |
    # The Problem

    Uses `seq` to prevent overly-eager evaluation.

    Without this, `λx.fg` would compile to `K(fg)` which will cause
    the evaluation of `(fg)` at definition time, instead of waiting until
    `x` is passed.

    We can prevent this by transforming `λx.fg` into `λx.SKxfg`,
    which will delay the evaluation of `fg` until the right time.

    This transformation is especially important in recursive code, which
    will almost always contain an if expressions. If both branches of
    the if expression are always evaluated, then the loop will never
    terminate.

    # The Algorithm

    The goal of this algorithm is to make sure that no significant
    evaluation takes place before the most recent lambda binding has
    been made available.

      An example of insignificant evaluation, is applying `K` to `S`,
      which trivially becomes `KS`.

      Another example, is applying `3` to `ADD`, which trivial becomes
      `(ADD 3)`. This is because the `ADD` jet has arity two.

      In general, applying a value to a function of arity one is
      significant, and all other application is insignificant.

    The basic idea is to recurse through expressions, starting at the
    leaves, determine their arity, and delay any significant applications
    by transforming `(fg)` into `(SKxfg)`. This will prevent `f` from
    being applied to `g` until `x` is supplied.

    We treat the most recently bound variable as having arity 0, which is
    treated differently. Expressions of arity 0 do not need to be changed,
    since their evaluation already depends on the most-recently-bound
    variable. Additionally, in the body of a jet that takes `n`
    arguments. All `n` of those variables should have arity `0`, since
    they are all made available at the same time.

    The arity of combinators:

      *S    -> 3
      *K    -> 2
      *J    -> 2
      *Jⁿ   -> 2
      *Jⁿtb -> n
      *D    -> 1

    The arity of variables:

      *x    -> 0 (most recently bound, or all from jet arguments)
      *f    -> 1 (free variable, arity statically unknowable)

    The arity of lambdas:

      *(λv.B) -> (*B)+1

    The arity of applications:

      *(Keₙ)  -> n+1
      *(e₀eₙ) -> 0
      *(eₙe₀) -> 0
      *(eₙeₘ) -> n-1

    The transformation of applications:

      *(e₁eₙ) -> SKxee
      *(eₙeₙ) -> ee
-}

module Urbit.Moon.MakeStrict (makeStrict, makeStrict') where

import ClassyPrelude hiding (try)

import Bound
import Urbit.Uruk.Class
import Urbit.Uruk.Bracket

import Control.Arrow          ((>>>), (<<<))
import Numeric.Natural        (Natural)
import Numeric.Positive       (Positive)

import qualified Urbit.Uruk.Fast.Types  as F
import qualified Urbit.Uruk.Refr.Jetted as Ur


--------------------------------------------------------------------------------

{-
      *S      -> 3
      *K      -> 2
      *J      -> 2
      *Jⁿ     -> 2
      *Jⁿtb   -> n
      *D      -> 1

      *x      -> 0 (most recently bound, or all from jet arguments)
      *f      -> 1 (free variable, arity statically unknowable)

      *(λv.B) -> (*B)+1

      *(Keₙ)  -> n+1
      *(e₀eₙ) -> 0
      *(eₙe₀) -> 0
      *(eₙeₘ) -> n-1
-}

type ExpV a = Exp () (Var () a)

{-
    [eₙe₀] -> ee
    [e₁eₙ] -> SKxee
    [eₙeₙ] -> ee
-}
fixApp :: (ExpV a, ExpV a) -> (Int, ExpV a) -> (Int, ExpV a) -> ExpV a
fixApp (s, k) (_xArgs, x) (0, y)      = x :@ y
fixApp (s, k) (1, x)      (_yArgs, y) = s :@ k :@ Var (B ()) :@ x :@ y
fixApp (s, k) (_, x)      (_yArgs, y) = x :@ y

{-
    *(Keₙ)  -> n+1
    *(e₀eₙ) -> 0
    *(eₙe₀) -> 0
    *(eₙeₘ) -> n-1
-}
appArity :: Bool -> Int -> Int -> Int
appArity True  _     yArgs = yArgs+1
appArity _xIsK 0     _     = 0
appArity _xIsK _     0     = 0
appArity _xIsK xArgs _     = xArgs-1

{- |
    Returns the arity of an expression and transform it if necessary.

    `f x` should return `(x == K, arity x)`.
-}
recur :: (ExpV a, ExpV a, a -> (Bool, Int)) -> ExpV a -> ((Bool, Int), ExpV a)
recur (s,k,f) = \case
  Var v -> case v of
    B () -> ((False, 0), Var (B ()))
    F v  -> (f v, Var (F v))

  Lam () b ->
    let ((_, arity), bodExp) = recur (F <$> s, F <$> k, wrap f) $ fromScope b
    in ((False, arity+1), Lam () (toScope bodExp))

  x :@ y ->
    let
      ((xIsK, xArgs), xVal) = recur (s,k,f) x
      ((yIsK, yArgs), yVal) = recur (s,k,f) y
      resVal = fixApp (s,k) (xArgs, xVal) (yArgs, yVal)
      resArgs = appArity xIsK xArgs yArgs
      resIsK = False
    in
      ((resIsK, resArgs), resVal)
 where
  wrap :: (a -> (Bool, Int)) -> Var () a -> (Bool, Int)
  wrap f (B ()) = (False, 1)
  wrap f (F v)  = f v

makeStrict' :: forall p . (Eq p, Uruk p) => Exp () p -> Exp () p
makeStrict' = go
 where
  go = \case
    x   :@ y -> go x :@ go y
    Lam () b -> Lam () $ toScope $ snd . recur (s, k, r) $ fromScope b
    Var v    -> Var v
  s = Var (F uEss)
  k = Var (F uKay)
  r = \x -> (x == uKay, uArity x)

makeStrict :: (Uruk p, Eq a) => (p -> a) -> Exp () a -> Exp () a
makeStrict initF = top initF
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
    Var _          -> False
    Lam () b       -> go (F v) (fromScope b)
    x   :@ y       -> go v x || go v y
