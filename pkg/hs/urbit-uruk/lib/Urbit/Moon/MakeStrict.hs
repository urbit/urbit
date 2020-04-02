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
    variable.

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

    ## Better Output for Jets

    - Let's look at an example:

      - `λx.λy.λz.xyz`

    - Without this trasnformation, this compiles to:

      - `SKK`

    - But that doesn't preserve evaluation order.

      - `(λx.λy.λz.xyz) ded %too-early` evaluates to:

         `(λz. ded %too-early z)`

      - But `SKK ded %too-early` evaluates to:

        `(ded %too-early)`

    - The transformation above soves the problem, by instead producing:

      ```
      (S (S (K S)
         (S (K (S (K S)))
            (S (K (S S (K K))) (S (K (S (K K) S)) (S (K (S (S K))) K)))))
      (K (S K)))
      ```

    - However, this is large. To solve this problem, we can take advantage
      of the jet system. Let's look at the same example, jetted:

       ```
       J J J K (λx.λy.λz.xyz)
       ```

    - Because the jet system will delay evaluation until all three
      arguments have been passed, the transformation is not necessary for
      those arguments.

    - So, we can recover our nice output:

      ```
      J J J K (S K K)
      ```

-}

module Urbit.Moon.MakeStrict (makeStrict, makeJetStrict) where

import ClassyPrelude hiding (try)

import Bound
import Urbit.Uruk.Class
import Urbit.Uruk.Bracket

import Control.Arrow          ((>>>), (<<<))
import Numeric.Natural        (Natural)
import Numeric.Positive       (Positive)

import qualified Urbit.Uruk.Fast.Types  as F
import qualified Urbit.Uruk.Refr.Jetted as Ur


-- Types -----------------------------------------------------------------------

type ExpV a = Exp () (Var () a)


-- Utils -----------------------------------------------------------------------

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

wrap :: (a -> (Bool, Int)) -> Var () a -> (Bool, Int)
wrap f = \case
  B () -> (False, 1)
  F v  -> f v

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


-- Entry-Point for Normal Functions --------------------------------------------

makeStrict :: (Eq p, Uruk p) => Exp () p -> Exp () p
makeStrict = go
 where
  go = \case
    x   :@ y -> go x :@ go y
    Lam () b -> Lam () $ toScope $ snd . recur (s, k, r) $ fromScope b
    Var v    -> Var v
  s = Var (F uEss)
  k = Var (F uKay)
  r = \x -> (x == uKay, uArity x)


-- Optimized Entry-Point for Jetted Functions ----------------------------------

{-
    TODO This is very complicated code to do something simple. Clean up!

    This expects it's input to be of the form `λx.λy.[...]b`. `n`
    bindings folloed by an expressions.

    It's the same as `makeStrict` except that it treats the first `n`
    bindings as having arity `0`.
-}
makeJetStrict :: (Eq p, Uruk p) => Int -> Exp () p -> Exp () p
makeJetStrict n topExp = top n topExp
 where
  top 0 e = makeStrict e
  top n (Var v) = makeStrict (Var v)
  top n (x :@ y) = makeStrict (x :@ y)
  top n (Lam () b) = Lam () $ toScope $ go initTup (n-1) $ fromScope b

  initTup = (,,,) (Var (F uEss))
                  (Var (F uKay))
                  (\x -> (x == uKay, uArity x))
                  (\x -> (x == uKay, uArity x))

  go :: (ExpV a, ExpV a, a -> (Bool, Int), a -> (Bool, Int)) -> Int -> ExpV a -> ExpV a
  go (s,k,f,j) 0 b          = snd $ jetRecur (s,k,f,j) b
  go (s,k,f,j) n b@(Var _)  = snd $ recur (s,k,f) b
  go (s,k,f,j) n b@(_ :@ _) = snd $ recur (s,k,f) b
  go (s, k, f, j) n (Lam () b) =
    Lam ()
      $ toScope
      $ go (F <$> s, F <$> k, wrap f, wrapJet j) (n - 1)
      $ fromScope b

  wrapJet :: (a -> (Bool, Int)) -> Var () a -> (Bool, Int)
  wrapJet f = \case
    B () -> (False, 0)
    F v  -> f v

{- |
    Returns the arity of an expression and transform it if necessary.

    `f x` should return `(x == K, arity x)`.
-}
jetRecur
  :: (ExpV a, ExpV a, a -> (Bool, Int), a -> (Bool, Int))
  -> ExpV a
  -> ((Bool, Int), ExpV a)
jetRecur (s,k,f,j) = \case
  Var v -> case v of
    B () -> ((False, 0), Var (B ()))
    F v  -> (j v, Var (F v))

  Lam () b ->
    let ((_, arity), bodExp) = recur (F <$> s, F <$> k, wrap f) $ fromScope b
    in ((False, arity+1), Lam () (toScope bodExp))

  x :@ y ->
    let
      ((xIsK, xArgs), xVal) = jetRecur (s,k,f,j) x
      ((yIsK, yArgs), yVal) = jetRecur (s,k,f,j) y
      resVal = fixApp (s,k) (xArgs, xVal) (yArgs, yVal)
      resArgs = appArity xIsK xArgs yArgs
      resIsK = False
    in
      ((resIsK, resArgs), resVal)
