{-- OPTIONS_GHC -Wall -Werror #-}

{-
  ## What should be lambda lifted?

  Any nested functions, but not LET/IFF/CAS/etc (pattern matching).

  ```
  THIS INPUT
    ++  (foo x y)
      ?:  (zer x)
        x
      %-  (x y)
      <v (add x (add y v))>
  BECOMES
    ++  (foo-inner x y v)
      (x y (add x (add y v)))
    ++  (foo x y)
      ?:  (zer x)
        x
      %-  (x y)
      (foo-inner x y)
  WHICH IS ACTUALLY
    ++  (foo x y)
      ?:  (zer x)
        x
      %-  (x y)
      %.  y
      %.  x
      ~/  3  foo-inner
      |=  (x y v)
      (add x (add y v))
  ```

  ## How to lambda lift?

  - Any string of abstractions becomes a new jet.

  - The *Arity* is the length of the abstraction string plus the number
    of free variables in the body.

  - The *Body* does not change at all.

  - The *Name* is the jet name plus `-inner`, `-inner2`, etc.

  - The abstraction string itself, is replaced by a call to the new jet
    with all of the variables that were free in that jet.

  ## Edge cases

  ### Empty Fix

  We can't turn this into a jet because it takes no arguments and has
  no free variables. This is an artificial and unimportant edge-case,
  so we can simply not lambda lift it.

  ```
  ..($ ~)  ==>  ..($ ~)
  ..($ $)  ==>  ..($ $)
  ```

  ### Fix must be on the outside.

  ```
  ..  $
  |=  (x)
  ($ x y)
    ==>
  %.  y
  ~/  2  jet-loop
  ..  $
  |=  (y x)
  ($ x y)

  ###

--  TODO Don't start lifting until enter first string of binders.
--  TODO Even under the binders, free variables from the outer-most
--       scope do not get lifted.
--  TODO Find some solution to naming.
--  TODO What to do about user jet tags?
-}

module Urbit.Moon.LambdaLift (lambdaLift) where

import ClassyPrelude

import Bound.Scope                (abstract1)
import Bound.Scope                (Scope(Scope), fromScope, hoistScope, toScope)
import Bound.Var                  (Var(..), unvar)
import Control.Monad.State.Strict (State, execState, get, put)
import Data.Void
import Urbit.Moon.AST             (AST(..), Exp(..), bind, unbind)

import qualified Data.Foldable as Fold


-- Types -----------------------------------------------------------------------

hoistOrd
  :: (Functor f, Ord b, Ord a, Ord (g a))
  => (forall x . Ord x => f x -> g x)
  -> Scope b f a
  -> Scope b g a
hoistOrd t (Scope s) = Scope $ t $ fmap t <$> s

maxEta :: Ord a => (Int -> Exp a -> Exp a) -> Exp a -> Exp a
maxEta muck e =
  let fv = ordNub (Fold.toList e)
      nf = length fv
  in  foldl' app (muck nf (foldr abs e fv)) fv
 where
  abs x e = Lam (abstract1 x e)
  app e x = App e (Var x)

lambdaLift :: Ord a => Text -> Exp a -> Exp a
lambdaLift nam = go
 where
  go :: Ord a => Exp a -> Exp a
  go = \case
    Var a     -> Var a
    App a b   -> App (go a) (go b)
    Jet n t b -> Jet n t (go b)
    Sig       -> Sig
    Con x y   -> Con (go x) (go y)
    Let v b   -> Let (go v) (hoistOrd go b)
    Cas x l r -> Cas (go x) (hoistOrd go l) (hoistOrd go l)
    Iff c t e -> Iff (go c) (go t) (go e)
    Bol b     -> Bol b
    Str t     -> Str t
    Lit l     -> Lit l
    Fix b     -> doFix b
    Lam b     -> doLam b

  doLam :: Ord a => Scope () Exp a -> Exp a
  doLam bod = maxEta
    (\nf b -> jet (depth (Lam bod) + nf) (nam <> "-inner") b)
    (reach (Lam bod))

  -- TODO This is wrong
  --   We need to re-order these somehow.
  --   We need to re-order these somehow.
  doFix :: Ord a => Exp a -> Exp a
  doFix = \case
    Lam bod -> maxEta
      (\nf b -> jet (pred (depth $ Lam bod) + nf) (nam <> "-loop") (Fix b))
      (reach (Lam bod))
    o -> Fix (go o)

  jet :: Int -> Text -> Exp a -> Exp a
  jet 0 _ b = b
  jet n t b = Jet (fromIntegral n) t b

  depth :: Exp a -> Int
  depth = loop 0
   where
    loop :: Int -> Exp a -> Int
    loop n (Lam b) = loop (succ n) (fromScope b)
    loop n other   = n

  reach :: Ord a => Exp a -> Exp a
  reach (Lam b) = Lam (hoistOrd reach b)
  reach other   = go other

rec1 = AFix "$" (AApp (AVar "$") (AVar "FREE"))

ex1 = (AFix "$" (AApp (ALam "a" $ AVar "a") (ALam "a" $ AVar "$")))

ex2 =
  (AFix
    "$"
    (ALam
      "x"
      (ALam "y" (AVar "$" `AApp` AVar "x" `AApp` AVar "y" `AApp` AVar "FREE"))
    )
  )

foo ast = do
  print ast
  print (unbind $ lambdaLift "jet" $ bind ast)
