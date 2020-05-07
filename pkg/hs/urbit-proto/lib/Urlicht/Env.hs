module Urlicht.Env where

import ClassyPrelude

import Bound

import Urlicht.Core

-- | Records information about a bound variable which is presently in scope
data Binder a
  = Fun' (Type a)            -- ^ a pi type abstraction, with argument type
  | Lam' (Type a)            -- ^ a function abstraction, with argument type
  | Let' (Type a) (Value a)  -- ^ a let, with both type and expr of rhs
  deriving (Functor, Foldable, Traversable)

binderTy :: Binder a -> Type a
binderTy = \case
  Fun' t   -> t
  Lam' t   -> t
  Let' t _ -> t

-- | Records information about all the bound variables in scope. We think of
-- the innermost var as being "on the right" because this is the convention
-- used with the "Gammas" in type theory texts.
data Env a where
  Nil :: Env a
  Snoc :: Env a -> Binder a -> Env (Var () a)

emptyEnv :: Env a
emptyEnv = Nil

scry :: Env a -> a -> Maybe (Binder a)
scry Nil _ = Nothing
-- TODO can we improve on perf here?
-- maybe Env a = [(a, Binder a)] was better because it memoized the cost of
-- F-ing so you paid it only on the first read.
scry (Snoc _ b) (B ()) = Just (F <$> b)
scry (Snoc e _) (F v) = fmap F <$> scry e v

-- | Returns a list of all the variables bound in this scope whose values are
-- unknown (i.e., non-let-bound vars), innermost first
unknowns :: Env a -> [a]
unknowns = \case
  Nil -> []
  Snoc e (Fun'{}) -> B () : (F <$> unknowns e)
  Snoc e (Lam'{}) -> B () : (F <$> unknowns e)
  Snoc e (Let'{}) -> F <$> unknowns e

bind :: Env a -> Binder a -> Env (Var () a)
bind = Snoc
