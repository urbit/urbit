{-
    Generic representation and evaluation of combinator expressions.

    This code will work with any set of combinators, given an arity and
    an implementation for each.
-}

module Uruk.Exp
    ( Exp(..)
    , Val(..)
    , Thunk
    , Whnf(..)
    , Partial(..)
    , expToThunk
    , forceEager
    , forceLazy
    , whnf
    , valToExp
    ) where

import ClassyPrelude

import Data.Flat     (Flat)
import GHC.Natural   (Natural)
import Data.Function ((&))


-- Types -----------------------------------------------------------------------

type Nat = Natural

infixl 5 :@;

data Exp a = A !a | Exp a :@ Exp a
  deriving (Eq, Ord, Generic, NFData, Flat, Functor, Foldable, Traversable)

data Val a = V !a !Nat ![Val a]
  deriving (Eq, Ord, Generic, NFData, Flat, Functor, Foldable, Traversable)

type Thunk a = Exp (Val a)

data Partial a
    = Norm !(Val a)
    | Lazy (Exp (Partial a))
    | Weak !(Whnf a)
  deriving (Eq, Ord, Generic, NFData, Flat, Functor, Foldable, Traversable)

data Whnf a = Whnf !a !Nat [Partial a]
  deriving (Eq, Ord, Generic, NFData, Flat, Functor, Foldable, Traversable)


-- Instances -------------------------------------------------------------------

instance Show c => Show (Val c) where
  show (V c _ []) = show c
  show (V c _ xs) = "(" <> show c <> (concat $ show <$> reverse xs) <> ")"

instance Show a => Show (Exp a) where
  show = \case
     A x    -> show x
     x :@ y -> "(" <> concat (show <$> appList x [y]) <> ")"
    where
      appList (f :@ x) acc = appList f (x:acc)
      appList e        acc = (e:acc)


-- Evaluation ------------------------------------------------------------------

expToThunk :: (a -> Nat) -> Exp a -> Thunk a
expToThunk arity = go
  where
    go (A x)    = A (V x (arity x) [])
    go (f :@ x) = go f :@ go x

valToExp :: Val a -> Exp a
valToExp (V p _ xs) = go xs
  where go []     = A p
        go [x]    = A p :@ valToExp x
        go (x:xs) = go xs :@ valToExp x

forceEager :: (a -> [Val a] -> Thunk a) -> Thunk a -> Val a
forceEager simp = go
  where
    go (A v)              = goVal v
    go (A (V c 0 k) :@ x) = go (simp c k :@ x)
    go (A (V c n k) :@ x) = goVal (V c (n-1) (go x : k))
    go (f :@ x :@ y)      = go (A (go (f :@ x)) :@ y)

    goVal (V c 0 k) = go (simp c k)
    goVal v         = v

--  Lazy evaluation
whnf :: ∀a. (a -> [Partial a] -> Whnf a) -> Partial a -> Whnf a
whnf simp = go
  where
    ev (A x)    = go x
    ev (x :@ y) = ev x & \case Whnf a 1 k -> simp a (Lazy y : k)
                               Whnf a n k -> Whnf a (n-1) (Lazy y : k)

    go (Norm (V c n k)) = Whnf c n (Norm <$> k)
    go (Weak x)         = x
    go (Lazy x)         = ev x

forceLazy :: ∀a. (a -> [Partial a] -> Whnf a) -> Partial a -> Val a
forceLazy simp = go
  where
    go :: Partial a -> Val a
    go v = V c n (go <$> k)
      where Whnf c n k = whnf simp v
