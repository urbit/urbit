{-
    Generic representation and evaluation of combinator expressions.

    This code will work with any set of combinators, given an arity and
    an implementation for each.
-}

module Uruk.Exp
    ( Exp(..)
    , Val(..)
    , Partial
    , Whnf(..)
    , Lazy(..)
    , expToPartial
    , expToLazy
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

{-
    A fully evaluated value.

    This is a "closure":
      - A combinator
      - The number of arguments that can be given without reduction.
      - A list of already-applied arguments.
-}
data Val a = V !a !Nat ![Val a]
  deriving (Eq, Ord, Generic, NFData, Flat, Functor, Foldable, Traversable)

{-
    A partially-evaluated value: An expression with fully-evaluated
    values at the leaves.
-}
type Partial a = Exp (Val a)

{-
    A lazy value. Either:

    - It's already fully evaluated.
    - It's an expression with lazy values at its leaves.
    - Or a `Val` with lazy values in the closure.

    The interesting thing about this, is that forcing it will do no work
    for sub-trees that have already been evaluated.
-}
data Lazy a
    = Norm !(Val a)
    | Lazy (Exp (Lazy a))
    | Weak !(Whnf a)
  deriving (Eq, Ord, Generic, NFData, Flat, Functor, Foldable, Traversable)
  deriving Show

{-
    A lazy value that has been evaluated just enough that the combinator
    is known.
-}
data Whnf a = Whnf !a !Nat [Lazy a]
  deriving (Eq, Ord, Generic, NFData, Flat, Functor, Foldable, Traversable)
  deriving Show


-- Instances -------------------------------------------------------------------

instance Show c => Show (Val c) where
  show (V c _ []) = show c
  show (V c _ xs) = "(" <> show c <> (concat $ show <$> reverse xs) <> ")"

instance Show a => Show (Exp a) where
  show = \case
     A x    → show x
     x :@ y → "(" <> concat (show <$> appList x [y]) <> ")"
    where
      appList (f :@ x) acc = appList f (x:acc)
      appList e        acc = (e:acc)


-- Evaluation ------------------------------------------------------------------

expToPartial :: (a → Nat) → Exp a → Partial a
expToPartial arity = go
  where
    go (A x)    = A (V x (arity x - 1) [])
    go (f :@ x) = go f :@ go x

expToLazy :: ∀a. (a → Nat) → Exp a → Lazy a
expToLazy arity = go
  where
    go :: Exp a → Lazy a
    go (A x)    = Norm (V x (arity x - 1) [])
    go (f :@ x) = Lazy (A (go f) :@ A (go x))

valToExp :: Val a → Exp a
valToExp (V p _ xs) = go xs
  where go []     = A p
        go [x]    = A p :@ valToExp x
        go (x:xs) = go xs :@ valToExp x

-- simplify ∷ ∀v
         -- . Show (v P)
         -- ⇒ (v P → Val P, Val P → v P)
         -- → P → [v P]
         -- → Exp (v P)

forceEager :: ( (Exp (Val a) → Val a)
              → (Val a → Exp (Val a))
              → a → [Val a] → Partial a
              )
           → Partial a
           → Val a
forceEager simp' = go
  where
    simp = simp' go A
    go (A v)              = v
    go (A (V c 0 k) :@ x) = go (simp c (go x : k))
    go (A (V c n k) :@ x) = V c (n-1) (go x : k)
    go (f :@ x :@ y)      = go (A (go (f :@ x)) :@ y)

--  Lazy evaluation
whnf :: ∀a. (a → [Lazy a] → Whnf a) → Lazy a → Whnf a
whnf simp = go
  where
    ev (A x)    = go x
    ev (x :@ y) = ev x & \case Whnf a 0 k → simp a (Lazy y : k)
                               Whnf a n k → Whnf a (n-1) (Lazy y : k)

    go (Norm (V c n k)) = Whnf c n (Norm <$> k)
    go (Weak x)         = x
    go (Lazy x)         = ev x

forceLazy ∷ ∀a
          . ( (Exp (Lazy a) → Val a)
            → (Val a → Exp (Lazy a))
            → a → [Lazy a] → Exp (Lazy a)
            )
          → Lazy a
          → Val a
forceLazy simp' = go
  where
    simp ∷ a → [Lazy a] → Whnf a
    simp x y = whnf simp $ Lazy $ simp' (go . Lazy) (A . Norm) x y

    go ∷ Lazy a → Val a
    go v = V c n (go <$> k)
      where Whnf c n k = whnf simp v
