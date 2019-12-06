{-
    Generic representation and evaluation of combinator expressions.

    This code will work with any set of combinators, given an arity and
    an implementation for each.
-}

module Uruk.Exp
    ( E(..)
    , V(..)
    , X
    , prep
    , toVal
    , toExp
    ) where

import ClassyPrelude

import Data.Flat   (Flat)
import GHC.Natural (Natural)


-- Types -----------------------------------------------------------------------

type Nat = Natural

infixl 5 :@;

data E c = C !c | E c :@ E c
  deriving (Eq, Ord, Generic, NFData, Flat)

data V c = V !c !Nat [V c]
  deriving (Eq, Ord, Generic, NFData, Flat)

type X c = E (V c)


-- Instances -------------------------------------------------------------------

instance Show c => Show (V c) where
  show (V c _ []) = show c
  show (V c _ xs) = "(" <> show c <> (concat $ show <$> reverse xs) <> ")"

instance Show c => Show (E c) where
  show = \case
     C c    -> show c
     x :@ y -> "(" <> concat (show <$> appList x [y]) <> ")"
    where
      appList (f :@ x) acc = appList f (x:acc)
      appList e        acc = (e:acc)


-- Evaluation ------------------------------------------------------------------

prep :: (c -> Nat) -> E c -> X c
prep arity = go
  where
    go (C x)    = C (V x (arity x) [])
    go (f :@ x) = go f :@ go x

toVal :: ∀c. (c -> [V c] -> X c) -> X c -> V c
toVal simp = go
  where
    go (C v)              = goVal v
    go (C (V c 0 k) :@ x) = go (simp c k :@ x)
    go (C (V c n k) :@ x) = goVal (V c (n-1) (go x : k))
    go (f :@ x :@ y)      = go (C (go (f :@ x)) :@ y)

    goVal (V c 0 k) = go (simp c k)
    goVal v         = v

toExp :: V c -> E c
toExp (V p _ xs) = go xs
  where go []     = C p
        go [x]    = C p :@ toExp x
        go (x:xs) = go xs :@ toExp x
