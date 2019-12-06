module Ur.CombExp where

import ClassyPrelude

import Data.Flat   (Flat)
import GHC.Natural (Natural)


-- Generic Evaluator for Combinator Expressions --------------------------------

type Nat = Natural

infixl 5 :@;

data E c = C c | E c :@ E c
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData, Flat)

data V c = V c Nat [V c]
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData, Flat)

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

cPrep :: (c -> V c) -> E c -> X c
cPrep f = go
  where
    go (C x)    = C (f x)
    go (f :@ x) = go f :@ go x

cEval :: ∀c. (c -> [V c] -> X c) -> X c -> V c
cEval simp = go
  where
    goVal (V c 0 k) = go (simp c k)
    goVal v         = v

    go (C v)              = goVal v
    go (C (V c 0 k) :@ x) = go (simp c k :@ x)
    go (C (V c n k) :@ x) = goVal (V c (n-1) (go x : k))
    go (f :@ x :@ y)      = go (C (go (f :@ x)) :@ y)

cDump :: ∀c. V c -> E c
cDump (V p _ xs) = go xs
  where go []     = C p
        go [x]    = C p :@ cDump x
        go (x:xs) = go xs :@ cDump x
