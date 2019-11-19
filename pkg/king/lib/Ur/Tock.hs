module Ur.Tock
    ( Dc(..), Pm(..)
    , S(..)
    , PM, epm
    , ST, edc
    ) where

import Ur.Common hiding (GT, L, R)


-- Concise Sum Types -----------------------------------------------------------

data S a b = L a | R b
  deriving (Eq, Ord)

instance (Show a, Show b) => Show (S a b) where
    show (L x) = "L" <> show x
    show (R x) = "R" <> show x


-- Types -----------------------------------------------------------------------

data Ex
    = Lam Ex Ex
    | Pie Ex Ex
    | Var At
    | App Ex Ex

    | Fix Ex
    | Eva Ex
    | Jet At Ex

    | Box Ex
    | Unbox Ex

    | Nat At
    | Inc Ex
    | Dec Ex Ex Ex
  deriving (Eq, Ord, Show)


{-
    # How are values serialized?

    atoms: using the rub algo.
    box: by indirection (sha256 hash).
    closures: using the `flat` algo.
        [cod/@ env/(list val)]

  - The type of a closure tells us nothing about it's environment.
    - What is a `val`

  add :: Nat -> Nat -> Nat
  add x y = x+y

  add = \@\@(plus 1 0) {}
    easy enough: empty environment, known argument types.

  add 3 = \@(plus 1 0) {3/@}
-}
