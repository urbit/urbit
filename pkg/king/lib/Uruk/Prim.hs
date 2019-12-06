{-
    Implements the Uruk primitives.
-}

module Uruk.Prim
    ( P(..)
    , eval
    , arity
    , simplify
    , toNat
    , dump
    ) where

import ClassyPrelude
import Data.Bits
import Uruk.Exp

import Data.Flat     (Flat)
import Data.Function ((&))
import GHC.Natural   (Natural)


-- Types -----------------------------------------------------------------------

type Nat = Natural

data P = S | K | J | D
  deriving (Eq, Ord, Show, Generic, NFData, Flat)


--------------------------------------------------------------------------------

arity ∷ P → Nat
arity = \case { S → 3; K → 2; D → 1; J → 3 }

{-
    This function will crash if combinators are simplified with the
    wrong number of arguments.
-}
simplify ∷ P → [V P] → X P
simplify = curry go
  where
    go ∷ (P, [V P]) → X P
    go ( S, [z,y,x] ) = C x :@ C z :@ (C y :@ C z)
    go ( K, [y,x]   ) = C x
    go ( D, [x]     ) = (toNat . dump . toExp) x
    go ( J, [a,n,b] ) = fast a n b
    go ( J, x:xs    ) = fire x xs
    go ( _, _       ) = error "bad-simplify"

eval ∷ E P → V P
eval = toVal simplify . prep arity

fast ∷ V P → V P → V P → X P
fast arity name body = C (V J arityNum [arity, name, body])
  where
    arityNum = arity & \case
        V S _ [] → 1
        V K _ [] → 2
        V D _ [] → 3
        V J _ [] → 4
        _        → error "bad jet arity"

fire ∷ V P → [V P] → X P
fire f []     = C f
fire x [f]    = C f :@ C x
fire l (x:xs) = fire x xs :@ C l

toNat ∷ Nat → X P
toNat 0 = C $ eval (C S :@ C K)
toNat 1 = C $ eval (C S :@ C K :@ C K)
toNat n = C inc :@ toNat (pred n)
  where
    inc = eval (C S :@ (C S :@ (C K :@ C S) :@ C K))

dump ∷ E P → Nat
dump =  snd . go
  where
    go ∷ E P → (Int, Nat)
    go = \case
        C S    → (2, 0)
        C K    → (2, 2)
        C D    → (2, 4)
        C J    → (2, 6)
        x :@ y → (rBits, rNum)
          where
            (xBits, xNum) = go x
            (yBits, yNum) = go y
            rBits = 1 + xBits + yBits
            rNum  = 1 .|. shiftL xNum 1
                      .|. shiftL yNum (1+xBits)
