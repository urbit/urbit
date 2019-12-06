{-
    Implements the Uruk primitives.
-}

module Uruk.Prim
    ( P(..)
    , eval
    , arity
    , simplify
    , church
    , dump
    , s, k, j, d, j1, j2, j3, j4, jNat
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
    go ( D, [x]     ) = (C . eval . church . dump . toExp) x
    go ( J, [b,n,a] ) = fast a n b
    go ( J, args    ) = fire args
    go ( _, _       ) = error "bad-simplify"

eval ∷ E P → V P
eval = toVal simplify . prep arity

fast ∷ V P → V P → V P → X P
fast arity name body = C (V J arityNum [body, name, arity])
  where
    arityNum = arity & \case
        V S _ [] → 1
        V K _ [] → 2
        V D _ [] → 3
        V J _ [] → 4
        e        → error ("bad jet arity: " <> show e)

fire ∷ [V P] → X P
fire = traceShowId . start . drop 2 . reverse . traceShowId
  where
    start []      = error "bad-fire"
    start (f:xs)  = go (C f) xs
    go acc []     = acc
    go acc (x:xs) = go (acc :@ C x) xs

--  Produces a jetted, church-encoded natural number.
church ∷ Nat → E P
church = (jNat :@) . go
  where
    go 0 = C S :@ C K
    go 1 = C S :@ C K :@ C K
    go n = inc :@ go (pred n)

    inc = C S :@ (C S :@ (C K :@ C S) :@ C K)

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

--  Jets a number with name `S` (0)
jNat = j2 :@ s

s,k,j,d,j1,j2,j3,j4 ∷ E P
s = C S
k = C K
j = C J
d = C D
j1 = j :@ s
j2 = j :@ k
j3 = j :@ d
j4 = j :@ j
