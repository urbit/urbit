{-
    Implements the Uruk primitives.
-}

module Uruk.Prim
    ( P(..)
    , evalEager
    , evalLazy
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
simplify ∷ ∀v
         . Show (v P)
         ⇒ (Exp (v P) → Val P)
         → (Val P → Exp (v P))
         → P → [v P]
         → Exp (v P)
simplify force fromVal = curry go
  where
    go ∷ (P, [v P]) → Exp (v P)
    go ( S, [z,y,x] ) = A x :@ A z :@ (A y :@ A z)
    go ( K, [y,x]   ) = A x
    go ( D, [x]     ) = jam x
    go ( J, [b,n,a] ) = fast fromVal (norm a) (norm n) (norm b)
    go ( J, args    ) = slow args
    go ( _, _       ) = error "bad-simplify"
    norm = force . A
    jam = fromVal . evalEager . church . dump . valToExp . norm

evalEager ∷ Exp P → Val P
evalEager = forceEager simplify . expToPartial arity

evalLazy ∷ Exp P → Val P
evalLazy = forceLazy simplify . expToLazy arity

fast ∷ ∀v
     . (Val P → Exp (v P))
     → Val P → Val P → Val P
     → Exp (v P)
fast fromVal arity name body = fromVal (V J (arityNum-1) [body, name, arity])
  where
    arityNum = arity & \case
        V S _ [] → 1
        V K _ [] → 2
        V D _ [] → 3
        V J _ [] → 4
        e        → error ("bad jet arity: " <> show e)

slow ∷ Show (v P) => [v P] → Exp (v P)
slow = traceShowId . start . drop 2 . reverse . traceShowId
  where
    start []      = error "bad-slow"
    start (f:xs)  = go (A f) xs
    go acc []     = acc
    go acc (x:xs) = go (acc :@ A x) xs

--  Produces a jetted, church-encoded natural number.
church ∷ Nat → Exp P
church = (jNat :@) . go
  where
    go 0 = A S :@ A K
    go 1 = A S :@ A K :@ A K
    go n = inc :@ go (pred n)

    inc = A S :@ (A S :@ (A K :@ A S) :@ A K)

dump ∷ Exp P → Nat
dump =  snd . go
  where
    go ∷ Exp P → (Int, Nat)
    go = \case
        A S    → (2, 0)
        A K    → (2, 2)
        A D    → (2, 4)
        A J    → (2, 6)
        x :@ y → (rBits, rNum)
          where
            (xBits, xNum) = go x
            (yBits, yNum) = go y
            rBits = 1 + xBits + yBits
            rNum  = 1 .|. shiftL xNum 1
                      .|. shiftL yNum (1+xBits)

--  Jets a number with name `S` (0)
jNat = j2 :@ s

s,k,j,d,j1,j2,j3,j4 ∷ Exp P
s = A S
k = A K
j = A J
d = A D
j1 = j :@ s
j2 = j :@ k
j3 = j :@ d
j4 = j :@ j
