{-
    Implements the Isin primitives.
-}

module Isin.Prim where
    -- ( P(..)
    -- , eval
    -- , arity
    -- , simplify
    -- , toNat
    -- , dump
    -- ) where

import ClassyPrelude
import Uruk.Exp
import Uruk.Prim (s, k, j1, j3, jNat)

import Data.Flat     (Flat)
-- ort Control.Arrow ((>>>))
import Data.Function ((&))
import GHC.Natural   (Natural)

import qualified Uruk.Prim as U


-- Types -----------------------------------------------------------------------

{-
type Nat = Natural

data P = S
       | K
       | J
       | D
       | N Nat
       | I
       | F
  deriving (Eq, Ord, Show, Generic, NFData, Flat)


--------------------------------------------------------------------------------

arity ∷ P → Nat
arity = \case
    S   → 3
    K   → 2
    D   → 1
    J   → 3
    N n → 2
    I   → 1
    F   → 1

{-
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
        e        → error ("bad jet arity: " <> show e)
-}

toUrukPrim ∷ P → Either U.P (V U.P)
toUrukPrim = \case
    S   → Left U.S
    K   → Left U.K
    J   → Left U.J
    D   → Left U.D
    N n → Right (U.evalEager (U.church n))
    I   → Right jSucc
    F   → Right jFold

toUruk ∷ V P → V U.P
toUruk (V p n xs) = toUrukPrim p & \case
    Left up            → V up n (toUruk <$> xs)
    Right (V U.J _ js) → V U.J n ((toUruk <$> xs) <> js)
    Right _            → error "internal error"

-- fromUruk ∷ V U.P → V P

{-
    eSucc n i z = f (n f x)
    eZero i z   = z
    eOne i z    = i z

    jSucc n     = jNat (f (n eSucc eZero))
    jFold n i z = n i z

    jSucc n     = jNat (f (n eSucc eZero))

    main = \n -> N (n succ 0)
-}
eSucc, eZero, eOne ∷ E U.P
eSucc = s:@(s:@(k:@s):@k)
eZero = s :@ k
eOne  = s :@ k :@ k

jFold, jSucc ∷ V U.P
jFold = U.evalEager (j3 :@ s :@ (s :@ k :@ k))
jSucc = U.evalEager (j1 :@ s :@ body)
  where
    body = s:@(k:@jNat):@(s:@(s:@(s:@k:@k):@(k:@eSucc)):@(k:@eZero))
-}
