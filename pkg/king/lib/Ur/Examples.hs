module Ur.Examples where

import Ur.Common hiding (flat, A, succ, some)
import Data.Flat (Flat)
import GHC.Natural
import Ur.Lang
import Ur.Simplify


-- Examples --------------------------------------------------------------------

infixl 5 %;
(%) = (:@)

i = S%K%K

two = inc%one
thr = inc%two

mul = jet 2 $ S%(K%S)%K
add = jet 2 $ S%(K%(S%(S%(K%S)%(S%(K%(S%(K%S)%K))))))%K

uTrue  = K%(K%K)
uFalse = K%(K%(S%K))

{-
    none     = \n s   -> n
    some     = \x n s -> s x
    maybe    = \l r v -> v l r
-}
none = jet 2 $ K
some = jet 2 $ S%(K%(S%(K%K)%(S%(S%K%K))))%K
mayb = jet 3 $ S%(K%(S%(K%(S%(K%(S%(K%(S%S%(K%K)))%K))%S))%(S%(S%K%K))))%K

twoMuld = toNat (mul % one % two)
ninMuld = toNat (mul % thr % thr)

twoAddd = toNat (add % one % one)
ninAddd = toNat (add % thr % (add % thr % thr))

{-
    what1 = fix (\loop x -> x) S
    what2 = fix (\loop x -> if x then x else loop true)
-}
what1 = F % (S%K) % S
what2 = F % (simp (S%(K%(S%(K%(S%(S%i%i)))%K))%(S%i%(K%K)))) % uTrue

{-

    dec x = fold x (maybe (some 0) (some . inc)) none

    eqNat x y =
      case (dec x, dec y) of
        (Nothing, Nothing) → 1
        (Just _,  Nothing) → 0
        (Nothing, Just _ ) → 0
        (Just x, Just y  ) → eqNat x y

    eqNat x y = maybe xZero xDecr x
      where
        xZero = maybe 1 (const 0) y
        xDecr xd = maybe 0 (\yd → eqNat xd yd) y

    eqNat x = maybe (maybe 1 (const 0) x) (\yd → maybe 0 (\xd → eqNat xd yd) x)
-}
-- eqNat ∷ E
-- eqNat = jet 2
-- fix
