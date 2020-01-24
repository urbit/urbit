module Ur.Examples where

import Ur.Lang
import Ur.Simplify


-- Examples --------------------------------------------------------------------

infixl 5 %;
(%) = (:@)

i = S%K%K

two = inc%one
thr = inc%two



mul = J 2 "mul" % (S%(K%S)%K)
add = J 2 "add" % (S%(K%(S%(S%(K%S)%(S%(K%(S%(K%S)%K))))))%K)

uTrue  = K%(K%K)
uFalse = K%(K%(S%K))

{-
    none     = \n s   -> n
    some     = \x n s -> s x
    maybe    = \l r v -> v l r
-}
none = J 2 "none" %(K)
some = J 2 "some" %(S%(K%(S%(K%K)%(S%(S%K%K))))%K)
mayb = J 3 "maybe"%(S%(K%(S%(K%(S%(K%(S%(K%(S%S%(K%K)))%K))%S))%(S%(S%K%K))))%K)

twoMuld = toNat (mul % one % two)
ninMuld = toNat (mul % thr % thr)

twoAddd = toNat (add % one % one)
ninAddd = toNat (add % thr % (add % thr % thr))

{-
    what1 = fix (\loop x -> x) S
    what2 = fix (\loop   -> C id (\x → loop (L x)))

case x of (L lv → lv); (Rif x then x else loop true)
-}
what1 = F % (S%K) % S
what2 = (S % (K%(C%i))%(S%(S%(K%S)%K)%(K%L))) % (S % (K%(C%i))%(S%(S%(K%S)%K)%(K%L))) % D

what3 = F % (S % (K%(C%i))%(S%(S%(K%S)%K)%(K%L))) % (R % zer)

----- = F % (S%(K%(S%(K%(S%(S%i%i)))%K))%(S%i%(K%K))) % uFalse

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
