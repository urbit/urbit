module Urbit.Uruk.JetSpec (jetSpec) where

import ClassyPrelude
import Data.FileEmbed      (embedStringFile)
import Text.RawString.QQ
import Urbit.Uruk.Dash.Exp (SingJet(..))


--------------------------------------------------------------------------------

jetSpec :: Text
jetSpec = [r|
++  (seq x y)    y
++  (yet f x y)  (f x y)

++  (eye x)      x
++  (bee f g x)  (f (g x))
++  (sea f g x)  (f x g)

++  skzero  (S K)
++  sksucc  (S (S (K S) K))

++  (pak n)    (J J K (n sksucc skzero))
++  (inc n)    (pak <i z (i (n i z))>)

++  0  (J J K skzero)
++  1  (J J K (sksucc skzero))
++  2  (J J K (sksucc (sksucc skzero)))

++  zee
  |=  f
  %-  <x (f <v (x x v)>)>
  <x (f <v (x x v)>)>

++  (fix fun arg)
  %^    zee
      |=  $
      %-  (J J %fix)
      <f x (f ($ f) x)>
    fun
  arg

++  oldfix
  ~/  2  fix
  %-  zee
  |=  $
  %-  (J J %fix)
  |=  (f x)
  (f ($ f) x)

++  (ded e)  (fix eye e)

++  (uni x)      (ded x)
++  (con h t f)  (f h t)
++  (car p)      (p <h t h>)
++  (cdr p)      (p <h t t>)
++  (lef x l r)  (l x)
++  (rit x l r)  (r x)
++  (cas b l r)  (b l r)

++  (lcon h t nod emp)  (nod h t)
++  (lnil nod emp)      (emp uni)

++  (yes t f)    t
++  (nah t f)    f
++  (iff c t e)  (c t e uni)

++  (dec n)
  %+  n
    |=  x
    %+  (cas x)
      <y (rit 0)>
    <y (rit (inc y))>
  (lef uni)

++  (fec n)    (cas (dec n) (K 0) eye)

++  (add x y)  (pak <i z (x i (y i z))>)
++  (mul x y)  (pak <i z (x (y i) z)>)

++  (sub x y)  (y <z (cas z lef dec)> (rit x))
++  (fub x y)  (cas (sub x y) (K 0) eye)

++  (not x)    (x (K nah) (K yes) uni)
++  (xor x y)  (x (y (K nah) (K yes) uni) (y (K yes) (K nah) uni))

++  (zer n)    (n (K nah) yes)
++  (eql x y)  (cas (sub x y) (K nah) zer)
++  (lth x y)  (cas (sub x y) (K yes) (K nah))
++  (gte x y)  (lth y x)
++  (bex x)    (x (mul 2) 1)
++  (lsh x n)  (mul (bex x) n)

++  divlop
  %-  (J J J %divlop)
  %-  fix
  |=  ($ divisor count remain)
  %+  (iff (lth remain divisor))
    <u count>
  <u (divisor (inc count) (fub remain divisor))>

++  (div dividend divisor)
  %+  (iff (zer divisor))
    <u (ded %divide-by-zero)>
  <u (divlop divisor 0 dividend)>

++  (mod a b)  (fub a (mul b (div a b)))

++  (trace x y)  (y uni)
|]
