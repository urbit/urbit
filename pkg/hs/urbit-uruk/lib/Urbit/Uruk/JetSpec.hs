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

++  (pak n)    (E E K (n sksucc skzero))
++  (inc n)    (pak <i z (i (n i z))>)

++  0  (E E K skzero)
++  1  (E E K (sksucc skzero))
++  2  (E E K (sksucc (sksucc skzero)))

++  (let x y)  (y x)

++  zee
  |=  f
  %-  <x (f <v (x x v)>)>
  <x (f <v (x x v)>)>

++  (fix fun arg)
  %^    zee
      |=  $
      %-  (E E %fix)
      <f x (f ($ f) x)>
    fun
  arg

++  oldfix
  ~/  2  fix
  %-  zee
  |=  $
  %-  (E E %fix)
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

++  (lcon h t nod emp)  (nod (con h t))
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
  %-  (E E E %divlop)
  %-  fix
  |=  ($ divisor count remain)
  %+  (iff (lth remain divisor))
    <u count>
  <u (divisor (inc count) (fub remain divisor))>

++  (div dividend divisor)
  %+  (iff (zer divisor))
    <u (ded %divide-by-zero)>
  <u (divlop divisor 0 dividend)>

++  (rsh x n)  (div n (bex x))
++  (mod a b)  (fub a (mul b (div a b)))

++  (trace x y)  (y uni)

++  snag
  %-  (E E %snag)
  %-  fix
  |=  ($ idx l)
  %+  (cas l)
    |=  n
    %+  (iff (eql 0 idx))
      <u (car n)>
    <u ($ (fec idx) (cdr n))>
  <u (ded %snag-fail)>

++  weld
  %-  (E E %weld)
  %-  fix
  |=  ($ a b)
  %+  (cas a)
    <p (lcon (car p) ($ (cdr p) b))>
  <u b>

++  turn
  %-  (E E %turn)
  %-  fix
  |=  ($ b fun)
  %+  (cas b)
    <p (lcon (fun (car p)) ($ (cdr p) fun))>
  <u lnil>

++  gulf
  %-  (E E %gulf)
  %-  fix
  |=  ($ a b)
  %+  (gte b a)
    <p (lcon a ($ (inc a) b))>
  <u lnil>

++  metlop
  %-  (E E %metlop)
  %-  fix
  |=  ($ x count)
  %-  %-  trace  (con 1 (con x count))  |=  ig
  %+  (zer x)
    %-  %-  trace  (con 1 (con x count))  |=  ig
    <u count>
  <u ($ (rsh 1 x) (inc count))>

++  (met x)
  (metlop x 0)

++  (round-up to-round multiple)
  %-  %-  trace  (con 2 (con to-round multiple))  |=  ig
  %+  (zer multiple)
    %-  %-  trace  (con 2 (con to-round multiple))  |=  ig
    <u to-round>
  |=  u
  %+  (zer (mod to-round multiple))
    %-  %-  trace  (con 2 (con to-round multiple))  |=  ig
    <u to-round>
  |=  u
  %-  %-  trace  (con 2 (con to-round multiple))  |=  ig
  (fub (add to-round multiple) (mod to-round multiple))

++  (cat a b c)
  %-  %-  trace  (con a (con b c))  |=  ig
  (add (lsh (round-up (met b) a) c) b)

++  rap
  %-  (E E %rap)
  %-  fix
  |=  ($ a b)
  %+  (cas b)
    <l (cat a (car l) ($ a (cdr l)))>
  <u 0>
|]

-- TODO: There is something wrong with the definition of +rap or one of its
-- subcomponents, and it is a jet mismatch.
