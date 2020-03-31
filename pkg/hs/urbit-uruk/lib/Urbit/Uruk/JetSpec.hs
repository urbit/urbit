module Urbit.Uruk.JetSpec (SingJet(..), jetSpec) where

import ClassyPrelude
import Data.FileEmbed (embedStringFile)
import Text.RawString.QQ


--------------------------------------------------------------------------------

data SingJet
  = EYE -- I
  | BEE -- B
  | SEA -- C
  | SEQ
  | YET
  | FIX
  | IFF
  | PAK
  | ZER
  | EQL
  | LTH
  | ADD
  | INC
  | DEC
  | FEC
  | MUL
  | DIV
  | SUB
  | BEX
  | LSH
  | DED
  | UNI
  | YES
  | NAH
  | LEF
  | RIT
  | CAS
  | CON
  | CAR
  | CDR
 deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)
 deriving anyclass NFData

jetSpec :: Text
jetSpec = [r|
++  (eye x)      x
++  (bee f g x)  (f (g x))
++  (sea f g x)  (f x g)

++  lamzero  <f x x>
++  lamsucc  <n f x (f (n f x))>

++  (pak n)  (J J K (n lamsucc lamzero))
++  (inc n)  (pak <i z (i (n i z))>)

++  0  (J J K lamzero)
++  1  (J J K (lamsucc lamzero))
++  2  (J J K (lamsucc (lamsucc lamzero)))

++  (seq x y)    y
++  (yet f x y)  (f x y)

++  zee
  |=  f
  %-  <x (f <v (yet x x v)>)>
  <x (f <v (yet x x v)>)>

++  fix
  ~/  2  fix
  %-  zee
  |=  $
  %-  (yet (J J %fix))
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

++  lessthan
  (J J %lsh (S (S (K S) (S (S (K S) (S (K (S (K cas))) sub)) (K (K (K yes))))) (K (K (K nah)))))

++  (zer n)    (n (K nah) yes)
++  (eql x y)  (cas (sub x y) (K nah) zer)
++  (lth x y)  (cas (sub x y) (K yes) (K nah))
++  (gte x y)  (lth y x)
++  (bex x)    (x (mul 2) 1)
++  (lsh x n)  (mul (bex x) n)

++  (div y x)  (x y)

++  (realdiv end sor)
  %^    fix
      |=  ($ n x)
      %^    iff
          (lth x sor)
        <x n>
      <x (seq x $ (inc n) (sub x sor))>
    0
  sor
|]
