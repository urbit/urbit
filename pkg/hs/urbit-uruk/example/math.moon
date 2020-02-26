
::  Twos exponent
=/  bex
  ~/  1  bex
  |=  a
  (a (mul 2) 1)

=/  lsh
  ~/  2  lsh
  (S (K mul) bex)
  ::  |=  (exponent num)
  ::  (mul (bex exponent) num)

:*
  :-  bex  (bex 128)
  :-  lsh  (lsh 8 1)
==
