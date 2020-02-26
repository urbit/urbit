
::  Twos exponent
=/  bex
  ~/  1  bex
  (S (S I (K (mul 2))) (K 1))
  :: |=  a
  :: %+  a  (mul 2)  1


=/  lsh
  ~/  2  lsh
  (S (K mul) bex)
  :: |=  (exponent num)
  :: (mul (bex exponent) num)

:*
  bex
  (bex 1024)
  lsh
  (lsh 8 1)
==
