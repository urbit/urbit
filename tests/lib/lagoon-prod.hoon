::  /tests/lib/lagoon-prod: +prod seeds from the kind's one, not integer 1
::
::  +prod used to fold from the raw bits 1, which as %i754 is the smallest
::  denormal: a 2x2 of 2.0 multiplied out to the denormal 0x10, not 16.0.
::  Only %uint was right, and only %uint was tested.
::
/-  *lagoon
/+  *test
/+  *lagoon
^|
|%
+$  rrow  [bloq=@ zero=@ one=@ two=@ three=@ six=@]
++  reals
  ^-  (list rrow)
  :~  [4 .~~0 .~~1 .~~2 .~~3 .~~6]
      [5 .0 .1 .2 .3 .6]
      [6 .~0 .~1 .~2 .~3 .~6]
      [7 .~~~0 .~~~1 .~~~2 .~~~3 .~~~6]
  ==
++  shapes  ^-  (list (list @))  ~[~[1 1] ~[2 3] ~[3 3]]
++  hed  |=(a=ray ^-(@ -:(ravel:la a)))
::  prod over [one two three] = six, at every %i754 width
++  test-prod  ^-  tang
  %-  zing
  %+  turn  reals
  |=  r=rrow
  ^-  tang
  =/  a  (en-ray:la [`meta`[~[3] bloq.r %i754 ~] ~[one.r two.r three.r]])
  (expect-eq !>(`@`six.r) !>((hed (prod:la a))))
::  prod of an all-ones ray is one, at every shape (a denormal seed fails this)
++  test-prod-ones  ^-  tang
  %-  zing
  %+  turn  reals
  |=  r=rrow
  ^-  tang
  %-  zing
  %+  turn  shapes
  |=  shp=(list @)
  ^-  tang
  =/  m=meta  [shp bloq.r %i754 ~]
  (expect-eq !>(`@`one.r) !>((hed (prod:la (ones:la m)))))
::  prod over [zero one two] is zero (a denormal seed also gives zero here;
::  this pins the zero case rather than the seed)
++  test-prod-zero  ^-  tang
  %-  zing
  %+  turn  reals
  |=  r=rrow
  ^-  tang
  =/  a  (en-ray:la [`meta`[~[3] bloq.r %i754 ~] ~[zero.r one.r two.r]])
  (expect-eq !>(`@`zero.r) !>((hed (prod:la a))))
::  %uint: 3 * 3 * 3 = 27, and a 2x2x2 of 2 = 256
++  test-prod-uint  ^-  tang
  ;:  weld
    (expect-eq !>(`@`27) !>((hed (prod:la (fill:la [~[3] 5 %uint ~] 3)))))
    (expect-eq !>(`@`256) !>((hed (prod:la (fill:la [~[2 2 2] 5 %uint ~] 2)))))
  ==
--
