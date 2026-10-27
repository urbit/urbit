/-  *lagoon
/+  *test
/+  *lagoon
::  /tests/lib/lagoon-change: +change (kind/width conversion) at every rank
::
::  +change used to round-trip through +de-ray/+en-ray, which only knew ranks
::  1 and 2: a rank-0 ray crashed on every path (S-23) and the %uint -> %uint
::  path crashed at rank 3 and up.  It now maps over the ravel and re-pins, so
::  rank is irrelevant; these tests pin that, plus the value semantics of each
::  wired path.
::
^|
|%
++  is-equal
  |=  [a=ray b=ray]  ^-  tang
  ?:  =(a b)  ~
  :~  [%palm [": " ~ ~ ~] [leaf+"expected" "{<`ray`a>}"]]
      [%palm [": " ~ ~ ~] [leaf+"actual  " "{<`ray`b>}"]]
  ==
::  a rank-0 ray: shape ~, prod(shape) = 1, one scalar under the 0x1 pin
++  scalar0
  |=  [b=@ k=kind x=@]  ^-  ray
  [[~ b k ~] (con (lsh [b 1] 1) x)]
::
++  test-change-rank0-uint-i754  ^-  tang
  %+  is-equal  (scalar0 5 %i754 .5)
  (change:la (scalar0 5 %uint 5) %i754 5)
++  test-change-rank0-uint-uint  ^-  tang
  %+  is-equal  (scalar0 6 %uint 5)
  (change:la (scalar0 5 %uint 5) %uint 6)
++  test-change-rank0-i754-i754  ^-  tang
  %+  is-equal  (scalar0 6 %i754 .~1.5)
  (change:la (scalar0 5 %i754 .1.5) %i754 6)
++  test-change-rank3-uint-uint  ^-  tang
  %+  is-equal  (fill:la [~[2 2 2] 6 %uint ~] 3)
  (change:la (fill:la [~[2 2 2] 5 %uint ~] 3) %uint 6)
++  test-change-rank3-uint-i754  ^-  tang
  %+  is-equal  (fill:la [~[2 2 2] 5 %i754 ~] .3)
  (change:la (fill:la [~[2 2 2] 5 %uint ~] 3) %i754 5)
::  distinct values: row-major order is preserved
++  test-change-uint-i754-values  ^-  tang
  %+  is-equal
    (en-ray:la [[~[2 3] 6 %i754 ~] ~[~[.~0 .~1 .~2] ~[.~3 .~4 .~5]]])
  (change:la (en-ray:la [[~[2 3] 6 %uint ~] ~[~[0 1 2] ~[3 4 5]]]) %i754 6)
++  test-change-i754-uint-values  ^-  tang
  %+  is-equal
    (en-ray:la [[~[2 3] 5 %uint ~] ~[~[0 1 2] ~[3 4 5]]])
  (change:la (en-ray:la [[~[2 3] 5 %i754 ~] ~[~[.0 .1 .2] ~[.3 .4 .5]]]) %uint 5)
::  %uint -> %uint narrowing truncates each element to the new width
++  test-change-uint-narrow  ^-  tang
  %+  is-equal  (fill:la [~[3] 3 %uint ~] 0x34)
  (change:la (fill:la [~[3] 5 %uint ~] 0x1234) %uint 3)
::  %i754 across widths is a float conversion, not a bit re-box
++  test-change-i754-widen  ^-  tang
  ;:  weld
    %+  is-equal  (fill:la [~[2 2] 6 %i754 ~] .~1.5)
    (change:la (fill:la [~[2 2] 5 %i754 ~] .1.5) %i754 6)
  ::
    %+  is-equal  (fill:la [~[3] 7 %i754 ~] .~~~-2.25)
    (change:la (fill:la [~[3] 4 %i754 ~] .~~-2.25) %i754 7)
  ==
++  test-change-i754-narrow  ^-  tang
  ;:  weld
    %+  is-equal  (fill:la [~[2] 5 %i754 ~] .0.75)
    (change:la (fill:la [~[2] 6 %i754 ~] .~0.75) %i754 5)
  ::
    %+  is-equal  (fill:la [~[2] 4 %i754 ~] .~~1.5)
    (change:la (fill:la [~[2] 6 %i754 ~] .~1.5) %i754 4)
  ==
::  an inexact narrowing rounds in the door's mode: bare +la is %z (truncate),
::  +lake %n is nearest.  The double 0.1 lies between the floats 0x3dcccccc
::  and 0x3dcccccd (= .0.1), nearer the latter.
++  test-change-i754-narrow-rounding  ^-  tang
  =/  a  (fill:la [~[2] 6 %i754 ~] .~0.1)
  ;:  weld
    %+  is-equal  (fill:la [~[2] 5 %i754 ~] 0x3dcc.cccc)
    (change:la a %i754 5)
  ::
    %+  is-equal  (fill:la [~[2] 5 %i754 ~] .0.1)
    (change:(lake %n) a %i754 5)
  ==
::  the same width is a bit-exact copy
++  test-change-i754-same  ^-  tang
  =/  a  (en-ray:la [[~[3] 5 %i754 ~] ~[.1 .-2.5 .1e-3]])
  (is-equal a (change:la a %i754 5))
::  unwired kinds refuse rather than returning garbage
++  test-change-unwired  ^-  tang
  ;:  weld
    (expect-fail |.((change:la (fill:la [~[2] 3 %int2 ~] 1) %i754 5)))
    (expect-fail |.((change:la (ones:la [~[2] 5 %uint ~]) %int2 5)))
    (expect-fail |.((change:la (ones:la [~[2] 5 %uint ~]) %i754 3)))
  ==
--
