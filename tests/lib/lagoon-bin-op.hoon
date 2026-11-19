::  /tests/lib/lagoon-bin-op: element-wise ops over long distinct operands
::
::  +bin-op walks both ravels in one pass (it used to +snag each index,
::  which is quadratic in the length); a long, distinct-valued operand
::  catches any misalignment that same-valued fills would hide.
::
/-  *lagoon
/+  *test
/+  *lagoon
^|
|%
++  test-add-long-uint  ^-  tang
  =/  a  (iota:la [~[300] 5 %uint ~])
  %+  expect-eq
    !>((turn (gulf 0 299) |=(i=@ (lsh 0 i))))
  !>((ravel:la (add:la a a)))
++  test-mul-long-uint  ^-  tang
  =/  a  (iota:la [~[300] 5 %uint ~])
  %+  expect-eq
    !>((turn (gulf 0 299) |=(i=@ (pow i 2))))
  !>((ravel:la (mul:la a a)))
::  %i754: i + i = 2*i for i in 0..199, exact
++  test-add-long-i754  ^-  tang
  =/  a  (change:la (iota:la [~[200] 6 %uint ~]) %i754 6)
  =/  b  (add:la a a)
  %+  expect-eq
    !>((ravel:la (change:la (en-ray:la [[~[200] 6 %uint ~] (turn (gulf 0 199) |=(i=@ (lsh 0 i)))]) %i754 6)))
  !>((ravel:la b))
--
