/+  *test
|%
::
::  Test addition (+)
::
++  test-add
  ;:  weld
    ::  Checks standard addition
    ::
    %+  expect-eq
      !>  2
      !>  (add 1 1)
    ::  Checks identity property (0 + n = n)
    ::
    %+  expect-eq
      !>  5
      !>  (add 0 5)
  ==
::
::  Test decrement (-1)
::
++  test-dec
  ;:  weld
    ::  Checks underflow error at 0
    ::
    %-  expect-fail
      |.  (dec 0)
    ::  Checks standard decrement
    ::
    %+  expect-eq
      !>  0
      !>  (dec 1)
  ==
::
::  Test division (/)
::
++  test-div
  ;:  weld
    ::  Checks division by zero fails
    ::
    %-  expect-fail
      |.  (div 1 0)
    ::  Checks division without remainder
    ::
    %+  expect-eq
      !>  3
      !>  (div 6 2)
    ::  Checks division with remainder (rounding down)
    ::
    %+  expect-eq
      !>  3
      !>  (div 7 2)
  ==
::
::  Test division with remainder (dvr)
::
++  test-dvr
  ;:  weld
    ::  Checks division by zero fails
    ::
    %-  expect-fail
      |.  (dvr 5 0)
    ::  Checks case where dividend is smaller than divisor
    ::
    %+  expect-eq
      !>  [0 3]
      !>  (dvr 3 5)
    ::  Checks exact division (5 / 5)
    ::
    %+  expect-eq
      !>  [1 0]
      !>  (dvr 5 5)
    ::  Checks division with remainder (10 / 3 -> [3,1])
    ::
    %+  expect-eq
      !>  [3 1]
      !>  (dvr 10 3)
  ==
::
::  Test greater than or equal (>=)
::
++  test-gte
  ;:  weld
    ::  Checks equality (3 >= 3)
    ::
    %-  expect
      !>  (gte 3 3)
    ::  Checks greater than (5 >= 3)
    ::
    %-  expect
      !>  (gte 5 3)
    ::  Checks less than (3 >= 5) is false
    ::
    %-  expect
      !>  !(gte 3 5)
  ==
::
::  Test greater than (>)
::
++  test-gth
  ;:  weld
    ::  Checks equality is false (3 > 3)
    ::
    %-  expect
      !>  !(gth 3 3)
    ::  Checks greater than case (5 > 3)
    ::
    %-  expect
      !>  (gth 5 3)
    ::  Checks less than case (3 > 5) is false
    ::
    %-  expect
      !>  !(gth 3 5)
  ==
::
::  Test less than or equal (<=)
::
++  test-lte
  ;:  weld
    ::  Checks equality (3 <= 3)
    ::
    %-  expect
      !>  (lte 3 3)
    ::  Checks smaller value (3 <= 5)
    ::
    %-  expect
      !>  (lte 3 5)
    ::  Checks greater value is false (5 <= 3)
    ::
    %-  expect
      !>  !(lte 5 3)
  ==
::
::  Test less than (<)
::
++  test-lth
  ;:  weld
    ::  Checks equality is false (3 < 3)
    ::
    %-  expect
      !>  !(lth 3 3)
    ::  Checks smaller value is true (0 < 3)
    ::
    %-  expect
      !>  (lth 0 3)
    ::  Checks greater value is false (3 < 0)
    ::
    %-  expect
      !>  !(lth 3 0)
    ::  Checks normal comparisons (3 < 5 is true, 5 < 3 is false)
    ::
    %-  expect
      !>  (lth 3 5)
    %-  expect
      !>  !(lth 5 3)
  ==
::
::  Test maximum (max)
::
++  test-max
  ;:  weld
    ::  Checks max(5,3) -> 5
    ::
    %+  expect-eq
      !>  5
      !>  (max 5 3)
    ::  Checks max(3,5) -> 5
    ::
    %+  expect-eq
      !>  5
      !>  (max 3 5)
    ::  Checks max(4,4) -> 4
    ::
    %+  expect-eq
      !>  4
      !>  (max 4 4)
  ==
::
::  Test minimum (min)
::
++  test-min
  ;:  weld
    ::  Checks min(3,5) -> 3
    ::
    %+  expect-eq
      !>  3
      !>  (min 3 5)
    ::  Checks min(5,3) -> 3
    ::
    %+  expect-eq
      !>  3
      !>  (min 5 3)
    ::  Checks min(4,4) -> 4
    ::
    %+  expect-eq
      !>  4
      !>  (min 4 4)
  ==
::
::  Test modulus (mod)
::
++  test-mod
  ;:  weld
    ::  Checks modulus by zero fails
    ::
    %-  expect-fail
      |.  (mod 5 0)
    ::  Checks when dividend is smaller than divisor (mod 3,5 -> 3)
    ::
    %+  expect-eq
      !>  3
      !>  (mod 3 5)
    ::  Checks exact division (6 mod 3 -> 0)
    ::
    %+  expect-eq
      !>  0
      !>  (mod 6 3)
    ::  Checks division with remainder (10 mod 3 -> 1)
    ::
    %+  expect-eq
      !>  1
      !>  (mod 10 3)
  ==
::
::  Test multiplication (*)
::
++  test-mul
  ;:  weld
    ::  Checks zero property (0 * n = 0)
    ::
    %+  expect-eq
      !>  0
      !>  (mul 0 5)
    %+  expect-eq
      !>  0
      !>  (mul 5 0)
    ::  Checks identity property (1 * n = n)
    ::
    %+  expect-eq
      !>  5
      !>  (mul 1 5)
    %+  expect-eq
      !>  5
      !>  (mul 5 1)
    ::  Checks normal multiplication (3 * 4 = 12)
    ::
    %+  expect-eq
      !>  12
      !>  (mul 3 4)
  ==
--