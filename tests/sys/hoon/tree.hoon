/+  *test
|%
::
::  Test tree addressing: cap
::
++  test-cap
  ;:  weld
    ::  Checks base case: cap of 2 should yield %2
    ::
    %+  expect-eq
      !>  %2
      !>  (cap 2)
    ::  Checks base case: cap of 3 should yield %3
    ::
    %+  expect-eq
      !>  %3
      !>  (cap 3)
    ::  Checks recursive case: cap of 4 reduces to cap of 2 (%2)
    ::
    %+  expect-eq
      !>  %2
      !>  (cap 4)
    ::  Checks recursive case: cap of 6 reduces to cap of 3 (%3)
    ::
    %+  expect-eq
      !>  %3
      !>  (cap 6)
  ==
::
::  Test tree addressing: mas
::
++  test-mas
  ;:  weld
    ::  Checks base case: mas of 2 returns 1
    ::
    %+  expect-eq
      !>  1
      !>  (mas 2)
    ::  Checks base case: mas of 3 returns 1
    ::
    %+  expect-eq
      !>  1
      !>  (mas 3)
    ::  Checks recursive case: mas(4) = 0 + 2*mas(2) = 2
    ::
    %+  expect-eq
      !>  2
      !>  (mas 4)
    ::  Checks recursive case: mas(5) = 1 + 2*mas(2) = 3
    ::
    %+  expect-eq
      !>  3
      !>  (mas 5)
    ::  Checks recursive case: mas(6) = 0 + 2*mas(3) = 2
    ::
    %+  expect-eq
      !>  2
      !>  (mas 6)
    ::  Checks recursive case: mas(7) = 1 + 2*mas(3) = 3
    ::
    %+  expect-eq
      !>  3
      !>  (mas 7)
  ==
::
::  Test tree addressing: peg
::
++  test-peg
  ;:  weld
    ::  Checks base case: peg(2,1) should return 2
    ::
    %+  expect-eq
      !>  2
      !>  (peg 2 1)
    ::  Checks base case: peg(2,2) should return (mul 2 2) = 4
    ::
    %+  expect-eq
      !>  4
      !>  (peg 2 2)
    ::  Checks base case: peg(2,3) should return (mul 2 2) + 1 = 5
    ::
    %+  expect-eq
      !>  5
      !>  (peg 2 3)
    ::  Checks peg with different root: peg(3,3) should return (mul 3 2) + 1 = 7
    ::
    %+  expect-eq
      !>  7
      !>  (peg 3 3)
  ==
--