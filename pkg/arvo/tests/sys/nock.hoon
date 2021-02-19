/+  *test
|%
::  nock 6 should allow invalid formulas in unevaluated branches
::
++  test-conditional-skips
  ;:  weld
    %+  expect-eq
      !>  43
      !>  .*(~ [%6 [%1 1] 0 [%1 43]])
    ::
    %-  expect-fail
      |.  .*(~ [%6 [%1 0] 0 [%1 43]])
    ::
    %+  expect-eq
      !>  42
      !>  .*(~ [%6 [%1 0] [%1 42] 0])
    ::
    %-  expect-fail
      |.  .*(~ [%6 [%1 1] [%1 42] 0])
    ::
    %+  expect-eq
      !>  42
      !>  .*(~ [%6 [%1 0] [%1 42] %1 43])
    ::
    %+  expect-eq
      !>  43
      !>  .*(~ [%6 [%1 1] [%1 42] %1 43])
    ::
    %-  expect-fail
      |.  .*(~ [%6 [%1 2] [%1 42] %1 43])
  ==
::  nock 9 should support axis 1
::
++  test-call-one
  %+  expect-eq
    !>  0
    !>  .*([3 0 1] [9 1 [0 1]])
--
