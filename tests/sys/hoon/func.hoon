::  Tests for currying gates and ++corl
::
/+  *test
|%
++  test-func
  ;:  weld
    %+  expect-eq
      !>  `(list)`~[0 1 2]
      !>  ((curr oust `(list)`~[0 1 2]))
    %+  expect-eq
      !>  `@`6
      !>  ((curr roll add) (gulf 1 3))
    %+  expect-eq
      !>  `@`6
      !>  ((cury roll (gulf 1 3)) add)
    ::  check that ++corl strips face from b's subject
    ::
    %+  expect-eq
      !>  `@`15
      !>  ((corl same (cury roll (gulf 1 5))) add)
  ==
::
--
