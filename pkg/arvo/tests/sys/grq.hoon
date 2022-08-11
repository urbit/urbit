::  test gall request queue fix, which implicates ames and gall
::
/+  *test, vane-test
|%
++  test-nec-bud
  =+  ames-nec-bud:vane-test
  ;:  weld
    %+  expect-eq
      !>  nec
      !>  nec
  ::
    %+  expect-eq
      !>  bud
      !>  bud
  ==
--
