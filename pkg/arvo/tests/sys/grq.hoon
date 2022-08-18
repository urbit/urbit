::  test gall request queue fix, which implicates ames and gall
::
/+  *test, vane-test
|%
++  test-setup-nec-bud
  =+  nec-bud:vane-test
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
