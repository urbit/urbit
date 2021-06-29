/+  *test, *trig-rs
::
::::
  ::
|%
++  test-factorial  ^-  tang
  ;:  weld
    %+  expect-eq
      !>  .1
      !>  (factorial .0)
    %+  expect-eq
      !>  .1
      !>  (factorial .1)
    %+  expect-eq
      !>  .120
      !>  (factorial .5)
    %+  expect-eq
      !>  .720
      !>  (factorial .6)
  ==
--

