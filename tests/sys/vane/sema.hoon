::  test sema vane
/+  *test, v=test-sema-gall
|%
++  test-watch
  %-  run-chain
  |.  :-  %&
  =+  (nec-bud:v [nec=2 bud=3] nec=0 bud=0)
  ~&  >  'call %sema with %poke'
  =^  *  ames.nec
   (sema-call:v ames.nec ~[/none] [%plea ~bud %g /ge/pub [%0 %s /foo]] *roof)
  %+  expect-eq
    !>  2
  !>  2
--
