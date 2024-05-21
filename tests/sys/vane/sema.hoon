::  test sema vane
::
/+  *test, v=test-sema-gall
|%
++  test-watch
  %-  run-chain
  |.  :-  %&
  =+  (nec-bud:v [nec=2 bud=3] nec=0 bud=0)
  ~&  >  'call %sema with %poke'
  =^  moves-1  ames.nec
    ::  %poke ~bud
    ::
    (sema-call:v ames.nec ~[/poke] [%plea ~bud %g /ge/pok [%0 %m noun/0]] *roof)
  ~&  moves-1
  %+  expect-eq
    !>  2
  !>  2
--
