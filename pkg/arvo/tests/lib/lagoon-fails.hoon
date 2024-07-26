/-  *lagoon
/+  *test
/+  *lagoon
^|
|_  $:  atol=_.1e-3          :: absolute tolerance for precision of operations
        rtol=_.1e-5          :: relative tolerance for precision of operations
    ==
::  Auxiliary tools
++  is-equal
  |=  [a=ray b=ray]  ^-  tang
  ?:  =(a b)  ~
  :~  [%palm [": " ~ ~ ~] [leaf+"expected" "{<`ray`a>}"]]
      [%palm [": " ~ ~ ~] [leaf+"actual  " "{<`ray`b>}"]]
  ==
::
++  is-close
  |=  [a=ray b=ray]  ^-  tang
  ?:  (all:la (is-close:la a b [atol rtol]))  ~
  :~  [%palm [": " ~ ~ ~] [leaf+"expected" "{<a>}"]]
      [%palm [": " ~ ~ ~] [leaf+"actual  " "{<b>}"]]
  ==
::
++  test-zeros-mismatch
  ;:  weld
  %-  expect-fail
    |.  %+  add:la
          (zeros:la [~[5 1] 5 %uint ~])
        (zeros:la [~[1 5] 5 %uint ~])
  %-  expect-fail
    |.  %+  add:la
          (zeros:la [~[5 1] 5 %uint ~])
        (zeros:la [~[5 1] 5 %i754 ~])
  %-  expect-fail
    |.  =/  mar  (zeros:la [~[5 1] 5 %i754 ~])
        =.  bloq.meta.mar  6
        ?>  (check:la mar)
        ~
  %-  expect-fail
    |.  =/  mar  (zeros:la [~[5 1] 5 %i754 ~])
        =.  shape.meta.mar  ~[6 1]
        ?>  (check:la mar)
        ~
  ==
::
++  test-ones-mismatch
  ;:  weld
  %-  expect-fail
    |.  %+  add:la
          (ones:la [~[5 1] 5 %uint ~])
        (ones:la [~[1 5] 5 %uint ~])
  %-  expect-fail
    |.  %+  add:la
          (ones:la [~[5 1] 5 %uint ~])
        (ones:la [~[5 1] 5 %i754 ~])
  %-  expect-fail
    |.  =/  mar  (ones:la [~[5 1] 5 %i754 ~])
        =.  bloq.meta.mar  7
        ?>  (check:la mar)
        ~
  %-  expect-fail
    |.  =/  mar  (ones:la [~[5 1] 5 %i754 ~])
        =.  shape.meta.mar  ~[7 1]
        ?>  (check:la mar)
        ~
  ==
::
++  test-arithmetic-bounds
  ;:  weld
  %-  expect-fail
    |.  %+  add:la
          (ones:la [~[5 1] 3 %i754 ~])
        (ones:la [~[5 1] 3 %i754 ~])
  %-  expect-fail
    |.  %+  add:la
          (ones:la [~[5 1] 8 %i754 ~])
        (ones:la [~[5 1] 8 %i754 ~])
  ::
  %-  expect-fail
    |.  %+  sub:la
          (ones:la [~[5 1] 3 %i754 ~])
        (ones:la [~[5 1] 3 %i754 ~])
  %-  expect-fail
    |.  %+  sub:la
          (ones:la [~[5 1] 8 %i754 ~])
        (ones:la [~[5 1] 8 %i754 ~])
  ::
  %-  expect-fail
    |.  %+  mul:la
          (ones:la [~[5 1] 3 %i754 ~])
        (ones:la [~[5 1] 3 %i754 ~])
  %-  expect-fail
    |.  %+  mul:la
          (ones:la [~[5 1] 8 %i754 ~])
        (ones:la [~[5 1] 8 %i754 ~])
  ::
  %-  expect-fail
    |.  %+  div:la
          (ones:la [~[5 1] 3 %i754 ~])
        (ones:la [~[5 1] 3 %i754 ~])
  %-  expect-fail
    |.  %+  div:la
          (ones:la [~[5 1] 8 %i754 ~])
        (ones:la [~[5 1] 8 %i754 ~])
  ::
  %-  expect-fail
    |.  %+  mod:la
          (ones:la [~[5 1] 3 %i754 ~])
        (ones:la [~[5 1] 3 %i754 ~])
  %-  expect-fail
    |.  %+  mod:la
          (ones:la [~[5 1] 8 %i754 ~])
        (ones:la [~[5 1] 8 %i754 ~])
  ==
::
++  test-iota-underflow
  ;:  weld
  %-  expect-fail
    |.  (iota:la [~[0] 3 %uint ~])
  ==
::
++  test-fill-clipping
  ;:  weld
  %+  expect-eq
    !>  (fill:la [~[5 1] 4 %uint ~] 0xffff)
    !>  (fill:la [~[5 1] 4 %uint ~] 0x1.ffff)
  %+  expect-eq
    !>  (fill:la [~[5 1] 3 %uint ~] 0xfe)
    !>  (fill:la [~[5 1] 3 %uint ~] 0x1.fffe)
  ==
::
--
