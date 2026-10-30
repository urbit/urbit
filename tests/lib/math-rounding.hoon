/+  *test, math
::::  /tests/lib/math-rounding -- round-bankers (ties-to-even) + log(-inf)
::
^|
|%
::  round-bankers ties to the nearest EVEN integer (banker's rounding).
++  test-round-bankers  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@rs`.2)  !>((~(round-bankers rs:math [%n .1e-5 .0]) .2.5))   ::  -> 2 (even)
    %+  expect-eq  !>(`@rs`.2)  !>((~(round-bankers rs:math [%n .1e-5 .0]) .1.5))   ::  -> 2 (even)
    %+  expect-eq  !>(`@rs`.4)  !>((~(round-bankers rs:math [%n .1e-5 .0]) .3.5))   ::  -> 4 (even)
    %+  expect-eq  !>(`@rs`.1)  !>((~(round-bankers rs:math [%n .1e-5 .0]) .1.49))  ::  -> 1
    %+  expect-eq  !>(`@rd`.~2)  !>((~(round-bankers rd:math [%n .~1e-10 .~0]) .~2.5))
  ==
::  log(-inf) is NaN (was 0.0 in the rd door -- a copy-paste from exp).
++  test-log-ninf  ^-  tang
  ;:  weld
    %+  expect-eq  !>(`@rd`0x7ff8.0000.0000.0000)
      !>((~(log rd:math [%n .~1e-10 .~0]) `@rd`0xfff0.0000.0000.0000))
    %+  expect-eq  !>(`@rs`0x7fc0.0000)
      !>((~(log rs:math [%n .1e-5 .0]) `@rs`0xff80.0000))
  ==
--
