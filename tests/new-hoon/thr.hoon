::  tests for the either core.
/+  new-hoon, tester
=,  thr:new-hoon
=/  data/(list (either @u tape))  [[%& 1] [%| "one"] [%& 2] [%| "two"] ~]
|_  _tester:tester
++  test-apply
  %-  expect-eq  !>
  :-  "right"
  %^  apply  `(either @u tape)`[%| "one"]
    |=(a/@u "left")
  |=(b/tape "right")
::
++  test-firsts
  %-  expect-eq  !>
  :-  [1 2 ~]
  (firsts data)
::
++  test-seconds
  %-  expect-eq  !>
  :-  ["one" "two" ~]
  (seconds data)
::
++  test-partition
  %-  expect-eq  !>
  :-  [[1 2 ~] ["one" "two" ~]]
  (partition data)
--
