::  tests for the either core.
/+  new-hoon, tester
=,  thr:new-hoon
=/  data/(list (either @u tape))  [[%& 1] [%| "one"] [%& 2] [%| "two"] ~]
|_  tester-type:tester
++  test-apply
  %+  expect-eq
    %^  apply  `(either @u tape)`[%| "one"]
      |=(a/@u "left")
    |=(b/tape "right")
  "right"
::
++  test-firsts
  %+  expect-eq
    (firsts data)
  [1 2 ~]
::
++  test-seconds
  %+  expect-eq
    (seconds data)
  ["one" "two" ~]
::
++  test-partition
  %+  expect-eq
    (partition data)
  [[1 2 ~] ["one" "two" ~]]
--
