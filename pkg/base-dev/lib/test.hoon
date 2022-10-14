::  testing utilities meant to be directly used from files in %/tests
::
|%
::  +expect-eq: compares :expected and :actual and pretty-prints the result
::
++  expect-eq
  |=  [expected=vase actual=vase]
  ^-  tang
  ::
  =|  result=tang
  ::
  =?  result  !=(q.expected q.actual)
    %+  weld  result
    ^-  tang
    :~  [%palm [": " ~ ~ ~] [leaf+"expected" (sell expected) ~]]
        [%palm [": " ~ ~ ~] [leaf+"actual  " (sell actual) ~]]
    ==
  ::
  =?  result  !(~(nest ut p.actual) | p.expected)
    %+  weld  result
    ^-  tang
    :~  :+  %palm  [": " ~ ~ ~]
        :~  [%leaf "failed to nest"]
            (~(dunk ut p.actual) %actual)
            (~(dunk ut p.expected) %expected)
    ==  ==
  result
::  +expect: compares :actual to %.y and pretty-prints anything else
::
++  expect
  |=  actual=vase
  (expect-eq !>(%.y) actual)
::  +expect-fail: kicks a trap, expecting crash. pretty-prints if succeeds
::
++  expect-fail
  |=  a=(trap)
  ^-  tang
  =/  b  (mule a)
  ?-  -.b
    %|  ~
    %&  ['expected failure - succeeded' ~]
  ==
::  +expect-runs: kicks a trap, expecting success; returns trace on failure
::
++  expect-success
  |=  a=(trap)
  ^-  tang
  =/  b  (mule a)
  ?-  -.b
    %&  ~
    %|  ['expected success - failed' p.b]
  ==
::  $a-test-chain: a sequence of tests to be run
::
::  NB: arms shouldn't start with `test-` so that `-test % ~` runs
::
+$  a-test-chain
  $_
  |?
  ?:  =(0 0)
    [%& p=*tang]
  [%| p=[tang=*tang next=^?(..$)]]
::  +run-chain: run a sequence of tests, stopping at first failure
::
++  run-chain
  |=  seq=a-test-chain
  ^-  tang
  =/  res  $:seq
  ?-  -.res
    %&  p.res
    %|  ?.  =(~ tang.p.res)
          tang.p.res
        $(seq next.p.res)
  ==
::  +category: prepends a name to an error result; passes successes unchanged
::
++  category
  |=  [a=tape b=tang]  ^-  tang
  ?:  =(~ b)  ~  :: test OK
  :-  leaf+"in: '{a}'"
  (turn b |=(c=tank rose+[~ "  " ~]^~[c]))
--
