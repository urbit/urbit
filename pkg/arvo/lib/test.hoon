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
        [%palm [": " ~ ~ ~] [leaf+"actual" (sell actual) ~]]
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
    %&  [leaf+"expected failure - succeeded" ~]
  ==
::  +category: prepends a name to an error result; passes successes unchanged
::
++  category
  |=  [a=tape b=tang]  ^-  tang
  ?:  =(~ b)  ~  :: test OK
  :-  leaf+"in: '{a}'"
  (turn b |=(c=tank rose+[~ "  " ~]^~[c]))
--
