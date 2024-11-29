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
    =/  face-printer  ::  modify face printer to hide all faces
        |=  [inp=tase:us eta=meta:us bas=base:us]
        ^-  (unit [meta:us tank:us])
        =+  typ=?-(-.inp %& p.inp, %| p.p.inp)
        ?>  ?=([%face *] typ)
        =+  ?-(-.inp %& inp(p q.typ), %| inp(p.p q.typ))
        (bas - eta)
    =/  custom-printers=(map term ppin:us)  (my [[%face face-printer] ~])
    :~
      :+  %palm  [": " ~ ~ ~]
      [leaf+"expected" (draw:us [%| expected] [20 %most custom-printers]) ~]
      :+  %palm  [": " ~ ~ ~]
      [leaf+"actual" (draw:us [%| actual] [20 %most custom-printers]) ~]
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
::  +expect-success: kicks a trap, expecting success; returns trace on failure
::
++  expect-success
  |=  a=(trap)
  ^-  tang
  =/  b  (mule a)
  ?-  -.b
    %&  ~
    %|  ['expected success - failed' p.b]
  ==
::
::  +expect-fail-message: kicks a trap, expecting crash, compares the resulting error message
++  expect-fail-message
  |=  [msg=@t a=(trap)]
  ^-  tang
  =/  b  (mule a)
  ?-  -.b
    %|  |^
        =/  =tang  (flatten +.b)
        ?:  ?=(^ (find (trip msg) tang))
          ~
        ['expected error message - not found' ~]
        ++  flatten
          |=  tang=(list tank)
          =|  res=tape
          |-  ^-  tape
          ?~  tang  res
          $(tang t.tang, res (weld ~(ram re i.tang) res))
        --
    %&  ['expected failure - succeeded' ~]
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
