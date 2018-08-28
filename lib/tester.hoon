|%
::  $test: a test with a fully resolved path
::
+$  test  [=path func=test-func]
::  $test-arm: a test with a name (derived from its arm name in a test core)
::
+$  test-arm  [name=term func=test-func]
::  $test-func: a single test, as a gate; sample is entropy, produces failures
::
+$  test-func  $-(@uvJ tang)
--
|%
::  +resolve-test-paths: add test names to file paths to form full identifiers
::
++  resolve-test-paths
  |=  paths-to-tests=(map path (list test-arm))
  ^-  (list test)
  ::
  %-  zing
  %+  turn  ~(tap by paths-to-tests)
  |=  [=path test-arms=(list test-arm)]
  ^-  (list test)
  ::  strip off leading 'tests' from :path
  ::
  =.  path
    ?>  ?=(^ path)
    ?>  ?=(%tests i.path)
    t.path
  ::  for each test, add the test's name to :path
  ::
  %+  turn  test-arms
  |=  =test-arm
  ^-  test
  [(weld path /[name.test-arm]) func.test-arm]
::  +get-test-arms: convert test arms to functions and produce them
::
++  get-test-arms
  |=  [test-core-type=type test-core=*]
  ^-  (list test-arm)
  ::
  =/  arms=(list @tas)  (sort (sloe test-core-type) aor)
  ::
  %+  turn  (skim arms has-test-prefix)
  |=  name=term
  ^-  test-arm
  ::
  ?>  (~(nest ut (~(peek ut test-core-type) %free 6)) & p:!>((init-test)))
  ::
  ~|  [%failed-to-compile-test-arm name]
  =/  run-arm=[=type =nock]
    (~(mint ut test-core-type) p:!>(*tang) [%limb name])
  ::
  :-  name
  ^-  test-func
  ::
  |=  eny=@uvJ
  ^-  tang
  ((hard tang) .*(test-core(+6 (init-test eny)) nock.run-arm))
::  +has-test-prefix: does the arm define a test we should run?
::
++  has-test-prefix
  |=  a=term  ^-  ?
  =((end 3 5 a) 'test-')
::  +init-test: data initialized on a per-test basis
::
++  init-test  |=(eny=@uvJ ~(. tester eny))
::  +tester: main testing core with helper arms to be used in tests
::
::    TODO provide a lot more helper functions.
::
++  tester
  |_  eny=@uvJ
  ::  +expect-eq: compares !>([expected actual]) and pretty-prints the result
  ::
  ++  expect-eq
    |=  a=vase
    ^-  tang
    ?@  q.a  [palm+[": " ~ ~ ~]^~[>%ex-expected-pair< (sell a)]]~
    ?:  =(-.q.a +.q.a)
      ~
    :~  palm+[": " ~ ~ ~]^~[leaf+"expected" (sell (slot 2 a))]
        palm+[": " ~ ~ ~]^~[leaf+"actual" (sell (slot 3 a))]
    ==
  ::
  ++  expect-nu-eq
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
  ::  +category: prepends a name to an error result; passes successes unchanged
  ::
  ++  category
    |=  [a=tape b=tang]  ^-  tang
    ?:  =(~ b)  ~  :: test OK
    :-  leaf+"in: '{a}'"
    (turn b |=(c=tank rose+[~ "  " ~]^~[c]))
  --
--
