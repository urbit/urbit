/+  new-hoon
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
  =/  run-arm=[=type =nock]
    (~(mint ut test-core-type) p:!>(*tang) [%limb name])
  ::
  :-  name
  ^-  test-func
  ::
  =-  ~&  [%compiled-test name]  -
  ::
  |=  eny=@uvJ
  ^-  tang
  ((hard tang) .*(test-core(+6 (init-test eny)) nock.run-arm))
::  +has-test-prefix: does the arm start with 'test-' or 'check-'?
::
::    TODO: what are 'check-' arms for? Are they different from test arms?
::
++  has-test-prefix
  |=  a=term  ^-  ?
  ?|  =((end 3 5 a) 'test-')
      =((end 3 6 a) 'check-')
  ==
::  +init-test: data initialized on a per-test basis
::
++  init-test  |=({eny/@uvJ} %*(. tester eny eny, check-iterations 10))
::  +tester: main testing core with helper arms to be used in tests
::
++  tester
  |_  $:  eny=@uvJ                                    ::  entropy
          check-iterations=@u                         ::  # of check trials
          current-iteration=@u                        ::  current iteration
      ==
  ::  #
  ::  #  %check
  ::  #
  ::    gates for quick check style tests.
  +|  %check
  ++  check
    |*  [generator=$-(@uvJ *) test=$-(* ?)]
    |-  ^-  tang
    ?:  (gth current-iteration check-iterations)
      ~
    ::  todo: wrap generator in mule so it can crash.
    =+  sample=(generator eny)
    ::  todo: wrap test in mule so it can crash.
    ?:  (test sample)
      %=  $
        eny    (shaf %huh eny)                        ::  xxx: better random?
        current-iteration  (add current-iteration 1)
      ==
    =/  case  +(current-iteration)
    =/  pl  ?+(case "" %1 "s")
    ::XXX sample is a noun
    [leaf+"falsified after {<case>} case{pl} by '{<`*`sample>}', seed {<eny>}"]~
  ::
  ::  todo: a generate function that takes an arbitrary span.
  ::
  ++  generate-range
    |=  [min=@ max=@]
    |=  c=@uvJ
    ^-  @
    =+  gen=(random:new-hoon c)
    =^  num  gen  (range:gen min max)
    num
  ::
  ++  generate-dict
    ::  generator which will produce a dict with {count} random pairs.
    |=  count=@u
    ::  generate a dict with entropy {c}.
    |=  c=@uvJ
    ::
    ::  gen: stateful random number generator
    ::  out: resulting map
    ::  i: loop counter
    ::
    =/  gen  (random:new-hoon c)
    =|  out=(dict:new-hoon @ud @ud)
    =|  i=@u
    |-
    ^-  (dict:new-hoon @ud @ud)
    ?:  =(i count)
      out
    =^  first  gen  (range:gen 0 100)
    =^  second  gen  (range:gen 0 100)
    $(out (put:dct:new-hoon out first second), i +(i))
  ::  #
  ::  #  %test
  ::  #
  ::    test expectation functions
  +|  %test
  ::  todo: unit testing libraries have a lot more to them than just eq.
  ++  expect-eq
    |=  a=vase
    ^-  tang
    ?@  q.a  [palm+[": " ~ ~ ~]^~[>%ex-expected-pair< (sell a)]]~
    ?:  =(-.q.a +.q.a)
      ~
    :~  palm+[": " ~ ~ ~]^~[leaf+"expected" (sell (slot 2 a))]
        palm+[": " ~ ~ ~]^~[leaf+"actual" (sell (slot 3 a))]
    ==
  ::  #
  ::  #  %formatting
  ::  #
  ::    test result presentation
  +|  %formatting
  ++  category
    |=  [a=tape b=tang]  ^-  tang
    ?:  =(~ b)  ~  :: test OK
    :-  leaf+"in: '{a}'"
    (turn b |=(c=tank rose+[~ "  " ~]^~[c]))
  --
--
